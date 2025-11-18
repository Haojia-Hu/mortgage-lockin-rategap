install.packages("data.table")
install.packages("lubridate")
library("data.table")
library("lubridate")

setwd("C:/Users/hh7473a/Desktop/HMDA_Data")
infile  <- "HMDA_clean/HMDA_30y_clean.csv"
out_dir <- getwd()

DT <- fread(infile, na.strings = c("", "NA", "Exempt"))

# ---- 1) Basic checks & type casting ----
need_cols <- c("activity_year","derived_msa_md","loan_amount","interest_rate","loan_term")
stopifnot(all(need_cols %in% names(DT)))

numify <- function(x) suppressWarnings(as.numeric(x))
DT[, loan_amount   := numify(loan_amount)]
DT[, interest_rate := numify(interest_rate)]
DT[, loan_term     := numify(loan_term)]

# Keep essential, valid rows
DT <- DT[
  !is.na(loan_amount) & loan_amount > 0 &
    !is.na(interest_rate) & interest_rate > 0 &
    !is.na(loan_term) & loan_term > 0 &
    !is.na(derived_msa_md) & derived_msa_md != ""
]

# If you haven't filtered out ARMs previously, you can uncomment:
if ("intro_rate_period" %in% names(DT)) {
 DT[, intro_rate_period := numify(intro_rate_period)]
 DT <- DT[is.na(intro_rate_period) | intro_rate_period == 0]
}

# ---- 2) Monthly evaluation grid (2018-01 to 2024-12) ----
start_date <- as.Date("2018-01-01")
end_date   <- as.Date("2024-12-01")
eval_dates <- seq.Date(from = start_date, to = end_date, by = "month")
grid <- data.table(eval_date = eval_dates)
grid[, `:=`(eval_year  = year(eval_date),
            eval_month = month(eval_date))]

# ---- 3) Remaining balance function (vectorized) ----
remaining_balance <- function(P, r_ann, N, k) {
  # P: principal; r_ann: annual rate in percent; N: months; k: elapsed months
  # returns outstanding balance after k payments (non-negative)
  r <- r_ann / 100 / 12
  k <- pmax(0, pmin(k, N))  # clamp to [0, N]
  eps <- 1e-8
  RB <- numeric(length(P))
  idx_pos <- which(r > eps)
  if (length(idx_pos)) {
    r1 <- r[idx_pos]; P1 <- P[idx_pos]; N1 <- N[idx_pos]; k1 <- k[idx_pos]
    num <- (1 + r1)^N1 - (1 + r1)^k1
    den <- (1 + r1)^N1 - 1
    RB[idx_pos] <- P1 * num / den
  }
  idx_zero <- which(!(r > eps))
  if (length(idx_zero)) {
    P0 <- P[idx_zero]; N0 <- N[idx_zero]; k0 <- k[idx_zero]
    RB[idx_zero] <- P0 * (1 - k0 / N0)
  }
  pmax(RB, 0)
}

# ---- 4) Expected weight per loan under rolling monthly originations ----
# Assumptions:
# - Each annual origination is uniformly distributed over 12 months (m = 1..12).
# - For a given evaluation (ty, tm), compute k_raw for each possible m,
#   keep only those months where k_raw >= 0 (already originated),
#   and average RB (optionally with exp(-lambda * k_raw)) over those months.
delta_mid <- 0.5   # "mid-of-month" adjustment (e.g., originated mid-month; evaluated at month-end)
lambda_m  <- 0.005  # exponential decay per month (0 = off; try 0.005 ~ 0.02 for sensitivity)

expected_weight_monthly <- function(ty, tm, P, r_ann, N, y) {
  # ty/tm: evaluation year/month (integers)
  # P, r_ann, N, y: vectors of loan-level principal, annual rate, term, orig. year
  # returns a vector of expected weights (size = length(P))
  
  n <- length(P)
  if (!all(lengths(list(r_ann, N, y)) == n)) stop("Input vectors must have equal length.")
  
  # base months since Jan of origination year to the evaluation month-end
  base <- (ty - y) * 12 + (tm - 1)
  
  # construct k_raw matrix for m = 1..12
  # k_raw[i, j] = floor(base[i] - (j-1) + delta_mid)
  m_idx <- 0:11
  k_raw <- outer(base, m_idx, FUN = function(b, mm) floor(b - mm + delta_mid))
  # ensure matrix shape (outer keeps dims, but be explicit)
  dim(k_raw) <- c(n, 12)
  
  # clamp to [0, N]: build N matrix explicitly to keep dimensions
  N_mat <- matrix(N, nrow = n, ncol = 12)
  k_mat <- pmax(0, pmin(k_raw, N_mat))
  dim(k_mat) <- c(n, 12)  # re-assert dims to avoid drop
  
  # compute RB for each column j = 1..12 (avoid dimension drop)
  RB_mat <- matrix(0.0, nrow = n, ncol = 12)
  for (j in 1:12) {
    RB_mat[, j] <- remaining_balance(P, r_ann, N, k_mat[, j])
  }
  
  # optional decay: exp(-lambda * k_raw) using the un-clamped k_raw (only where originated)
  if (lambda_m > 0) {
    decay_mat <- exp(-lambda_m * pmax(k_raw, 0))
    RB_mat <- RB_mat * decay_mat
  }
  
  # mask for "already originated" months (k_raw >= 0)
  mask <- (k_raw >= 0)
  
  # expected weight: average over originated months for each loan
  denom <- pmax(rowSums(mask), 1L)            # avoid division by zero
  w_exp <- rowSums(RB_mat * mask) / denom
  w_exp[denom == 0] <- 0                     # before origination year -> 0 weight
  
  return(w_exp)
}

# ---- 5) Main loop: build MSA × month panel ----
res_list <- vector("list", nrow(grid))

for (idx in seq_len(nrow(grid))) {
  ty <- grid$eval_year[idx]
  tm <- grid$eval_month[idx]
  
  # keep loans with origination year <= ty
  D <- DT[activity_year <= ty]
  if (nrow(D) == 0L) {
    res_list[[idx]] <- data.table(
      eval_year = ty, eval_month = tm,
      derived_msa_md = character(0),
      weighted_rate = numeric(0),
      total_outstanding = numeric(0),
      n_loans_alive = integer(0)
    )
    next
  }
  
  # expected outstanding weight per loan at (ty, tm)
  w_exp <- expected_weight_monthly(
    ty = ty, tm = tm,
    P = D$loan_amount,
    r_ann = D$interest_rate,
    N = D$loan_term,
    y = D$activity_year
  )
  
  D[, weight_outstanding := w_exp]
  D <- D[weight_outstanding > 0]
  if (nrow(D) == 0L) {
    res_list[[idx]] <- data.table(
      eval_year = ty, eval_month = tm,
      derived_msa_md = character(0),
      weighted_rate = numeric(0),
      total_outstanding = numeric(0),
      n_loans_alive = integer(0)
    )
    next
  }
  
  # aggregate by MSA
  agg <- D[
    , .(
      weighted_rate = sum(interest_rate * weight_outstanding, na.rm = TRUE) /
        sum(weight_outstanding, na.rm = TRUE),
      total_outstanding = sum(weight_outstanding, na.rm = TRUE),
      n_loans_alive     = .N
    ),
    by = .(derived_msa_md)
  ]
  agg[, `:=`(eval_year = ty, eval_month = tm)]
  setcolorder(agg, c("eval_year","eval_month","derived_msa_md",
                     "weighted_rate","total_outstanding","n_loans_alive"))
  
  res_list[[idx]] <- agg
  
  if (idx %% 6 == 0) {
    cat(sprintf("Done %d/%d -> %04d-%02d (%d MSAs)\n",
                idx, nrow(grid), ty, tm, nrow(agg)))
  }
}

MSA_monthly <- rbindlist(res_list, use.names = TRUE, fill = TRUE)

# ---- 6) Save outputs ----
fwrite(MSA_monthly, file.path(out_dir, "MSA_outstanding_weighted_rate_monthly.csv"))
saveRDS(MSA_monthly, file.path(out_dir, "MSA_outstanding_weighted_rate_monthly.rds"))
