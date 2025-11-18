install.packages("data.table")
library("data.table")

setwd("C:/Users/hh7473a/Desktop/HMDA_Data")
infile  <- "HMDA_clean/HMDA_30y_clean.csv"
out_dir <- getwd()

DT <- fread(infile, na.strings = c("", "NA", "Exempt"))

# ==== Basic checking availability ====
# Ensure necessary columns exist
need_cols <- c("activity_year","derived_msa_md","loan_amount","interest_rate","loan_term")
stopifnot(all(need_cols %in% names(DT)))

# Transfer the type of values
numify <- function(x) suppressWarnings(as.numeric(x))
DT[, loan_amount   := numify(loan_amount)]
DT[, interest_rate := numify(interest_rate)]
DT[, loan_term     := numify(loan_term)]
DT <- DT[!is.na(loan_amount) & loan_amount>0 &
           !is.na(interest_rate) & interest_rate>0 &
           !is.na(loan_term) & loan_term>0]

# Double check：intro_rate_period == 0 或 NA
if ("intro_rate_period" %in% names(DT)) {
  DT[, intro_rate_period := numify(intro_rate_period)]
  DT <- DT[is.na(intro_rate_period) | intro_rate_period == 0]
}

# Only keep records with MSA (can change to county if needs)
DT <- DT[!is.na(derived_msa_md) & derived_msa_md != ""]

# ==== Prepare the evaluation year grid (take all years in the data range) ====
years <- sort(unique(DT$activity_year))

# ==== Remaining principal function (vectorized) ====
remaining_balance <- function(P, r_ann, N, k) {
  # P: Initial principal
  # r_ann: Annualized contract interest rate (percentage)
  # N: Total number of installments (months)
  # k: Number of months repaid
  r <- r_ann / 100 / 12
  # Boundary Processing
  k <- pmax(0, pmin(k, N))
  # r≈0 Robust treatment of: linear amortized approximation
  eps <- 1e-8
  RB <- numeric(length(P))
  # r > eps
  idx1 <- which(r > eps)
  if (length(idx1)) {
    r1 <- r[idx1]; P1 <- P[idx1]; N1 <- N[idx1]; k1 <- k[idx1]
    # Use a more stable form
    num <- (1 + r1)^N1 - (1 + r1)^k1
    den <- (1 + r1)^N1 - 1
    RB[idx1] <- P1 * num / den
  }
  # r <= eps：linearity
  idx0 <- which(!(r > eps))
  if (length(idx0)) {
    P0 <- P[idx0]; N0 <- N[idx0]; k0 <- k[idx0]
    RB[idx0] <- P0 * (1 - k0 / N0)
  }
  # Negative values are truncated to 0
  pmax(RB, 0)
}

# ==== Calculate the "year-end month age k" and obtain the stock balance weight ====
### Rule: Assume "mid-year payment" -> Year-end monthly age k = floor(12*(t - y + 0.5))
delta_midyear <- 0.5

# Output a panel of (MSA × evaluation year t)
res_list <- vector("list", length(years))
names(res_list) <- years

for (t in years) {
  DTt <- DT[activity_year <= t]
  
  # Calculate the age in months of the assessment year
  k <- floor(12 * (t - DTt$activity_year + delta_midyear))
  k <- pmax(0, pmin(k, DTt$loan_term))
  
  # Remaining principal (weight)
  w_out <- remaining_balance(
    P     = DTt$loan_amount,
    r_ann = DTt$interest_rate,
    N     = DTt$loan_term,
    k     = k
  )
  
  DTt[, weight_outstanding := w_out]
  DTt <- DTt[weight_outstanding > 0]
  
  # Weighted interest rate by MSA aggregate stock
  aggt <- DTt[
    , .(
      weighted_rate = sum(interest_rate * weight_outstanding, na.rm = TRUE) /
        sum(weight_outstanding, na.rm = TRUE),
      total_outstanding = sum(weight_outstanding, na.rm = TRUE),
      n_loans_alive = .N
    ),
    by = .(derived_msa_md)
  ]
  aggt[, eval_year := t]
  setcolorder(aggt, c("eval_year","derived_msa_md","weighted_rate","total_outstanding","n_loans_alive"))
  res_list[[as.character(t)]] <- aggt
  cat(sprintf("Year %d done: %d MSAs\n", t, nrow(aggt)))
}

MSA_panel <- rbindlist(res_list, use.names = TRUE, fill = TRUE)

### Temporary output results ###
fwrite(MSA_panel, file.path(out_dir, "MSA_outstanding_weighted_rate.csv"))
saveRDS(MSA_panel, file.path(out_dir, "MSA_outstanding_weighted_rate.rds"))

