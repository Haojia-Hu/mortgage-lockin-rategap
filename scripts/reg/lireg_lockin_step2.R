install.packages("data.table")
install.packages("dplyr")
install.packages("stringr")
install.packages("purrr")
install.packages("sandwich")
install.packages("tibble")
install.packages("AER")

library(data.table)
library(dplyr)
library(stringr)
library(purrr)
library(sandwich)
library(tibble)
library(AER)

# ==== Path ====
base_dir <- "C:/Users/hh7473a/Desktop/Lockin proxy"

# ---- Helper: read a proxy file into a (cbsa_code, ym, proxy_col) tibble ----
read_proxy <- function(file, newname){
  fread(file.path(base_dir, file)) %>%
    transmute(
      cbsa_code = str_pad(as.character(cbsa_code), 5, pad = "0"),
      ym        = as.character(yearmon),
      !!newname := as.numeric(proxy_value)
    ) %>% distinct(cbsa_code, ym, .keep_all = TRUE)
}

# 5 proxies
inv      <- read_proxy("zillow_invt.csv",            "proxy_invt")
newl     <- read_proxy("zillow_newlisting.csv",      "proxy_newlist")
pend     <- read_proxy("zillow_newlypending.csv",    "proxy_pending")
daytp    <- read_proxy("zillow_daytopending.csv",    "proxy_days")
pricecut <- read_proxy("zillow_sharepricecut.csv",   "proxy_cut")

# Combined them
proxy_all <- reduce(list(inv, newl, pend, daytp, pricecut),
                    full_join, by = c("cbsa_code","ym"))

# Read in the Step 1 panel (including rate_gap & controls), and [Single IV] Z_bartik
# Note: SS-IV.csv / Bartik_IV_matrix.csv should have the following columns: cbsa_code, ym, Z_bartik
panel_core <- fread(file.path("C:/Users/hh7473a/Desktop/Panel_rategap_hat.csv")) %>%
  mutate(cbsa_code = str_pad(as.character(cbsa_code), 5, pad = "0"),
         ym        = as.character(ym))

# ==== Comnbined proxies and IV and rategap ====
panel <- panel_core %>%
  left_join(proxy_all, by = c("cbsa_code","ym")) %>%
  filter(!is.na(rate_gap),
         !is.na(Z_bartik),
         !is.na(unemployment_rate),
         !is.na(mig_rate_month),
         !is.na(bps_total))

# ==== Check ====
names(panel)

cat("Summary of panel data coverage:\n")
cat("Total observations:", nrow(panel), "\n")
cat("Unique CBSAs:", n_distinct(panel$cbsa_code), "\n")
cat("Unique months:", n_distinct(panel$ym), "\n")

panel_summary <- panel %>%
  summarise(
    total_obs = n(),
    n_cbsa = n_distinct(cbsa_code),
    n_month = n_distinct(ym)
  )
print(panel_summary)

# 1pp 版本
panel <- panel %>% mutate(rate_gap_pp = rate_gap)
panel <- panel %>%
  mutate(
    cbsa_code = as.factor(cbsa_code),
    ym        = as.factor(ym)
  )

# ==== Diagnostic extraction tools ====
.extract_diag <- function(fit){
  s <- summary(fit, diagnostics = TRUE)
  diag_tbl <- tryCatch(s$diagnostics, error = function(e) NULL)
  get_stat <- function(tbl, pattern, col){
    if (is.null(tbl)) return(NA_real_)
    rn <- rownames(tbl); hit <- which(grepl(pattern, rn, ignore.case = TRUE))
    if (length(hit) == 0) return(NA_real_)
    as.numeric(tbl[hit[1], col, drop = TRUE])
  }
  list(
    KP_F     = get_stat(diag_tbl, "Kleibergen|Weak instruments", "statistic"),
    AR_p     = get_stat(diag_tbl, "Anderson|Anderson.?Rubin",    "p-value"),
    Wu_p     = get_stat(diag_tbl, "Wu.?Hausman|Wu-?Hausman",     "p-value"),
    Sargan_p = get_stat(diag_tbl, "Sargan|Basmann|Hansen",       "p-value")
  )
}

# ==== IV：same as 2SLS ====
.partialled_sds <- function(fit, yvar, xvar){
  mf <- model.frame(fit)  # same as ivreg sample
  all_terms <- attr(terms(fit), "term.labels")
  ctrls <- setdiff(all_terms, xvar)  # controls+FE
  
  f_y <- as.formula(paste(yvar, "~", paste(ctrls, collapse = " + ")))
  y_res <- resid(lm(f_y, data = mf, na.action = na.omit))
  
  f_x <- as.formula(paste(xvar, "~", paste(ctrls, collapse = " + ")))
  x_res <- resid(lm(f_x, data = mf, na.action = na.omit))
  
  c(sd_y = sd(y_res, na.rm = TRUE),
    sd_x = sd(x_res, na.rm = TRUE))
}

# ==== 2SLS: Raw(1pp) ====
run_iv_raw <- function(depvar){
  fml <- as.formula(paste0(
    depvar, " ~ rate_gap + unemployment_rate + mig_rate_month + bps_total + cbsa_code + ym | ",
    "Z_bartik + unemployment_rate + mig_rate_month + bps_total + cbsa_code + ym"
  ))

  
  fit <- ivreg(fml, data = panel, x = TRUE, y = TRUE, model = TRUE)
  
  mf <- model.frame(fit)  # 与 ivreg 完全一致的样本
  
  # Key point: Clearly define the data environment for parsing clustering variables in mf.
  vc_cl <- vcovCL(fit, cluster = ~ cbsa_code, data = mf)
  
  
  beta_raw <- unname(coef(fit)["rate_gap"])
  se_raw   <- sqrt(diag(vc_cl))["rate_gap"]
  
  # The correct "standardized" conversion (partialled-out)
  sds <- .partialled_sds(fit, yvar = depvar, xvar = "rate_gap")
  scale_fx <- sds["sd_x"] / sds["sd_y"]
  beta_std <- beta_raw * scale_fx
  se_std   <- se_raw   * scale_fx
  
  dg <- .extract_diag(fit)
  
  tibble(
    proxy   = depvar,
    spec    = "Raw(1pp)",
    beta    = beta_raw,
    se      = se_raw,
    beta_std = beta_std,
    se_std   = se_std,
    KP_F  = dg$KP_F, AR_p = dg$AR_p, Wu_p = dg$Wu_p, Sargan_p = dg$Sargan_p
  )
}

# ==== Summary ====
proxy_cols <- c("proxy_invt","proxy_newlist","proxy_pending","proxy_days","proxy_cut")

results_iv_raw <- purrr::map_dfr(proxy_cols, run_iv_raw)

# View1：Raw 1pp
results_raw_view <- results_iv_raw %>%
  transmute(proxy, spec = "Raw(1pp)",
            beta, se, KP_F, AR_p, Wu_p, Sargan_p)

# View2：Z-score（partialled）
results_std_view <- results_iv_raw %>%
  transmute(proxy, spec = "Z-score (partialled)",
            beta = beta_std, se = se_std,
            KP_F, AR_p, Wu_p, Sargan_p)

results_iv_all <- dplyr::bind_rows(results_raw_view, results_std_view)
print(results_iv_all)
fwrite(results_iv_all, file.path("C:/Users/hh7473a/Desktop/lockin_reg_results_new.csv"))

