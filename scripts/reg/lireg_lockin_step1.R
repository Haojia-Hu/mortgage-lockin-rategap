install.packages("data.table")
install.packages("dplyr")
install.packages("stringr")
install.packages("fixest")
install.packages("broom")
install.packages("readr")

library(data.table)
library(dplyr)
library(stringr)
library(fixest)   
library(broom)
library(readr)

base_dir <- "C:/Users/hh7473a/Desktop"

# ==== Read and Input ====

# 1. Rate Gap
rg <- fread(file.path(base_dir, "Rate_gap_monthly.csv")) %>%
  transmute(cbsa_code = str_pad(as.character(derived_msa_md), 5, "left", "0"),
            ym = sprintf("%04d-%02d", as.integer(eval_year), as.integer(eval_month)),
            rate_gap = as.numeric(rate_gap))

# 2. SS-IV
iv <- fread(file.path(base_dir, "SS_IV.csv")) %>%
  transmute(
    cbsa_code = str_pad(as.character(cbsa_code), 5, "left", "0"),
    ym        = as.character(ym),
    # use the "Horizontal Residual × Exposure" tool; if we want to try cumulative impact, we change it to Z_bartik_cum
    Z_bartik  = as.numeric(Z_bartik_level)
  )

# 3. Unemployment
unemp <- fread(file.path(base_dir, "laus_unemployment_msa.csv")) %>%
  transmute(cbsa_code = str_pad(as.character(cbsa_code), 5, "left", "0"),
            ym = as.character(yearmon),
            unemployment_rate = as.numeric(unemployment_rate))

# 4. Migration
mig <- fread(file.path(base_dir, "cbsa_migration_monthly.csv")) %>%
  transmute(cbsa_code = str_pad(as.character(cbsa_code), 5, "left", "0"),
            ym = as.character(yearmon),
            mig_rate_month = as.numeric(mig_rate_month))

# 5. Building Permits (BPS)
bps <- fread(file.path(base_dir, "bps_cbsa_monthly.csv")) %>%
  transmute(cbsa_code = str_pad(as.character(cbsa), 5, "left", "0"),
            ym = as.character(yearmon),
            bps_total = as.numeric(total))

# ==== Combined datasets as panel data ====
panel <- rg %>%
  left_join(iv,    by = c("cbsa_code","ym")) %>%
  left_join(unemp, by = c("cbsa_code","ym")) %>%
  left_join(mig,   by = c("cbsa_code","ym")) %>%
  left_join(bps,   by = c("cbsa_code","ym")) %>%
  filter(
    !is.na(rate_gap),
    !is.na(Z_bartik),
    !is.na(unemployment_rate),
    !is.na(mig_rate_month),
    !is.na(bps_total)
  )

cat("No.of row:", nrow(panel),
    "No.of MSA:", length(unique(panel$cbsa_code)),
    "No.of Month:", length(unique(panel$ym)), "\n")


# ==== Step1(2sls): Regression ====
# Use "double fixed effects": regional FE + time FE; and cluster robust SE by cbsa_code
fsm <- feols(
  rate_gap ~ Z_bartik + unemployment_rate + mig_rate_month + bps_total | cbsa_code + ym,
  data = panel,
  cluster = ~ cbsa_code
)
print(summary(fsm))

# ==== F-test / Wald on IV ====
t_val <- summary(fsm)$coeftable["Z_bartik", "t value"]
F_stat <- t_val^2
F_stat


# ==== Output ====
panel$rategap_hat <- fitted(fsm)

out_file_hat   <- file.path(base_dir, "Rategap_hat_singleIV.csv")
out_file_panel <- file.path(base_dir, "Panel_rategap_hat.csv")
fwrite(panel %>% select(cbsa_code, ym, rategap_hat), out_file_hat)
fwrite(panel, out_file_panel)
cat("Output finished:", out_file_hat, "\n")
