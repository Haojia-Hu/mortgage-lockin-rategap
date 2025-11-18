install.packages("stringr")
install.packages("lubridate")
install.packages("readxl")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("data.table")
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(data.table)

setwd("C:/Users/hh7473a/Desktop")
getwd()
# ==== Pathway ====
pmms_file <- "historicalweeklydata.xlsx"   
gs10_file  <- "GS10.csv"           
out_file   <- "NatShock.csv" 

# ==== Read pmms weekly (header starts at row 5, we only need first two columns) ====
pmms_raw <- read_excel(
  path      = pmms_file,
  skip      = 4,      # start from the 5th row
  col_names = TRUE
)

# Standardize column names to avoid spaces/slashes issues
names(pmms_raw)[1:2] <- c("Week", "FRM30")

# Keep only Week & FRM30; drop empty rows
pmms_raw <- as.data.table(pmms_raw[, c("Week","FRM30")])
pmms_raw <- pmms_raw[!(is.na(Week) & is.na(FRM30))]

# Robust date parsing for mixed formats (ymd, mdy, excel serial)
to_Date_excel_mixed <- function(v) {
  # Handle numeric excel serial (including accidentally parsed as character digits)
  if (is.numeric(v)) {
    return(as.Date(v, origin = "1899-12-30"))
  }
  v_chr <- as.character(v)
  res <- as.Date(rep(NA_real_, length(v_chr)), origin = "1970-01-01")
  
  # Excel serial stored as 5+ digits string
  idx_serial <- grepl("^\\d{5,}$", v_chr)
  if (any(idx_serial)) {
    res[idx_serial] <- as.Date(as.numeric(v_chr[idx_serial]), origin = "1899-12-30")
  }
  
  # Try ymd first (handles yyyy/mm/dd and yyyy-mm-dd)
  idx_other <- which(!idx_serial)
  if (length(idx_other)) {
    tmp <- suppressWarnings(ymd(v_chr[idx_other]))
    # Fallback to mdy (handles mm/dd/yyyy)
    bad <- is.na(tmp)
    if (any(bad)) {
      tmp[bad] <- suppressWarnings(mdy(v_chr[idx_other][bad]))
    }
    res[idx_other] <- tmp
  }
  res
}

pmms_raw[, Week := to_Date_excel_mixed(Week)]
pmms_raw <- pmms_raw[!is.na(Week)]
pmms_raw[, FRM30 := as.numeric(FRM30)]
pmms_raw <- pmms_raw[!is.na(FRM30)]

# Restrict pmms weekly window (as specified) 
pmms_raw <- pmms_raw[
  Week >= as.Date("2018-01-04") & Week <= as.Date("2024-12-26")
]

# Weekly -> Monthly (simple monthly average of weekly FRM30)
pmms_raw[, `:=`(eval_year = year(Week), eval_month = month(Week))]
pmms_monthly <- pmms_raw[
  eval_year >= 2018 & eval_year <= 2024,
  .(pmms_30y = mean(FRM30, na.rm = TRUE)),
  by = .(eval_year, eval_month)
]

# Combine eval_year and eval_month into a proper Date (use first day of month)
pmms_monthly[, date := as.Date(sprintf("%d-%02d-01", eval_year, eval_month))]


# ==== Read 10-year treasury yield ====
gs10_raw <- read_csv("GS10.csv",
                     col_types = cols(
                       observation_date = col_character(),
                       GS10 = col_double()
                     ))

gs10_monthly <- gs10_raw %>%
  # ensure observation_date is correctly parsed
  mutate(
    # try ymd() first; if your date is like "2018-01-02"
    date = ymd(observation_date)
  ) %>%
  # if the previous line still gives NA, try mdy()
  # mutate(date = mdy(observation_date)) %>%
  # round down to month
  mutate(date = floor_date(date, unit = "month")) %>%
  arrange(date) %>%
  # keep 2018–2024 period
  filter(date >= ymd("2018-01-01") & date <= ymd("2024-12-31")) %>%
  transmute(
    date,
    gs10 = as.numeric(GS10)
  )

# ==== Finding the residual ====
shock_data <- pmms_monthly %>%
  inner_join(gs10_monthly, by = "date") %>%
  arrange(date)

# Reg：PMMS_t = α + β * GS10_t + u_t
model_lvl <- lm(pmms_30y ~ gs10, data = shock_data)

# Residual（NatShock_level）
shock_data <- shock_data %>%
  mutate(NatShock_level = resid(model_lvl))

library(ggplot2)
summary(model_lvl)
ggplot(shock_data, aes(date, NatShock_level)) + geom_line() + theme_minimal()


 acf(shock_data$NatShock_level, main = "ACF of NatShock")
 pacf(shock_data$NatShock_level, main = "PACF of NatShock")
 mean(shock_data$NatShock_level, na.rm = TRUE)
 sd(shock_data$NatShock_level, na.rm = TRUE)




# ========= Exposure item =========
install.packages("tidyverse")
library(tidyverse)

hmda_path <- "C:/Users/hh7473a/Desktop/HMDA_Data/HMDA_clean/HMDA_30y_clean.csv"

expo_year_min <- 2018
expo_year_max <- 2021

half_life_years <- 5

ref_year <- 2021

bin_edges <- c(-Inf, 3, 4, 5, 6, Inf)
bin_labels <- c("lt3", "3to4", "4to5", "5to6", "ge6")

hmda_raw <- read_csv(hmda_path,
                     col_types = cols(
                       activity_year = col_integer(),
                       state_code = col_character(),
                       county_code = col_character(),
                       derived_msa_md = col_character(),
                       loan_amount = col_double(),
                       interest_rate = col_character(),   # 可能是字符，后面转数值
                       loan_term = col_character(),
                       derived_loan_product_type = col_character(),
                       loan_purpose = col_character(),
                       occupancy_type = col_character(),
                       debt_to_income_ratio = col_character(),
                       loan_to_value_ratio = col_character(),
                       property_value = col_double(),
                       tract_to_msa_income_percentage = col_double()
                     ))

# Cleaning: Keep only necessary columns, convert interest_rate to numerical value
hmda <- hmda_raw %>%
  transmute(
    activity_year,
    msa = derived_msa_md,
    loan_amount = as.numeric(loan_amount),
    # Convert possible characters in interest_rate to numeric values
    interest_rate = readr::parse_number(interest_rate)
  ) %>%
  filter(!is.na(msa), !is.na(loan_amount), loan_amount > 0, !is.na(interest_rate))

# ==== choose exposure window ====
hmda_expo_base <- hmda %>%
  filter(activity_year >= expo_year_min, activity_year <= expo_year_max) %>%
  mutate(year_weight = 1)

if (!is.na(half_life_years) && is.finite(half_life_years)) {
  # Apply half-decay to each year in the window according to ref_year (the closer the ref_year, the greater the weight)
  hmda_expo_base <- hmda_expo_base %>%
    mutate(year_weight = 0.5 ^ ((ref_year - activity_year) / half_life_years))
}

# ==== Generate interest rate bins & calculate multi-bin exposure ====
hmda_binned <- hmda_expo_base %>%
  mutate(
    rate_bin = cut(interest_rate, breaks = bin_edges, labels = bin_labels, right = FALSE)
  ) %>%
  filter(!is.na(rate_bin))

# Use "amount x annual weight" as the weight
expo_df <- hmda_binned %>%
  mutate(
    rate_bin = cut(interest_rate, breaks = bin_edges, labels = bin_labels, right = FALSE)
  ) %>%
  filter(!is.na(rate_bin)) %>%
  group_by(msa, rate_bin) %>%
  summarise(amt_w = sum(loan_amount * year_weight, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(msa_total = sum(amt_w, na.rm = TRUE),
         w = ifelse(msa_total > 0, amt_w / msa_total, NA_real_)) %>%
  ungroup() %>%
  select(msa, rate_bin, w) %>%
  tidyr::pivot_wider(names_from = rate_bin, values_from = w, names_prefix = "w_") %>%
  # —— Fix 1：Fill missing buckets with 0
  mutate(across(starts_with("w_"), ~replace_na(., 0))) %>%
  # —— Fix 2：The row sum is normalized to 1 (to ensure the weight property)
  rowwise() %>%
  mutate(w_sum = sum(c_across(starts_with("w_")))) %>%
  ungroup() %>%
  mutate(across(starts_with("w_"), ~ ifelse(w_sum > 0, .x / w_sum, 0))) %>%
  select(-w_sum)

# ========= Unified cbsa_code column =========
expo_df <- expo_df %>%
  mutate(cbsa_code = stringr::str_pad(as.character(msa), 5, pad = "0"))

# ========= Compressed into a single Exposure index and decentralized =========
# Thinking: The more low-interest-rate assets (w_lt3, w_3to4), the easier it is to "lock in" → Exposure is given a positive weight.
# The more high-interest-rate assets (w_4to5, w_5to6, w_ge6), the less helpful/negative for locking in → Exposure is given a negative weight.
expo_df <- expo_df %>%
  mutate(across(starts_with("w_"), ~replace_na(., 0))) %>%
  rowwise() %>% mutate(w_sum = sum(c_across(starts_with("w_")))) %>% ungroup() %>%
  mutate(across(starts_with("w_"), ~ ifelse(w_sum > 0, .x / w_sum, 0))) %>%
  select(-w_sum)

# ========= Use PCA to compress into a single Exposure index =========
# 1) Take out the share matrix 
w_cols <- c("w_lt3", "w_3to4", "w_4to5", "w_5to6", "w_ge6")
W <- expo_df %>% select(all_of(w_cols)) %>% as.matrix()

# 2) Principal components (centering + standardization to avoid the impact of dimension/variance differences)
pca <- prcomp(W, center = TRUE, scale. = TRUE)

# 3) Take the first principal component as the original exposure index
Exposure_raw <- pca$x[, 1]

# 4) Unify the direction: Make "low interest rate share higher → exposure higher"
#    (w_lt3 - w_ge6) is used as the direction anchor. If the correlation is negative, flip the sign.
anchor <- (expo_df$w_lt3 - expo_df$w_ge6)
if (cor(Exposure_raw, anchor, use = "complete.obs") < 0) {
  Exposure_raw <- -Exposure_raw
}

# 5) Decentralization (compatible with time fixed effects; does not scale variance)
expo_df$Exposure <- as.numeric(scale(Exposure_raw, center = TRUE, scale = FALSE))


# ========= Align shock_data (monthly) and construct Z =========
# 1) Sort shock (keep month and NatShock_level)
shock_monthly <- shock_data %>%
  transmute(
    date = as.Date(date),
    ym   = format(date, "%Y-%m"),
    NatShock_level = NatShock_level
  ) %>%
  distinct(ym, .keep_all = TRUE) %>%
  arrange(ym)

# 2) Construct (MSA × Month) panel skeleton and merge Exposure and Shock
Z_panel <- tidyr::crossing(
  cbsa_code = unique(expo_df$cbsa_code),
  ym        = unique(shock_monthly$ym)
) %>%
  left_join(expo_df %>% select(cbsa_code, Exposure), by = "cbsa_code") %>%
  left_join(shock_monthly %>% select(ym, NatShock_level), by = "ym") %>%
  mutate(
    Z_bartik_level = Exposure * NatShock_level
  ) %>%
  arrange(cbsa_code, ym)

# （Optional）Make another "cumulative impact" version for comparison
shock_monthly_cum <- shock_monthly %>%
  mutate(CumShock = cumsum(coalesce(NatShock_level, 0)))

Z_panel <- Z_panel %>%
  left_join(shock_monthly_cum %>% select(ym, CumShock), by = "ym") %>%
  mutate(
    Z_bartik_cum = Exposure * CumShock
  )

# 3) Output IV
Z_out <- Z_panel %>%
  select(cbsa_code, ym, Z_bartik_level, Z_bartik_cum, Exposure, NatShock_level)

readr::write_csv(Z_out, "SS_IV.csv")
