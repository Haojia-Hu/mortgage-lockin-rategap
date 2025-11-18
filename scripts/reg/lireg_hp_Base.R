install.packages("data.table")
install.packages("dplyr")
install.packages("stringr")
install.packages("purrr")
install.packages("sandwich")
install.packages("tibble")
install.packages("AER")
install.packages("fixest")
install.packages("broom")

library(data.table)
library(dplyr)
library(stringr)
library(purrr)
library(sandwich)
library(tibble)
library(AER)
library(fixest)
library(broom)

# ==== Path ====
base_dir  <- "C:/Users/huwen/Desktop/MRdata"
path_price <- file.path(base_dir, "Lockin proxy/zillow_zhvi.csv")   # 价格文件
path_panel <- file.path(base_dir, "Panel_rategap_hat.csv")
path_newlist <- file.path(base_dir, "Lockin proxy/zillow_newlisting.csv")

# ==== 1) Read New Listings (Lock-in Proxy) ====
newl <- fread(path_newlist) %>%
    transmute(
        cbsa_code = str_pad(as.character(cbsa_code), 5, pad = "0"),
        ym        = substr(as.character(yearmon), 1, 7),
        proxy_newlist = as.numeric(proxy_value)
    ) %>%
    distinct(cbsa_code, ym, .keep_all = TRUE) %>%
    group_by(cbsa_code) %>%
    mutate(proxy_newlist = log(pmax(proxy_newlist, 1))) %>%   # log 形式
    ungroup()

# ==== 2) Read RateGap & IV & control variables ====
panel_core <- fread(path_panel) %>%
    mutate(
        cbsa_code = str_pad(as.character(cbsa_code), 5, pad = "0"),
        ym        = as.character(ym)
    )

# ==== 3) Read price data & build price_chg ====
price_raw <- fread(path_price)
price <- price_raw %>%
    transmute(
        cbsa_code = str_pad(as.character(cbsa_code), 5, pad = "0"),
        ym        = substr(as.character(yearmon), 1, 7),
        price     = as.numeric(proxy_value)
    ) %>%
    arrange(cbsa_code, ym) %>%
    group_by(cbsa_code) %>%
    mutate(price_chg = 100 * (log(price) - log(dplyr::lag(price)))) %>%
    ungroup() %>%
    select(cbsa_code, ym, price_chg)

# ==== 4) Combined dataset ====
panel <- panel_core %>%
    left_join(newl,  by = c("cbsa_code","ym")) %>%
    left_join(price, by = c("cbsa_code","ym")) %>%
    filter(!is.na(rate_gap),
           !is.na(Z_bartik),
           !is.na(proxy_newlist),
           !is.na(price_chg),
           !is.na(unemployment_rate),
           !is.na(mig_rate_month),
           !is.na(bps_total))

# ==== 5) 2sls regression (Confirm whether the total effect is significant) ====
baseline <- feols(
    price_chg ~ unemployment_rate + mig_rate_month + bps_total | 
        cbsa_code + ym |
        rate_gap ~ Z_bartik,
    cluster = ~ cbsa_code,
    data = panel
)
summary(baseline, stage = 1:2)
summary(baseline)


# ==== (1) Adding lagged dependent variables (semi-dynamic)
panel_lag <- panel %>%
    group_by(cbsa_code) %>%
    arrange(ym) %>%
    mutate(price_chg_lag = lag(price_chg)) %>%
    ungroup() %>%
    filter(!is.na(price_chg_lag))

baseline_dynamic <- feols(
    price_chg ~ price_chg_lag + unemployment_rate + mig_rate_month | 
        cbsa_code + ym |
        rate_gap ~ Z_bartik,
    cluster = ~ cbsa_code,
    data = panel_lag
)
summary(baseline_dynamic, stage = 1:2)

# ==== (2) Remove extremes by CBSA (rate_gap)
panel_robust <- panel %>%
    group_by(cbsa_code) %>%
    filter(
        rate_gap > quantile(rate_gap, 0.05) & 
            rate_gap < quantile(rate_gap, 0.95)
    ) %>%
    ungroup()

baseline_robust <- feols(
    price_chg ~ unemployment_rate + mig_rate_month + bps_total | 
        cbsa_code + ym |
        rate_gap ~ Z_bartik,
    cluster = ~ cbsa_code,
    data = panel_robust
)
summary(baseline_robust)

# ==== (3) High vs. Low rate_gap regions
panel_high <- panel %>% filter(rate_gap > median(rate_gap, na.rm=TRUE))
panel_low  <- panel %>% filter(rate_gap <= median(rate_gap, na.rm=TRUE))

# Regression respectively
baseline_high <- feols(
    price_chg ~ unemployment_rate + mig_rate_month + bps_total | 
        cbsa_code + ym |
        rate_gap ~ Z_bartik,
    cluster = ~ cbsa_code,
    data = panel_high
)
summary(baseline_high)

baseline_low  <- feols(
    price_chg ~ unemployment_rate + mig_rate_month + bps_total | 
        cbsa_code + ym |
        rate_gap ~ Z_bartik,
    cluster = ~ cbsa_code,
    data = panel_low
)
summary(baseline_low)

# ==== 2018-2022 (during the pandemic) vs 2023-2024 (during the interest rate hike period)
panel_covid <- panel %>% filter(ym >= "2018-08" & ym <= "2022-03")
panel_hike  <- panel %>% filter(ym >= "2022-03")

baseline_covid <- feols(
    price_chg ~ unemployment_rate + mig_rate_month + bps_total | 
        cbsa_code + ym |
        rate_gap ~ Z_bartik,
    cluster = ~ cbsa_code,
    data = panel_covid
)
summary(baseline_covid)

baseline_hike <- feols(
    price_chg ~ unemployment_rate + mig_rate_month + bps_total | 
        cbsa_code + ym |
        rate_gap ~ Z_bartik,
    cluster = ~ cbsa_code,
    data = panel_hike
)
summary(baseline_hike)

