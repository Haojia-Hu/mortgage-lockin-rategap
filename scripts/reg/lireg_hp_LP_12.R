install.packages("data.table")
install.packages("dplyr")
install.packages("stringr")
install.packages("purrr")
install.packages("sandwich")
install.packages("tibble")
install.packages("AER")
install.packages("fixest")
install.packages("broom")
install.packages("purrr")
install.packages("ggplot2")

library(data.table)
library(dplyr)
library(stringr)
library(purrr)
library(sandwich)
library(tibble)
library(AER)
library(fixest)
library(broom)
library(purrr)
library(ggplot2)

# ==== Path ====
base_dir  <- "C:/Users/huwen/Desktop/MRdata"
path_price <- file.path(base_dir, "Lockin proxy/zillow_zhvi.csv")   
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
    mutate(proxy_newlist = log(pmax(proxy_newlist, 1))) %>%   # log form
    ungroup()

# ==== 2) Read RateGap & IV & control variable ====
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

# ==== 4) Combined ====
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

#panel <- panel %>%
#    group_by(cbsa_code) %>%
#    mutate(
#        time_index = row_number()
#    ) %>% ungroup()

# detrend price growth
#detrend_mod <- feols(price_chg ~ cbsa_code*time_index, data = panel)
#panel$price_detrended <- residuals(detrend_mod)

#panel <- panel %>%
#    group_by(ym) %>%
#    mutate(national_growth = mean(price_chg, na.rm=TRUE)) %>%
#    ungroup() %>%
#    mutate(price_rel = price_chg - national_growth)

# ==== Horizon: 1–12, IRF ====
horizons <- 1:12

results_lp_period <- map_dfr(horizons, function(h) {
    
    # Dependent variable: Price change in period t+h (non-cumulative)
    panel_lp_period <- panel %>%
        group_by(cbsa_code) %>%
        arrange(ym) %>%
        mutate(y_h = dplyr::lead(price_chg, h)) %>%  # h=0: the current period's price_detrended
        ungroup()

    # IV Regression: rate_gap uses Z_bartik
    model <- feols(
        y_h ~ unemployment_rate + mig_rate_month + bps_total |
            cbsa_code + ym |
            rate_gap ~ Z_bartik,
        cluster = ~ cbsa_code,
        data = panel_lp_period
    )
    
    beta  <- coef(model)[["fit_rate_gap"]]
    se_rg <- se(model)[["fit_rate_gap"]]
    
    tibble(
        horizon  = h,
        coef     = beta,
        se       = se_rg,
        ci_lower = beta - 1.96 * se_rg,
        ci_upper = beta + 1.96 * se_rg
    )
})

print(results_lp_period)


ggplot(results_lp_period, aes(x = horizon, y = coef)) +
    geom_line(size = 1.2, color = "blue") +
    geom_point(size = 3, color = "black") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
    labs(
        title = "Dynamic Effect of Rate Gap on House Prices",
        subtitle = "Non-cumulative impulse response (month-by-month)",
        x = "No. of Months after shock",
        y = "Effect on price growth",
        caption = "Note: Shaded area represents 95% confidence interval.\nControls: unemployment rate, migration rate, building permits."
    ) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        axis.title = element_text(face = "bold")
    )


# ===== check check SE =====
results_lp_period <- results_lp_period %>%
    mutate(
        se_rel   = se / se[horizon == 1],              # 相对于 h=1 的 SE 比例
        d_se     = se - dplyr::lag(se),                # 相邻 horizon 的 SE 增量
        se_ratio = se / dplyr::lag(se)                 # 相邻 horizon 的 SE 比例
    )


results_lp_period %>%
    select(horizon, coef, se, se_rel, d_se, se_ratio) %>%
    print(n = Inf)
#(a) original SE changes with the horizon.
ggplot(results_lp_period, aes(x = horizon, y = se)) +
    geom_line() +
    geom_point() +
    labs(x = "Horizon (months)", y = "Standard error")
# (b) Relative to SE (based on h = 1)
ggplot(results_lp_period, aes(x = horizon, y = se_rel)) +
    geom_line() +
    geom_point() +
    labs(x = "Horizon (months)", y = "SE relative to h = 1")


