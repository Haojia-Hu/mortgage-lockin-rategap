install.packages("data.table")
install.packages("dplyr")
install.packages("stringr")
install.packages("purrr")
install.packages("sandwich")
install.packages("tibble")
install.packages("AER")
install.packages("fixest")
install.packages("broom")
install.packages("mFilter")
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
library(mFilter)
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
price_raw <- fread(path_price) %>%
    transmute(
        cbsa_code = str_pad(as.character(cbsa_code), 5, pad = "0"),
        ym        = substr(as.character(yearmon), 1, 7),
        price     = as.numeric(proxy_value)
    )

# HP filter: applied individually to each CBSA
price <- price_raw %>%
    arrange(cbsa_code, ym) %>%
    group_by(cbsa_code) %>%
    group_modify(~ {
        x <- .
        # 1) HP filter on log(price)
        hp_res <- mFilter::hpfilter(log(x$price), freq = 129600, type = "lambda")
        
        cycle  <- as.numeric(hp_res$cycle)
        
        # 2) Take the first difference of the cycle and multiply by 100 to get the "percentage point".
        price_chg_cycle <- c(NA, diff(cycle)) * 100
        
        x %>%
            mutate(
                price_cycle      = cycle,
                price_chg_cycle  = price_chg_cycle
            )
    }) %>%
    ungroup() %>%
    # Keep only the variables you need
    select(cbsa_code, ym, price_chg_cycle)

# ==== 4) Combined dataset ====
panel <- panel_core %>%
    left_join(newl,  by = c("cbsa_code","ym")) %>%
    left_join(price, by = c("cbsa_code","ym")) %>%
    filter(!is.na(rate_gap),
           !is.na(Z_bartik),
           !is.na(proxy_newlist),
           !is.na(price_chg_cycle),
           !is.na(unemployment_rate),
           !is.na(mig_rate_month),
           !is.na(bps_total))

# Shorter time horizon
horizons <- 1:6

results_lp_cycle <- map_dfr(horizons, function(h) {
    
    panel_lp_period <- panel %>%
        group_by(cbsa_code) %>%
        arrange(ym) %>%
        mutate(y_h = lead(price_chg_cycle, h)) %>%  # Using the changes of cycle
        ungroup()
    
    model <- feols(
        y_h ~ unemployment_rate + mig_rate_month + bps_total |
            cbsa_code + ym |
            rate_gap ~ Z_bartik,
        cluster = ~ cbsa_code,
        data    = panel_lp_period
    )
    
    beta  <- coef(model)["fit_rate_gap"]
    se_rg <- se(model)["fit_rate_gap"]
    
    tibble(
        horizon  = h,
        coef     = beta,
        se       = se_rg,
        ci_lower = beta - 1.96 * se_rg,
        ci_upper = beta + 1.96 * se_rg
    )
})

print(results_lp_cycle)

ggplot(results_lp_cycle, aes(x = horizon, y = coef)) +
    geom_line(size = 1.2, color = "steelblue") +
    geom_point(size = 3, color = "steelblue") +
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



unstable_range <- c(6, 10)

ggplot(results_lp_period, aes(x = horizon, y = coef)) +
    # === Shading marks unstable regions ===
    annotate("rect",
             xmin = unstable_range[1], xmax = unstable_range[2],
             ymin = -Inf, ymax = Inf,
             alpha = 0.1, fill = "red") +
    
    geom_line(size = 1.2, color = "black") +
    geom_point(size = 3, color = "black") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                alpha = 0.2, fill = "gray60") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
    labs(
        title = "Dynamic Effect of Rate Gap on House Prices",
        subtitle = "Non-cumulative impulse response (month-by-month)",
        x = "No. of Months after shock",
        y = "Effect on price growth"
    ) +
    theme_minimal(base_size = 12)
