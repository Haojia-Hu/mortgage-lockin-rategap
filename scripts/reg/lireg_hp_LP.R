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

# ==== 4) Combined dataset  ====
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




# ==== Local Projection - IV ====
# Complete horizon cumulative price change
panel_lp <- panel %>%
    group_by(cbsa_code) %>%
    arrange(ym) %>%
    mutate(
        across(
            .cols = starts_with("price_chg"),
            .fns = list(h0 = ~.),
            .names = "{.col}"
        )
    ) %>%
    ungroup()

# Create cumulative variables for h = 1 to 12
for (h in 1:24) {
    panel_lp <- panel_lp %>%
        group_by(cbsa_code) %>%
        arrange(ym) %>%
        mutate(
            !!paste0("price_chg_h", h) := rowSums(
                sapply(0:(h-1), function(k) lead(price_chg, k)), 
                na.rm = FALSE  # 保留 NA，避免不完整的累积
            )
        ) %>%
        ungroup()
}

# Use 24 months
horizons <- c(0, 1, 3, 6, 9, 12, 18, 24)


# Estimate the price change over period t+h
results_lp_period <- map_dfr(c(0, 1, 3, 6, 9, 12, 18, 24), function(h) {
    
    # Dependent variable: Price change in period h
    panel_lp_period <- panel %>%
        group_by(cbsa_code) %>%
        arrange(ym) %>%
        mutate(y_h = lead(price_chg, h)) %>%
        ungroup()
    
    # IV regression
    model <- feols(
        y_h ~ unemployment_rate + mig_rate_month + bps_total |
            cbsa_code + ym |
            rate_gap ~ Z_bartik,
        cluster = ~ cbsa_code,
        data = panel_lp_period
    )
    
    tibble(
        horizon = h,
        coef = coef(model)["fit_rate_gap"],
        se = se(model)["fit_rate_gap"],
        ci_lower = coef - 1.96 * se,
        ci_upper = coef + 1.96 * se
    )
})

print(results_lp_period)


# ==== Drawing IRF plots ====
results_lp_period <- results_lp_period %>%
    mutate(significant = ifelse(ci_lower * ci_upper > 0, "Significant", "Not Significant"))

#ggplot(results_lp_period, aes(x = horizon, y = coef)) +
#    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.8) +
#    geom_line(size = 1.5, color = "steelblue") +
#    geom_point(aes(color = significant), size = 3) +
#    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, size = 0.8) +
#    scale_color_manual(values = c("Significant" = "red", "Not Significant" = "gray60")) +
#    annotate("rect", xmin = 8, xmax = 19, ymin = -Inf, ymax = Inf, 
#             alpha = 0.1, fill = "green") +
#    annotate("text", x = 13.5, y = 8, label = "Supply Effect Dominates", 
#             size = 4, color = "darkgreen", fontface = "bold") +
#    labs(
#        title = "Dynamic Effect of Rate Gap on House Price",
#        subtitle = "Non-cumulative impulse response (month-by-month)",
#        x = "Months after shock",
#        y = "Effect on price growth (%)",
#        color = "",
#        caption = "Note: Error bars represent 95% confidence intervals.\nShaded area highlights period of significant positive effects (h=9-18)."
#    ) +
#    theme_minimal(base_size = 13) +
#    theme(
#        plot.title = element_text(face = "bold", size = 15),
#        legend.position = "bottom",
#        panel.grid.minor = element_blank()
#    )

# ggsave("LP_IV_Period_Effects.png", width = 11, height = 7, dpi = 300)


ggplot(results_lp_period, aes(x = horizon, y = coef)) +
    geom_line(size = 1.2, color = "blue") +
    geom_point(size = 3, color = "blue") +
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

# 保存图表
# ggsave("LP_IV_RateGap_HousePrice.png", width = 10, height = 6, dpi = 300)


