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
path_price <- file.path(base_dir, "Lockin proxy/zillow_zhvi.csv")   # 价格文件
path_panel <- file.path(base_dir, "Panel_rategap_hat.csv")
path_newlist <- file.path(base_dir, "Lockin proxy/zillow_newlisting.csv")
path_emp   <- file.path(base_dir, "QCEW_cbsa_totalemp.csv")
path_ZD_iv <- file.path(base_dir, "Z_D_lockin.csv")
path_saiz  <- file.path(base_dir, "saiz2010/saiz2010_cbsa.csv")

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

# ==== 4) Read the employment data ====
employment <- fread(path_emp) %>%
    mutate(
        cbsa_code = str_pad(as.character(cbsa_code), 5, pad = "0"),
        ym        = substr(as.character(ym), 1, 7),
        employment_total = as.numeric(emp_total)
    ) %>%
    group_by(cbsa_code) %>%
    arrange(ym) %>%
    mutate(employment_growth = (employment_total - lag(employment_total)) / lag(employment_total) * 100) %>%
    ungroup()

# ==== 5) Read the Bartik-iv for demand side(employment) ==== 
Z_D <- fread(path_ZD_iv) %>%
    mutate(
        cbsa_code = str_pad(as.character(cbsa_code), 5, pad = "0"),
        ym        = substr(as.character(ym), 1, 7),
        Z_D = as.numeric(Z_D)
    ) %>%
    group_by(cbsa_code) %>%
    arrange(ym) %>%
    ungroup()

# ==== 6) Saiz2010 Elasticity (use 1/elasticity) ====
saiz <- fread(path_saiz) %>%
    mutate(
        cbsa_code  = str_pad(as.character(cbsa_code), 5, pad = "0"),
        elasticity = as.numeric(elasticity),
        inv_elast  = 1 / elasticity
    ) %>%
    select(cbsa_code, elasticity, inv_elast)

saiz <- saiz %>%
    distinct(cbsa_code, .keep_all = TRUE)

# ==== 7) Combined dataset ====
panel <- panel_core %>%
    left_join(newl,  by = c("cbsa_code","ym")) %>%
    left_join(price, by = c("cbsa_code","ym")) %>%
    left_join(employment, by = c("cbsa_code","ym"))%>%
    left_join(Z_D, by = c("cbsa_code","ym"))%>%
    left_join(saiz, by = c("cbsa_code"))%>%
    filter(!is.na(rate_gap),
           !is.na(Z_bartik),
           !is.na(proxy_newlist),
           !is.na(price_chg_cycle),
           !is.na(unemployment_rate),
           !is.na(mig_rate_month),
           !is.na(bps_total),
           !is.na(employment_growth),
           !is.na(Z_D))

# ==== 8) Grouped by employment growth ====
# Calculate average employment growth at the CBSA level.
panel <- panel %>%
    group_by(cbsa_code) %>%
    mutate(avg_emp_growth = mean(employment_growth, na.rm = TRUE)) %>%
    ungroup()

median_emp <- median(panel$avg_emp_growth, na.rm = TRUE)

# Grouping 
panel_high_emp <- panel %>% filter(avg_emp_growth > median_emp)
panel_low_emp <- panel %>% filter(avg_emp_growth <= median_emp)

# ==== High employment growth regions ====
model_high_static <- feols(
    price_chg_cycle ~ mig_rate_month + bps_total | cbsa_code + ym |
        rate_gap ~ Z_bartik,
    cluster = ~ cbsa_code,
    data = panel_high_emp
)

# ==== Low employment growth regions ====
model_low_static <- feols(
    price_chg_cycle ~ mig_rate_month + bps_total | cbsa_code + ym |
        rate_gap ~ Z_bartik,
    cluster = ~ cbsa_code,
    data = panel_low_emp
)
# Report two coefficients
tibble(
    group = c("High Employment Growth", "Low Employment Growth"),
    coef = c(coef(model_high_static)["fit_rate_gap"], 
             coef(model_low_static)["fit_rate_gap"]),
    se = c(se(model_high_static)["fit_rate_gap"], 
           se(model_low_static)["fit_rate_gap"])
)


horizons <- 1:6


# Function: Constructs LP variables for given data
create_lp_data <- function(data) {
    data_lp <- data
    
    # Manually create variables for each horizon.
    for (h in 1:12) {
        data_lp <- data_lp %>%
            group_by(cbsa_code) %>%
            arrange(ym) %>%
            mutate(
                !!paste0("price_chg_h", h) := lead(price_chg_cycle, h)  
            ) %>%
            ungroup()
    }
    
    return(data_lp)
}

panel_lp_high <- create_lp_data(panel_high_emp)
panel_lp_low <- create_lp_data(panel_low_emp)


# LP-IV estimate (high employment growth)
results_lp_high <- map_dfr(horizons, function(h) {
    # don't consider the h=0
    yvar <- paste0("price_chg_h", h)
    
    model <- feols(
        as.formula(paste0(
            yvar,
            " ~ mig_rate_month + bps_total | cbsa_code + ym | rate_gap ~ Z_bartik"
        )),
        cluster = ~ cbsa_code,
        data = panel_lp_high
    )
    
    
    tibble(
        group   = "High Employment Growth",
        horizon = h,
        coef    = coef(model)["fit_rate_gap"],
        se      = se(model)["fit_rate_gap"],
        n_obs   = nobs(model)
    )
})

# LP-IV estimate (low employment growth)
results_lp_low <- map_dfr(horizons, function(h) {
    yvar <- paste0("price_chg_h", h)   
    
    model <- feols(
        as.formula(paste0(
            yvar,
            " ~ mig_rate_month + bps_total | cbsa_code + ym | rate_gap ~ Z_bartik"
        )),
        cluster = ~ cbsa_code,
        data = panel_lp_low
    )
    
    tibble(
        group   = "Low Employment Growth",
        horizon = h,
        coef    = coef(model)["fit_rate_gap"],   
        se      = se(model)["fit_rate_gap"],
        n_obs   = nobs(model)
    )
})

# Combined results
results_lp_hetero <- bind_rows(results_lp_high, results_lp_low) %>%
    mutate(
        ci_lower = coef - 1.96 * se,
        ci_upper = coef + 1.96 * se
    )

print(results_lp_hetero)

# Figure out
ggplot(results_lp_hetero, aes(x = horizon, y = coef, color = group, fill = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
    scale_color_manual(values = c("High Employment Growth" = "steelblue", 
                                  "Low Employment Growth" = "coral")) +
    scale_fill_manual(values = c("High Employment Growth" = "steelblue", 
                                 "Low Employment Growth" = "coral")) +
    labs(
        title = "Dynamic Effect of Rate Gap on House Prices",
        subtitle = "Heterogeneity by Local Employment",
        x = "No. of Months after shock",
        y = "Effect on price growth (%)",   # 小建议：这里注明是 cyclical
        color = "",
        fill = ""
    ) +
    theme_minimal(base_size = 13) +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold")
    )


# ==== 9) Grouped by supply elasticity ====
median_elast <- median(panel$elasticity, na.rm = TRUE)

panel_high_elast <- panel %>% filter(elasticity > median_elast)  # 供给弹性高
panel_low_elast  <- panel %>% filter(elasticity <= median_elast) # 供给弹性低

# ==== Static IV (Quick comparison, retained) ====
model_high_elast_static <- feols(
    price_chg_cycle ~ unemployment_rate + mig_rate_month + bps_total | cbsa_code + ym |
        rate_gap ~ Z_bartik,
    cluster = ~ cbsa_code,
    data = panel_high_elast
)

model_low_elast_static <- feols(
    price_chg_cycle ~ unemployment_rate + mig_rate_month + bps_total | cbsa_code + ym |
        rate_gap ~ Z_bartik,
    cluster = ~ cbsa_code,
    data = panel_low_elast
)

static_elasticity <- tibble(
    group = c("High Supply Elasticity", "Low Supply Elasticity"),
    coef  = c(coef(model_high_elast_static)["fit_rate_gap"], 
              coef(model_low_elast_static)["fit_rate_gap"]),
    se    = c(se(model_high_elast_static)["fit_rate_gap"], 
              se(model_low_elast_static)["fit_rate_gap"])
)

print("=== Static Heterogeneity by Supply Elasticity ===")
print(static_elasticity)

# ==== Construct panel_lp (using price_chg_cycle after HP filter, h = 1:6) ====

# Encapsulate a function, trying to use your previous syntax as much as possible.
create_lp_data <- function(data, max_h = 6) {
    data_lp <- data
    for (h in 1:max_h) {
        data_lp <- data_lp %>%
            group_by(cbsa_code) %>%
            arrange(ym) %>%
            mutate(!!paste0("price_chg_h", h) := lead(price_chg_cycle, h)) %>%
            ungroup()
    }
    data_lp
}

panel_lp_high_elast <- create_lp_data(panel_high_elast, max_h = 6)
panel_lp_low_elast  <- create_lp_data(panel_low_elast,  max_h = 6)

# only use 6 month for IRF
horizons <- 1:6

# ==== LP-IV：High supply elasticity ====
results_lp_high_elast <- map_dfr(horizons, function(h) {
    yvar <- paste0("price_chg_h", h)
    
    model <- feols(
        as.formula(paste0(
            yvar,
            " ~ unemployment_rate + mig_rate_month + bps_total | cbsa_code + ym | rate_gap ~ Z_bartik"
        )),
        cluster = ~ cbsa_code,
        data = panel_lp_high_elast
    )
    
    tibble(
        group   = "High Supply Elasticity",
        horizon = h,
        coef    = coef(model)["fit_rate_gap"],
        se      = se(model)["fit_rate_gap"],
        n_obs   = nobs(model)
    )
})

# ==== LP-IV：Low supply elasticity ====
results_lp_low_elast <- map_dfr(horizons, function(h) {
    yvar <- paste0("price_chg_h", h)
    
    model <- feols(
        as.formula(paste0(
            yvar,
            " ~ unemployment_rate + mig_rate_month + bps_total | cbsa_code + ym | rate_gap ~ Z_bartik"
        )),
        cluster = ~ cbsa_code,
        data = panel_lp_low_elast
    )
    
    tibble(
        group   = "Low Supply Elasticity",
        horizon = h,
        coef    = coef(model)["fit_rate_gap"],
        se      = se(model)["fit_rate_gap"],
        n_obs   = nobs(model)
    )
})

#  Combined results
results_lp_elasticity <- bind_rows(results_lp_high_elast, results_lp_low_elast) %>%
    mutate(
        ci_lower = coef - 1.96 * se,
        ci_upper = coef + 1.96 * se
    )

print("=== LP-IV by Supply Elasticity (1–6 months) ===")
print(results_lp_elasticity)

# ==== Plotting: x-axis 1–6 months ====
ggplot(results_lp_elasticity, aes(x = horizon, y = coef, color = group, fill = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.8) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
    scale_color_manual(values = c("High Supply Elasticity" = "steelblue", 
                                  "Low Supply Elasticity" = "coral")) +
    scale_fill_manual(values = c("High Supply Elasticity" = "steelblue", 
                                 "Low Supply Elasticity" = "coral")) +
    scale_x_continuous(breaks = 1:6, limits = c(1, 6)) +
    labs(
        title   = "Dynamic Effect of Rate Gap on House Prices",
        subtitle = "Heterogeneity by Supply Elasticity (Saiz 2010)",
        x = "No. of Months after shock",
        y = "Effect on price growth (%)",
        color = "",
        fill  = "",
        caption = "High elasticity: easier to build new housing. Low elasticity: supply is more constrained."
    ) +
    theme_minimal(base_size = 13) +
    theme(
        legend.position   = "bottom",
        plot.title        = element_text(face = "bold", size = 15),
        panel.grid.minor  = element_blank()
    )
