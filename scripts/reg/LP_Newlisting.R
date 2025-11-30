install.packages("dplyr")
install.packages("tidyr")
install.packages("purrr")
install.packages("fixest")
install.packages("ggplot2")
install.packages("stringr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("zoo")
install.packages("tibble")

library(dplyr)
library(tidyr)
library(purrr)
library(fixest)
library(ggplot2)
library(stringr)
library(tidyverse)
library(lubridate)
library(zoo)
library(tibble)

path_rategap <- "C:/Users/huwen/Desktop/MRdata/panel_rategap_hat.csv"
path_newlist <- "C:/Users/huwen/Desktop/MRdata/Lockin proxy/zillow_newlisting.csv"

rategap <- read_csv(path_rategap) %>%
    mutate(
        ym = as.yearmon(ym),         
        ym = as.Date(ym)             
    )

newlisting <- read_csv(path_newlist) %>%
    rename(new_listing = proxy_value) %>%
    mutate(
        ym = as.yearmon(yearmon),
        ym = as.Date(ym)
    ) %>%
    select(cbsa_code, ym, new_listing)

panel_lp <- rategap %>%
    left_join(newlisting, by = c("cbsa_code", "ym"))

# ==== h = 0:12 ====
panel_lp <- panel_lp %>%
    group_by(cbsa_code) %>%
    arrange(ym) %>%
    mutate(
        newlist_h0 = new_listing
    ) %>%
    ungroup()

for (h in 1:12) {
    panel_lp <- panel_lp %>%
        group_by(cbsa_code) %>%
        arrange(ym) %>%
        mutate(
            !!paste0("newlist_h", h) := lead(new_listing, h)
        ) %>%
        ungroup()
}

# ==== LP-IV ==== 
iv_lp_newlisting <- function(h, df) {
    
    y_var <- paste0("newlist_h", h)
    
    # newlist_hh ~ 1 | FE | rate_gap ~ Z_bartik
    fml <- as.formula(
        paste0(y_var, " ~ 1 | cbsa_code + ym | rate_gap ~ Z_bartik")
    )
    
    res <- feols(
        fml,
        data = df,
        cluster = ~ cbsa_code
    )
    
    tibble(
        horizon = h,
        beta = coef(res)[1],
        se = se(res)[1],
        N = nobs(res)
    )
}

horizons <- 1:12

results_lp_newlisting <- map_dfr(
    horizons,
    ~ iv_lp_newlisting(.x, panel_lp)
)

print(results_lp_newlisting)

# ==== Figure out ====
results_lp_newlisting %>%
    ggplot(aes(x = horizon, y = beta)) +
    geom_line(size = 1.2, color = "steelblue") +
    geom_point(size = 3, color = "steelblue") +
    geom_ribbon(aes(ymin = beta - 1.96 * se,
                    ymax = beta + 1.96 * se),
                alpha = 0.2, fill = "steelblue") +
    labs(
        title = "Dynamic Response of New Listings to the Mortgage Rate Gap",
        x = "Horizon (months)",
        y = "Effect on new listings"
    ) +
    theme_minimal()