install.packages("data.table")
install.packages("dplyr")
install.packages("stringr")
install.packages("lubridate")
install.packages("fixest")
install.packages("tidyr")
install.packages("broom")
install.packages("ggplot2")
install.packages("seasonal")

library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(fixest)
library(tidyr)
library(broom)
library(ggplot2)
library(seasonal)

# Outstanding weighted mortgage rate
w_rate <- fread("C:/Users/huwen/Desktop/MRdata/MSA_outstanding_weighted_rate_monthly.csv") %>%
  mutate(
    cbsa_code = str_pad(as.character(derived_msa_md), 5, pad = "0"),
    ym = sprintf("%d-%02d", eval_year, eval_month)
  )

# baseline exposure: average outstanding mortgage rate in 2021
lock_exposure <- w_rate %>%
filter(eval_year == 2021) %>%
  group_by(cbsa_code) %>%
  summarise(
    baseline_rate = mean(weighted_rate, na.rm = TRUE)
  ) %>% 
  ungroup()

# Define HighLockedIn dummy: lowest 1/3 baseline_rate = highest lock-in exposure
cutoff <- quantile(lock_exposure$baseline_rate, probs = 1/3, na.rm = TRUE)

lock_exposure <- lock_exposure %>%
  mutate(
    HighLockedIn = ifelse(baseline_rate <= cutoff, 1, 0)
  )
head(lock_exposure)

# Read New Listing
newlist <- fread("C:/Users/huwen/Desktop/MRdata/Lockin proxy/zillow_newlisting.csv") %>%
  mutate(
    cbsa_code = str_pad(as.character(cbsa_code), 5, pad = "0"),
    ym = as.character(yearmon),
  ) %>%
  select(cbsa_code, ym, newlisting = proxy_value)

# Merge New Listing + Lock Exposure
panel_rd <- newlist %>%
  left_join(lock_exposure, by = "cbsa_code")
panel_rd <- panel_rd %>% drop_na(baseline_rate, HighLockedIn)

# Create a Post-Shock Dummy (after March 2022 = 1)
panel_rd <- panel_rd %>%
  mutate(
    ym_date = as.Date(paste0(ym, "-01")),
    Post = ifelse(ym_date >= as.Date("2022-03-01"), 1, 0)
  )

# ==== DID Regression：HighLockedIn × Post ====
# Use log (A very small constant can be added)
panel_rd <- panel_rd %>%
  mutate(
    ln_newlisting = log(newlisting)
  )

rd_res <- feols(
  ln_newlisting ~ HighLockedIn * Post | cbsa_code + ym,
  cluster = ~ cbsa_code,
  data = panel_rd
)

summary(rd_res)


# ==== Event-study ====
panel_es <- panel_rd %>%
  mutate(
    year  = as.integer(substr(ym, 1, 4)),
    month = as.integer(substr(ym, 6, 7)),
    rel_time = (year - 2022) * 12 + (month - 3)
  )

panel_es <- panel_es %>%
  filter(rel_time >= -12, rel_time <= 12)

event_res <- feols(
  ln_newlisting ~ i(rel_time, HighLockedIn, ref = -1) | cbsa_code + ym,
  cluster = ~ cbsa_code,
  data = panel_es
)

summary(event_res)

# ==== Extract event-study coefficients ====
est <- broom::tidy(event_res)
es_df <- est %>%
  filter(grepl("^rel_time::", term)) %>%   # 只保留 rel_time 项
  mutate(
    rel_time = as.numeric(gsub("rel_time::(-?[0-9]+):HighLockedIn", "\\1", term))
  )

ggplot(es_df, aes(x = rel_time, y = estimate)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = estimate - 1.96*std.error,
                  ymax = estimate + 1.96*std.error),
              alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Event Study: Differential Listing Response in High vs. Low Lock-In Markets",
    x = "Months relative to 2022-03",
    y = "Estimated Effect on Log New Listings"
  ) +
  theme_minimal()

# ==== Event study 2 ====
panel_es_sa <- panel_es %>%
    group_by(cbsa_code) %>%
    arrange(ym_date) %>%
    mutate(
        sa_listing = purrr::map_dbl(
            seq_along(cbsa_code),   
            function(i) NA          
        )
    ) %>% ungroup()

unique_cbsa <- unique(panel_es$cbsa_code)

for (cbsa in unique_cbsa) {
    
    tmp <- panel_es %>% 
        filter(cbsa_code == cbsa) %>%
        arrange(ym_date)
    
    # 如果数据太少 (<24个月)，跳过
    if (nrow(tmp) < 24) next
    
    # 设置 start 时间
    start_year  <- year(min(tmp$ym_date))
    start_month <- month(min(tmp$ym_date))
    
    # 构建 TS
    x <- ts(tmp$newlisting,
            start = c(start_year, start_month),
            frequency = 12)
    
    # seasonal adjustment
    sa_val <- tryCatch({
        final(seas(x))
    }, error = function(e) rep(NA, length(x)))
    
    # 写回主表
    panel_es_sa$sa_listing[panel_es_sa$cbsa_code == cbsa] <- sa_val
}

panel_es_sa <- panel_es_sa %>% mutate(
    ln_sa_listing = log(sa_listing)
)

event_res_sa <- feols(
    ln_sa_listing ~ i(rel_time, HighLockedIn, ref = -1) |
        cbsa_code + ym,
    cluster = ~ cbsa_code,
    data = panel_es_sa
)

summary(event_res_sa)

es_df_sa <- broom::tidy(event_res_sa) %>%
    filter(grepl("^rel_time::", term)) %>%
    mutate(
        rel_time = as.numeric(
            gsub("rel_time::(-?[0-9]+):HighLockedIn", "\\1", term)
        )
    )

ggplot(es_df_sa, aes(x = rel_time, y = estimate)) +
    geom_point() +
    geom_line() +
    geom_ribbon(
        aes(ymin = estimate - 1.96 * std.error,
            ymax = estimate + 1.96 * std.error),
        alpha = 0.2
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    labs(
        title = "Event Study: Seasonally Adjusted Listing Differences",
        x = "Months relative to 2022-03",
        y = "Effect on log(Listings / pre-shock same-month mean)"
    ) +
    theme_minimal()