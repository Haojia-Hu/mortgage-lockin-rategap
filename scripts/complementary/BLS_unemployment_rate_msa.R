install.packages("readr")
install.packages("dplyr")
install.packages("stringr")
install.packages("tidyr")
library(readr)
library(dplyr)
library(stringr)
library(tidyr)


setwd("C:/Users/hh7473a/Desktop/LAUS")
getwd()

laus_area    <- read_tsv("la.area.txt",    col_types = cols(.default = "c"))
laus_series  <- read_tsv("la.series.txt",  col_types = cols(.default = "c"))
laus_measure <- read_tsv("la.measure.txt", col_types = cols(.default = "c"))
laus_data    <- read_tsv("la.data.60.Metro.txt", col_types = cols(.default = "c"))

# Only keep MSA（area_type_code == 'B'），extract cbsa_code from area_code
laus_area_b <- laus_area %>%
  filter(area_type_code == "B") %>%
  mutate(
    cbsa_code = substr(area_code, 5, 9),        # extract number 5~9
    msa_name  = area_text
  ) %>%
  select(area_code, cbsa_code, msa_name)

# Only need to keep "unemployment rate" corresponding measure
measure_unemp <- laus_measure %>%
  filter(str_detect(tolower(measure_text), "unemployment rate")) %>%
  select(measure_code)

# series metadata: Limited to seasonal adjustment (S) and MSA (implemented by joining area_code later)
laus_series_sel <- laus_series %>%
  filter(seasonal == "U") %>%                    
  semi_join(measure_unemp, by = "measure_code") %>%
  select(series_id, seasonal, area_code, measure_code)


# Merge the data table with the metadata and region tables
dat_unemp <- laus_data %>%
  semi_join(laus_series_sel, by = "series_id") %>%
  left_join(laus_series_sel, by = "series_id") %>%
  left_join(laus_area_b,    by = "area_code") %>%
  # Only keep monthly M01~M12, interval 2018~2024
  filter(str_detect(period, "^M\\d{2}$"), period != "M13") %>%
  mutate(
    year  = as.integer(year),
    month = as.integer(str_remove(period, "M"))
  ) %>%
  filter(year >= 2018, year <= 2024) %>%
  # Numerical value (unemployment rate = a percentage value)
  mutate(value = suppressWarnings(as.numeric(value))) %>%
  filter(!is.na(value)) %>%
  mutate(
    yearmon = sprintf("%04d-%02d", year, month)
  ) %>%
  select(yearmon, cbsa_code, msa_name, unemployment_rate = value) %>%
  arrange(yearmon, cbsa_code)

# Check: Are there any records with missing cbsa_code?
chk_missing_cbsa <- dat_unemp %>% filter(is.na(cbsa_code) | cbsa_code == "")
if (nrow(chk_missing_cbsa) > 0) {
  message("There are rows where cbsa_code cannot be extracted from area_code. It is recommended to check：")
  print(head(chk_missing_cbsa, 10))
}

install.packages("lubridate")
library(lubridate)
library(fixest)

dat_unemp_fe <- dat_unemp %>%
  mutate(date  = as.Date(paste0(yearmon, "-01")),
         month = month(date, label = TRUE, abbr = TRUE))  # Jan, Feb, ...

# Preview results
print(head(dat_unemp_fe, 12))

write.csv(dat_unemp, "laus_unemployment_msa.csv", row.names = FALSE)
