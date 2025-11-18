install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")
install.packages("zoo")
install.packages("stringr")

library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(zoo)
library(stringr)

# ====Input and output ====
dir_path <- "C:/Users/hh7473a/Desktop/pop"
in_file  <- file.path(dir_path, "cbsa_population_18_24.xlsx")  
out_file <- file.path(dir_path, "cbsa_migration_monthly.csv")

# The annual population is "July 1st estimate";
anchor_month <- 7

# ===== Read the inputs =====
pop_raw <- read_excel(in_file)
names(pop_raw) <- tolower(gsub("\\s+", "_", names(pop_raw)))

# Automatically identify the cbsa column and the name column
cbsa_col <- if ("cbsa_code" %in% names(pop_raw)) {
  "cbsa_code"
} else {
  # Find the cbsa_ column
  cand <- names(pop_raw)[grepl("^cbsa", names(pop_raw))]
  if (length(cand) == 0) stop("check title again")
  cand[1]
}

name_col <- if ("cbsa_title" %in% names(pop_raw)) "cbsa_title" else if ("area" %in% names(pop_raw)) "area" else NA

# ==== Cleaning: Remove rows without cbsa_code and keep the required columns ====
yrs <- 2018:2024
nm  <- names(pop_raw)

extract_year <- function(x) suppressWarnings(as.integer(str_extract(x, "20\\d{2}")))
nm_year <- sapply(nm, extract_year)

# choose target years
year_cols <- nm[!is.na(nm_year) & nm_year %in% yrs]
if (length(year_cols) == 0) stop("Double checks")


keep_cols <- c(cbsa_col, if (!is.na(name_col)) name_col, year_cols)
pop_clean <- pop_raw %>%
  dplyr::select(all_of(keep_cols)) %>%
  dplyr::filter(!is.na(.data[[cbsa_col]])) %>%
  dplyr::mutate(
    # Unified to 5 characters CBSA
    cbsa_code = sprintf("%05d", as.integer(.data[[cbsa_col]]))
  ) %>%
  # Delete those that cannot be converted to integers (indicating they are not legal CBSA)
  dplyr::filter(!is.na(as.integer(cbsa_code)))

# ==== Width to length (annual population) ====
pop_long <- pop_clean %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to  = "year_raw",
    values_to = "population"
  ) %>%
  mutate(
    year       = extract_year(year_raw),
    population = as.numeric(gsub(",", "", as.character(population)))
  ) %>%
  filter(!is.na(year), year %in% yrs) %>%
  select(cbsa_code, !!name_col, year, population)

# ==== Anchor the annual point value to July of the current year and interpolate to the complete monthly series ====
pop_yearmon <- pop_long %>%
  mutate(yearmon_anchor = as.yearmon(sprintf("%04d-%02d", year, anchor_month))) %>%
  select(cbsa_code, {{name_col}}, yearmon = yearmon_anchor, population)

# Generate a monthly series from 2018-01 to 2024-12, linearly interpolating the population
pop_monthly <- pop_yearmon %>%
  group_by(cbsa_code) %>%
  complete(
    yearmon = seq(as.yearmon("2018-01"), as.yearmon("2024-12"), by = 1/12),
    fill = list(population = NA_real_)
  ) %>%
  arrange(cbsa_code, yearmon) %>%
  mutate(
    population = na.approx(population, na.rm = FALSE)  # Linear interpolation by annual anchor point
  ) %>%
  ungroup() %>%
  mutate(yearmon = format(yearmon, "%Y-%m"))

# ==== Calculate the monthly net inflow volume/rate ====
pop_monthly <- pop_monthly %>%
  group_by(cbsa_code) %>%
  arrange(yearmon, .by_group = TRUE) %>%
  mutate(
    population_lag = lag(population, 1),
    net_mig_month  = population - population_lag,            # Monthly net inflow
    mig_rate_month = net_mig_month / population_lag          # Monthly net migration rate
  ) %>%
  ungroup() %>%
  select(cbsa_code, yearmon, population, net_mig_month, mig_rate_month)

head(pop_monthly, 12)

# ==== Result Output ====
write.csv(pop_monthly, out_file, row.names = FALSE)
cat("Output：", out_file, "\n")
