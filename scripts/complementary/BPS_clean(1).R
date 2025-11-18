install.packages("readxl")
install.packages("dplyr")
install.packages("purrr")
install.packages("stringr")

library(readxl)
library(dplyr)
library(stringr)
library(purrr)

# Unification of head title
clean_names <- function(x) {
  x <- trimws(as.character(x))
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)  
  x <- gsub("^_|_$", "", x)
  x
}

# YYYYMM -> YYYY-MM
ym_label <- function(ym) paste0(substr(ym, 1, 4), "-", substr(ym, 5, 6))

read_one <- function(file) {
  ym <- str_extract(basename(file), "[0-9]{6}")
  
  # 1) read the sheet with out head title
  raw <- readxl::read_excel(file, col_names = FALSE)
  
  # 2) Auto find the “column titles”
  header_row <- which(
    apply(raw, 1, function(r) any(str_detect(toupper(as.character(r)), "^CBSA(\\b|\\s|_)?")))
  )[1]
  
  if (is.na(header_row)) {
    stop("can't find the column title（no rows includes 'CBSA'）：", file)
  }
  
  # 3) Set the row of column names，and the next row are data
  header <- as.character(unlist(raw[header_row, ]))
  nm     <- clean_names(header)
  
  dat <- raw[(header_row + 1):nrow(raw), , drop = FALSE]
  names(dat) <- nm
  
  # 4) Find the CBSA column（includes cbsa / cbsa_code）
  cbsa_candidates <- which(nm %in% c("cbsa", "cbsa_code"))
  if (length(cbsa_candidates) == 0) {
    stop("can't find the column named CBSA （includes cbsa/cbsa_code）：", file)
  }
  cbsa_col <- cbsa_candidates[1]
  
  # 5) Find the “Current Month: Total”column：from left to right to check the column title includes 'total'
  total_candidates <- grep("(^|_)total($|_)", nm, ignore.case = TRUE)
  if (length(total_candidates) == 0) {
    stop("Can't find the column named 'total'：", file)
  }
  current_total_col <- total_candidates[1]  # 第一个 total 列（通常就是 Current Month → Total）

  # 6) Only keep yearmon / CBSA / total 三列，并做类型清理
  out <- tibble(
    yearmon = ym_label(ym),
    cbsa    = trimws(as.character(dat[[cbsa_col]])),
    total   = suppressWarnings(as.numeric(dat[[current_total_col]]))
  ) %>%
    filter(!is.na(cbsa), cbsa != "", !is.na(total))
  
  out
}

# ======== Read 2024 ========
dir_bps_2024 <- "C:/Users/hh7473a/Desktop/BPS/2024"   
files_2024 <- list.files(dir_bps_2024, pattern = "^cbsamonthly_[0-9]{6}\\.xls$", full.names = TRUE)

cbsa_2024 <- purrr::map_dfr(files_2024, read_one) %>%
  arrange(yearmon, cbsa) %>%
  select(yearmon, cbsa, total)

print(head(cbsa_2024, 12))
write.csv(cbsa_2024, "2024_bps_monthly.csv", row.names = FALSE)

# ======== Read 2023 ========
dir_bps_2023 <- "C:/Users/hh7473a/Desktop/BPS/2023"
files_2023 <- list.files(dir_bps_2023, pattern = "^msamonthly_[0-9]{6}\\.xls$", full.names = TRUE)

cbsa_2023 <- purrr::map_dfr(files_2023, read_one) %>%
  arrange(yearmon, cbsa) %>%
  select(yearmon, cbsa, total)

print(head(cbsa_2023, 12))
write.csv(cbsa_2023, "2023_bps_monthly.csv", row.names = FALSE)


# ======== Read 2022 ========
dir_bps_2022 <- "C:/Users/hh7473a/Desktop/BPS/2022"
files_2022 <- list.files(dir_bps_2022, pattern = "^msamonthly_[0-9]{6}\\.xls$", full.names = TRUE)

cbsa_2022 <- purrr::map_dfr(files_2022, read_one) %>%
  arrange(yearmon, cbsa) %>%
  select(yearmon, cbsa, total)

print(head(cbsa_2022, 12))
write.csv(cbsa_2022, "2022_bps_monthly.csv", row.names = FALSE)

# ======== Read 2021 ========
dir_bps_2021 <- "C:/Users/hh7473a/Desktop/BPS/2021"
files_2021 <- list.files(dir_bps_2021, pattern = "^msamonthly_[0-9]{6}\\.xls$", full.names = TRUE)

cbsa_2021 <- purrr::map_dfr(files_2021, read_one) %>%
  arrange(yearmon, cbsa) %>%
  select(yearmon, cbsa, total)

print(head(cbsa_2021, 12))
write.csv(cbsa_2021, "2021_bps_monthly.csv", row.names = FALSE)

# ======== Read 2020 ========
dir_bps_2020 <- "C:/Users/hh7473a/Desktop/BPS/2020"
files_2020 <- list.files(dir_bps_2020, pattern = "^msamonthly_[0-9]{6}\\.xls$", full.names = TRUE)

cbsa_2020 <- purrr::map_dfr(files_2020, read_one) %>%
  arrange(yearmon, cbsa) %>%
  select(yearmon, cbsa, total)

print(head(cbsa_2020, 12))
write.csv(cbsa_2020, "2020_bps_monthly.csv", row.names = FALSE)

# ======== Read 2019 ========
dir_bps_2019 <- "C:/Users/hh7473a/Desktop/BPS/2019"
files_2019 <- list.files(dir_bps_2019, pattern = "^msamonthly_[0-9]{6}\\.xls$", full.names = TRUE)

cbsa_2019 <- purrr::map_dfr(files_2019, read_one) %>%
  arrange(yearmon, cbsa) %>%
  select(yearmon, cbsa, total)

print(head(cbsa_2019, 12))
write.csv(cbsa_2019, "2019_bps_monthly.csv", row.names = FALSE)


