install.packages("tidyr")

library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# 1) Range
start_ym <- "201801"
end_ym   <- "201910"

# 2) Output
out_csv  <- "2018_bps_monthly.csv"  # 可改名；不想保存就注释 write.csv 那行


# Month time-series（YYYYMM）
ym_seq <- {
  s <- as.Date(paste0(start_ym, "01"), "%Y%m%d")
  e <- as.Date(paste0(end_ym,   "01"), "%Y%m%d")
  format(seq(s, e, by = "month"), "%Y%m")
}

# URL templete：offical .txt links
url_from_ym <- function(ym) sprintf("https://www.census.gov/construction/bps/txt/tb3u%s.txt", ym)

# Clean columns' names
clean_names <- function(x) {
  x <- trimws(as.character(x))
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# YYYYMM -> YYYY-MM
ym_label <- function(ym) paste0(substr(ym, 1, 4), "-", substr(ym, 5, 6))

# Explore one txt（Read from URL）
read_one_txt_url <- function(ym) {
  url <- url_from_ym(ym)
  message("↘️ Crawl：", url)
  
  # Read websites（read_lines can read https text directly）
  lines <- tryCatch(
    readr::read_lines(url, progress = FALSE),
    error = function(e) {
      warning("Read Failure（404）：", url)
      return(character(0))
    }
  )
  if (length(lines) == 0) {
    return(tibble(yearmon = character(), cbsa = character(), total = numeric()))
  }
  
  # find the head title：like "CSA  CBSA  Name  Total ..."
  hdr_idx <- which(str_detect(lines, regex("^\\s*CSA\\s+CBSA\\s+Name\\s+Total", ignore_case = TRUE)))[1]
  if (is.na(hdr_idx)) {
    warning("未找到表头（含 'CSA  CBSA  Name  Total'）：", url)
    return(tibble(yearmon = character(), cbsa = character(), total = numeric()))
  }
  
  header_line <- lines[hdr_idx]
  
  # Infer column width based on the starting position of the keyword in the table header (fixed width parsing)
  starts <- c(
    regexpr("\\bCSA\\b",  header_line, ignore.case = TRUE),
    regexpr("\\bCBSA\\b", header_line, ignore.case = TRUE),
    regexpr("\\bName\\b", header_line, ignore.case = TRUE),
    regexpr("\\bTotal\\b",header_line, ignore.case = TRUE)
  ) |> as.integer()
  ends <- c(starts[-1] - 1, nchar(header_line))
  
  # Data area: from the next line of the header to the end of the file
  dat_lines <- lines[(hdr_idx + 1):length(lines)]
  
  # Read four columns using fixed-width positions
  pos <- fwf_positions(
    start = starts,
    end   = ends,
    col_names = c("CSA", "CBSA", "Name", "Total_and_more")
  )
  tmp <- read_fwf(I(dat_lines), pos, col_types = "cccc")
  
  # Clean and extract the first digit as the Total of the Current Month
  out <- tmp %>%
    mutate(
      CBSA = str_extract(CBSA, "\\d{5}"),      # Pick 5 digits
      Name = trimws(Name),
      total = as.numeric(str_extract(Total_and_more, "\\d+"))  # first number in the line
    ) %>%
    filter(!is.na(CBSA), CBSA != "", !is.na(total)) %>%
    transmute(
      yearmon = ym_label(ym),
      cbsa    = CBSA,
      total   = total
    )
  
  if (nrow(out) == 0) {
    warning("No data：", url)
  }
  out
}

# Batch crawling + merging
cbsa_monthly_txt <- purrr::map_dfr(ym_seq, read_one_txt_url) %>%
  arrange(yearmon, cbsa)

# Preview
print(head(cbsa_monthly_txt, 12))

# Save output
write.csv(cbsa_monthly_txt, out_csv, row.names = FALSE)


# ==== Combined all the BPS data together(2018 to 2024) ====
dir_csv <- "C:/Users/hh7473a/Desktop"  # .csv file on the desktop

# Find all the time-line 2018–2024
files <- list.files(
  dir_csv,
  pattern = "^(2018|2019|2020|2021|2022|2023|2024)_bps_monthly\\.csv$",
  full.names = TRUE, recursive = TRUE
)

# Read + Unified form + Merge
panel <- map_dfr(files, ~ read_csv(.x, show_col_types = FALSE)) %>%
  rename_with(tolower) %>%
  transmute(
    yearmon = as.character(yearmon),
    cbsa    = as.character(cbsa),
    total   = as.numeric(total)
  ) %>%
  mutate(
    # Unified yearmon
    yearmon = str_replace_all(yearmon, "/", "-"),
    yearmon = if_else(str_detect(yearmon, "^\\d{6}$"),
                      paste0(substr(yearmon,1,4), "-", substr(yearmon,5,6)),
                      yearmon)
  ) %>%
  filter(yearmon >= "2018-01", yearmon <= "2024-12",
         !is.na(cbsa), cbsa != "", !is.na(total)) %>%
  group_by(yearmon, cbsa) %>%                      
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
  arrange(yearmon, cbsa)

print(head(panel, 10))

# Save output
write_csv(panel, file.path(dir_csv, "bps_cbsa_monthly.csv"))

