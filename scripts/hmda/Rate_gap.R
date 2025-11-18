install.packages("data.table")
install.packages("readxl")
install.packages("lubridate")
library("data.table")
library("readxl")
library("lubridate")

setwd("C:/Users/hh7473a/Desktop")
out_dir <- getwd()

hmda_panel_file <- "MSA_outstanding_weighted_rate.csv"  
pmms_file       <- "historicalweeklydata.xlsx"                      

# ==== Read the HMDA cleaned MSA-level data ====
MSA_panel <- fread(hmda_panel_file, na.strings = c("", "NA"))
stopifnot(all(c("eval_year","derived_msa_md","weighted_rate") %in% names(MSA_panel)))
MSA_panel[, eval_year := as.integer(eval_year)]

# ==== Read the PMMS weekly data ====
PMMS_raw <- read_excel(
  path = pmms_file,
  skip = 4,              
  col_names = TRUE
)

# Unified listing
names(PMMS_raw)[1:2] <- c("Week", "FRM30")
PMMS_raw <- as.data.table(PMMS_raw[, c("Week","FRM30")])
# Remove all NA rows
PMMS_raw <- PMMS_raw[!(is.na(Week) & is.na(FRM30))]

# ==== Date parsing functions (element-by-element/type-by-type processing) ====
to_Date_excel_mixed <- function(v) {
  # If the entire column is numeric (Excel serial number)
  if (is.numeric(v)) {
    return(as.Date(v, origin = "1899-12-30"))
  }
  # Otherwise, the entire column is character (or is read as character)
  v_chr <- as.character(v)
  res <- as.Date(rep(NA_real_, length(v_chr)), origin = "1970-01-01")
  
  # (a) Pure 5-digit string -> used as Excel serial number
  idx5 <- grepl("^\\d{5}$", v_chr)
  if (any(idx5)) {
    res[idx5] <- as.Date(as.numeric(v_chr[idx5]), origin = "1899-12-30")
  }
  # (b) Other character strings: first ymd (yyyy/mm/dd), then mdy (mm/dd/yyyy)
  idx_other <- which(!idx5)
  if (length(idx_other)) {
    tmp <- suppressWarnings(ymd(v_chr[idx_other]))
    bad <- is.na(tmp)
    if (any(bad)) {
      tmp[bad] <- suppressWarnings(mdy(v_chr[idx_other][bad]))
    }
    res[idx_other] <- tmp
  }
  res
}

# Actual transfer
PMMS_raw[, Week := to_Date_excel_mixed(Week)]
# Keep valid rows
PMMS_raw <- PMMS_raw[!is.na(Week)]
PMMS_raw[, FRM30 := as.numeric(FRM30)]
PMMS_raw <- PMMS_raw[!is.na(FRM30)]

# Limiting the time range
PMMS_raw <- PMMS_raw[Week >= as.Date("2018-01-04") & Week <= as.Date("2024-12-26")]

# ==== Extract the year and make an annual average (combine it with HMDA by year) ====
PMMS_raw[, eval_year := year(Week)]
pmms_yearly <- PMMS_raw[
  eval_year >= 2018 & eval_year <= 2024,
  .(pmms_30y = mean(FRM30, na.rm = TRUE)),
  by = eval_year
]

print(head(pmms_yearly))

# ==== Calculate the Rate Gap ==== 
Rate_gap <- merge(MSA_panel, pmms_yearly, by = "eval_year", all.x = TRUE)
Rate_gap[, rate_gap := pmms_30y - weighted_rate]

fwrite(Rate_gap, "Rate_gap.csv")
saveRDS(Rate_gap, "Rate_gap.rds")


# ==== Visualization ==== 
install.packages("ggplot2")
library("ggplot2")

setwd("C:/Users/hh7473a/Desktop")
getwd()

DT <- fread("Rate_gap.csv")
# Make sure column title
stopifnot(all(c("eval_year", "derived_msa_md", "weighted_rate", "rate_gap") %in% names(DT)))

# ==== National average trend ====
agg_year <- DT[, .(
  avg_weighted_rate = mean(weighted_rate, na.rm = TRUE),
  avg_rate_gap = mean(rate_gap, na.rm = TRUE)
), by = eval_year]

# Long format
agg_melt <- melt(agg_year, id.vars = "eval_year",
                 variable.name = "Series", value.name = "Rate")

# Plotting national-level average Trends
ggplot(agg_melt, aes(x = eval_year, y = Rate, color = Series, group = Series)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("avg_weighted_rate" = "blue", 
                                "avg_rate_gap" = "red")) +
  labs(title = "National Average: Outstanding Weighted Rate vs Rate Gap",
       x = "Year", y = "Rate (%)",
       color = "Series") +
  theme_minimal(base_size = 14)


# ==== Plotting Scatters (relationship) ====
ggplot(DT, aes(x = weighted_rate, y = rate_gap)) +
  geom_point(alpha = 0.4, color = "gray40") +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Cross-Section: Rate Gap vs Weighted Contract Rate",
       x = "Weighted Contract Rate (%)", y = "Rate Gap (%)") +
  theme_minimal(base_size = 14)

