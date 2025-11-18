install.packages("data.table")
install.packages("readxl")
install.packages("lubridate")
library("data.table")
library("readxl")
library("lubridate")

setwd("C:/Users/hh7473a/Desktop")
out_dir <- getwd()

hmda_panel_file <- "MSA_outstanding_weighted_rate_monthly.csv"  # your monthly MSA panel
pmms_file       <- "historicalweeklydata.xlsx"                   # PMMS weekly workbook

# ==== Read MSA monthly panel (already computed from HMDA) ====
MSA_panel <- fread(hmda_panel_file, na.strings = c("", "NA"))
stopifnot(all(c("eval_year","eval_month","derived_msa_md","weighted_rate") %in% names(MSA_panel)))

# Coerce types
MSA_panel[, eval_year  := as.integer(eval_year)]
MSA_panel[, eval_month := as.integer(eval_month)]
MSA_panel[, weighted_rate := as.numeric(weighted_rate)]

# Restrict to 2018–2024 monthly window
MSA_panel <- MSA_panel[eval_year >= 2018 & eval_year <= 2024]

# ==== Read PMMS weekly (header starts at row 5, we only need first two columns) ====
PMMS_raw <- read_excel(
  path      = pmms_file,
  skip      = 4,      # start from the 5th row
  col_names = TRUE
)

# Standardize column names to avoid spaces/slashes issues
names(PMMS_raw)[1:2] <- c("Week", "FRM30")

# Keep only Week & FRM30; drop empty rows
PMMS_raw <- as.data.table(PMMS_raw[, c("Week","FRM30")])
PMMS_raw <- PMMS_raw[!(is.na(Week) & is.na(FRM30))]

# ==== Robust date parsing for mixed formats (ymd, mdy, excel serial) ====
to_Date_excel_mixed <- function(v) {
  # Handle numeric excel serial (including accidentally parsed as character digits)
  if (is.numeric(v)) {
    return(as.Date(v, origin = "1899-12-30"))
  }
  v_chr <- as.character(v)
  res <- as.Date(rep(NA_real_, length(v_chr)), origin = "1970-01-01")
  
  # Excel serial stored as 5+ digits string
  idx_serial <- grepl("^\\d{5,}$", v_chr)
  if (any(idx_serial)) {
    res[idx_serial] <- as.Date(as.numeric(v_chr[idx_serial]), origin = "1899-12-30")
  }
  
  # Try ymd first (handles yyyy/mm/dd and yyyy-mm-dd)
  idx_other <- which(!idx_serial)
  if (length(idx_other)) {
    tmp <- suppressWarnings(ymd(v_chr[idx_other]))
    # Fallback to mdy (handles mm/dd/yyyy)
    bad <- is.na(tmp)
    if (any(bad)) {
      tmp[bad] <- suppressWarnings(mdy(v_chr[idx_other][bad]))
    }
    res[idx_other] <- tmp
  }
  res
}

PMMS_raw[, Week := to_Date_excel_mixed(Week)]
PMMS_raw <- PMMS_raw[!is.na(Week)]
PMMS_raw[, FRM30 := as.numeric(FRM30)]
PMMS_raw <- PMMS_raw[!is.na(FRM30)]

# Restrict PMMS weekly window (as specified) ====
PMMS_raw <- PMMS_raw[
  Week >= as.Date("2018-01-04") & Week <= as.Date("2024-12-26")
]

# Weekly -> Monthly (simple monthly average of weekly FRM30)
PMMS_raw[, `:=`(eval_year = year(Week), eval_month = month(Week))]
pmms_monthly <- PMMS_raw[
  eval_year >= 2018 & eval_year <= 2024,
  .(pmms_30y = mean(FRM30, na.rm = TRUE)),
  by = .(eval_year, eval_month)
]

print(pmms_monthly[order(eval_year, eval_month)][1:12])

# ==== Merge monthly PMMS with MSA monthly panel; compute rate gap ====
Rate_gap_monthly <- merge(
  MSA_panel,
  pmms_monthly,
  by = c("eval_year","eval_month"),
  all.x = TRUE
)

Rate_gap_monthly[, rate_gap := pmms_30y - weighted_rate]

# ==== 导出（文件名改为月度版）====
fwrite(Rate_gap_monthly, file.path(out_dir, "Rate_gap_monthly.csv"))
saveRDS(Rate_gap_monthly, file.path(out_dir, "Rate_gap_monthly.rds"))



# ==== Visualization ==== 
install.packages("ggplot2")
library("ggplot2")
library("data.table")

setwd("C:/Users/hh7473a/Desktop")

# ==== Load monthly rate gap file ====
DT <- fread("Rate_gap_monthly.csv")

# Keep only needed columns
stopifnot(all(c("eval_year", "eval_month", "derived_msa_md", "rate_gap") %in% names(DT)))

# ==== Create a Year-Month date ====
DT[, eval_date := as.Date(paste(eval_year, sprintf("%02d", eval_month), "01", sep = "-"))]

# ==== National average monthly rate_gap ====
agg_month <- DT[, .(
  avg_rate_gap = mean(rate_gap, na.rm = TRUE)
), by = eval_date][order(eval_date)]

# ==== Line plot: National monthly average of rate gap only ====
ggplot(agg_month, aes(x = eval_date, y = avg_rate_gap)) +
  geom_line(size = 1.1) +
  labs(
    title = "National Monthly Average Rate Gap",
    x = "Date",
    y = "Rate Gap (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
