install.packages("dplyr")
install.packages("data.table")
library("dplyr")
library("data.table")

setwd("C:/Users/hh7473a/Desktop/HMDA_Data")
getwd()

raw_dir   <- file.path(getwd(), "HMDA_raw")
clean_dir <- file.path(getwd(), "HMDA_clean")

#### ==== Define columns need to keep ====
cols_keep <- c(
  # ---- Location & Time ----
  "activity_year", "state_code", "county_code", "derived_msa_md",
  # ---- rate gap core ----
  "loan_amount", "interest_rate", "loan_term",
  # ---- Filter ----
  "action_taken", "lien_status", "business_or_commercial_purpose",
  "open_end_line_of_credit", "reverse_mortgage",
  "intro_rate_period", "derived_loan_product_type",
  "loan_purpose", "occupancy_type",
  # ---- Regression Needs ----
  "debt_to_income_ratio", "loan_to_value_ratio", "property_value",
  "tract_to_msa_income_percentage"
)

#### ==== Define cleaning function ====
hmda_clean <- function(file_path) {
  year <- gsub("_public_lar_csv\\.csv", "", basename(file_path))
  message("Processing ", year, " ...")
  
  # ==== read the columns ====
  infile  <- file.path(raw_dir, "2018_public_lar_csv.csv")
  dt <- fread(file_path, select = cols_keep, na.strings = c("", "NA", "Exempt"))

  # ==== Basic cleaning: ====
  # interest_rate、loan_amount & loan_term need to transfers into Numerical form
  numify <- function(x) suppressWarnings(as.numeric(x))
  dt[, interest_rate := numify(interest_rate)]
  dt[, loan_amount   := numify(loan_amount)]
  dt[, loan_term     := numify(loan_term)]


  # ==== Line filtering: ====
  # ==== (ensure it is issued, first loan, non-commercial, non-line of credit, non-reverse)
  # action_taken: 1 = Loan originated
  # lien_status: 1 = First lien
  # business_or_commercial_purpose: 2 = Not primarily for a business/commercial purpose
  # open_end_line_of_credit: 2 = Not open-end
  # reverse_mortgage: 2 = Not reverse
  dt <- dt[
    action_taken == 1 &
      lien_status == 1 &
      business_or_commercial_purpose == 2 &
      open_end_line_of_credit == 2 &
      reverse_mortgage == 2
  ]

  # ==== Screen for fixed rates (closer to PMMS 30Y FRM): ====
  # Fixed rates typically have intro_rate_period == 0 or NA
  dt <- dt[is.na(intro_rate_period) | numify(intro_rate_period) == 0]

  # interest_rate, loan_amount & loan_term must exist and be reasonable
  dt <- dt[!is.na(interest_rate) & interest_rate > 0 &
            !is.na(loan_amount)   & loan_amount > 0 &
            !is.na(loan_term)     & loan_term > 0]

  # ==== Drop useless columns
  cols_drop <- c("action_taken", "lien_status", "business_or_commercial_purpose",
                "open_end_line_of_credit", "reverse_mortgage",
                "intro_rate_period")

  dt[, (cols_drop) := NULL]

  # ==== Output results and save files(.csv & .rds files) ====
  outfile_csv <- file.path(clean_dir, paste0(year, "_HMDA_clean.csv"))
  outfile_rds <- file.path(clean_dir, paste0(year, "_HMDA_clean.rds"))

  fwrite(dt, outfile_csv)
  saveRDS(dt, outfile_rds)

  cat(sprintf("✔ %s done: %s rows saved\n", year, format(nrow(dt), big.mark=",")))
}

#### ==== Build file list ====

file_list <- list.files(raw_dir, pattern = "_public_lar_csv\\.csv$", full.names = TRUE)

#### ==== Batch processing ====
for (f in file_list) {
  hmda_clean(f)
}
