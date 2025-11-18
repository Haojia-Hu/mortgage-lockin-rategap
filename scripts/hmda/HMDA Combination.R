install.packages("data.table")
library("data.table")

setwd("C:/Users/hh7473a/Desktop/HMDA_Data")
getwd()

clean_dir <- file.path(getwd(), "HMDA_clean")

# Find the existed cleaned files
file_list <- list.files(clean_dir, pattern = "_HMDA_clean\\.csv$", full.names = TRUE)

# Read and Merge all files
hmda_all <- rbindlist(lapply(file_list, fread), fill = TRUE)

# Filter 30-year term
hmda_30y <- hmda_all[loan_term >= 350 & loan_term <= 370]

# Filter 15-year term (kept separately for backup)
hmda_15y <- hmda_all[loan_term >= 170 & loan_term <= 190]

# Output results
fwrite(hmda_30y, file.path(clean_dir, "HMDA_30y_clean.csv"))
saveRDS(hmda_30y, file.path(clean_dir, "HMDA_30y_clean.rds"))

fwrite(hmda_15y, file.path(clean_dir, "HMDA_15Y_clean.csv"))
saveRDS(hmda_15y, file.path(clean_dir, "HMDA_15Y_clean.rds"))

