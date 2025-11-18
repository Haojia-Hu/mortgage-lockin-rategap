install.packages(c("readr", "dplyr", "stringr", "readxl"), dep = TRUE)
library(readr)
library(dplyr)
library(stringr)
library(readxl)

# === 0) pathway ===
path_saiz <- "C:/Users/hh7473a/Desktop/saiz2010/saiz2010.csv"        
path_xls  <- "C:/Users/hh7473a/Desktop/saiz2010/cbsa03_msa99.xls"    
out_csv   <- "C:/Users/hh7473a/Desktop/saiz2010/saiz2010_cbsa.csv" 


# 1) Read in Saiz and pad the old MSA code with 4 characters
saiz <- read_csv(path_saiz, show_col_types = FALSE) %>%
  mutate(msanecma = str_pad(as.character(msanecma), 4, pad = "0"))

# 2) Read in the crosswalk (county level) code
xw_raw <- read_excel(path_xls)

xw <- xw_raw %>%
  rename(
    C_MSA_1999_Code = matches("C/?MSA_?1999_?Code",  ignore.case = TRUE),
    CBSA_2003_Code  = matches("CBSA_?2003_?Code",    ignore.case = TRUE)
  ) %>%
  mutate(
    C_MSA_1999_Code = str_pad(as.character(C_MSA_1999_Code), 4, pad = "0"),
    CBSA_2003_Code  = str_pad(as.character(CBSA_2003_Code),  5, pad = "0")
  ) %>%
  filter(!is.na(C_MSA_1999_Code), !is.na(CBSA_2003_Code))

# 3) Aggregate from county to old MSA and select a "primary CBSA" for each old MSA
msa_to_cbsa <- xw %>%
  count(C_MSA_1999_Code, CBSA_2003_Code, name = "n_counties") %>%
  group_by(C_MSA_1999_Code) %>%
  arrange(desc(n_counties), CBSA_2003_Code) %>%
  slice(1L) %>%
  ungroup() %>%
  transmute(
    msanecma = C_MSA_1999_Code,
    cbsa_code = CBSA_2003_Code
  )


# 4) Combined Saiz，& add cbsa_code
saiz_cbsa <- saiz %>%
  left_join(msa_to_cbsa, by = "msanecma")
saiz_cbsa <- saiz_cbsa %>%
  filter(!is.na(cbsa_code))

# 5) Test NA
unmatched <- saiz_cbsa %>% filter(is.na(cbsa_code)) %>% distinct(msanecma)
print(unmatched)
saiz_cbsa <- saiz_cbsa %>%
  filter(!is.na(cbsa_code))

# 6) output
write_csv(saiz_cbsa, out_csv)
message("已导出：", out_csv)

