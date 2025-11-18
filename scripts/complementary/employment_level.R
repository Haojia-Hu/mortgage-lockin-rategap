install.packages('data.table')
install.packages('readxl')

library(data.table)
library(readxl)


# -------- 1) 路径与文件列表 --------
base_dir   <- "C:/Users/hh7473a/Desktop"
qcew_dir   <- file.path(base_dir, "QCEW")                 # 存放 quarterly singlefile CSV 的文件夹
xwalk_path <- file.path(base_dir, "list1_2023.xlsx")      # county→CBSA 对应表（从第3行开始为表头）

# 读取 2018–2024 所有季度 CSV（按你的命名规则调整 pattern）
files_q <- list.files(qcew_dir, pattern = "\\q4.singlefile\\.csv$", full.names = TRUE)
# 只保留 2018–2024 的文件（如果文件名里包含年份）
years_keep <- 2018:2024
files_q <- files_q[grepl(paste(years_keep, collapse="|"), files_q)]


# -------- 2) 需要的列（存在多少取多少） --------
needed_cols <- c(
  "area_fips","own_code","industry_code","year","qtr",
  "month1_emplvl","month2_emplvl","month3_emplvl"
)

# 安全读取（只选存在的列，避免 select 不存在时报错）
safe_fread <- function(path, select) {
  # 先读表头
  hdr <- names(fread(path, nrows = 0, showProgress = FALSE))
  use_cols <- intersect(select, hdr)
  fread(path, select = use_cols, showProgress = FALSE)
}

# -------- 3) 读入 county→CBSA crosswalk --------
xwalk_raw <- read_excel(xwalk_path, skip = 2)
xwalk <- as.data.table(xwalk_raw)[
  !is.na(`CBSA Code`) & !is.na(`FIPS State Code`) & !is.na(`FIPS County Code`),
  .(
    cbsa_code   = sprintf("%05d", as.integer(`CBSA Code`)),
    county_fips = sprintf("%02d%03d", as.integer(`FIPS State Code`), as.integer(`FIPS County Code`))
  )
]
xwalk <- unique(xwalk)  # 去重

# -------- 4) 主循环：逐文件读取 → 过滤 → 映射 → 展开为月度 → 聚合 --------
agg_list <- vector("list", length(files_q))

for (i in seq_along(files_q)) {
  dt <- safe_fread(files_q[i], select = needed_cols)
  
  # 标准化类型
  # county 级：area_fips 应该是 5 位县FIPS 或者州/全国等；我们后面 join 时只会保留能匹配 county 的
  dt[, area_fips      := sprintf("%05s", as.character(area_fips))]
  dt[, own_code       := as.character(own_code)]
  dt[, industry_code  := as.character(industry_code)]
  dt[, year           := as.integer(year)]
  dt[, qtr            := as.integer(qtr)]
  
  # 仅保留：all(0) + 总行业(10)
  dt <- dt[own_code == "0" & industry_code == "10"]
  
  # 只保留 2018–2024
  dt <- dt[year %in% years_keep]
  
  # 提取三个月度值
  # 有些文件会出现字符型/空字符串，这里安全转数值
  to_num <- function(x) suppressWarnings(as.numeric(x))
  dt[, emp1 := to_num(month1_emplvl)]
  dt[, emp2 := to_num(month2_emplvl)]
  dt[, emp3 := to_num(month3_emplvl)]
  
  # 通过 county→CBSA 映射（只保留能匹配到县FIPS的记录）
  dt <- dt[xwalk, on = .(area_fips = county_fips), nomatch = 0L]
  
  # —— 把季度的3个月展开成真实月份 —— #
  # Q1 → 月1,2,3；Q2 → 4,5,6；Q3 → 7,8,9；Q4 → 10,11,12
  dt_long <- melt(
    dt,
    id.vars = c("cbsa_code","year","qtr"),
    measure.vars = c("emp1","emp2","emp3"),
    variable.name = "mvar",
    value.name   = "emp"
  )
  
  dt_long[, m := fifelse(mvar == "emp1", 1L, fifelse(mvar == "emp2", 2L, 3L))]
  dt_long[, month := (qtr - 1L) * 3L + m]       # 1..12
  dt_long[, ym    := sprintf("%04d-%02d", year, month)]
  
  # 聚合到 CBSA×月（避免重复行）
  agg <- dt_long[
    ,
    .(emp_total = sum(emp, na.rm = TRUE)),
    by = .(cbsa_code, year, month, ym)
  ]
  
  rm(dt, dt_long); gc()
  agg_list[[i]] <- agg
}

# -------- 5) 合并全部文件 & 排序 --------
emp_monthly <- rbindlist(agg_list, use.names = TRUE, fill = TRUE)
setorder(emp_monthly, cbsa_code, year, month)

# -------- 6)（可选）稀疏性检查：是否每个 CBSA 都覆盖 2018m1–2024m12 --------
# 这只是粗略检查：每个 CBSA 的月份条数
coverage <- emp_monthly[, .N, by = cbsa_code][order(-N)]
print(head(coverage))

# -------- 7) 结果示例 --------
print(head(emp_monthly))

# -------- 8) 导出 CSV --------
out_path <- file.path(base_dir, "QCEW_cbsa_totalemp.csv")
fwrite(emp_monthly, out_path)
cat("已导出：", out_path, "\n")

