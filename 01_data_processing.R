#1. 使用清华镜像安装基础包和 worldmet
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
install.packages("worldmet")
install.packages("tidyverse") # 顺便装上数据处理全家桶

library(worldmet)
library(tidyverse)

# 搜索上海的站点
sh_sites <- getMeta(site = "shanghai")
print(sh_sites)

# 看看这个变量到底有没有东西
nrow(sh_sites) 


# 1. 绕过 getMeta，直接下载上海 2019-2023 年的气象原始数据
# code 583620-99999 是上海国际站的固定 ID
sh_data_raw <- importNOAA(code = "583620-99999", year = 2019:2023)

# 2. 看看数据是不是已经抓下来了（你应该能看到几万行数据）
nrow(sh_data_raw)

# 3. 弹窗看一眼数据表格，确认有气温（air_temp）和湿度（RH）
View(sh_data_raw)

# 1. 处理日期的工具包
library(lubridate)
library(dplyr)

# 2. 开始转换：从“小时”到“周”
shanghai_weekly <- sh_data_raw %>%
  # 第一步：把时间转换成纯日期（去掉时分秒）
  mutate(date_only = as.Date(date)) %>%
  
  # 第二步：计算每天的平均值（排除缺失值 na.rm = TRUE）
  group_by(date_only) %>%
  summarize(
    daily_temp = mean(air_temp, na.rm = TRUE),
    daily_rh = mean(RH, na.rm = TRUE)
  ) %>%
  
  # 第三步：将日期归类到“流行病学周”
  # week_start = 7 表示以周日开始，这通常是流感周报的对齐标准
  mutate(epi_week = floor_date(date_only, unit = "week", week_start = 7)) %>%
  
  # 第四步：计算每周的平均气温和平均湿度
  group_by(epi_week) %>%
  summarize(
    avg_temp = mean(daily_temp, na.rm = TRUE),
    avg_rh = mean(daily_rh, na.rm = TRUE)
  )

# 3. 检查一下转换后的成果（应该只有 260 行左右，对应 5 年的周数）
nrow(shanghai_weekly)
head(shanghai_weekly)

# 4. 把这个干干净净的周数据保存到桌面，以防万一
write.csv(shanghai_weekly, "Shanghai_Weekly_Weather_Clean.csv", row.names = FALSE)



#已抓取完NOAA日气象数据



# 安装读取 Excel 的包
install.packages("readxl")
# 加载包
library(readxl)

flu_data <- read_xlsx("2019-2023年ILI流感周报.xlsx")
# 检查
head(flu_data)


#已成功录入流感数据



# 1. 自动识别列名并对齐（用序号代替名字：第1列是年，第2列是周）
flu_data_clean <- flu_data %>%
  # 使用 .[[1]] 代表第一列，.[[2]] 代表第二列
  mutate(epi_week = floor_date(as.Date(paste(.[[1]], .[[2]], 1, sep="-"), "%Y-%U-%u"), 
                               unit = "week", week_start = 7))

# 2. 检查一下转换后的流感表
# 重点看最后一列是不是出现了叫 epi_week 的日期
head(flu_data_clean)

# 3. 再次尝试合并
final_dataset <- left_join(shanghai_weekly, flu_data_clean, by = "epi_week")

# 4. 看看大功告成的表格！
View(final_dataset)



#滞后效应
final_dataset <- final_dataset %>%
  mutate(
    temp_lag1 = lag(avg_temp, 1), # 滞后1周的气温
    temp_lag2 = lag(avg_temp, 2),  # 滞后2周的气温
    temp_lag3 = lag(avg_temp, 3),  # 滞后3周的气温
    temp_lag4 = lag(avg_temp, 4)  # 滞后4周的气温
  )


# 1. 弹窗选择：shanghai_weekly_weather.csv
shanghai_weekly <- read.csv(file.choose())

# 2. 弹窗选择：2019-2023年ILI流感周报.xlsx
library(readxl)
flu_data <- read_xlsx(file.choose())


# 1. 强制转换日期格式，确保万无一失
shanghai_weekly$epi_week <- as.Date(shanghai_weekly$epi_week)

# 2. 处理流感表：用列序号 [[1]] 代表年份, [[2]] 代表周次
flu_data_clean <- flu_data %>%
  mutate(epi_week = floor_date(as.Date(paste(.[[1]], .[[2]], 1, sep="-"), "%Y-%U-%u"), 
                               unit = "week", week_start = 7))

# 3. 终极缝合：合并数据并生成滞后、季节变量
final_dataset <- left_join(shanghai_weekly, flu_data_clean, by = "epi_week") %>%
  arrange(epi_week) %>%
  mutate(
    # --- 滞后效应 (Lag Effect) ---
    temp_lag1 = lag(avg_temp, 1), # 上周气温
    temp_lag2 = lag(avg_temp, 2), # 前周气温
    
    # --- 季节效应 (Seasonality) ---
    month = month(epi_week),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5)  ~ "Spring",
      month %in% c(6, 7, 8)  ~ "Summer",
      TRUE ~ "Autumn"
    )
  ) %>%
  # 剔除因为滞后计算产生的开头空行
  filter(!is.na(temp_lag2))

# 4. 导出成品到当前文件夹
write.csv(final_dataset, "Final_Analysis_Data_V3.csv", row.names = FALSE)

View(final_dataset)

getwd()
# 看看能不能跳出合并后的数据预览
View(final_dataset)



# 只保留：日期、气温、湿度、流感率、滞后项、月份、季节
final_clean_data <- final_dataset %>%
  select(
    epi_week, 
    avg_temp, 
    avg_rh, 
    ILI = `流感周报.x`,   # 将其中一列重命名为标准的 ILI
    temp_lag1, 
    temp_lag2, 
    month, 
    season
  )

# 检查一下，现在是不是清爽多了？
View(final_clean_data)

# 保存这个真正的“论文最终版”
write.csv(final_clean_data, "Final_Data_For_Thesis.csv", row.names = FALSE)



# 在现有数据基础上增加滞后 3 和 4
final_dataset <- final_dataset %>%
  arrange(epi_week) %>%
  mutate(
    temp_lag3 = lag(avg_temp, 3),
    temp_lag4 = lag(avg_temp, 4)
  ) %>%
  # 为了模型准确，剔除开头因为滞后 4 周产生的 4 行空值
  filter(!is.na(temp_lag4))

# 只保留核心列，彻底甩掉那些 ...11, ...12 等 NA 列
final_clean_v4 <- final_dataset %>%
  select(
    epi_week, avg_temp, avg_rh, 
    ILI = `流感周报.x`, 
    temp_lag1, temp_lag2, temp_lag3, temp_lag4, 
    season
  )

# 检查一下新列是否出现
View(final_clean_v4)

# 导出这个最全的版本
write.csv(final_clean_v4, "Final_Analysis_Data_V4_FullLags.csv", row.names = FALSE)



# 1. 核心清洗与滞后项补全
final_dataset_v4 <- final_dataset %>%
  arrange(epi_week) %>%
  mutate(
    temp_lag3 = lag(avg_temp, 3),
    temp_lag4 = lag(avg_temp, 4)
  ) %>%
  # 只挑选论文需要的核心变量，彻底告别 ...11, ...12 等冗余列
  select(
    epi_week, 
    avg_temp, 
    avg_rh, 
    ILI = `流感周报.x`, # 统一列名
    temp_lag1, 
    temp_lag2, 
    temp_lag3, 
    temp_lag4, 
    month, 
    season
  ) %>%
  # 剔除因为滞后4周产生的开头几行空值，保证回归分析不报错
  filter(!is.na(temp_lag4))

# 2. 保存到当前文件夹
write.csv(final_dataset_v4, "Shanghai_Flu_Climate_Final_V4.csv", row.names = FALSE)

# 3. 打印前几行确认一下
head(final_dataset_v4)




# 1. 确保气象数据日期格式正确
shanghai_weekly$epi_week <- as.Date(shanghai_weekly$epi_week)

# 2. 重新处理流感原始表 (flu_data)
# 假设：第1列是年，第2列是周，第3列是真正的流感数据(ILI)
flu_data_fixed <- flu_data %>%
  mutate(
    epi_week = floor_date(as.Date(paste(.[[1]], .[[2]], 1, sep="-"), "%Y-%U-%u"), 
                          unit = "week", week_start = 7),
    actual_ILI = as.numeric(.[[3]])  # 【核心】强制提取第3列作为真正的流感数据
  ) %>%
  select(epi_week, actual_ILI)

# 3. 重新合并
final_v5 <- shanghai_weekly %>%
  left_join(flu_data_fixed, by = "epi_week") %>%
  arrange(epi_week) %>%
  mutate(
    temp_lag1 = lag(avg_temp, 1),
    temp_lag2 = lag(avg_temp, 2),
    temp_lag3 = lag(avg_temp, 3),
    temp_lag4 = lag(avg_temp, 4),
    month = month(epi_week),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5)  ~ "Spring",
      month %in% c(6, 7, 8)  ~ "Summer",
      TRUE ~ "Autumn"
    )
  )

# 4. 检查数据量
print(paste("合并后的总行数:", nrow(final_v5)))

# 5. 保存并查看
write.csv(final_v5, "Shanghai_Final_Fixed_V5.csv", row.names = FALSE)
View(final_v5)


# A. 确保气象表日期格式正确
shanghai_weekly$epi_week <- as.Date(shanghai_weekly$epi_week)

# B. 精准提取流感表（不看列名，只看位置）
flu_data_fixed <- flu_data %>%
  mutate(
    # 强制将前两列拼成日期
    epi_week = floor_date(as.Date(paste(.[[1]], .[[2]], 1, sep="-"), "%Y-%U-%u"), 
                          unit = "week", week_start = 7),
    # 【核心】提取第3列作为真正的流感 ILI 数据
    actual_ILI = as.numeric(.[[3]]) 
  ) %>%
  select(epi_week, actual_ILI)

# C. 以流感表为基准进行合并（保证 261 行的完整性）
final_v5 <- flu_data_fixed %>%
  left_join(shanghai_weekly, by = "epi_week") %>%
  arrange(epi_week) %>%
  mutate(
    # 生成 1-4 周滞后项
    temp_lag1 = lag(avg_temp, 1),
    temp_lag2 = lag(avg_temp, 2),
    temp_lag3 = lag(avg_temp, 3),
    temp_lag4 = lag(avg_temp, 4),
    # 重新生成季节
    month = month(epi_week),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5)  ~ "Spring",
      month %in% c(6, 7, 8)  ~ "Summer",
      TRUE ~ "Autumn"
    )
  ) %>%
  # 只保留核心论文变量，删除所有 NA 列和冗余列
  select(epi_week, ILI = actual_ILI, avg_temp, avg_rh, 
         temp_lag1, temp_lag2, temp_lag3, temp_lag4, season)

# D. 导出文件
write.csv(final_v5, "Shanghai_Flu_Climate_Clean_V5.csv", row.names = FALSE)

# E. 验证
View(final_v5)
print(paste("当前总行数:", nrow(final_v5)))


# 1. 确保气象数据日期格式正确
# 如果 shanghai_weekly 不在内存，请先运行: shanghai_weekly <- read.csv(file.choose())
shanghai_weekly$epi_week <- as.Date(shanghai_weekly$epi_week)

# 2. 精准提取流感表（重点：解决你提到的“年份”错误）
# 强制提取第3列（通常是百分比数值），避开第1列（年份）
flu_data_fixed <- flu_data %>%
  mutate(
    # 根据前两列自动生成对齐的周日期
    epi_week = floor_date(as.Date(paste(.[[1]], .[[2]], 1, sep="-"), "%Y-%U-%u"), 
                          unit = "week", week_start = 7),
    # 【核心修正】强制抓取第 3 列作为真正的流感比例数字
    actual_ILI = as.numeric(.[[3]]) 
  ) %>%
  select(epi_week, actual_ILI)

# 3. 缝合所有特征（保持 261 行完整性）
final_v5 <- flu_data_fixed %>%
  left_join(shanghai_weekly, by = "epi_week") %>%
  arrange(epi_week) %>%
  mutate(
    # 补齐你要求的 Lag 1 到 Lag 4 滞后项
    temp_lag1 = lag(avg_temp, 1),
    temp_lag2 = lag(avg_temp, 2),
    temp_lag3 = lag(avg_temp, 3),
    temp_lag4 = lag(avg_temp, 4),
    # 重新划分季节
    month = month(epi_week),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5)  ~ "Spring",
      month %in% c(6, 7, 8)  ~ "Summer",
      TRUE ~ "Autumn"
    )
  ) %>%
  # 只选择论文需要的“清爽”列，丢弃所有带 .x, .y 或 NA 的列
  select(
    epi_week, 
    ILI = actual_ILI, 
    avg_temp, 
    avg_rh, 
    temp_lag1, 
    temp_lag2, 
    temp_lag3, 
    temp_lag4, 
    season
  )

# 4. 导出文件
write.csv(final_v5, "Shanghai_Flu_Climate_Final_V5.csv", row.names = FALSE)

# 5. 打印状态确认
print(paste("文件已成功导出！总行数:", nrow(final_v5)))
View(final_v5)


# 1. 安装并加载包
if(!require(worldmet)) install.packages("worldmet")


# 2. 获取上海虹桥站 2019-2023 数据
# air_temp: 气温, ws: 风速, psl: 海平面气压, precip: 降水量
shanghai_noaa_raw <- importNOAA(code = "583670-99999", year = 2019:2023)

# 3. 将小时/日数据转化为“周”数据，以便和你现有的 V5 表格合并
shanghai_noaa_weekly <- shanghai_noaa_raw %>%
  mutate(epi_week = floor_date(date, unit = "week", week_start = 7)) %>%
  group_by(epi_week) %>%
  summarize(
    avg_ws = mean(ws, na.rm = TRUE),      # 平均风速 (m/s)
    avg_psl = mean(psl, na.rm = TRUE),    # 平均气压 (hPa)
    total_precip = sum(precip, na.rm = TRUE) # 周总降水量 (mm)
  )

# 4. 预览一下抓取到的新指标
head(shanghai_noaa_weekly)


# 将新抓取的 NOAA 指标合并到你之前的 V5 表中
final_v6_ultra <- final_v5 %>%
  left_join(shanghai_noaa_weekly, by = "epi_week")

# 导出这个包含所有气象指标的最终版本
write.csv(final_v6_ultra, "Shanghai_Flu_Climate_Ultra_V6.csv", row.names = FALSE)




# 1. 把下载的 NOAA 原始数据转化成“周”单位
# 注意：这里假设你下载的数据变量名叫 shanghai_noaa_raw（即截图里显示的那个对象）
shanghai_noaa_weekly <- shanghai_noaa_raw %>%
  mutate(epi_week = floor_date(date, unit = "week", week_start = 7)) %>%
  group_by(epi_week) %>%
  summarize(
    avg_ws = mean(ws, na.rm = TRUE),      # 平均风速 (m/s)
    avg_psl = mean(psl, na.rm = TRUE),    # 平均海平面气压 (hPa)
    total_precip = sum(precip, na.rm = TRUE) # 周总降水量 (mm)
  )

# 2. 与你之前的 V5 表格合并（确保 261 行完整）
final_v6_ultra <- final_v5 %>%
  left_join(shanghai_noaa_weekly, by = "epi_week")

# 3. 导出这个“论文终极数据包”
write.csv(final_v6_ultra, "Shanghai_Flu_Climate_Ultra_V6.csv", row.names = FALSE)

# 4. 预览一下，看看最后三列是不是有数了
View(final_v6_ultra)


# 1. 检查刚才下载的对象叫什么，并统一命名为 noaa_data
# 如果你运行 importNOAA 时没写变量名，请重新运行下面这行：
shanghai_noaa_raw <- importNOAA(code = "583670-99999", year = 2019:2023)

# 2. 现在运行这段处理代码（已经避开了刚才的错误）


shanghai_noaa_weekly <- shanghai_noaa_raw %>%
  mutate(epi_week = floor_date(date, unit = "week", week_start = 7)) %>%
  group_by(epi_week) %>%
  summarize(
    avg_ws = mean(ws, na.rm = TRUE),      # 平均风速
    avg_psl = mean(psl, na.rm = TRUE),    # 平均气压
    total_precip = sum(precip, na.rm = TRUE) # 周总降水
  )

# 3. 合并到你的 V5 主表
final_v6_ultra <- final_v5 %>%
  left_join(shanghai_noaa_weekly, by = "epi_week")

# 4. 导出最终成品
write.csv(final_v6_ultra, "Shanghai_Flu_Climate_Ultra_V6.csv", row.names = FALSE)

# 5. 查看结果
View(final_v6_ultra)

getwd()

# --- 毕业论文数据一键生成脚本 (2019-2023 终极版) ---
library(worldmet)
library(dplyr)
library(lubridate)
library(readxl) # 如果你的原始文件是Excel
library(worldmet)
library(dplyr)
library(lubridate)

# 1. 重新抓取数据
shanghai_noaa_raw <- importNOAA(code = "583670-99999", year = 2019:2023)

# 【核心改进】查看实际列名，防止找不到对象
print("当前下载到的原始数据列名为：")
print(names(shanghai_noaa_raw))

# 2. 稳健汇总：自动识别列名并处理
shanghai_noaa_weekly <- shanghai_noaa_raw %>%
  mutate(epi_week = floor_date(date, unit = "week", week_start = 7)) %>%
  group_by(epi_week) %>%
  summarize(
    # 尝试寻找风速列 (ws)，找不到则给 NA
    avg_ws = if("ws" %in% names(.)) mean(ws, na.rm = TRUE) else NA,
    # 尝试寻找气压列 (slp 或 atmos_pres)，找不到则给 NA
    avg_psl = if("slp" %in% names(.)) mean(slp, na.rm = TRUE) else 
      if("atmos_pres" %in% names(.)) mean(atmos_pres, na.rm = TRUE) else NA,
    # 尝试寻找降水列 (precip)，找不到则给 NA
    total_precip = if("precip" %in% names(.)) sum(precip, na.rm = TRUE) else NA
  )

# 3. 载入流感基础表 (如果你内存里没有 final_v5，请手动选择 CSV)
if(!exists("final_v5")) {
  message(">>> 请在弹出窗口中选择你之前的上海流感基础表(V5等) <<<")
  final_v5 <- read.csv(file.choose())
  final_v5$epi_week <- as.Date(final_v5$epi_week)
}

# 4. 合并数据
final_v6_ultra <- final_v5 %>%
  left_join(shanghai_noaa_weekly, by = "epi_week")

# 5. 强制导出到桌面 (绝对能找着)
desktop_file <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "Shanghai_Flu_Final_V6.csv")
write.csv(final_v6_ultra, desktop_file, row.names = FALSE)

# 6. 成功反馈
cat("\n--------------------------------------------------\n")
cat("【任务完成！】\n")
cat("文件已存放在桌面：Shanghai_Flu_Final_V6.csv\n")
cat("如果最后三列全是 NA，说明该站点不提供该项指标。\n")
cat("--------------------------------------------------\n")

View(final_v6_ultra)


save.image("Shanghai_Flu_Project_Backup.RData")               # 安装必要的包
install.packages(c("tidyverse", "GSODR", "lubridate"))

library(tidyverse)
library(GSODR)   # 专门用于下载 NOAA GSOD 每日摘要数据
library(lubridate)

# 1. 下载数据（保持不变）
sh_raw <- get_GSOD(years = 2019:2023, station = "583620-99999")

# 2. 修改清洗部分：将 DATE 替换为 YEARMODA
weather_daily <- sh_raw %>%
  # 注意：这里改为 YEARMODA，且 GSODR 的降水列名通常是 PRCP，气压是 SLP
  select(YEARMODA, SLP, PRCP) %>%
  # 将 PRCP 中的缺失值标识 99.9 替换为 NA
  mutate(PRCP = ifelse(PRCP == 99.9, NA, PRCP)) %>%
  # 转换为日期格式
  mutate(YEARMODA = as.Date(YEARMODA))

# 3. 计算周平均
weather_weekly <- weather_daily %>%
  # 统一使用 YEARMODA 进行计算
  mutate(epi_week = floor_date(YEARMODA, unit = "week", week_start = 7)) %>%
  group_by(epi_week) %>%
  summarise(
    new_avg_psl = mean(SLP, na.rm = TRUE),
    new_total_precip = sum(PRCP, na.rm = TRUE)
  )

# 4. 读取并合并（路径请确保正确）
file_path <- "C:/Users/Valar/Desktop/毕业论文/正文/数据/上海流感气象数据.csv"
original_data <- read_csv(file_path)

# 确保原表的 epi_week 也是 Date 类型
final_data <- original_data %>%
  mutate(epi_week = as.Date(epi_week)) %>%
  left_join(weather_weekly, by = "epi_week") %>%
  mutate(
    avg_psl = ifelse(is.na(avg_psl), new_avg_psl, avg_psl),
    total_precip = ifelse(is.na(total_precip), new_total_precip, total_precip)
  ) %>%
  select(-new_avg_psl, -new_total_precip)

# 保存
write_csv(final_data, file_path)

# 查看这两列的缺失值统计
sum(is.na(final_data$avg_psl))
sum(is.na(final_data$total_precip))

# 快速查看数值范围是否正常
summary(final_data[c("avg_psl", "total_precip")])