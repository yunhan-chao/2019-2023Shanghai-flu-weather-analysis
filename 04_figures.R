library(ggplot2)
library(scales)
library(lubridate)


#1. 时间趋势图
df_clean <- df %>%
  mutate(
    ILI = as.numeric(as.character(ILI)),
    # 将 Excel 数字转换为 R 的日期对象
    real_date = as.Date(as.numeric(as.character(epi_week)), origin = "1899-12-30")) %>%
  filter(!is.na(real_date) & !is.na(ILI))

ggplot(df_clean, aes(x = real_date, y = ILI, group = 1)) +
  geom_line(color = "#E41A1C", linewidth = 0.8) + 
  # 核心修改：使用 scale_x_date 自带的格式化功能
  scale_x_date(
    date_breaks = "26 weeks",      # 每半年（26周）显示一个刻度，防止标签挤在一起
    date_labels = "%Y-W%V"         # %Y 是年份，W 是字符，%V 是 ISO 周号
  ) + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
  labs(
    title = "Weekly Influenza-Like Illness (ILI) Trend in Shanghai",
    x = "Epidemiological Week (Year-Week)",
    y = "ILI Value")



#2. 温度趋势图
df_plot <- df %>%
  mutate(
    date_val = as.Date(parse_date_time(as.character(epi_week), orders = c("ymd", "Ymd"))),
    temp_val = as.numeric(as.character(avg_temp))
  ) %>%
  mutate(
    date_val = if_else(is.na(date_val), 
                       as.Date(as.numeric(as.character(epi_week)), origin = "1899-12-30"), 
                       date_val)
  ) %>%
  filter(!is.na(date_val), !is.na(temp_val)) %>%
  arrange(date_val)
ggplot(df_plot, aes(x = date_val, y = temp_val)) +
  geom_line(color = "#1F77B4", linewidth = 0.7, group = 1, na.rm = TRUE) + 
  geom_smooth(method = "loess", formula = y ~ x, 
              color = "#D95F02", fill = "#D95F02", alpha = 0.15, na.rm = TRUE) +
  scale_x_date(
    date_breaks = "26 weeks", 
    date_labels = "%Y-W%V"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(
    title = "Weekly Average Temperature Trend in Shanghai (2019-2023)",
    x = "Epidemiological Week (Year-Week)",
    y = "Average Temperature (°C)"
  )



#3. 季节箱线ILI图
ggplot(df, aes(x = season, y = as.numeric(as.character(ILI)), fill = season)) +
  geom_boxplot() + theme_minimal()

#4. 2023年 Lag2 效应图
best_fit <- gam(ILI ~ s(temp_lag2, k=4) + s(rh, k=4) + s(time_idx, bs="cr"), data = df_2023, method = "REML")
plot(best_fit, select = 1, shade = TRUE, main = "Effect of Temp (Lag 2) on ILI (2023)")

#5. 季节交互效应四分图
par(mfrow = c(2, 2))
plot(fit_season_interaction, select = 1, shade = TRUE, main = "Spring: Temp Lag2 Effect")
plot(fit_season_interaction, select = 2, shade = TRUE, main = "Summer: Temp Lag2 Effect")
plot(fit_season_interaction, select = 3, shade = TRUE, main = "Autumn: Temp Lag2 Effect")
plot(fit_season_interaction, select = 4, shade = TRUE, main = "Winter: Temp Lag2 Effect")

#6. 基于GAM模型提取的2023年上海气温（滞后2周）对ILI%的部分效应图
best_fit <- gam(ILI ~ s(temp_lag2, k=4) + s(rh, k=4) + s(time_idx, bs='cr'), 
                data = df_2023, 
                method = "REML")
plot(best_fit, select = 1, shade = TRUE, 
     main = "Partial Effect of Temperature (Lag 2) on ILI in 2023",
     xlab = "Temperature at Lag 2 (°C)", 
     ylab = "Partial Effect on ILI")


#7. 2023年的多变量回归滞后效应分析
df_2023 <- df_model %>% filter(epi_week >= "2023-01-01")

fit_gam_2023 <- gam(ILI ~ s(temp_lag1, k=4) + s(rh, k=4) + s(time_idx, bs="cr"), 
                    data = df_2023, method = "REML")
summary(fit_gam_2023)
plot(fit_gam_2023, pages = 1, shade = TRUE)
