#1. 数据加载与预处理
library(tidyverse)
library(readxl)
library(psych)
library(mgcv)

#1. 表1（Mean,SD,Min,Max）
df <- read_excel("C:/Users/Valar/Desktop/GitHub/2019-2023Shanghai-flu-weather-analysis/data/processed/shanghai_data.xlsx")
glimpse(df)
summary(df)
df$season <- factor(df$season,
                    levels = c("Spring","Summer","Autumn","Winter"))
colSums(is.na(df))
library(psych)
colnames(df)
describe(df[,c("ILI","avg_temp","avg_rh（湿度）","avg_ws（风速）","avg_psl（气压）","total_precip（降水）")])



#2. spearman相关分析
df_cor <- df %>%
  mutate(across(c(ILI, avg_temp, `avg_rh（湿度）`, `avg_ws（风速）`, `avg_psl（气压）`, `total_precip（降水）`), 
                ~as.numeric(as.character(.))))
vars <- c("avg_temp", "avg_rh（湿度）", "avg_ws（风速）", "avg_psl（气压）", "total_precip（降水）")
for(v in vars){
  if(v %in% colnames(df_cor)){
    cat("\n--- Spearman Correlation for:", v, "---\n")
    # 使用转换后的 df_cor 
    # use = "complete.obs" 会自动处理 NA 值，这对统计结果非常重要
    test_result <- cor.test(df_cor$ILI, df_cor[[v]], 
                            method = "spearman", 
                            exact = FALSE, 
                            na.action = "na.omit")
    print(test_result)
  } else {
    cat("\n[警告] 找不到列名:", v, "\n")
  }
}



#3. 季节滞后
lags <- c("temp_lag1", "temp_lag2", "temp_lag3", "temp_lag4")
cat("\n======= 分季节滞后性分析结果 =======\n")
df_model <- df %>%
  rename(
    rh = `avg_rh（湿度）`,
    ws = `avg_ws（风速）`,
    psl = `avg_psl（气压）`,
    precip = `total_precip（降水）`
  ) %>%
  mutate(across(c(ILI, temp_lag1, temp_lag2, temp_lag3, temp_lag4, rh, ws, psl, precip), 
                ~as.numeric(as.character(.)))) %>%
  mutate(time_idx = row_number()) %>%
  filter(!is.na(temp_lag1), !is.na(rh))
colnames(df_model)
fit_gam <- gam(ILI ~ s(temp_lag1, k=4) + 
                 s(rh, k=4) + 
                 s(time_idx, bs="cr"), 
               data = df_model, 
               method = "REML")
df_model %>%
  group_by(season) %>%
  group_split() %>%
  walk(~ {
    s_name <- as.character(unique(.x$season))
    cat("\n>>> 季节:", s_name, "\n")
    
    for(v in lags){
      if(sum(!is.na(.x[[v]])) > 5){
        res <- cor.test(.x$ILI, .x[[v]], method = "spearman", exact = FALSE)
        sig <- if(res$p.value < 0.05) " (显著*)" else ""
        cat(paste0(v, ": rho = ", round(res$estimate, 3), 
                   ", p = ", round(res$p.value, 4), sig, "\n"))} } })


#4. 2023选择temp2
df_2023 <- df_model %>% filter(epi_week >= "2023-01-01")
lag_vars <- c("temp_lag1", "temp_lag2", "temp_lag3")
comparison_results <- list()
for(l in lag_vars){
  form <- as.formula(paste0("ILI ~ s(", l, ", k=4) + s(rh, k=4) + s(time_idx, bs='cr')"))
  fit <- gam(form, data = df_2023, method = "REML")
  sum_fit <- summary(fit)
  comparison_results[[l]] <- data.frame(
    滞后项 = l,
    P值 = sum_fit$s.table[1, 4],
    AIC值 = AIC(fit),
    调整R方 = sum_fit$r.sq)}
final_table <- do.call(rbind, comparison_results)
print("======= 2023年不同滞后周数模型对比表 =======")
print(final_table)


#5. GAM季节交互
df_model$season <- factor(df_model$season, levels = c("Spring", "Summer", "Autumn", "Winter"))
nrow(df_model)
fit_season_interaction <- gam(ILI ~ 
                                s(temp_lag2, by = season, k = 4) + 
                                s(rh, k = 4) +                   
                                s(time_idx, bs = "cr"),            
                              data = df_model, 
                              method = "REML",
                              family = gaussian()) 
summary_interaction <- summary(fit_season_interaction)
print("======= 季节交互 GAM 模型结果汇总 =======")
print(summary_interaction)
