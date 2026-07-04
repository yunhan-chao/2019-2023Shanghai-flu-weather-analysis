library(mgcv)
library(tidyverse)

#1. 数据准备
df_model <- df %>%
  rename(rh = `avg_rh（湿度）`, ws = `avg_ws（风速）`, psl = `avg_psl（气压）`, precip = `total_precip（降水）`) %>%
  mutate(across(c(ILI, temp_lag1, temp_lag2, temp_lag3, temp_lag4, rh, ws, psl, precip), 
                ~as.numeric(as.character(.)))) %>%
  mutate(time_idx = row_number()) %>%
  filter(!is.na(temp_lag4), !is.na(rh))

#2. 2023年滞后效应对比 (用于寻找最优滞后项)
df_2023 <- df_model %>% filter(epi_week >= "2023-01-01")
lag_vars <- c("temp_lag1", "temp_lag2", "temp_lag3")

comparison_results <- map_df(lag_vars, ~{
  fit <- gam(as.formula(paste0("ILI ~ s(", .x, ", k=4) + s(rh, k=4) + s(time_idx, bs='cr')")), 
             data = df_2023, method = "REML")
  data.frame(滞后项 = .x, P值 = summary(fit)$s.table[1, 4], AIC值 = AIC(fit), 调整R方 = summary(fit)$r.sq)
})
print(comparison_results)

#3. 季节交互作用模型 (最终模型)
fit_season_interaction <- gam(ILI ~ s(temp_lag2, by = season, k = 4) + s(rh, k = 4) + s(time_idx, bs = "cr"), 
                              data = df_model, method = "REML")
summary(fit_season_interaction)

