########################################################################
#################### Optimal models table #############################
########################################################################

#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Script")
setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
# Sourcing data from data.r 
source("data.r")

load(file = "../Data/optimal_models.Rdata")
load(file = "../Data/fit_dynamic_arima.Rdata")
load(file = "../Data/mse_weighted_fc.Rdata")
load(file = "../Data/fourier_table.Rdata")

conclusion_table <- 
  fc_ets_optimal %>% 
  accuracy(unemployment_test_ts) %>% 
  bind_rows(
    fc_arima_optimal %>%  accuracy(unemployment_test_ts),
    fc_dynamic_naive_forecastobject %>%  accuracy(unemployment_test_ts),
    fc_fourier_table %>% arrange(MASE) %>% slice(1)
    ) %>% 
  rename("Model" = .model) %>% 
  
  bind_rows(
    data.frame(Model = "Multivariate VECM model VAR(10)", 
               Type = "Test", 
               RMSE = RMSE(vecm_resids),
               MAE =  MAE(vecm_resids),
               MAPE = fabletools:: MAPE(.resid = vecm_resids, .actual = c(unemployment_test$unemployed)),
               MASE = MASE(.resid = vecm_resids, .train = c(unemployment_train_ts$unemployed), .period = 12),
               RMSSE = RMSSE(.resid = vecm_resids, .train = c(unemployment_train_ts$unemployed), .period = 12)),
    mse_weighted_fc
  ) %>% 
  dplyr:: select(Model, RMSE, MASE, MAE, MAPE, RMSSE)   %>% 
  arrange(MASE)


conclusion_table %>% 
  kable(caption = "Comparison of all optimal models discussed", digits = 3) %>%
  kable_classic(full_width = F, html_font = "Times new roman") 