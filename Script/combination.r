########################################################################
#################### COMBINE FORECASTS  ################################
########################################################################

#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Script")
#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
# Sourcing data from data.r 


" Average of best three models "
load(file = "../Data/optimal_models.Rdata")

fc_arima_optimal %>% 
  bind_rows(fc_ets_optimal,fc_dynamic_naive )  %>% 
  accuracy(unemployment_test_ts)  %>% 
  dplyr::select(.model, RMSE:RMSSE, -MPE)  %>% 
  arrange(MASE)


## Take the average of our three best models
comb_mean_fc <- bind_cols(fc_arima_optimal, fc_ets_optimal, fc_dynamic_naive)  %>% 
  rowwise() %>% 
  mutate(mean_fc=mean(c(.mean...4, .mean...8, .mean...12 )))

vec <- c(unemployment_test$unemployed- comb_mean_fc$mean_fc)
MASE(.resid =  vec,  .train = c(unemployment_train_ts$unemployed), .period = 12)
RMSE(vec)
RMSSE(.resid =  vec,  .train = c(unemployment_train_ts$unemployed), .period = 12)
MASE(.resid = unemployment_test$unemployed, .actual = comb_mean_fc$mean_fc, .period = 12)
RMSSE(comb_mean_fc$mean_fc, unemployment_test_ts$unemployed, .period = 12)

comb_mean_fc  %>% 
  ggplot() +
  geom_line(aes(x = date...2, y = mean_fc, color = "Mean forecasting model: ARIMA, ETS, ARIMA NAIVE")) +
  geom_line(aes(x = date, y = unemployed, color = "Observed unemployment"), data = unemployment_test_ts) +
  theme_bw() +
  theme(legend.position = "bottom")
