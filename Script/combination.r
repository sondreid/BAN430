########################################################################
#################### COMBINE FORECASTS  ################################
########################################################################

#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Script")
#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
# Sourcing data from data.r 
source("data.r")

" Average of best three models "
load(file = "../Data/optimal_models.Rdata")

fc_arima_optimal %>% 
  bind_rows(fc_ets_optimal,fc_dynamic_naive )  %>% 
  accuracy(unemployment_test_ts)  %>% 
  dplyr::select(.model, RMSE:RMSSE, -MPE)  %>% 
  arrange(MASE)

#######################################################################################################
############################ Combinational forecast: Mean TOP #########################################
#######################################################################################################
comb_mean_fc <- bind_rows(fc_arima_optimal, fc_ets_optimal, fc_dynamic_naive) %>%
  as_tibble() %>% 
  dplyr::select(-cpi, -export, -Model,-unemployed) %>% 
  pivot_wider(names_from = .model, values_from = .mean) %>% 
  rowwise() %>% 
  mutate(mean_fc = mean(ARIMA_dynamic, ARIMA_optimal, ETS_optimal))

vec_mean_fc <- c(unemployment_test$unemployed- comb_mean_fc$mean_fc)
bind_cols(
  Model = "Combined forecast mean weighted",
  MASE = MASE(.resid =  vec_mean_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12),
  RMSE = RMSE(vec_mean_fc),
  RMSSE = RMSSE(.resid =  vec_mean_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12)) %>% 
  kbl(caption = "Combined forecast accuracy", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")


comb_mean_fc  %>% 
  ggplot() +
  geom_line(aes(x = date, y = mean_fc, color = "Mean forecasting model: ARIMA, ETS, ARIMA NAIVE")) +
  geom_line(aes(x = date, y = unemployed, color = "Observed unemployment"), data = unemployment_test_ts) +
  theme_bw() +
  theme(legend.position = "bottom")

#######################################################################################################
#################### Advanced combination method: MSE (Stock and Watson(2001)) ########################
#######################################################################################################
mse_arima_optimal <- mean((augment(fit_arima_optimal)$.fitted - unemployment_train_ts$unemployed)^2)
mse_ets_optimal <- mean((augment(fit_ets_optimal)$.fitted - unemployment_train_ts$unemployed)^2)
mse_dynamic_arima <- mean((augment(fit_dynamic_arima)$.fitted  - unemployment_train_ts$unemployed)^2)

w_sum <- sum(1/mse_arima_optimal, 1/mse_ets_optimal, 1/mse_dynamic_arima)

w_arima_optimal <- (1/mse_arima_optimal)/w_sum
w_ets_optimal <- (1/mse_ets_optimal)/w_sum
w_dynamic_arima <- (1/mse_dynamic_arima)/w_sum


w_ets_optimal
w_arima_optimal
w_dynamic_arima

comb_mse_fc <- bind_rows(fc_arima_optimal, fc_ets_optimal, fc_dynamic_naive) %>%
  as_tibble() %>% 
  dplyr::select(-cpi, -export, -Model,-unemployed) %>% 
  pivot_wider(names_from = .model, values_from = .mean) %>% 
  rowwise() %>% 
  mutate(mean_fc = sum(w_arima_optimal * ARIMA_optimal, 
                       w_ets_optimal * ETS_optimal, 
                       w_dynamic_arima * ARIMA_dynamic))


vec_mse_fc <- c(unemployment_test$unemployed - comb_mse_fc$mean_fc) # residuals vector

bind_cols(
  Model = "Combined forecast weighted on MSE",
  MASE = MASE(.resid =  vec_mse_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12),
  RMSE = RMSE(vec_mse_fc),
  RMSSE = RMSSE(.resid =  vec_mse_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12)) %>% 
  kbl(caption = "Combined forecast accuracy", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")


comb_mse_fc  %>% 
  ggplot() +
  geom_line(aes(x = date, y = mean_fc, color = "Mean forecasting model: ARIMA, ETS, ARIMA NAIVE")) +
  geom_line(aes(x = date, y = unemployed, color = "Observed unemployment"), data = unemployment_test_ts) +
  theme_bw() +
  theme(legend.position = "bottom")







#############################################################################################################
############# RESTEN ER BARE TULLLLLL#################################################
###########################################################################################################

# Advanced combination method: difference in standard deviation
sd_fc_arima_optimal <- sd(fc_arima_optimal$.mean)
sd_fc_ets_optimal <- sd(fc_ets_optimal$.mean)
sd_fc_dynamic_naive <- sd(fc_dynamic_naive$.mean)


comb_sd_fc <- bind_rows(fc_arima_optimal, fc_ets_optimal, fc_dynamic_naive) %>%
  as_tibble() %>% 
  dplyr::select(-cpi, -export, -Model,-unemployed) %>% 
  pivot_wider(names_from = .model, values_from = .mean) %>% 
  rowwise() %>% 
  mutate(mean_fc = mean(ARIMA_dynamic, ARIMA_optimal, ETS_optimal))







# Advanced combination method: difference in AIC
fit_dynamic_arima %>% 
  accuracy()
k <- nrow(coefficients(report(fit_dynamic_arima)))
k
AIC(fit_dynamic_arima$ARIMA_dynamic, k = 8)

extractAIC(fit_dynamic_arima$ARIMA_dynamic[[1]]$model[2])

fc_arima_optimal %>% 
  accuracy(unemployment_train_ts)


comb_aic_fc <- bind_rows(fc_arima_optimal, fc_ets_optimal, fc_dynamic_naive) %>%
  as_tibble() %>% 
  dplyr::select(-cpi, -export, -Model,-unemployed) %>% 
  pivot_wider(names_from = .model, values_from = .mean) %>% 
  rowwise() %>% 
  mutate(mean_fc = mean(ARIMA_dynamic, ARIMA_optimal, ETS_optimal))






