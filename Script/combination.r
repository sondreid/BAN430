########################################################################
#################### COMBINE FORECASTS  ################################
########################################################################

#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Script")
#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
# Sourcing data from data.r 
source("data.r")

" Average of best three models "
load(file = "../Data/optimal_models.Rdata")
load("../Data/fit_dynamic_arima.Rdata")
fit_dynamic_arima
load("../Data/arima_optimal.Rdata")
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
  mutate(mean_fc = mean(c(ARIMA_dynamic, ARIMA_optimal, ETS_optimal)))

vec_mean_fc <- c(unemployment_test$unemployed- comb_mean_fc$mean_fc)
mean_weighted_fc <- bind_cols(
  Model = "Mean weighted",
  RMSE = RMSE(vec_mean_fc),
  MASE = MASE(.resid =  vec_mean_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12),
  MAE = MAE(vec_mean_fc),
  MAPE = fabletools::MAPE(vec_mean_fc, .actual = c(unemployment_test_ts$unemployed)),
  RMSSE = RMSSE(.resid =  vec_mean_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12),
  ) 

mean_weighted_fc %>% 
  kbl(caption = "Combined forecast accuracy", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")


#######################################################################################################
#################### Advanced combination method: MSE (Stock and Watson(2001)) ########################
#######################################################################################################
mse_arima_optimal <- mean((augment(fit_arima_optimal)$.fitted - unemployment_train_ts$unemployed)^2)
mse_ets_optimal <- mean((augment(fit_ets_optimal)$.fitted - unemployment_train_ts$unemployed)^2)
mse_dynamic_arima <- mean((augment(fit_dynamic_arima)$.fitted  - unemployment_train_ts$unemployed)^2)

w_sum <- sum(1/mse_arima_optimal, 1/mse_ets_optimal, 1/mse_dynamic_arima)

w_mse_arima_optimal <- (1/mse_arima_optimal)/w_sum
w_mse_ets_optimal <- (1/mse_ets_optimal)/w_sum
w_mse_dynamic_arima <- (1/mse_dynamic_arima)/w_sum


w_mse_ets_optimal
w_mse_arima_optimal
w_mse_dynamic_arima

comb_mse_fc <- bind_rows(fc_arima_optimal, fc_ets_optimal, fc_dynamic_naive) %>%
  as_tibble() %>% 
  dplyr::select(-cpi, -export, -Model,-unemployed) %>% 
  pivot_wider(names_from = .model, values_from = .mean) %>% 
  rowwise() %>% 
  mutate(mse_fc = sum(w_mse_arima_optimal * ARIMA_optimal, 
                       w_mse_ets_optimal * ETS_optimal, 
                       w_mse_dynamic_arima * ARIMA_dynamic))


vec_mse_fc <- c(unemployment_test$unemployed - comb_mse_fc$mse_fc) # residuals vector

mse_weighted_fc <- bind_cols(
  Model = "MSE weighted",
  RMSE = RMSE(vec_mse_fc),
  MASE = MASE(.resid =  vec_mse_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12),
  MAE = MAE(vec_mse_fc),
  MAPE = fabletools::MAPE(vec_mse_fc, .actual = c(unemployment_test_ts$unemployed)),
  RMSSE = RMSSE(.resid =  vec_mse_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12),
  ) 

mse_weighted_fc %>% 
  kbl(caption = "Combined forecast accuracy", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")



#######################################################################################################
############################## Advanced combination method: AIC #######################################
#######################################################################################################
aic_fit_arima_optimal <- glance(fit_arima_optimal)$AIC 
aic_fit_ets_optimal <- glance(fit_ets_optimal)$AIC
aic_fit_dynamic_arima <- glance(fit_dynamic_arima)$AIC

aic_min <- min(aic_fit_arima_optimal, aic_fit_ets_optimal, aic_fit_dynamic_arima)

delta_w_fit_arima_optimal <- aic_fit_arima_optimal - aic_min
delta_w_aic_fit_ets_optimal <- aic_fit_ets_optimal - aic_min
delta_w_aic_fit_dynamic_arima <- aic_fit_dynamic_arima - aic_min

exp_delta_w_fit_arima_optimal <- exp(-0.5 * delta_w_fit_arima_optimal)
exp_delta_w_aic_fit_ets_optimal <- exp(-0.5 * delta_w_aic_fit_ets_optimal)
exp_delta_w_aic_fit_dynamic_arima <- exp(-0.5 * delta_w_aic_fit_dynamic_arima)

sum_exp_delta_w <- sum(exp_delta_w_fit_arima_optimal, 
                       exp_delta_w_aic_fit_ets_optimal, 
                       exp_delta_w_aic_fit_dynamic_arima)

w_aic_fit_arima_optimal <- exp_delta_w_fit_arima_optimal/sum_exp_delta_w
w_aic_fit_ets_optimal <- exp_delta_w_aic_fit_ets_optimal/sum_exp_delta_w
w_aic_fit_dynamic_arima <- exp_delta_w_aic_fit_dynamic_arima/sum_exp_delta_w

w_aic_fit_arima_optimal
w_aic_fit_ets_optimal
w_aic_fit_dynamic_arima


comb_aic_fc <- bind_rows(fc_arima_optimal, fc_ets_optimal, fc_dynamic_naive) %>%
  as_tibble() %>% 
  dplyr::select(-cpi, -export, -Model,-unemployed) %>% 
  pivot_wider(names_from = .model, values_from = .mean) %>% 
  rowwise() %>% 
  mutate(aic_fc = sum(w_aic_fit_arima_optimal * ARIMA_dynamic, 
                      w_aic_fit_ets_optimal * ARIMA_optimal, 
                      w_aic_fit_dynamic_arima * ETS_optimal))

vec_aic_fc <- c(unemployment_test$unemployed - comb_aic_fc$aic_fc) # residuals vector
aic_weighted_fc <- bind_cols(
  Model = "AIC weighted",
  RMSE = RMSE(vec_aic_fc), 
  MASE = MASE(.resid =  vec_aic_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12),
  MAE = MAE(.resid =  vec_aic_fc),
  MAPE = fabletools::MAPE(.resid =  vec_aic_fc, .actual = c(unemployment_test_ts$unemployed)),
  RMSSE = RMSSE(.resid =  vec_aic_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12),
  ) 

aic_weighted_fc %>% 
  kbl(caption = "Combined forecast accuracy", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")



#############################################################################################
#################### Comparison of combinational forecast methods ###########################
#############################################################################################

comparison_combined_forecast_accuracy <- bind_rows(
  mean_weighted_fc,
  mse_weighted_fc,
  aic_weighted_fc) %>% 
  arrange(MASE)

comparison_combined_forecast_accuracy %>% 
  kbl(caption = "Combined forecast accuracy", digits = 3) %>%
  kable_classic(full_width = F, html_font = "Times new roman")


ggplot() +
  geom_line(aes(x = date, y = mean_fc, color = "Mean Weighted"), data = comb_mean_fc) +
  geom_line(aes(x = date, y = mse_fc, color = "MSE Weighted"), data = comb_mse_fc) +
  geom_line(aes(x = date, y = aic_fc, color = "AIC Weighted"), data = comb_aic_fc) +
  geom_line(aes(x = date, y = unemployed, color = "Observed unemployment"), data = unemployment_test_ts %>% filter(year(date) >= 2015)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = c("red", "orange", "blue", "black")) +
  labs(title = "Combinational forecast",
       caption = paste(fit_arima_optimal$ARIMA_optimal,
                       " & ",
                       fit_ets_optimal$ETS_optimal,
                       " & ",
                       fit_dynamic_arima$ARIMA_dynamic), 
       x = "Month",
       y = "Unemployment level") +
  guides(colour = guide_legend(title = "Series:"))




save(mse_weighted_fc, file = "../Data/mse_weighted_fc.Rdata")



















