############################# HISTORY ########################


unemployment_dynamic_data <- unemployment_train_ts %>% 
  left_join(cpi_train, by = "date")  %>% 
  left_join(export_train, by = "date")  %>% 
  select(-seasonal_unemployed)

fit_tslm <- unemployment_dynamic_data  %>% 
  model(ARIMA(unemployed ~ cpi + export + pdq(0,0,0) + PDQ(0,0,0)))

train_tslm_data <- fit_tslm  %>% 
  augment()

new_tslm_data <- new_data(train_tslm_data, 24)  %>% 
  mutate(cpi = mean(unemployment_dynamic_data$cpi),
         export = mean(unemployment_dynamic_data$export))  %>% 
  select(-.model)  

fc_tslm <- forecast(fit_tslm, new_data = new_tslm_data)

fc_tslm %>% 
  autoplot(unemployment_test_ts)


############################# TABLE comparison ##############################

multivariate_var_table_data <- data.frame(Model = "Multivariate VAR model", 
                                          Type = "Test", 
                                          RMSE = RMSE(unemployment_test$unemployed, fc_multivariate_var$.mean_unemployed),
                                          MAE =  MAE(unemployment_test$unemployed, fc_multivariate_var$.mean_unemployed),
                                          MAPE = MAPE(unemployment_test$unemployed, fc_multivariate_var$.mean_unemployed),
                                          MASE = MASE(unemployment_test$unemployed, fc_multivariate_var$.mean_unemployed, .period = 1),
                                          RMSSE = RMSSE(unemployment_test$unemployed, fc_multivariate_var$.mean_unemployed, .period = 1))

fc_multivariate_arima  %>% select(.model, date, .mean)  %>% 
  accuracy(unemployment_test_ts) %>% 
  rename("Model" = .model,
         "Type" = .type)  %>% 
  mutate(Model = "Multivariate ARIMA")  %>% 
  select(Model:MAE, MAPE, MASE, RMSSE, -ME) %>%
  bind_rows(multivariate_var_table_data) %>% 
  kbl(caption = "Multivariate ARIMA and Var models", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")












