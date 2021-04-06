###############################################################################
########################## Multivariate models ################################
###############################################################################

#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Data/Script")
#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
# Sourcing data from data.r 
source("data.r")

###############################################################################
################## Stationarity test: Exports and CPI #########################
###############################################################################

# Joining unemployment data with consumer price index (cpi) and export
multivariate_data <- unemployment_train_ts %>% 
  dplyr::select(date, unemployed)  %>% 
  left_join(cpi_train, by = "date")  %>% 
  left_join(export_train, by = "date") 


# CPI: Autocorrelation plots
ggtsdisplay(multivariate_data$cpi, 
            plot.type = "partial", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "CPI")

# Export: Autocorrelation plots
ggtsdisplay(multivariate_data$export, 
            plot.type = "partial", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Exports")

# CPI: KPSS test. Outcome: 1.26% p-value, needs differencing
multivariate_data  %>% 
  features(cpi, unitroot_kpss)

# CPI: KPSS test. Outcome: 10% p-value, no need for further differencing
multivariate_data  %>% 
  features(export, unitroot_kpss)

# CPI: Autocorrelation plots after difference
ggtsdisplay(difference(multivariate_data$cpi), 
            plot.type = "partial", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Differenced CPI")

# Export: KPSS test. Outcome: 1% p-value, needs differencing
multivariate_data  %>% 
  features(export, unitroot_kpss) 

# Export: KPSS test. Outcome: 10% p-value, no need for further differencing
multivariate_data  %>% 
  features(difference(export), unitroot_kpss)

# Export: Autocorrelation plots after difference

diff_ex <- diff(diff((multivariate_data$export)))
diff_cpi <- diff(diff((multivariate_data$cpi)))
diff_unemp <- diff((multivariate_data$unemployed)) 

unitroot_kpss(diff_ex)
unitroot_kpss(diff_cpi)
unitroot_kpss(diff_unemp)



################################################################################
############# Stationarity fix: Unemployed, Export & CPI #######################
################################################################################

multivariate_data_stationary <- multivariate_data  %>% 
    mutate(diff_unemployed = difference(unemployed),
           diff_diff_export                   = difference(export),
           diff_diff_cpi                      = difference(cpi))  %>% 
    dplyr::select(date, diff_unemployed, diff_diff_export, diff_diff_cpi)  %>% 
    as_tsibble()  
    
 multivariate_data_stationary   %<>% 
    filter(date > yearmonth("2000-02-01"))
 

##################################################################################
########### Multivariate forecast with stationary data: VAR ######################
##################################################################################

# VAR: optimal model by AICc and BIC
fit_multivariate_var <- multivariate_data_stationary %>% 
  model(VAR_aicc = fable::VAR(vars(diff_unemployed , diff_diff_cpi, diff_diff_export), ic = "aicc"),
        VAR_bic = fable::VAR(vars(diff_unemployed , diff_diff_cpi, diff_diff_export), ic = "bic"))

# VAR: forecasts
fc_multivariate_var <- fit_multivariate_var  %>% 
  forecast(h = 24)  %>% 
  as_tsibble(index = date)

# VAR: forecast adjusted with level
forecast_level <- fc_multivariate_var %>% 
  dplyr::select(".mean_diff_unemployed")  %>% 
  rename(diff_unemployed = ".mean_diff_unemployed")  %>% 
  group_by(.model)  %>% 
  mutate(diff_unemployed = cumsum(diff_unemployed) +  multivariate_data$unemployed %>% tail(1)) ## Add level

# AICc and BIC optimized forecast, VAR(5) and VAR(4)           
forecast_level  %>% 
  autoplot() +
  autolayer(unemployment_test_ts %>% filter(year(date) > 2014)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Unemployment level",
       x = "Month",
       title = "Forecasting with VAR methods") +
  guides(colour = guide_legend(title = "Method:")) +
  scale_colour_manual(values=c("#56B4E9", "orange")) +
  guides(colour = guide_legend(title = "Series"))



##################################################################################
####################### Multivariate forecast accuracy: VAR ######################
##################################################################################


VARselect(multivariate_data_stationary[,2:4], lag.max =24, type="const")[["selection"]] # Confirming AR term
#Fit VAR(1)
var1 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 1, type="const")
var2 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 2, type="const")
var4 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 4, type="const")
var5 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 5, type="const")
var13 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 13, type="const")
var11 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 11, type="const")
"Failed portmanteau test: Set of autocorrelation tests most likely ljung box test for several variables"
serial.test(var4, lags.pt=24, type="PT.asymptotic")


aicc <- forecast_level %>% filter(.model == "VAR_aicc")
bic <- forecast_level %>% filter(.model == "VAR_bic")

var_aicc_resid <- c(unemployment_test$unemployed- aicc$diff_unemployed)
var_bic_resid  <- c(unemployment_test$unemployed- bic$diff_unemployed)
data.frame(Model = "Multivariate VAR model AICc optimized", 
            Type = "Test", 
            RMSE = RMSE(var_aicc_resid),
            MAE =  MAE(var_aicc_resid),
            MAPE = fabletools::MAPE(.resid = var_aicc_resid, .actual = c(unemployment_test$unemployed)),
            MASE =  MASE(.resid = var_aicc_resid, .train = c(unemployment_train_ts$unemployed), .period = 12),
            RMSSE = RMSSE(.resid = var_aicc_resid, .train = c(unemployment_train_ts$unemployed), .period = 12)) %>% 
    bind_rows( data.frame(Model = "Multivariate VAR model BIC optimized", 
            Type = "Test", 
            RMSE = RMSE(var_bic_resid),
            MAE =  MAE(var_bic_resid),
            MAPE = fabletools:: MAPE(.resid = var_bic_resid, .actual = c(unemployment_test$unemployed)),
            MASE = MASE(.resid = var_bic_resid, .train = c(unemployment_train_ts$unemployed), .period = 12),
            RMSSE = RMSSE(.resid = var_bic_resid, .train = c(unemployment_train_ts$unemployed), .period = 12)))%>% 
    kbl(caption = "Multivariate VAR models", digits = 2) %>%
    kable_classic(full_width = F, html_font = "Times new roman")



#################################################################################################
############################## Multivariate foorecast with VECM #################################
#################################################################################################
"Cointegrated stochastic trends --> VECM"

VARselect(multivariate_data[,2:4], lag.max = 12, type="const")[["selection"]] # Confirming AR term

########### Johansen test ######################
### Identify cointegration between variables
ca_jo <- ca.jo(multivariate_data[,2:4], ecdet = "const", type = "trace",
               K = 10, season = 12  ) ## k = 10 AR terms


summary(ca_jo)

var_vec <- vec2var(ca_jo, r =1)

"Failed portmanteau test: Set of autocorrelation tests most likely ljung box test for several variables"
serial.test(var_vec, lags.pt=24, type="PT.asymptotic")


################# VECM forecast ###############
fc_vecm <- predict(var_vec, n.ahead = 24)
fc_var_vec <- data.frame("date"= unemployment_test$date,
                      "unemployed" = (predict(var_vec, n.ahead = 24)[[1]]$unemployed)[,1:3])
colnames(fc_var_vec) <- c("date", "unemployed", "lower", "upper")



fc_var_vec  %>% 
    ggplot() +
    geom_line(aes(x = date, y = unemployed, color = "VECM model")) +
    geom_line(aes(x = date, y = unemployed, color = "Observed unemployment"), data = unemployment_test_ts %>% filter(year(date) > 2014)) +   
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper), alpha = 0.15, colour = "orange") + 
    scale_colour_manual(values=c("black", "orange")) +
    theme_bw() + 
    labs(title = "VECM forecast", y = "Unemployment level", x = "Month") +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(title = "Series"))

#################Performance metrics table ###############

vecm_resids <- (unemployment_test$unemployed - fc_var_vec$unemployed)
data.frame(Model = "Multivariate VAR model AICc optimized VAR(5)", 
            Type = "Test", 
            RMSE = RMSE(var_aicc_resid),
            MAE =  MAE(var_aicc_resid),
            MAPE = fabletools::MAPE(.resid = var_aicc_resid, .actual = c(unemployment_test$unemployed)),
            MASE =  MASE(.resid = var_aicc_resid, .train = c(unemployment_train_ts$unemployed), .period = 12),
            RMSSE = RMSSE(.resid = var_aicc_resid, .train = c(unemployment_train_ts$unemployed), .period = 12)) %>% 
    bind_rows( data.frame(Model = "Multivariate VECM model VAR(10)", 
            Type = "Test", 
            RMSE = RMSE(vecm_resids),
            MAE =  MAE(vecm_resids),
            MAPE = fabletools:: MAPE(.resid = vecm_resids, .actual = c(unemployment_test$unemployed)),
            MASE = MASE(.resid = vecm_resids, .train = c(unemployment_train_ts$unemployed), .period = 12),
            RMSSE = RMSSE(.resid = vecm_resids, .train = c(unemployment_train_ts$unemployed), .period = 12)),
            data.frame(Model = "Multivariate VAR model BIC optimized VAR(3)", 
                       Type = "Test", 
                       RMSE = RMSE(var_bic_resid),
                       MAE =  MAE(var_bic_resid),
                       MAPE = fabletools:: MAPE(.resid = var_bic_resid, .actual = c(unemployment_test$unemployed)),
                       MASE = MASE(.resid = var_bic_resid, .train = c(unemployment_train_ts$unemployed), .period = 12),
                       RMSSE = RMSSE(.resid = var_bic_resid, .train = c(unemployment_train_ts$unemployed), .period = 12))) %>%
    arrange(MASE) %>% 
    kbl(caption = "Multivariate VAR models", digits = 2) %>%
    kable_classic(full_width = F, html_font = "Times new roman")


###################################################################################################
############################### Dynamic regression: ARIMA #########################################
###################################################################################################

#Interpretation of task:
#Forecast (using a fitting ARIMA model) each of the other two variable. Then do a combined forecast

################################
# Forecasting of each regressors
################################

# CPI: fit models
#fit_cpi <- multivariate_data  %>% 
#    model(arima = ARIMA(cpi, stepwise = FALSE, approximation = FALSE),
#          ets   = ETS(cpi),
#          naive = NAIVE(cpi),
#          snaive= SNAIVE(cpi),
#          mean  = MEAN(cpi),
#          drift = RW(cpi ~ drift()))

# Export: fit models
#fit_export <-  multivariate_data  %>% 
#    model(arima = ARIMA(export, stepwise = FALSE, approximation = FALSE),
#          ets   = ETS(export),
#          naive = NAIVE(export),
#          snaive= SNAIVE(export),
#          mean  = MEAN(export),
#          drift = RW(export ~ drift()))

# Saving the fitted models to RData file
#save(fit_cpi, fit_export, file = "../Data/dynamic_regression_export_cpi_fit.Rdata")

# Load the fitted models
load("../Data/dynamic_regression_export_cpi_fit.Rdata")

fit_cpi
fit_export

# CPI: forecast
fc_cpi <- fit_cpi  %>% 
  forecast(h = 24)

# Export: forecast
fc_export <- fit_export  %>% 
  forecast(h = 24)

# CPI: traning accuracy, outcome: ARIMA(2,1,3)(0,0,1)[12]
fit_cpi  %>% 
  accuracy()  %>% 
  arrange(MASE)  %>% 
  dplyr::select(-ME, -ACF1) %>% 
  kbl(caption = "Model fitting of predictor: CPI", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")


# Export: training accuracy, outcome: ARIMA(2,1,2)(0,0,2)[12] w/ drift
fit_export  %>% 
  accuracy()  %>% 
  arrange(MASE)   %>% 
  dplyr::select(-ME, -ACF1) %>% 
  kbl(caption = "Model fitting of predictor: Export", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")

# ARIMA optimized model by reducing AICc
fit_dynamic_arima <- multivariate_data %>%
   model(ARIMA_dynamic = ARIMA(unemployed ~ cpi + export, stepwise = FALSE, approximation = FALSE))

save(fit_dynamic_arima, file = "../Data/fit_dynamic_arima.Rdata")
#load("../Data/fit_dynamic_arima.Rdata")
fit_dynamic_arima
report(fit_dynamic_arima)

# ARIMA forecasts of CPI and Export 
fc_predictors_arima <- new_data(fit_dynamic_arima  %>% augment(), 24)  %>% 
  left_join(fc_cpi  %>%  filter(.model == "arima"), by = "date")  %>% 
  left_join(fc_export %>%  filter(.model == "arima"), by = "date")  %>% 
  dplyr::select(-.model.x, -.model.y, -.model, -cpi, -export)  %>% 
  rename(cpi = .mean.x,
         export = .mean.y)  

# Forecast of Unemployment level with forecasted predictors using ARIMA method
fc_dynamic_arima_forecastobject <- forecast(fit_dynamic_arima,
                             new_data = fc_predictors_arima) %>% 
  mutate(Model = c("Predictor ARIMA"))

fc_dynamic_arima <- forecast(fit_dynamic_arima,
                             new_data = fc_predictors_arima) %>% 
  mutate(Model = c("Predictor ARIMA")) %>% 
  as_tibble(index = date)

# Plot: Forecast of Unemployment level with forecasted predictors using ARIMA method
# fc_dynamic_arima %>% 
#   ggplot() +
#   geom_line(aes(x = date, y  = .mean, color = "Multivariate")) +
#   geom_line(aes(x = date, y = unemployed, color = "Observed"), data = unemployment) +
#   theme_bw() +
#   scale_colour_manual(values=c("#56B4E9", "black")) +
#   theme(legend.position = "bottom") +
#   labs(title = "Dynamic forecaste",
#        y = "Unemployment level",
#        x = "Month") +
#   guides(colour = guide_legend(title = "Series"))


# NAIVE forecast of CPI and Export
fc_predictors_naive <- new_data(fit_dynamic_arima  %>% augment(), 24)  %>% 
  left_join(fc_cpi  %>%  filter(.model == "naive"), by = "date")  %>% 
  left_join(fc_export %>%  filter(.model == "naive"), by = "date")  %>% 
  dplyr::select(-.model.x, -.model.y, -.model, -cpi, -export)  %>% 
  rename(cpi = .mean.x,
         export = .mean.y) 

# Forecast of Unemployment level with forecasted predictors using NAIVE method
fc_dynamic_naive_forecastobject <- forecast(fit_dynamic_arima, 
                             new_data = fc_predictors_naive)  %>% 
  mutate(Model = c("Predictor NAIVE"))

fc_dynamic_naive <- forecast(fit_dynamic_arima, 
                             new_data = fc_predictors_naive)  %>% 
  mutate(Model = c("Predictor NAIVE")) %>% 
  as_tibble(index = date)





# Plot: Forecast of Unemployment level with forecasted predictors using NAIVE method
# fc_dynamic_naive %>% 
#   ggplot() +
#   geom_line(aes(x = date, y  = .mean, color = "Multivariate")) +
#   geom_line(aes(x = date, y = unemployed, color = "Observed"), data = unemployment) +
#   theme_bw() +
#   scale_colour_manual(values=c("#56B4E9", "black")) +
#   theme(legend.position = "bottom") +
#   labs(title = "Dynamic forecaste",
#        y = "Unemployment level",
#        x = "Month") +
#   guides(colour = guide_legend(title = "Series"))

fc_dynamic <- bind_rows(
                        fc_dynamic_arima,
                        fc_dynamic_naive)

# Forecast of ARIMA model with ARIMA and NAIVE forecasted predictors
fc_dynamic %>% 
  ggplot() +
  geom_line(aes(x = date, y  = .mean, color = Model)) +
  geom_line(aes(x = date, y = unemployed, color = "Observed"), data = unemployment_test_ts) +
  theme_bw() +
  scale_colour_manual(values=c("black", "#56B4E9", "orange")) +
  theme(legend.position = "bottom") +
  labs(title = "Dynamic forecast",
       subtitle = fit_dynamic_arima$`ARIMA(unemployed ~ cpi + export, stepwise = FALSE, approximation = FALSE)`,
       y = "Unemployment level",
       x = "Month") +
  guides(colour = guide_legend(title = "Series"))


# Plot with prediciton interval 95%
unemployment_test_ts %>% 
  autoplot() +
  autolayer(fc_dynamic_arima_forecastobject, colour = "blue", level = 95, alpha = 0.5) +
  autolayer(fc_dynamic_naive_forecastobject, colour = "orange", level = 95, alpha = 0.2) +
  theme_bw() +
  scale_colour_manual(values=c("black", "#56B4E9", "orange")) +
  theme(legend.position = "bottom") +
  labs(title = "Dynamic forecast",
       y = "Unemployment level",
       x = "Month") 


# Forecast: accuracy
bind_rows(
  fc_dynamic_arima_forecastobject %>% accuracy(unemployment_test_ts) %>% mutate(Model = c("ARIMA forecast with ARIMA forecasted predictors")),
  fc_dynamic_naive_forecastobject %>% accuracy(unemployment_test_ts)  %>% mutate(Model = c("ARIMA forecast with NAIVE forecasted predictors"))) %>% 
  dplyr::select(-ME, -ACF1, -.model, -.type) %>% 
  arrange(MASE) %>% 
  relocate(Model)  %>% 
  kbl(caption = "Accuracy Dynamic Forecast", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")



Residual <- (fit_dynamic_arima  %>% augment())$.innov

ggtsdisplay(Residual, 
            plot.type = "histogram", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Residuals of multivariate model")


save(fc_dynamic_naive, fc_dynamic_arima, fit_dynamic_arima, file = "../Data/fit_dynamic_arima.Rdata")
