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
  features(difference(cpi), unitroot_kpss)

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
ggtsdisplay(difference(multivariate_data$export), 
            plot.type = "partial", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Differenced Export")



################################################################################
############# Stationarity fix: Unemployed, Export & CPI #######################
################################################################################

multivariate_data_stationary <- multivariate_data  %>% 
    mutate(diff_diff_seasonal_unemployed = difference(difference(unemployed, lag = 12)),
           diff_export                   = difference(export),
           diff_cpi                      = difference(cpi))  %>% 
    dplyr::select(date, diff_diff_seasonal_unemployed, diff_export, diff_cpi)  %>% 
    as_tsibble()  
    
 multivariate_data_stationary   %<>% 
    filter(date > yearmonth("2001-01-01"))


##################################################################################
########### Multivariate forecast with stationary data: VAR ######################
##################################################################################

# VAR: optimal model by AICc and BIC
fit_multivariate_var <- multivariate_data_stationary %>% 
  model(VAR_aicc = fable::VAR(vars(diff_diff_seasonal_unemployed , diff_cpi, diff_export), ic = "aicc"),
        VAR_bic = fable::VAR(vars(diff_diff_seasonal_unemployed , diff_cpi, diff_export), ic = "bic"))

# VAR: forecasts
fc_multivariate_var <- fit_multivariate_var  %>% 
  forecast(h = 24)  %>% 
  as_tsibble(index = date)

# VAR: forecast adjusted with level
forecast_level <- fc_multivariate_var %>% 
  dplyr::select(".mean_diff_diff_seasonal_unemployed")  %>% 
  rename(diff_unemployed = ".mean_diff_diff_seasonal_unemployed")  %>% 
  group_by(.model)  %>% 
  mutate(diff_unemployed = cumsum(diff_unemployed) +  multivariate_data$unemployed %>% tail(1)) ## Add level

# AICc and BIC optimized forecast, VAR(5) and VAR(1)           
forecast_level  %>% 
  autoplot() +
  autolayer(unemployment_test_ts) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Unemployment level",
       x = "Month",
       title = "Forecasting with VAR methods") +
  guides(colour = guide_legend(title = "Method:")) +
  scale_colour_manual(values=c("#56B4E9", "orange")) 



##################################################################################
####################### Multivariate forecast accuracy: VAR ######################
##################################################################################


VARselect(multivariate_data_stationary[,2:4], lag.max =24, type="const")[["selection"]] # Confirming AR term
#Fit VAR(1)
var1 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 1, type="const")
var2 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 2, type="const")
var5 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 5, type="const")
var20 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 20, type="const")

"Failed portmanteau test: Set of autocorrelation tests most likely ljung box test for several variables"
serial_port_test <- (serial.test(var20, lags.pt=24, type="PT.asymptotic"))$serial
data.frame("Chi-squared" = serial_port_test[1],
           "df"         = serial_port_test[2],
           "p.value"    = serial_port_test[3]) %>% kbl()


forecast(object= var2, h = 24) %>%
  autoplot() + xlab("Month")



aicc <- forecast_level %>% filter(.model == "VAR_aicc")
bic <- forecast_level %>% filter(.model == "VAR_bic")

data.frame(Model = "Multivariate VAR model AICc optimized", 
            Type = "Test", 
            RMSE = RMSE(unemployment_test$unemployed, aicc$diff_unemployed),
            MAE =  MAE(unemployment_test$unemployed, aicc$diff_unemployed),
            MAPE = MAPE(unemployment_test$unemployed, aicc$diff_unemployed),
            MASE = MASE(unemployment_test$unemployed, aicc$diff_unemployed, .period = 12),
            RMSSE = RMSSE(unemployment_test$unemployed, aicc$diff_unemployed, .period = 12)) %>% 
    bind_rows( data.frame(Model = "Multivariate VAR model BIC optimized", 
            Type = "Test", 
            RMSE = RMSE(unemployment_test$unemployed, bic$diff_unemployed),
            MAE =  MAE(unemployment_test$unemployed, bic$diff_unemployed),
            MAPE = MAPE(unemployment_test$unemployed, bic$diff_unemployed),
            MASE = MASE(unemployment_test$unemployed, bic$diff_unemployed, .period = 12),
            RMSSE = RMSSE(unemployment_test$unemployed, bic$diff_unemployed, .period = 12))) %>% 
    kbl(caption = "Multivariate VAR models", digits = 2) %>%
    kable_classic(full_width = F, html_font = "Times new roman")





#################################################################################################
############################## Multivariate foorecast with VECM #################################
#################################################################################################
"Cointegrated stochastic trends --> VECM"

# Johansen test
### Identify cointegration between variables
ca_jo <- ca.jo(multivariate_data[,2:4], ecdet = "const", type = c("eigen", "trace"),
               K = 5, spec = "longrun") ## k = 5 AR terms

summary(ca_jo)

vecm_seas_constant <- gen_vec(data = ts(multivariate_data[,2:4]),
                              p = 5, r = 1)

VECM_model_var_5 <- VECM(multivariate_data[,2:4], lag = 3, r = 1, exogen = vecm_seas_constant, estim = "ML")


var_vec <- vec2var(ca_jo, r =1)
summary(var_vec)
serial.test(var_vec, lags.pt=24, type="PT.asymptotic")
plot(predict(var_vec, n.ahead = 24))


fc_vecm <- predict(var_vec, n.ahead = 24)
fc_var_vec <- tsibble("date"= unemployment_test$date,
                      "unemployed" = (predict(var_vec, n.ahead = 24)[[1]]$unemployed)[,1:3])
colnames(fc_var_vec) <- c("date", "unemployed", "lower", "upper")

MASE(fc_var_vec$unemployed[,"fcst"], unemployment_test$unemployed, .period = 12)


fc_var_vec  %>% 
    ggplot() +
    geom_line(aes(x = date, y = unemployed[,"fcst"], color = "VECM model")) +
    geom_line(aes(x = date, y = unemployed, color = "Observed unemployment"), data = unemployment_test_ts) +
    geom_line(aes(x = date, y = unemployed[,"lower"], color = "Lower")) + 
    geom_line(aes(x = date, y = unemployed[,"upper"], color = "Upper")) 



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
  dplyr::select(-ME, -ACF1)

# Export: training accuracy, outcome: ARIMA(2,1,2)(0,0,2)[12] w/ drift
fit_cpi  %>% 
  accuracy()  %>% 
  arrange(MASE)   %>% 
  dplyr::select(-ME, -ACF1)

# ARIMA optimized model by reducing AICc
# fit_dynamic_arima <- multivariate_data %>% 
#   model(ARIMA(unemployed ~ cpi + export, stepwise = FALSE, approximation = FALSE))

#save(fit_dynamic_arima, file = "../Data/fit_dynamic_arima.Rdata")
load("../Data/fit_dynamic_arima.Rdata")

report(fit_dynamic_arima)

# ARIMA forecasts of CPI and Export 
fc_predictors_arima <- new_data(fit_dynamic_arima  %>% augment(), 24)  %>% 
  left_join(fc_cpi  %>%  filter(.model == "arima"), by = "date")  %>% 
  left_join(fc_export %>%  filter(.model == "arima"), by = "date")  %>% 
  dplyr::select(-.model.x, -.model.y, -.model, -cpi, -export)  %>% 
  rename(cpi = .mean.x,
         export = .mean.y)  

# Forecast of Unemployment level with forecasted predictors using ARIMA method
fc_dynamic_arima <- forecast(fit_dynamic_arima, 
                                  new_data = fc_predictors_arima)  

# Plot: Forecast of Unemployment level with forecasted predictors using ARIMA method
fc_dynamic_arima %>% 
  ggplot() +
  geom_line(aes(x = date, y  = .mean, color = "Multivariate")) +
  geom_line(aes(x = date, y = unemployed, color = "Observed"), data = unemployment) +
  theme_bw() +
  scale_colour_manual(values=c("#56B4E9", "black")) +
  theme(legend.position = "bottom") +
  labs(title = "Dynamic forecaste",
       y = "Unemployment level",
       x = "Month") +
  guides(colour = guide_legend(title = "Series"))


# NAIVE forecast of CPI and Export
fc_predictors_naive <- new_data(fit_dynamic_arima  %>% augment(), 24)  %>% 
  left_join(fc_cpi  %>%  filter(.model == "naive"), by = "date")  %>% 
  left_join(fc_export %>%  filter(.model == "naive"), by = "date")  %>% 
  dplyr::select(-.model.x, -.model.y, -.model, -cpi, -export)  %>% 
  rename(cpi = .mean.x,
         export = .mean.y) 

# Forecast of Unemployment level with forecasted predictors using NAIVE method
fc_dynamic_naive <- forecast(fit_dynamic_arima, 
                                  new_data = fc_predictors_naive)  

# Plot: Forecast of Unemployment level with forecasted predictors using NAIVE method
fc_dynamic_naive %>% 
  ggplot() +
  geom_line(aes(x = date, y  = .mean, color = "Multivariate")) +
  geom_line(aes(x = date, y = unemployed, color = "Observed"), data = unemployment) +
  theme_bw() +
  scale_colour_manual(values=c("#56B4E9", "black")) +
  theme(legend.position = "bottom") +
  labs(title = "Dynamic forecaste",
       y = "Unemployment level",
       x = "Month") +
  guides(colour = guide_legend(title = "Series"))

# Forecast: accuracy
bind_rows(
  fc_dynamic_arima  %>% accuracy(unemployment_test_ts) %>% mutate(Model = c("Predictor ARIMA")),
  fc_dynamic_naive  %>% accuracy(unemployment_test_ts)  %>% mutate(Model = c("Predictor NAIVE"))) %>% 
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

fit_multivariate_arima_augment  %>% 
   features(.innov, ljung_box, lag = 24, dof = 4)


########################################################################
#################### COMBINE FORECASTS  ################################
########################################################################

" Average of best three models "
load(file = "../Data/optimal_models.Rdata")
library(opera)

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
