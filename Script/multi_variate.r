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
    select(date, unemployed)  %>% 
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



######################################################################################
#################### Non-statinary multivariate model ################################
######################################################################################
## Forecasting using vars package
library(vars)

#bic optimized
fit_multivariate_var <- multivariate_data %>% 
    model(VAR_aicc = fable::VAR(vars(unemployed, cpi, export), ic = "aicc"),
          VAR_aic = fable::VAR(vars(unemployed, cpi, export)~AR(13)),
          VAR_bic =  fable::VAR(vars(unemployed, cpi, export) ~AR(5:20), ic = "bic"))

VARselect(multivariate_data[,2:4], lag.max =24, type="const")[["selection"]]
#Fit VAR(1)
var1 <- vars:: VAR(ts(multivariate_data[,2:4]), p = 1, type="const")
var2 <- vars:: VAR(ts(multivariate_data[,2:4]), p = 2, type="const")
var4 <- vars:: VAR(ts(multivariate_data[,2:4]), p = 4, type="const")
var5 <- vars:: VAR(ts(multivariate_data[,2:4]), p = 5, type="const")


"Failed portmanteau test: Set of autocorrelation tests most likely ljung box test for several variables"
serial.test(var4, lags.pt=24, type="PT.asymptotic")

forecast(object= var4, h = 24) %>%
  autoplot() + xlab("Month")

fc_multivariate_var <- fit_multivariate_var  %>%  
    forecast(h = 24)  %>% 
    filter(.model == "VAR_aic") %>%
    as_tsibble(index = date)

                  
fc_multivariate_var  %>% 
    ggplot() +
    geom_line(aes(x = date, y  = .mean_unemployed , color = "Multivariate forecasts")) +
    geom_line(aes(x = date, y = unemployed, color = "Observed"), data = unemployment) +
    #geom_line(aes(x = date, y = .fitted , color = "Fitted"), data = fit_multivariate_var %>% augment() %>% filter(.response == "unemployed")) +
    theme_bw() +
    scale_colour_manual(values=c("#56B4E9", "black", "#56e99b")) +
    theme(legend.position = "bottom") +
    labs(title = "Multivariate forecaste",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = guide_legend(title = "Series"))






################################################################################
############# Stationarity fix: Unemployed, Export & CPI #######################
################################################################################

multivariate_data_stationary <- multivariate_data  %>% 
    mutate(diff_diff_seasonal_unemployed = difference(difference(unemployed, lag = 12)),
           diff_export                   = difference(export),
           diff_cpi                      = difference(cpi))  %>% 
    select(date, diff_diff_seasonal_unemployed, diff_export, diff_cpi)  %>% 
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
    guides(colour = guide_legend(title = "Method:"))


# VAR: accuracy
 RMSSE(forecast_level$diff_unemployed, unemployment_test_ts$unemployed, .period = 1)


















# Old plot
forecast_level   %>% 
    ggplot() +
    geom_line(aes(x = date, y  = diff_unemployed, color = "Multivariate forecasts")) +
    geom_line(aes(x = date, y = unemployed, color = "Observed"), data = unemployment  %>% filter(date > yearmonth("2001-01-01"))) +
    #geom_line(aes(x = date, y = .fitted , color = "Fitted"), data = fit_multivariate_var %>% augment() %>% filter(.response == "unemployed")) +
    theme_bw() +
    scale_colour_manual(values=c("#56B4E9", "black", "#56e99b")) +
    theme(legend.position = "bottom") +
    labs(title = "Multivariate forecaste",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = guide_legend(title = "Series"))





###############################

## Forecasting using vars package
library(vars)
VARselect(multivariate_data_stationary[,2:4], lag.max =24, type="const")[["selection"]]
#Fit VAR(1)
var1 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 1, type="const")
var2 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 2, type="const")
var5 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 5, type="const")


"Failed portmanteau test: Set of autocorrelation tests most likely ljung box test for several variables"
serial.test(var5, lags.pt=24, type="PT.asymptotic")

forecast(object= var2, h = 24) %>%
  autoplot() + xlab("Month")


#####################################################################
#################### Dynamic regression: ARIMA ######################
#####################################################################

"Interpretation of task:
Forecast (using a fitting ARIMA model) each of the other two variable. Then do a combined forecast"


fit_multivariate_arima <- multivariate_data %>% 
    model(ARIMA(unemployed ~ cpi + export))

report(fit_multivariate_arima)

fit_multivariate_arima_augment <- fit_multivariate_arima  %>% 
    augment()  

fc_multivariate_arima <- new_data(fit_multivariate_arima_augment, 24)  %>% 
    mutate(cpi = mean(multivariate_data$cpi),
           export = mean(multivariate_data$export))  %>% 
    select(-.model)  

fc_multivariate_arima <- forecast(fit_multivariate_arima, new_data = fc_multivariate_arima)  


fc_multivariate_arima %>% 
    ggplot() +
    geom_line(aes(x = date, y  = .mean, color = "Multivariate")) +
    geom_line(aes(x = date, y = unemployed, color = "Observed"), data = unemployment) +
    theme_bw() +
    scale_colour_manual(values=c("#56B4E9", "black")) +
    theme(legend.position = "bottom") +
    labs(title = "Multivariate forecaste",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = guide_legend(title = "Series"))

Residual <- fit_multivariate_arima_augment$.innov

ggtsdisplay(Residual, 
            plot.type = "histogram", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Residuals of multivariate model")

fit_multivariate_arima_augment  %>% 
    features(.innov, ljung_box, lag = 24, dof = 4)


########################################################################
#################### Forecast with CPI + EXPORT ########################
########################################################################
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

