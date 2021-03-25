###############################################################################
########################## Multivariate models ################################
###############################################################################

# Sourcing data from data.r 
source("data.r")



###############################################################################
################## Stationarity test: Exports and CPI #########################
###############################################################################

# Joining unemployment data with consumer price index (cpi) and export
multivariate_data <- unemployment_train_ts %>% 
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

# CPI: Ljung-box test. Outcome: 0% p-value, clearly autocorrelation problems
multivariate_data  %>% 
    features(cpi, ljung_box, lag = 24, dof = 8)

# CPI: KPSS test. Outcome: 1.26% p-value, needs differencing
multivariate_data  %>% 
    features(cpi, unitroot_kpss)

# Export: Ljung-box test. Outcome: 0% p-value, clearly autocorrelation problems
multivariate_data  %>% 
    features(export, ljung_box, lag = 24, dof = 8)

# Export: KPSS test. Outcome: 1% p-value, needs differencing
multivariate_data  %>% 
    features(export, unitroot_kpss)



################################################################################
############# Stationarity fix: Unemployed, Export & CPI #######################
################################################################################

multivariate_data_stationary <- multivariate_data  %>% 
    mutate(diff_diff_seasonal_unemployed = difference(difference(unemployed, lag = 12)),
           diff_export                   = difference(export),
           diff_cpi                      = difference(cpi))  %>% 
           select(date,diff_diff_seasonal_unemployed, diff_export, diff_cpi)


################################################################################
######################## Multivariate forecast: VAR ############################
################################################################################

# Bic optimized model
fit_multivariate_var <- multivariate_data_stationary %>% 
    model(VAR = fable::VAR(vars(diff_diff_seasonal_unemployed, diff_cpi, diff_export), ic = "bic"))


library(vars)
test <- VARselect(multivariate_data_stationary[,2:5], lag.max =24, type="const")

test <- VAR(multivariate_data_stationary, p=1, type="const")


fc_multivariate_var <- fit_multivariate_var  %>% 
    forecast(h = 24)  %>% 
    as_tsibble(index = date)

fc_multivariate_var  %>% 
    ggplot() +
    geom_line(aes(x = date, y  = .mean_diff_diff_seasonal_unemployed, color = "Multivariate forecasts")) +
    geom_line(aes(x = date, y = unemployed, color = "Observed"), data = unemployment) +
    geom_line(aes(x = date, y = .fitted , color = "Fitted"), data = fit_multivariate_var %>% augment() %>% filter(.response == "unemployed")) +
    theme_bw() +
    scale_colour_manual(values=c("#56B4E9", "black", "#56e99b")) +
    theme(legend.position = "bottom") +
    labs(title = "Multivariate forecaste",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = guide_legend(title = "Series"))


#Portmantau residual tests











#####################################################################
#################### Dynamic regression: ARIMA ######################
#####################################################################




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

