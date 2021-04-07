###############################################################################
################            BAN430 exam R script                  #############
################################################################################


################################################################################
############################## Package installation ############################
################################################################################
" Installing the X13 binary files needed to perform X13-SEATS decomposition"
#install.packages("seasonal", type = "source") 

#install.packages(c("ffp3", "readxl", "vars", "lubridate",
 #                  "magrittr", "tidyverse", "forecast", "feasts",
 #                  "janitor", "seasonal", "x13binary", "kableExtra", "tseries",
 #                  "urca", "latex2exp", "tsDyn", "bvartools"))




################################################################################
############################## LIBRARIES #######################################
################################################################################
library(fpp3)
library(readxl)
library(vars)
library(lubridate)
library(magrittr)
library(tidyverse)
library(forecast)
library(feasts)
library(janitor)
library(seasonal)
library(x13binary)
library(kableExtra)
library(tseries)
library(urca) 
library(latex2exp)
library(tsDyn)
library(bvartools)

## Check X13 binary
checkX13()

###############################################################################
############################ LOAD DATA ########################################
###############################################################################

load(file = "unemployment_cpi_exports.Rdata")


#################################################################################
########################### Time series data sets ###############################
#################################################################################
   
unemployment_train <- unemployment  %>% 
    filter(year(date) <= 2017)  %>% 
    dplyr::select(date, unemployed, seasonal_unemployed)

unemployment_test <- unemployment  %>% 
    filter(year(date) > 2017)  %>% 
    dplyr::select(date, unemployed, seasonal_unemployed)

unemployment_train_ts <-  unemployment_train %>% 
    as_tsibble(index = date) 

unemployment_test_ts <- unemployment %>% 
    as_tsibble(index = date)

cpi_train <- 
    cpi_data  %>% 
    as_tsibble(index = date) %>% 
    filter(year(date) <= 2017)

export_train <- 
    export_data  %>% 
    as_tsibble(index = date) %>% 
    filter(year(date) <= 2017)

###################################################################################
############################ DESCRIPTIV STATISTICS ################################
###################################################################################

unemp_df %>%
    mutate(date = yearmonth(date))  %>% 
    filter(year(date) >= 2000)   %>% 
    dplyr::select(date, unemployed, seasonal_unemployed)   %>% 
    ggplot() +
    geom_line(aes(x = date, y = unemployed, col = "Unadjusted seasonal")) +
    geom_line(aes(x = date, y = seasonal_unemployed, col = "Adjusted seasonal")) +
    labs(title = "Unemployment in the USA",
         subtitle = "[2000-2020]",
         y = "Unemployment level",
         x = "Months") +
    guides(col = guide_legend(title = "Series:")) +
    theme_bw() +
    theme(legend.position = "bottom")

unemployment %>% 
    ggplot() +
    geom_line(aes(x = date, y = unemployed, col = "Unadjusted seasonal")) +
    geom_line(aes(x = date, y = seasonal_unemployed, col = "Adjusted seasonal")) +
    labs(title = "Unemployment in the USA",
         subtitle = "[2000-2019]",
         y = "Unemployment level",
         x = "Months") +
    guides(col = guide_legend(title = "Series:")) +
    theme_bw() +
    theme(legend.position = "bottom")


###################################################################################
############################## Summary statistics #################################
###################################################################################

# Minimum, 25%-percentile, Mean, Median, 75%-percentile and Maximum by month
unemployment_train  %>% 
    mutate(month = lubridate::month(date))  %>%
    group_by(month)  %>% 
    rename(Month = month) %>% 
    summarise( 
        Min = min(unemployed),
        "25%-percentil" = quantile(unemployed, 0.25),
        Mean = mean(unemployed),
        Median = median(unemployed),
        "75%entil" = quantile(unemployed, 0.75),
        Max = max(unemployed)) %>% 
    kbl(caption = "Summary statistics of unemployment level in US") %>%
    kable_classic(full_width = F, html_font = "Times new roman")

# Summary statistics for the whole period
unemployment_train  %>% 
    mutate(month = lubridate::month(date))  %>%
    rename(Month = month) %>% 
    summarise( 
        Min = min(unemployed),
        "25%-percentil" = quantile(unemployed, 0.25),
        Mean = mean(unemployed),
        Median = median(unemployed),
        "75%-percentil" = quantile(unemployed, 0.75),
        Max = max(unemployed)) %>% 
    kbl(caption = "Summary statistics of unemployment level in US") %>%
    kable_classic(full_width = F, html_font = "Times new roman")



ggtsdisplay(unemployment_train$unemployed, 
            plot.type = "histogram", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Unemployment")

# Subseries of the training set
unemployment_train_ts  %>%  
    gg_subseries(unemployed) +
    labs(x = "Month", 
         y = "Unemployment level",
         title = "Monthly subseries of Unemployment level in US") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



###############################################################################################
##########################  Decomposition by various methods ##################################
###############################################################################################

# x11 season
x11_seas <- seas(ts(unemployment_train %>% dplyr::select(unemployed), 
                    start = c("2000"), 
                    frequency = 12), 
                 x11 = "")


# x11 season
x13_seas <- seas(ts(unemployment_train %>% dplyr::select(unemployed), 
                    start = c("2000"), 
                    frequency = 12))

# x11 decomposing with seas
x11_dcmp <- data.frame(x11_seas) %>%
    left_join(dplyr::select(unemployment_train_ts, unemployed), 
              by = "date") %>% 
    dplyr::select(-adjustfac, -final)  %>% 
    mutate(date = yearmonth(date)) %>% 
    as_tsibble(index = date)

# x13 decomposing with seas
x13_dcmp <- data.frame(x13_seas) %>% 
    left_join(dplyr::select(unemployment_train_ts, unemployed), 
              by = "date") %>% 
    dplyr::select(-adjustfac, -final)  %>% 
    mutate(date = yearmonth(date))  %>%
    as_tsibble(index = date)

# STL decomposition
stl_dcmp <- unemployment_train_ts %>%
    model(
        STL(unemployed ~ trend(window = 21) + season(window = 13),
            robust = TRUE)) %>%
    components()



# Plot of the X11 decomposition VS Bureau adjustment
ggplot() +
    geom_line(aes(x = date, y = seasonal_unemployed, col = "Unemployment US SA"), data = unemployment_train_ts) +
    geom_line(aes(x = date, y = seasonaladj, col = "x11 SA"), data = x11_dcmp ) +
    labs(title = "Replication of the seasonal adjusted data",
         y     = "Seasonal Adjusted Unemployment level",
         x     = "Month") +
    guides(colour = guide_legend("Decomposition method:")) +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_colour_manual(values=c("black","orange", "red", "orange"))


# Compare RMSE to find closest fit to original seasonal adjusted unemployment data of US
t(bind_rows(
    "Model" = c("ME", "RMSE", "MAE",  "MPE", "MAPE" ),
    X11 = c((ts(x11_dcmp$seasonaladj) %>% accuracy(ts(unemployment_train_ts$seasonal_unemployed)))[1:5]) %>% round(2),
    X13 = c((ts(x13_dcmp$seasonaladj) %>% accuracy(ts(unemployment_train_ts$seasonal_unemployed)))[1:5]) %>% round(2),
    STL = c((ts(stl_dcmp$season_adjust) %>% accuracy(ts(unemployment_train_ts$seasonal_unemployed)))[1:5]) %>%  round(2)))  %>% 
    janitor::row_to_names(1) %>% 
    kbl(caption = "Evaluation metrics of decomposition methods", digits = 2) %>%
    kable_classic(full_width = F, html_font = "Times new roman")


# x11 decomposed plot
x11_dcmp %>% 
    dplyr::select(-seasonaladj) %>% 
    pivot_longer(cols = seasonal:unemployed,
                 names_to = "components",
                 values_to = "values") %>% 
    autoplot() +
    facet_grid(vars(components),
               scales = "free_y") +
    labs(title = "X11 decomposition of Unemployment US",
         subtitle = "Unemployed = Trend + Seasonal + Irregular",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = FALSE) +
    theme_bw()

########################################################################################
######################## Forecasting of X11 decomposed series ##########################
########################################################################################
# Choosing X11 because of best RMSE
# forecast::forecast individual components of the X11 decomposition



evaluate_forecast <- function(df, train = unemployment_train_ts$unemployed, test = unemployment_test$unemployed, column) {
    #' Function that calculates performance metrics for an input dataframe
    #' Calculates a vector of residuals based on the column of the dataframe specified in the 
    #' column parameter. 
    decompositon_fc_table <- data.frame("Model" = character(),
                                        "RMSE" = double(),
                                        "MASE" = double(),
                                        "MAE" = double(),
                                        "MAPE" = double(),
                                        "RMSSE" = double()) %>%  as_tibble()
    for (model in df$.model %>% unique()) {
        data = df %>%  filter(.model == model)
        resids = c(test) - c(data[column][[1]])
        decompositon_fc_table %<>% bind_rows(
            data.frame(Model = model,
                       RMSE = RMSE(resids),
                       MASE = MASE(resids, .train = train, .period = 12),
                       MAE =  MAE(.resid =  resids),
                       MAPE = fabletools::MAPE(.resid = resids, .actual = test),
                       RMSSE = RMSSE(resids, .train = train, .period = 12))
        )
    }
    return(decompositon_fc_table %>% arrange(MASE) )
}


################################################################################
######### Train and Test set of seasonal and seasonal adjusted components
################################################################################
# Decomposed in seasonal, trend and irregularities of testset
# Testset of the best decomposition method
x11_seas_test <- seas(ts(unemployment %>% dplyr::select(unemployed), 
                         start = c("2000"), 
                         frequency = 12), 
                      x11 = "")

x11_dcmp_test <- data.frame(x11_seas_test) %>%
    left_join(dplyr::select(unemployment_test_ts, unemployed), by = "date") %>% 
    dplyr::select(-adjustfac, -final,)  %>% 
    mutate(date = yearmonth(date)) %>% 
    as_tsibble(index = date)  %>% 
    pivot_longer(cols = seasonal:unemployed,
                 names_to = "components",
                 values_to = "values") 
# Test sets
x11_dcmp_seasonal_train <- x11_dcmp_test %>% 
    filter(components == "seasonal" & year(date) <= 2017)

x11_dcmp_seasonal_adjusted_train <- x11_dcmp_test %>% 
    filter(components == "seasonaladj" & year(date) <= 2017)

x11_dcmp_seasonal_test <- x11_dcmp_test %>% 
    filter(components == "seasonal" & year(date) >= 2018)

x11_dcmp_seasonal_adjusted_test <- x11_dcmp_test %>% 
    filter(components == "seasonaladj" & year(date) >= 2018)


# Train with mean, drift, naive, snaive, ets models 
x11_season <- x11_dcmp %>%
    dplyr::select(seasonal) %>% 
    model(Mean = MEAN(seasonal),
          Drift = RW(seasonal ~ drift()),
          Naive = NAIVE(seasonal),
          SNaive = SNAIVE(seasonal ~ lag("year")))

x11_seasonal_adjust <- x11_dcmp %>%
    dplyr::select(seasonaladj) %>% 
    model(Mean = MEAN(seasonaladj),
          Drift = RW(seasonaladj ~ drift()),
          Naive = NAIVE(seasonaladj),
          Arima = ARIMA(seasonaladj ~ PDQ(0,0,0), stepwise = FALSE, approximation = FALSE),
          ETS   = ETS(seasonaladj ~ season("N"), ic = "aicc"))
# Forecasts of seasonal component
fc_x11_season <- x11_season %>% forecast::forecast(h = 24)
#Forecast of seasonally adjusted component
fc_x11_seasonal_adjust <- x11_seasonal_adjust %>% forecast::forecast(h = 24)
# Formed decomposition forecastr
fc_combined <- fc_x11_season %>% left_join(fc_x11_seasonal_adjust, by = c("date", ".model"))

# Filter out SNaive forecast from seasonal component forecast
snaive  <-  (fc_x11_season %>% filter(.model == "SNaive"))$.mean

# Add ETS and SNaive forecast
fc_combined <- fc_x11_seasonal_adjust %>% 
    filter(.model %in% c("ETS")) %>% 
    mutate(.model = "ETS formed decomposition") %>% 
    mutate(.mean = .mean + snaive) 

### Training set of formed decomposition forecast
x11_train <- x11_dcmp %>%
    pivot_longer(cols = c("seasonal", "trend", "irregular", "unemployed"),
                 names_to = "components",
                 values_to = "values") %>% 
    dplyr::select(date, components, values)



#### Components facet plot
x11_train  %>% 
    filter(components != "seasonaladj") %>% 
    ggplot() +
    geom_line(aes(x = date, y = values, col = components)) +
    facet_grid(vars(components),
               scales = "free_y") +
    labs(title = "forecast::forecast with X11 decomposition",
         subtitle = "Unemployed = Trend + Seasonal + Irregular",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = guide_legend(title = "Model:")) +
    theme_bw()  +
    theme(legend.position = "bottom")

### Seasonal component plot

color_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                   "#0072B2", "#D55E00", "#CC79A7")
fc_x11_season  %>% 
    ggplot() + 
    geom_line(aes(x = date, y = values, color = "Original data"), data = x11_dcmp_test %>%  filter(year(date) > 2014, components == "seasonal")) +
    geom_line(aes(x = date, y = .mean, color = .model)) + 
    theme_bw() + 
    labs(title = "Seasonal component forecast::forecast", y = "Seasonal unemployment level", 
         x = "Month",
         subtitle = TeX("$\\hat{S_t}$")) +
    theme(legend.position = "bottom") +
    scale_colour_manual(values = color_palette) +
    guides(colour = guide_legend(title = "Series"))


## Seasonally adjusted component plot 

fc_x11_seasonal_adjust  %>% 
    ggplot() + 
    geom_line(aes(x = date, y = values, color = "Original data"), data = x11_dcmp_test %>%  filter(year(date) > 2014, components == "seasonaladj")) +
    geom_line(aes(x = date, y = .mean, color = .model)) + 
    theme_bw() + 
    labs(title = "Seasonally adjusted component forecast::forecast", y = "Seasonal unemployment level", x = "Month", 
         subtitle = TeX("$\\hat{A_t} = \\hat{T_t} + \\hat{R_t} $")) +
    theme(legend.position = "bottom") +
    scale_colour_manual(values = color_palette) +
    guides(colour = guide_legend(title = "Series"))


### Plot combined decomposition forecast::forecasting

fc_combined %>% 
    ggplot() + 
    geom_line(aes(x = date, y = .mean, color = .model)) + 
    geom_line(aes(x = date, y = unemployed, color = "Original data"), data = unemployment_test_ts %>%  filter(year(date) > 2014)) +
    theme_bw() + 
    labs(title = "X11 forecast::forecast", y = "Seasonal unemployment level", 
         x = "Month",
         subtitle = TeX("$\\hat{y_t} = \\hat{S_t} + \\hat{A_t} $")) +
    theme(legend.position = "bottom") +
    scale_colour_manual(values = color_palette) +
    guides(colour = guide_legend(title = "Series"))

################################################################################
######### Decomposition tables
################################################################################
## Season table

evaluate_forecast(df = fc_x11_season, column = ".mean", train = x11_dcmp_seasonal_train$values, test = x11_dcmp_seasonal_test$values) %>% 
    kable(caption = "Seasonal component forecast::forecast", digits = 3) %>%
    kable_classic(full_width = F, html_font = "Times new roman") 

## Seasonal adjusted table

evaluate_forecast(df = fc_x11_seasonal_adjust, column = ".mean", train = x11_dcmp_seasonal_adjusted_train$values, test = x11_dcmp_seasonal_adjusted_test$values) %>% 
    kable(caption = "Seasonally adjusted component forecast::forecast", digits = 3) %>%
    kable_classic(full_width = F, html_font = "Times new roman") 

## Decomposition forecast::forecast table
evaluate_forecast(fc_combined, column = ".mean") %>% 
    kable(caption = "X11 combined forecast::forecast", digits = 3) %>%
    kable_classic(full_width = F, html_font = "Times new roman") 

##################################################################################
################################ ETS model #######################################
##################################################################################

# Fitting the training set with the best ETS model by minimizing AICc
fit_ets <- unemployment_train_ts %>%
    dplyr::select(date, unemployed) %>% 
    model(ETS_optimal = ETS(unemployed, ic = "aicc"),
          "ETS(A,A,A)"  = ETS(unemployed ~ error("A") + trend("A") + season("N"),  ic = "aicc")
    ) 
fit_ets # Error: Additive, Trend: Additive damped, Seasonal: Additive
fit_ets_optimal <- unemployment_train_ts %>%
    dplyr::select(date, unemployed) %>% 
    model(ETS_optimal = ETS(unemployed)
    ) 

# Forecast optimal ets
fc_ets_optimal <-  fit_ets_optimal %>% forecast::forecast(h = 24)

#Plot with prediction intervals
fit_ets_optimal %>% 
    forecast(h = 24) %>% 
    autoplot(unemployment_test_ts %>% filter(year(date) >= 2015), 
             level = 95) +
    labs(title = "Forecast of Unemployment level with",
         subtitle = fit_ets_optimal$ETS_optimal,
         y = "Unemployment level", 
         x = "Month") +
    theme_bw() +
    scale_color_manual(values = color_palette) +
    theme(legend.position = "bottom") +
    guides(level = guide_legend(title = "Prediction interval %: "))


## Print coefficients
tidy(fit_ets) %>% 
    dplyr::select(-.model) %>%
    t() %>% 
    kbl(caption = "Coefficients of ETS(A,Ad,A)", digits = 2) %>%
    kable_classic(full_width = F, html_font = "Times new roman")


## ETS decompositon plot
fit_ets_optimal %>% 
    components() %>% 
    autoplot() +
    labs(x = "Month",
         y = "Unemployment level") +
    theme_bw()


## ETS residual plots
ggtsdisplay(residuals(fit_ets)$.resid, 
            plot.type = "histogram", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Residuals of ETS(A,Ad,A) model")



################################################################################
########################### ARIMA PREPARATION ##################################
################################################################################

unemployment_train_ts_stationarity <- unemployment_train_ts %>% 
    mutate(diff_season_unemployed = difference(unemployed, 12),
           diff_unemployed        = difference(unemployed),
           diff_diff_season_unemployed = difference(diff_season_unemployed))


ggtsdisplay(unemployment_train_ts_stationarity$unemployed, 
            plot.type = "partial", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Non-stationary Unemployment level in US")


# Unitroot KPSS test on unemployed
unemployment_train_ts_stationarity %>% 
    features(unemployed, unitroot_kpss)

unemployment_train_ts_stationarity %>% 
    features(diff_unemployed, unitroot_kpss) # p-value of 10%, no need for more differencing.

# Seasonal difference and first order difference solves stationarity issue
ggtsdisplay(unemployment_train_ts_stationarity$diff_diff_season_unemployed, 
            plot.type = "partial", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Difference of seasonal differenced Unemployment level in US")

# Determine optimal arima fit based on training data while optimizing aicc
fit_arima_optimal <- unemployment_train_ts %>% 
    dplyr::select(date, unemployed) %>% 
    model(ARIMA_optimal = ARIMA(unemployed, 
                                stepwise = FALSE,
                                approximation = FALSE))

# AR, difference, and MA terms
fit_arima_optimal

## Forecast using the optimal arima model
fc_arima_optimal <- fit_arima_optimal %>% 
    forecast::forecast(h = 24)

fc_arima_optimal %>% 
    autoplot(unemployment_test_ts %>% filter(year(date) >= 2015), 
             level = 95) +
    labs(title = "Forecast of Unemployment level with",
         subtitle = fit_arima_optimal$ARIMA_optimal,
         y = "Unemployment level", 
         x = "Month") +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(level = guide_legend(title = "Prediction interval %: "))




## Residual plot of optimal arima model
ggtsdisplay(augment(fit_arima_optimal)$.resid, 
            plot.type = "histogram", 
            lag.max = 24, 
            theme = theme_bw(),
            main = paste("Residuals of ", fit_arima_optimal$ARIMA_optimal, "model"))

## Confirm no residual autocorrelation
fit_arima_optimal %>% 
    augment() %>% 
    features(.innov, ljung_box, lag = 24, dof = 0) 


## Forecasting performance metrics

fc_arima_optimal %>%  accuracy(unemployment_test %>% as_tsibble())


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


multivariate_data  %>% 
    features(cpi, unitroot_kpss)

# CPI: KPSS test. Outcome: 10% p-value, no need for further differencing
multivariate_data  %>% 
    features(export, unitroot_kpss)

unitroot_kpss(multivariate_data$export) # Export: KPSS test. Outcome: 1% p-value, needs differencing
unitroot_kpss(multivariate_data$cpi)    # CPI: KPSS test. Outcome: 1.26% p-value, needs differencing 
unitroot_kpss(multivariate_data$unemployed) #Unemployewd: Outcome 1 % p-value, needs differencing

## Unit root KPSS tests after differencing
unitroot_kpss(diff(multivariate_data$export))
unitroot_kpss(diff(multivariate_data$cpi))
unitroot_kpss(diff(multivariate_data$unemployed))

# CPI: Autocorrelation plots after difference
ggtsdisplay(difference(multivariate_data$cpi), 
            plot.type = "partial", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Differenced CPI")

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
  forecast::forecast(h = 24)  %>% 
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
  autolayer(unemployment_test_ts %>% filter(year(date) >= 2015)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Unemployment level",
       x = "Month",
       title = "Forecasting with VAR methods") +
  guides(colour = guide_legend(title = "Method:")) +
  scale_colour_manual(values=c("#56B4E9", "orange")) +
  guides(colour = guide_legend(title = "Series"))

### Confirming stationarity BIC optimized

tseries::adf.test((fc_multivariate_var %>%  dplyr::filter(.model == "VAR_aicc"))$.mean_diff_unemployed)

tseries::adf.test((fc_multivariate_var %>%  dplyr::filter(.model == "VAR_bic"))$.mean_diff_unemployed)

### Autocorrelation tests
#Construct new VAR3 and VAR5 models as found by fable
var3 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 3, type="const")
var5 <- vars:: VAR(ts(multivariate_data_stationary[,2:4]), p = 5, type="const")
serial.test(var3, lags.pt=24, type="PT.asymptotic")
serial.test(var5, lags.pt=24, type="PT.asymptotic")

#################################################################################################
############################## Multivariate foorecast with VECM #################################
#################################################################################################

VARselect(multivariate_data[,2:4], lag.max = 12, type="const")[["selection"]] # Confirming AR term

########### Johansen test ######################
### Identify cointegration between variables
ca_jo <- ca.jo(multivariate_data[,2:4], ecdet = "const", type = "trace",
               K = 10, season = 12  ) ## k = 10 AR terms
# Find relationshiprank
summary(ca_jo)

# Create new VEC model based on the VAR(10) model with r = 1
var_vec <- vec2var(ca_jo, r =1)



################# VECM forecast ###############
fc_vecm <- predict(var_vec, n.ahead = 24)
fc_var_vec <- data.frame("date"= unemployment_test$date,
                      "unemployed" = (predict(var_vec, n.ahead = 24)[[1]]$unemployed)[,1:3])
colnames(fc_var_vec) <- c("date", "unemployed", "lower", "upper")


"Failed portmanteau test: Set of autocorrelation tests most likely ljung box test for several variables"
serial.test(var_vec, lags.pt=24, type="PT.asymptotic")


### Confirming stationarity

tseries::adf.test(fc_var_vec$unemployed)

#Calculate residuals for the two VAR models
aicc <- forecast_level %>% filter(.model == "VAR_aicc")
bic <- forecast_level %>% filter(.model == "VAR_bic")

var_aicc_resid <- c(unemployment_test$unemployed- aicc$diff_unemployed)
var_bic_resid  <- c(unemployment_test$unemployed- bic$diff_unemployed)

## Calculate residuals for VECM
vecm_resids <- (unemployment_test$unemployed - fc_var_vec$unemployed) 

fc_var_vec  %>% 
    ggplot() +
    geom_line(aes(x = date, y = unemployed, color = "VECM model")) +
    geom_line(aes(x = date, y = unemployed, color = "Observed unemployment"), data = unemployment_test_ts %>% filter(year(date) >= 2015)) +   
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper), alpha = 0.15, colour = "orange") + 
    scale_colour_manual(values=c("black", "orange")) +
    theme_bw() + 
    labs(title = "VECM forecast", y = "Unemployment level", x = "Month") +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(title = "Series")) +
    labs(title = "Forecast with VECM model",
         x = "Month",
         y = "Unemployment level")

#################################################################################################
############################## Performance metrics table #######################################
#################################################################################################

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


################################# Forecasting of each regressors ################################

# CPI: fit models
fit_cpi <- multivariate_data  %>% 
    model(arima = ARIMA(cpi, stepwise = FALSE, approximation = FALSE),
          ets   = ETS(cpi),
          naive = NAIVE(cpi),
          snaive= SNAIVE(cpi),
          mean  = MEAN(cpi),
          drift = RW(cpi ~ drift()))

# Export: fit models
fit_export <-  multivariate_data  %>% 
    model(arima = ARIMA(export, stepwise = FALSE, approximation = FALSE),
          ets   = ETS(export),
          naive = NAIVE(export),
          snaive= SNAIVE(export),
          mean  = MEAN(export),
          drift = RW(export ~ drift()))

# CPI: forecast
fc_cpi <- fit_cpi  %>% 
  forecast(h = 24)

# Export: forecast
fc_export <- fit_export  %>% 
  forecast(h = 24)

# CPI: traning accuracy, outcome: ARIMA(2,1,3)(0,0,1)[12]
fit_cpi  %>% 
  accuracy()  %>% 
  rename("Model" = .model) %>% 
  arrange(MASE)  %>% 
  dplyr::select(-ME, -ACF1) %>% 
  kbl(caption = "Model fitting of predictor: CPI", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")


# Export: training accuracy, outcome: ARIMA(2,1,2)(0,0,2)[12] w/ drift
fit_export  %>% 
  accuracy()  %>% 
  rename("Model" = .model) %>% 
  arrange(MASE)   %>% 
  dplyr::select(-ME, -ACF1) %>% 
  kbl(caption = "Model fitting of predictor: Export", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")

# ARIMA optimized model by reducing AICc
fit_dynamic_arima <- multivariate_data %>%
   model(ARIMA_dynamic = ARIMA(unemployed ~ cpi + export, stepwise = FALSE, approximation = FALSE))

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

fc_dynamic_naive <- forecast::forecast(fit_dynamic_arima, 
                             new_data = fc_predictors_naive)  %>% 
  mutate(Model = c("Predictor NAIVE")) %>% 
  as_tibble(index = date)

# Forecast of ARIMA model with ARIMA and NAIVE forecasted predictors
bind_rows(fc_dynamic_arima,
          fc_dynamic_naive) %>% 
  ggplot() +
  geom_line(aes(x = date, y  = .mean, color = Model)) +
  geom_line(aes(x = date, y = unemployed, color = "Observed"), data = unemployment_test_ts %>%  filter(year(date) > 2014)) +
  theme_bw() +
  scale_colour_manual(values=c("black", "#56B4E9", "orange")) +
  theme(legend.position = "bottom") +
  labs(title = "Dynamic forecast",
       subtitle = fit_dynamic_arima$`ARIMA(unemployed ~ cpi + export, stepwise = FALSE, approximation = FALSE)`,
       y = "Unemployment level",
       x = "Month") +
  guides(colour = guide_legend(title = "Series"))


### Dynamic arima residual plot
ggtsdisplay((fit_dynamic_arima  %>% augment())$.innov, 
            plot.type = "histogram", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Residuals of dynamic arima model")

###################################################################################################
############################### Forecast using deterministic trend ################################
###################################################################################################

## Fit arima model with linear deterministic trend
fit_deterministic <- unemployment_train_ts %>% 
  dplyr::select(date, unemployed) %>% 
  model("Deterministic trend" = ARIMA(unemployed ~ 1 + trend() + pdq(d = 0)))

fit_deterministic %>% report()

## Print kable table of coefficients
fit_deterministic %>% report() %>% coefficients() %>% dplyr::select(term, estimate) %>%  
  kbl(caption = "Deterministic trend", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")

# Forecast using arima model with linear determinstic trend
fc_deterministic <- fit_deterministic %>% 
  forecast(h = 24)


## Residuals of linear determinstic trend model
ggtsdisplay(Residuals, 
            plot.type = "histogram", 
            lag.max = 24,
            theme = theme_bw(),
            main = paste("Residuals of ARIMA ", fit_deterministic$`Deterministic trend`))

### Plot of forecast
fc_deterministic %>% 
  autoplot(unemployment_test_ts %>% filter(year(date) >= 2015), level = 95) +
  labs(title = "Deterministic trend forecast",
       subtitle = fit_deterministic$Deterministic,
       x = "Month",
       y = "Unemployment level") +
  theme_bw() +
  theme(legend.position = "bottom") + 
  guides(level = guide_legend(title = "Prediction level"))

fc_deterministic_table <- fc_deterministic %>% 
  accuracy(unemployment_test_ts) 


# Accuracy table
fc_deterministic_table   %>%  
  arrange(MASE) %>% 
  rename("Model"  = .model) %>% 
  dplyr::select(-.type, -ME, -ACF1) %>% 
  kbl(caption = "Deterministic forecast accuracy", digits = 3) %>%
  kable_classic(full_width = F, html_font = "Times new roman")

###################################################################################################
############################### Forecast using deterministic fourier trend ########################
###################################################################################################


fit_fourier <- unemployment_train_ts  %>% 
    dplyr::select(date, unemployed)   %>% 
    model("Fourier K1" = ARIMA(unemployed ~ fourier(K = 1)  + PDQ(0,0,0)),
          "Fourier K2" = ARIMA(unemployed ~ fourier(K = 2)  + PDQ(0,0,0)),
          "Fourier K3" = ARIMA(unemployed ~ fourier(K = 3)  + PDQ(0,0,0)),
          "Fourier K4" = ARIMA(unemployed ~ fourier(K = 4)  + PDQ(0,0,0)),
          "Fourier K5" = ARIMA(unemployed ~ fourier(K = 5)  + PDQ(0,0,0)),
          "Fourier K6" = ARIMA(unemployed ~ fourier(K = 6)  + PDQ(0,0,0)))

# ARIMA model with fourier terms coefficients
fit_fourier %>% 
  dplyr::select("Fourier K2") %>% 
  coefficients() %>% 
  dplyr::select(term, estimate) %>% 
  kbl(caption = "Fourier terms", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")

## Forecast using ARIMA model with fourier term
fc_fourier <- fit_fourier %>% 
    forecast(h = 24)

# Fourier table data
fc_fourier_table <- 
  fc_fourier %>% 
  accuracy(unemployment_test_ts)

####### Models with determinstic trends performance metrics  ############################
fc_fourier_table %>% 
  bind_rows(
  fc_deterministic_table)  %>% 
  rename("Model"  = .model) %>% 
  arrange(MASE) %>% 
  dplyr::select(-.type, -ME, -ACF1) %>% 
  kbl(caption = "Deterministic forecasting methods", digits = 3) %>%
  kable_classic(full_width = F, html_font = "Times new roman")


#######################################################################################################
############################ Combinational forecast: Mean TOP #########################################
#######################################################################################################

## Take mean of all forecasts
comb_mean_fc <- bind_rows(fc_arima_optimal, fc_ets_optimal, fc_dynamic_naive) %>%
  as_tibble() %>% 
  dplyr::select(-cpi, -export, -Model,-unemployed) %>% 
  pivot_wider(names_from = .model, values_from = .mean) %>% 
  rowwise() %>% 
  mutate(mean_fc = mean(c(ARIMA_dynamic, ARIMA_optimal, ETS_optimal)))


vec_mean_fc <- c(unemployment_test$unemployed- comb_mean_fc$mean_fc) # Residuals vector
#Performance metric table
mean_weighted_fc <- bind_cols(
  Model = "Mean weighted",
  RMSE = RMSE(vec_mean_fc),
  MASE = MASE(.resid =  vec_mean_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12),
  MAE = MAE(vec_mean_fc),
  MAPE = fabletools::MAPE(vec_mean_fc, .actual = c(unemployment_test_ts$unemployed)),
  RMSSE = RMSSE(.resid =  vec_mean_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12),
  ) 

#######################################################################################################
#################### Advanced combination method: MSE (Stock and Watson(2001)) ########################
#######################################################################################################

## Calculate mean squared errors for our chosen models
mse_arima_optimal <- mean((augment(fit_arima_optimal)$.fitted - unemployment_train_ts$unemployed)^2)
mse_ets_optimal <- mean((augment(fit_ets_optimal)$.fitted - unemployment_train_ts$unemployed)^2)
mse_dynamic_arima <- mean((augment(fit_dynamic_arima)$.fitted  - unemployment_train_ts$unemployed)^2)

w_sum <- sum(1/mse_arima_optimal, 1/mse_ets_optimal, 1/mse_dynamic_arima)

## Calculate weights
w_mse_arima_optimal <- (1/mse_arima_optimal)/w_sum
w_mse_ets_optimal <- (1/mse_ets_optimal)/w_sum
w_mse_dynamic_arima <- (1/mse_dynamic_arima)/w_sum

## Print weights
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
# Performance metrics table
mse_weighted_fc <- bind_cols(
  Model = "MSE weighted",
  RMSE = RMSE(vec_mse_fc),
  MASE = MASE(.resid =  vec_mse_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12),
  MAE = MAE(vec_mse_fc),
  MAPE = fabletools::MAPE(vec_mse_fc, .actual = c(unemployment_test_ts$unemployed)),
  RMSSE = RMSSE(.resid =  vec_mse_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12),
  ) 

#######################################################################################################
############################## Advanced combination method: AIC #######################################
#######################################################################################################

## Retrieve aic values for each of our chosen models
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

## Combine models
comb_aic_fc <- bind_rows(fc_arima_optimal, fc_ets_optimal, fc_dynamic_naive) %>%
  as_tibble() %>% 
  dplyr::select(-cpi, -export, -Model,-unemployed) %>% 
  pivot_wider(names_from = .model, values_from = .mean) %>% 
  rowwise() %>% 
  mutate(aic_fc = sum(w_aic_fit_arima_optimal * ARIMA_dynamic, 
                      w_aic_fit_ets_optimal * ARIMA_optimal, 
                      w_aic_fit_dynamic_arima * ETS_optimal))

vec_aic_fc <- c(unemployment_test$unemployed - comb_aic_fc$aic_fc) # residuals vector
#Performance metric table
aic_weighted_fc <- bind_cols(
  Model = "AIC weighted",
  RMSE = RMSE(vec_aic_fc), 
  MASE = MASE(.resid =  vec_aic_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12),
  MAE = MAE(.resid =  vec_aic_fc),
  MAPE = fabletools::MAPE(.resid =  vec_aic_fc, .actual = c(unemployment_test_ts$unemployed)),
  RMSSE = RMSSE(.resid =  vec_aic_fc,  .train = c(unemployment_train_ts$unemployed), .period = 12),
  ) 


#############################################################################################
#################### Comparison of combinational forecast methods ###########################
#############################################################################################

## Combine performance metrics
comparison_combined_forecast_accuracy <- bind_rows(
  mean_weighted_fc,
  mse_weighted_fc,
  aic_weighted_fc) %>% 
  arrange(MASE)

# Kable table
comparison_combined_forecast_accuracy %>% 
  kbl(caption = "Combined forecast accuracy", digits = 3) %>%
  kable_classic(full_width = F, html_font = "Times new roman")

## Plot combinational forecasts
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



########################################################################
#################### MONTECARLO SIMULATION #############################
########################################################################


generate_y <- function(fit, n) {
  #' Function that passes the standard deviation of the residuals of our optimal Arima model
  #' Automatically finds and passes ar and ma terms to the arima.sim (stats package), and
  #' returns the genereated series
  sigma <- sd(residuals(fit)$.resid)
  ar_terms <- (fit  %>% coefficients %>%  filter(str_detect(term, "ar")))$estimate %>% c(.) # AR terms and their coefficients
  sma_terms <- (fit  %>% coefficients %>%  filter(str_detect(term, "sma")))$estimate %>% c(.)
  arima_sim_model <- list(order = c(5, 1, 0), ar = ar_terms, sma = sma_terms)
  y <- arima.sim(n = n, arima_sim_model, sd = sigma)
  return(y)
}

### Example plot of a generated series based on optimal arima fit #####
data.frame(y = generate_y(fit_arima_optimal, 216)[1:216], date = unemployment_train_ts$date) %>% as_tsibble()  %>% 
  ggplot() +
  geom_line(aes(x = date, y = y, color = "Generated series")) +
  scale_colour_manual(values=c("black")) +
  theme_bw() + 
  theme(legend.position = "bottom") +    
  labs(title = "Sample generated series from estimated ARIMA model",
       y = "Generated values",
       x = "Month") +
  guides(colour = guide_legend(title = "Series"))


simulate <- function(fit, R, train_length , h ) {
    #' Function that generates a new series x based on an arima simulation returned by generate_y. 
    #' Compares two models, and populates which contains a series of forecast evaluation metrics.
    #' Returns the populated matrix.
    res <- matrix(0,2,5)
    colnames(res) <- c("RMSE", "MASE", "MAE", "MAPE",  "RMSSE")
    rownames(res) <- c("VAR multivariate", "ARIMA yt")
    for(i in 1:R){
        y <- diff(generate_y(fit, train_length+h))
        y_e <- y[1:train_length]
        y_t <- y[(train_length+1):(train_length+h)]
        x <- c()
        x[1] <- y[1]
        for (j in 2:(train_length+h)) {
            x[j] <- 0.5*y[j-1] + 0.5*x[j-1] + rnorm(n = 1, mean = 0, sd = sd(y))
        }
        x_e <- x[1:train_length]
        x_t <- x[(train_length+1):(train_length+h)]
        data_x_y = data.frame(date = (1:train_length), x_e = x_e, y_e = y_e)  %>%  as_tsibble(index = date)
        var_multi  <- vars:: VAR(data_x_y[,2:3], p  = 1,  type = "const")                              # VAR(1) model
        arima_uni <- data_x_y  %>% model(Arima =  ARIMA(y_e ~ 0 + pdq(1,0,0) + PDQ(0,0,0)))            # ARIMA pdq(1,1,0) model
        var_resids <-   y_t -  predict(var_multi, n.ahead = h)$fcst$y_e[,1]
        arima_resids <- y_t -  (arima_uni %>% forecast(h = h))$.mean                            
        
        res[1,1] <- res[1,1] + RMSE(var_resids)/R     
        res[2,1] <- res[2,1] + RMSE(arima_resids)/R  

        res[1,2] <- res[1,2] + MASE(.resid = var_resids, .train = y_e, .period = 12)/R   
        res[2,2] <- res[2,2] + MASE(.resid = arima_resids, .train = y_e, .period = 12)/R  
        
        res[1,3] <- res[1,3] + MAE(.resid = var_resids)/R   
        res[2,3] <- res[2,3] + MAE(.resid = arima_resids)/R   
        
        res[1,4] <- res[1,4] + fabletools::MAPE(.resid = var_resids, .actual = y_t, .period = 12)/R   
        res[2,4] <- res[2,4] + fabletools::MAPE(.resid = arima_resids, .actual = y_t, .period = 12)/R   
        
        
        res[1,5] <- res[1,5] + RMSSE(.resid = var_resids, .train = y_e, .period = 12)/R   
        res[2,5] <- res[2,5] + RMSSE(.resid = arima_resids, .train = y_e, .period = 12)/R   
    }
    return(res)
}


wrapperSim <- function(R, sample_size, test_ratio) {
        #' Wrapper function that splits the unemployment series into
        #' test and training lengths based on an input sample length and
        #' test ratio of the overall series length.
        #' Passes this as parameters to the simulate function
        cl <- parallel::makeCluster(parallel::detectCores())                                                                                         ### Make clusters
        doParallel::registerDoParallel(cl)
        train_length <- floor(sample_size * (1-test_ratio))
        h <- ceiling(sample_size* test_ratio)
        print(paste("Training length: ", train_length))
        print(paste("h : ", h))
        start <- (nrow(unemployment_train_ts) - sample_size)
        sim_res <- simulate(fit_arima_optimal, R, train_length, h) %>%
          as.data.frame() %>% 
          mutate("Sample length" = sample_size)
        parallel::stopCluster(cl)
        
        return(sim_res)
}

# Sample sizes for simulation
sample_sizes <- c(50,100, 150, 200)

# Add to table
table <- data.frame()

# Loop through all sample sizes, and perform a Monte Carlo simulation using 1000 samples of y and x
for (size in sample_sizes) {
  table <- table %>% rbind(., wrapperSim(R= 1000, sample_size = size, test_ratio = 0.2)) #Populate table for each sample size
}

#### Print kable table ####
table  %>% 
       kable(caption = "Monte Carlo simulations: 1000 sample paths ", digits = 3) %>%
       kable_classic(full_width = F, html_font = "Times new roman") 



########################################################################
#################### Optimal models table #############################
########################################################################

##### A final table containing all optimal models of all types visited in this report ####
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
    mse_weighted_fc,
    evaluate_forecast(fc_combined, ".mean") %>% slice(1) %>% mutate(Model = "ETS decompositon")
  ) %>% 
  dplyr:: select(Model, RMSE, MASE, MAE, MAPE, RMSSE)   %>% 
  arrange(MASE)


conclusion_table %>% 
  kable(caption = "Comparison of all optimal models discussed", digits = 3) %>%
  kable_classic(full_width = F, html_font = "Times new roman") 