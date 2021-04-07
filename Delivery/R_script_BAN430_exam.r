###############################################################################
################ Working Directory for Windows and Mac ########################
###############################################################################


# Choose the first if you use Mac OS and second if Windows
#Sys.setenv(X13_PATH = "../x13binary/bin")
#Sys.setenv(X13_PATH = "../windows_x13/bin")

################################################################################
############################## Package installation ############################
################################################################################
" Installing the X13 binary files needed to perform X13-SEATS decomposition"
#install.packages("seasonal", type = "source") 

install.packages(c("ffp3", "readxl", "vars", "lubridate",
                   "magrittr", "tidyverse", "forecast", "feasts",
                   "janitor", "seasonal", "x13binary", "kableExtra", "tseries",
                   "urca", "latex2exp", "tsDyn", "bvartools"))

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
######################## forecast::forecastING OF DECOMPOSITION ########################
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


