###############################################################################
################ Working Directory for Windows and Mac ########################
###############################################################################

#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Data")

# Choose the first if you use Mac OS and second if Windows
#Sys.setenv(X13_PATH = "../x13binary/bin")
#Sys.setenv(X13_PATH = "../windows_x13/bin")


" For installing the seasonal package without manual binary file"
#install.packages("seasonal", type = "source") 


################################################################################
############################## LIBRARIES #######################################
################################################################################
library(fpp3)
library(readxl)
library(lubridate)
library(magrittr)
library(tidyverse)
library(forecast)
library(feasts)
library(seasonal)
#library(x13binary)
library(kableExtra)

#checkX13()


###############################################################################
############################ DATA RETRIEVAL ###################################
###############################################################################

unemp_df <- read_xls("../Data/US/unemployment_data.xls", sheet = 2)  %>% 
    `colnames<-`( c("date", 
                    "seasonal_unemp_men", 
                    "seasonal_unemp_women", 
                    "seasonal_unemp_less_high_school", 
                    "seasonal_unemp_high_school", 
                    "seasonal_unemp_college", 
                    "unemployed", 
                    "unemp_men" , 
                    "unemp_women", 
                    "unemp_less_high_school",
                    "unemp_high_school",
                    "unemp_college", 
                    "seasonal_unemployed"))


#################################################################################
########################### Time series data sets ###############################
#################################################################################
unemployment <- unemp_df  %>% 
    mutate(date = yearmonth(date))  %>% 
    filter(year(date) >= 2000 & year(date) <= 2019)   %>% 
    select(date, unemployed, seasonal_unemployed)   

unemployment_train <- unemployment  %>% 
    filter(year(date) <= 2017)  %>% 
    select(date, unemployed, seasonal_unemployed)

unemployment_test <- unemployment  %>% 
    filter(year(date) > 2017)  %>% 
    select(date, unemployed, seasonal_unemployed)

unemployment_train_ts <-  unemployment_train %>% 
    as_tsibble(index = date) 

unemployment_test_ts <- unemployment %>% 
    as_tsibble(index = date)


###################################################################################
############################ DESCRIPTIV STATISTICS ################################
###################################################################################

unemp_df %>%
    mutate(date = yearmonth(date))  %>% 
    filter(year(date) >= 2000)   %>% 
    select(date, unemployed, seasonal_unemployed)   %>% 
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

# Minimum, 25%-percentile, Mean, Median, 75%-percentile and Maximum BY MONTH
unemployment_train  %>% 
    mutate(month = lubridate::month(date))  %>%
    group_by(month)  %>% 
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

# SUMMARY STATISTICS FOR THE WHOLE PERIOD
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

# 
Unemployment <- unemployment_train$unemployed

ggtsdisplay(Unemployment, 
            plot.type = "histogram", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Unemployment")

# Subseries of the training set
unemployment_train_ts  %>%  
    gg_subseries(unemployed) +
    labs(x = "Month", 
         y = "Unemployment level") +
    theme_bw()

###############################################################################################
##########################  Decomposition by various methods ##################################
###############################################################################################

# x11 season
x11_seas <- seas(ts(unemployment_train %>% select(unemployed), 
                    start = c("2000"), 
                    frequency = 12), 
                 x11 = "")


# x11 season
x13_seas <- seas(ts(unemployment_train %>% select(unemployed), 
                    start = c("2000"), 
                    frequency = 12))

# x11 decompoising with feasts
x11_dcmp_feasts <- unemployment_train_ts  %>% 
    model(x11 = feasts:::X11(unemployed, 
                             type = "additive"))  %>% 
    components()

# x11 decomposing with seas
x11_dcmp <- data.frame(x11_seas) %>%
    left_join(select(unemployment_train_ts, unemployed), 
              by = "date") %>% 
    select(-adjustfac, -final)  %>% 
    mutate(date = yearmonth(date)) %>% 
    as_tsibble(index = date)

# x13 decomposing with seas
x13_dcmp <- data.frame(x13_seas) %>% 
    left_join(select(unemployment_train_ts, unemployed), 
              by = "date") %>% 
    select(-adjustfac, -final)  %>% 
    mutate(date = yearmonth(date))  %>%
    as_tsibble(index = date)

# STL decomposition
stl_dcmp <- unemployment_train_ts %>%
    model(
        STL(unemployed ~ trend(window = 21) + season(window = 13),
            robust = TRUE)) %>%
    components()

# Plot of the various decomposition methods
ggplot() +
    geom_line(aes(x = date, y = seasonal_unemployed, col = "Unemployment US SA"), data = unemployment_train_ts) +
    geom_line(aes(x = date, y = seasonaladj, col = "x13 SA"), data = x13_dcmp) +
    geom_line(aes(x = date, y = seasonaladj, col = "x11 SA"), data = x11_dcmp ) +
    geom_line(aes(x = date, y = season_adjust, col = "STL SA"), data = stl_dcmp) +
    labs(title = "Replication of the seasonal adjusted data",
         y     = "Seasonal Adjusted Unemployment level",
         x     = "Month") +
    guides(colour = guide_legend("Decomposition method:")) +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_colour_manual(values=c("black","green", "red", "orange"))

# Compare RMSE to find closest fit to original seasonal adjusted unemployment data of US


 t(bind_rows(
    "Model" = c("ME", "RMSE", "MAE",  "MPE", "MAPE" ),
    X11 = c((ts(x11_dcmp$seasonaladj) %>% accuracy(ts(unemployment_train_ts$seasonal_unemployed)))[1:5]),
    X13 = c((ts(x13_dcmp$seasonaladj) %>% accuracy(ts(unemployment_train_ts$seasonal_unemployed)))[1:5]),
    STL = c((ts(stl_dcmp$season_adjust) %>% accuracy(ts(unemployment_train_ts$seasonal_unemployed)))[1:5])))  %>% 
    kbl(caption = "Evaluation metrics of decomposition methods", digits = 2) %>%
    kable_classic(full_width = F, html_font = "Times new roman")

# x11 decomposed plot
x11_dcmp %>% 
    select(-seasonaladj) %>% 
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

# x13 decomposed plot
x13_dcmp %>% 
    select(-seasonaladj) %>% 
    pivot_longer(cols = seasonal:unemployed,
                 names_to = "components",
                 values_to = "values") %>% 
    autoplot() +
    facet_grid(vars(components),
               scales = "free_y") +
    labs(title = "X13 decomposition of Unemployment US",
         subtitle = "unemployed = trend + seasonal + irregular",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = FALSE)




########################################################################################
######################## FORECASTING OF DECOMPOSITION ##################################
########################################################################################
# Choosing X11 because of best RMSE
# Forecast indivial compnents of the X11 decomposition

# Testset of the best decomposition method
x11_seas_test <- seas(ts(unemployment %>% select(unemployed), 
                         start = c("2000"), 
                         frequency = 12), 
                      x11 = "")

# Decomposed in seasonal, trend and irregularities of testset
x11_dcmp_test <- data.frame(x11_seas_test) %>%
    left_join(select(unemployment_test_ts, unemployed), by = "date") %>% 
    select(-adjustfac, -final, -seasonaladj)  %>% 
    mutate(date = yearmonth(date)) %>% 
    as_tsibble(index = date)  %>% 
    pivot_longer(cols = seasonal:unemployed,
                 names_to = "components",
                 values_to = "values") 


# Train with mean, drift, naive, snaive, ets models 
x11_models <- x11_dcmp %>%
    pivot_longer(cols = seasonal:unemployed,
                 names_to = "components",
                 values_to = "values") %>% 
    model(Mean = MEAN(values),
          Drift = RW(values ~ drift()),
          Naive = NAIVE(values),
          SNaive = SNAIVE(values ~ lag("year"))) # HUKS ? SJEKKE ETS!!!!!!!!!!!!!!!!!!!!!!!!!

# x11 forecasting each of the decomposition part
fc_x11 <- x11_models %>% 
    forecast(h = 24)

# Forecasting each of the individual decomposed series 
x11_models  %>% 
    filter(components != "seasonaladj") %>% 
    forecast(h = 24)  %>%
    ggplot() +
    geom_line(aes(x = date, y = .mean, col = .model)) +
    geom_line(aes(x = date, y = values), data = x11_dcmp_test %>% filter(year(date) >= 2000 & year(date) <= 2019 )) +
    facet_grid(vars(components),
               scales = "free_y") +
    labs(title = "Forecast with X11 decomposition",
         subtitle = "Unemployed = Trend + Seasonal + Irregular",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = guide_legend(title = "Model:")) +
    theme_bw()  +
    theme(legend.position = "bottom")
# HUSK ? FIKSE LEGENDS


# Forming forcaste of the test
fc_x11 %>% 
    filter(!components %in% c("seasonaladj", "unemployed")) %>% 
    group_by(.model) %>% 
    summarise("Unemployment level" = sum(.mean)) %>% 
    autoplot() +
    autolayer(unemployment_test_ts %>% filter(year(date) >= 2000)) +
    guides(colour = guide_legend(title = "Model:")) +
    labs(title = "Forcasting with X11 decomposition",
         subtitle = "Unemployed = Trend + Season + Irregular",
         x = "Month") +
    theme_bw() +
    theme(legend.position = "bottom")



##################################################################################
################################ ETS model #######################################
##################################################################################

# Fitting the trainingset with the best ETS model by minimizing AICc
fit_ets <- unemployment_train_ts %>%
    select(date, unemployed) %>% 
    model(ETS_optimal = ETS(unemployed, ic = "aicc"),
          "ETS(A,A,A)"  = ETS(unemployed ~ error("A") + trend("A") + season("N"),  ic = "aicc")
          ) 

fit_ets_optimal <- unemployment_train_ts %>%
    select(date, unemployed) %>% 
    model(ETS_optimal = ETS(unemployed)
    ) 

fit_ets # Error: Additive, Trend: Additive damped, Seasonal: Additive

tidy(fit_ets) %>% 
    select(-.model) %>% 
    pivot_wider(names_from = term, values_from = estimate) %>% 
    kbl(caption = "Coefficients of ETS(A,Ad,A)", digits = 2) %>%
    kable_classic(full_width = F, html_font = "Times new roman")


# ETS decomposition plot
fit_ets %>% 
    components() %>%
    pivot_longer(cols = unemployed:remainder,
                 names_to  = "components",
                 values_to = "values") %>%
    autoplot()+
    facet_grid(vars(components),
               scales = "free_y") +
    labs(title = "ETS(A, AD, A)",
         y = "Component unemployment level",
         x = "Month") +
    theme_bw() 

    
### Forecast ETS with levels
fit_ets %>% 
    forecast(h = 24) %>% 
    autoplot(level = 95) +
    autolayer(unemployment_test_ts  %>% filter(year(date) >= 2007), unemployed) +
    labs(title = "Forecast with ETS",
         y = "Unemployment level",
         x = "Month") +
    theme_bw() +
    theme(legend.position = "bottom")


##################### Comparison with other methods ###################################

models_ets_comparisons_naiv_snaive <-  unemployment_train_ts %>%
    model(snaive = SNAIVE(unemployed),
          naiv   = NAIVE(unemployed)) %>% 
    forecast(h = 24) 

# Plot vs simple forcecast methods
fit_ets %>% 
    forecast(h = 24) %>% 
    ggplot() +
    geom_line(aes(x = date, y  = .mean, col = "ETS(A, AD, A)")) +
    geom_line(aes(x = date, y  = .mean, col = "Snaive"), data = models_ets_comparisons_naiv_snaive %>% filter(.model == "snaive")) + 
    geom_line(aes(x = date, y  = .mean, col = "Naive"), data = models_ets_comparisons_naiv_snaive %>% filter(.model == "naiv")) + 
    geom_line(aes(x = date, y  = unemployed, col = "Observed unemployment"), data = unemployment_test_ts %>% filter(year(date) > 2007, year(date) < 2019)) + 
    labs(title = "Forecast with ETS",
         y = "Unemployment level",
         x = "Month") +
    theme_bw() +
    guides(colour = guide_legend(title = "Legend")) +
    theme(legend.position = "bottom")


fit_ets_optimal %>% 
    forecast(h = 24) %>% 
    autoplot(unemployment_test_ts %>% filter(year(date) >= 2000), 
             level = 95) +
    labs(title = "Forecast of Unemployment level with",
         subtitle = fit_ets_optimal$ETS_optimal,
         y = "Unemployment level", 
         x = "Month") +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(level = guide_legend(title = "Prediction interval %: "))

fit_ets_optimal %>% 
    components() %>% 
    autoplot() +
    labs(x = "Month",
         y = "Unemployment level") +
    theme_bw()



models_ets_comparisons <-  unemployment_train_ts %>%
    model(snaive = SNAIVE(unemployed),
          naiv   = NAIVE(unemployed)) %>% forecast(h = 24) %>%
    bind_rows(fit_ets %>% 
              forecast(h = 24)) %>% 
    accuracy(unemployment_test_ts) %>% 
    mutate(.model = c("ETS(A,A,A)", "ETS(A,Ad,A)", "Naive", "SNaive")) %>% 
    arrange(MASE) %>% 
    rename("Model" = .model) %>% 
    select(-".type", -ACF1)

# Comparisons with simpler forecast methods
models_ets_comparisons %>% 
    kbl(caption = "ETS model compared with simple benchmark forecasting models", digits = 2) %>%
    kable_classic(full_width = F, html_font = "Times new roman")



# Checking for problems with non-stationarity, acf or normal-distribution ------

# The residuals does not seem to have sign of correlation, the histogram is a little bit skewed but seems to be normally distributed. We can use the prediction interval.  


Residuals <- residuals(fit_ets)$.resid

ggtsdisplay(residuals, 
            plot.type = "histogram", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Residuals of ETS(A,Ad,A) model")

fit_ets_optimal %>% 
    components()


Residuals <- augment(fit_ets_optimal)$.resid
ggtsdisplay(Residuals, 
            plot.type = "histogram", 
            lag.max = 24, 
            theme = theme_bw(),
            main = paste("Residuals of ", fit_ets_optimal$ETS_optimal, "model"))

fit_ets %>% 
    augment() %>% 
    features(.innov, ljung_box, lag = 34, dof = 4) # p-value of 3.61% under the 5%-significance level indicates that the autocorrelation comes from white-noise --> OK


# Accuracy of ETS by train and test
accuracy_ets <- bind_rows(
    fit_ets %>% accuracy(),
    fit_ets %>% forecast(h = 12) %>% accuracy(unemployment_test_ts)
) %>% 
    select(-ME, -MPE, -ACF1) %>% 
    arrange(MASE)
accuracy_ets %>% 
    kbl(caption = "Accuracy of ETS(A,Ad,A)", digits = 2) %>%
    kable_classic(full_width = F, html_font = "Times new roman")




################################################################################
########################### ARIMA PREPARATION ##################################
################################################################################
# Plots of differenced unemployed, autocorrelation and partial autocorrelation



ggtsdisplay(unemployment_train_ts_stationarity$unemployed, 
            plot.type = "partial", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Non-stationary Unemployment level in US")



unemployment_train_ts  %>% 
    features(unemployed, ljung_box, lag = 24) # Problems with autocorrelation

# Unitroot KPSS test on unemployed
unemployment_train_ts_stationarity %>% 
    features(unemployed, unitroot_kpss)

"Clearly non- stationary data with heavy trend in mean. Differencing first order makes mean stationary.
Variance not an issue, if so then log-transform"

unemployment_train_ts %>% mutate(diff_unemployed  = difference(unemployed)) %>% ACF(var = diff_unemployed, lag_max = 24) %>% autoplot()

" Significant lag at 12 and 24 months suggest seasonal autocorrelation. It is therefore necessary to perform a seasonal differencing operation."

unemployment_train_ts_stationarity <- unemployment_train_ts %>% 
    mutate(diff_season_unemployed = difference(unemployed, 12),
           diff_unemployed        = difference(unemployed),
           diff_diff_season_unemployed = difference(diff_season_unemployed))


unemployment_train_ts_stationarity %>%  ACF(var = diff_season_unemployed) %>% autoplot()


# number of diffs
unemployment_train_ts_stationarity %>% 
    features(diff_season_unemployed, unitroot_ndiffs)

unemployment_train_ts_stationarity %>% 
    features(diff_unemployed, unitroot_kpss)  # p-value of 1% is significant under 5%-significance level. KPSS(Kviatkowski-Phillips-Scmidt_Shin), indicating that there is need for differencing

unemployment_train_ts_stationarity %>% 
    features(diff_season_unemployed, unitroot_ndiffs)

unemployment_train_ts_stationarity %>% 
    gg_tsdisplay(diff_unemployed, plot_type = "partial")



"From the PACF we can see that there is "

unemployment_train_ts_stationarity %>% 
    features(diff_season_unemployed, unitroot_kpss) # p-value of 5.49%, might be need for differencing

unemployment_train_ts_stationarity %>% 
    features(diff_unemployed, unitroot_kpss) # p-value of 10%, no need for more differencing.


unemployment_train_ts_stationarity %>% 
    features(diff_diff_season_unemployed, unitroot_kpss)


unemployment_train_ts_stationarity %>% 
    gg_tsdisplay(diff_diff_season_unemployed, plot_type = "partial", lag_max = 24)

ggtsdisplay(unemployment_train_ts_stationarity$diff_diff_season_unemployed, 
            plot.type = "partial", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Difference of seasonal differenced Unemployment level in US")


# Plotting unemployed and diff-unemployed
unemployment_train_ts_stationarity %>% 
    select(-seasonal_unemployed) %>% 
    pivot_longer(cols = unemployed:diff_season_unemployed,
                 names_to = "components",
                 values_to = "values") %>% 
    autoplot() +
    facet_grid(vars(components))

# Autocorrelation of unemployed and diff-unemployed

unemployment_train_ts_stationarity %>% 
    select(-seasonal_unemployed) %>% 
    pivot_longer(cols = unemployed:diff_diff_season_unemployed,
                 names_to = "components",
                 values_to = "values") %>% 
    ACF(lag_max = 24) %>% 
    autoplot() +
    facet_grid(vars(components))

unemployment_train_ts_stationarity %>% 
    select(-seasonal_unemployed) %>% 
    pivot_longer(cols = unemployed:diff_diff_season_unemployed,
                 names_to = "components",
                 values_to = "values") %>% 
    PACF(lag_max = 24) %>% 
    autoplot() +
    facet_grid(vars(components))


# Plots of differenced unemployed, autocorrelation and partial autocorrelation
unemployment_train_ts_stationarity %>% 
    gg_tsdisplay(diff_unemployed, plot_type = "partial")

unemployment_train_ts_stationarity %>% 
    gg_tsdisplay(diff_season_unemployed, plot_type = "partial")





################################################################################
############################# ARIMA MODELLING ##################################
################################################################################

# Optimizing the best ARIMA model by minimizing AICc
# ARIMA_optimal <- unemployment_train_ts %>% model(ARIMA_optimal = ARIMA(unemployed, ic = "aicc", stepwise = FALSE, approximation = FALSE))
# ARIMA_optimal has Non-seasonal part pdq(5,1,0) and seasonal part PDQ(0,1,1) lag 12.

# Different ARIMA models to fit the unemployment trainingset
arima_manual_fits <- unemployment_train_ts %>% 
    select(date, unemployed) %>% 
    model(ARIMA311011 = ARIMA(unemployed ~ pdq(3,1,1) + PDQ(0,1,1)),
          ARIMA111011 = ARIMA(unemployed ~ pdq(1,1,1) + PDQ(0,1,1)),
          ARIMA313011 = ARIMA(unemployed ~ pdq(3,1,1) + PDQ(0,1,1)),
          ARIMA510111 = ARIMA(unemployed ~ pdq(5,1,0) + PDQ(1,1,0))
    ) %>% 
    bind_cols(ARIMA_optimal)



# Accuracy of traningset and testset
accuracy_arima <- bind_rows(
    arima_manual_fits  %>% accuracy(),
    arima_manual_fits  %>% forecast(h = 24)  %>%  accuracy(unemployment_test_ts)
    )  %>% 
    arrange(.type, MASE)  

# Residualplot 
arima_manual_fits  %>%
    select(ARIMA_optimal) %>%
    gg_tsresiduals()



fc_arima_manual_fits <- arima_manual_fits %>% 
    forecast(h = 24)   %>% 
    filter(year(date) <= 2020)

fc_arima_manual_fits  %>% 
    filter(.model %in% c("ARIMA_optimal", "ARIMA111011", "SNAIVE" ))  %>% 
    ggplot() +
    geom_line(aes(x = date, y = .mean, col = .model)) + 
    geom_line(aes(x = date, y = unemployed), col = "black", data = unemployment_test_ts %>% filter(year(date) > 2015))



arima_manual_fits  %>% 
    augment()  %>% 
    filter(.model %in% c("ARIMA311011", "ARIMA111011", "SNAIVE" ))  %>% 
    autoplot(.fitted) +
    autolayer(unemployment_train_ts, unemployed, col = "black")



arima_manual_fits  %>% accuracy(unemployment_test_ts)


#************************USING UNEMPLOYED**************************************#
# Finding the global optimal ARIMA-model by minimizing AICc
fit_arima_optimal <- unemployment_train_ts %>% 
    select(date, unemployed) %>% 
    model(ARIMA_optimal = ARIMA(unemployed, 
                                stepwise = FALSE,
                                approximation = FALSE))

?CV
### Store in Rdata file
fit_arima_optimal # Non-seasonal part (p,d,q) = (3,0,1) and Seasonal-part (P,D,Q)m = (0,1,1)12

report(fit_arima_optimal)

fit_arima_optimal %>% 
    glance() # check out the AICc


# Checking for problems with non-stationarity, acf or normal-distribution ------
fit_arima_optimal %>% 
    gg_tsresiduals() # Does not seems to be sign of correlation in resduals, and the histogram shows a normally distributed, this means that the prediction interval will be ok. 

Residuals <- augment(fit_arima_optimal)$.resid
ggtsdisplay(Residuals, 
            plot.type = "histogram", 
            lag.max = 24, 
            theme = theme_bw(),
            main = paste("Residuals of ", fit_arima_optimal$ARIMA_optimal, "model"))


fit_arima_optimal %>% 
    augment() %>% 
    features(.innov, ljung_box, lag = 24, dof = 4) # KVIFOR ER DET FORSKJELLIG SVAR ETTER ENDRING AV LAG OG DOF?????



fit_arima_optimal %>% 
    forecast(h = 24) %>% 
    autoplot(unemployment_test_ts %>% filter(year(date) >= 2000), 
             level = 95) +
    labs(title = "Forecast of Unemployment level with",
         subtitle = fit_arima_optimal$ARIMA_optimal,
         y = "Unemployment level", 
         x = "Month") +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(level = guide_legend(title = "Prediction interval %: "))



fc_arima_optimal <- fit_arima_optimal %>% 
    forecast(h = 12)

# RMSE of ARIMA-optimal; two methods -------------------------------------------
sqrt(mean((unemployment_test$unemployed - fc_arima_optimal$.mean)^2))


### Plot AIC optimal ARIMA vs Custom ARIma
unemployment_test_ts %>%  
    filter(year(date) >= 2017)  %>% 
    ggplot() +
    geom_line(aes(x= date, y = unemployed, col = "Original data")) +
    geom_line(aes(x = date, y = .mean, col = "fc_arima_optimal"), data =  fc_arima_optimal) + 
    geom_line(aes(x = date, y = .mean, col = "fc_arima311011"), data =  fc_arima311011) +
    labs(title = "Forecasting of unemployment US")

accuracy_arima  <- bind_rows(
    fit_arima_optimal %>% accuracy(),
    fc_arima_optimal %>% accuracy(unemployment_test_ts),
    arima_manual_fits  %>% accuracy(),
    fc_arima311011  %>%  accuracy(unemployment_test_ts)
) %>%
    select(.model:RMSSE)  %>% 
    arrange(RMSSE)
accuracy_arima


"When comparing models using AICc, the most important part is that
the models have the same differecing order (I).
Even if the all the models does not pass a ljung-box test (prediction interval cannot
be interpreted), we can still forecast"

# Comparing ETS vs ARIMA -------------------------------------------------------
accuracy_models <- bind_rows(
    accuracy_ets,
    accuracy_arima) %>% 
    arrange(RMSSE) # Root mean square standardized effect
accuracy_models



########## LOAD NEW VARIABLES #######################

## CPI data

cpi_data <- read_csv("../Data/US/cpi_data.csv") %>% 
    rename("date" = TIME, "cpi" = Value)  %>% 
    mutate(date = yearmonth(as.Date(paste(as.character(date), "-01", sep =""))))  %>% 
    filter(year(date) >= 2000, LOCATION == "USA")   %>% 
    select(date, cpi)  %>% 
    as_tsibble(index = date) 
    
cpi_train <- cpi_data  %>% filter(year(date) <= 2017)

export_ts <- read_csv("../Data/US/export_data.csv")  %>% 
    rename("date" = TIME, "export" = Value)  %>% 
    mutate(date = yearmonth(as.Date(paste(as.character(date), "-01", sep =""))))  %>% 
    filter(LOCATION == "USA", year(date) >= 2000 & year(date) <= 2019) %>%
    select(date, export)  %>% 
    as_tsibble(index = date) 

export_train <- export_ts  %>% 
    filter(year(date) <= 2017)



#####################################################################
#################### Multivariate model: ARIMA #############################
#####################################################################

multivariate_data <- unemployment_train_ts %>% 
    left_join(cpi_train, by = "date")  %>% 
    left_join(export_train, by = "date") 
 
 
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




# Multivariate forecast with Vectorized auto regression
fit_multivariate_var <- multivariate_data %>% 
     model(VAR = VAR(vars(unemployed,cpi,export), ic = "aicc"))

fc_multivariate_var <- fit_multivariate_var  %>% 
    forecast(h = 24)  %>% 
    as_tsibble(index = date)

fc_multivariate_var  %>% 
    ggplot() +
        geom_line(aes(x = date, y  = .mean_unemployed, color = "Multivariate forecasts")) +
        geom_line(aes(x = date, y = unemployed, color = "Observed"), data = unemployment) +
        geom_line(aes(x = date, y = .fitted , color = "Fitted"), data = fit_multivariate_var %>% augment() %>% filter(.response == "unemployed")) +
        theme_bw() +
        scale_colour_manual(values=c("#56B4E9", "black", "#56e99b")) +
        theme(legend.position = "bottom") +
        labs(title = "Multivariate forecaste",
             y = "Unemployment level",
             x = "Month") +
        guides(colour = guide_legend(title = "Series"))







multivariate_var_table_data <- data.frame(Model = "Multivariate VAR model", 
                                          Type = "Test", 
                                          RMSE = RMSE(unemployment_test$unemployed, fc_multivariate_var$.mean_unemployed),
                                          MAE =  MAE(unemployment_test$unemployed, fc_multivariate_var$.mean_unemployed),
                                          MAPE = MAPE(unemployment_test$unemployed, fc_multivariate_var$.mean_unemployed),
                                          MASE = MASE(unemployment_test$unemployed, fc_multivariate_var$.mean_unemployed, .period = 1)
                                        )

### TABLE comparison #####

multivariate_table_data <- 
    fc_multivariate_arima  %>% select(.model, date, .mean)  %>% 
    accuracy(unemployment_test_ts) %>% 
    rename("Model" = .model,
            "Type" = .type)  %>% 
    mutate(Model = "Multivariate ARIMA")  %>% 
    select(Model:MAE, MAPE, MASE, RMSSE, -ME)
    bind_rows(multivariate_var_table_data)


#Call kbl
multivariate_table_data %>% 
    kbl(caption = "Multivariate ARIMA and Var models", digits = 2) %>%
    kable_classic(full_width = F, html_font = "Times new roman")




########################################################################
#################### Forecast with CPI + EXPORT ########################
########################################################################
unemployment_dynamic_data <- unemployment_train_ts %>% 
    left_join(cpi_train, by = "date")  %>% 
    left_join(export_train, by = "date")  %>% 
    select(-seasonal_unemployed)

fit_tslm <- unemployment_dynamic_data  %>% 
    model(ARIMA(unemployed ~ cpi + export + pdq(0,0,0)))

fc_tslm <- fit_tslm  %>% 
    forecast(h = 24)


