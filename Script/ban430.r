###############################################################################
################ Working Directory for Windows and Mac ########################
###############################################################################

#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
setwd("/Users/olaiviken/Documents/BAN430/BAN430/Data")

# Choose the first if you use Mac OS and second if Windows
Sys.setenv(X13_PATH = "../x13binary/bin")
#Sys.setenv(X13_PATH = "../windows_x13/bin")


################################################################################
############################## LIBRARIES #######################################
################################################################################
library(fpp3)
library(readxl)
library(lubridate)
library(magrittr)
library(feasts)
library(seasonal)
library(x13binary)

checkX13()


###############################################################################
############################ DATA RETRIEVAL ###################################
###############################################################################
df <- read_xls("../Data/US/Forecasting_economic_data.xls", sheet = 2)  %>% 
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
########################### TRAINING AND TEST ###################################
#################################################################################
unemployment <- df  %>% 
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
unemployment %>% 
    ggplot() +
    geom_line(aes(x = date, y = unemployed, col = "Unadjusted seasonal")) +
    geom_line(aes(x = date, y = seasonal_unemployed, col = "Adjusted seasonal")) +
    labs(title = "Unemployment in USA",
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

# Unemployment level in US by traning set
unemployment_train_ts   %>%  
    ggplot() +
    geom_line(aes(x = date, y = unemployed, col = "Training data")) +
    labs(title = "Unemployment",
         subtitle = "Train [2000-2017]",
         y = "Unemployment level",
         x = "Month") +
    guides(col = FALSE) 


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
        STL(unemployed ~ trend(window = 7) + season(window = "periodic"),
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
    theme(legend.position = "bottom")

# Compare RMSE to find closest fit to original seasonal adjusted unemployment data of US
bind_cols(
    "Accuracy method" = "RMSE",
    #"x11 feasts" = sqrt(mean((unemployment_train_ts$seasonal_unemployed - x11_dcmp_feasts$season_adjust)^2)),
    x11 = sqrt(mean((unemployment_train_ts$seasonal_unemployed - x11_dcmp$seasonaladj)^2)),
    x13 = sqrt(mean((unemployment_train_ts$seasonal_unemployed - x13_dcmp$seasonaladj)^2)),
    stl = sqrt(mean((unemployment_train_ts$seasonal_unemployed - stl_dcmp$season_adjust)^2))
) %>% 
    kbl(caption = "RMSE of decomposing methods", digits = 2) %>%
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

# Forecasting of traningset decomposition using seasonal adjusted data
# x11_dcmp %>%
#     select(date, seasonaladj) %>% 
#     model(Mean = MEAN(seasonaladj),
#           Drift = RW(seasonaladj ~ drift()),
#           Naive = NAIVE(seasonaladj),
#           Holt =  ETS(seasonaladj ~ error("A") + trend("A") + season("N"))) %>% 
#     forecast(h = 24) %>% 
#     autoplot(level = NULL) +
#     autolayer(unemployment_test_ts  %>% filter(year(date) >= 2007), unemployed) +
#     labs(title = "Forecast with X11",
#          y = "Unemployment level",
#          x = "Month")

# Train with mean, drift, naive, snaive, ets models 
x11_models <- x11_dcmp %>%
    pivot_longer(cols = seasonal:unemployed,
                 names_to = "components",
                 values_to = "values") %>% 
    model(Mean = MEAN(values),
          Drift = RW(values ~ drift()),
          Naive = NAIVE(values),
          SNaive = SNAIVE(values ~ lag("year")),
          ETS = ETS(values)) # HUKS Å SJEKKE ETS!!!!!!!!!!!!!!!!!!!!!!!!!

# x11 forecasting each of the decomposition part
fc_x11 <- x11_models %>% 
    forecast(h = 24)

# Forecasting each of the individual decomposed series 
x11_models  %>% 
    filter(components != "seasonaladj") %>% 
    forecast(h = 24)  %>% 
    autoplot(level = NULL) +
    autolayer(x11_dcmp_test %>% filter(year(date) >= 2007 & year(date) <= 2017 )) +
    facet_grid(vars(components),
               scales = "free_y") +
    labs(title = "Forecast of X11 decomposition",
         subtitle = "Unemployed = Trend + Seasonal + Irregular",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = guide_legend(title = "Legend")) +
    theme_bw()  +
    theme(legend.position = "bottom")
# HUSK Å FIKSE LEGENDS


# Forming forcaste of the test
fc_x11 %>% 
    filter(!components %in% c("seasonaladj", "unemployed")) %>% 
    group_by(.model) %>% 
    summarise("Unemployment level" = sum(.mean)) %>% 
    autoplot() +
    autolayer(unemployment_test_ts %>% filter(year(date) >= 2015)) +
    guides(colour = guide_legend(title = "Model:")) +
    labs(title = "Forcasting with x11 decomposition",
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
    model(ETS(unemployed)) 

fit_ets # Error: Additive, Trend: Additive damped, Seasonal: Additive

tidy(fit_ets) %>% 
    select(-.model) %>% 
    pivot_wider(names_from = term, values_from = estimate) %>% 
    kbl(caption = "Coefficients of ETS(A,Ad,A)", digits = 2) %>%
    kable_classic(full_width = F, html_font = "Times new roman")


# ETS decomposition plot
fit_ets %>% 
    components() %>%
    autoplot() +
    theme_bw()

# Forecasting with seasonal adjusted 
fit_ets %>% 
    forecast(h = 24) %>% 
    autoplot(level = 95) +
    autolayer(unemployment_test_ts  %>% filter(year(date) >= 2007), unemployed) +
    labs(title = "Forecast with ETS",
         y = "Unemployment level",
         x = "Month") +
    theme_bw() +
    theme(legend.position = "bottom")

# Checking for problems with non-stationarity, acf or normal-distribution ------

# The residuals does not seem to have sign of correlation, the histogram is a little bit skewed but seems to be normally distributed. We can use the prediction interval.  


Residuals <- residuals(fit_ets)$.resid

ggtsdisplay(residuals, 
            plot.type = "histogram", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Residuals of ETS(A,Ad,A) model")

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

#############################################################################################
#################################### CROSS VALIDATION #######################################
#############################################################################################

# Cross validation set for ETS and ARIMA forecast models
#unemployment_train_ts_cv <- unemployment %>% 
#    as_tsibble(index = date) %>% 

#   stretch_tsibble(.init = 12, .step = 1) 






#############################################################################################
#################################### ETS MODEL Cross validation #######################################
#############################################################################################

## Store as R.data file
load("../Data/model_cv_fits.Rdata")
#save(fit_ets_cv, file = "../Data/model_cv_fits.Rdata")



fc_ets_cv <- fit_ets_cv %>% 
    forecast(h = 12) %>% 
    filter(year(date) >= 2019)

accuracy_ets_cv <- bind_rows(
    fit_ets_cv %>% accuracy(),
    fc_ets_cv %>% accuracy(unemployment %>% as_tsibble())
) %>%
    select(-ME, -MPE, -ACF1) %>% 
    arrange(RMSSE)

accuracy_ets_cv %>% 
    filter(.type == "Test")

fit_ets_cv %>%
    filter(.id == 185) %>% 
    forecast(h = 12) %>% 
    autoplot(unemployment_train_ts)  # FINNE UT HVORDAN VI VELGER RIKTIG FORECAST DATO!!

"Vi må finne ut korleis vi sjekka accuracy på test(mulig det er snittet av testene)
fra denne skal vi då PLUKKE DEN BESTE MODELLEN MED MINST RMSSE!!"

################################################################################
########################### ARIMA PREPARATION ##################################
################################################################################
# Plots of differenced unemployed, autocorrelation and partial autocorrelation
unemployment_train_ts %>% 


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
          ARIMA510111 = ARIMA(unemployed ~ pdq(5,1,0) + PDQ(1,1,0)),
          SNAIVE       = SNAIVE(unemployed),
          ETS          = ETS(unemployed)
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


### Store in Rdata file
fit_arima_optimal # Non-seasonal part (p,d,q) = (3,0,1) and Seasonal-part (P,D,Q)m = (0,1,1)12

report(fit_arima_optimal)

fit_arima_optimal %>% 
    glance() # check out the AICc


# Checking for problems with non-stationarity, acf or normal-distribution ------
fit_arima_optimal %>% 
    gg_tsresiduals() # Does not seems to be sign of correlation in resduals, and the histogram shows a normally distributed, this means that the prediction interval will be ok. 

fit_arima_optimal %>% 
    augment() %>% 
    features(.innov, ljung_box, lag = 24, dof = 4) # KVIFOR ER DET FORSKJELLIG SVAR ETTER ENDRING AV LAG OG DOF?????

fit_arima_optimal %>% 
    forecast(h = 12) %>% 
    autoplot(unemployment_test_ts %>% filter(year(date) >= 2017), 
             level = 95) +
    labs(title = "Forecast of unemployment with ARIMA",
         subtitle = fit_arima_optimal$ARIMA_optimal,
         y = "Unemployment level", 
         x = "Month")



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
    accuracy_arima
) %>% 
    arrange(RMSSE) # Root mean square standardized effect
accuracy_models
