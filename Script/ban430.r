source("data.r")

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
        "75%entil" = quantile(unemployed, 0.75),
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

# x11 decompoising with feasts
x11_dcmp_feasts <- unemployment_train_ts  %>% 
    model(x11 = feasts:::X11(unemployed, 
                             type = "additive"))  %>% 
    components()

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

# Plot of the various decomposition methods
ggplot() +
    geom_line(aes(x = date, y = seasonal_unemployed, col = "Unemployment US SA"), data = unemployment_train_ts) +
    #geom_line(aes(x = date, y = seasonaladj, col = "x13 SA"), data = x13_dcmp) +
    geom_line(aes(x = date, y = seasonaladj, col = "x11 SA"), data = x11_dcmp ) +
    #geom_line(aes(x = date, y = season_adjust, col = "STL SA"), data = stl_dcmp) +
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

# x13 decomposed plot
x13_dcmp %>% 
    dplyr::select(-seasonaladj) %>% 
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
# Forecast individual components of the X11 decomposition



evaluate_forecast <- function(df, train = unemployment_train_ts$unemployed, test = unemployment_test$unemployed, column = "Unemployment level") {
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

# Testset of the best decomposition method
x11_seas_test <- seas(ts(unemployment %>% dplyr::select(unemployed), 
                         start = c("2000"), 
                         frequency = 12), 
                      x11 = "")

# Decomposed in seasonal, trend and irregularities of testset
x11_dcmp_test <- data.frame(x11_seas_test) %>%
    left_join(dplyr::select(unemployment_test_ts, unemployed), by = "date") %>% 
    dplyr::select(-adjustfac, -final,)  %>% 
    mutate(date = yearmonth(date)) %>% 
    as_tsibble(index = date)  %>% 
    pivot_longer(cols = seasonal:unemployed,
                 names_to = "components",
                 values_to = "values") 

################################################################################
#### Train and Test set of seasonal and seasonal adjusted components
################################################################################
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

fc_x11_season <- x11_season %>% forecast(h = 24)

fc_x11_seasonal_adjust <- x11_seasonal_adjust %>% forecast(h = 24)

fc_combined <- fc_x11_season %>% left_join(fc_x11_seasonal_adjust, by = c("date", ".model"))


snaive  <-  (fc_x11_season %>% filter(.model == "SNaive"))$.mean

fc_combined <- fc_x11_seasonal_adjust %>% 
    filter(.model %in% c("Naive", "Arima", "ETS")) %>% 
    mutate(.mean = .mean + snaive) 


### Seasonal component plot

color_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#0072B2", "#D55E00", "#CC79A7")
fc_x11_season  %>% 
    ggplot() + 
    geom_line(aes(x = date, y = values, color = "Original data"), data = x11_dcmp_test %>%  filter(year(date) > 2014, components == "seasonal")) +
    geom_line(aes(x = date, y = .mean, color = .model)) + 
    theme_bw() + 
    labs(title = "Seasonal component forecast", y = "Seasonal unemployment level", 
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
    labs(title = "Seasonally adjusted component forecast", y = "Seasonal unemployment level", x = "Month", 
         subtitle = TeX("$\\hat{A_t} = \\hat{T_t} + \\hat{R_t} $")) +
    theme(legend.position = "bottom") +
    scale_colour_manual(values = color_palette) +
    guides(colour = guide_legend(title = "Series"))


### Plot combined decomposition forecasting

fc_combined %>% 
    ggplot() + 
    geom_line(aes(x = date, y = .mean, color = .model)) + 
    geom_line(aes(x = date, y = unemployed, color = "Original data"), data = unemployment_test_ts %>%  filter(year(date) > 2014)) +
    theme_bw() + 
    labs(title = "X11 forecast", y = "Seasonal unemployment level", 
         x = "Month",
         subtitle = TeX("$\\hat{y_t} = \\hat{S_t} + \\hat{A_t} $")) +
    theme(legend.position = "bottom") +
    scale_colour_manual(values = color_palette) +
    guides(colour = guide_legend(title = "Series"))



# Train with mean, drift, naive, snaive, ets models 
x11_models <- x11_dcmp %>%
    pivot_longer(cols = c("seasonal", "seasonaladj"),
                 names_to = "components",
                 values_to = "values") %>% 
    model(Mean = MEAN(values),
          Drift = RW(values ~ drift()),
          Naive = NAIVE(values),
          SNaive = SNAIVE(values ~ lag("year"))) 



x11_train <- x11_dcmp %>%
    pivot_longer(cols = c("seasonal", "trend", "irregular", "unemployed"),
                 names_to = "components",
                 values_to = "values") %>% 
    dplyr::select(date, components, values)


# x11 forecasting each of the decomposition part
fc_x11 <- x11_models %>% 
    forecast(h = 24)

# Forecasting each of the individual decomposed series 
x11_models  %>% 
    filter(components != "seasonaladj") %>% 
    forecast(h = 24)  %>%
    ggplot() +
    #geom_line(aes(x = date, y = .mean, col = .model)) +
    geom_line(aes(x = date, y = values), data = x11_dcmp_test %>% filter(year(date) >= 2000 & year(date) <= 2017 )) +
    facet_grid(vars(components),
               scales = "free_y") +
    labs(title = "Forecast with X11 decomposition",
         subtitle = "Unemployed = Trend + Seasonal + Irregular",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = guide_legend(title = "Model:")) +
    theme_bw()  +
    theme(legend.position = "bottom")


#### Components facet plot
x11_train  %>% 
    filter(components != "seasonaladj") %>% 
    ggplot() +
    geom_line(aes(x = date, y = values, col = components)) +
    facet_grid(vars(components),
               scales = "free_y") +
    labs(title = "Forecast with X11 decomposition",
         subtitle = "Unemployed = Trend + Seasonal + Irregular",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = guide_legend(title = "Model:")) +
    theme_bw()  +
    theme(legend.position = "bottom")

#### Utg?r??
# Forming forcaste of the test
data_added_x11 <- 
    x11_dcmp %>% 
    filter(!components %in% c("seasonaladj", "unemployed")) %>% 
    group_by(.model) %>% 
    summarise("Unemployment level" = sum(.mean))


fc_added_x11 <- 
    fc_x11 %>% 
    filter(!components %in% c("seasonaladj", "unemployed")) %>% 
    group_by(.model) %>% 
    summarise("Unemployment level" = sum(.mean))

fc_added_x11 %>% 
    autoplot() +
    autolayer(unemployment_test_ts %>% filter(year(date) >= 2015)) +
    guides(colour = guide_legend(title = "Model:")) +
    labs(title = "Forcasting with X11 decomposition",
         subtitle = "Unemployed = Trend + Season + Irregular",
         x = "Month") +
    theme_bw() +
    theme(legend.position = "bottom")

##### Decomposition tables #####

## Season table

evaluate_forecast(df = fc_x11_season, column = ".mean", train = x11_dcmp_seasonal_train$values, test = x11_dcmp_seasonal_test$values) %>% 
    kable(caption = "Seasonal component forecast", digits = 3) %>%
    kable_classic(full_width = F, html_font = "Times new roman") 

## Seasonal adjusted table

evaluate_forecast(df = fc_x11_seasonal_adjust, column = ".mean", train = x11_dcmp_seasonal_adjusted_train$values, test = x11_dcmp_seasonal_adjusted_test$values) %>% 
    kable(caption = "Seasonally adjusted component forecast", digits = 3) %>%
    kable_classic(full_width = F, html_font = "Times new roman") 

## Decomposition forecast table
evaluate_forecast(fc_combined, ".mean") %>% 
    kable(caption = "X11 combined forecast", digits = 3) %>%
    kable_classic(full_width = F, html_font = "Times new roman") 

# Checking accuarcy on the test set
fc_x11_accuracy <- fc_x11 %>% 
  filter(!components %in% c("seasonaladj", "unemployed")) %>% 
  group_by(.model) %>% 
  summarise("Unemployment level" = sum(.mean))

fc_x11_accuracy %>% accuracy()


##################################################################################
################################ ETS model #######################################
##################################################################################

# Fitting the trainingset with the best ETS model by minimizing AICc
fit_ets <- unemployment_train_ts %>%
    dplyr::select(date, unemployed) %>% 
    model(ETS_optimal = ETS(unemployed, ic = "aicc"),
          "ETS(A,A,A)"  = ETS(unemployed ~ error("A") + trend("A") + season("N"),  ic = "aicc")
    ) 

fit_ets_optimal <- unemployment_train_ts %>%
    dplyr::select(date, unemployed) %>% 
    model(ETS_optimal = ETS(unemployed)
    ) 
fc_ets_optimal <-  fit_ets_optimal %>% forecast(h = 24)
fit_ets # Error: Additive, Trend: Additive damped, Seasonal: Additive

tidy(fit_ets) %>% 
    dplyr::select(-.model) %>% 
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
    autoplot(unemployment_test_ts %>% filter(year(date) >= 2015), 
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
    arrange(MASE) %>% 
    mutate(.model = c("ETS(A,Ad,A) ", "ETS(A,A,A)", "Naive", "SNaive")) %>% 
    rename("Model" = .model) %>% 
    dplyr::select(-".type", -ACF1)

# Comparisons with simpler forecast methods
models_ets_comparisons %>% 
    kbl(caption = "ETS model compared with simple benchmark forecasting models", digits = 2) %>%
    kable_classic(full_width = F, html_font = "Times new roman")



# Checking for problems with non-stationarity, acf or normal-distribution ------

# The residuals does not seem to have sign of correlation, the histogram is a little bit skewed but seems to be normally distributed. We can use the prediction interval.  


residuals <- residuals(fit_ets)$.resid

ggtsdisplay(residuals(fit_ets)$.resid, 
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
    dplyr::select(-ME, -MPE, -ACF1) %>% 
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
    dplyr::select(-seasonal_unemployed) %>% 
    pivot_longer(cols = unemployed:diff_season_unemployed,
                 names_to = "components",
                 values_to = "values") %>% 
    autoplot() +
    facet_grid(vars(components))

# Autocorrelation of unemployed and diff-unemployed

unemployment_train_ts_stationarity %>% 
    dplyr::select(-seasonal_unemployed) %>% 
    pivot_longer(cols = unemployed:diff_diff_season_unemployed,
                 names_to = "components",
                 values_to = "values") %>% 
    ACF(lag_max = 24) %>% 
    autoplot() +
    facet_grid(vars(components))

unemployment_train_ts_stationarity %>% 
    dplyr::select(-seasonal_unemployed) %>% 
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




# Finding the global optimal ARIMA-model by minimizing AICc
fit_arima_optimal <- unemployment_train_ts %>% 
    dplyr::select(date, unemployed) %>% 
    model(ARIMA_optimal = ARIMA(unemployed, 
                                stepwise = FALSE,
                                approximation = FALSE))



coefficients(fit_arima_optimal)$estimate
reportfit_arima_optimal


r <- report(fit_arima_optimal)
r

arima_manual_fits <- unemployment_train_ts %>% 
    dplyr::select(date, unemployed) %>% 
    model(ARIMA311011 = ARIMA(unemployed ~ pdq(3,1,1) + PDQ(0,1,1)),
          ARIMA111011 = ARIMA(unemployed ~ pdq(1,1,1) + PDQ(0,1,1)),
          ARIMA313011 = ARIMA(unemployed ~ pdq(3,1,1) + PDQ(0,1,1)),
          ARIMA510111 = ARIMA(unemployed ~ pdq(5,1,0) + PDQ(1,1,0))
    ) %>% 
    bind_cols(fit_arima_optimal)



# Accuracy of traningset and testset
accuracy_arima <- bind_rows(
    arima_manual_fits  %>% accuracy(),
    arima_manual_fits  %>% forecast(h = 24)  %>%  accuracy(unemployment_test_ts)
)  %>% 
    arrange(.type, MASE)  




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
    autoplot(unemployment_test_ts %>% filter(year(date) >= 2015), 
             level = 95) +
    labs(title = "Forecast of Unemployment level with",
         subtitle = fit_arima_optimal$ARIMA_optimal,
         y = "Unemployment level", 
         x = "Month") +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(level = guide_legend(title = "Prediction interval %: "))



fc_arima_optimal <- fit_arima_optimal %>% 
    forecast(h = 24)

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
    dplyr::select(.model:RMSSE)  %>% 
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



save(fit_ets_optimal, fit_arima_optimal, fc_arima_optimal, fc_ets_optimal, file = "../Data/optimal_models.Rdata")


