
#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Data")

### Libraries ----
library(fpp3)
library(readxl)
library(lubridate)
library(magrittr)
library(feasts)
library(seasonal)
library(x13binary)
#Sys.setenv(X13_PATH = "/Users/olaiviken/Documents/BAN430/BAN430/x13binary/bin")
#Sys.setenv(X13_PATH = "../windows_x13/bin")
checkX13()

df <- read_xls("../Data/US/Forecasting_economic_data.xls", sheet = 2)


clear_names <- c(   "date", 
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
                    "seasonal_unemployed")

colnames(df) <- clear_names

# unemployment %>% 
#     ggplot() +
#     geom_line(aes(x = date, y = unemployed, col = "Unadjusted seasonal")) +
#     geom_line(aes(x = date, y = seasonal_unemployed, col = "Adjusted seasonal")) +
#     labs(   title = "Unemployment in USA",
#             subtitle = "Seasonal adj. vs non adj.",
#             y = "Unemployment level",
#             x = "Months") +
#     guides(col = guide_legend(title = "Series:")) 

#unemployment  %>% 
#    model(classical_decomposition(unemployed, type = "additive"))


unemployment <- df  %>% 
    mutate(date = yearmonth(date))  %>% 
    filter(year(date) >= 1995 & year(date) <= 2019)   %>% 
    select(date, unemployed, seasonal_unemployed)   


unemployment_train <- unemployment  %>% 
    filter(year(date) <= 2018)  %>% 
    select(date, unemployed, seasonal_unemployed)



###### Summary statistics #########


### Mean, median etc.


unemployment_train  %>% 
    mutate(month = lubridate::month(date))  %>%
    group_by(month)  %>% 
    summarise(  min = min(unemployed),
                "25%-percentil" = quantile(unemployed, 0.25),
                mean = mean(unemployed),
                median = median(unemployed),
                "75%-percentil" = quantile(unemployed, 0.75),
                max = max(unemployed)) 



unemployment_train_ts <-  unemployment_train %>% 
    as_tsibble(index = date) 

unemployment_test_ts <- unemployment %>% 
    as_tsibble(index = date)

#Seasonal subseries
unemployment_train_ts  %>%  
    gg_subseries(unemployed) +
    labs(x = "Month", y = "Unemployment level")

# Train series
unemployment_train_ts   %>%  
    ggplot() +
    geom_line(aes(x = date, y = unemployed, col = "Training data")) +
    labs(   title = "Unemployment",
            subtitle = "Train [1995-2018]",
            y = "Unemployment level",
            x = "Month") +
    guides(col = FALSE) 



## SEAS X13
x13_seas <- seas(ts(unemployment_train %>% select(unemployed), start = c("1995"), frequency = 12))

x11_seas <- seas(ts(unemployment_train %>% select(unemployed), start = c("1995"), frequency = 12), x11 = "")

#Convert results to dataframe
x13_dcmp <- data.frame(x13_seas) %>% 
    left_join(select(unemployment_train_ts, unemployed), by = "date") %>% 
    select(-adjustfac, -final)  %>% 
    mutate(date = yearmonth(date))  %>%
    as_tsibble(index = date)


x11_dcmp <- data.frame(x11_seas) %>%
    left_join(select(unemployment_train_ts, unemployed), by = "date") %>% 
    select(-adjustfac, -final)  %>% 
    mutate(date = yearmonth(date)) %>% 
    as_tsibble(index = date)


x11_dcmp_feasts <- unemployment_train_ts  %>% 
    model(x11 = feasts:::X11(unemployed, type = "additive"))  %>% 
    components()

stl <- unemployment_train_ts %>%
    model(
        STL(unemployed ~ trend(window = 7) +
                season(window = "periodic"),
            robust = TRUE)) %>%
    components()

ggplot() +
    geom_line(aes(x = date, y = seasonaladj, col = "x13 SA"), data = x13_dcmp) +
    geom_line(aes(x= date, y = seasonal_unemployed, col = "Unemployment US SA"), data = unemployment_train_ts) +
    geom_line(aes(x= date, y = seasonaladj, col = "x11 SA"), data = x11_dcmp ) +
    geom_line(aes(x= date, y = season_adjust, col = "STL SA"), data = stl)



#Compare RMSE to find closest fit to original US labor statistics decomposition

# x13 with seas
mean((unemployment_train_ts$seasonal_unemployed - x13_dcmp$seasonaladj)^2)

## X11 with feasts
mean((unemployment_train_ts$seasonal_unemployed - x11_dcmp_feasts$season_adjust)^2)

# X11 with seas
mean((unemployment_train_ts$seasonal_unemployed - x11_dcmp$seasonaladj)^2)

# stl 
mean((unemployment_train_ts$seasonal_unemployed - stl$season_adjust)^2)


# Decomposing of the series ----------------------------------------------------
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

x11_dcmp %>% 
    select(-seasonaladj) %>% 
    pivot_longer(cols = seasonal:unemployed,
                 names_to = "components",
                 values_to = "values") %>% 
    autoplot() +
    facet_grid(vars(components),
               scales = "free_y") +
    labs(title = "X11 decomposition of Unemployment US",
         subtitle = "unemployed = trend + seasonal + irregular",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = FALSE)


stl %>%
    pivot_longer(cols = unemployed:remainder,
                 names_to = "components",
                 values_to = "values") %>% 
    autoplot(values) +
    facet_grid(vars(components),
               scales = "free_y") +
    labs(title = "STL decomposition of Unemployment US",
         subtitle = "unemployed = trend + season_year + remainder",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = FALSE)


# Choosing X11 because of best RMSE


### Forecast indivial compnents of the X11 decomposition

#Decompose a test time series
x11_seas_test <- seas(ts(unemployment %>% select(unemployed), start = c("1995"), frequency = 12), x11 = "")

x11_dcmp_test <- data.frame(x11_seas_test) %>%
    left_join(select(unemployment_test_ts, unemployed), by = "date") %>% 
    select(-adjustfac, -final, -seasonaladj)  %>% 
    mutate(date = yearmonth(date)) %>% 
    as_tsibble(index = date)  %>% 
    pivot_longer(cols = seasonal:unemployed,
                 names_to = "components",
                 values_to = "values") 


x11_dcmp %>%
    select(date, seasonaladj) %>% 
    model(Mean = MEAN(seasonaladj),
          Drift = RW(seasonaladj ~ drift()),
          Naive = NAIVE(seasonaladj),
          SNaive = SNAIVE(seasonaladj ~ lag("year"))) %>% 
    forecast(h = 12) %>% 
    autoplot(level = NULL) +
    autolayer(unemployment_test_ts  %>% filter(year(date) >= 2007), unemployed) +
    labs(title = "Forecast with X11",
         y = "Unemployment level",
         x = "Month")


x11_dcmp %>% 
    select(-seasonaladj) %>%
    pivot_longer(cols = seasonal:unemployed,
                 names_to = "components",
                 values_to = "values") %>% 
    model(Mean = MEAN(values),
          Drift = RW(values ~ drift()),
          Naive = NAIVE(values),
          SNaive = SNAIVE(values ~ lag("year")),
          ETS = ETS(values))  %>% 
    forecast(h = 12)  %>% 
    autoplot(level = FALSE) +
    autolayer(x11_dcmp_test %>% filter(year(date) >= 2007)) +
    facet_grid(vars(components),
               scales = "free_y") +
    labs(title = "Forecast of X11 decomposition",
         subtitle = "unemployed = trend + seasonal + irregular",
         y = "Unemployment level",
         x = "Month")  # HUSK Å FIKSE LEGENDS



### Model types
x11_models <- x11_dcmp %>% 
    select(-seasonaladj) %>%
    pivot_longer(cols = seasonal:unemployed,
                 names_to = "components",
                 values_to = "values") %>% 
    model(Mean = MEAN(values),
          Drift = RW(values ~ drift()),
          Naive = NAIVE(values),
          SNaive = SNAIVE(values ~ lag("year")),
          ETS = ETS(values)) # HUKS Å SJEKKE ETS!!!


## Cross validation

source("cross_validation.r")
" Cross validate.
Finne rett steg
Optimale vindu
"


### ETS model ###



x11_dcmp %>%
    select(date, seasonaladj) %>% 
    model(ETS(seasonaladj)) %>% 
    forecast(h = 12) %>% 
    autoplot(level = 95) +
    autolayer(unemployment_test_ts  %>% filter(year(date) >= 2007), unemployed) +
    labs(title = "Forecast with X11",
         y = "Unemployment level",
         x = "Month")


#### ARIMA









