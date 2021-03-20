
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
Sys.setenv(X13_PATH = "/Users/olaiviken/Documents/BAN430/BAN430/x13binary/bin")
#Sys.setenv(X13_PATH = "../windows_x13/bin")
checkX13()

df <- read_xls("../Data/US/Forecasting_economic_data.xls", sheet = 2)


clear_names <- c(   "date", 
                    "seasonal_unemp_men", 
                    "seasonal_unemp_women", 
                    "seasonal_unemp_less_high_school", 
                    "seasonal_unemp_high_school", 
                    "seasonal_unemp_college", 
                    "unemp_level", 
                    "unemp_men" , 
                    "unemp_women", 
                    "unemp_less_high_school",
                    "unemp_high_school",
                    "unemp_college", 
                    "seasonal_unemp_level")

colnames(df) <- clear_names

# unemployment %>% 
#     ggplot() +
#     geom_line(aes(x = date, y = unemp_level, col = "Unadjusted seasonal")) +
#     geom_line(aes(x = date, y = seasonal_unemp_level, col = "Adjusted seasonal")) +
#     labs(   title = "Unemployment in USA",
#             subtitle = "Seasonal adj. vs non adj.",
#             y = "Unemployment level",
#             x = "Months") +
#     guides(col = guide_legend(title = "Series:")) 

#unemployment  %>% 
#    model(classical_decomposition(unemp_level, type = "additive"))


unemployment <- df  %>% 
    mutate(date = yearmonth(date))  %>% 
    filter(year(date) >= 1995 & year(date) <= 2019)   %>% 
    select(date, unemp_level, seasonal_unemp_level)   


unemployment_train <- unemployment  %>% 
    filter(year(date) <= 2018)  %>% 
    select(date, unemp_level, seasonal_unemp_level)



###### Summary statistics #########


### Mean, median etc.


unemployment_train  %>% 
    mutate(month = lubridate::month(date))  %>%
    group_by(month)  %>% 
    summarise(  min = min(unemp_level),
                "25%-percentil" = quantile(unemp_level, 0.25),
                mean = mean(unemp_level),
                median = median(unemp_level),
                "75%-percentil" = quantile(unemp_level, 0.75),
                max = max(unemp_level)) 
    


unemployment_train_ts <-  unemployment_train %>% 
    as_tsibble(index = date) 

#Seasonal subseries
unemployment_train_ts  %>%  
    gg_subseries(unemp_level) +
    labs(x = "Month", y = "Unemployment level")

# Train series
unemployment_train_ts   %>%  
    ggplot() +
    geom_line(aes(x = date, y = unemp_level, col = "Training data")) +
    labs(   title = "Unemployment",
            subtitle = "Train [1995-2018]",
            y = "Unemployment level",
            x = "Month") +
    guides(col = FALSE) 



## SEAS X13
x13_seas <- seas(ts(unemployment_train %>% select(unemp_level), start = c("1995"), frequency = 12))

x11_seas <- seas(ts(unemployment_train %>% select(unemp_level), start = c("1995"), frequency = 12), x11 = "")

#Convert results to dataframe
x13_dcmp <- data.frame(x13_seas) %>% 
    left_join(select(unemployment_train_ts, unemp_level), by = "date") %>% 
    select(-adjustfac, -final)  %>% 
    mutate(date = yearmonth(date))  %>%
    as_tsibble(index = date)


x11_dcmp <- data.frame(x11_seas) %>%
    left_join(select(unemployment_train_ts, unemp_level), by = "date") %>% 
    select(-adjustfac, -final)  %>% 
    mutate(date = yearmonth(date)) %>% 
    as_tsibble(index = date)


x11_dcmp_feasts <- unemployment_train_ts  %>% 
    model(x11 = feasts:::X11(unemp_level, type = "additive"))  %>% 
    components()

x11_dcmp_feasts  %>% 
    ggplot() +
    geom_line(aes(x= date, y = season_adjust, col = "seasonal adjusted")) +
    geom_line(aes(x= date, y = seasonal_unemp_level, col = "US labor statistics"), data = unemployment_train_ts)



x13_dcmp  %>% 
    ggplot() +
    geom_line(aes(x = date, y = seasonaladj, col = "x13 SA")) +
    geom_line(aes(x= date, y = seasonal_unemp_level, col = "US labor statistics"), data = unemployment_train_ts) +
    geom_line(aes(x= date, y = season_adjust, col = "x11 SA"), data = x11_dcmp )


#Compare RMSE to find closest fit to original US labor statistics decomposition

# x13 with seas
mean((unemployment_train_ts$seasonal_unemp_level - x13_dcmp$seasonaladj)^2)

## X11 with feasts
mean((unemployment_train_ts$seasonal_unemp_level - x11_dcmp_feasts$season_adjust)^2)

# X11 with seas
mean((unemployment_train_ts$seasonal_unemp_level - x11_dcmp$seasonaladj)^2)


# Decomposing of the series ----
x13_dcmp %>% 
    select(-seasonaladj) %>% 
    pivot_longer(cols = seasonal:unemp_level,
                 names_to = "components",
                 values_to = "values") %>% 
    autoplot() +
    facet_grid(vars(components),
               scales = "free_y") +
    labs(title = "Decomposition of X13",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = guide_legend("Components"))

x11_dcmp %>% 
    select(-seasonaladj) %>% 
    pivot_longer(cols = seasonal:unemp_level,
                 names_to = "components",
                 values_to = "values") %>% 
    autoplot() +
    facet_grid(vars(components),
               scales = "free_y") +
    labs(title = "Decomposition of X11",
         y = "Unemployment level",
         x = "Month") +
    guides(colour = guide_legend("Components"))





# Only for checking
x13_dcmp %>% 
    data.frame() %>% 
    select(-date, -seasonaladj) %>% 
    apply(., 1, sum)





### ETS model ###

#### ARIMA







