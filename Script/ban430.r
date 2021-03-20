
#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Data")

### Libraries ----
library(fpp3)
library(readxl)
#library(doParallel)  
library(lubridate)
library(magrittr)
library(feasts)
library(seasonal)
library(RJDemetra)
library(rJava)
library(ggfortify)

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

unemployment  %>% 
    model(classical_decomposition(unemp_level, type = "additive"))


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
library(x13binary)
Sys.setenv(X13_PATH = "F:/x13binary/bin")
checkX13()

test <- ts(unemployment_train %>% select(unemp_level), start = c("1995"), frequency = 12)
x_13_seas <- seas(test)


x_13_df <- data.frame(
    x13 = x_13_seas)  %>% 
    mutate(date = yearmonth(x13.date))  %>% 
    select(-x13.date) %>%
    as_tsibble(index = date)




x13_mod <- x13(ts(unemployment_train %>% select(date, unemp_level), start = c("1995"), frequency = 12))
plot(x13_mod, type_chart = "sa-trend")
autoplot(x13_mod)


unemployment_train_ts  %>%  seas(x = unemp_level,
                arima.model = "(1 1 1)(0 1 1)",
                regression.aictest = NULL,
                outlier = NULL,
                transform.function = "none")




x11_dcmp <- unemployment_train_ts  %>% 
    model(x11 = feasts:::X11(unemp_level, type = "additive"))  %>% 
    components()

x11_dcmp  %>% 
    ggplot() +
    geom_line(aes(x= date, y = season_adjust, col = "seasonal adjusted")) +
    geom_line(aes(x= date, y = seasonal_unemp_level, col = "US labor statistics"), data = unemployment_train_ts)



x_13_df  %>% 
    ggplot() +
    geom_line(aes(x = date, y = x13.seasonaladj, col = "x13 SA")) +
    geom_line(aes(x= date, y = seasonal_unemp_level, col = "US labor statistics"), data = unemployment_train_ts) +
    geom_line(aes(x= date, y = season_adjust, col = "x11 SA"), data = x11_dcmp )


### ETS model ###

#### ARIMA

?seas()





