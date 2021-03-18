
#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Data")

### Libraries ----
library(fpp3)
library(readxl)
#library(ggfortify)


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

unemployment <- df  %>% 
    mutate(date = yearmonth(date))  %>% 
    filter(year(date) >= 1995)   %>% 
    select(date, unemp_level, seasonal_unemp_level)   %>% 
    tsibble(index = date)


# Point forecasting accuracy ----
train <- unemployment  %>% 
    filter(year(date) < 2019)  %>% 
    select(date, unemp_level)

fit <- train  %>% 
    model(  Mean = MEAN(unemp_level),
            Naive = NAIVE(unemp_level),
            Seasonal_Naive = SNAIVE(unemp_level),
            Drift = RW(unemp_level ~ drift()))


fc <- fit %>% 
    forecast(h = 8) %> %>% 
    accuracy(unemployment)



# Cross-Validation with step = 1 ----
unemployment_train_cv_1 <- unemployment %>%
    filter(year(date) < 2020)  %>% 
    stretch_tsibble(.init = 3, .step = 1)

unemployment_train_cv_3 <- unemployment %>% 
    filter(year(date) < 2020)  %>% 
    stretch_tsibble(.init = 5, .step = 3)

fit_cv <- unemployment_train_cv_1 %>% 
    model(  Mean = MEAN(unemp_level),
            Naive = NAIVE(unemp_level),
            Seasonal_Naive = SNAIVE(unemp_level ~ lag("year")),
            Drift = RW(unemp_level ~ drift())) 

fc_cv <- fit_cv %>% 
    forecast(h = 8)

accuracy_cv <- accuracy(fc_cv, unemployment)

unemployment %>% 
    ggplot() +
    geom_line(aes(x = date, y = unemp_level, col = "Unadjusted seasonal")) +
    geom_line(aes(x = date, y = seasonal_unemp_level, col = "Adjusted seasonal")) +
    labs(   title = "Unemployment in USA",
            subtitle = "Seasonal adj. vs non adj.",
            y = "Unemployment level",
            x = "Months") +
    guides(col = guide_legend(title = "Series:"))

unemployment  %>% 
    model(classical_decomposition(unemp_level, type = "additive"))

autoplot(fc_cv, level = NULL)
fc_cv

