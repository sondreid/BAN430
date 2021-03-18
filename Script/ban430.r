
#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Data")

### Libraries ----
library(fpp3)
library(readxl)


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
    mutate(date = as.Date(date))  %>% 
    filter(year(date) >= 1995)   %>% 
    select(date, unemp_level, seasonal_unemp_level)  %>% 
    tsibble()

# Cross-Validation with step = 8 ----
unemployment_train_cv <- unemployment %>% 
    stretch_tsibble(.init = 3, .step = 1)

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

