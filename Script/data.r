###############################################################################
################ Working Directory for Windows and Mac ########################
###############################################################################

#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Script")

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
library(vars)
library(lubridate)
library(magrittr)
library(tidyverse)
library(forecast)
library(feasts)
library(janitor)
library(seasonal)
#library(x13binary)
library(kableExtra)
library(tseries)
library(urca) 
library(latex2exp)
library(tsDyn)
library(bvartools)

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


cpi_data <- read_csv("../Data/US/cpi_data.csv") %>% 
    rename("date" = TIME, "cpi" = Value)  %>% 
    mutate(date = yearmonth(as.Date(paste(as.character(date), "-01", sep =""))))  %>% 
    filter(year(date) >= 2000, LOCATION == "USA")   %>% 
    dplyr::select(date, cpi)  


export_data <- read_csv("../Data/US/export_data.csv")  %>% 
    rename("date" = TIME, "export" = Value)  %>% 
    mutate(date = yearmonth(as.Date(paste(as.character(date), "-01", sep =""))))  %>% 
    filter(LOCATION == "USA", year(date) >= 2000 & year(date) <= 2019) %>%
    dplyr::select(date, export)  

unemployment <- unemp_df  %>% 
    mutate(date = yearmonth(date))  %>% 
    filter(year(date) >= 2000 & year(date) <= 2019)   %>% 
    dplyr::select(date, unemployed, seasonal_unemployed)

unemployment_with_COVID19 <- unemp_df  %>% 
    mutate(date = yearmonth(date))  %>% 
    filter(year(date) >= 2000 & year(date) <= 2020)   %>% 
    dplyr::select(date, unemployed, seasonal_unemployed)
###############################################################################
############################ LOAD DATA ########################################
###############################################################################
#save(unemployment, cpi_data, export_data, unemployment_with_COVID19, file =  "../Data/unemployment_cpi_exports.Rdata" )
load(file = "../Data/unemployment_cpi_exports.Rdata")


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

