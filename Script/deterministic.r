###################################################################################################
############################### DETERMINISTIC FORECAST ############################################
###################################################################################################
#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Script")
#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
# Sourcing data from data.r 
source("data.r")

fit_deterministic <- unemployment_train_ts %>% 
  dplyr::select(date, unemployed) %>% 
  model(Deterministic = ARIMA(unemployed ~ 1 + trend() + pdq(d=0) + PDQ(d = 0))

fc_deterministic <- fit_deterministic %>% 
  forecast(h = 24)

ggtsdisplay(Residuals, 
            plot.type = "histogram", 
            lag.max = 24,
            theme = theme_bw(),
            main = paste("Residuals of ARIMA ", fit_deterministic$Deterministic))

fc_deterministic %>% 
  autoplot(unemployment_test_ts, level = 95) +
  labs(title = "Deterministic forecast",
       x = "Month",
       y = "Unemployment level") +
  theme_bw() +
  theme(legend.position = "bottom") + 
  guides(level = guide_legend(title = "Prediction level"))




####### SPLINES #######


splines_deterministic <- splinef(unemployment_train_ts$unemployed, h  = 24) %>% plot()


###### FOURIER ######

fourier(ts(unemployment_train_ts$unemployed), K = 1, h  = 24)



###### COMPARISONS#################
