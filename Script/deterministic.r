###################################################################################################
############################### DETERMINISTIC FORECAST ############################################
###################################################################################################
#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Script")
#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
# Sourcing data from data.r 
source("data.r")

fit_deterministic <- unemployment_train_ts %>% 
  dplyr::select(date, unemployed) %>% 
  model(Deterministic = ARIMA(unemployed ~ 1 + trend() + pdq(d=0) + PDQ(d = 0)))

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


splines_deterministic <- splinef(unemployment_train_ts$unemployed, h  = 24)


###### FOURIER ######

fourier <- fourier(ts(unemployment_train_ts$unemployed, frequency =  12), K = 2, h  = 24)



fit_fourier <- unemployment_train_ts  %>% 
    dplyr::select(date, unemployed)   %>% 
    model(Fourier_k1 = ARIMA(unemployed ~ fourier(K = 1) + PDQ(0,0,0)),
          Fourier_k2 = ARIMA(unemployed ~ fourier(K = 2) + PDQ(0,0,0)),
          Fourier_k3 = ARIMA(unemployed ~ fourier(K = 3) + PDQ(0,0,0)),
          Fourier_k4 = ARIMA(unemployed ~ fourier(K = 4) + PDQ(0,0,0)),
          Fourier_k5 = ARIMA(unemployed ~ fourier(K = 5) + PDQ(0,0,0)),
          Fourier_k6 = ARIMA(unemployed ~ fourier(K = 6) + PDQ(0,0,0)))



fit_fourier  %>% glance()  %>% arrange(AICc)



fc_fourier <- fit_fourier %>% 
    forecast(h = 24)
    
    
fc_fourier%>% 
    autoplot(unemployment_test_ts  %>% filter(year(date) >= 2010), level = 95) +
    facet_wrap(vars(.model), ncol = 2) +
    theme_bw()
    

fc_fourier %>% accuracy(unemployment_test_ts)  %>%  arrange(MASE)


###### COMPARISONS#################
