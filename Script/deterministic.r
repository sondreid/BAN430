###################################################################################################
############################### DETERMINISTIC FORECAST ############################################
###################################################################################################
#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Script")
#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
# Sourcing data from data.r 
source("data.r")


## Fit arima model with linear deterministic trend
fit_deterministic <- unemployment_train_ts %>% 
  dplyr::select(date, unemployed) %>% 
  model("Deterministic trend" = ARIMA(unemployed ~ 1 + trend() + pdq(d = 0)))

fit_deterministic %>% report()

## Print kable table of coefficients
fit_deterministic %>% report() %>% coefficients() %>% dplyr::select(term, estimate) %>%  
  kbl(caption = "Deterministic trend", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")

# Forecast using arima model with linear determinstic trend
fc_deterministic <- fit_deterministic %>% 
  forecast(h = 24)


## Residuals of linear determinstic trend model
ggtsdisplay(Residuals, 
            plot.type = "histogram", 
            lag.max = 24,
            theme = theme_bw(),
            main = paste("Residuals of ARIMA ", fit_deterministic$`Deterministic trend`))

### Plot of forecast
fc_deterministic %>% 
  autoplot(unemployment_test_ts %>% filter(year(date) >= 2015), level = 95) +
  labs(title = "Deterministic trend forecast",
       subtitle = fit_deterministic$Deterministic,
       x = "Month",
       y = "Unemployment level") +
  theme_bw() +
  theme(legend.position = "bottom") + 
  guides(level = guide_legend(title = "Prediction level"))

fc_deterministic_table <- fc_deterministic %>% 
  accuracy(unemployment_test_ts) 


# Accuracy table
fc_deterministic_table   %>%  
  arrange(MASE) %>% 
  rename("Model"  = .model) %>% 
  dplyr::select(-.type, -ME, -ACF1) %>% 
  kbl(caption = "Deterministic forecast accuracy", digits = 3) %>%
  kable_classic(full_width = F, html_font = "Times new roman")


####### SPLINES #######


splines_deterministic <- splinef(unemployment_train_ts$unemployed, h  = 24)
splines_deterministic %>% summary()
splines_deterministic %>% autoplot()

splines_forecast <- c(splines_deterministic$mean)


resids <- c(unemployment_test_ts$unemployed) - splines_forecast

splines <- unemployment_train_ts %>%  model(splines = splinef(unemployment_train_ts$unemployed, h = 24))


###### FOURIER ######

fourier <- fourier(ts(unemployment_train_ts$unemployed, frequency =  12), K = 2, h  = 24)


### Not differenced
fit_fourier <- unemployment_train_ts  %>% 
    dplyr::select(date, unemployed)   %>% 
    model("Fourier K1" = ARIMA(unemployed ~ fourier(K = 1)  + PDQ(0,0,0)),
          "Fourier K2" = ARIMA(unemployed ~ fourier(K = 2)  + PDQ(0,0,0)),
          "Fourier K3" = ARIMA(unemployed ~ fourier(K = 3)  + PDQ(0,0,0)),
          "Fourier K4" = ARIMA(unemployed ~ fourier(K = 4)  + PDQ(0,0,0)),
          "Fourier K5" = ARIMA(unemployed ~ fourier(K = 5)  + PDQ(0,0,0)),
          "Fourier K6" = ARIMA(unemployed ~ fourier(K = 6)  + PDQ(0,0,0)))

### Differenced

fit_fourier <- unemployment_train_ts  %>% 
  dplyr::select(date, unemployed)   %>% 
  model("Fourier K1" = ARIMA(unemployed ~ fourier(K = 1) +  PDQ(0,0,0)),
        "Fourier K2" = ARIMA(unemployed ~ fourier(K = 2) +  PDQ(0,0,0)),
        "Fourier K3" = ARIMA(unemployed ~ fourier(K = 3) +  PDQ(0,0,0)),
        "Fourier K4" = ARIMA(unemployed ~ fourier(K = 4) +  PDQ(0,0,0)),
        "Fourier K5" = ARIMA(unemployed ~ fourier(K = 5) +  PDQ(0,0,0)),
        "Fourier K6" = ARIMA(unemployed ~ fourier(K = 6) +  PDQ(0,0,0)))

fit_fourier %>% 
  dplyr::select("Fourier K2") %>% 
  report()

fit_fourier %>% dplyr::select("Fourier K5")


fit_fourier %>% 
  dplyr::select("Fourier K2") %>% 
  coefficients() %>% 
  dplyr::select(term, estimate) %>% 
  kbl(caption = "Fourier terms", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")

fit_fourier  %>% glance()  %>% arrange(AICc)


fc_fourier <- fit_fourier %>% 
    forecast(h = 24)
    
    
fc_fourier%>% 
  autoplot(unemployment_test_ts  %>% filter(year(date) >= 2015), level = 95) +
  facet_wrap(facets = vars(.model), ncol = 3) +
  labs(title = "Fourier terms forecast",
       x = "Month",
       y = "Unemployment level") +
  theme_bw() +
  guides(level = FALSE) +
  theme(legend.position = "bottom")
    


fc_fourier_table <- 
  fc_fourier %>% 
  accuracy(unemployment_test_ts)


## Mulig ? slette #################################
fc_fourier_table %>% 
  arrange(MASE) %>% 
  rename("Model"  = .model) %>% 
  dplyr::select(-.type, -ME, -ACF1) %>% 
  kbl(caption = "Fourier forecast accuracy", digits = 3) %>%
  kable_classic(full_width = F, html_font = "Times new roman")


###### COMPARISONS#################

fc_fourier_table %>% 
  bind_rows(
  fc_deterministic_table)  %>% 
  rename("Model"  = .model) %>% 
  arrange(MASE) %>% 
  dplyr::select(-.type, -ME, -ACF1) %>% 
  kbl(caption = "Deterministic forecasting methods", digits = 3) %>%
  kable_classic(full_width = F, html_font = "Times new roman")

save(fc_fourier_table,  file = "../Data/fourier_table.Rdata")

