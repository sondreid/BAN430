############################# HISTORY ########################


unemployment_dynamic_data <- unemployment_train_ts %>% 
  left_join(cpi_train, by = "date")  %>% 
  left_join(export_train, by = "date")  %>% 
  select(-seasonal_unemployed)

fit_tslm <- unemployment_dynamic_data  %>% 
  model(ARIMA(unemployed ~ cpi + export + pdq(0,0,0) + PDQ(0,0,0)))

train_tslm_data <- fit_tslm  %>% 
  augment()

new_tslm_data <- new_data(train_tslm_data, 24)  %>% 
  mutate(cpi = mean(unemployment_dynamic_data$cpi),
         export = mean(unemployment_dynamic_data$export))  %>% 
  select(-.model)  

fc_tslm <- forecast(fit_tslm, new_data = new_tslm_data)

fc_tslm %>% 
  autoplot(unemployment_test_ts)


############################# TABLE comparison ##############################

multivariate_var_table_data <- data.frame(Model = "Multivariate VAR model", 
                                          Type = "Test", 
                                          RMSE = RMSE(unemployment_test$unemployed, fc_multivariate_var$.mean_unemployed),
                                          MAE =  MAE(unemployment_test$unemployed, fc_multivariate_var$.mean_unemployed),
                                          MAPE = MAPE(unemployment_test$unemployed, fc_multivariate_var$.mean_unemployed),
                                          MASE = MASE(unemployment_test$unemployed, fc_multivariate_var$.mean_unemployed, .period = 1),
                                          RMSSE = RMSSE(unemployment_test$unemployed, fc_multivariate_var$.mean_unemployed, .period = 1))

fc_multivariate_arima  %>% select(.model, date, .mean)  %>% 
  accuracy(unemployment_test_ts) %>% 
  rename("Model" = .model,
         "Type" = .type)  %>% 
  mutate(Model = "Multivariate ARIMA")  %>% 
  select(Model:MAE, MAPE, MASE, RMSSE, -ME) %>%
  bind_rows(multivariate_var_table_data) %>% 
  kbl(caption = "Multivariate ARIMA and Var models", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")





wrapperSim <- function(R, sample_size, test_ratio) {
  #' Wrapper function that splits the unemployment series into
  #' test and training lengths based on an input sample length and
  #' test ratio of the overall series length.
  #' Passes this as parameters to the simulate function
  cl <- parallel::makeCluster(parallel::detectCores())                                                                                         ### Make clusters
  doParallel::registerDoParallel(cl)
  train_length <- floor(sample_size * (1-test_ratio))
  h <- ceiling(sample_size* test_ratio)
  print(train_length)
  print(h)
  start <- (nrow(unemployment_ts) - sample_size)
  arima_fit <- unemployment_ts[start:(nrow(unemployment_ts)), ]  %>%                                     
    model(arima_optimal = ARIMA(unemployed, stepwise = FALSE, approximation = FALSE))
  sim_res <- simulate(arima_fit, R, train_length, h) %>%
    as.data.frame() %>% 
    mutate("Sample length" = sample_size)
  parallel::stopCluster(cl)
  
  return(sim_res)
}


generate_y <- function(fit, n, diff = 1, seas_diff = 1) {
  #' 
  #' 
  sigma <- sd(residuals(fit)$.resid)
  resids <- residuals(fit)$.resid
  ar_terms <- fit  %>% coefficients %>% dplyr::select(term, estimate)  %>%  filter(str_detect(term, "ar")) # AR terms and their coefficients
  sma_terms <- fit  %>% coefficients %>% dplyr::select(term, estimate)  %>%  filter(str_detect(term, "sma")) # Seasonal moving average terms and their coefficients
  p <- ar_terms %>%  nrow()                                                                                # AR term number
  m <- sma_terms  %>%  nrow()                                                                               # MA term number
  b <- 10                                                                                                  # Burn-ins
  #y <- rnorm(n+b, mean = mean(unemployment_ts$unemployed), sd = sigma)
  y <- rnorm(n+b, mean = 0, sd = (sigma/sqrt(n)))
  
  for (i in (p+2):(n+b)) {
    #y[i] <- y[i] +  y[i-1] 
    if (p > 0 ) {
      for(j in 1:p) {
        y[i] <- y[i] + (y[i-j] - y[i-j-1]) * ar_terms$estimate[j]
      }
    }
    if( m > 0 && i > 12*m) {
      for(k in 1:m) {
        y[i] <- y[i] + (resids[i-12*k] * sma_terms$estimate[k])
      }
    }
  }
  return (y)
}



if (any(is.na(predict(var_multi, n.ahead = h)$fcst$y_e))) { 
  next
}


generate_y <- function(fit, n, diff = 1, seas_diff = 1) {
  #' 
  #' 
  sigma <- sd(residuals(fit)$.resid)
  resids <- residuals(fit)$.resid
  ar_terms <- fit  %>% coefficients %>% dplyr::select(term, estimate)  %>%  filter(str_detect(term, "ar")) # AR terms and their coefficients
  sma_terms <- fit  %>% coefficients %>% dplyr::select(term, estimate)  %>%  filter(str_detect(term, "sma")) # Seasonal moving average terms and their coefficients
  p <- ar_terms %>%  nrow()                                                                                # AR term number
  m <- sma_terms  %>%  nrow()                                                                               # MA term number
  b <- 10                                                                                                  # Burn-ins
  #y <- rnorm(n+b, mean = mean(unemployment_ts$unemployed), sd = sigma)
  y <- rnorm(n+b, mean = mean(unemployment_train_ts$unemployed), sd = (sigma/sqrt(n)))
  
  for (i in (p+2):(n+b)) {
    #y[i] <- y[i] +  y[i-1] 
    if (p > 0 ) {
      for(j in 1:p) {
        y[i] <- y[i] + (y[i-j] - y[i-j-1]) * ar_terms$estimate[j]
      }
    }
    if( m > 0 && i > 12*m) {
      for(k in 1:m) {
        y[i] <- y[i] + (resids[i-12*k] * sma_terms$estimate[k])
      }
    }
  }
  return (y)
}

simulate <- function(fit, R, train_length , h ) {
  #' Function that generates a new series x based on an arima simulation returned by generate_y. 
  #' Compares two models, and populates which contains a series of forecast evaluation metrics.
  #' Returns the populated matrix.
  res <- matrix(0,2,5)
  colnames(res) <- c("RMSE", "MASE", "MAE", "MAPE",  "RMSSE")
  rownames(res) <- c("VAR multivariate", "ARIMA yt")
  for(i in 1:R){
    y <- diff(generate_y(fit, train_length+h))
    y_e <- y[1:train_length]
    y_t <- y[(train_length+1):(train_length+h)]
    x <- c()
    x[1] <- y[1]
    for (j in 2:(train_length+h)) {
      x[j] <- 0.5*y[j-1] + 0.5*x[j-1] + rnorm(1, 0, sd(y))
    }
    x_e <- x[1:train_length]
    x_t <- x[(train_length+1):(train_length+h)]
    data_x_y = data.frame(date = (1:train_length), x_e = x_e, y_e = y_e)  %>%  as_tsibble(index = date)
    ar_term <- VARselect(data_x_y[,2:3], lag.max =10, type="const")[["selection"]][[2]]            # Confirming AR term
    var_multi  <- vars:: VAR(data_x_y[,2:3], p  = 1,  type = "const")                              # VAR(1) model
    arima_uni <- data_x_y  %>% model(Arima =  ARIMA(y_e ~ 1 + pdq(1,0,0) + PDQ(0,0,0)))
    var_resids <-   y_t -  predict(var_multi, n.ahead = h)$fcst$y_e[,1]
    arima_resids <- y_t -  (arima_uni %>% forecast(h = h))$.mean                            
    
    res[1,1] <- res[1,1] + RMSE(var_resids)/R     
    res[2,1] <- res[2,1] + RMSE(arima_resids)/R  
    
    res[1,2] <- res[1,2] + MASE(.resid = var_resids, .train = y_e, .period = 12)/R   
    res[2,2] <- res[2,2] + MASE(.resid = arima_resids, .train = y_e, .period = 12)/R  
    
    res[1,3] <- res[1,3] + MAE(.resid = var_resids)/R   
    res[2,3] <- res[2,3] + MAE(.resid = arima_resids)/R   
    
    res[1,4] <- res[1,4] + fabletools::MAPE(.resid = var_resids, .actual = y_t, .period = 12)/R   
    res[2,4] <- res[2,4] + fabletools::MAPE(.resid = arima_resids, .actual = y_t, .period = 12)/R   
    
    
    res[1,5] <- res[1,5] + RMSSE(.resid = var_resids, .train = y_e, .period = 12)/R   
    res[2,5] <- res[2,5] + RMSSE(.resid = arima_resids, .train = y_e, .period = 12)/R   
    
  }
  
  return(res)
}
#simulate(testfit, R = 1, train_length = 160, h = 40)


