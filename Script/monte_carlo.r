########################################################################
#################### MONTECARLO  ######################################
########################################################################

#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Script")
setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
# Sourcing data from data.r 
source("data.r")

load(file = "../Data/optimal_models.Rdata")



unemployment_ts <- unemployment %>% as_tsibble(index = date)



set.seed(12345)


generate_y <- function(fit, n, diff = 1, seas_diff = 1) {
    #' 
    #' 
    sigma <- sd(residuals(fit)$.resid)
    resids <- residuals(fit)$.resid
    mean_resid <- mean(residuals(fit)$.resid)
    ar_terms <- fit  %>% coefficients %>% dplyr::select(term, estimate)  %>%  filter(str_detect(term, "ar")) # AR terms and their coefficients
    ma_terms <- fit  %>% coefficients %>% dplyr::select(term, estimate)  %>%  filter(str_detect(term, "ma")) # MA terms and their coefficients
    p <- ar_terms %>%  nrow()                                                                                # AR term number
    m <- ma_terms  %>%  nrow()                                                                               # MA term number
    b <- 10                                                                                                  # Burn-ins
    y <- rnorm(n+b, mean = mean(unemployment_ts$unemployed), sd = sigma)
    #y <- rnorm(n+b, mean = mean(unemployment_ts$unemployed), sd = (sigma/sqrt(n)))

    for (i in (p+2):(n+b)) {
        #y[i] <- y[i] +  y[i-1] 
        if (p > 0 ) {
            for(j in 1:p) {
                y[i] <- y[i] + (y[i-j] - y[i-j-1]) * ar_terms$estimate[j]
            }
        }
        if( m > 0 && i > 12*m) {
            for(k in 1:m) {
                y[i] <- y[i] + (resids[i-12*k] * ma_terms$estimate[k])
            }
        }
     }
    return (y)
}

testfit  <- unemployment_ts %>%                                     
            model(arima_optimal = ARIMA(unemployed, stepwise = TRUE, approximation = TRUE))
generate_y(testfit, 216, 1)
plot(generate_y(testfit, 216, 1))





'
train_length <- 216
h <- 24
R <- 10
fit <- testfit'

simulate <- function(fit, R, train_length , h ) {

    res <- matrix(0,2,5)
    colnames(res) <- c("RMSE", "MASE", "MAE", "MAPE",  "RMSSE")
    rownames(res) <- c("VAR multivariate", "ARIMA yt")
    for(i in 1:R){
        y <- generate_y(fit, train_length+h)
        y_e <- y[1:train_length]
        y_t <- y[(train_length+1):(train_length+h)]
        x <- c()
        x[1] <- y[1]
        for (j in 2:(train_length+h)) {
            x[j] <- 0.5*y[j-1] + 0.5*x[j-1] 
        }
        x_e <- x[1:train_length]
        x_t <- x[(train_length+1):(train_length+h)]
        data_x_y = data.frame(date = (1:train_length), x_e = x_e, y_e = y_e)  %>%  as_tsibble(index = date)
        ar_term <- VARselect(data_x_y[,2:3], lag.max =10, type="const")[["selection"]][[2]]            # Confirming AR term
        var_multi  <- vars:: VAR(data_x_y[,2:3], p  = 1,  type = "const")
        if (any(is.na(predict(var_multi, n.ahead = h)$fcst$y_e))) { 
            next
        }
        else {
            arima_uni <- data_x_y  %>% model(Arima = ARIMA(y_e, stepwise = TRUE, approximation = FALSE))
            #arima_uni <- data_x_y  %>% model(Arima =  ARIMA(y_e ~ pdq(1,0,0) + PDQ(0,0,0)))
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
    }
    
    return(res)
}
#simulate(testfit, R = 1, train_length = 160, h = 40)


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

simres  <- wrapperSim(R= 1000, sample_size = 240, test_ratio = 0.2)
simres


sample_sizes <- c(100, 150, 200, 240)

table <- data.frame()
for (size in sample_sizes) {
  table <- table %>% rbind(., wrapperSim(R= 1000, sample_size = size, test_ratio = 0.2))
}
save(table, file = "../Data/sim_data.Rdata")



table  %>% 
       kable(caption = "Monte Carlo simulations: 1000 sample paths ", label = "test", digits = 2) %>%
       kable_classic(full_width = F, html_font = "Times new roman") 


