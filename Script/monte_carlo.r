########################################################################
#################### MONTECARLO SIMULATION #############################
########################################################################

#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Script")
setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
# Sourcing data from data.r 
source("data.r")

load(file = "../Data/optimal_models.Rdata")

set.seed(12345)



generate_y <- function(fit, n) {
  #' Function that passes the standard deviation of the residuals of our optimal Arima model
  #' Automatically finds and passes ar and ma terms to the arima.sim (stats package), and
  #' returns the genereated series
  sigma <- sd(residuals(fit)$.resid)
  ar_terms <- (fit  %>% coefficients %>%  filter(str_detect(term, "ar")))$estimate %>% c(.) # AR terms and their coefficients
  sma_terms <- (fit  %>% coefficients %>%  filter(str_detect(term, "sma")))$estimate %>% c(.)
  arima_sim_model <- list(order = c(5, 1, 0), ar = ar_terms, sma = sma_terms)
  y <- arima.sim(n = n, arima_sim_model, sd = sigma)
  return(y)
}

### Example plot of a generated series based on optimal arima fit #####
data.frame(y = generate_y(fit_arima_optimal, 216)[1:216], date = unemployment_train_ts$date) %>% as_tsibble()  %>% 
  ggplot() +
  geom_line(aes(x = date, y = y, color = "Generated series")) +
  scale_colour_manual(values=c("black")) +
  theme_bw() + 
  theme(legend.position = "bottom") +    
  labs(title = "Sample generated series from estimated ARIMA model",
       y = "Generated values",
       x = "Month") +
  guides(colour = guide_legend(title = "Series"))



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
            x[j] <- 0.5*y[j-1] + 0.5*x[j-1] + rnorm(n = 1, mean = 0, sd = sd(y))
        }
        x_e <- x[1:train_length]
        x_t <- x[(train_length+1):(train_length+h)]
        data_x_y = data.frame(date = (1:train_length), x_e = x_e, y_e = y_e)  %>%  as_tsibble(index = date)
        var_multi  <- vars:: VAR(data_x_y[,2:3], p  = 1,  type = "const")                              # VAR(1) model
        arima_uni <- data_x_y  %>% model(Arima =  ARIMA(y_e ~ 0 + pdq(1,0,0) + PDQ(0,0,0)))            # ARIMA pdq(1,1,0) model
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



wrapperSim <- function(R, sample_size, test_ratio) {
        #' Wrapper function that splits the unemployment series into
        #' test and training lengths based on an input sample length and
        #' test ratio of the overall series length.
        #' Passes this as parameters to the simulate function
        cl <- parallel::makeCluster(parallel::detectCores())                                                                                         ### Make clusters
        doParallel::registerDoParallel(cl)
        train_length <- floor(sample_size * (1-test_ratio))
        h <- ceiling(sample_size* test_ratio)
        print(paste("Training length: ", train_length))
        print(paste("h : ", h))
        start <- (nrow(unemployment_train_ts) - sample_size)
        sim_res <- simulate(fit_arima_optimal, R, train_length, h) %>%
          as.data.frame() %>% 
          mutate("Sample length" = sample_size)
        parallel::stopCluster(cl)
        
        return(sim_res)
}
#simulate(testfit, R = 1, train_length = 160, h = 40)
simres  <- wrapperSim(R= 50, sample_size = 216, test_ratio = 0.2)
simres

# Sample sizes for simulation
sample_sizes <- c(50,100, 150, 200)

# Add to table
table <- data.frame()

# Loop through all sample sizes, and perform a Monte Carlo simulation using 1000 samples of y and x
for (size in sample_sizes) {
  table <- table %>% rbind(., wrapperSim(R= 1000, sample_size = size, test_ratio = 0.2)) #Populate table for each sample size
}
save(table, file = "../Data/sim_data.Rdata")


#### Print kable table ####
table  %>% 
       kable(caption = "Monte Carlo simulations: 1000 sample paths ", digits = 3) %>%
       kable_classic(full_width = F, html_font = "Times new roman") 

load("../Data/sim_data.Rdata")
