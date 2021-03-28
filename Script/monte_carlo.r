########################################################################
#################### MONTECARLO  ######################################
########################################################################

#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Script")
#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
# Sourcing data from data.r 
source("data.r")

load(file = "../Data/optimal_models.Rdata")



unemployment_ts <- unemployment %>% as_tsibble(index = date)
arima_fit <- unemployment_ts  %>% 
    model(arima_optimal = ARIMA(unemployed, stepwise = FALSE, approximation = FALSE))

arima_fit  %>% forecast(h=24)  %>% autoplot() + autolayer(unemployment_ts)

gg_tsresiduals(arima_fit)

set.seed(12345)
n <- 240
fit <- arima_fit
generate_y <- function(fit, n, d = 1, sd = 1) {
    #' 
    #' Returns vector of residuals + random component
    sigma <- sd(residuals(fit)$.resid)
    resids <- residuals(fit)$.resid
    mean_resid <- mean(residuals(fit)$.resid)
    ar_terms <- arima_fit  %>% coefficients %>% dplyr::select(term, estimate)  %>%  filter(str_detect(term, "ar")) # AR terms and their coefficients
    ma_terms <- arima_fit  %>% coefficients %>% dplyr::select(term, estimate)  %>%  filter(str_detect(term, "ma")) # AR terms and their coefficients
    p <- ar_terms %>%  nrow() # AR term number
    m <- ma_terms  %>%  nrow()
    y <- rnorm(n, mean = mean(unemployment_ts$unemployed), sd = (sigma/sqrt(n)))
    # if (d > 0 && i > 1) {
    #     for (f in 1:d ) {
    #             y <- difference(y)
    #         }
    #     }
    # if (sd > 0 && i > 12) {
    #     for (b in 1:sd ) {
    #             y <- difference(y, lag = 12)
    #         }
    #     }
    for (i in (p+2):n) {
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
generate_y(arima_fit, 216, 1)

plot(generate_y(arima_fit, 216), type = "l")
library(foreach)


determineTrainTest <- function(h = 24, maxlength = 19) {
        num_h <- sample(1:19, 1)*12
        train <- num_h - 24
        train_range <- c((1:train))
        test_range <-  c((train+1):num_h)
        return(c(train_range, test_range))
}
determineTrainTest()


simulate <- function(R = 100, train_length = 216, h = 24) {
    cl <- parallel::makeCluster(8)                                                                                         ### Make clusters
    doParallel::registerDoParallel(cl)
    res <- matrix(0,2,1)
    colnames(res) <- c("MSE")
    rownames(res)<- c("VAR multivariate", "ARIMA yt")
    for(i in 1:R){
        num_h <- sample(1:19, 1)*12
        train <- num_h - 24
        train_range <- c((1:train))
        test_range <-  c((train+1):num_h)


        y <- generate_y(arima_fit, train_length+h)
        y_e <- y[1:train_length]
        y_t <- y[(train_length+1):(train_length+h)]
        x <- c()
        x[1] <- 0
        for (j in 2:(train_length+h)) {
            x[j] <- 0.5*y[j-1] + 0.5*x[j-1] 
        }
        x_e <- x[1:train_length]
        x_t <- x[(train_length+1):(train_length+h)]
        data_x_y = data.frame(date = unemployment_train_ts$date, x_e = x_e, y_e = y_e)  %>%  as_tsibble(index = date)
        ar_term <- VARselect(data_x_y[,2:3], lag.max =10, type="const")[["selection"]] # Confirming AR term
        var_multi  <- vars:: VAR(data_x_y[,2:3], p  = ar_term[[2]])
        #var_multi <- data_x_y %>% model(var_multi = fable::VAR(vars(y_e, x_e) ~ AR(p = 0:5)))
        # print(paste("y_t", y_t))
        # print(paste("y_e", y_e))
        # print(y_t- predict(var_multi, n.ahead = 24)$fcst$y_e)
        if (any(is.na(predict(var_multi, n.ahead = 24)$fcst$y_e))) { 
            next
        }
        else {
            res[1] <- res[1] + (y_t -  predict(var_multi, n.ahead = 24)$fcst$y_e)[,1]^2/R
            arima_uni <- data_x_y  %>% model(Arima = ARIMA(y_e, stepwise = TRUE, approximation = TRUE))
            res[2] <- res[2] + (y_t -  predict(arima_uni)$.mean)^2/R
        }
    }
    parallel::stopCluster(cl)
    return(res)
}

sim_res <- simulate()

sim_res  %>% 
  kbl(caption = "Metrics of Monte Carlo simulated forecasts on generated data", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times new roman")









#### UNDER TESTING
y <- arima.sim(model = list(
                            ar = c(0.07207483, 0.18150995, 0.07974563, 0.04508594, 0.28128337),
                            ma = c(-0.87115053)), n = 240,
                            n.start = 24,
                            start.innov = augment(arima_fit)$.innov)


plot(y)

?arima.sim

report(arima_fit)
cov(residuals(arima_fit)$.resid)

library(mvtnorm)
data(finland)
test <- VECM(finland, lag=2, estim="ML", r=1)
sigma <- cov(test$residuals)
rmvnorm(n+b, mean =rep(0,nrow(sigma)), sigma = sigma)
test$k





set.seed(12345)
library(urca)
library(tsDyn)
library(mvtnorm)
data(finland)
fit <- VECM(finland, lag=2, estim="ML", r=1)
genx <- function(fit,n){
sigma <- cov(fit$residuals)
k <- fit$k
p <- fit$lag
r <- fit$model.specific$r
if(r>0){
beta <- fit$model.specific$beta
alpha <- fit$coefficients[,1:r]
IPI <- diag(k)+alpha%*%t(beta)
}else{
IPI <- diag(k)
}
# Number of burn in obs...
b <- 10
x <- rmvnorm(n+b, mean = rep(0, nrow(sigma)), sigma = sigma)
for(i in (p+2):(n+b)){
x[i,] <- x[i,]+ fit$coefficients[,r+1]+x[i-1,]%*%t(IPI)
if(p>0){
for(j in 1:p){
x[i,] <- x[i,]+ (x[i-j,]-x[i-j-1,])%*%t(fit$coefficients[,(r+2+k*(j-1)):(r+1+k*j)])
}
}
}
x <- x[(b+1):(b+n),]
return(x)
}


genx(fit, 200)