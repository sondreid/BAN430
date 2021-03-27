########################################################################
#################### MONTECARLO  ######################################
########################################################################

#setwd("/Users/olaiviken/Documents/BAN430/BAN430/Script")
#setwd("G:/Dokumenter/Google drive folder/NHH/Master/BAN430/Repository/Script")
# Sourcing data from data.r 
source("data.r")

load(file = "../Data/optimal_models.Rdata")

set.seed(12345)

unemployment_ts <- unemployment %>% as_tsibble(index = date)
arima_fit <- unemployment_ts  %>% 
    model(arima_optimal = ARIMA(unemployed, stepwise = FALSE, approximation = FALSE))

geny <- function(fit, n) {
    #' 
    #' Returns vector of residuals + random component
    sigma <- sd(residuals(arima_fit)$.resid)
    mean_resid <- mean(residuals(arima_fit)$.resid)
    y <- rnorm(n, mean = mean_resid, sd = sigma)
    # for (i in 2:(n)) {
    #     y[i] = y[i] +  y[i-1]  
    # }
    return (cumsum(y))
}


geny(arima_fit, 240)

R <- 100
h <- 24
n_e <- 216
n_t <- 24
res <- matrix(0,2,2)
colnames(res) <- c("Model", "RMSE")
res[,1] <- c("VAR multivariate", "ARIMA yt")
for(i in 1:R){
    y <- geny(arima_fit, n_e+n_t)
    y_e <- y[1:n_e]
    y_t <- y[n_e+1:n_e+n_t]
    x <- c()
    x[1] <- 0
    for (j in 2:n_e) {
         x[j] <- 0.5*y[j-1] + 0.5*x[j-1]
    }
    x_e <- x[1:n_e]
    x_t <- x[n_e+1:n_e+n_t]
    data_x_y = data.frame(date = unemployment_train_ts$date, x = x_e, y = y_e)  %>%  as_tsibble(index = date)
    ar_term <- VARselect(data_x_y[,2:3], lag.max =24, type="const")[["selection"]] # Confirming AR term
    var1  <- vars:: VAR(data_x_y[,2:3], p  = ar_term[[2]])
    res[1,1] <- res[1,1] + ((y_t - predict(var1, n.ahead = n_t)^2)/R)
    
}



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





fit <- VECM(finland, lag=2, estim="ML", r=1)
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
x[i,] <- x[i,]+ fit$coefficients[,r+1]+x[i-1,]%*%t(IPI) }
if(p>0){
for(j in 1:p){
x[i,] <- x[i,]+ (x[i-j,]-x[i-j-1,])%*%t(fit$coefficients[,(r+2+k*(j-1)):(r+1+k*j)])
}
}
}
x <- x[(b+1):(b+n),]
return(x)






# Generate y

S0 <- 

