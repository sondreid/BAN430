
"Load libraries and functions from main script "
source("ban430.r")

# Cross-Validation with step = 1 ----
unemployment_train_cv_1 <- unemployment_2018 %>%
    stretch_tsibble(.init = 3, .step = 1)

unemployment_train_cv_3 <- unemployment_2018 %>%
    stretch_tsibble(.init = 5, .step = 3)

fit_cv_func <- function(train_df) {
    fit_cv <- train_df %>% 
        model(  Mean = MEAN(unemp_level),
                Naive = NAIVE(unemp_level),
                Seasonal_Naive = SNAIVE(unemp_level ~ lag("year")),
                Drift = RW(unemp_level ~ drift())
                ) 
    return(fit_cv)
}


fit_cv_1 <- fit_cv_func(unemployment_train_cv_1)
fit_cv_3 <- fit_cv_func(unemployment_train_cv_3)

fc_cv_1 <- fit_cv_1 %>% 
    forecast(h = 8)

fc_cv_3 <- fit_cv_3 %>% 
    forecast(h = 8)

accuracy(fc_cv_1, unemployment)
accuracy(fc_cv_3, unemployment)