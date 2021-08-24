# RESULTS : Long ARIMA Model Training and Prediction

# 1 Get Set Up
rm(list=ls())
setwd("~/summerproj/")
#install.packages("Metrics")
#install.packages("fUnitRoots")
#install.packages("aTSA")
library(fUnitRoots)
library(Metrics)
library(dplyr)
library(tictoc)
library(lubridate)
library(tidyr)
library(ggplot2)
library(tsibble)
library(feasts)
library(latex2exp)
library(fUnitRoots)
library(fable)
library(fabletools)
#install.packages("forecast")
#install.packages("tseries")
library(forecast)



## I have my original data: train
## I have my model: fit
## I have my predictions: predictions

## Now I need to make sure I show I've done the proper model development process

# (0) First, check what your model actually is
# (1) Identification: Show plot + ACF/PACF. Test stationarity with ADF and KPSS tests
# (2) Estimation: Show what your mdel actually is, AICc values etc
# (3) Diagnosis: Check residuals plot + Ljung-Box test and Durbin-Watson test to test significant difference from white noise

######### NO EXTERNAL PREDICTORS #########

######### (0) #########
train <- readRDS('results_data/long_expost.rds') %>% dplyr::filter(traintest == 0)

load("results_data/long_timeonly_ARIMA_fit")

# get lambda
lambda <- train %>%
  features(texts, features = guerrero) %>%
  pull(lambda_guerrero)

fit %>% pivot_longer(everything(), names_to = "Model name",
                     values_to = "Orders")
errors <- summary(lm(box_cox(texts,lambda) ~ localtime + weekday, data=train))$residuals
errors_time <- seq(from=1,to=length(errors))

length(train$chunk[1:9143])
errors_time <- train$chunk[1:9143]

errors <- cbind(errors_time,errors) %>% as.data.frame %>% mutate(errors_time=as.POSIXct(errors_time, origin='1970-01-01')) %>% as_tsibble(index=errors_time)
errors
######### (1) #########
theme_set(theme_bw())

png(file="results_output/08_ARIMA_timeonly_ident_plot.png", width=4, height=5, units="in", res=300)
errors %>% gg_tsdisplay(difference(errors), plot_type='partial', lag=24) +
  labs(title="Single differenced time series", y="Texts, transf. & diff.",x="Time")
dev.off()

show <- train %>% gg_tsdisplay(difference(box_cox(texts,lambda)), plot_type='partial', lag=24) +
  labs(title="Differencing", y="",x="Time")

# ADF test: Null hypothesis that data IS NOT stationary
adfTest(train$texts)
adfTest(difference(errors$errors))

# KPSS test: Null hypothesis is that time series IS stationary
library(tseries)
kpss.test(difference(errors$errors))

######### (2) #########

# Check the automatic findings of the model
fit %>% pivot_longer(everything(), names_to = "Model name", values_to = "Orders")
# Check the coefficients of the model
coef <- tidy(fit) %>% dplyr::select(term, estimate, std.error, p.value)
head(coef)
write.csv(coef, "results_output/08_ARIMA_timeonly_coefs.csv")
# Check AICc: Lower is better
glance(fit) %>% arrange(AICc) %>% dplyr::select(.model:BIC)

######### (3) #########

# Plot residuals: should look like white noise

png(file="results_output/08_ARIMA_timeonly_diagn_plot.png", width=4, height=5, units="in", res=300)
fit %>% gg_tsresiduals(lag=48) + labs(title="Model residuals", x="Time")
dev.off()

# Ljung-Box test (if it has a large p value, we're good):
aug <- fit %>% augment
aug %>% dplyr::select(.innov) %>% features(.innov, ljung_box, lag=10)
aug %>% dplyr::select(.innov) %>% features(.innov, ljung_box, lag=1)
aug %>% dplyr::select(.innov) %>% features(.innov, ljung_box, lag=10)
aug %>% dplyr::select(.innov) %>% features(.innov, ljung_box, lag=20)
aug %>% dplyr::select(.innov) %>% features(.innov, ljung_box, lag=24)
aug %>% dplyr::select(.innov) %>% features(.innov, ljung_box, lag=48)

# Durbin-Watson test for autocorrelation in rsiduals: Null = uncorrelated. If P value is large we're good
#library(car)
#car::durbinWatsonTest(aug$.resid)









######### WITH EXTERNAL PREDICTORS #########

load("results_data/long_ext_ARIMA_fit")

######### (0) #########
train <- readRDS('results_data/long_expost.rds') %>% dplyr::filter(traintest == 0)

# get lambda
lambda <- train %>%
  features(texts, features = guerrero) %>%
  pull(lambda_guerrero)

fit %>% pivot_longer(everything(), names_to = "Model name",
                     values_to = "Orders")

errors <- summary(lm(box_cox(texts,lambda) ~ localtime + weekday + daily_temp + clouds + covid_deaths + covid_announcements + log(tweet_avg+0.1) + log(retweet_avg+0.1) + log(likes_avg+0.1), data=train))$residuals
errors_time <- seq(from=1,to=length(errors))

length(train$chunk[1:9143])
errors_time <- train$chunk[1:9143]

errors <- cbind(errors_time,errors) %>% as.data.frame %>% mutate(errors_time=as.POSIXct(errors_time, origin='1970-01-01')) %>% as_tsibble(index=errors_time)
errors

######### (1) #########
theme_set(theme_bw())

png(file="results_output/08_ARIMA_ext_ident_plot.png", width=4, height=5, units="in", res=300)
errors %>% gg_tsdisplay(difference(errors), plot_type='partial', lag=24) +
  labs(title="Single differenced time series", y="Texts, transf. & diff.",x="Time")
dev.off()

#show <- train %>% gg_tsdisplay(difference(box_cox(texts,lambda)), plot_type='partial', lag=24) + labs(title="Differencing", y="",x="Time")

# ADF test: Null hypothesis that data IS NOT stationary
adfTest(errors$errors)
adfTest(difference(errors$errors))
adfTest(difference(errors$errors),lag=24)

# KPSS test: Null hypothesis is that time series IS stationary
library(tseries)
kpss.test(difference(errors$errors))

######### (2) #########

# Check the automatic findings of the model
fit %>% pivot_longer(everything(), names_to = "Model name", values_to = "Orders")
# Check the coefficients of the model
coef <- tidy(fit) %>% dplyr::select(term, estimate, std.error, p.value)
head(coef)
write.csv(coef, "results_output/08_ARIMA_ext_coefs.csv")
# Check AICc: Lower is better
glance(fit) %>% arrange(AICc) %>% dplyr::select(.model:BIC)

######### (3) #########

# Plot residuals: should look like white noise

png(file="results_output/08_ARIMA_ext_diagn_plot.png", width=4, height=5, units="in", res=300)
fit %>% gg_tsresiduals(lag=48) + labs(title="Model residuals", x="Time")
dev.off()

# Ljung-Box test (if it has a large p value, we're good):
aug <- fit %>% augment
aug %>% dplyr::select(.innov) %>% features(.innov, ljung_box, lag=10)
aug %>% dplyr::select(.innov) %>% features(.innov, ljung_box, lag=1)
aug %>% dplyr::select(.innov) %>% features(.innov, ljung_box, lag=10)
aug %>% dplyr::select(.innov) %>% features(.innov, ljung_box, lag=20)
aug %>% dplyr::select(.innov) %>% features(.innov, ljung_box, lag=24)
aug %>% dplyr::select(.innov) %>% features(.innov, ljung_box, lag=48)
