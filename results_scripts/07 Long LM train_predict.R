# RESULTS : Long Linear Model Training and Prediction

# 1 Get Set Up
rm(list=ls())
setwd("~/summerproj/")
#install.packages("Metrics")
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


make_prediction <- function(df,fit) {
  df <- forecast(fit, df, point_forecast = list(.mean = mean)) %>% merge(df,by="chunk") %>% dplyr::transmute(chunk,truth=texts.y,preds=.mean)
  return(df)
}

# Train without External Predictors

# read in correct data: train and test
train <- readRDS('results_data/long_expost_3motr.rds') %>% dplyr::filter(traintest == 0)
test <- readRDS('results_data/long_expost_3motr.rds') %>% dplyr::filter(traintest == 1)

# train SARIMAX model
tic("Train Time")
fit <- train %>% model(
  TSLM(sqrt(texts) ~ localtime + weekday)
)
toc()
#print(SARIMAXfit %>% pivot_longer(everything(), names_to = "Model name", values_to = "Orders"))
#fit %>% report()

# save trained models
save(fit,file='results_data/long_timeonly_LM_fit')
#load("results_data/long_timeonly_LM_fit")

# make predictions
predictions <- make_prediction(test,fit)

# save predictions
saveRDS(predictions, 'results_data/long_timeonly_LM_preds.rds')

# make TRAINING predictions
predictions <- make_prediction(train,fit)

# save TRAINING predictions
saveRDS(predictions, 'results_data/long_timeonly_LM_train_preds.rds')

# remove data, models, and predictions
rm(train)
rm(test)
rm(fit)
rm(predictions)


# Train with External Predictors

# read in correct data: train and test
train <- readRDS('results_data/long_expost.rds') %>% dplyr::filter(traintest == 0)
test <- readRDS('results_data/long_expost.rds') %>% dplyr::filter(traintest == 1)
antetest <- readRDS('results_data/long_exante.rds') %>% dplyr::filter(traintest == 1)

# train SARIMAX model
tic("Train Time")
fit <- train %>% model(
  TSLM(sqrt(texts) ~ localtime + weekday + daily_temp + clouds + covid_deaths + covid_announcements + log(tweet_avg+0.1) + log(retweet_avg+0.1) + log(likes_avg+0.1))
)
toc()
#print(SARIMAXfit %>% pivot_longer(everything(), names_to = "Model name", values_to = "Orders"))
#fit %>% report()

# save trained models
save(fit,file='results_data/long_ext_LM_fit')
#load("results_data/long_ext_ARIMA_fit")

# make EXPOST predictions
predictions <- make_prediction(test,fit)

# save EXPOST predictions
saveRDS(predictions, 'results_data/long_expost_LM_preds.rds')

# make EXANTE predictions
predictions <- make_prediction(antetest,fit)

# save EXANTE predictions
saveRDS(predictions, 'results_data/long_exante_LM_preds.rds')

# make TRAINING predictions
predictions <- make_prediction(train,fit)

# save TRAINING predictions
saveRDS(predictions, 'results_data/long_ext_LM_train_preds.rds')

# remove data, models, and predictions
rm(train)
rm(test)
rm(fit)
rm(predictions)






############# Linear Mixed Model ##############
library(lme4)
str(train)
model <- lmer(sqrt(texts) ~ (1|localtime) + (1|weekday) + daily_temp + clouds + covid_deaths + covid_announcements + log(tweet_avg+0.1) + log(retweet_avg+0.1) + log(likes_avg+0.1), data=train)
summary(model)
preds <- predict(model,test)
preds <- cbind(test,preds) %>% transmute(chunk, truth=texts, preds = preds^2)
head(preds)

error_metrics <- function(truth, preds) {
  library(Metrics)
  mae <- Metrics::mae(truth,preds)
  my_mae <- mean(abs(preds-truth))
  rmse <- Metrics::rmse(truth,preds)
  my_rmse <- sqrt(mean((preds-truth)^2))
  me <- sum(preds-truth)/length(truth)
  sdae <- sd(abs(preds-truth))
  out <- cbind(mae,my_mae,rmse,my_rmse,me, sdae)
  return(out)
}

error_metrics(preds$truth, preds$preds)
error_metrics(predictions$truth, predictions$preds)
