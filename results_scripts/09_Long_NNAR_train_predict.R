# RESULTS : Long NNAR Model Training and Prediction
print("started")
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
  tic("Making Prediction")
  df <- forecast(fit, df, point_forecast = list(.mean = mean)) %>% merge(df,by="chunk") %>% dplyr::transmute(chunk,truth=texts.y,preds=.mean)
  toc()
  return(df)
}





# Train without External Predictors

# read in correct data: train and test
train <- readRDS('results_data/long_expost.rds') %>% dplyr::filter(traintest == 0)
test <- readRDS('results_data/long_expost.rds') %>% dplyr::filter(traintest == 1)
train_predset <- readRDS('results_data/long_expost_train.rds') %>% dplyr::filter(traintest == 1)

# train NNAR model
tic("Train Time") 
xreg <- train %>% dplyr::transmute(localtime, weekday)
fit <- train %>% model(
  NNAR = NNETAR(sqrt(texts), xreg=xreg)
)
toc()
#print(fit %>% pivot_longer(everything(), names_to = "Model name", values_to = "Orders"))
#fit %>% report()

# save trained models
save(fit,file='results_data/long_timeonly_NNAR_fit')
#load("results_data/long_timeonly_NNAR_fit")

# make predictions
predictions <- make_prediction(test,fit)

# save predictions
saveRDS(predictions, 'results_data/long_timeonly_NNAR_preds.rds')

# make TRAINING predictions
predictions <- make_prediction(train_predset,fit)

# save TRAINING predictions
saveRDS(predictions, 'results_data/long_timeonly_NNAR_train_preds.rds')

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
train_predset <- readRDS('results_data/long_expost_train.rds') %>% dplyr::filter(traintest == 1)

# train NNAR model
tic("Train Time") 
xreg <- train %>% dplyr::transmute(localtime, weekday, daily_temp, clouds, covid_deaths, covid_announcements, log(tweet_avg+0.1), log(retweet_avg+0.1), log(likes_avg+0.1))
fit <- train %>% model(
  NNAR = NNETAR(sqrt(texts), xreg=xreg)
)
toc()
#print(fit %>% pivot_longer(everything(), names_to = "Model name", values_to = "Orders"))
#fit %>% report()

# save trained models
save(fit,file='results_data/long_ext_NNAR_fit')
#load("results_data/long_ext_NNAR_fit")

# make EXPOST predictions
predictions <- make_prediction(test,fit)

# save EXPOST predictions
saveRDS(predictions, 'results_data/long_expost_NNAR_preds.rds')

# make EXANTE predictions
predictions <- make_prediction(antetest,fit)

# save EXANTE predictions
saveRDS(predictions, 'results_data/long_exante_NNAR_preds.rds')

# make TRAINING predictions
predictions <- make_prediction(train_predset,fit)

# save TRAINING predictions
saveRDS(predictions, 'results_data/long_ext_NNAR_train_preds.rds')

# remove data, models, and predictions
rm(train)
rm(test)
rm(fit)
rm(predictions)

