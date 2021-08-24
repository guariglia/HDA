# RESULTS : Short ARIMA Model Training and Prediction

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


loop <- 1:6

# Super Short Train and Test Set Predictions
for (i in loop) {
  # read in correct data: train and test
  eval(parse(text=paste0("train <- readRDS('results_data/super_short_",i,".rds') %>% dplyr::filter(traintest == 0)")))
  eval(parse(text=paste0("test <- readRDS('results_data/super_short_",i,".rds') %>% dplyr::filter(traintest == 1)")))
  
  # get lambda
  lambda <- train %>%
    features(texts, features = guerrero) %>%
    pull(lambda_guerrero)
  
  # train SARIMAX model
  tic("Train Time")
  fit <- train %>% model(
    ARIMA(box_cox(texts,lambda) ~ localtime + weekday + daily_temp + clouds + covid_deaths + covid_announcements + log(tweet_avg+0.1) + log(retweet_avg+0.1) + log(likes_avg+0.1))
  )
  toc()
  #print(SARIMAXfit %>% pivot_longer(everything(), names_to = "Model name", values_to = "Orders"))
  #fit %>% report()
  
  # save trained models
  eval(parse(text=paste0("save(fit,file='results_data/super_short_",i,"_ARIMA_fit')")))
  load("results_data/super_short_6_ARIMA_fit")
  fit %>% pivot_longer(everything(), names_to = "Model name",values_to = "Orders")
  
  # make predictions
  predictions <- make_prediction(test,fit)
  predictions <- predictions[1:12,]
  
  # save predictions
  eval(parse(text=paste0("saveRDS(predictions, 'results_data/super_short_",i,"_ARIMA_preds.rds')")))

  # remove data, models, and predictions
  rm(train)
  rm(test)
  rm(fit)
  rm(predictions)
}


loop <- 1:6

# Short Train and Test Set Predictions
for (i in loop) {
  # read in correct data: train and test
  eval(parse(text=paste0("train <- readRDS('results_data/short_",i,".rds') %>% dplyr::filter(traintest == 0)")))
  eval(parse(text=paste0("test <- readRDS('results_data/short_",i,".rds') %>% dplyr::filter(traintest == 1)")))
  
  # get lambda
  lambda <- train %>%
    features(texts, features = guerrero) %>%
    pull(lambda_guerrero)
  
  # train SARIMAX model
  tic("Train Time")
  fit <- train %>% model(
    ARIMA(box_cox(texts,lambda) ~ localtime + weekday + daily_temp + clouds + covid_deaths + covid_announcements + log(tweet_avg+0.1) + log(retweet_avg+0.1) + log(likes_avg+0.1))
  )
  toc()
  #print(SARIMAXfit %>% pivot_longer(everything(), names_to = "Model name", values_to = "Orders"))
  #fit %>% report()
  
  # save trained models
  eval(parse(text=paste0("save(fit,file='results_data/short_",i,"_ARIMA_fit')")))
  #load("results_data/super_short_3_ARIMA_fit")
  
  # make predictions
  predictions <- make_prediction(test,fit)
  predictions <- predictions[1:12,]
  
  # save predictions
  eval(parse(text=paste0("saveRDS(predictions, 'results_data/short_",i,"_ARIMA_preds.rds')")))
  
  # remove data, models, and predictions
  rm(train)
  rm(test)
  rm(fit)
  rm(predictions)
}