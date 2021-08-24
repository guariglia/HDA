# RESULTS : Short NNAR Model Training and Prediction

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
print("starting super short predictions - testing new approach!!! seeing if it comes out better")
for (i in loop) {
  # read in correct data: train and test
  eval(parse(text=paste0("train <- readRDS('results_data/super_short_",i,".rds') %>% dplyr::filter(traintest == 0)")))
  eval(parse(text=paste0("test <- readRDS('results_data/super_short_",i,".rds') %>% dplyr::filter(traintest == 1)")))
  
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
  eval(parse(text=paste0("save(fit,file='results_data/super_short_",i,"_NNAR_fit')")))
  #load("results_data/super_short_1_NNAR_fit")
  
  # make predictions
  tic("Making Predictions")
  #predictions <- make_prediction(test,fit)
  #predictions <- predictions[1:12,]
  predictions <- forecast(fit, test[1:12,], point_forecast = list(.mean = mean)) 
  toc()
  predictions <- predictions %>% merge(test[1:12,],by="chunk")
  saveRDS(predictions, 'results_data/super_short_i_NNAR_preds.rds')
  predictions <- predictions %>% dplyr::transmute(chunk,truth=texts.y,preds=.mean)
  
  # save predictions
  eval(parse(text=paste0("saveRDS(predictions, 'results_data/super_short_",i,"_NNAR_preds.rds')")))
  
  # remove data, models, and predictions
  rm(train)
  rm(test)
  rm(fit)
  rm(predictions)
}





# Short Train and Test Set Predictions
print("starting short predictions")
for (i in loop) {
  # read in correct data: train and test
  eval(parse(text=paste0("train <- readRDS('results_data/short_",i,".rds') %>% dplyr::filter(traintest == 0)")))
  eval(parse(text=paste0("test <- readRDS('results_data/short_",i,".rds') %>% dplyr::filter(traintest == 1)")))
  
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
  eval(parse(text=paste0("save(fit,file='results_data/short_",i,"_NNAR_fit')")))
  #load("results_data/super_short_1_NNAR_fit")
  
  # make predictions
  tic("Making Predictions")
  predictions <- make_prediction(test[1:12,],fit)
  #predictions <- predictions[1:12,]
  toc()
  
  # save predictions
  eval(parse(text=paste0("saveRDS(predictions, 'results_data/short_",i,"_NNAR_preds.rds')")))
  
  # remove data, models, and predictions
  rm(train)
  rm(test)
  rm(fit)
  rm(predictions)
}

