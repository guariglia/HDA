# RESULTS : Long Naive Model Prediction

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
train <- readRDS('results_data/long_expost.rds') %>% dplyr::filter(traintest == 0)
test <- readRDS('results_data/long_expost.rds') %>% dplyr::filter(traintest == 1)

# build naive model
n_rows <- dim(train)[1]
fit <- train[(n_rows-(23)):n_rows,1:3] %>% as.data.frame %>% transmute(localtime, preds = texts)
dim(fit)


# make predictions
dim(test[,1:3])
test_truth <- test[,1:3] %>% as.data.frame %>% transmute(localtime, chunk, truth = texts)
predictions <- merge(fit, test_truth, by="localtime", all.y=TRUE)
predictions <- as_tsibble(predictions, index=chunk) %>% transmute(chunk, preds, truth)

# save predictions
saveRDS(predictions, 'results_data/long_naive_preds.rds')

# make TRAIN predictions
dim(train[,1:3])
test_truth <- train[,1:3] %>% as.data.frame %>% transmute(localtime, chunk, truth = texts)
predictions <- merge(fit, test_truth, by="localtime", all.y=TRUE)
predictions <- as_tsibble(predictions, index=chunk) %>% transmute(chunk, preds, truth)

# save TRAIN predictions
saveRDS(predictions, 'results_data/long_naive_train_preds.rds')

# remove data, models, and predictions
rm(train)
rm(test)
rm(fit)
rm(predictions)