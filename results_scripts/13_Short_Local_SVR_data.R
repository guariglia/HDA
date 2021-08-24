# RESULTS : Local SVR data preparation

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

#(1) Window the full dataset: 8 hrs input * all predictors + 3pm onwards 12 hours output of just texts
#(2) Split into train and test 
#(3) Create a function that allows you to do this easily with any train-test date
#(4) Save 6 datasets with training and test set instances
#(5) Make the training windows "local" (as well as the algorithm)

setwd("~/summerproj/data/")
ts <- as_tsibble(readRDS("030 full data.rds"), index = chunk) %>% dplyr::select(chunk, texts, localtime, weekday, temp, clouds, covid_deaths, covid_announcements, tweet_avg, retweet_avg, likes_avg) %>%
  # Transformations
  mutate(texts = sqrt(texts), weekday= as.numeric(as.factor(weekday)), tweet_avg = log(tweet_avg+0.1), retweet_avg = log(retweet_avg+0.1), likes_avg = log(likes_avg+0.1))
head(ts)
tail(ts)

pred_time <- "15:00:00"
input_window <- 8
seasonal_lags <- 4
seasonal_window <- 2
chosen_rows <- ts$localtime==pred_time
n_rows <- length(chosen_rows)
windows <- sum(chosen_rows)

#load("results_data/local_svr_data_cols")
matrix <- matrix(nrow=0,ncol=57)
#colnames(matrix) <- c("chunk",col_names)
matrix <- as.data.frame(matrix)
#summary(ts)
x<-0

tic("Time")# 66 sec
for (i in (input_window+seasonal_lags*24+1):n_rows) {
  if (chosen_rows[i]==TRUE) {
    print(i)#; i <- 16+seasonal_lags*24
    x<-x+1
    
    input <- ts[(i-input_window):(i-1),]
    names <- colnames(input %>% as.data.frame %>% dplyr::select(texts, temp, tweet_avg, retweet_avg, likes_avg))
    for (name in names) {
      eval(parse(text=paste0(name," <- input %>% as.data.frame %>% dplyr::select(localtime, ",name,") %>% mutate(localtime = paste0('in_",name,"_', substr(localtime,1,2)) ) %>% pivot_wider(names_from=localtime, values_from=",name,")")))
    }
    weekday <- ts[(i-1),'weekday'] %>% transmute(in_weekday=weekday) %>% as.data.frame()
    clouds <- ts[(i-1),'clouds'] %>% transmute(in_clouds=clouds) %>% as.data.frame()
    covid_deaths <- ts[(i-1),'covid_deaths'] %>% transmute(in_covid_deaths=covid_deaths) %>% as.data.frame()
    covid_announcements <- ts[(i-1),'covid_announcements'] %>% transmute(in_covid_announcements=covid_announcements) %>% as.data.frame()
    chunk <- ts[i,'chunk'] %>% as.data.frame() #%>% mutate(chunk = as.numeric(chunk))
    
    for (b in 1:seasonal_lags) {
      seasonal_input <- ts[(i-15-24*(b-1)-seasonal_window+1):(i-15-24*(b-1)),]
      names <- colnames(seasonal_input %>% as.data.frame %>% dplyr::select(texts, temp, tweet_avg,))
      for (name in names) {
        eval(parse(text=paste0("lag",b,"_",name," <- seasonal_input %>% as.data.frame %>% dplyr::select(localtime, ",name,") %>% mutate(localtime = paste0('in_lag",b,"_",name,"_', substr(localtime,1,2)) ) %>% pivot_wider(names_from=localtime, values_from=",name,")")))
      }
    }
    #lag_texts <- paste0("lag",(1:b),"_texts")
    lag_texts <- cbind(lag1_texts,lag2_texts,lag3_texts,lag4_texts
                       #,lag1_temp,lag2_temp,lag3_temp,lag4_temp
                       #,lag1_tweet_avg,lag2_tweet_avg,lag3_tweet_avg,lag4_tweet_avg
                       )
    
    localtime_vector <- c("15:00:00", "16:00:00", "17:00:00", "18:00:00", "19:00:00", "20:00:00", "21:00:00", "22:00:00", "23:00:00", "00:00:00", "01:00:00", "02:00:00")
    output <- ts[i:(i+11),] %>% as.data.frame %>% dplyr::select(texts) %>% cbind(localtime_vector) %>% transmute(texts,localtime = paste0("out_", substr(localtime_vector,1,2)) ) %>% pivot_wider(names_from=localtime, values_from=texts)
    
    the_row <- cbind(chunk, texts, lag_texts, temp, tweet_avg, retweet_avg, likes_avg, weekday, clouds, covid_deaths, covid_announcements, output)
    if (x == 1) {
      matrix <- the_row
    } else {
      colnames(the_row) <- colnames(matrix)
      matrix <- rbind(matrix,the_row)
    }
    
  }
}
toc()

matrix <- matrix[complete.cases(matrix),]

str(matrix)
head(matrix)
tail(matrix)

svr_traintest <- function(ts, trainstart, teststart, testend) {
  train <- ts %>% dplyr::filter(chunk >= trainstart & chunk < teststart)
  test <- ts %>% dplyr::filter(chunk >= teststart & chunk < testend)
  
  # Combine train and test sets
  train$traintest <- rep(0, dim(train)[1])
  test$traintest <- rep(1, dim(test)[1])
  output <- rbind(train, test)
  return(output)
}

super_short_1 <- svr_traintest(matrix, "2021-01-09", "2021-01-23 15:00:00", "2021-01-24 03:00:00")
super_short_2 <- svr_traintest(matrix, "2021-01-19", "2021-02-02 15:00:00", "2021-02-03 03:00:00") 
super_short_3 <- svr_traintest(matrix, "2021-03-14", "2021-03-28 15:00:00", "2021-03-29 03:00:00")
super_short_4 <- svr_traintest(matrix, "2020-12-21", "2021-01-04 15:00:00", "2021-01-05 03:00:00") 
super_short_5 <- svr_traintest(matrix, "2021-03-10", "2021-03-24 15:00:00", "2021-03-25 03:00:00")
super_short_6 <- svr_traintest(matrix, "2021-03-28", "2021-04-12 15:00:00", "2021-04-13 03:00:00") 

setwd("~/summerproj/results_data/")
loop <- 1:6
for (i in loop){
  eval(parse(text=paste0("write.csv(super_short_",i,",'lags_super_short_windowed",i,".csv')")))
}

# 1 year training set will be far too long
short_1 <- svr_traintest(matrix, "2020-03-01", "2021-01-23 15:00:00", "2021-01-24 03:00:00") 
short_2 <- svr_traintest(matrix, "2020-03-01", "2021-02-02 15:00:00", "2021-02-03 03:00:00") 
short_3 <- svr_traintest(matrix, "2020-03-01", "2021-03-28 15:00:00", "2021-03-29 03:00:00") 
short_4 <- svr_traintest(matrix, "2020-03-01", "2021-01-04 15:00:00", "2021-01-05 03:00:00")
short_5 <- svr_traintest(matrix, "2020-03-01", "2021-03-24 15:00:00", "2021-03-25 03:00:00")
short_6 <- svr_traintest(matrix, "2020-03-01", "2021-04-12 15:00:00", "2021-04-13 03:00:00") 

# 1 month might work
mo_short_1 <- svr_traintest(matrix, "2020-12-23", "2021-01-23 15:00:00", "2021-01-24 03:00:00")
mo_short_2 <- svr_traintest(matrix, "2021-01-02", "2021-02-02 15:00:00", "2021-02-03 03:00:00") 
mo_short_3 <- svr_traintest(matrix, "2021-02-28", "2021-03-28 15:00:00", "2021-03-29 03:00:00")
mo_short_4 <- svr_traintest(matrix, "2020-12-04", "2021-01-04 15:00:00", "2021-01-05 03:00:00") 
mo_short_5 <- svr_traintest(matrix, "2021-02-24", "2021-03-24 15:00:00", "2021-03-25 03:00:00")
mo_short_6 <- svr_traintest(matrix, "2021-03-12", "2021-04-12 15:00:00", "2021-04-13 03:00:00")

setwd("~/summerproj/results_data/")
loop <- 1:6
for (i in loop){
  eval(parse(text=paste0("write.csv(mo_short_",i,",'lags_mo_short_windowed",i,".csv')")))
}

str(super_short_1)    
str(short_1)
