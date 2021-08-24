# RESULTS : Data Wrangling - Train Test Function

# 1 Get Set Up
rm(list=ls())
setwd("~/summerproj/results_data/")
#install.packages("Metrics")
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

#################### DEFINE THE FUNCTION #################### 

## Inputs to function:
# "year-month-day hr:mi:se"
# 'expost' or 'exante'

tstraintest <- function(ts, trainstart, teststart, testend, ex) {
  train <- ts %>% dplyr::filter(chunk >= trainstart & chunk < teststart)
  test <- ts %>% dplyr::filter(chunk >= teststart & chunk < testend)
  if (ex == "expost") {
    print("expost set complete")
  } else if (ex == "exante") {
    # Get average twitter and covid values from last 24 hrs
    twitter_covid <- train[(dim(train)[1]-24):dim(train)[1],] %>% as.data.frame %>% # last 24 hrs
      mutate(year = floor_date(chunk, "year")) %>%
      group_by(year) %>% summarise(tweet_avg = mean(tweets),
                                   retweet_avg = mean(retweets),
                                   likes_avg = mean(likes),
                                   covid_cases = mean(covid_cases),
                                   covid_deaths = mean(covid_deaths),
                                   covid_announcements = mean(covid_announcements))
    # Get average weather from full history prior to test set
    weather <- read_csv("openweather_2010-_2021_snarestone_uk.csv") %>% dplyr::select(dt,dt_iso, temp, clouds_all, rain_1h)
    weather$chunk <- as.POSIXct(weather$dt, tz = "GMT", origin="1970-01-01")
    weather <- weather %>% transmute(chunk, temp, clouds = clouds_all, rainfall = rain_1h) %>% dplyr::filter(chunk<teststart)
    weather$rainfall <- ifelse(is.na(weather$rainfall),0,weather$rainfall)
    weather <- weather %>% mutate(daymonthhour = paste(substr(floor_date(chunk, "days"),9,10),substr(floor_date(chunk, "months"),6,7),substr(chunk,12,13) ))
     
    hourly <- weather %>% group_by(daymonthhour) %>% summarise(temp = mean(temp),
                                                               clouds = mean(clouds),
                                                               rainfall = mean(rainfall),)
    daily_temp <- weather %>% mutate(day = paste(substr(floor_date(chunk, "days"),9,10),substr(floor_date(chunk, "months"),6,7)))
    daily_temp <- daily_temp %>% group_by(day) %>% summarise(daily_temp = mean(temp))
    rm(weather)
    
    # Merge into a test set
    base_test <- test %>% dplyr::select(chunk,texts,localtime,weekday,school_hols,exam_results_week,bank_holiday)
    rm(test)
    base_test <- base_test %>% mutate(daymonthhour = paste(substr(floor_date(chunk, "days"),9,10),substr(floor_date(chunk, "months"),6,7),substr(chunk,12,13) ))
    base_test <- merge(base_test,hourly, by="daymonthhour",all.x=TRUE) %>% dplyr::select(-daymonthhour)
    rm(hourly)
    base_test <- base_test %>% mutate(day = paste(substr(floor_date(chunk, "days"),9,10),substr(floor_date(chunk, "months"),6,7)))
    base_test <- merge(base_test,daily_temp, by="day",all.x=TRUE) %>% dplyr::select(-day)
    rm(daily_temp)
    base_test <- base_test %>% mutate(year = floor_date(chunk, "year"))
    base_test <- merge(base_test,twitter_covid, by="year",all.x=TRUE) %>% dplyr::select(-year)
    rm(twitter_covid)
    test <- as_tsibble(base_test,index=chunk)
    rm(base_test)
    
    train <- train %>% dplyr::select(-c(tweets,retweets,likes))
    
    print("exante set complete")
  } else {
    print("ex command is invalid. Please select 'expost' or 'exante' as an input. Expost simply splits into train and test whereas exante only includes simple forecasts for exogenous regressors.")
  }
  train$traintest <- rep(0, dim(train)[1])
  test$traintest <- rep(1, dim(test)[1])
  output <- rbind(train, test)
  return(output)
}



#################### SHORT TERM TRAIN-TEST SETS #################### 
setwd("~/summerproj/data/")
ts <- as_tsibble(readRDS("030 full data.rds"), index = chunk)
# The test sets here are 2 weeks long just to make ARIMAX work
super_short_1 <- tstraintest(ts, "2021-01-09", "2021-01-23 15:00:00", "2021-01-30 03:00:00", "exante")
super_short_2 <- tstraintest(ts, "2021-01-19", "2021-02-02 15:00:00", "2021-02-09 03:00:00", "exante") 
super_short_3 <- tstraintest(ts, "2021-03-14", "2021-03-28 15:00:00", "2021-04-04 03:00:00", "exante")
super_short_4 <- tstraintest(ts, "2020-12-21", "2021-01-04 15:00:00", "2021-01-11 03:00:00", "exante") 
super_short_5 <- tstraintest(ts, "2021-03-10", "2021-03-24 15:00:00", "2021-03-31 03:00:00", "exante")
super_short_6 <- tstraintest(ts, "2021-03-28", "2021-04-12 15:00:00", "2021-04-19 03:00:00", "exante") 

short_1 <- tstraintest(ts, "2020-03-01", "2021-01-23 15:00:00", "2021-01-30 03:00:00", "exante") 
short_2 <- tstraintest(ts, "2020-03-01", "2021-02-02 15:00:00", "2021-02-09 03:00:00", "exante") 
short_3 <- tstraintest(ts, "2020-03-01", "2021-03-28 15:00:00", "2021-04-04 03:00:00", "exante") 
short_4 <- tstraintest(ts, "2020-03-01", "2021-01-04 15:00:00", "2021-01-11 03:00:00", "exante")
short_5 <- tstraintest(ts, "2020-03-01", "2021-03-24 15:00:00", "2021-03-31 03:00:00", "exante")
short_6 <- tstraintest(ts, "2020-03-01", "2021-04-12 15:00:00", "2021-04-19 03:00:00", "exante") 

mo_short_1 <- tstraintest(ts, "2020-12-23", "2021-01-23 15:00:00", "2021-01-30 03:00:00", "exante")
mo_short_2 <- tstraintest(ts, "2021-01-02", "2021-02-02 15:00:00", "2021-02-09 03:00:00", "exante") 
mo_short_3 <- tstraintest(ts, "2021-02-28", "2021-03-28 15:00:00", "2021-04-04 03:00:00", "exante")
mo_short_4 <- tstraintest(ts, "2020-12-04", "2021-01-04 15:00:00", "2021-01-11 03:00:00", "exante") 
mo_short_5 <- tstraintest(ts, "2021-02-24", "2021-03-24 15:00:00", "2021-03-31 03:00:00", "exante")
mo_short_6 <- tstraintest(ts, "2021-03-12", "2021-04-12 15:00:00", "2021-04-19 03:00:00", "exante")

#super_short <- super_short_6
#plot(super_short$chunk, super_short$texts, type='l')

# SAVE TRAIN AND TEST SETS
setwd("~/summerproj/results_data/")
loop <- 1:6
for (i in loop){
  eval(parse(text=paste0("write.csv(short_",i,",'short_",i,".csv')")))
}
for (i in loop){
  eval(parse(text=paste0("write.csv(super_short_",i,",'super_short_",i,".csv')")))
}
for (i in loop){
  eval(parse(text=paste0("saveRDS(short_",i,",'short_",i,".rds')")))
}
for (i in loop){
  eval(parse(text=paste0("saveRDS(super_short_",i,",'super_short_",i,".rds')")))
}


for (i in loop){
  eval(parse(text=paste0("write.csv(mo_short_",i,",'mo_short_",i,".csv')")))
}

#################### LONG TERM TRAIN-TEST SETS #################### 
setwd("~/summerproj/data/")
ts <- as_tsibble(readRDS("030 full data.rds"), index = chunk)
# Test set here starts after 16th March to avoid weird Twitter period
long_expost <- tstraintest(ts, "2020-03-01", "2021-03-17", "2021-05-13", "expost")
#ggplot(dplyr::filter(long_expost, traintest == 1), aes(chunk, sqrt(tweet_avg))) + geom_line()
long_exante <- tstraintest(ts, "2020-03-01", "2021-03-17", "2021-05-13", "exante")

long_expost_3motr <- tstraintest(ts, "2020-12-17", "2021-03-17", "2021-05-13", "expost")
long_exante_3motr <- tstraintest(ts, "2020-12-17", "2021-03-17", "2021-05-13", "exante")

# TRAINING SET AS TEST FOR SARIMA, ARNN, and RNN - test set needs to be length 50
long_expost_train <- tstraintest(ts, "2020-03-01", "2021-01-26", "2021-03-17", "expost")
long_exante_train <- tstraintest(ts, "2020-03-01", "2021-01-26", "2021-03-17", "exante")

# SAVE TRAIN AND TEST SETS
setwd("~/summerproj/results_data/")

write.csv(long_expost,'long_expost.csv')
saveRDS(long_expost,'long_expost.rds')

write.csv(long_exante,'long_exante.csv')
saveRDS(long_exante,'long_exante.rds')

write.csv(long_expost_3motr,'long_expost_3motr.csv')
saveRDS(long_expost_3motr,'long_expost_3motr.rds')

write.csv(long_exante_3motr,'long_exante_3motr.csv')
saveRDS(long_exante_3motr,'long_exante_3motr.rds')

# TRAINING
#write.csv(long_expost_train,'long_expost_train.csv')
saveRDS(long_expost_train,'long_expost_train.rds')

#write.csv(long_exante_train,'long_exante_train.csv')
saveRDS(long_exante_train,'long_exante_train.rds')


#################### LONG TERM SIMULATION TEST SETS #################### 
setwd("~/summerproj/data/")
ts <- as_tsibble(readRDS("030 full data.rds"), index = chunk)
long <- tstraintest(ts, "2020-03-01", "2021-03-17", "2021-05-13", "expost")

# High : 75th Quartile Twitter Activity; 25th Quartile Avg Temperature; 75th Quartile COVID Activity
long <- long %>% mutate(year = substr(chunk, 1,2))
str(long)
long_core <- long %>% dplyr::filter(traintest==1) %>% dplyr::select(chunk, texts, localtime, weekday, year)

high_values <-  long %>% as.data.frame %>% group_by(year) %>% 
                summarise(
                  tweet_avg = quantile(tweet_avg, prob=0.9),
                  retweet_avg = quantile(retweet_avg, prob=0.9),
                  likes_avg = quantile(likes, prob=0.9),
                  
                  covid_deaths = quantile(tweet_avg, prob=0.9),
                  covid_announcements = 0,
                  
                  daily_temp = quantile(daily_temp, prob=0.1),
                  clouds = mean(clouds)
                  
                  )
high <- merge(long_core, high_values, by='year') %>% dplyr::select(-year)
high <- high %>% mutate(covid_announcements = ifelse(chunk >= "2021-05-01" & chunk < "2021-05-02", 1, 0) )

low_values <-  long %>% as.data.frame %>% group_by(year) %>% 
  summarise(
    tweet_avg = quantile(tweet_avg, prob=0.1),
    retweet_avg = quantile(retweet_avg, prob=0.1),
    likes_avg = quantile(likes, prob=0.1),
    
    covid_deaths = quantile(tweet_avg, prob=0.1),
    covid_announcements = 0,
    
    daily_temp = quantile(daily_temp, prob=0.9),
    clouds = mean(clouds)
    
  )
low <- merge(long_core, low_values, by='year') %>% dplyr::select(-year)
str(low)
low <- low %>% mutate(covid_announcements = ifelse(chunk >= "2021-05-01" & chunk < "2021-05-02", 1, 0) )

# SAVE
setwd("~/summerproj/results_data/")
write.csv(high,'long_simulation_high.csv')
saveRDS(high,'long_simulation_high.rds')

write.csv(low,'long_simulation_low.csv')
saveRDS(low,'long_simulation_low.rds')

