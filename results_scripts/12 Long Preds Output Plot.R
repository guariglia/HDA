# RESULTS : Long Predictions Output Stats and Plots

# Combine them into one dataframe
  # Time only
  # Expost
  # Exante

  # Time only training performance
  # External Training performance


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
library(RColorBrewer)
#display.brewer.all(colorblindFriendly = T)

########## FUNCTIONS ##########
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

######### IMPORT TIME ONLY DATA ###########

loop <- 0:5

preds0 <- readRDS('results_data/long_naive_preds.rds')
preds1 <- readRDS('results_data/long_timeonly_LM_preds.rds')
preds2 <- readRDS('results_data/long_timeonly_ARIMA_preds.rds')
preds3 <- readRDS('results_data/long_timeonly_NNAR_preds.rds')
preds4 <- read.csv('results_data/long_timeonly_SVR_preds.csv') %>% mutate(chunk = as.POSIXct(chunk))
preds5 <- read.csv('results_data/long_timeonly_RNN_preds.csv')
date <- read.csv('results_data/long_expost.csv') %>% dplyr::filter(traintest == 1) %>% dplyr::select(chunk, texts)
preds5 <- cbind(date[1:(24*50),], preds5) %>% mutate(chunk = as.POSIXct(chunk))

########### OUTPUTS TIME ONLY DATA ##################

# (1) Combine predictions dataframes into one

for (i in loop){
  if (i == 0 ) {
    timeonly <- preds0 %>% dplyr::transmute(chunk, truth, preds0=preds)
  } else {
    eval(parse(text=paste0("preds <- preds",i," %>% dplyr::transmute(chunk, preds",i,"=preds)")))
    timeonly <- merge(timeonly,preds, by="chunk")
  }
}
timeonly <- timeonly[1:(24*50),] # Take first 50 days only
head(timeonly)

# (2) Calculate Error Metrics
for (i in loop){
  if (i == 0 ) {
    timeonly_errors <- error_metrics(timeonly$truth, timeonly$preds0)
  } else {
    eval(parse(text=paste0("errors <- error_metrics(timeonly$truth, timeonly$preds",i,")")))
    timeonly_errors <- rbind(timeonly_errors,errors)
  }
}
dim(timeonly_errors)
rownames(timeonly_errors) <- c("Naive","LM","ARIMA","NNAR","SVR","RNN")
timeonly_errors

# (3) Produce a Forecast Plot at different horizons with Truth in bold


# Full length plot
alpha<-0.7
fulllength<- timeonly
ggplot(data = fulllength, aes(chunk,truth)) + geom_line(alpha=0) + theme_bw() + ylab('Text Volume') + xlab('Time') + scale_color_brewer(palette="Set2") +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds0, col = "Naive")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds1, col = "LM")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds2, col = "ARIMA")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds3, col = "AR NN")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds4, col = "SVR")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds5, col = "RNN")) +
  geom_line(data = fulllength, alpha=1, aes(chunk, truth)) +
  ggtitle("Full test set forecasts")
ggsave('results_output/long_1timeonly_fullforecast.png',plot=last_plot(),width=8,height=3)

# Only last week plot
alpha<-0.7
lastweek <- timeonly[(dim(timeonly)[1]-(24*7)):dim(timeonly)[1],]
ggplot(data = lastweek, aes(chunk,truth)) + geom_line() + theme_bw() + ylab('Text Volume') + xlab('Time') + scale_color_brewer(palette="Set2") +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds0, col = "Naive")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds1, col = "LM")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds2, col = "ARIMA")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds3, col = "AR NN")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds4, col = "SVR")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds5, col = "RNN")) +
  ggtitle("Final week of test set forecasts")
ggsave('results_output/long_1timeonly_finalweekforecast.png',plot=last_plot(),width=9,height=5)

# (4) Plot by hour of day
timeonly <- timeonly %>% mutate(hour = as.numeric(substr(chunk, 12,13)),
                                ae0 = abs(preds0-truth),
                                ae1 = abs(preds1-truth),
                                ae2 = abs(preds2-truth),
                                ae3 = abs(preds3-truth),
                                ae4 = abs(preds4-truth),
                                ae5 = abs(preds5-truth)
                                )
ggplot(timeonly, aes(x=ae1)) + geom_density()
  # , colour=cond ## If you melt by model, you can get overlaid density plots with different colours

data_length <- dim(timeonly)[1]
byhour <- timeonly %>% group_by(hour) %>% summarise(mae0 = mean(ae0),
                                                    ci0 = 1.96*sd(ae0)/sqrt(data_length),
                                                    mae1 = mean(ae1),
                                                    ci1 = 1.96*sd(ae1)/sqrt(data_length),
                                                    mae2 = mean(ae2),
                                                    ci2 = 1.96*sd(ae2)/sqrt(data_length),
                                                    mae3 = mean(ae3),
                                                    ci3 = 1.96*sd(ae3)/sqrt(data_length),
                                                    mae4 = mean(ae4),
                                                    ci4 = 1.96*sd(ae4)/sqrt(data_length),
                                                    mae5 = mean(ae5),
                                                    ci5 = 1.96*sd(ae5)/sqrt(data_length),
                                                    )
byhour


ggplot(byhour,aes(hour, mae4)) + theme_bw() + scale_color_brewer(palette="Set2") +
  geom_line(aes(col="SVR")) + geom_ribbon(aes(ymin = mae4-ci4, ymax = mae4+ci4), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae0, col="Naive")) + geom_ribbon(aes(ymin = mae0-ci0, ymax = mae0+ci0), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae1, col="LM")) + geom_ribbon(aes(ymin = mae1-ci1, ymax = mae1+ci1), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae2, col="ARIMA")) + geom_ribbon(aes(ymin = mae2-ci2, ymax = mae2+ci2), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae3, col="AR NN")) + geom_ribbon(aes(ymin = mae3-ci3, ymax = mae3+ci3), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae4, col="SVR")) + geom_ribbon(aes(ymin = mae4-ci4, ymax = mae4+ci4), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae5, col="RNN")) + geom_ribbon(aes(ymin = mae5-ci5, ymax = mae5+ci5), alpha = 0.1) +
  ylab("Mean Absolute Error") + xlab("Hour of Day") + ggtitle("MAE by Hour of Day")

# (5)Horizon Plots
ten   <-timeonly[1:(dim(timeonly)[1]*10/50),] %>% mutate(horizon=10)
twenty<-timeonly[(dim(timeonly)[1]*10/50+1):(dim(timeonly)[1]*20/50),] %>% mutate(horizon=20)
thirty<-timeonly[(dim(timeonly)[1]*20/50+1):(dim(timeonly)[1]*30/50),] %>% mutate(horizon=30)
fourty<-timeonly[(dim(timeonly)[1]*30/50+1):(dim(timeonly)[1]*40/50),] %>% mutate(horizon=40)
fifty <-timeonly[(dim(timeonly)[1]*40/50+1):(dim(timeonly)[1]*50/50),] %>% mutate(horizon=50)
timeonly<- rbind(ten,twenty,thirty,fourty,fifty)

data_length <- dim(timeonly)[1]
byhorizon <- timeonly %>% group_by(horizon) %>% summarise(mae0 = mean(ae0),
                                                    ci0 = 1.96*sd(ae0)/sqrt(data_length),
                                                    mae1 = mean(ae1),
                                                    ci1 = 1.96*sd(ae1)/sqrt(data_length),
                                                    mae2 = mean(ae2),
                                                    ci2 = 1.96*sd(ae2)/sqrt(data_length),
                                                    mae3 = mean(ae3),
                                                    ci3 = 1.96*sd(ae3)/sqrt(data_length),
                                                    mae4 = mean(ae4),
                                                    ci4 = 1.96*sd(ae4)/sqrt(data_length),
                                                    mae5 = mean(ae5),
                                                    ci5 = 1.96*sd(ae5)/sqrt(data_length),
)

ggplot(byhorizon,aes(x=horizon, y=mae4)) + theme_bw() + scale_color_brewer(palette="Set2") +
  geom_line(aes(col="SVR")) + geom_ribbon(aes(ymin = mae4-ci4, ymax = mae4+ci4), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae0, col="Naive")) + geom_ribbon(aes(ymin = mae0-ci0, ymax = mae0+ci0), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae1, col="LM")) + geom_ribbon(aes(ymin = mae1-ci1, ymax = mae1+ci1), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae2, col="ARIMA")) + geom_ribbon(aes(ymin = mae2-ci2, ymax = mae2+ci2), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae3, col="AR NN")) + geom_ribbon(aes(ymin = mae3-ci3, ymax = mae3+ci3), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae4, col="SVR")) + geom_ribbon(aes(ymin = mae4-ci4, ymax = mae4+ci4), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae5, col="RNN")) + geom_ribbon(aes(ymin = mae5-ci5, ymax = mae5+ci5), alpha = 0.1) +
  ylab("Mean Absolute Error") + xlab("Forecast Horizon") + ggtitle("MAE by Forecast Horizon")



######### IMPORT TRAINING TIME ONLY DATA ###########

loop <- 0:5 #1:5
preds0 <- readRDS('results_data/long_naive_train_preds.rds')
preds1 <- readRDS('results_data/long_timeonly_LM_train_preds.rds')
preds2 <- readRDS('results_data/long_timeonly_ARIMA_train_preds.rds')
preds3 <- readRDS('results_data/long_timeonly_NNAR_train_preds.rds')
preds4 <- read.csv('results_data/long_timeonly_SVR_train_preds.csv') %>% mutate(chunk = as.POSIXct(chunk))
preds5 <- read.csv('results_data/long_timeonly_RNN_train_preds.csv')
date <- read.csv('results_data/long_expost.csv') %>% dplyr::filter(traintest == 0) %>% dplyr::select(chunk, texts)
preds5 <- cbind(date[(dim(date)[1]-(24*50)+1):(dim(date)[1]),], preds5) %>% mutate(chunk = as.POSIXct(chunk))


########### OUTPUTS TIME ONLY DATA ##################

# (1) Combine predictions dataframes into one

for (i in loop){
  if (i == 0 ) {
    timeonly <- preds0 %>% dplyr::transmute(chunk, truth, preds0=preds)
  } else {
    eval(parse(text=paste0("preds <- preds",i," %>% dplyr::transmute(chunk, preds",i,"=preds)")))
    timeonly <- merge(timeonly,preds, by="chunk")
  }
}
timeonly <- timeonly[1:(24*50),] # Take first 50 days only
head(timeonly)

# (2) Calculate Error Metrics
for (i in loop){
  if (i == 0 ) {
    timeonly_errors <- error_metrics(timeonly$truth, timeonly$preds0)
  } else {
    eval(parse(text=paste0("errors <- error_metrics(timeonly$truth, timeonly$preds",i,")")))
    timeonly_errors <- rbind(timeonly_errors,errors)
  }
}
dim(timeonly_errors)
rownames(timeonly_errors) <- c("Naive","LM","ARIMA","AR NN","SVR","RNN")
timeonly_errors

# (3) Produce a Forecast Plot at different horizons with Truth in bold


# Full length plot
alpha<-0.7
fulllength<- timeonly
ggplot(data = fulllength, aes(chunk,truth)) + geom_line(alpha=0) + theme_bw() + ylab('Text Volume') + xlab('Time') + scale_color_brewer(palette="Set2") +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds1, col = "LM")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds2, col = "ARIMA")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds3, col = "NNAR")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds4, col = "SVR")) +
  geom_line(data = fulllength, alpha=1, aes(chunk, truth))
ggsave('results_output/long_1timeonly_fullforecast.png',plot=last_plot(),width=8,height=3)

# Only last week plot
alpha<-0.7
lastweek <- timeonly[(dim(timeonly[1])-(24*7)):dim(timeonly[1]),]
ggplot(data = lastweek, aes(chunk,truth)) + geom_line() + theme_bw() + ylab('Text Volume') + xlab('Time') + scale_color_brewer(palette="Set2") +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds1, col = "LM")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds2, col = "ARIMA")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds3, col = "NNAR")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds4, col = "SVR"))
ggsave('results_output/long_1timeonly_finalweekforecast.png',plot=last_plot(),width=5,height=3)







######### IMPORT EXPOST DATA ###########

loop <- 0:4
preds0 <- readRDS('results_data/long_naive_preds.rds')
preds1 <- readRDS('results_data/long_expost_LM_preds.rds')
preds2 <- readRDS('results_data/long_expost_ARIMA_preds.rds')
preds3 <- readRDS('results_data/long_expost_NNAR_preds.rds')
preds4 <- read.csv('results_data/long_expost_SVR_preds.csv') %>% mutate(chunk = as.POSIXct(chunk))
# RNN

########### OUTPUTS EXPOST DATA ##################

# (1) Combine predictions dataframes into one

for (i in loop){
  if (i == 0 ) {
    timeonly <- preds0 %>% dplyr::transmute(chunk, truth, preds0=preds)
  } else {
    eval(parse(text=paste0("preds <- preds",i," %>% dplyr::transmute(chunk, preds",i,"=preds)")))
    timeonly <- merge(timeonly,preds, by="chunk")
  }
}
timeonly <- timeonly[1:(24*50),] # Take first 50 days only
head(timeonly)

# (2) Calculate Error Metrics
for (i in loop){
  if (i == 0 ) {
    timeonly_errors <- error_metrics(timeonly$truth, timeonly$preds0)
  } else {
    eval(parse(text=paste0("errors <- error_metrics(timeonly$truth, timeonly$preds",i,")")))
    timeonly_errors <- rbind(timeonly_errors,errors)
  }
}
dim(timeonly_errors)
rownames(timeonly_errors) <- c("Naive","LM","ARIMA","NNAR","SVR")
timeonly_errors

# (3) Produce a Forecast Plot at different horizons with Truth in bold

# Full length plot
alpha<-0.7
fulllength<- timeonly
ggplot(data = fulllength, aes(chunk,truth)) + geom_line(alpha=0) + theme_bw() + ylab('Text Volume') + xlab('Time') + scale_color_brewer(palette="Set2") +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds0, col = "Naive")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds1, col = "LM")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds2, col = "ARIMA")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds3, col = "AR NN")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds4, col = "SVR")) +
  geom_line(data = fulllength, alpha=0, aes(chunk, preds4, col = "RNN")) +
  geom_line(data = fulllength, alpha=1, aes(chunk, truth)) +
  ggtitle("Full test set forecasts")
ggsave('results_output/long_2expost_fullforecast.png',plot=last_plot(),width=8,height=3)

# Only last week plot
alpha<-0.7
lastweek <- timeonly[(dim(timeonly)[1]-(24*7)):dim(timeonly)[1],]
ggplot(data = lastweek, aes(chunk,truth)) + geom_line() + theme_bw() + ylab('Text Volume') + xlab('Time') + scale_color_brewer(palette="Set2") +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds0, col = "Naive")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds1, col = "LM")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds2, col = "ARIMA")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds3, col = "AR NN")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds4, col = "SVR")) +
  geom_line(data = lastweek, alpha=0, aes(chunk, preds4, col = "RNN")) +
  ggtitle("Final week of test set forecasts")
ggsave('results_output/long_2expost_finalweekforecast.png',plot=last_plot(),width=9,height=5)

# (4) Plot by hour of day
timeonly <- timeonly %>% mutate(hour = as.numeric(substr(chunk, 12,13)),
                                ae0 = abs(preds0-truth),
                                ae1 = abs(preds1-truth),
                                ae2 = abs(preds2-truth),
                                ae3 = abs(preds3-truth),
                                ae4 = abs(preds4-truth)
)

data_length <- dim(timeonly)[1]
byhour <- timeonly %>% group_by(hour) %>% summarise(mae0 = mean(ae0),
                                                    ci0 = 1.96*sd(ae0)/sqrt(data_length),
                                                    mae1 = mean(ae1),
                                                    ci1 = 1.96*sd(ae1)/sqrt(data_length),
                                                    mae2 = mean(ae2),
                                                    ci2 = 1.96*sd(ae2)/sqrt(data_length),
                                                    mae3 = mean(ae3),
                                                    ci3 = 1.96*sd(ae3)/sqrt(data_length),
                                                    mae4 = mean(ae4),
                                                    ci4 = 1.96*sd(ae4)/sqrt(data_length)
)
byhour


ggplot(byhour,aes(hour, mae4)) + theme_bw() + scale_color_brewer(palette="Set2") +
  geom_line(aes(col="SVR")) + geom_ribbon(aes(ymin = mae4-ci4, ymax = mae4+ci4), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae0, col="Naive")) + geom_ribbon(aes(ymin = mae0-ci0, ymax = mae0+ci0), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae1, col="LM")) + geom_ribbon(aes(ymin = mae1-ci1, ymax = mae1+ci1), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae2, col="ARIMA")) + geom_ribbon(aes(ymin = mae2-ci2, ymax = mae2+ci2), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae3, col="AR NN")) + geom_ribbon(aes(ymin = mae3-ci3, ymax = mae3+ci3), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae4, col="SVR")) + geom_ribbon(aes(ymin = mae4-ci4, ymax = mae4+ci4), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae4, col="RNN"),alpha=0) + geom_ribbon(aes(ymin = mae4-ci4, ymax = mae4+ci4), alpha = 0) +
  ylab("Mean Absolute Error") + xlab("Hour of Day") + ggtitle("MAE by Hour of Day")

# (5)Horizon Plots
ten   <-timeonly[1:(dim(timeonly)[1]*10/50),] %>% mutate(horizon=10)
twenty<-timeonly[(dim(timeonly)[1]*10/50+1):(dim(timeonly)[1]*20/50),] %>% mutate(horizon=20)
thirty<-timeonly[(dim(timeonly)[1]*20/50+1):(dim(timeonly)[1]*30/50),] %>% mutate(horizon=30)
fourty<-timeonly[(dim(timeonly)[1]*30/50+1):(dim(timeonly)[1]*40/50),] %>% mutate(horizon=40)
fifty <-timeonly[(dim(timeonly)[1]*40/50+1):(dim(timeonly)[1]*50/50),] %>% mutate(horizon=50)
timeonly<- rbind(ten,twenty,thirty,fourty,fifty)

data_length <- dim(timeonly)[1]
byhorizon <- timeonly %>% group_by(horizon) %>% summarise(mae0 = mean(ae0),
                                                          ci0 = 1.96*sd(ae0)/sqrt(data_length),
                                                          mae1 = mean(ae1),
                                                          ci1 = 1.96*sd(ae1)/sqrt(data_length),
                                                          mae2 = mean(ae2),
                                                          ci2 = 1.96*sd(ae2)/sqrt(data_length),
                                                          mae3 = mean(ae3),
                                                          ci3 = 1.96*sd(ae3)/sqrt(data_length),
                                                          mae4 = mean(ae4),
                                                          ci4 = 1.96*sd(ae4)/sqrt(data_length)
)

ggplot(byhorizon,aes(x=horizon, y=mae4)) + theme_bw() + scale_color_brewer(palette="Set2") +
  geom_line(aes(col="SVR")) + geom_ribbon(aes(ymin = mae4-ci4, ymax = mae4+ci4), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae0, col="Naive")) + geom_ribbon(aes(ymin = mae0-ci0, ymax = mae0+ci0), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae1, col="LM")) + geom_ribbon(aes(ymin = mae1-ci1, ymax = mae1+ci1), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae2, col="ARIMA")) + geom_ribbon(aes(ymin = mae2-ci2, ymax = mae2+ci2), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae3, col="AR NN")) + geom_ribbon(aes(ymin = mae3-ci3, ymax = mae3+ci3), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae4, col="SVR")) + geom_ribbon(aes(ymin = mae4-ci4, ymax = mae4+ci4), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae4, col="RNN"),alpha=0) + geom_ribbon(aes(ymin = mae4-ci4, ymax = mae4+ci4), alpha = 0) +
  ylab("Mean Absolute Error") + xlab("Forecast Horizon") + ggtitle("MAE by Forecast Horizon")








######### IMPORT EXANTE DATA ###########

loop <- 0:5
preds0 <- readRDS('results_data/long_naive_preds.rds')
preds1 <- readRDS('results_data/long_exante_LM_preds.rds')
preds2 <- readRDS('results_data/long_exante_ARIMA_preds.rds')
preds3 <- readRDS('results_data/long_exante_NNAR_preds.rds')
preds4 <- read.csv('results_data/long_exante_SVR_preds.csv') %>% mutate(chunk = as.POSIXct(chunk))
preds5 <- read.csv('results_data/long_expost_RNN_preds.csv')
date <- read.csv('results_data/long_expost.csv') %>% dplyr::filter(traintest == 1) %>% dplyr::select(chunk, texts)
preds5 <- cbind(date[1:(24*50),], preds5) %>% mutate(chunk = as.POSIXct(chunk))


########### OUTPUTS TIME ONLY DATA ##################

# (1) Combine predictions dataframes into one

for (i in loop){
  if (i == 0 ) {
    timeonly <- preds0 %>% dplyr::transmute(chunk, truth, preds0=preds)
  } else {
    eval(parse(text=paste0("preds <- preds",i," %>% dplyr::transmute(chunk, preds",i,"=preds)")))
    timeonly <- merge(timeonly,preds, by="chunk")
  }
}
timeonly <- timeonly[1:(24*50),] # Take first 50 days only
head(timeonly)

# (2) Calculate Error Metrics
for (i in loop){
  if (i == 0 ) {
    timeonly_errors <- error_metrics(timeonly$truth, timeonly$preds0)
  } else {
    eval(parse(text=paste0("errors <- error_metrics(timeonly$truth, timeonly$preds",i,")")))
    timeonly_errors <- rbind(timeonly_errors,errors)
  }
}
dim(timeonly_errors)
rownames(timeonly_errors) <- c("Naive","LM","ARIMA","NNAR","SVR","RNN")
timeonly_errors

# (3) Produce a Forecast Plot at different horizons with Truth in bold

# Full length plot
alpha<-0.7
fulllength<- timeonly
ggplot(data = fulllength, aes(chunk,truth)) + geom_line(alpha=0) + theme_bw() + ylab('Text Volume') + xlab('Time') + scale_color_brewer(palette="Set2") +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds0, col = "Naive")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds1, col = "LM")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds2, col = "ARIMA")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds3, col = "AR NN")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds4, col = "SVR")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds5, col = "RNN")) +
  geom_line(data = fulllength, alpha=1, aes(chunk, truth)) +
  ggtitle("Full test set forecasts")
ggsave('results_output/long_3exante_fullforecast.png',plot=last_plot(),width=8,height=3)

# Only last week plot
alpha<-0.7
lastweek <- timeonly[(dim(timeonly)[1]-(24*7)):dim(timeonly)[1],]
ggplot(data = lastweek, aes(chunk,truth)) + geom_line() + theme_bw() + ylab('Text Volume') + xlab('Time') + scale_color_brewer(palette="Set2") +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds0, col = "Naive")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds1, col = "LM")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds2, col = "ARIMA")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds3, col = "AR NN")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds4, col = "SVR")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds5, col = "RNN")) +
  ggtitle("Final week of test set forecasts")
ggsave('results_output/long_3exante_finalweekforecast.png',plot=last_plot(),width=9,height=5)

# (4) Plot by hour of day
timeonly <- timeonly %>% mutate(hour = as.numeric(substr(chunk, 12,13)),
                                ae0 = abs(preds0-truth),
                                ae1 = abs(preds1-truth),
                                ae2 = abs(preds2-truth),
                                ae3 = abs(preds3-truth),
                                ae4 = abs(preds4-truth),
                                ae5 = abs(preds5-truth)
)

data_length <- dim(timeonly)[1]
byhour <- timeonly %>% group_by(hour) %>% summarise(mae0 = mean(ae0),
                                                    ci0 = 1.96*sd(ae0)/sqrt(data_length),
                                                    mae1 = mean(ae1),
                                                    ci1 = 1.96*sd(ae1)/sqrt(data_length),
                                                    mae2 = mean(ae2),
                                                    ci2 = 1.96*sd(ae2)/sqrt(data_length),
                                                    mae3 = mean(ae3),
                                                    ci3 = 1.96*sd(ae3)/sqrt(data_length),
                                                    mae4 = mean(ae4),
                                                    ci4 = 1.96*sd(ae4)/sqrt(data_length),
                                                    mae5 = mean(ae5),
                                                    ci5 = 1.96*sd(ae5)/sqrt(data_length),
)
byhour


ggplot(byhour,aes(hour, mae4)) + theme_bw() + scale_color_brewer(palette="Set2") +
  geom_line(aes(col="SVR")) + geom_ribbon(aes(ymin = mae4-ci4, ymax = mae4+ci4), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae0, col="Naive")) + geom_ribbon(aes(ymin = mae0-ci0, ymax = mae0+ci0), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae1, col="LM")) + geom_ribbon(aes(ymin = mae1-ci1, ymax = mae1+ci1), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae2, col="ARIMA")) + geom_ribbon(aes(ymin = mae2-ci2, ymax = mae2+ci2), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae3, col="AR NN")) + geom_ribbon(aes(ymin = mae3-ci3, ymax = mae3+ci3), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae4, col="SVR")) + geom_ribbon(aes(ymin = mae4-ci4, ymax = mae4+ci4), alpha = 0.1) +
  geom_line(data=byhour,aes(hour, mae5, col="RNN")) + geom_ribbon(aes(ymin = mae5-ci5, ymax = mae5+ci5), alpha = 0.1) +
  ylab("Mean Absolute Error") + xlab("Hour of Day") + ggtitle("MAE by Hour of Day")

# (5)Horizon Plots
ten   <-timeonly[1:(dim(timeonly)[1]*10/50),] %>% mutate(horizon=10)
twenty<-timeonly[(dim(timeonly)[1]*10/50+1):(dim(timeonly)[1]*20/50),] %>% mutate(horizon=20)
thirty<-timeonly[(dim(timeonly)[1]*20/50+1):(dim(timeonly)[1]*30/50),] %>% mutate(horizon=30)
fourty<-timeonly[(dim(timeonly)[1]*30/50+1):(dim(timeonly)[1]*40/50),] %>% mutate(horizon=40)
fifty <-timeonly[(dim(timeonly)[1]*40/50+1):(dim(timeonly)[1]*50/50),] %>% mutate(horizon=50)
timeonly<- rbind(ten,twenty,thirty,fourty,fifty)

data_length <- dim(timeonly)[1]
byhorizon <- timeonly %>% group_by(horizon) %>% summarise(mae0 = mean(ae0),
                                                          ci0 = 1.96*sd(ae0)/sqrt(data_length),
                                                          mae1 = mean(ae1),
                                                          ci1 = 1.96*sd(ae1)/sqrt(data_length),
                                                          mae2 = mean(ae2),
                                                          ci2 = 1.96*sd(ae2)/sqrt(data_length),
                                                          mae3 = mean(ae3),
                                                          ci3 = 1.96*sd(ae3)/sqrt(data_length),
                                                          mae4 = mean(ae4),
                                                          ci4 = 1.96*sd(ae4)/sqrt(data_length),
                                                          mae5 = mean(ae5),
                                                          ci5 = 1.96*sd(ae5)/sqrt(data_length),
)

ggplot(byhorizon,aes(x=horizon, y=mae4)) + theme_bw() + scale_color_brewer(palette="Set2") +
  geom_line(aes(col="SVR")) + geom_ribbon(aes(ymin = mae4-ci4, ymax = mae4+ci4), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae0, col="Naive")) + geom_ribbon(aes(ymin = mae0-ci0, ymax = mae0+ci0), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae1, col="LM")) + geom_ribbon(aes(ymin = mae1-ci1, ymax = mae1+ci1), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae2, col="ARIMA")) + geom_ribbon(aes(ymin = mae2-ci2, ymax = mae2+ci2), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae3, col="AR NN")) + geom_ribbon(aes(ymin = mae3-ci3, ymax = mae3+ci3), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae4, col="SVR")) + geom_ribbon(aes(ymin = mae4-ci4, ymax = mae4+ci4), alpha = 0.1) +
  geom_line(data=byhorizon,aes(x=horizon, y=mae5, col="RNN")) + geom_ribbon(aes(ymin = mae5-ci5, ymax = mae5+ci5), alpha = 0.1) +
  ylab("Mean Absolute Error") + xlab("Forecast Horizon") + ggtitle("MAE by Forecast Horizon")






######### IMPORT TRAINING EXT DATA ###########

loop <- 1:5
preds1 <- readRDS('results_data/long_ext_LM_train_preds.rds')
preds2 <- readRDS('results_data/long_ext_ARIMA_train_preds.rds')
preds3 <- readRDS('results_data/long_ext_NNAR_train_preds.rds')
preds4 <- read.csv('results_data/long_ext_SVR_train_preds.csv') %>% mutate(chunk = as.POSIXct(chunk))
preds5 <- read.csv('results_data/long_expost_RNN_train_preds.csv')
date <- read.csv('results_data/long_expost.csv') %>% dplyr::filter(traintest == 0) %>% dplyr::select(chunk, texts)
preds5 <- cbind(date[(dim(date)[1]-(24*50)+1):(dim(date)[1]),], preds5) %>% mutate(chunk = as.POSIXct(chunk))
########### OUTPUTS TRAINING EXT DATA ##################

# (1) Combine predictions dataframes into one

for (i in loop){
  if (i == 1 ) {
    timeonly <- preds1 %>% dplyr::transmute(chunk, truth, preds1=preds)
  } else {
    eval(parse(text=paste0("preds <- preds",i," %>% dplyr::transmute(chunk, preds",i,"=preds)")))
    timeonly <- merge(timeonly,preds, by="chunk")
  }
}
timeonly <- timeonly[1:(24*50),] # Take first 50 days only
head(timeonly)

# (2) Calculate Error Metrics
for (i in loop){
  if (i == 1 ) {
    timeonly_errors <- error_metrics(timeonly$truth, timeonly$preds1)
  } else {
    eval(parse(text=paste0("errors <- error_metrics(timeonly$truth, timeonly$preds",i,")")))
    timeonly_errors <- rbind(timeonly_errors,errors)
  }
}
dim(timeonly_errors)
rownames(timeonly_errors) <- c("LM","ARIMA","AR NN","SVR","RNN")
timeonly_errors

# (3) Produce a Forecast Plot at different horizons with Truth in bold


# Full length plot
alpha<-0.7
fulllength<- timeonly
ggplot(data = fulllength, aes(chunk,truth)) + geom_line(alpha=0) + theme_bw() + ylab('Text Volume') + xlab('Time') + scale_color_brewer(palette="Set2") +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds1, col = "LM")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds2, col = "ARIMA")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds3, col = "NNAR")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds4, col = "SVR")) +
  geom_line(data = fulllength, alpha=1, aes(chunk, truth))
ggsave('results_output/long_1timeonly_fullforecast.png',plot=last_plot(),width=8,height=3)

# Only last week plot
alpha<-0.7
lastweek <- timeonly[(dim(timeonly[1])-(24*7)):dim(timeonly[1]),]
ggplot(data = lastweek, aes(chunk,truth)) + geom_line() + theme_bw() + ylab('Text Volume') + xlab('Time') + scale_color_brewer(palette="Set2") +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds1, col = "LM")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds2, col = "ARIMA")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds3, col = "NNAR")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds4, col = "SVR"))
ggsave('results_output/long_1timeonly_finalweekforecast.png',plot=last_plot(),width=5,height=3)








######## SIMULATIONS #########

# loop <- 1:5
# SVR high and low predictions
high <- read.csv('results_data/long_simulation_high_SVR_preds.csv') %>% mutate(chunk = as.POSIXct(chunk))
low  <- read.csv('results_data/long_simulation_low_SVR_preds.csv') %>% mutate(chunk = as.POSIXct(chunk))

########### OUTPUTS TIME ONLY DATA ##################

# (1) Combine predictions dataframes into one
sims <- merge(high,low,by="chunk")
sims <- sims[1:(24*50),] # Take first 50 days only
head(sims)

# (3) Produce a Forecast Plot at different horizons with Truth in bold


# Full length plot
alpha<-0.7
fulllength<- sims
ggplot(data = fulllength, aes(chunk,preds.y)) + geom_line(alpha=0) + theme_bw() + ylab('Text Volume') + xlab('Time') + scale_color_brewer(palette="Set2",direction=-1) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds.x, col = "High")) +
  geom_line(data = fulllength, alpha=alpha, aes(chunk, preds.y, col = "Low")) +
  ggtitle("Full test set simulation forecasts") #+
  #geom_line(data = fulllength, alpha=1, aes(chunk, truth.x))
ggsave('results_output/long_simulation_SVR_fullforecast.png',plot=last_plot(),width=8,height=3)

# Only last week plot
alpha<-0.7
lastweek <- sims[(dim(sims[1])-(24*7)):dim(sims[1]),]
ggplot(data = lastweek, aes(chunk,preds.x)) + geom_line(alpha=0) + theme_bw() + ylab('Text Volume') + xlab('Time') + scale_color_brewer(palette="Set2",direction=-1) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds.x, col = "High")) +
  geom_line(data = lastweek, alpha=alpha, aes(chunk, preds.y, col = "Low")) +
  ggtitle("Final week of test set simulation forecasts")
ggsave('results_output/long_simulation_SVR_finalweekforecast.png',plot=last_plot(),width=9,height=5)

