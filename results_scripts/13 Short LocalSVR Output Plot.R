# RESULTS : Short LocalSVR Model Output Stats and Plots

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

error_metrics <- function(df) {
  library(Metrics)
  mae <- Metrics::mae(df$truth,df$preds)
  rmse <- Metrics::rmse(df$truth,df$preds)
  me <- sum(df$preds-df$truth)/(dim(df)[1])
  sdae <- sd(abs(df$preds-df$truth))
  peakper <- 100*max(df$preds)/max(df$truth)
  out <- cbind(mae,rmse,me, sdae, peakper)
  return(out)
}


######### IMPORT DATA ###########

# Super Short Train, Short Test Set Predictions
loop <- 1:6
for (i in loop) {
  # read in correct data: train and test
  eval(parse(text=paste0("preds",i," <- read.csv('results_data/super_short_lag_",i,"_Local_SVR_preds.csv')")))
  eval(parse(text=paste0("preds",i," <- preds",i," %>% transmute(X, truth=Truth,preds=Preds)")))
}

for (i in loop) {
  eval(parse(text=paste0("chunk <- (readRDS('results_data/short_",i,".rds') %>% dplyr::filter(traintest ==1))[1:12,1]")))
  eval(parse(text=paste0("preds",i," <- cbind(preds",i,", chunk)")))
}

########### PREDICTIONS ##################
for (i in loop) {
  eval(parse(text=paste0("preds <- preds",i)))
  preds <- preds[1:12,]
  errors <- error_metrics(preds)
  print(errors)
}

for (i in loop){
  eval(parse(text=paste0("preds <- preds",i)))
  
  if (i == 3 | i == 6) {
    preds <- preds %>% mutate(localtime = as.POSIXct(chunk)+60*60)
  } else {
    preds <- preds %>% mutate(localtime = as.POSIXct(chunk))
  }
  errors <- error_metrics(preds)
  # MAE = errors[1]
  plot <- ggplot(preds, aes(x=localtime, y=truth, col="Truth")) + geom_line(alpha = 1) +
    geom_line(data=preds, aes(x=localtime, y=preds, col = "Preds")) +
    theme_bw() + theme(legend.position = c(.2, .9)) + scale_color_brewer(palette="Set2") +
    ylab("Texts") + xlab("Hour") +
    ylim(30,350) #+ annotate("text", x=16, y = 200, label = "italic(MAE) == whatever",parse = TRUE)
    # + Choose colour blind colour scale!
  eval(parse(text=paste0("ggsave(filename = 'results_output/super_short_lag_",i,"_LocalSVR_preds_plot.png', plot = plot, width = 3, height = 5)")))
  print(errors)
}
