# RESULTS : SVR Validation Curves


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

######### IMPORT TIME ONLY DATA ###########

cv <- read.csv('results_data/11_long_SVR_timeonly_cross_val.csv') %>% dplyr::select(param_C, param_epsilon, param_gamma, mean_test_score, rank_test_score)
str(cv)
cv

# Top score
cv %>% dplyr::filter(rank_test_score==1)

# C
C <- cv %>% group_by(param_C) %>% summarise(R2.Score = mean(mean_test_score)) 
ggplot(data=C,aes(x=param_C,y=R2.Score)) + geom_line(col='purple') + 
  theme_bw() + scale_x_continuous(trans='log10') +
  labs(y="R2 Score", x="Parameter C")

C <- cv %>% dplyr::select(-rank_test_score) %>% mutate(Gamma=factor(paste(param_gamma))) %>% 
  melt(id.vars=c('param_C','Gamma'), measure.vars="mean_test_score",variable.name="variable") %>% 
  group_by(param_C, Gamma) %>% summarise(R2.Score = mean(value)) 
ggplot(data=C,aes(x=param_C,y=R2.Score,col=Gamma)) + geom_line() + 
  theme_bw() + scale_x_continuous(trans='log10') +
  labs(y="R2 Score", x="Parameter C") + scale_color_brewer(palette="Set2") +
  ggtitle("Tuning of gamma and C parameters")

# Gamma
gamma <- cv %>% group_by(param_gamma) %>% summarise(R2.Score = mean(mean_test_score))
ggplot(data=gamma,aes(x=param_gamma,y=R2.Score)) + geom_line(col='purple') + 
  theme_bw() + scale_x_continuous(trans='log10') +
  labs(y="R2 Score", x="Parameter Gamma")

# Epsilon
epsilon <- cv %>% group_by(param_epsilon) %>% summarise(R2.Score = mean(mean_test_score))
ggplot(data=epsilon,aes(x=param_epsilon,y=R2.Score)) + geom_line(col='purple') + 
  theme_bw() + scale_x_continuous(trans='log10') +
  labs(y="R2 Score", x="Parameter Epsilon") + ggtitle("Tuning of epsilon parameter")







######### IMPORT EXT DATA ###########

cv <- read.csv('results_data/11_long_SVR_ext_cross_val.csv') %>% dplyr::select(param_C, param_epsilon, param_gamma, mean_test_score, rank_test_score)
str(cv)
cv

# Top score
cv %>% dplyr::filter(rank_test_score==1)

# C
C <- cv %>% group_by(param_C) %>% summarise(R2.Score = mean(mean_test_score))
ggplot(data=C,aes(x=param_C,y=R2.Score)) + geom_line(col='purple') + 
  theme_bw() + scale_x_continuous(trans='log10') +
  labs(y="R2 Score", x="Parameter C")

C <- cv %>% dplyr::select(-rank_test_score) %>% mutate(Gamma=factor(paste(param_gamma))) %>% 
  melt(id.vars=c('param_C','Gamma'), measure.vars="mean_test_score",variable.name="variable") %>% 
  group_by(param_C, Gamma) %>% summarise(R2.Score = mean(value)) 
ggplot(data=C,aes(x=param_C,y=R2.Score,col=Gamma)) + geom_line() + 
  theme_bw() + scale_x_continuous(trans='log10') +
  labs(y="R2 Score", x="Parameter C") + scale_color_brewer(palette="Set2") +
  ggtitle("Tuning of gamma and C parameters")


# Gamma
gamma <- cv %>% group_by(param_gamma) %>% summarise(R2.Score = mean(mean_test_score))
ggplot(data=gamma,aes(x=param_gamma,y=R2.Score)) + geom_line(col='purple') + 
  theme_bw() + scale_x_continuous(trans='log10') +
  labs(y="R2 Score", x="Parameter Gamma")

# Epsilon
epsilon <- cv %>% group_by(param_epsilon) %>% summarise(R2.Score = mean(mean_test_score))
ggplot(data=epsilon,aes(x=param_epsilon,y=R2.Score)) + geom_line(col='purple') + 
  theme_bw() + scale_x_continuous(trans='log10') +
  labs(y="R2 Score", x="Parameter Epsilon") + ggtitle("Tuning of epsilon parameter")

