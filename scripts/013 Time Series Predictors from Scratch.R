# This is an extra script in which I am trialling:
  # Adding date predictors into the time series model for the full dataset:
    # Time of day
    # Day of week
    # BST vs GMT
  # Removal of 5am peak
  # Linear modelling and plotting (nb. linear model not appropriate due to heteroscedasticity)

setwd("~/summerproj/data/")
library(dplyr)
library(lubridate)
library(chron)

# Full dataset import
chunkdf <- read.csv('000_shout_texts_10minchunk.csv') %>% mutate(chunk = as.POSIXct(chunk, format="%Y-%m-%d %H:%M:%S",tz="GMT"),texts = n) %>% dplyr::select(chunk,texts)
str(chunkdf)
head(chunkdf)
tail(chunkdf)
class(datetime$chunk)
class(chunkdf$chunk)

# Making a full set of timestamps from start to finish
dates <- seq(from = ymd('2018-05-13'), to = ymd('2021-05-13'), by='days')
hm <- merge(0:23, seq(0, 50, by = 10))
datetime <- merge(dates, chron(time = paste(hm$x, ':', hm$y, ':', 0)))
colnames(datetime) <- c('date', 'time')
datetime$dt <- as.POSIXct(paste(datetime$date, datetime$time),tz="GMT")
datetime <- as.data.frame(datetime[order(datetime$dt),'dt'])
row.names(datetime) <- NULL
colnames(datetime) <- c("chunk")
df <- merge(datetime, chunkdf, by=c("chunk"),all.x=TRUE) 
df[is.na(df)] <- 0
#plot(df$chunk,df$texts,type='l')
#summary(df)
#head(df)
#class(df$chunk)

# Adding predictors
df <- df %>% mutate(localtime = strftime(chunk, format="%H:%M:%S"),GMTtime = strftime(chunk, format="%H:%M:%S",tz="GMT"), weekday = weekdays(chunk), month = month(chunk))
saved <- df
# Checking where 5am peak is on GMT time and Local time
#df[1:10,]
#df[90000:90010,]
#aggrGMT <- df %>% group_by(GMTtime) %>% summarise(texts = mean(texts)) %>% mutate(GMTtime = as.POSIXct(GMTtime,format="%H:%M:%S"))
#head(aggrGMT)
#aggrlocal <- df %>% group_by(localtime) %>% summarise(texts = mean(texts)) %>% mutate(localtime = as.POSIXct(localtime,format="%H:%M:%S"))
#head(aggrlocal)
#plot(aggrGMT$GMTtime,aggrGMT$texts, type='l') # One Peak at GMT
#plot(aggrlocal$localtime,aggrlocal$texts, type='l') # Two Peaks at Local Time
# aggrGMT %>% filter(GMTtime >= as.POSIXct("05:00:00",format="%H:%M:%S") & GMTtime < as.POSIXct("06:00:00",format="%H:%M:%S"))
# Peak is just at 5am GMT

# Removing 5am GMT peak by averaging text values with those either side of it
str(df)
df[df$GMTtime == "05:00:00",] # This selects the rows I want

for (i in 1:(dim(df)[1])) {
  if (df$GMTtime[i] == "05:00:00") {
    df$texts[i] <- mean(df$texts[i+1],df$texts[i-1])
  }
}

# Checking this has worked
#aggrGMT <- df %>% group_by(GMTtime) %>% summarise(texts = mean(texts)) %>% mutate(GMTtime = as.POSIXct(GMTtime,format="%H:%M:%S"))
#head(aggrGMT)
#plot(aggrGMT$GMTtime,aggrGMT$texts, type='l') # There is no peak
#aggrGMT %>% filter(GMTtime >= as.POSIXct("05:00:00",format="%H:%M:%S") & GMTtime < as.POSIXct("06:00:00",format="%H:%M:%S"))
# There is no peak

write.csv(df,'013 Time Series Full Prediction.csv')
saveRDS(df,'013 Time Series Full Prediction.rds')

plot(df$chunk,df$texts, type='l',col="green")






# Linear modelling
df <- readRDS('013 Time Series Full Prediction.rds')
str(df)
train <- df %>% filter(chunk < "2021-03-13")
test <- df %>% filter(chunk >= "2021-03-13")

colnames(train)
lm <- lm(texts ~ chunk + factor(localtime) + factor(weekday) + factor(month), data=train)
summary(lm)

# Predict on Test Set
preds <- predict(lm,test)
test.preds <- cbind(test,preds)
head(test.preds)

# Performance/Error Metrics: Training and Test
# MAE
# MAPE
# RMSE
# MFE (gives a sense of bias)

# R native funcitons
mse = MSE(test.preds$preds, test.preds$texts)
mae = MAE(test.preds$preds, test.preds$texts)
# caret package functions 
rmse = RMSE(test.preds$preds, test.preds$texts)
r2 = R2(test.preds$preds, test.preds$texts, form = "traditional")
# My functions
d = test.preds$texts - test.preds$preds
mfe = mean((d))
mape = mean(abs((d)/test.preds$texts))*100

summary(test.preds$texts)
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2, "\n",
    "MFE:", mfe, "\n", "MAPE:", mape)

#Performance plot
ggplot(test.preds,aes(preds,texts)) + geom_point(alpha=0.5) + theme_bw()

# An example day
eg_plot <- test.preds %>% filter(chunk >= "2021-03-20" & chunk < "2021-03-21")
ggplot(eg_plot) + geom_line(aes(chunk,texts),col="green") + geom_line(aes(chunk,preds),col="blue") + theme_bw()

eg_plot <- test.preds %>% filter(chunk >= "2021-05-11" & chunk < "2021-05-12")
ggplot(eg_plot,aes(chunk,texts)) + geom_line(col="green") + geom_line(aes(chunk,preds),col="blue") + theme_bw()
