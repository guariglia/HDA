# RESULTS : Observational Analyses

rm(list=ls())
setwd("~/summerproj/data/")
library(dplyr)
library(lubridate)
library(lmtest)


df <- as_tsibble(readRDS("022 full data.rds"), index = chunk)
head(df)
tail(df)
str(df)
ts <- df %>% dplyr::filter(chunk >= "2020-03-01") 

# Set between function
between <- function(df, startdate, enddate) {
  data <- dplyr::filter(df,chunk >= startdate & chunk < enddate)
  return(data)
}

# Summary Stats Plot and Table
summarystats <- cbind(
  summary(df$texts),
  summary(between(df,"2018-07-01","2018-10-01")$texts),
  summary(between(df,"2018-10-01","2019-01-01")$texts),
  summary(between(df,"2019-01-01","2019-04-01")$texts),
  summary(between(df,"2019-04-01","2019-07-01")$texts),
  summary(between(df,"2019-07-01","2019-10-01")$texts),
  summary(between(df,"2019-10-01","2020-01-01")$texts),
  summary(between(df,"2020-01-01","2020-04-01")$texts),
  summary(between(df,"2020-04-01","2020-07-01")$texts),
  summary(dplyr::filter(df,chunk >= "2020-07-01" & chunk < "2020-10-01")$texts),
  summary(dplyr::filter(df,chunk >= "2020-10-01" & chunk < "2021-01-01")$texts),
  summary(dplyr::filter(df,chunk >= "2021-01-01" & chunk < "2021-04-01")$texts),
  summary(dplyr::filter(df,chunk >= "2021-04-01" & chunk < "2021-07-01")$texts))
#colnames(summarystats)<- c("Total","Q2 2020","Q3 2020","Q4 2020","Q1 2021","Q2 2021 (so far)")
colnames(summarystats)<- c("Total","Q3 2018","Q4 2018","Q1 2019","Q2 2019","Q3 2019","Q4 2019","Q1 2020","Q2 2020","Q3 2020","Q4 2020","Q1 2021","Q2 2021")
summarytable <- as.data.frame(t(round(summarystats,2)))
colnames(summarytable) <- c("Min","Q1","Median","Mean","Q3","Max")

summarytable <- mutate(summarytable, IQR = Q3-Q1)
barplot(summarystats[3:4,],beside = T,legend=T,args.legend=list("x"="topleft"))

summarytable$rownames <- c("Total","Q3 2018","Q4 2018","Q1 2019","Q2 2019","Q3 2019","Q4 2019","Q1 2020","Q2 2020","Q3 2020","Q4 2020","Q1 2021","Q2 2021")

tidy <- summarytable %>% dplyr::filter(rownames != "Total") %>% dplyr::select('rownames','Mean','Median') %>% 
  reshape2::melt(id.vars='rownames', variable.name='Variable', value.name='value')
tidy1 <- tidy %>% dplyr::filter(Variable=='Mean') %>%
  mutate(rownames=factor(rownames, levels=rownames))
tidy2 <- tidy %>% dplyr::filter(Variable=='Median') %>%
  mutate(rownames=factor(rownames, levels=rownames))
tidy <- rbind(tidy1, tidy2)
ggplot(tidy, aes(x=rownames,y=value, fill=Variable)) + scale_fill_brewer(palette="Set2") +
  geom_col(position = "dodge") + theme_bw() +
  xlab('Time, Quarters') + ylab('Hourly Texts') + ggtitle("Increasing text volumes over time")


n_rows <- dim(ts)[1]
ci <- function(vector) {
  return(1.96*sd(vector)/sqrt(n_rows))
}
chocolate <- df %>% mutate(year = floor_date(chunk, 'year')) %>% 
  as.data.frame %>% group_by(year) %>%
  summarise(texts_mean = mean(texts),
            texts_ci = ci(texts),
            temp_mean = mean(temp),
            temp_ci = ci(temp),
            cloud_mean = mean(clouds),
            cloud_ci = ci(clouds),
            rain_mean = mean(rainfall),
            rain_ci = ci(rainfall),
            covid_cases_mean = mean(covid_cases),
            covid_cases_ci = ci(covid_cases),
            covid_deaths_mean = mean(covid_deaths),
            covid_deaths_ci = ci(covid_deaths),
            covid_announcements_mean = mean(covid_announcements),
            covid_announcements_ci = ci(covid_announcements),
            tweets_mean = mean(tweets),
            tweets_ci = ci(tweets),
            likes_mean = mean(likes),
            likes_ci = ci(likes),
            retweets_mean = mean(retweets),
            retweets_ci = ci(retweets)
            )
chocolate

# Line plots with increasing zoom
# (a) daily data over the last 2 years
df$day <- lubridate::floor_date(df$chunk, "days")
day <- df %>% as.data.frame() %>% group_by(day) %>% summarise(texts = sum(texts))
head(day)
ggplot(dplyr::filter(day, day >= "2019-05-13"),aes(day,texts)) + geom_line(col="red") +
  theme_bw() + ggtitle("Daily volumes: May 2019 - May 2021") + xlab("Time, Day") +ylab("Texts")
# (b) hourly data over the last 1 month
df$hour <- lubridate::floor_date(df$chunk, "hours")
hour <- df %>% as.data.frame() %>% group_by(hour) %>% summarise(texts = sum(texts))
ggplot(dplyr::filter(hour, hour >= "2021-04-11"),aes(hour,texts)) + geom_line(col="red") +
  theme_bw() + ggtitle("Hourly volumes: April 2021 - May 2021") + xlab("Time, Hour") +ylab("Texts")
# (c) 10 min data over the last 1 week
tenmin <- as_tsibble(readRDS("~/summerproj/data/014 text+tweet full time series.rds"),index=chunk)
head(tenmin)
ggplot(dplyr::filter(tenmin, chunk >= "2021-05-03"),aes(chunk,texts)) + geom_line(col="red") + 
  theme_bw() + ggtitle("10 min volumes: May 2021") + xlab("Time, 10min Chunks") +ylab("Texts")

# Looking at changein activity since the start of COVID in the UK.
pre_covid <- day %>% dplyr::filter(day < "2020-03-01" & day > "2020-02-01")
peak_covid <- day %>% dplyr::filter(day < "2021-01-01" & day > "2020-12-01")
head(pre_covid)
mean(pre_covid$texts)
mean(peak_covid$texts)
mean(peak_covid$texts)/mean(pre_covid$texts)

# Autocorrelation plots
ts %>%
  ACF(texts) %>%
  autoplot() + labs(title="ACF: Text Volume") +theme_bw()
ts %>%
  PACF(texts) %>%
  autoplot() + labs(title="PACF: Text Volume") + theme_bw()



# STL Decomposition
ts %>%
  model(
    STL(texts ~ trend(window = 1000) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()




hist(ts$texts) # clearly non-normal
summary(ts$texts)
hist(sqrt(ts$texts)) # much more normal

# Localtime vs Texts in that hour BoxPlot
head(ts)
ts$localtime_numeric <- as.numeric(substr(ts$localtime,start=1,stop=2))
ts$localtime <- substr(ts$localtime,start=1,stop=2)
ggplot(ts,aes(localtime,texts)) + geom_boxplot() + #ggtitle("Text Volume (from 03/20)") +
    ylab("Texts per hr") + xlab("Time of Day") + theme_bw()
ts$localtime
# Linear Model
summary(lm(sqrt(texts) ~ localtime,data=ts))

  # Plot mean texts per hour
data_length <- dim(ts)[1]
byhour <- ts %>% as.data.frame %>% group_by(localtime) %>% summarise(mean = mean(texts),
                                                    ci = 1.96*sd(texts)/sqrt(data_length)
                                                    )
ggplot(data=byhour,aes(x=as.numeric(localtime), y=mean)) + geom_line(col='purple') +
  geom_ribbon(aes(ymin = mean-ci, ymax = mean+ci), alpha = 0.1) +
  ylab("Hourly Texts") + xlab("Hour of Day") + ggtitle("Mean text volume by hour of day") + theme_bw()
  # Forest plot of linear model results
# First make our dataframe with the linear model results
coef <- summary(lm(sqrt(texts) ~ localtime,data=ts))$coefficients
coef <- coef[2:24,1:2] %>% as.data.frame
colnames(coef) <- c('mean','se')
coef <- coef %>% dplyr::transmute(mean, lower = mean-1.96*se, upper = mean+1.96*se)
label <- paste0(1:23)
coef <- cbind(label, coef)
# reverses the factor level ordering for labels after coord_flip()
coef$label <- factor(coef$label, levels=rev(coef$label))
library(ggplot2)
fp <- ggplot(data=coef, aes(x=label, y=mean, ymin=lower, ymax=upper)) +
  geom_pointrange(col='purple') + 
  #geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Hour of Day") + ylab("Beta Coefficient (95% CI)") + ggtitle("Hour of day linear regression coefficients") +
  theme_bw()  # use a white background
print(fp)
  # Getting a table to save
coef <- summary(lm(sqrt(texts) ~ localtime,data=ts))$coefficients
coef <- coef %>% as.data.frame
colnames(coef) <- c('beta','se','t_value','P_value')
coef <- coef %>% dplyr::transmute(beta, lower = beta-1.96*se, upper = beta+1.96*se, P_value)
label <- c("int",paste0(1:23))
coef <- cbind(label, coef)
write.csv(coef,'030 timeofday_lm.csv')

# Weekday vs Texts in that hour BoxPlot
order <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
ts$weekday <- factor(ts$weekday,levels = order)
ts <- ts %>% arrange(weekday)
ggplot(ts,aes(weekday,texts),stat='identity') + geom_boxplot() + #ggtitle("Text Volume Since Feb 2020")
  ylab("Texts per hr") + xlab("Time of Day") + theme_bw()
# Linear Model
ts$weekday <- relevel(ts$weekday, ref = 'Friday') 
summary(lm(sqrt(texts) ~ weekday,data=ts))
  # Plot barplot
data_length <- dim(ts)[1]
byhour <- ts %>% as.data.frame %>% group_by(weekday) %>% summarise(mean = mean(texts),
                                                                     ci = 1.96*sd(texts)/sqrt(data_length)
)
order <- c("Mon","Tue","Weds","Thurs","Fri","Sat","Sun")
byhour$weekday <- order
byhour$weekday <- factor(byhour$weekday,levels = order)
ggplot(data=byhour,aes(x=weekday, y=mean)) + geom_col(fill='dark green') +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), alpha = 0.5) +
  ylab("Hourly Texts") + xlab("Weekday") + ggtitle("Mean text volume by weekday") + theme_bw()
  # Forest plot of linear model results
# First make our dataframe with the linear model results
coef <- summary(lm(sqrt(texts) ~ weekday,data=ts))$coefficients
coef <- coef[2:7,1:2] %>% as.data.frame
colnames(coef) <- c('mean','se')
coef <- coef %>% dplyr::transmute(mean, lower = mean-1.96*se, upper = mean+1.96*se)
label <- c("Monday", "Tuesday", "Wednesday", "Thursday","Saturday", "Sunday")
order <- c(3,4,5,6,1,2)
coef <- cbind(label, coef)
coef <- cbind(order, coef)
coef <- arrange(coef, order)
coef
# reverses the factor level ordering for labels after coord_flip()
coef$label <- factor(coef$label, levels=rev(coef$label))
library(ggplot2)
fp <- ggplot(data=coef, aes(x=label, y=mean, ymin=lower, ymax=upper)) +
  geom_pointrange(col='dark green') + 
  #geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Weekday") + ylab("Beta Coefficient (95% CI)") + ggtitle("Weekday linear regression coefficients") +
  theme_bw()  # use a white background
print(fp)
  # Getting a table to save
coef <- summary(lm(sqrt(texts) ~ weekday,data=ts))$coefficients
coef <- coef %>% as.data.frame
colnames(coef) <- c('beta','se','t_value','P_value')
coef <- coef %>% dplyr::transmute(beta, lower = beta-1.96*se, upper = beta+1.96*se, P_value)
label <- c("int","Monday", "Saturday", "Sunday", "Thursday", "Tuesday", "Wednesday")
coef <- cbind(label, coef)
write.csv(coef,'030 weekday_lm.csv')


# Bank Holiday vs Texts in that hour BoxPlot
ggplot(ts,aes(factor(bank_holiday),sqrt(texts)),stat='identity') + geom_boxplot() + ggtitle("Text Volume Since Feb 2020")
# Linear Model
summary(lm(sqrt(texts) ~ bank_holiday,data=ts))
cor.test(ts$bank_holiday,sqrt(ts$texts))
grangertest(sqrt(texts) ~ bank_holiday, order = 1, data = ts)
grangertest(sqrt(texts) ~ bank_holiday, order = 2, data = ts)


# School Hold vs Texts in that hour BoxPlot
ggplot(ts,aes(factor(school_hols),sqrt(texts)),stat='identity') + geom_boxplot() + ggtitle("Text Volume Since Feb 2020")
# Linear Model
summary(lm(sqrt(texts) ~ school_hols,data=ts))
cor.test(ts$school_hols,sqrt(ts$texts))
grangertest(sqrt(texts) ~ school_hols, order = 1, data = ts)
grangertest(sqrt(texts) ~ school_hols, order = 2, data = ts)

# Exam results vs Texts in that hour BoxPlot
ggplot(ts,aes(factor(exam_results_week),sqrt(texts)),stat='identity') + geom_boxplot() + ggtitle("Text Volume Since Feb 2020")
# Linear Model
summary(lm(sqrt(texts) ~ exam_results_week,data=ts))
summary(lm(sqrt(texts) ~ exam_results_week + temp,data=ts))
cor.test(ts$exam_results_week,sqrt(ts$texts))
grangertest(sqrt(texts) ~ exam_results_week, order = 1, data = ts)
grangertest(sqrt(texts) ~ exam_results_week, order = 2, data = ts)


head(ts)







# Weather Predictors
  # temperature
ggplot(ts,aes(chunk,temp)) + geom_line(col="darkgreen") + theme_bw()
hist(ts$temp)
ggplot(ts,aes(temp,sqrt(texts))) + geom_point(alpha=0.1) + ggtitle("Temperature and Texts Relationship")
ts$avg.temp <- ifelse(ts$temp<0,"0 & below",
                      ifelse(ts$temp<5,"0-5",
                             ifelse(ts$temp<10,"5-10",
                                    ifelse(ts$temp<15,"10-15",
                                           ifelse(ts$temp<20,"15-20",
                                                  ifelse(ts$temp<25,"20-25","25+"))))))
ts$avg.temp <- factor(ts$avg.temp, levels=c("0 & below","0-5","5-10","10-15","15-20","20-25","25+"), ordered=TRUE)
temperatures<- ts %>% as.data.frame() %>% group_by(avg.temp) %>% summarise(mean_texts = mean(texts))
head(temperatures)
ggplot(temperatures,aes(avg.temp,mean_texts)) + geom_col()
cor.test(ts$temp,sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ temp))
grangertest(sqrt(texts) ~ temp, order = 1, data = ts)
grangertest(sqrt(texts) ~ temp, order = 2, data = ts)

# take a moving avg
ts$temp_avg <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(25,(dim(ts)[1]),by=1)) {
  ts[i,'temp_avg'] <- (ts[(i-24):(i-1),'temp']  %>% summarise_if(is.numeric, mean))[[1]]
}
summary(ts$temp_avg)
cor.test(ts$temp_avg,sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ temp_avg))
grangertest(sqrt(texts) ~ temp_avg, order = 1, data = ts)
grangertest(sqrt(texts) ~ temp_avg, order = 2, data = ts)

# take a daily avg
ts <- ts %>% as.data.frame %>% mutate(day = as.character(floor_date(chunk, 'day')))
daily_temp <- ts %>% group_by(day) %>% summarise(daily_temp = mean(temp))
ts <- merge(ts,daily_temp, by="day") %>% as_tsibble(index = chunk) %>% dplyr::select(-day)

cor.test(ts$daily_temp,sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ daily_temp))
grangertest(sqrt(texts) ~ daily_temp, order = 1, data = ts)
grangertest(sqrt(texts) ~ daily_temp, order = 2, data = ts)

  # clouds
ggplot(ts,aes(chunk,clouds)) + geom_line(col="darkgreen") + theme_bw()
hist(ts$clouds)
ggplot(ts,aes(clouds,sqrt(texts))) + geom_point(alpha=0.1) + ggtitle("Clouds and Texts Relationship")
ts$avg.cloud <- ifelse(ts$clouds <15,"<15",
                       ifelse(ts$clouds <30,"<30",
                              ifelse(ts$clouds <50,"<50",
                                     ifelse(ts$clouds <80,"<80",">80"
                                     ))))
barplot(table(ts$avg.cloud))
ts$avg.cloud <- factor(ts$avg.cloud, levels=c("<15","<30","<50","<80",">80"), ordered=TRUE)
clouds<- ts %>% as.data.frame() %>% group_by(avg.cloud) %>% summarise(texts = mean(texts))
ggplot(clouds,aes(avg.cloud,texts)) + geom_col()
cor.test(ts$clouds,sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ avg.cloud))
summary(lm(data=ts,sqrt(texts) ~ clouds))
grangertest(sqrt(texts) ~ clouds, order = 1, data = ts)
grangertest(sqrt(texts) ~ clouds, order = 2, data = ts)
  # rainfall
ggplot(ts,aes(chunk,rainfall)) + geom_line(col="darkgreen") + theme_bw()
hist(ts$rainfall)
ggplot(ts,aes(rainfall,sqrt(texts))) + geom_point(alpha=0.1) + ggtitle("Rainfall and Texts Relationship")
ts$avg.rain <- ifelse(ts$rainfall<0.001,"0",
                      ifelse(ts$rainfall<1,"<1",
                             ifelse(ts$rainfall<5,"<5",
                                    ifelse(ts$rainfall<10,"<10",">10"
                                    ))))
ts$avg.rain <- factor(ts$avg.rain, levels=c("0","<1","<5","<10",">10"), ordered=TRUE)
rains<- ts %>% as.data.frame %>% group_by(avg.rain) %>% summarise(texts = mean(texts)) %>% as.data.frame()
ggplot(rains,aes(avg.rain,texts)) + geom_col()
cor.test(log(ts$rainfall+0.1),sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ log(ts$rainfall+0.1)))
hist(sqrt(ts$rainfall))
hist(log(ts$rainfall+0.1))
grangertest(sqrt(texts) ~ log(rainfall+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(rainfall+0.1), order = 2, data = ts)





# Twitter
  # Tweets
ggplot(ts,aes(chunk,tweets)) + geom_line(col="purple") + theme_bw()
hist(ts$tweets)
ggplot(ts,aes(log(tweets+0.1),sqrt(texts))) + geom_point(alpha=0.1) + ggtitle("Tweets and Texts Relationship")
ts$avg.tweet <- ifelse(ts$tweets<0,"0 & below",
                      ifelse(ts$tweets<5,"0-5",
                             ifelse(ts$tweets<10,"5-10",
                                    ifelse(ts$tweets<20,"10-20",
                                           ifelse(ts$tweets<40,"20-40",
                                                  ifelse(ts$tweets<70,"40-70","70+"))))))
ts$avg.tweet <- factor(ts$avg.tweet, levels=c("0 & below","0-5","5-10","10-20","20-40","40-70","70+"), ordered=TRUE)
tweeteratures<- ts %>% as.data.frame() %>% group_by(avg.tweet) %>% summarise(mean_texts = mean(texts))
head(tweeteratures)
ggplot(tweeteratures,aes(avg.tweet,mean_texts)) + geom_col()
cor.test(log(ts$tweets+0.1),sqrt(ts$texts))
summary(lm(data=ts,texts ~ tweets))
summary(lm(data=ts,texts ~ sqrt(tweets)))
summary(lm(data=ts,sqrt(texts) ~ log(tweets+0.1)))
grangertest(sqrt(texts) ~ log(tweets+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(tweets+0.1), order = 2, data = ts)

  # Take a moving average of the last 24 hr of tweets.
ts$tweet_avg <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(25,(dim(ts)[1]),by=1)) {
  ts[i,'tweet_avg'] <- (ts[(i-24):(i-1),'tweets']  %>% summarise_if(is.numeric, mean))[[1]]
}
summary(ts$tweet_avg)
hist(ts$tweet_avg)
cor.test(log(ts$tweet_avg+0.1),sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ log(tweet_avg+0.1)))
grangertest(sqrt(texts) ~ log(tweet_avg+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(tweet_avg+0.1), order = 2, data = ts)

# Take a moving average of the last 5 hr of tweets.
ts$tweet_avg <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(6,(dim(ts)[1]),by=1)) {
  ts[i,'tweet_avg'] <- (ts[(i-5):(i),'tweets']  %>% summarise_if(is.numeric, mean))[[1]]
}
summary(ts$tweet_avg)
hist(ts$tweet_avg)
cor.test(log(ts$tweet_avg+0.1),sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ log(tweet_avg+0.1)))
grangertest(sqrt(texts) ~ log(tweet_avg+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(tweet_avg+0.1), order = 2, data = ts)

ggplot(ts,aes(log(tweet_avg+0.1),sqrt(texts))) + geom_point(alpha=0.1) + ggtitle("Tweets and Texts Relationship") +
  theme_bw() + xlab("5hr Moving Average of Shout Tweets (#/@), log") + ylab("Text Volume, sqrt")
ts$avg.tweet <- ifelse(ts$tweet_avg==0,"0",
                       ifelse(ts$tweet_avg<5,"0-5",
                              ifelse(ts$tweet_avg<10,"5-10",
                                     ifelse(ts$tweet_avg<15,"10-15",
                                            ifelse(ts$tweet_avg<30,"15-30","30+")))))
ts$avg.tweet <- factor(ts$avg.tweet, levels=c("0","0-5","5-10","10-15","15-30","30+"), ordered=TRUE)
summary(ts$avg.tweet)
tweeteratures<- ts %>% as.data.frame() %>% group_by(avg.tweet) %>% summarise(mean_texts = mean(texts))
tweeteratures
ggplot(tweeteratures,aes(avg.tweet,mean_texts)) + geom_col() + theme_bw() +
  xlab("5hr Moving Average of Shout Tweets (#/@)") + ylab("Text Volume")

# Take a moving average of the last 10 hr of tweets.
ts$tweet_avg <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(11,(dim(ts)[1]),by=1)) {
  ts[i,'tweet_avg'] <- (ts[(i-10):(i),'tweets']  %>% summarise_if(is.numeric, mean))[[1]]
}
summary(ts$tweet_avg)
hist(ts$tweet_avg)
cor.test(log(ts$tweet_avg+0.1),sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ log(tweet_avg+0.1)))
grangertest(sqrt(texts) ~ log(tweet_avg+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(tweet_avg+0.1), order = 2, data = ts)

# Take a lagged tweet column.
ts$tweet_lag1 <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(2,(dim(ts)[1]),by=1)) {
  ts[i,'tweet_lag1'] <- ts[(i-1),'tweets']
}
summary(ts$tweet_lag1)
hist(ts$tweet_lag1)
cor.test(log(ts$tweet_lag1+0.1),sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ log(tweet_lag1+0.1)))
grangertest(sqrt(texts) ~ log(tweet_lag1+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(tweet_lag1+0.1), order = 2, data = ts)

# Take a lagged tweet column.
ts$tweet_lag2 <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(3,(dim(ts)[1]),by=1)) {
  ts[i,'tweet_lag2'] <- ts[(i-2),'tweets']
}
summary(ts$tweet_lag2)
hist(ts$tweet_lag2)
cor.test(log(ts$tweet_lag2+0.1),sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ log(tweet_lag2+0.1)))
grangertest(sqrt(texts) ~ log(tweet_lag2+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(tweet_lag2+0.1), order = 2, data = ts)

# Take a lagged tweet column.
ts$tweet_lag3 <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(4,(dim(ts)[1]),by=1)) {
  ts[i,'tweet_lag3'] <- ts[(i-3),'tweets']
}
summary(ts$tweet_lag3)
hist(ts$tweet_lag3)
cor.test(log(ts$tweet_lag3+0.1),sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ log(tweet_lag3+0.1)))
grangertest(sqrt(texts) ~ log(tweet_lag3+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(tweet_lag3+0.1), order = 2, data = ts)

# Take a lagged tweet column.
ts$tweet_lag4 <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(5,(dim(ts)[1]),by=1)) {
  ts[i,'tweet_lag4'] <- ts[(i-4),'tweets']
}
summary(ts$tweet_lag4)
hist(ts$tweet_lag4)
cor.test(log(ts$tweet_lag4+0.1),sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ log(tweet_lag4+0.1)))
grangertest(sqrt(texts) ~ log(tweet_lag4+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(tweet_lag4+0.1), order = 2, data = ts)

# Take a lagged tweet column.
ts$tweet_lag5 <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(6,(dim(ts)[1]),by=1)) {
  ts[i,'tweet_lag5'] <- ts[(i-5),'tweets']
}
summary(ts$tweet_lag5)
hist(ts$tweet_lag5)
cor.test(log(ts$tweet_lag5+0.1),sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ log(tweet_lag5+0.1)))
grangertest(sqrt(texts) ~ log(tweet_lag5+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(tweet_lag5+0.1), order = 2, data = ts)

# Take a lagged tweet column.
ts$tweet_lag6 <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(7,(dim(ts)[1]),by=1)) {
  ts[i,'tweet_lag6'] <- ts[(i-6),'tweets']
}
summary(ts$tweet_lag6)
hist(ts$tweet_lag6)
cor.test(log(ts$tweet_lag6+0.1),sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ log(tweet_lag6+0.1)))
grangertest(sqrt(texts) ~ log(tweet_lag6+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(tweet_lag6+0.1), order = 2, data = ts)

ggplot(ts,aes(chunk,tweets)) + geom_line(col="purple") + theme_bw()
hist(ts$tweet_lag4)
ggplot(ts,aes(log(tweet_lag4+0.1),sqrt(texts))) + geom_point(alpha=0.1) + ggtitle("Tweets and Texts Relationship")

  # Likes
ggplot(ts,aes(chunk,likes)) + geom_line(col="purple") + theme_bw()
hist(ts$likes)
ggplot(ts,aes(log(likes),sqrt(texts))) + geom_point(alpha=0.1) + ggtitle("Likes and Texts Relationship")
ts$avg.like <- ifelse(ts$likes<0,"0",
                       ifelse(ts$likes<50,"<50",
                              ifelse(ts$likes<100,"<100",
                                     ifelse(ts$likes<1000,"<1000",
                                            ifelse(ts$likes<2500,"<2500","2500+")))))
ts$avg.like <- factor(ts$avg.like, levels=c("0","<50","<100","<1000","<2500","2500+"), ordered=TRUE)
likeeratures<- ts %>% as.data.frame() %>% group_by(avg.like) %>% summarise(mean_texts = mean(texts))
head(likeeratures)
ggplot(likeeratures,aes(avg.like,mean_texts)) + geom_col()
cor.test(log(ts$likes+0.1),sqrt(ts$texts))
summary(lm(data=ts,texts ~ likes))
summary(lm(data=ts,sqrt(texts) ~ log(likes+0.1)))
grangertest(sqrt(texts) ~ log(likes+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(likes+0.1), order = 2, data = ts)

# Take a moving average of the last 5 hr of likes.
ts$likes_avg <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(6,(dim(ts)[1]),by=1)) {
  ts[i,'likes_avg'] <- (ts[(i-5):(i),'likes']  %>% summarise_if(is.numeric, mean))[[1]]
}
summary(ts$likes_avg)
hist(ts$likes_avg)
cor.test(log(ts$likes_avg+0.1),sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ log(likes_avg+0.1)))
grangertest(sqrt(texts) ~ log(likes_avg+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(likes_avg+0.1), order = 2, data = ts)

  # Retweets
ggplot(ts,aes(chunk,retweets)) + geom_line(col="purple") + theme_bw()
hist(ts$retweets)
ggplot(ts,aes(log(retweets),sqrt(texts))) + geom_point(alpha=0.1) + ggtitle("retweets and Texts Relationship")
ts$avg.retweet <- ifelse(ts$retweets<0,"0",
                      ifelse(ts$retweets<50,"<50",
                             ifelse(ts$retweets<100,"<100",
                                    ifelse(ts$retweets<1000,"<1000",
                                           ifelse(ts$retweets<2500,"<2500","2500+")))))
ts$avg.retweet <- factor(ts$avg.retweet, levels=c("0","<50","<100","<1000","<2500","2500+"), ordered=TRUE)
retweeteratures<- ts %>% as.data.frame() %>% group_by(avg.retweet) %>% summarise(mean_texts = mean(texts))
head(retweeteratures)
ggplot(retweeteratures,aes(avg.retweet,mean_texts)) + geom_col()
cor.test(log(ts$retweets+0.1),sqrt(ts$texts))
summary(lm(data=ts,texts ~ retweets))
summary(lm(data=ts,sqrt(texts) ~ log(retweets+0.1)))
grangertest(sqrt(texts) ~ log(retweets+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(retweets+0.1), order = 2, data = ts)


# Take a moving average of the last 5 hr of retweets.
ts$retweet_avg <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(6,(dim(ts)[1]),by=1)) {
  ts[i,'retweet_avg'] <- (ts[(i-5):(i),'retweets']  %>% summarise_if(is.numeric, mean))[[1]]
}
summary(ts$retweet_avg)
hist(ts$retweet_avg)
cor.test(log(ts$retweet_avg+0.1),sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ log(retweet_avg+0.1)))
grangertest(sqrt(texts) ~ log(retweet_avg+0.1), order = 1, data = ts)
grangertest(sqrt(texts) ~ log(retweet_avg+0.1), order = 2, data = ts)

  # 85258 vs not
str(ts)
summary(lm(data=ts,texts ~ sqrt(shout_w85258_tweets) + sqrt(shout_wo85258_tweets) ))
confint(lm(data=ts,texts ~ sqrt(shout_w85258_tweets) + sqrt(shout_wo85258_tweets) ))
summary(lm(data=ts,texts ~ sqrt(shout_w85258_likes) + sqrt(shout_wo85258_likes) ))
summary(lm(data=ts,texts ~ sqrt(shout_w85258_retweets) + sqrt(shout_wo85258_retweets) ))








# COVID!
  # Cases
head(ts)
ggplot(ts,aes(chunk,covid_cases)) + geom_line(col="purple") + theme_bw()
hist(ts$covid_cases)
ggplot(ts,aes(sqrt(covid_cases),sqrt(texts))) + geom_point(alpha=0.1) + ggtitle("covid_cases and Texts Relationship")
ts$avg.covid_case <- ifelse(ts$covid_cases<0,"0",
                       ifelse(ts$covid_cases<1000,"<1000",
                              ifelse(ts$covid_cases<5000,"<5000",
                                     ifelse(ts$covid_cases<10000,"<10000",
                                            ifelse(ts$covid_cases<30000,"<30000",
                                                   ifelse(ts$covid_cases<50000,"<50000","50000+"))))))
ts$avg.covid_case <- factor(ts$avg.covid_case, levels=c("0","<1000","<5000","<10000","<30000","<50000","50000+"), ordered=TRUE)
covid_caseeratures<- ts %>% as.data.frame() %>% group_by(avg.covid_case) %>% summarise(mean_texts = mean(texts))
head(covid_caseeratures)
ggplot(covid_caseeratures,aes(avg.covid_case,mean_texts)) + geom_col()
cor.test(ts$covid_cases,sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ covid_cases))
summary(lm(data=ts,texts ~ sqrt(covid_cases)))
summary(lm(data=ts,sqrt(texts) ~ sqrt(covid_cases)))
grangertest(sqrt(texts) ~ covid_cases, order = 1, data = ts)
grangertest(sqrt(texts) ~ covid_cases, order = 2, data = ts)
  # Deaths
ggplot(ts,aes(chunk,covid_deaths)) + geom_line(col="purple") + theme_bw()
hist(ts$covid_deaths)
ggplot(ts,aes(sqrt(covid_deaths),sqrt(texts))) + geom_point(alpha=0.1) + ggtitle("covid_deaths and Texts Relationship")
ts$avg.covid_death <- ifelse(ts$covid_deaths<0,"0",
                            ifelse(ts$covid_deaths<50,"<50",
                                   ifelse(ts$covid_deaths<200,"<200",
                                          ifelse(ts$covid_deaths<400,"<400",
                                                 ifelse(ts$covid_deaths<600,"<600",
                                                        ifelse(ts$covid_deaths<1000,"<1000","1000+"))))))
ts$avg.covid_death <- factor(ts$avg.covid_death, levels=c("0","<50","<200","<400","<600","<1000","1000+"), ordered=TRUE)
covid_deatheratures<- ts %>% as.data.frame() %>% group_by(avg.covid_death) %>% summarise(mean_texts = mean(texts))
head(covid_deatheratures)
ggplot(covid_deatheratures,aes(avg.covid_death,mean_texts)) + geom_col()
cor.test(ts$covid_deaths,sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ covid_deaths))
summary(lm(data=ts,texts ~ sqrt(covid_deaths)))
summary(lm(data=ts,sqrt(texts) ~ sqrt(covid_deaths)))
grangertest(sqrt(texts) ~ covid_deaths, order = 1, data = ts)
grangertest(sqrt(texts) ~ covid_deaths, order = 2, data = ts)
  # Announcements
ggplot(ts,aes(chunk,factr(covid_announcements))) + geom_line(col="purple") + theme_bw()
hist(ts$covid_announcements)
covid_announcementeratures <- ts %>% as.data.frame() %>% group_by(covid_announcements) %>% summarise(mean_texts = mean(texts))
ggplot(covid_announcementeratures,aes(factor(covid_announcements),mean_texts)) + geom_col()
cor.test(ts$covid_announcements,sqrt(ts$texts))
summary(lm(data=ts,sqrt(texts) ~ covid_announcements))
summary(lm(data=ts,texts ~ sqrt(covid_announcements)))
summary(lm(data=ts,sqrt(texts) ~ sqrt(covid_announcements)))
grangertest(sqrt(texts) ~ covid_announcements, order = 1, data = ts)
grangertest(sqrt(texts) ~ covid_announcements, order = 2, data = ts)









# Look at Top Peaks
head(day)
plot(day$day,day$texts,type='l')
topdays <- day %>% top_n(25,texts) %>% arrange(desc(texts))
topdays[1:10,]
topdays[11:20,]

# Identify what makes an average day
dim(day)
day <- day %>% dplyr::filter(day >= "2021-01-01") %>% arrange(texts)
day[round(dim(day)[1]*50/100),]




# NEW DATAFRAME
df <- as_tsibble(readRDS("022 full data.rds"), index = chunk)
tail(df)
str(df)
ts <- df %>% dplyr::filter(chunk >= "2020-02-27") 

ts <- ts %>% as.data.frame %>% mutate(day = as.character(floor_date(chunk, 'day')))
daily_temp <- ts %>% group_by(day) %>% summarise(daily_temp = mean(temp))
ts <- merge(ts,daily_temp, by="day") %>% as_tsibble(index = chunk) %>% dplyr::select(-day)

ts$tweet_avg <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(11,(dim(ts)[1]),by=1)) {
  ts[i,'tweet_avg'] <- (ts[(i-10):(i),'tweets']  %>% summarise_if(is.numeric, mean))[[1]]
}

ts$retweet_avg <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(11,(dim(ts)[1]),by=1)) {
  ts[i,'retweet_avg'] <- (ts[(i-10):(i),'retweets']  %>% summarise_if(is.numeric, mean))[[1]]
}

ts$likes_avg <- rep(NA, (dim(ts)[1])) %>% as.numeric
for (i in seq(11,(dim(ts)[1]),by=1)) {
  ts[i,'likes_avg'] <- (ts[(i-10):(i),'likes']  %>% summarise_if(is.numeric, mean))[[1]]
}

ts <- ts %>% dplyr::filter(chunk >= "2020-02-01") %>%
  dplyr::select(-c(shout_w85258_tweets, shout_w85258_retweets, shout_w85258_likes,
                   shout_wo85258_tweets, shout_wo85258_retweets, shout_wo85258_likes))

head(ts)

saveRDS(ts,"030 full data.rds")
ts <- readRDS("030 full data.rds")


## TWEET PLOTS ##
df <- as_tsibble(readRDS("030 full data.rds"), index = chunk)
ts <- df %>% dplyr::filter(chunk >= "2020-03-01") 

ggplot(ts,aes(log(tweet_avg+0.1),sqrt(texts))) + geom_point(alpha=0.25, col="light blue") + ggtitle("Tweets and texts scatterplot") +
  theme_bw() + xlab("5hr Moving Average of Shout Tweets (#/@), log") + ylab("Hourly Texts, sqrt")
ts$avg.tweet <- ifelse(ts$tweet_avg==0,"0",
                       ifelse(ts$tweet_avg<5,"0-5",
                              ifelse(ts$tweet_avg<10,"5-10",
                                     ifelse(ts$tweet_avg<15,"10-15",
                                            ifelse(ts$tweet_avg<30,"15-30","30+")))))
ts$avg.tweet <- factor(ts$avg.tweet, levels=c("0","0-5","5-10","10-15","15-30","30+"), ordered=TRUE)
summary(ts$avg.tweet)
tweeteratures<- ts %>% as.data.frame() %>% group_by(avg.tweet) %>% summarise(mean_texts = mean(texts))
tweeteratures
ggplot(tweeteratures,aes(avg.tweet,mean_texts)) + geom_col(fill="light blue") + theme_bw() + ggtitle("Tweets and texts barplot") +
  xlab("5hr Moving Average of Shout Tweets (#/@)") + ylab("Hourly texts")


## NORMALISED PLOTS ##

rm(list=ls())
setwd("~/summerproj/data")


# Quick look at normalised line plots comparing twitter activity with text outcomes.
df <- as_tsibble(readRDS("030 full data.rds"), index = chunk)
ts <- df %>% dplyr::filter(chunk >= "2020-03-01") 
head(ts)
mean_log_tweets <- mean(log(ts$tweet_avg+0.1))
sd_log_tweets <- sd(log(ts$tweet_avg+0.1))
mean_log_retweets <- mean(log(ts$retweets+0.1))
sd_log_retweets <- sd(log(ts$retweets+0.1))
mean_log_likes <- mean(log(ts$likes+0.1))
sd_log_likes <- sd(log(ts$likes+0.1))

mean_sq_texts <- mean(sqrt(ts$texts))
sd_sq_texts <- sd(sqrt(ts$texts))
ts <- ts %>% mutate(norm_sq_texts = (sqrt(texts) - mean_sq_texts)/sd_sq_texts,
                    norm_log_tweets = (log(tweet_avg+0.1) - mean_log_tweets)/sd_log_tweets,
                    norm_log_retweets = (log(retweet_avg+0.1) - mean_log_retweets)/sd_log_retweets,
                    norm_log_likes = (log(likes_avg+0.1) - mean_log_likes)/sd_log_likes)
head(ts)
ggplot(ts, aes(x=chunk, y=norm_sq_texts, col="Texts, sqrt")) + geom_line(alpha = 0.6) + 
  geom_line(aes(y=norm_log_tweets, col = "5hMAvg Tweets, log"), alpha=0.6) + 
  #geom_line(aes(y=norm_log_retweets, col = "5hMAvg Retweets, log"), alpha=0.6) + 
  #geom_line(aes(y=norm_log_likes, col = "5hMAvg Likes, log"), alpha=0.6) + 
  #ylim(-100,200) +
  theme_bw() + xlab("Hour") + ylab("Normalised Volume") + ggtitle("Normalised hourly tweet and text volumes: March 2020 - May 2021") + scale_color_brewer(palette="Set2",direction=-1)

ggplot(dplyr::filter(ts,chunk>"2020-12-28" & chunk<"2021-01-07"), aes(x=chunk, y=norm_sq_texts, col="Texts, sqrt")) + geom_line(alpha = 0.6) + 
  geom_line(aes(y=norm_log_tweets, col = "5hMAvg Tweets, log"), alpha=0.6) + 
  geom_line(aes(y=norm_log_retweets, col = "5hMAvg Retweets, log"), alpha=0.6) + 
  geom_line(aes(y=norm_log_likes, col = "5hMAvg Likes, log"), alpha=0.6) + 
  #ylim(-100,200) +
  theme_bw() + xlab("Hour") + ylab("Normalised Volume") + scale_color_brewer(palette="Set2",direction=-1)

ggplot(dplyr::filter(ts,chunk>"2021-01-01"), aes(x=chunk, y=norm_texts, col="Sqrt Texts")) + geom_line(alpha = 0.6) + 
  geom_line(aes(y=norm_tweet_avg, col = "Sqrt TweetAvg"), alpha=0.6) + 
  geom_line(aes(y=norm_retweets, col = "Sqrt ReTweets"), alpha=0.6) + 
  geom_line(aes(y=norm_likes, col = "Sqrt Likes"), alpha=0.6) + 
  ylim(-100,200) +
  theme_blank()

ggplot(dplyr::filter(ts,chunk<"01-10-2020"), aes(x=chunk, y=norm_texts, col="Texts")) + geom_line(alpha = 0.6) + 
  geom_line(aes(y=norm_tweets, col = "Tweets"), alpha=0.6) + 
  geom_line(aes(y=norm_retweets, col = "ReTweets"), alpha=0.6) + 
  geom_line(aes(y=norm_likes, col = "Likes"), alpha=0.6) + 
  ylim(-1,20) +
  theme_blank()

head(ts)
tail(ts)
sum(ts$texts)



### Check test set

setwd("~/summerproj/results_data/")
test <- readRDS('long_expost.rds') %>% dplyr::filter(traintest==1)
test %>% as.data.frame %>% summarise(mean_texts = mean(texts))
