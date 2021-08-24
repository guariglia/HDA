#Text + Tweet Full Time Series Hourly

setwd("~/summerproj/data/")
library(ggplot2)
library(dplyr)
library(lubridate)
df <- readRDS("014 text+tweet full time series.rds") 
str(df)
summary(df)

df$hourchunk <- floor_date(df$chunk, unit = "hours")
df2 <- df %>% group_by(hourchunk) %>%
          summarise(texts = sum(texts),
                    retweets = sum(retweets),
                    likes = sum(likes),
                    quotes = sum(quotes),
                    replies = sum(replies),
                    tweets = sum(tweets)) %>%
          transmute(chunk = hourchunk,
                    texts,
                    sqrttexts = sqrt(texts),
                    localtime = strftime(chunk, format="%H:%M:%S"),
                    GMTtime = strftime(chunk, format="%H:%M:%S",tz="GMT"),
                    weekday = weekdays(hourchunk),
                    retweets,
                    likes,
                    quotes,
                    replies,
                    tweets
                    )
str(df2)
head(df2)
summary(df2)
plot(df2$chunk,df2$sqrttexts,type='l')

saveRDS(df2,"016 hourly text+tweet full time series.rds")
write.csv(df2,"016 hourly text+tweet full time series.csv")
