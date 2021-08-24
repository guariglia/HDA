# 014 Twitter+Text Predictors from Scratch.R

# Here we take our texts time series df and layer on Twitter data.

# Twitter Volume
tweetsdf <- readRDS('000 shouttweets.rds')
tweetsdf$chunk <- lubridate::floor_date(tweetsdf$created_at, "10 minutes")
tweetsdf <- tweetsdf %>% transmute(chunk,
                                   retweets = public_metrics.retweet_count,
                                   likes = public_metrics.like_count,
                                   quotes = public_metrics.quote_count,
                                   replies = public_metrics.reply_count)
counts <- tweetsdf %>% count(chunk)
metrics <- tweetsdf %>%
  group_by(chunk) %>%
  summarize(retweets = sum(retweets),
            likes = sum(likes),
            quotes = sum(quotes),
            replies = sum(replies))
head(counts)
head(metrics)
tweetsdf <- cbind(counts,metrics[,2:5])
head(tweetsdf)
tail(tweetsdf)
write.csv(tweetsdf,"000_shout_tweets_10minchunk.csv")


# Combining Our 3 Datasets
textsdf <- readRDS('013 Time Series Full Prediction.rds')
str(textsdf)
tweetsdf <- read.csv('000_shout_tweets_10minchunk.csv') %>% mutate(tweets = n,chunk = as.POSIXct(chunk, format="%Y-%m-%d %H:%M:%S",tz="GMT")) %>% dplyr::select(-c(X,n))
str(tweetsdf)

merged <- merge(textsdf, tweetsdf,by=c("chunk"),all.x=TRUE)
merged[is.na(merged)] <- 0
#plot(merged$chunk,merged$texts,type='l')
summary(merged)
tail(merged)
str(merged)
write.csv(merged,"014 text+tweet full time series.csv")
saveRDS(merged,"014 text+tweet full time series.rds")
