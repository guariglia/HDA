# Twitter API Import
setwd("~/summerproj/data")

install.packages("academictwitteR")
# Intro
vignette("academictwitteR-intro")
R.Version()

library(academictwitteR)
library(dplyr)
library(jsonlite)
library(lubridate)

https://github.com/cjbarrie/academictwitteR
https://cran.r-project.org/web/packages/academictwitteR/index.html
# Workhorse:
# get_all_tweets()

# You can call strings: "apples OR oranges" "apples AND oranges"
# You can call users: c("TwitterDev","jack")
# When a data_path is specified, files are stored as JSON files
# If a filename is supplied, functions will save the tweet-level information as a .rds file
# Therefore, functions always return a data.frame object unless a data_path is specified and bind_tweets is set to false
# For large amounts of data it is recommended that data_path and bind_tweets = FALSE options are used to mitigate potential data loss in case query is interrupted
# Users can then use bind_tweet_jsons() to bundle the jsons into a data.frame object!

bearer_token <- # Insert bearer token - it is confidential and cannot be shared publicly


# First Try!

tweets <-
  get_all_tweets(
    "#Shout85258",
    "2020-01-01T00:00:00Z",
    "2020-01-02T00:00:00Z",
    bearer_token,
    file = "000 shouttweets1"
  )

head(tweets)
str(tweets)
dim(tweets)
colnames(tweets)
tweets[,5]
keycols <- tweets[,c('created_at','text','public_metrics','possibly_sensitive')]
  # 'public_metrics.retweet_count','public_metrics.reply_count','public_metrics.like_count','public_metrics.quote_count'
head(keycols)
str(head(keycols))
str(keycols)
metrics <- tweets[,c('created_at','public_metrics')]
head(metrics)
summary(metrics)







#Shout85258 Full History! (subsequently replaced by the next request below)

full <-
  get_all_tweets(
    "#Shout85258",
    "2017-01-01T00:00:00Z",
    "2021-05-15T00:00:00Z",
    is_retweet = FALSE,
    bearer_token,
    file = "000 shouttweets"
  )
# 975 Tweets
# Choose columns I want and flatten
fullmetrics <- jsonlite::flatten(full[,c('created_at','public_metrics')])
# Convert times
fullmetrics <- fullmetrics %>% mutate(created_at = as.POSIXct(created_at, format="%Y-%m-%dT%H:%M:%S.000Z",tz="GMT"),
                                            created_at_UNIX = as.numeric(as.POSIXct(created_at, format="%Y-%m-%dT%H:%M:%S.000Z",tz="GMT")))
# Print Basic Histogram
hist(fullmetrics[,1], breaks="weeks",freq=TRUE,xlab="Date (weeks)")
# Create weekly bins
fullmetrics$created_at_weeks <- lubridate::floor_date(fullmetrics$created_at, "week")
countmetrics <- fullmetrics %>%
  count(created_at_weeks)
summarymetrics <- fullmetrics %>%
  group_by(created_at_weeks) %>%
  summarize(retweets = sum(public_metrics.retweet_count),
            likes = sum(public_metrics.like_count),
            quotes = sum(public_metrics.quote_count),
            replies = sum(public_metrics.reply_count))
summarymetrics <- cbind(summarymetrics,countmetrics['n'])
summarymetrics
# Plot!
plot(summarymetrics$created_at_weeks,summarymetrics$retweets,col ="blue",type='l',xlab='Date (weeks, 2020-2021)',ylab='#Shout85258 Frequency/week')
lines(summarymetrics$created_at_weeks,summarymetrics$n,col ="red")
lines(summarymetrics$created_at_weeks,summarymetrics$likes,col ="purple")
lines(summarymetrics$created_at_weeks,summarymetrics$quotes,col ="orange")
lines(summarymetrics$created_at_weeks,summarymetrics$replies,col ="green")
legend('topleft',inset=0.05,legend=c('Tweets','Retweets','Likes','Quotes','Replies'),cex=.8,col=c('red','blue','purple','orange','green'),pch=c(1,1,1,1,1))






#Shout85258 + @GiveUsAShout Full History!

# Split Data Import into 8 Chunks as it often fails if too large AND doesn't seem to work on submitted jobs
temp <- get_all_tweets(
  "#Shout85258 OR @GiveUsAShout",
  "2017-01-01T00:00:01Z",
  "2019-06-01T00:00:00Z",
  #"2021-05-15T00:00:00Z",
  is_retweet = FALSE,
  bearer_token,
  file = "data/000 shout#@tweets1")
# 1646 Tweets
temp <- get_all_tweets(
  "#Shout85258 OR @GiveUsAShout",
  "2019-06-01T00:00:01Z",
  "2019-12-01T00:00:00Z",
  #"2021-05-15T00:00:00Z",
  is_retweet = FALSE,
  bearer_token,
  file = "data/000 shout#@tweets2")
# 6404 Tweets
temp <- get_all_tweets(
  "#Shout85258 OR @GiveUsAShout",
  "2019-12-01T00:00:01Z",
  "2020-03-01T00:00:00Z",
  #"2021-05-15T00:00:00Z",
  is_retweet = FALSE,
  bearer_token,
  file = "data/000 shout#@tweets3")
# 2740 Tweets
temp <- get_all_tweets(
  "#Shout85258 OR @GiveUsAShout",
  "2020-03-01T00:00:01Z",
  "2020-06-01T00:00:00Z",
  #"2021-05-15T00:00:00Z",
  is_retweet = FALSE,
  bearer_token,
  file = "data/000 shout#@tweets4")
# 3755 Tweets
temp <- get_all_tweets(
  "#Shout85258 OR @GiveUsAShout",
  "2020-06-01T00:00:01Z",
  "2020-10-01T00:00:00Z",
  #"2021-05-15T00:00:00Z",
  is_retweet = FALSE,
  bearer_token,
  file = "data/000 shout#@tweets5")
# 4369 Tweets
temp <- get_all_tweets(
  "#Shout85258 OR @GiveUsAShout",
  "2020-10-01T00:00:01Z",
  "2021-01-01T00:00:00Z",
  #"2021-05-15T00:00:00Z",
  is_retweet = FALSE,
  bearer_token,
  file = "data/000 shout#@tweets6")
# 3167 Tweets
temp <- get_all_tweets(
  "#Shout85258 OR @GiveUsAShout",
  "2021-01-01T00:00:01Z",
  "2021-04-01T00:00:00Z",
  #"2021-05-15T00:00:00Z",
  is_retweet = FALSE,
  bearer_token,
  file = "data/000 shout#@tweets7")
# 6743 Tweets
temp <- get_all_tweets(
  "#Shout85258 OR @GiveUsAShout",
  "2021-04-01T00:00:01Z",
  "2021-05-15T00:00:00Z",
  is_retweet = FALSE,
  bearer_token,
  file = "data/000 shout#@tweets8")
# 2349 Tweets

# Combine all the RDS files I have just imported
for(i in seq(1:8)) {
  temp <- jsonlite::flatten(readRDS(paste0('data/000 shout#@tweets',i,'.rds'))[,c('created_at','public_metrics','id')])
  if(i == 1) {
    df <- temp
  } else {
    df <- rbind(df,temp)
  }
}
dim(df)
# Convert times
fullmetrics <- df %>% mutate(created_at = as.POSIXct(created_at, format="%Y-%m-%dT%H:%M:%S.000Z",tz="GMT"),
                              created_at_UNIX = as.numeric(as.POSIXct(created_at, format="%Y-%m-%dT%H:%M:%S.000Z",tz="GMT")))
# Print Basic Histogram
hist(fullmetrics[,1], breaks="weeks",freq=TRUE,xlab="Date (weeks)")
# Create weekly bins
fullmetrics$created_at_weeks <- lubridate::floor_date(fullmetrics$created_at, "week")
countmetrics <- fullmetrics %>%
  count(created_at_weeks)
summarymetrics <- fullmetrics %>%
  group_by(created_at_weeks) %>%
  summarize(retweets = sum(public_metrics.retweet_count),
            likes = sum(public_metrics.like_count),
            quotes = sum(public_metrics.quote_count),
            replies = sum(public_metrics.reply_count))
summarymetrics <- cbind(summarymetrics,countmetrics['n'])
summarymetrics
# Plot!
plot(summarymetrics$created_at_weeks,summarymetrics$likes,col ="purple",type='l',xlab='Date (weeks, 2020-2021)',ylab='#Shout85258 OR @GiveUsAShout Frequency/week')
lines(summarymetrics$created_at_weeks,summarymetrics$n,col ="red")
lines(summarymetrics$created_at_weeks,summarymetrics$retweets,col ="blue")
lines(summarymetrics$created_at_weeks,summarymetrics$quotes,col ="orange")
lines(summarymetrics$created_at_weeks,summarymetrics$replies,col ="green")
legend('topleft',inset=0.05,legend=c('Tweets','Retweets','Likes','Quotes','Replies'),cex=.8,col=c('red','blue','purple','orange','green'),pch=c(1,1,1,1,1))








# Just 85258 activity in which Shout is mentioned
one <-
  get_all_tweets(
    "85258 shout", # Boolean AND logic when there is a space
    "2018-01-01T00:00:00Z",
    "2021-05-15T00:00:00Z",
    is_retweet = FALSE,
    country = "GB",
    bearer_token
  )
# 224 Tweets
two <-
  get_all_tweets(
    "#Shout85258",
    "2018-01-01T00:00:00Z",
    "2021-05-15T00:00:00Z",
    is_retweet = FALSE,
    bearer_token
  )
# 995 Tweets
full <- rbind(jsonlite::flatten(one[,c('created_at','public_metrics','id')]),jsonlite::flatten(two[,c('created_at','public_metrics','id')])) %>% distinct()
head(full)
str(full)
saveRDS(full,"twitter_import2/85258 Tweets.rds")
# Convert times
fullmetrics <- full %>% mutate(created_at = as.POSIXct(created_at, format="%Y-%m-%dT%H:%M:%S.000Z",tz="GMT"),
                                      created_at_UNIX = as.numeric(as.POSIXct(created_at, format="%Y-%m-%dT%H:%M:%S.000Z",tz="GMT")))
# Print Basic Histogram
hist(fullmetrics[,1], breaks="weeks",freq=TRUE,xlab="Date (weeks)")
# Create weekly bins
fullmetrics$created_at_weeks <- lubridate::floor_date(fullmetrics$created_at, "week")
countmetrics <- fullmetrics %>%
  count(created_at_weeks)
summarymetrics <- fullmetrics %>%
  group_by(created_at_weeks) %>%
  summarize(retweets = sum(public_metrics.retweet_count),
            likes = sum(public_metrics.like_count),
            quotes = sum(public_metrics.quote_count),
            replies = sum(public_metrics.reply_count))
summarymetrics <- cbind(summarymetrics,countmetrics['n'])
summarymetrics
# Plot!
plot(summarymetrics$created_at_weeks,summarymetrics$retweets,col ="blue",type='l',xlab='Date (weeks, 2020-2021)',ylab='#Shout85258 Frequency/week')
lines(summarymetrics$created_at_weeks,summarymetrics$n,col ="red")
lines(summarymetrics$created_at_weeks,summarymetrics$likes,col ="purple")
lines(summarymetrics$created_at_weeks,summarymetrics$quotes,col ="orange")
lines(summarymetrics$created_at_weeks,summarymetrics$replies,col ="green")
legend('topleft',inset=0.05,legend=c('Tweets','Retweets','Likes','Quotes','Replies'),cex=.8,col=c('red','blue','purple','orange','green'),pch=c(1,1,1,1,1))








# Just Shout activity in which 85258 is not mentioned
getwd()
one <-
  get_all_tweets(
    "@giveusashout",
    "2018-01-01T00:00:00Z",
    "2021-05-15T00:00:00Z",
    exclude = "85258",
    is_retweet = FALSE,
    bearer_token,
    data_path = "twitter_import2/shout/",
    bind_tweets = FALSE
  )
# 20k tweets
resume_collection(data_path = "twitter_import2/shout/", bearer_token) # didn't even need to use this
tweets <- bind_tweet_jsons(data_path = "twitter_import2/shout/")

head(tweets)
str(tweets)
full <- jsonlite::flatten(tweets[,c('created_at','public_metrics','id')])
head(full)
str(full)
saveRDS(full,"twitter_import2/Shout (no 85258) Tweets.rds")
# Convert times
fullmetrics <- full %>% mutate(created_at = as.POSIXct(created_at, format="%Y-%m-%dT%H:%M:%S.000Z",tz="GMT"),
                               created_at_UNIX = as.numeric(as.POSIXct(created_at, format="%Y-%m-%dT%H:%M:%S.000Z",tz="GMT")))
# Print Basic Histogram
hist(fullmetrics[,1], breaks="weeks",freq=TRUE,xlab="Date (weeks)")
# Create weekly bins
fullmetrics$created_at_weeks <- lubridate::floor_date(fullmetrics$created_at, "week")
countmetrics <- fullmetrics %>%
  count(created_at_weeks)
summarymetrics <- fullmetrics %>%
  group_by(created_at_weeks) %>%
  summarize(retweets = sum(public_metrics.retweet_count),
            likes = sum(public_metrics.like_count),
            quotes = sum(public_metrics.quote_count),
            replies = sum(public_metrics.reply_count))
summarymetrics <- cbind(summarymetrics,countmetrics['n'])
summarymetrics
# Plot!
plot(summarymetrics$created_at_weeks,summarymetrics$retweets,col ="blue",type='l',xlab='Date (weeks, 2020-2021)',ylab='#Shout85258 Frequency/week')
lines(summarymetrics$created_at_weeks,summarymetrics$n,col ="red")
lines(summarymetrics$created_at_weeks,summarymetrics$likes,col ="purple")
lines(summarymetrics$created_at_weeks,summarymetrics$quotes,col ="orange")
lines(summarymetrics$created_at_weeks,summarymetrics$replies,col ="green")
legend('topleft',inset=0.05,legend=c('Tweets','Retweets','Likes','Quotes','Replies'),cex=.8,col=c('red','blue','purple','orange','green'),pch=c(1,1,1,1,1))








#Covid19 Full History
get_all_tweets(
  "#covid19",
  "2018-01-01T00:00:00Z",
  "2021-05-15T00:00:00Z",
  is_retweet = FALSE,
  country = "GB",
  bearer_token,
  data_path = "twitter_import2/covid/",
  bind_tweets = FALSE
)
# ?? tweets
resume_collection(data_path = "twitter_import2/covid/", bearer_token) # didn't even need to use this
tweets <- bind_tweet_jsons(data_path = "twitter_import2/covid/")

head(tweets)
str(tweets)
full <- jsonlite::flatten(tweets[,c('created_at','public_metrics','id')])
head(full)
str(full)
saveRDS(full,"twitter_import2/Covid Tweets.rds")
# Convert times
fullmetrics <- fullmetrics %>% mutate(created_at = as.POSIXct(created_at, format="%Y-%m-%dT%H:%M:%S.000Z",tz="GMT"),
                                      created_at_UNIX = as.numeric(as.POSIXct(created_at, format="%Y-%m-%dT%H:%M:%S.000Z",tz="GMT")))
# Print Basic Histogram
hist(fullmetrics[,1], breaks="weeks",freq=TRUE,xlab="Date (weeks)")
# Create weekly bins
fullmetrics$created_at_weeks <- lubridate::floor_date(fullmetrics$created_at, "week")
# Save File
saveRDS(fullmetrics,'000 shouttweets.rds')
# Create Binned Dataset
countmetrics <- fullmetrics %>%
  count(created_at_weeks)
summarymetrics <- fullmetrics %>%
  group_by(created_at_weeks) %>%
  summarize(retweets = sum(public_metrics.retweet_count),
            likes = sum(public_metrics.like_count),
            quotes = sum(public_metrics.quote_count),
            replies = sum(public_metrics.reply_count))
summarymetrics <- cbind(summarymetrics,countmetrics['n'])
summarymetrics
# Plot!
plot(summarymetrics$created_at_weeks,summarymetrics$retweets,col ="blue",type='l',xlab='Date (weeks, 2020-2021)',ylab='#Shout85258 Frequency/week')
lines(summarymetrics$created_at_weeks,summarymetrics$n,col ="red")
lines(summarymetrics$created_at_weeks,summarymetrics$likes,col ="purple")
lines(summarymetrics$created_at_weeks,summarymetrics$quotes,col ="orange")
lines(summarymetrics$created_at_weeks,summarymetrics$replies,col ="green")
legend('topleft',inset=0.05,legend=c('Tweets','Retweets','Likes','Quotes','Replies'),cex=.8,col=c('red','blue','purple','orange','green'),pch=c(1,1,1,1,1))
