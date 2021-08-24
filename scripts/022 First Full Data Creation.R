# First Full Data Creation

# I want to now create a dataset with all of the predictors I want and need.
# Dataset is for 2019-2021.

# Text volumes
# Time Series
  # Date and Time (chunk)
  # Time of Day (localtime)
  # Weekday
  # School Holidays
  # Runup to Exam Results
  # Bank Holidays
# External
  # Twitter Shout #/@ Activity: Tweets, Retweets, Likes
  # Weather: Temp, Cloud Cover, Rainfall
  # Covid: Cases, Lockdowns, Announcements, UK #covid19 Tweets
# Additional
  # 85258 activity vs all Shout non-85258 activity
  # Duke/Duchess of Cambridge and Royal Foundation Tweets/News

setwd("~/summerproj/data/")
library(tidyverse)
df <- readRDS("016 hourly text+tweet full time series.rds")
str(df)
summary(df)
dim(df)
head(df)
tail(df)
  # currently goes from 2018 05 to 2021 05

#--------------------------------------------------

# Text volumes
  # Time Series
  # Date and Time (chunk)
  # Time of Day (localtime)
  # Weekday
fulldata <- df %>% dplyr::select(chunk, texts, localtime, weekday)
  # School Holidays
fulldata <- fulldata %>% mutate(school_hols = ifelse((
      chunk < "2018-09-03" |
      (chunk > "2018-10-26" & chunk < "2018-11-05") |
      (chunk > "2018-12-21" & chunk < "2019-01-07") |
      (chunk > "2019-02-15" & chunk < "2019-02-25") |
      (chunk > "2019-04-05" & chunk < "2019-04-23") |
        (chunk > "2019-05-24" & chunk < "2019-06-03") |
        (chunk > "2019-07-19" & chunk < "2019-09-02") |
        (chunk > "2019-10-18" & chunk < "2019-10-28") |
        (chunk > "2019-12-19" & chunk < "2020-01-06") |
      (chunk > "2020-02-14" & chunk < "2020-02-24") |
        (chunk > "2020-04-03" & chunk < "2020-04-20") |
        (chunk > "2020-05-22" & chunk < "2020-06-01") |
        (chunk > "2020-07-21" & chunk < "2020-09-22") |
        (chunk > "2020-10-23" & chunk < "2020-11-02") |
      (chunk > "2020-12-18" & chunk < "2021-01-04") |
        (chunk > "2021-02-12" & chunk < "2021-02-22") |
        (chunk > "2021-03-31" & chunk < "2021-04-19") |
        (chunk > "2021-05-28" & chunk < "2021-06-07")
    ),1,0))
  # Runup to Exam Results
fulldata <- fulldata %>% mutate(exam_results_week = ifelse((
  (chunk <= "2019-08-22" & chunk > "2019-08-8") |
  (chunk <= "2020-08-20" & chunk > "2020-08-6")
),1,0))
  # Bank Holidays
fulldata <- fulldata %>% mutate(bank_holiday = ifelse((
  (chunk >= "2019-01-01" & chunk < "2019-01-02") |
    (chunk >= "2019-04-19" & chunk < "2019-04-20") |
    (chunk >= "2019-04-22" & chunk < "2019-04-23") |
    (chunk >= "2019-05-06" & chunk < "2019-05-07") |
    (chunk >= "2019-05-27" & chunk < "2019-05-28") |
    (chunk >= "2019-08-26" & chunk < "2019-08-27") |
    (chunk >= "2019-12-25" & chunk < "2019-12-27") |
  (chunk >= "2020-01-01" & chunk < "2020-01-02") |
    (chunk >= "2020-04-10" & chunk < "2020-04-11") |
    (chunk >= "2020-04-13" & chunk < "2020-04-14") |
    (chunk >= "2020-05-08" & chunk < "2020-05-09") |
    (chunk >= "2020-05-25" & chunk < "2020-05-26") |
    (chunk >= "2020-08-31" & chunk < "2020-09-01") |
    (chunk >= "2020-12-25" & chunk < "2020-12-26") |
    (chunk >= "2020-12-28" & chunk < "2020-12-29") |
  (chunk >= "2021-01-01" & chunk < "2021-01-02") |
    (chunk >= "2021-04-02" & chunk < "2021-04-03") |
    (chunk >= "2021-04-05" & chunk < "2021-04-06") |
    (chunk >= "2021-05-03" & chunk < "2021-05-04") |
    (chunk >= "2021-05-31" & chunk < "2021-06-01")
  ),1,0))
summary(fulldata$bank_holiday)
# External
  # Twitter Shout #/@ Activity: Tweets, Retweets, Likes
twittershout <- df %>% dplyr::select(chunk, tweets, retweets, likes)
fulldata <- merge(fulldata, twittershout, by="chunk")
  # Weather: Temp, Cloud Cover, Rainfall
weather <- read_csv("openweather_2010-_2021_snarestone_uk.csv") %>% dplyr::select(dt,dt_iso, temp, clouds_all, rain_1h)
weather$chunk <- as.POSIXct(weather$dt, tz = "GMT", origin="1970-01-01")
str(weather)
weather <- weather %>% transmute(chunk, temp, clouds = clouds_all, rainfall = rain_1h)
fulldata <- merge(fulldata, weather, by="chunk",all.x = TRUE)
fulldata$rainfall <- ifelse(is.na(fulldata$rainfall),0,fulldata$rainfall)
  # Covid: Cases, Lockdowns, Announcements
    # cases
covid_cases <- read_csv("covid_cases_data_2021-Jun-26.csv")
covid_cases <- covid_cases %>% transmute(chunk = as.POSIXct(date,tz="GMT"), covid_cases = newCasesByPublishDate)
row <- cbind(chunk = c(as.POSIXct("2018-05-13 00:00:00",tz="GMT")), covid_cases = c(0))
covid_cases <- rbind(row,covid_cases)
covid_cases$chunk <- as.POSIXct(covid_cases$chunk, tz = "GMT", origin="1970-01-01")
class(covid_cases$chunk)
head(covid_cases)
fulldata <- merge(fulldata, covid_cases, by="chunk",all.x = TRUE)
head(fulldata, 30)
fulldata$covid_cases <- na.locf(fulldata$covid_cases)
tail(fulldata, 30)
    # deaths
covid_deaths <- read_csv("covid_deaths_data_2021-Jun-26.csv")
head(covid_deaths)
covid_deaths <- covid_deaths %>% transmute(chunk = as.POSIXct(date,tz="GMT"), covid_deaths = newDeaths28DaysByDeathDate)
row <- cbind(chunk = c(as.POSIXct("2018-05-13 00:00:00",tz="GMT")), covid_deaths = c(0))
covid_deaths <- rbind(row,covid_deaths)
covid_deaths$chunk <- as.POSIXct(covid_deaths$chunk, tz = "GMT", origin="1970-01-01")
class(covid_deaths$chunk)
head(covid_deaths)
fulldata <- merge(fulldata, covid_deaths, by="chunk",all.x = TRUE)
head(fulldata, 30)
fulldata$covid_deaths <- na.locf(fulldata$covid_deaths)
head(fulldata, 30)
    # announcements # https://www.instituteforgovernment.org.uk/charts/uk-government-coronavirus-lockdowns
fulldata <- fulldata %>% dplyr::mutate(covid_announcements = ifelse(
    (chunk >= as.POSIXlt.character("2020-03-16",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-03-17",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-03-19",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-03-20",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-03-23",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-03-24",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-03-25",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-03-26",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-03-26",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-03-27",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-04-16",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-04-17",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-04-30",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-04-31",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-05-10",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-05-11",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-06-01",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-06-02",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-06-15",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-06-16",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-06-23",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-06-24",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-06-29",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-06-30",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-07-04",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-07-05",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-07-18",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-07-19",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-08-03",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-08-04",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-08-14",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-08-15",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-09-14",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-09-15",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-09-22",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-09-23",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-09-30",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-09-31",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-10-14",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-10-15",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-10-31",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-11-01",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-11-05",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-11-06",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-11-24",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-11-25",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-12-02",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-12-03",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-12-15",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-12-16",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-12-19",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-12-20",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-12-21",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-12-22",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2020-12-26",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2020-12-27",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2021-01-04",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2021-01-05",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2021-01-06",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2021-01-07",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2021-02-15",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2021-02-16",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2021-02-22",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2021-02-23",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2021-03-08",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2021-03-09",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2021-03-29",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2021-03-30",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2021-04-12",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2021-04-13",tz="GMT",format="%Y-%m-%d"))|
    (chunk >= as.POSIXlt.character("2021-05-10",tz="GMT",format="%Y-%m-%d")& chunk < as.POSIXlt.character("2021-05-11",tz="GMT",format="%Y-%m-%d"))   # https://www.theguardian.com/world/2021/may/10/boris-johnson-confirms-further-easing-of-lockdown-in-england
    ,1,0))
fulldata$covid_announcements <- ifelse(is.na(fulldata$covid_announcements),0,fulldata$covid_announcements)
summary(fulldata$covid_announcements)
head(fulldata)
# Additional
  # 85258 activity vs all Shout non-85258 activity
    # 85258 activity
tweetsdf <- readRDS("twitter_import2/85258 Tweets.rds")
tweetsdf <- tweetsdf %>% mutate(created_at = as.POSIXct(created_at, format="%Y-%m-%dT%H:%M:%S.000Z",tz="GMT"))
tweetsdf$chunk <- lubridate::floor_date(tweetsdf$created_at, "1 hour")
tweetsdf <- tweetsdf %>% transmute(chunk,
                                   retweets = public_metrics.retweet_count,
                                   likes = public_metrics.like_count,
                                   quotes = public_metrics.quote_count,
                                   replies = public_metrics.reply_count)
counts <- tweetsdf %>% count(chunk)
metrics <- tweetsdf %>%
  group_by(chunk) %>%
  summarize(retweets = sum(retweets),
            likes = sum(likes))
tweetsdf <- cbind(counts,metrics[,2:3])
colnames(tweetsdf) <- c("chunk","shout_w85258_tweets","shout_w85258_retweets","shout_w85258_likes")
fulldata <- merge(fulldata,tweetsdf,by="chunk",all.x=TRUE)
fulldata <- fulldata %>% dplyr::mutate(shout_w85258_tweets = ifelse(is.na(shout_w85258_tweets),0,shout_w85258_tweets),
                                       shout_w85258_retweets = ifelse(is.na(shout_w85258_retweets),0,shout_w85258_retweets),
                                       shout_w85258_likes = ifelse(is.na(shout_w85258_likes),0,shout_w85258_likes))

    # all Shout non-85258 activity
tweetsdf <- readRDS("twitter_import2/Shout (no 85258) Tweets.rds")
tweetsdf <- tweetsdf %>% mutate(created_at = as.POSIXct(created_at, format="%Y-%m-%dT%H:%M:%S.000Z",tz="GMT"))
tweetsdf$chunk <- lubridate::floor_date(tweetsdf$created_at, "1 hour")
tweetsdf <- tweetsdf %>% transmute(chunk,
                                   retweets = public_metrics.retweet_count,
                                   likes = public_metrics.like_count,
                                   quotes = public_metrics.quote_count,
                                   replies = public_metrics.reply_count)
counts <- tweetsdf %>% count(chunk)
metrics <- tweetsdf %>%
  group_by(chunk) %>%
  summarize(retweets = sum(retweets),
            likes = sum(likes))
tweetsdf <- cbind(counts,metrics[,2:3])
colnames(tweetsdf) <- c("chunk","shout_wo85258_tweets","shout_wo85258_retweets","shout_wo85258_likes")
fulldata <- merge(fulldata,tweetsdf,by="chunk",all.x=TRUE)
fulldata <- fulldata %>% dplyr::mutate(shout_wo85258_tweets = ifelse(is.na(shout_wo85258_tweets),0,shout_wo85258_tweets),
                                       shout_wo85258_retweets = ifelse(is.na(shout_wo85258_retweets),0,shout_wo85258_retweets),
                                       shout_wo85258_likes = ifelse(is.na(shout_wo85258_likes),0,shout_wo85258_likes))



head(fulldata)
duplicates(fulldata)
fulldata <- distinct(fulldata)
str(fulldata)

saveRDS(fulldata, "022 full data.rds")
write.csv(fulldata, "022 full data.csv")
