# Load the libraries
library(tidyverse)
library(lubridate)

# Load data
tweets_sentiment <- read.csv("tweets_sentiment.csv", header = TRUE)
tweets_sentiment$date <- as.Date(tweets_sentiment$date)

# Basic model
mod1 <- lm(admin_per_100k ~  new_case + new_death + sentiment + tot_cases + tot_death + Admin_New, 
           data = tweets_sentiment)
summary(mod1)


mod2 <- lm(admin_per_100k ~  new_case*new_death + sentiment + tot_cases + tot_death + Admin_New, 
           data = tweets_sentiment)
summary(mod2)

mod3 <- lm(admin_per_100k ~  new_case + new_death + exp(sentiment) + tot_cases*tot_death + Admin_New, 
           data = tweets_sentiment)
summary(mod3)
