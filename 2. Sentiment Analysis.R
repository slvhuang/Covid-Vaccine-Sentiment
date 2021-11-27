# Load the libraries
library(tidyverse)
library(lubridate)
library(twitteR)
library(tm)
library(wordcloud)
library(syuzhet)

# Load the data
tweets_cases_vaccine <- read.csv("tweets_cases_vaccine.csv", header = TRUE)

# Visualize most frequent words and hash tags
wordcloud(tweets_cases_vaccine$text, min.freq = 1000, colors = brewer.pal(8, "Dark2"),
          random.color = TRUE, max.words = 15000)

wordcloud(tweets_cases_vaccine$hashtags, min.freq = 500, colors = brewer.pal(8, "Dark2"),
          random.color = TRUE, max.words = 15000)

## Sentiment Analysis
syuzhet <- get_sentiment(tweets_cases_vaccine$text, method="syuzhet")
tweets_sentiment <- add_column(tweets_cases_vaccine, sentiment = syuzhet, .after = "text")
colnames(tweets_sentiment) <- tolower(colnames(tweets_sentiment))
tweets_sentiment[is.na(tweets_sentiment)] <- 0
#write.csv(tweets_sentiment, "tweets_sentiment.csv", row.names=FALSE)

