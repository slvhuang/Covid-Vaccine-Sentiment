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

tweets <- iconv(tweets_cases_vaccine$text)
senti <- get_nrc_sentiment(tweets)
#write.csv(senti, "nrc_sentiment.csv", row.names=FALSE)

#calculationg total score for each sentiment
senti_score<-data.frame(colSums(senti[,]))

names(senti_score)<-"Score"
senti_score<-cbind("sentiment"=rownames(senti_score),senti_score)
rownames(senti_score)<-NULL


#plotting the sentiments with scores
ggplot(data = senti_score, aes(x = sentiment,y = Score))+geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none")+
  theme_light() +
  xlab("Sentiments") + 
  ylab("Scores") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
ggsave("senti_bar.png", width = 6, height = 4)
