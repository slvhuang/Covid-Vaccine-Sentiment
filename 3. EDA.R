# Load the libraries
library(tidyverse)
library(lubridate)

# Load saved dataset
tweets_sentiment <- read.csv("tweets_sentiment.csv", header = TRUE)
tweets_sentiment$date <- as.Date(tweets_sentiment$date)
summary(tweets_sentiment)

# Split data before/after the start of vaccination
before_vac <- tweets_sentiment[tweets_sentiment$admin_per_100k == 0,]
after_vac <- tweets_sentiment[tweets_sentiment$admin_per_100k != 0,]


# Distribution of tweets collected by state
ggplot(tweets_sentiment, aes(x = fct_infreq(user_state))) + 
  geom_bar() + 
  labs(title = "Tweet Users by State", x = "State", y = "Count")

# Sentiment by state before vaccination
ggplot(before_vac, aes(x = factor(user_state), y = sentiment)) + geom_boxplot()
senti_state_before <- aggregate(before_vac$sentiment, list(before_vac$user_state), mean)

# Sentiment by state after vaccination
ggplot(after_vac, aes(x = factor(user_state), y = sentiment)) + geom_boxplot()
senti_state_after <- aggregate(after_vac$sentiment, list(after_vac$user_state), mean)

ggplot(tweets_sentiment, aes(x = date, y = Admin_Per_100K)) + geom_point()
ggplot(tweets_sentiment, aes(x = date, y = Administered_Dose1_Pop_Pct, color = user_state)) + geom_line()

ggplot(before_vac, aes(x = date, y = sentiment, color = user_state)) + geom_point()
ggplot(after_vac, aes(x = Admin_Per_100K, y = sentiment)) + geom_point()

round(sqrt(var(tweets_sentiment$series_complete_pop_pct)), 2)
