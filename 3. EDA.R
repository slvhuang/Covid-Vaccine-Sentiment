# Load the libraries
library(tidyverse)
library(lubridate)
library(maps)
library(ggthemes)


# Load saved dataset
tweets_sentiment <- read.csv("tweets_sentiment.csv", header = TRUE)
tweets_sentiment$date <- as.Date(tweets_sentiment$date)
summary(tweets_sentiment)
cities <- read.csv("uscities.csv", header = TRUE)

us_states <- map_data("state")
state_names <- cities %>% select(state_id, state_name) %>% distinct()
  
tweets_sentiment <- merge(tweets_sentiment, state_names, by.x = "user_state", 
                          by.y = "state_id")

tweets_sentiment$state_name <- tolower(tweets_sentiment$state_name)


# Split data before/after the start of vaccination
before_vac <- tweets_sentiment[tweets_sentiment$admin_per_100k == 0,]
after_vac <- tweets_sentiment[tweets_sentiment$admin_per_100k != 0,]


# Distribution of tweets collected by state
ggplot(tweets_sentiment, aes(x = fct_infreq(user_state))) + 
  geom_bar() + 
  labs(title = "Tweets Before Vaccination by State ", x = "State", y = "Count") +
  coord_flip() +
  theme_light()

# Sentiment by state before vaccination
ggplot(before_vac, aes(x = reorder(factor(user_state), sentiment, FUN = median), y = sentiment)) + 
  geom_boxplot() +
  coord_flip() +
  theme_light() +
  labs(title = "Sentiments Before Vaccination by State ", x = "State", y = "Count")
  
senti_state_before <- before_vac %>% 
  group_by(user_state) %>%
  summarize(mean_sentiment = mean(sentiment),
            n =  n()) %>%
  merge(state_names, by.x = "user_state", 
        by.y = "state_id")

senti_state_before$state_name <- tolower(senti_state_before$state_name)

before_vac %>% 
  group_by(date) %>%
  summarize(mean_sentiment = mean(sentiment), n =  n()) %>%
  ggplot(aes(x = date, y = mean_sentiment)) + 
  geom_point() +
  geom_smooth(span = 0.4)

us_senti_before <- left_join(senti_state_before, us_states, by = c("state_name" = "region"))

ggplot(data = us_senti_before,
       mapping = aes(x = long, y = lat, group = group, fill = mean_sentiment)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  scale_fill_gradient2() +
  labs(title = "Sentiment Before Vaccination") + 
  theme_map() + 
  labs(fill = "Sentiment")

ggsave("sentiment_before_vaccination.png")

# Sentiment by state after vaccination
ggplot(after_vac, aes(x = reorder(factor(user_state), sentiment, FUN = median), y = sentiment)) + 
  geom_boxplot() +
  coord_flip() +
  theme_light() +
  labs(title = "Sentiments After Vaccination by State ", x = "State", y = "Count")

senti_state_after <- after_vac %>% 
  group_by(user_state) %>%
  summarize(mean_sentiment = mean(sentiment),
            n =  n()) %>%
  merge(state_names, by.x = "user_state", 
        by.y = "state_id")

senti_state_after$state_name <- tolower(senti_state_after$state_name)

after_vac %>% 
  group_by(date) %>%
  summarize(mean_sentiment = mean(sentiment), n =  n()) %>%
  ggplot(aes(x = date, y = mean_sentiment)) + 
  geom_point() + 
  geom_smooth(span = 0.4)

us_senti_after <- left_join(senti_state_after, us_states, by = c("state_name" = "region"))

ggplot(data = us_senti_after,
       mapping = aes(x = long, y = lat, group = group, fill = mean_sentiment)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  scale_fill_gradient2() +
  labs(title = "Sentiment After Vaccination") + 
  theme_map() + 
  labs(fill = "Sentiment")

ggsave("sentiment_after_vaccination.png")

ggplot(tweets_sentiment, aes(x = date, y = admin_per_100k)) + geom_point()

ggplot(before_vac, aes(x = date, y = sentiment, color = user_state)) + geom_point()

ggplot(after_vac, aes(x = admin_per_100k, y = sentiment)) + geom_point()


