### Load libraries
library(tidyverse)
library(lubridate)
library(maps)
library(ggthemes)

#### Load datasets
tweets_sentiment <- read.csv("tweets_sentiment.csv", header = TRUE)
tweets_sentiment$date <- as.Date(tweets_sentiment$date)
cities <- read.csv("uscities.csv", header = TRUE)
us_states <- map_data("state")

### Map state_id to state name
state_names <- cities %>% select(state_id, state_name) %>% distinct()
tweets_sentiment <- merge(tweets_sentiment, state_names, by.x = "user_state", by.y = "state_id")
tweets_sentiment$state_name <- tolower(tweets_sentiment$state_name)

### Split data before/after the start of vaccination
before_vac <- tweets_sentiment[tweets_sentiment$admin_per_100k == 0,]
after_vac <- tweets_sentiment[tweets_sentiment$admin_per_100k != 0,]

### 1. Distribution of tweeter users by state - All time
user_distribution <- tweets_sentiment %>% 
  group_by(state_name, user_name) %>%
  summarize(n =  n()) %>%
  summarize(count =  n()) %>%
  left_join(us_states, by = c("state_name" = "region"))

ggplot(data = user_distribution, mapping = aes(x = long, y = lat, group = group, fill = count)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  scale_fill_gradient2(low = "white", high = "#CB454A") +
  labs(title = "Tweeter Users Distribution") + 
  theme_map() + 
  labs(fill = "Count")
ggsave("Tweeter Users Distribution.png")


### 2. mean Sentiment by state - All time
senti_state <- tweets_sentiment %>% 
  group_by(state_name) %>% 
  summarize(mean_senti = mean(sentiment)) %>% 
  left_join(us_states, by = c("state_name" = "region"))

ggplot(data = senti_state, mapping = aes(x = long, y = lat, group = group, fill = mean_senti)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  labs(title = "Mean Sentiment by State") + 
  scale_fill_gradient2() +
  theme_map() + 
  labs(fill = "Sentiment")
ggsave("Mean Sentiment by State.png")


### 3. Time Series for Mean Sentiment Trend
tweets_sentiment %>% 
  group_by(date) %>% 
  summarize(mean_senti = mean(sentiment)) %>%
  ggplot(aes(x = date, y = mean_senti, color = mean_senti)) + 
  geom_point() +
  theme_light() + 
  labs(title = "Mean Sentiment by Date", x = "Date", y = "Mean Sentiment", color = "Sentiment") +
  geom_smooth(span = 0.4, color = "dark green") +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = as.Date("2020-12-25"))
ggsave("Mean Sentiment by Date.png")


### 4. Mean Sentiment by state - Before Vaccination
senti_state_before <- before_vac %>% 
  group_by(state_name) %>% 
  summarize(mean_senti = mean(sentiment)) %>% 
  left_join(us_states, by = c("state_name" = "region"))

ggplot(data = senti_state_before, mapping = aes(x = long, y = lat, group = group, fill = mean_senti)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  labs(title = "Mean Sentiment by State Before Vaccination") + 
  scale_fill_gradient2() +
  theme_map() + 
  labs(fill = "Sentiment")
ggsave("Mean Sentiment by State Before Vaccination.png")


### 5. Mean Sentiment by state - After Vaccination
senti_state_after <- after_vac %>% 
  group_by(state_name) %>% 
  summarize(mean_senti = mean(sentiment)) %>% 
  left_join(us_states, by = c("state_name" = "region"))

ggplot(data = senti_state_after, mapping = aes(x = long, y = lat, group = group, fill = mean_senti)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  labs(title = "Mean Sentiment by State After Vaccination") + 
  scale_fill_gradient2() +
  theme_map() + 
  labs(fill = "Sentiment")
ggsave("Mean Sentiment by State After Vaccination.png")



