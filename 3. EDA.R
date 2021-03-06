### Load libraries
library(tidyverse)
library(lubridate)
library(maps)
library(ggthemes)
library(dplyr)
library(zoo)
library(ggpubr)

#### Load datasets
tweets_sentiment <- read.csv("tweets_sentiment.csv", header = TRUE)
vaccine <- read.csv("covid_vaccine_admistrated.csv", header = TRUE)
cases <- read.csv("covid_cases.csv", header = TRUE)
vaccine$Date <- as.Date(parse_date_time(vaccine$Date, 'mdY', tz = "UTC"))
cases$submission_date <- as.Date(parse_date_time(cases$submission_date, 'mdY', tz = "UTC"))
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
  theme_map() + 
  labs(fill = "Count") +
  theme(legend.position = "right", plot.margin = unit(c(1, 1, 1, 1), "pt"))
ggsave(path = "graphs", filename = "user_dist.png", width = 6, height = 4)


### 2. mean Sentiment by state - All time
senti_state <- tweets_sentiment %>% 
  group_by(state_name) %>% 
  summarize(mean_senti = mean(sentiment)) %>% 
  left_join(us_states, by = c("state_name" = "region"))

ggplot(data = senti_state, mapping = aes(x = long, y = lat, group = group, fill = mean_senti)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  scale_fill_gradient2(limit = c(-0.6, 0.8)) +
  theme_map() + 
  labs(fill = "Sentiment") +
  theme(legend.position = "right", plot.margin = unit(c(1, 1, 1, 1), "pt"))
ggsave(path = "graphs", filename = "senti_state_all.png", width = 6, height = 4)


### 3. Time Series for Mean Sentiment Trend
tweets_sentiment %>% 
  group_by(date) %>% 
  summarize(mean_senti = mean(sentiment)) %>%
  ggplot(aes(x = date, y = mean_senti, color = mean_senti)) + 
  geom_point() +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Date", y = "Mean Sentiment", color = "Sentiment") +
  geom_smooth(span = 0.4, color = "dark green") +
  geom_vline(xintercept = as.Date("2020-12-25"), linetype="dashed", color = "black", size=1) +
  geom_label(aes(as.Date("2020-12-25"), 1.2), label = "Vaccination Start", show.legend = FALSE)
ggsave(path = "graphs", filename = "senti_time_all.png", width = 6, height = 4)


### 4. Mean Sentiment by state - Before Vaccination
senti_state_before <- before_vac %>% 
  group_by(state_name) %>% 
  summarize(mean_senti = mean(sentiment)) %>% 
  left_join(us_states, by = c("state_name" = "region"))

senti_state_bef <- ggplot(data = senti_state_before, mapping = aes(x = long, y = lat, group = group, fill = mean_senti)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  scale_fill_gradient2(limits=c(-0.6, 0.8)) +
  theme_map() + 
  labs(subtitle = "Before Mass Vaccination", fill = "Sentiment") +
  theme(legend.position = "bottom", plot.margin = unit(c(1, 1, 1, 1), "pt"))
senti_state_bef
ggsave(path = "graphs", filename = "senti_state_before.png", width = 6, height = 4)

### 5. Mean Sentiment by state - After Vaccination
senti_state_after <- after_vac %>% 
  group_by(state_name) %>% 
  summarize(mean_senti = mean(sentiment)) %>% 
  left_join(us_states, by = c("state_name" = "region"))

senti_state_af <- ggplot(data = senti_state_after, mapping = aes(x = long, y = lat, group = group, fill = mean_senti)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  scale_fill_gradient2(limits=c(-0.6, 0.8)) +
  theme_map() + 
  labs(subtitle = "After Mass Vaccination", fill = "Sentiment") +
  theme(legend.position = "bottom", plot.margin = unit(c(1, 1, 1, 1), "pt"))
senti_state_af
ggsave(path = "graphs", filename = "senti_state_after.png", width = 6, height = 4)

ggarrange(senti_state_bef, senti_state_af, ncol = 2, nrow = 1)
ggsave(path = "graphs", filename = "senti_state_b+a.png", width = 12, height = 4)

### 6. Vaccine Rates Trend
cases %>% group_by(submission_date) %>%
  summarise(case = mean(new_case)) %>%
  ggplot(aes(x = submission_date, y = case, color = case)) + 
  geom_point() +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Date", y = "New Cases", color = "Mean New Cases") +
  theme(legend.position = "bottom", plot.margin = unit(c(1, 1, 1, 1), "pt"))
ggsave(path = "graphs", filename = "case_time.png", width = 6, height = 4)

cases %>% group_by(submission_date) %>%
  summarise(case = mean(new_death)) %>%
  ggplot(aes(x = submission_date, y = case, color = case)) + 
  geom_point() +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Date", y = "New Deaths", color = "Mean New Deaths") +
  theme(legend.position = "bottom", plot.margin = unit(c(1, 1, 1, 1), "pt"))
ggsave(path = "graphs", filename = "death_time.png", width = 6, height = 4)


vaccine <- vaccine %>% group_by(Location) %>%
  arrange(Location, Date) %>%
  mutate(Admin_New = rollapply(Admin_Per_100K, 2, diff , align = 'right', fill = NA))


vaccine %>% group_by(Date) %>%
  summarise(admin = mean(Admin_New)) %>%
  ggplot(aes(x = Date, y = admin)) + 
  geom_point() +
  theme_light() +
  labs(x = "Date", y = "New Administrated Per 100K", title = "New Administrated Per 100K by Date")

ggsave("New Administrated Per 100K by Date.png")

vaccine <- subset(vaccine, select = c(Date, Location, Admin_New))
tweets_sentiment <- merge(tweets_sentiment, vaccine, by.x = c("date", "user_state"), 
                              by.y = c("Date", "Location"), all.x = TRUE)
tweets_sentiment$Admin_New[is.na(tweets_sentiment$Admin_New)] = 0

#write.csv(tweets_sentiment, "tweets_sentiment.csv", row.names = FALSE)
