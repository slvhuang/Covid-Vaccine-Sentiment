# Load the libraries
library(tidyverse)
library(lubridate)

# Load data
tweets_sentiment <- read.csv("tweets_sentiment.csv", header = TRUE)
tweets_sentiment$date <- as.Date(tweets_sentiment$date)
after_vac <- tweets_sentiment[tweets_sentiment$admin_per_100k != 0,]

# standardize variables
after_vac <- after_vac %>% select("admin_per_100k", "new_case", "new_death", "sentiment", "tot_cases", "tot_death") %>%
  mutate_all(scale)

cov(after_vac)


# Basic model
mod0 <-  lm(admin_per_100k ~ sentiment, 
            data = after_vac)
summary(mod0)



# death 
mod1 <- lm(admin_per_100k ~  new_death + sentiment, 
           data = after_vac)
summary(mod1)


mod2 <- lm(admin_per_100k ~  new_death + sentiment + tot_death, 
           data = after_vac)
summary(mod2)

mod3 <- lm(admin_per_100k ~  sentiment  + new_death*tot_death, 
           data = after_vac)
summary(mod3)

mod3a <- lm(admin_per_100k ~  sentiment*new_death*tot_death, 
           data = after_vac)
summary(mod3a)


# case
mod4 <- lm(admin_per_100k ~  new_case + sentiment,
           data = after_vac)
summary(mod4)

mod5 <- lm(admin_per_100k ~  tot_cases + sentiment + new_case, 
           data = after_vac)
summary(mod5)

mod6 <- lm(admin_per_100k ~ sentiment + tot_cases*new_case, 
           data = after_vac)
summary(mod6)

mod6a <- lm(admin_per_100k ~ sentiment*tot_cases*new_case, 
           data = after_vac)
summary(mod6a)

