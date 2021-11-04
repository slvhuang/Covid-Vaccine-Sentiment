# Load the libraries
library(tidyverse)
library(lubridate)

# Load the datasets
tweets <- read.csv("covid_vaccine_v36.csv", header = TRUE)
vaccine <- read.csv("covid_vaccine_admistrated.csv", header = TRUE)
cases <- read.csv("covid_cases.csv", header = TRUE)
cities <- read.csv("uscities.csv", header = TRUE)

## Tweets data cleaning
# Cleaning Date
# Convert chr date and time to date object
tweets <- tweets[!tweets$date == "", ]
tweets$date <- as.Date(parse_date_time(tweets$date, orders = c('dmY HM', 'Ymd HMS'), tz = "UTC"))
tweets <-  tweets[!is.na(tweets$date), ]

# Cleaning and match user_location
# remove empty user_location rows
tweets <- tweets[!tweets$user_location == "", ]
# filter and cleaning location only in US cities
# Find the 10 most frequent locations
tail(names(sort(table(tweets$user_location))), 10)

# Roughly remove rows where user_location not in US
rmv <- c("India", "United Kingdom", "England", "London", "UK", "Canada", "Australia")
for(i in 1:length(rmv)) {
  tweets <- filter(tweets, str_detect(user_location, regex(rmv[i], ignore_case = T), 
                                      negate = TRUE))
}

# get rid of all non-ASCII characters
tweets$user_location <- sapply(tweets$user_location,
                               function(row) iconv(row, "latin1", "ASCII", sub=""))
tweets <- tweets[!tweets$user_location == "", ]


# 1. Strict match location by the format City, State
tweets <- add_column(tweets, user_state = rep(NA), .after = "user_location")
format1 <- str_c(cities$city_ascii, cities$state_id, sep = ", ", collapse = NULL)
cities <- add_column(cities, format1 = format1, .after = "state_name")

for (i in 1:length(tweets$user_location)) {
  lca <- tweets$user_location[i]
  j <- match(lca, cities$format1)
  state <- cities$state_id[j]
  tweets$user_state[i] <- state
}


tweets_left <- tweets[is.na(tweets$user_state),]
tweets <- tweets[!is.na(tweets$user_state),]

# see what we get
ggplot(tweets, aes(user_state)) + geom_bar()

# 2. Strict match location by the format City,State
format2 <- str_c(cities$city_ascii, cities$state_id, sep = ",", collapse = NULL)
cities <- add_column(cities, format2 = format2, .after = "format1")

for (i in 1:length(tweets_left$user_location)) {
  lca <- tweets_left$user_location[i]
  j <- match(lca, cities$format2)
  state <- cities$state_id[j]
  tweets_left$user_state[i] <- state
}

tweets <- rbind(tweets, tweets_left[!is.na(tweets_left$user_state),])
tweets_left <- tweets_left[is.na(tweets_left$user_state),]

# see what we get now
ggplot(tweets, aes(user_state)) + geom_bar()

# 3. Strict match location by the State Name
for (i in 1:length(tweets_left$user_location)) {
  lca <- tweets_left$user_location[i]
  j <- match(lca, cities$state_name)
  state <- cities$state_id[j]
  tweets_left$user_state[i] <- state
}

tweets <- rbind(tweets, tweets_left[!is.na(tweets_left$user_state),])
tweets_left <- tweets_left[is.na(tweets_left$user_state),]

# see what we get now
ggplot(tweets, aes(user_state)) + geom_bar()

# 4. Strict match location by the format State, USA
format3 <- str_c(cities$state_name, "USA", sep = ", ", collapse = NULL)
states <- unique(cbind.data.frame(cities$state_id, cities$state_name, format3))
colnames(states) <- c("state_id", "state_name", "format3")

for (i in 1:length(tweets_left$user_location)) {
  lca <- tweets_left$user_location[i]
  j <- match(lca, states$format3)
  state <- states$state_id[j]
  tweets_left$user_state[i] <- state
}

tweets <- rbind(tweets, tweets_left[!is.na(tweets_left$user_state),])
tweets_left <- tweets_left[is.na(tweets_left$user_state),]

# see what we get now and save the filtered file
ggplot(tweets, aes(user_state)) + geom_bar()
#write.csv(tweets, "tweets_cleaned.csv", row.names=FALSE)



# Cleaning tweets text
# Load cleaned file
tweets <- read.csv("tweets_cleaned.csv", header = TRUE)
tweets$date <- as.Date(tweets$date)

# Convert all text to lower case
tweets$text <- iconv(tweets$text, "latin1", "ASCII", "")
tweets$text <- tolower(tweets$text)

# Replace @UserName
tweets$text <- gsub("@\\w+", "", tweets$text)

# Remove punctuation
tweets$text <- gsub("[[:punct:]]", "", tweets$text)

# Remove links
tweets$text <- gsub("http\\w+", "", tweets$text)

# Replace tabs
tweets$text <- gsub("[ |\t]{2,}", " ", tweets$text)

# Replace blank space
tweets$text <- gsub("\\s+"," ", tweets$text)

# Remove blank spaces at the beginning
tweets$text <- gsub("^ ", "", tweets$text)

# Remove blank spaces at the end
tweets$text <- gsub(" $", "", tweets$text)

# Drop NA rows
tweets <- tweets[!is.na(tweets$text), ]
tweets <- tweets[!tweets$text == "", ]


## Vaccine data cleaning

# Convert chr date and time to datetime
vaccine$Date <- as.Date(parse_date_time(vaccine$Date, 'mdY', tz = "UTC"))

## Cases data cleaning

# Convert chr date and time to datetime
cases$submission_date <- as.Date(parse_date_time(cases$submission_date, 'mdY', tz = "UTC"))

## select useful columns
tweets <- subset(tweets, select = c(user_name, user_location, user_state, date, 
                                    text, hashtags))

cases <- subset(cases, select = c(submission_date, state, tot_cases, new_case, tot_death, 
                                  new_death))

vaccine <- subset(vaccine, select = c(Date, Location, Admin_Per_100K))


## Combine datasets
tweets_cases <- merge(tweets, cases, by.x = c("date", "user_state"), 
                      by.y = c("submission_date", "state"))

tweets_cases_vaccine <- merge(tweets_cases, vaccine, by.x = c("date", "user_state"), 
                              by.y = c("Date", "Location"), all.x = TRUE)

#write.csv(tweets_cases_vaccine, "tweets_cases_vaccine.csv", row.names=FALSE)
