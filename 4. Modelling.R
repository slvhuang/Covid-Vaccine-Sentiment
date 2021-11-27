# Load the libraries
library(tidyverse)
library(lubridate)

# Load the data
mod1 <- lm(admin_per_100k ~ date + sentiment + new_case + new_death + tot_cases + tot_death, 
           data = after_vac)
summary(mod1)
