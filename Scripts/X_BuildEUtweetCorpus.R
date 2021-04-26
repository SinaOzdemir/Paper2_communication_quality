##############################################
# Project:  EU Tweet
# Task:     Build joint corpus of EU accounts
# Author:   @ChRauh (26.04.2021)
##############################################

# Packages
library(tidyverse)


# Get indvidual files ####

# Containing tweet text and metadata from individual accounts
# As harvested by Sina through the academix API

# NOTE: folter "tweecorpora" is gitignored for space reasons
# I picked the data from the respective OneDrive folder
# Adapt when finalising this !!!!

# NOTE: 117 files - correct?

files <- list.files(path = "./tweetcorpora/eutweets_14-03-2021", pattern = "*.RDS")
files <- paste0("./tweetcorpora/eutweets_14-03-2021/", files) # Add path to filename


# Joint corpus ####
# All files should be identically structured

# Target DF
corpus <- data.frame()

for (i in 1:length(files)) {
  
  # Progress
  print(i)
  
  # Read file
  current <- read_rds(files[i])
  
  # Appended to target DF
  corpus <- rbind(corpus, current)
}

rm(current) # Remove last instance 


# Look at some descriptives ####

# Accounts
# 117
length(unique(corpus$screen_name))

# Language
table(corpus$lang, useNA = "ifany")
# 285765 (87%) in english (en)
# What is 'uk' and 'eu' for example?

# Symbols present?
# Only three incidences What is measured here?
table(is.na(corpus$symbols))
symbols <- corpus %>% filter(!is.na(symbols))
# Drop var for now
corpus$symbols <- NULL

# Number of followers
# Is this measured at time of tweet?
hist(corpus$followers_count)

# Encoding issues


# Export ####
write_rds(corpus, "./tweetcorpora/EUtweets.RDS")
