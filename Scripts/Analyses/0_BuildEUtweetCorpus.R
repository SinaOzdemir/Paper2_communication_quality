##############################################
# Project:  EU Tweet
# Task:     Build joint corpus of EU accounts
# Author:   @ChRauh (26.04.2021)
# Debugger: @SinaOzdemir (01.05.2021)
##############################################

# Packages
library(tidyverse)


# Get indvidual files ####

# Containing tweet text and metadata from individual accounts
# As harvested by Sina through the academic API

# NOTE: folter "tweecorpora" is gitignored for space reasons
# I picked the data from the respective OneDrive folder
# Adapt when finalising this !!!!

# NOTE: 117 files - correct? 
#Yep, 117 accounts because the Council of the EU and European Council merged their twitter accounts

files <- list.files(path = "./tweetcorpora/eutweets_14-03-2021", pattern = "*.RDS",full.names = T)

#files <- paste0("./tweetcorpora/eutweets_14-03-2021/", files) # Add path to filename
#no need for this

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

#@Sina: here is a faster and cleaner alternative:
#however, the resulting data is far too large for my laptop to keep it in the memory
#my preferred method of analysis is to write a function and apply it individually to 
#each rds. otherwise it takes a long time.

#########Alternative data reading script#############
#rdsdataDIR<- paste0(getwd(),"/Data/rds/")
#rds_list<- list.files(path = rdsdataDIR,pattern = "*.RDS",full.names = T)
#tweet_corpus <-  lapply(rds_list,readRDS)
#However, each file needs to be same dimensions, twitter academic API rds files are not
#I am going to fix it
#####################################################


#<<<<<<< HEAD

# Look at some descriptives ####

# All variables (and number of missings)
corpus  %>% 
  summarise_each(funs(sum(is.na(.)))) -> vars 
vars <- vars %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "variable") %>% 
  rename(nas = V1) %>% 
  mutate(na_share = nas/nrow(corpus))

=======
# Look at some descriptives ####

#>>>>>>> 54109f0f2826d999a0239385c2a3af8e4210620c
# Accounts
# 117
length(unique(corpus$screen_name))

# Language
table(corpus$lang, useNA = "ifany")
# 285765 (87%) in english (en)
# What is 'uk' and 'eu' for example?

# Symbols present?
# Only three incidences What is measured here?
#
table(is.na(corpus$symbols))
symbols <- corpus %>% filter(!is.na(symbols))
<<<<<<< HEAD# What are these?
=======#and these?
# Drop var for now
corpus$symbols <- NULL
>>>>>>> 54109f0f2826d999a0239385c2a3af8e4210620c

# Number of followers
# Is this measured at time of tweet?
hist(corpus$followers_count)

<<<<<<< HEAD
followers <- corpus %>% 
  select(screen_name, followers_count) %>% 
  group_by(screen_name) %>% 
  summarise(mean = mean(followers_count),
            min = min(followers_count),
            max = max(followers_count)) %>% 
  arrange(desc(mean))
# Seems to by fixed by screen_name
# Poor Euratom ...
# Normalise engagement indicators along this var ... ?

# Media types
mtypes <- unique(corpus$media_type)
# Only 1 or two phots in there (in 44% of cases)

# How many are 'only' retweets?
sum(corpus$is_retweet)
(sum(corpus$is_retweet)/nrow(corpus))*100 # Percent

# How many are quotes?
sum(corpus$is_quote)
(sum(corpus$is_quote)/nrow(corpus))*100 # Percent



# Analyse daily tweet volume ####

# Daily number of tweets in sample
days <- corpus %>% 
  mutate(day = str_trim(str_extract(created_at, ".*^? "))) %>% 
  select(day) %>% 
  group_by(day) %>% 
  summarise(count = n())

# Descriptives
min(days$day)  
max(days$day)
mean(days$count)

# Time series
dates <- seq(as.Date(min(days$day)), as.Date(max(days$day)), by = "days") %>% # all dates within range
  as.data.frame() %>% 
  rename(day = 1) %>% 
  mutate(day = as.character(day))

dates <- left_join(dates, days, by = "day")
dates$count[is.na(dates$count)] <- 0

breaks <- dates %>%  # Breaks for x-axis
  filter(str_detect(day, "01-01$")) %>% # 1st January
  select(day)
breaks <- as.character(breaks[,1])
labels <- str_remove(breaks, "-.*$") %>% unique() # Year only

ggplot(dates, aes(x= day, y= count)) + 
  geom_col()+
  scale_x_discrete(breaks = breaks, labels = labels)+
  labs(title = "Daily volume of tweets in EUtweet sample",
       subtitle = "117 verified accounts of supranational persons, offices, and institutions",
       x = "\nDay",
       y = "Number of tweets\n")+
  theme_bw()+
  theme()

ggsave("./plots/TweetVolumeDaily.png", width = 16, height = 10, units = "cm")

# Weekday
old.setting <- Sys.getlocale("LC_TIME") # Store current time setting of locale
Sys.setlocale("LC_TIME","English_United States.1252") # switch to English
wdays <- dates %>% 
  mutate(weekday = weekdays(as.Date(day))) %>% 
  group_by(weekday) %>% 
  summarise(mean = mean(count))
Sys.setlocale("LC_TIME", old.setting) # Reinstate old LC Time setting
wdays$weekday <- factor(wdays$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(wdays, aes(x= mean, y= fct_rev(weekday))) + 
  geom_col()+
  labs(title = "Tweets per weekday in EUtweet sample",
       subtitle = "117 verified accounts of supranational persons, offices, and institutions",
       x = "\nAverage number of tweets\n(2009-03-13/2021-03-14) ",
       y = "")+
  theme_bw()+
  theme()

ggsave("./plots/TweetVolumeWeekday.png", width = 16, height = 10, units = "cm")


# Export account info ####
# for coding additional info manually
accounts <- corpus %>% 
  select(user_id ,screen_name, profile_url, profile_expanded_url) %>% 
  unique() %>% # should be 177
  mutate(handle = paste0("https://twitter.com/", screen_name),
         commission = ifelse(str_detect(profile_expanded_url, fixed("ec.europa.eu")), 1, 0),
         account_type  = 0)
write.csv2(accounts, "./data/EUaccountsCoding.csv", row.names = F)
rm(accounts)


# Drop uneeded columns ####

# Drop variables that contain only missings
empty <- vars %>% filter(na_share == 1) %>% select(variable)
corpus <- corpus %>% select(-empty$variable)  
rm(empty)

# Drop info on internal twitter urls
corpus <- corpus %>% select(-contains("t.co"))

# Drop other unneeded info
corpus <- corpus %>% select(-c(display_text_width, status_url, symbols,
                               profile_banner_url, profile_background_url, profile_url,
                               country, country_code)) %>% # Empty in 97% of cases
  select(-contains("place")) %>%# Empty in 97% of cases
  select(-contains("coords")) %>% # Empty in 97% of cases
  select(-c(media_url, urls_url, ext_media_url)) # Keep only expanded urls
  

# Clean up a little ####

corpus$source <- corpus$source %>% 
  str_remove("Twitter for") %>% 
  str_remove("Twitter") %>% 
  str_remove_all(" ")

# Encoding issues
# Photo / Media type
# Date


# Export corpus ####
write_rds(corpus, "./tweetcorpora/EUtweets.RDS")
