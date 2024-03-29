#####################################################################
# Project:  EU Tweet
# Task:     Build joint corpora of scraped Twitter account timelines
# Authors:   @ChRauh, @SinaOzdemir (18.05.2021)
#####################################################################

# Packages
library(tidyverse)
library(lubridate) 
library(Hmisc)


# Corpus construction function ####

# Tweets and meta harvested through the academic track API 
# are in one file per account, one folder by ype of actor / tweet sample - currently [EU|UK|IO|TWT]

# Function appends and mildly cleans all files within one of those folders

buildTweetCorpus <- function(actor.type = character(0)) {
  
  # Establish path to data folder
  # Relative path to EUtweet toot folder
  data.path<- paste0("./data/", actor.type , "/rds_clean/")
  
  # List individual files therein 
  files <- list.files(path = data.path, pattern = "*.RDS", full.names = T,
                      recursive = T, include.dirs = T) # Include sub folders
  
  # Target DF
  corpus <- data.frame()
  
  # Load individual files and append to corpus
  for (i in 1:length(files)) {
    
    # Progress
    print(paste0("Load/append file ", i, " of ", length(files)))
    
    # Read file
    current <- read_rds(files[i])
    
    # Should column structure be partially inconsitent across files
    # add explicit dplyr::select() call here
    
    # Append to target DF
    corpus <- rbind(corpus, current)
  }
  
  # Remove last instance 
  rm(current) 
  
  # Some cleaning
  print("Cleaning appended data frame")
  
  # Variable/column structure
  corpus <- corpus %>% 
    rename(screen_name = user_username,
           userid = author_id, 
           timestamp = tweet_created_at,
           sensitive = tweet_possibly_sensitive) %>% 
    select(!starts_with("user_")) %>% # Drop all remaining static user info
    select(-tweet_referenced_tweets) %>% # basically replicates referenced_tweets_id
    relocate(screen_name, userid, tweet_id, timestamp, tweet_text, starts_with("is_"))
  
  names(corpus) <- names(corpus) %>% 
    str_remove("^tweet_") %>% 
    str_remove(fixed("public_metrics."))
  
  # Clean up some variables
  
  corpus$source <- corpus$source %>% 
    str_remove("Twitter for") %>% 
    str_remove("Twitter") %>% 
    str_remove_all(" ")
  
  corpus <- corpus %>% # Assumption correct? #yep it is
    mutate(is_retweet = as.logical(replace_na(is_retweet, 0)),
           is_reply = as.logical(replace_na(is_reply, 0)),
           is_quote = as.logical(replace_na(is_quote, 0)))
  
  # Output
  return(corpus)
  
}



# Function to collect account info ####

# Static user info on all tweets in respective folder
collectAccountInfo <- function(actor.type = character(0)) {
  
  # Establish path to data folder
  # Relative path to EUtweet toot folder
  data.path<- paste0("./data/", actor.type , "/rds_clean/")
  
  # List individual files therein 
  files <- list.files(path = data.path, pattern = "*.RDS", full.names = T,
                      recursive = T, include.dirs = T) # Include sub folders
  
  # Target DF
  accounts <- data.frame()
  
  # Load individual files and append to target DF
  for (i in 1:length(files)) {
    
    # Progress
    print(paste0("Load/append file ", i, " of ", length(files)))
    
    # Read file
    current <- read_rds(files[i])
    
    # Should column structure be partially inconsitent across files
    # add explciti dplyr::select() call here
    
    # Select static user info 
    # and follow naming conventions of corpus
    
    current <- current %>% 
      select(author_id, starts_with("user_")) %>% 
      unique() %>% 
      rename(screen_name = user_username,
             userid = author_id) %>% 
      relocate(screen_name, userid)
      
    # Append to target DF
    accounts <- rbind(accounts, current)
  }
  
  # Remove last instance 
  rm(current) 
  
  # Output
  return(accounts)
    
}



# EU tweets ####

# corp <- buildTweetCorpus("EU")
# write_rds(corp, "./data/corpii/EU_corpus.RDS")
# rm(eucorp)
# 
# accounts <- collectAccountInfo("EU")
# write_rds(accounts, "./analysis_data/EU_account_list.RDS")
# rm(euaccounts)


# IO tweets ####

# corp <- buildTweetCorpus("IO")
# write_rds(corp, "./data/corpii/IO_corpus.RDS")
# rm(corp)
# 
# accounts <- collectAccountInfo("IO")
# write_rds(accounts, "./analysis_data/IO_account_list.RDS")
# rm(accounts)


# UK tweets ####

# corp <- buildTweetCorpus("UK")
# write_rds(corp, "./data/corpii/UK_corpus.RDS")
# rm(corp)
# 
# accounts <- collectAccountInfo("UK")
# write_rds(accounts, "./analysis_data/UK_account_list.RDS")
# rm(accounts)





# Random tweets ####

# As the TWT comes from a different API with a different column and variable structure
# A special function is needed which includes manual harmonization steps

buildTweetCorpusTWT <- function(actor.type = character(0)) {
  
  # Establish path to data folder
  # Relative path to EUtweet toot folder
  data.path<- paste0("./data/", actor.type , "/rds_clean/")
  
  # List individual files therein 
  files <- list.files(path = data.path, pattern = "*.RDS", full.names = T,
                      recursive = T, include.dirs = T) # Include sub folders
  
  # Target DF
  corpus <- data.frame()
  
  # Load individual files and append to corpus
  for (i in 1:length(files)) {
    
    # Progress
    print(paste0("Load/append file ", i, " of ", length(files)))
    
    # Read file
    current <- read_rds(files[i])
    
    # Should column structure be partially inconsitent across files
    # add explicit dplyr::select() call here
    
    # Append to target DF
    corpus <- rbind(corpus, current)
  }
  
  # Remove last instance 
  rm(current) 
  
  # cleaning and harmonization
  print("Cleaning appended data frame")
  
  # Rebuild joint url object as in other API
  # eu$entities.urls[1] # Reference structure (the target)
  # names.twt[grepl("url", names.twt)] # All url variables in TWT structure
  print("Harmonizing url storage")
  
  corpus$entities.urls <- NA
  
  pb <- txtProgressBar(min = 0, max = nrow(corpus), style = 3) # Progress bar
  
  for (i in 1:nrow(corpus)) {
    
    # Collapse all urls of the twit in one list object
    urls <- c(corpus$urls_expanded_url[i], corpus$media_expanded_url[i], corpus$ext_media_expanded_url[i]) %>% 
      as.data.frame() %>% 
      t() %>% 
      as.data.frame() %>% 
      rename(expanded_url = 1) %>% 
      mutate(start = NA,
             end = NA,
             url = NA,
             display_url = NA) %>% 
      relocate(start, end, url, expanded_url, display_url) %>% 
      list()
    
    # Store in variable
    corpus$entities.urls[i] <- urls
    
    # Update progress bar
    setTxtProgressBar(pb, i)
  }
  
  print("Clean up data and harmonize structure.")
  
  # Rebuild column structure as in EU sample
  # Renaming, non_existent vars to NA
  corpus <- corpus %>% 
    rename(userid = user_id,
           timestamp = created_at,
           id = status_id,
           mentioned_username = mentions_screen_name,
           in_reply_to_user_id = reply_to_user_id,
           like_count = favourites_count,
           attachments.media_keys = media_url) %>% 
    mutate(is_reply = !is.na(reply_to_status_id),
           contains_media = !is.na(media_type),
           sensitive = NA,
           referenced_tweets_type = NA,
           referenced_tweets_id = NA,
           conversation_id = NA,
           context_annotations = NA,
           entities.mentions = NA,
           entities.hashtags = NA,
           entities.annotations = NA)
  
  # Enforce order as in EU corpus as constructed above
  corpus <- corpus %>% 
    select(screen_name, userid, id, timestamp, text, is_retweet, is_reply, is_quote, mentioned_username, 
           hashtags, referenced_tweets_type, referenced_tweets_id, conversation_id, lang, source, sensitive, context_annotations, in_reply_to_user_id, 
           entities.mentions, entities.hashtags, entities.urls, entities.annotations, retweet_count, reply_count,
           like_count, quote_count, attachments.media_keys, retweet_status_id, reply_to_status_id, quoted_status_id, contains_media)
  
  # Clean up some variables
  
  corpus$source <- corpus$source %>% 
    str_remove("Twitter for") %>% 
    str_remove("Twitter") %>% 
    str_remove_all(" ")
  
  corpus <- corpus %>% # Assumption correct? #yep it is
    mutate(is_retweet = as.logical(replace_na(is_retweet, 0)),
           is_reply = as.logical(replace_na(is_reply, 0)),
           is_quote = as.logical(replace_na(is_quote, 0)))
  
  # Output
  return(corpus)
  
}

# Execute it
corp <- buildTweetCorpusTWT("TWT")
write_rds(corp, "./data/corpii/TWT_corpus.RDS")
# rm(corp)





#@Sina: here is a faster and cleaner alternative:
#however, the resulting data is far too large for my laptop to keep it in the memory
#my preferred method of analysis is to write a function and apply it individually to 
#each rds. otherwise it takes a long time.

# CR: while the apply functions are often faster than a foor loop, here you overburden your me
# because this solution needs to hold all RDS from folder in environment at the same time
# while my loop approach only holds the target object and one current file at each point in time ...

#########Alternative data reading script#############
#rdsdataDIR<- paste0(getwd(),"/Data/rds/")
#rds_list<- list.files(path = rdsdataDIR,pattern = "*.RDS",full.names = T)
#tweet_corpus <-  lapply(rds_list,readRDS)
#However, each file needs to be same dimensions, twitter academic API rds files are not
#I am going to fix it
#####################################################







# Export account info ####
# for coding additional info manually
# accounts <- corpus %>% 
#   select(user_id ,screen_name, profile_url, profile_expanded_url) %>% 
#   unique() %>% # should be 177
#   mutate(handle = paste0("https://twitter.com/", screen_name),
#          commission = ifelse(str_detect(profile_expanded_url, fixed("ec.europa.eu")), 1, 0),
#          account_type  = 0)
# write.csv2(accounts, "./data/EUaccountsCoding.csv", row.names = F)
# rm(accounts)
# 
# 
# # Drop uneeded columns ####
# 
# # Drop variables that contain only missings
# empty <- vars %>% filter(na_share == 1) %>% select(variable)
# corpus <- corpus %>% select(-empty$variable)  
# rm(empty)
# 
# # Drop info on internal twitter urls
# corpus <- corpus %>% select(-contains("t.co"))
# 
# # Drop other unneeded info
# corpus <- corpus %>% select(-c(display_text_width, status_url, symbols,
#                                profile_banner_url, profile_background_url, profile_url,
#                                country, country_code)) %>% # Empty in 97% of cases
#   select(-contains("place")) %>%# Empty in 97% of cases
#   select(-contains("coords")) %>% # Empty in 97% of cases
#   select(-c(media_url, urls_url, ext_media_url)) # Keep only expanded urls
#   

# Clean up a little ####



# Encoding issues
# Photo / Media type
# Date


# Export corpus ####
write_rds(corpus, "./tweetcorpora/EUtweets.RDS")
