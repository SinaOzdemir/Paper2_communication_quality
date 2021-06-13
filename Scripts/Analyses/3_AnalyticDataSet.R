########################################################
# Project:    EU Tweet
# Task:       Combined analytical data set of Tweet data
# Author:     Christian Rauh (31.05.2021)
########################################################


# Packages #####
library(tidyverse)


# Function to harmonize analytic data sets ####
# Includes extraction of info on external urls and media (takes time!)

# Expects data frame put out by 1_TextCleaning.R

harmonizeAnalytics <- function(corpus = data.frame(0)) {
  
  # Variable preparation #
  print("Extracting count indicators from Tweet data.")
  
  # Day of tweet
  corpus$day <- corpus$timestamp %>% 
    str_extract("^.*?T") %>% 
    str_remove("T") %>% 
    as.Date(format = "%Y-%m-%d")
  
  # Number of mentions
  corpus$nmentions <- ifelse(corpus$mentioned_username == "NULL" , 0,
                             str_count(as.character(corpus$mentioned_username), ",")+1)
  
  # Number of hashtags
  corpus$nhashtags <- ifelse(corpus$hashtags == "NULL" , 0,
                             str_count(as.character(corpus$hashtags), ",")+1)
  
  # Number of languages
  corpus$nlang <- str_count(corpus$tweetlanguages, ",") + 1
  
  
  # External urls and media #
  print("Extracting urls and media types from tweet data. This takes some time!")
  
  # Target variables
  corpus$urls <- NA
  corpus$mediaurls <- NA
  corpus$exturls <- NA
  
  # create progress bar
  pb <- txtProgressBar(min = 0, max = nrow(corpus), style = 3)
  
  # Loop over obs and extract urls
  for (i in 1:nrow(corpus)) {
    
    # Check whether the tweet data contain url entity information 
    # (first element of respective list must contain something),
    # jump to next iteration if not
    
    if(length(corpus$entities.urls[i][[1]]) == 0) {next}
    
    # Urls stored for each obs
    urls <- corpus$entities.urls[i][[1]]
    
    # Store urls as charater string, separate by ', '
    corpus$urls[i] <- paste(unique(urls$expanded_url), collapse = ", ")
    
    # Only URLS refering to media stored on dedicated twitter servers #
    # CHECK TWIMG!!!
    media <- paste(unique(urls$expanded_url[str_detect(urls$display_url, "(pic\\.twitter\\.com)|(twimg)")]),
                   collapse = ", ")
    corpus$mediaurls[i] <- ifelse(length(media) > 0 , media, NA)
    
    # Only URLS not referring to media stored on dedicated twitter servers
    ext <- paste(unique(urls$expanded_url[!str_detect(urls$display_url, "(pic\\.twitter\\.com)|(twimg)")]),
                 collapse = ", ")
    corpus$exturls[i] <- ifelse(length(ext) > 0 , ext, NA)
    
    # update progress bar
    setTxtProgressBar(pb, i)
    
  }
  
  
  # Count media urls 
  print("Counting external urls and media types.")
  
  corpus$nphotos <- str_count(corpus$mediaurls, "(/photo/)|(pbs\\.twimg\\.com/media/)")
  corpus$nvideos <- str_count(corpus$mediaurls, "(/video/)|(pbs\\.twimg\\.com/tweet_video/)|(amp\\.twimg\\.com/v/)")
  
  # Note: If nvideos > 1 it is often a recyled video posted earlier on twitter - use as-logical() to be safe
  # For pictures it seems to be similar 
  # For true number, different scraping (extended entites would be required)
  
  # hist(corpus$nphotos)
  # hist(corpus$nvideos)
  # test <- corpus %>% filter(nphotos > 1)
  
  # Note: All of this may work more simply with the prefixes of the media_keys 
  # (where 3_ seems to refer to pics, and 7_ to videos) but I didn't find proper documentation
  
  # Embedded videos from other plattforms
  corpus$ntube <- str_count(corpus$exturls, "(youtube)|(vimeo)|(tiktok)|(twitch)")
  # test <- corpus %>% filter(str_detect(exturls, "twitch"))
  
  # Other external urls
  corpus$nexturl <- (str_count(corpus$exturls, ",") + 1) - corpus$ntube
  
  
  
  # Variable to store whether english text is available
  print("Finalizing")
  corpus$en_av <- nchar(corpus$texten) > 1
  # sum(corpus$en_av)
  
  
  # Select analytic variables #
  corpus <- corpus %>% 
    select(c(id, screen_name, userid, timestamp, day,               # Identifiers
             is_retweet, is_reply, is_quote, en_av,                 # Tweet types
             like_count, retweet_count, reply_count, quote_count,   # User engagement
             contains_media, nphotos, nvideos, ntube, emojicount,   # Media
             nmentions, nhashtags, nexturl,                         # Interlangae indicators
             nlang, in_reply_to_user_id))                           # Number of languages, interaction with other users
  
}




# EU Tweet sample ####

# Extract analytic data
eu <- harmonizeAnalytics(read_rds("./data/corpii/EU_corpus_cleaned.RDS"))

# Text indicators
tindicators <- read_rds("./data/corpii/EU_corpus_TextIndicators.RDS")

# Combine with analytic data
eu <- eu %>% left_join(tindicators, by = "id")

# Mark sample
# For the EU case, distinguishing personal and inst. accounts
# Along Serra's coding here

# Raw account classification (by Serra Erkenci)
accounts <- read.csv2("./data/EUaccountsCoding_Serra.csv") %>%
  select(screen_name, account_type)
accounts$account_type <- accounts$account_type %>%
  recode(`3` = 2L) %>% # Subsume office accounts into institional accounts (effectively Ombudsman and eucopresident)
  recode(`1` = "Personal Accounts",
         `2` = "Institutional Accounts") %>%
  factor(levels = c("Personal Accounts", "Institutional Accounts"))

eu <- eu %>% left_join(accounts, by = "screen_name")

eu$tweetsample <- ifelse(eu$account_type == "Personal Accounts",
                         "EU (pers. account)",
                         "EU (inst. account)")

eu$account_type <- NULL




# IO Tweet sample ####

# Extract analytic data
io <- harmonizeAnalytics(read_rds("./data/corpii/IO_corpus_cleaned.RDS"))

# Text indicators
tindicators <- read_rds("./data/corpii/IO_corpus_TextIndicators.RDS")

# Combine with analytic data
io <- io %>% left_join(tindicators, by = "id")

# Mark sample
io$tweetsample <- "IO"




# UK Tweet sample ####

# Extract analytic data
# uk <- harmonizeAnalytics(read_rds("./data/corpii/UK_corpus_cleaned.RDS") %>% sample_n(100)) # Sample
uk <- harmonizeAnalytics(read_rds("./data/corpii/UK_corpus_cleaned.RDS"))

# Text indicators
tindicators <- read_rds("./data/corpii/UK_corpus_TextIndicators.RDS")

# Combine with analytic data
uk <- uk %>% left_join(tindicators, by = "id")


# Add account info
accounts <- read.csv2("./data/uk_accounts_CRedit.csv") %>% 
  mutate(twitter_handle = str_trim(twitter_handle)) %>% 
  rename(screen_name = twitter_handle) %>% 
  select(screen_name, account_type, institutional_affiliation)

uk <- uk %>%  left_join(accounts, by = "screen_name")

# Filter out parliamentary accounts
uk <- uk %>% filter(!str_detect(institutional_affiliation, "House of "))

# Mark sample
uk$tweetsample <- ifelse(uk$account_type == "individual",
                         "UK (pers. account)",
                         "UK (inst. account)")

uk$account_type <- NULL
uk$institutional_affiliation <- NULL



# TWT sample ####

# Extract analytic data
twt <- harmonizeAnalytics(read_rds("./data/corpii/TWT_corpus_cleaned.RDS"))

# Text indicators
tindicators <- read_rds("./data/corpii/TWT_corpus_TextIndicators.RDS")

# Combine with analytic data
twt <- twt %>% left_join(tindicators, by = "id")

# Mark sample
twt$tweetsample <- "Random Tweets"



# Combine and export analytic data set ####

# df <- rbind(eu, io)
# df <- rbind(eu, io, uk, twt)
df <- rbind(eu, io, twt)

write_rds(df, "./data/AnalyticData_AllSamples.RDS")
