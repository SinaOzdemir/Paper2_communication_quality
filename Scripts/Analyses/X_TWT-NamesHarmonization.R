library(tidyverse)

# Get exemplary col structures
eu <- read_rds("./data/corpii/EU_corpus.RDS")
names.eu <- names(eu)
twt <- read_rds("./data/TWT/rds_clean/Austria_2021-05-13-14-08-41.RDS")
names.twt <- names(twt)


# Rebuild joint url object as in other API



# Rebuild column structure as in EU sample
# Renaming, non_existent vars to NA
corpus <- twt %>% 
  rename(userid = user_id,
         timestamp = created_at,
         id = status_id,
         mentioned_username = mentions_screen_name,
         in_reply_to_user_id = reply_to_user_id,
         entities.urls = urls_expanded_url,
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

# Enforce order as in EU sample
corpus <- corpus %>% 
  select(screen_name, userid, id, timestamp, text, is_retweet, is_reply, is_quote, mentioned_username, 
         hashtags, referenced_tweets_type, referenced_tweets_id, conversation_id, lang, source, sensitive, context_annotations, in_reply_to_user_id, 
         entities.mentions, entities.hashtags, entities.urls, entities.annotations, retweet_count, reply_count,
         like_count, quote_count, attachments.media_keys, retweet_status_id, reply_to_status_id, quoted_status_id, contains_media)
  
# Cross-Check
names(corpus) == names(eu)
