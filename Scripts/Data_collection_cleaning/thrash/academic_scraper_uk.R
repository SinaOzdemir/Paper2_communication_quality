#data scraper (to be used wih academic twitter access v2.0)
##UK accounts need to be refined and validated
devtools::install_github("cjbarrie/academictwitteR")

library(academictwitteR)
library(tidyverse)
bearer_token <- "AAAAAAAAAAAAAAAAAAAAANh3NgEAAAAAYpb57mP0saX5D%2BCi4vfCvf%2FWp6Q%3DD6xFxcFGBkLGz0BWs6f4EylxpdxEeLIpLk867py48blqPVwQUm"
data_path<-paste0(getwd(),"/data/")

# Identify verified UK accounts -------------------------------------------

uk_handles<- metadata %>% pull(handle) %>% gsub("@","",.)

library(rtweet)

v_1_token<-rtweet::create_token(app = "Functionised_collector",
                                consumer_key = "c0wODdFRKzNDz23l7O9A6GBig",
                                consumer_secret = "uwraekdxEX2BaVZSNLw7sjkVbq3wNRByYYiOolHrYaikqZHKUW",
                                access_token = "1151438784384421888-eas7PxAdeoohUCykI5XeX2Wc2lppjl",
                                access_secret = "96TaBIVuZXlpICK2QAP7IurCFuYPfHSUbXYUTTSRak3gr",
                                set_renv = F)

uk_prof_info<- rtweet::lookup_users(users = uk_handles,parse = T,token = v_1_token)

uk_missing<- metadata %>%
  mutate(screen_name = gsub("@","",handle)) %>%
  filter(!screen_name%in%uk_prof_info$screen_name)
#API didn't return a value for these accounts. 
#I manually checked a random 10 handles, accounts exist under a different handle
#For example UKTIITALIA = UKTIItalia, FCOtravel = fcotravel,PHammondMP = PhilipHammondUK.
# This is a normal phenomena, I had to manually validate EU accounts every month and update
# the list while I was collecting my phd data. First I will scrape the data for available accounts
# then if need be I'll valida and scrape these accounts as well
table(uk_prof_info$protected,uk_prof_info$verified,dnn = c("protected","verified"))
#oof, only 285 of the handles are verified and not protected
#as opposed to 530 public accounts
#and 286 verified accounts...

uk_accounts_valid<- uk_prof_info %>% filter(verified == T & protected == F)
saveRDS(uk_accounts_valid,file = paste0(data_path,"/Accounts/UK_accounts_unverified.RDS"))
#accounts also seems all over the place. The dataset includes twitter accounts of 
#ambassadors, government departments, MPs , government members, embassies, local agencies,
#various government branches (ministry of defence) and sub-departments of ministries (Defence Infrastructure Organisation)
#Nice breadth, I am not sure how much of it would be noise...

# scrape the tweets ------------------------------------------------
#amele işi indirme...

#first scrape tweets:

accounts<- uk_accounts_valid$screen_name
accounts<- accounts[which(grepl(pattern = "jp_British|JoannaCrellin",x = accounts))]

for (i in 1:length(accounts)) {
  #get_creation_date
  creation_date<- uk_accounts_valid %>%
    filter(screen_name == accounts[i]) %>%
    pull(account_created_at) %>%
    as.character() %>%
    str_split(pattern = " ")
  #formulate the start date
  start_date<-  paste0(creation_date[[1]][1],"T",creation_date[[1]][2],"Z")
  #formulate the end date
  end_date <-  paste0(Sys.Date(),"T00:00:00Z")
  #formulate the json save directory
  
  if(dir.exists(paste0(getwd(),"/data/UK/json/"))){
    data.path<-paste0(getwd(),"/data/UK/json/",accounts[i],"/")}
  else{
    dir.create(paste0(getwd(),"/data/UK/json/"))
    data.path<-paste0(getwd(),"/data/UK/json/",accounts[i],"/")
    }
  
  #get the tweets and users information
  
  
  tweets<- academictwitteR::get_user_tweets(users = accounts[i],
                                            start_tweets = start_date,
                                            end_tweets = end_date,
                                            bearer_token = bearer_token,
                                            bind_tweets = F,
                                            data_path = data.path)
  
    
}


# bind user data and tweet data together in RDS -------------------------------------------------------------------


for (i in 1:length(accounts)){
  print(accounts[i])
 #some accounts don't exist anymore, so the loop throws an error
  #I was going home before I ran this script and I didn't want it to break down
  #when I was away, thats why I used try(,silent = T)
  data.path<-paste0(getwd(),"/data/UK/json/",accounts[i],"/")
  
  tweet_data <- academictwitteR::bind_tweet_jsons(data.path)
  
  tweet_vars<-colnames(tweet_data)[-which(colnames(tweet_data)%in%"author_id")]
  
  colnames(tweet_data)[-which(colnames(tweet_data)%in%"author_id")]<- paste("tweet",tweet_vars,sep = "_")
  
  user<- tweet_data$author_id[1]
  
  user_data <- academictwitteR::bind_user_jsons(data.path) %>%
    filter(id == user) %>%
    rename(author_id = id) %>%
    sample_n(size = 1)
  
  user_vars<- colnames(user_data)[-which(colnames(user_data)%in%"author_id")]
  
  colnames(user_data)[-which(colnames(user_data)%in%"author_id")]<- paste("user",user_vars,sep = "_")
  
  tweet_data<- left_join(tweet_data,user_data,by = "author_id")
  
  if(dir.exists(paste0(getwd(),"/data/UK/rds"))){
  
    saveRDS(tweet_data, file = paste0(getwd(),"/data/UK/rds/tweets_and_prof_info_",accounts[i],".RDS"))
  
    }else{
    
      dir.create(paste0(getwd(),"/data/UK/rds"))
    
      saveRDS(tweet_data, file = paste0(getwd(),"/data/UK/rds/tweets_and_prof_info_",accounts[i],".RDS"))
  }}

# cleaning dfs: ---------------------------------------------------------------
rds.path<- paste0(getwd(),"/data/UK/rds/")
rds.files<- list.files(rds.path,"*.RDS",full.names = T)
rds.file.names<- list.files(rds.path,"*.RDS") %>% gsub("tweets_and_prof_info|.RDS","",x = .)

meta_extra <- function(file_path,file_names){
  a <- readRDS(file = file_path) %>% jsonlite::flatten()
  b <- Hmisc::contents(a)[[1]] %>%
    as_tibble(rownames = "var.names") %>% 
    mutate(screen_name = rep(file_names, times = nrow(.))) %>%
    select(-any_of("NAs"))
  return(b)
}


ncol_meta_data<- map2(.x = rds.files, .y = rds.file.names, .f = meta_extra)

ncol_meta_data_df<- do.call("rbind",ncol_meta_data)

#lets see which columns are problematic:
#if all the columns appear in equal number (i.e all the data is same dimension)
#their count number should be the same

prob_variables<-ncol_meta_data_df %>%
  group_by(var.names) %>%
  summarise(col_count = n()) %>%
  filter(col_count < max(.$col_count)) %>% 
  pull(var.names)

#some of the important variables like hashtags are missing in some dataset
#Maybe they have actually never used hashtags

twitter_rds_cleaner<- function(file,read = F,save = F,dim_even = F,prob_dim,saveDIR = NULL){
  #if the function will be used with an existing RDS on a local drive,
  #the function can read it now. User needs to provide full path to the file
  if(isTRUE(read)){
    twitter_data<- readRDS(file) %>% jsonlite::flatten()
  }else{
    #the function assumes that it will be used with an R object in the global env.
    
    twitter_data <- file
  }
  
  
  #make the dimensions even----
  #this changes rest of the function a little.
  if(isTRUE(dim_even)){
    #this simply removes uncommon columns
    missing<- ifelse(prob_variables%in%colnames(twitter_data),1,0)
    
    missing_cols<-cbind(prob_variables,missing) %>%
      as.data.frame() %>%
      filter(missing == 0 ) %>%
      pull(prob_variables)
    
    twitter_data[missing_cols]<-NA
  }
  
  #referenced_tweets for retweets, quotes and replies----
  if(isTRUE("tweet_referenced_tweets"%in%colnames(twitter_data)) && isFALSE(all(is.na(twitter_data$tweet_referenced_tweets)))){
    for (i in 1:nrow(twitter_data)) {
      if(length(twitter_data$tweet_referenced_tweets[[i]])==0){
        twitter_data$tweet_referenced_tweets[i]<-NA
      }
      
    }
    
    
    twitter_data<- twitter_data[!is.na(twitter_data$tweet_referenced_tweets),] %>%
      select(tweet_id,tweet_referenced_tweets) %>%
      group_by(tweet_id) %>%
      unnest(cols = c(tweet_referenced_tweets),names_sep = "_") %>%
      right_join(x = .,y=twitter_data, by = "tweet_id")
  }else{
    missing_reference_vars<-c("tweet_referenced_tweets_type","tweet_referenced_tweets_id")
    twitter_data[missing_reference_vars]<-NA
  }
  #create is_retweet, is_quote, is_reply and reply_to_status_id for later calculations
  #here it breaks for some reason
  
  twitter_data$is_retweet = ifelse(twitter_data$tweet_referenced_tweets_type == "retweeted",1,0)
  twitter_data$retweet_status_id = ifelse(twitter_data$tweet_referenced_tweets_type == "retweeted",twitter_data$tweet_referenced_tweets_id,NA)
  twitter_data$is_reply = ifelse(twitter_data$tweet_referenced_tweets_type == "replied_to",1,0)
  twitter_data$reply_to_status_id = ifelse(twitter_data$tweet_referenced_tweets_type == "replied_to",twitter_data$tweet_referenced_tweets_id,NA)
  twitter_data$is_quote = ifelse(twitter_data$tweet_referenced_tweets_type == "quoted",1,0)
  twitter_data$quoted_status_id = ifelse(twitter_data$tweet_referenced_tweets_type =="quoted",twitter_data$tweet_referenced_tweets_id,NA)
  
  #entities: hashtags and mentions----
  if(isTRUE("tweet_entities.hashtags"%in%colnames(twitter_data)) && isFALSE(all(is.na(twitter_data$tweet_entities.hashtags)))){
    #hashtags
    twitter_data<- twitter_data %>%
      select(tweet_id, tweet_entities.hashtags) %>%
      group_by(tweet_id) %>%
      unnest(cols = c(tweet_entities.hashtags)) %>% 
      plyr::dlply(.data = . , .variables = "tweet_id",.fun = function(i)i[["tag"]]) %>% 
      tibble(tweet_id = names(.),tweet_hashtags = .) %>%
      right_join(x = .,y = twitter_data,by = "tweet_id")
  }else{
    twitter_data["tweet_hashtags"]<-NA
  }
  #mentions
  
  if(isTRUE("tweet_entities.mentions"%in%colnames(twitter_data)) && isFALSE(all(is.na(twitter_data$tweet_entities.mentions)))){
    twitter_data<- twitter_data %>%
      select(tweet_id, tweet_entities.mentions) %>%
      group_by(tweet_id) %>%
      unnest(cols = c(tweet_entities.mentions)) %>% 
      plyr::dlply(.data = . , .variables = "tweet_id",.fun = function(i)i[["username"]]) %>% 
      tibble(tweet_id = names(.),tweet_mentioned_username = .) %>%
      right_join(x = .,y = twitter_data,by = "tweet_id")
  }else{
    twitter_data["tweet_mentioned_username"]<-NA
  }
  
  #contains media ----
  
  if(isTRUE("tweet_attachments.media_keys"%in%colnames(twitter_data)) && isFALSE(all(is.na(twitter_data$tweet_attachments.media_keys)))){
    for(i in 1:nrow(twitter_data)){
      if(length(twitter_data$tweet_attachments.media_keys[[i]]) == 0){
        twitter_data$tweet_attachments.media_keys[i]<- NA
      }
    }
    
    twitter_data$contains_media <- ifelse(is.na(twitter_data$tweet_attachments.media_keys),0,1)
  }else{
    twitter_data["contains_media"]<-NA
  }
  #save modified rds----
  if(isTRUE(save)){
    #User needs to provide a save directory if 
    #modified dataset needs to be saved
    #!!!!IMPORTANT!!!
    #The directory should not end with /
    #otherwise the function will not save properly.
    user_name<- twitter_data$author_id[1]
    saveRDS(twitter_data,file = paste0(saveDIR,"/",user_name,".RDS"))
  }else{
    return(twitter_data)
  }
  #finally return the modified dataset.
  
}

#run the function on all data
if(dir.exists(paste0(getwd(),"/data/UK/rds_clean"))){
clean_rds_path<- paste0(getwd(),"/data/UK/rds_clean")}else{
  dir.create(paste0(getwd(),"/data/UK/rds_clean"))
  clean_rds_path<- paste0(getwd(),"/data/UK/rds_clean")
}

for (i in 1:length(rds.files)){
  twitter_rds_cleaner(file = rds.files[i],
                      read = T,
                      save = T,
                      dim_even = T,
                      prob_dim = prob_variables,
                      saveDIR = clean_rds_path)
}

#create a single corpus out of clean rds files.
#Otherwise the data is too big to read into memory
analysis_vars<- c("tweet_id",
                  "is_retweet",
                  "retweet_status_id",
                  "is_reply",
                  "reply_to_status_id",
                  "is_quote",
                  "quoted_status_id",
                  "contains_media",
                  "tweet_mentioned_username",
                  "tweet_hashtags",
                  "tweet_created_at",
                  "tweet_text",
                  "tweet_lang",
                  "tweet_public_metrics.retweet_count",
                  "tweet_public_metrics.reply_count",
                  "tweet_public_metrics.like_count",
                  "tweet_public_metrics.quote_count",
                  "author_id",
                  "user_username",
                  "user_name",
                  "user_description",
                  "user_created_at",
                  "user_public_metrics.followers_count",
                  "user_public_metrics.following_count",
                  "user_public_metrics.tweet_count",
                  "user_public_metrics.listed_count")

#no need for this anymore
twitter_rds_reader<- function(file_path, analysis_vars){
  data<-readRDS(file_path)
  #choose the important variables and
  #make sure that everything is the same class
  #I shouldn't have done this... only certain things should be character
  data <- data %>%
    select(any_of(analysis_vars)) %>%
    mutate(across(everything(),~as.character(.x)))
  
  #recode nas in binaries
  na_vars<- Hmisc::contents(object = data)[[1]] %>%
    as_tibble(rownames = "var_names") %>% 
    filter(NAs > 0) %>% 
    pull(var_names) %>%
    grep(pattern = "is_*",x = .,perl = T,value = T)
  
  data<-data %>% mutate(across(.cols = all_of(na_vars), ~replace_na(.x,0))) %>%
    mutate(tweet_date = lubridate::as_date(.$tweet_created_at)) %>% 
    mutate(tweet_year = lubridate::floor_date(tweet_date,unit = "year"),
           tweet_month = lubridate::floor_date(tweet_date, unit = "month"),
           tweet_day = lubridate::floor_date(tweet_date, unit = "day"),
           tweet_calendar_date = lubridate::wday(tweet_date,label = T))
  return(data)
}

tweet_corpus <-  map_dfr(.x = clean_rds_path,.f = twitter_rds_reader,analysis_vars = analysis_vars)
#save the data

if(dir.exists(paste0(data_path,"corpii"))){
  saveRDS(tweet_corpus,file = paste0(data_path,"corpii/","UK_twitter_corpus.RDS"))
}else{
  dir.create(paste0(data_path,"corpii"))
  saveRDS(tweet_corpus,file = paste0(data_path,"corpii/","UK_twitter_corpus.RDS"))
}

