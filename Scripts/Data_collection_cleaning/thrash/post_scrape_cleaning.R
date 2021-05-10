# Cleaning and reshaping the data coming from academic API 2.0
# This will be integrated to data_scraping script later on.
#most variables are list of lists
#there is no convenient way of identifying empty lists in a list
#because of hierarchical data structure, (eg.data$list_of_lists[1] is not null because it contains a list which is empty)
#recursive functions seems to be a way to solve this (https://stackoverflow.com/questions/38247954/find-empty-lists-in-nested-list-of-lists)
#but they are definitely not a one line solution either, so might as well do loop in loop across dataset.

######Tasks######:

#1) Academic API 2.0 returned data with different dimensions they need to be harmonized
#2) Extracting and flattening the important indicators from json
    #is_retweet
    #is_reply
    #mentioned screen name
    #reply_to_status_id
    #reply_to_screen_name
    #contains media
    #media type (if possible)
#################.

# setup -------------------------------------------------------------------
library(pacman)

packs<- c("tidyverse","jsonlite","plyr")
#I haven't updated my packages in a while,if you have these packages and they are up to date, ignore install = T, update = T

lapply(packs, library,character.only = T)

rdsdataDIR<- paste0(getwd(),"/Data/rds/")

files<- list.files(path = rdsdataDIR,pattern = ".RDS",full.names = T)

if(dir.exists(paste0(getwd(),"/Data/cleaned_rds"))){
  clean_rds_saveDIR<-paste0(getwd(),"/Data/cleaned_rds")
}else{
  dir.create(paste0(getwd(),"/Data/cleaned_rds"))
  clean_rds_saveDIR<-paste0(getwd(),"/Data/cleaned_rds")
}

important_variables<- readRDS(paste0(getwd(),"/dictionaries/analysis_variables.RDS"))

twitter_rds_cleaner<- function(file,read = F,save = F, saveDIR = NULL,vars_to_keep){
  #if the function will be used with an existing RDS on a local drive,
  #the function can read it now. User needs to provide full path to the file
  if(isTRUE(read)){
    twitter_data<- readRDS(file)
  }else{
    #the function assumes that it will be used with an R object in the global env.
    
    twitter_data <- file
  }
  
  
  #make the dimensions even----
  twitter_data<- twitter_data %>% jsonlite::flatten() %>% select(all_of(vars_to_keep))
  #referenced_tweets for retweets, quotes and replies----
  
  for (i in 1:nrow(twitter_data)) {
    if(is.null(twitter_data$referenced_tweets[[i]])){
      twitter_data$referenced_tweets[i]<-NA
    }
    
  }
  
  twitter_data<- twitter_data[!is.na(twitter_data$referenced_tweets),] %>%
    select(id,referenced_tweets) %>%
    group_by(id) %>%
    unnest(cols = c(referenced_tweets),names_sep = "_") %>%
    right_join(x = .,y=twitter_data, by = "id")
  
  #create is_retweet, is_quote, is_reply and reply_to_status_id for later calculations
  
  twitter_data <- twitter_data %>% mutate(is_retweet = ifelse(.$referenced_tweets_type == "retweeted",1,0),
                                          retweet_status_id = ifelse(.$referenced_tweets_type == "retweeted",.$referenced_tweets_id,NA),
                                          is_reply = ifelse(.$referenced_tweets_type == "replied_to",1,0),
                                          reply_to_status_id = ifelse(.$referenced_tweets_type == "replied_to",.$referenced_tweets_id,NA),
                                          is_quote = ifelse(.$referenced_tweets_type == "quoted",1,0),
                                          quoted_status_id = ifelse(.$referenced_tweets_type =="quoted",.$referenced_tweets_id,NA))
  
  #entities: hashtags and mentions----
  #hashtags
  twitter_data<- twitter_data %>%
    select(id, entities.hashtags) %>%
    group_by(id) %>%
    unnest(cols = c(entities.hashtags)) %>% 
    plyr::dlply(.data = . , .variables = "id",.fun = function(i)i[["tag"]]) %>% 
    tibble(id = names(.),hashtags = .) %>%
    right_join(x = .,y = twitter_data,by = "id")
  
  #mentions
  twitter_data<- twitter_data %>%
    select(id, entities.mentions) %>%
    group_by(id) %>%
    unnest(cols = c(entities.mentions)) %>% 
    plyr::dlply(.data = . , .variables = "id",.fun = function(i)i[["username"]]) %>% 
    tibble(id = names(.),mentioned_username = .) %>%
    right_join(x = .,y = twitter_data,by = "id")
  
  
  #contains media ----
  for(i in 1:nrow(twitter_data)){
    if(is.null(twitter_data$attachments.media_keys[[i]])){
      twitter_data$attachments.media_keys[i]<- NA
    }else{
      twitter_data$attachments.media_keys[i]<-twitter_data$attachments.media_keys[i]
    }
  }
  
  twitter_data$contains_media <- ifelse(is.na(twitter_data$attachments.media_keys),0,1)
  
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

for (i in 1:length(files)) {
  print(i)
  twitter_rds_cleaner(file = files[i],read = T,save = T,saveDIR = clean_rds_saveDIR,vars_to_keep = important_variables)  
}