#data scraping and shaping functions:


meta_extra <- function(file_path,file_names){
  a <- readRDS(file = file_path) %>% jsonlite::flatten()
  b <- Hmisc::contents(a)[[1]] %>%
    as_tibble(rownames = "var.names") %>% 
    mutate(screen_name = rep(file_names, times = nrow(.))) %>%
    select(-any_of("NAs"))
  return(b)
}


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
