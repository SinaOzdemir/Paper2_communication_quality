#data scraping and shaping functions:

# packages: ---------------------------------------------------------------

packs<- c("tidyverse","Hmisc","rtweet","academictwitteR","jsonlite")
existing_packs<-installed.packages()[,1]

package_loader<- function(packs = character(0)){
  existing_packs<- installed.packages()[,1]
  if(all(packs%in%existing_packs)){
    lapply(packs,library, character.only = T)
  }else{
    missing_packages<- packs[-which(packs%in%existing_packs)]
    install.packages(missing_packages)
    lapply(packs,library(character.only = T))
    }
  
}


package_loader(packs = packs)

# helper functions --------------------------------------------------------

make.dir<- function(file.path){
  if(dir.exists(file.path)){
    return(file.path)
  }else{
    dir.create(path = file.path, recursive = T)
    return(file.path)
  }
}

meta_extra <- function(file_path,file_names){
  a <- readRDS(file = file_path) %>% jsonlite::flatten()
  b <- Hmisc::contents(a)[[1]] %>%
    as_tibble(rownames = "var.names") %>% 
    mutate(screen_name = rep(file_names, times = nrow(.))) %>%
    select(-any_of("NAs"))
  return(b)
}


# twitter scrapers --------------------------------------------------------

twitter_json_scraper<- function(accounts,#provide account handles
                                token_v1,#twitter API v1.0 token to look up account information,necessary if account_look_up = T
                                bearer_token,#twitter API v2.0 token to scrape tweets
                                account_look_up = F,#if accounts is only names, lookup meta data on accounts
                                data.folder = character(0),#the folder to store the data
                                case = c("EU","UK","IO","TWT"),#the case name (EU,IO,UK, or TWT),user must choose one
                                stream_days = numeric(0),#if the case is chosen as TWT, user needs to specify how long the streaming will go on as number of days
                                starting_time = "2021-05-13 11:53:20 CEST"){
                                #provide a starting date in character format for streaming
  
  
  
      
  #if the user only provided twitter handles,
  #this chunk looks up meta information on the account
  #meta-information account creation date is necessary
  #for the rest of the code to work
  if(case == "TWT"){
    
    eu_countries <- read_html("https://europa.eu/european-union/about-eu/countries_en") %>%
      rvest::html_elements("#year-entry2 a") %>% html_text2()
    eu_countries_df <- tibble(country_name = eu_countries,
                              country_code = countrycode::countrycode(eu_countries,origin = "country.name",destination = "iso2c"))
    
    
    country_coords<-jsonlite::fromJSON(txt = "https://raw.githubusercontent.com/sandstrom/country-bounding-boxes/master/bounding-boxes.json")
    
    country_bbox<- do.call("rbind",country_coords) %>%
      as.data.frame %>%
      mutate(country_code = rownames(.)) %>%
      rename(country_name = V1, bbox = V2) %>% 
      mutate(country_code = as.vector(country_code, mode = "character"),
             country_name = as.vector(country_name, mode = "character"))
    
    eu_bbox<- left_join(x = eu_countries_df,country_bbox, by = "country_code") %>%
      drop_na()
    
    repeat{
      
      for (i in 1:nrow(eu_bbox)) {
        
        country_name<-eu_bbox$country_name.x[i]
        rl<-rate_limits(token = v_1_token)
        print(paste0(country_name," ",Sys.time()))
        
        if(any(rl$remaining <= 2)){
          print("rate limit reached, sleeping 15 min.")
          Sys.sleep(15*60)
        }else{
          
          country_coords<- eu_bbox$bbox[[i]]
          tweet_stream<- stream_tweets(q = country_coords,
                                       timeout = (60*5),
                                       language = "en",
                                       parse = T,
                                       token = v_1_token)
          time<-as.character(Sys.time()) %>% gsub(":|\\s","-",.)
          saveRDS(tweet_stream,file = paste0(data.path,"TWT/",country_name,"_",time,".RDS"))
        }}
      end_time<- Sys.time()  
      
      time_diff<- as.numeric(difftime(end_time,starting_time, units = "days"))
      
      if(time_diff >= stream_days){
        break
      }
    }
    
  }
  
  if(isTRUE(account_look_up)){
    
    require(rtweet)
    
    if(is.vector(accounts,mode = "character")){
    account_meta_data<- rtweet::lookup_users(users = accounts, parse = T,token = token_v1)
    
    }else if(is.data.frame(accounts)||is_tibble(accounts)){
     
       account_handles<- accounts %>%
        select(matches(match = "username")) %>%
        pull()
      
       account_meta_data<- rtweet::lookup_users(users = account_handles,parse = T, token = token_v1)
      
    }else{
      
      stop("please provide accounts handles either as a character vector or as a column of a dataframe containing 'username' in the column name ")
      
    }
    account_handles<- account_meta_data %>% pull(screen_name)
    
  }else{
    account_meta_data<-accounts
    account_handles<- account_meta_data %>% pull(screen_name)
  }
  
  case_path<- make.dir(paste0(data_path,"/",case,"/","json/"))
  for (i in 1:length(account_handles)) {
    print(paste0("getting data for ", account_handles[i]," at ",Sys.time()))
    
    account_creation_date<- account_meta_data %>% 
      filter(screen_name == account_handles[i]) %>% 
      pull(account_created_at) %>% 
      as.character() %>% 
      str_split(pattern = " ")
    
    data_start_date <- paste0(account_creation_date[[1]][1],"T",account_creation_date[[1]][2],"Z")
    
    data_end_date <- paste0(Sys.Date(),"T00:00:00Z")
    
    data.path<- paste0(case_path,account_handles[i],"/")
    
    tweets<- academictwitteR::get_user_tweets(users = account_handles[i],
                                              start_tweets = data_start_date,
                                              end_tweets = data_end_date,
                                              bearer_token = bearer_token,
                                              data_path = data.path)
    
    
  }
  
}


# data cleaners -----------------------------------------------------------

#data binder

data_binder<- function(data.path,case = c("EU","IO","UK","TWT")){
  
  if(case == "TWT"){
    file_path<- paste(data.path,case,sep = "/")
    rds_files<-list.files(path = file_path,pattern = "*.RDS",full.names = T)
    rds<- purrr::map_dfr(rds_files,readRDS)
    saveRDS(rds,file = paste0(file_path,"random_tweets.RDS"))
  }else{
  
  json_directory<- paste(data.path,case,"json",sep = "/")
  json_files<- list.dirs(path = json_directory,full.names = T,recursive = F)
  case_names<- list.dirs(path = json_directory,full.names = F,recursive = F)
  
  for (i in 1:length(json_files)) {
    json_case_data<- paste0(json_files[i],"/")
    tweet_data<- academictwitteR::bind_tweet_jsons(json_case_data)
    tweet_vars<- colnames(tweet_data)[-whcih(colnames(tweet_data)%in%"author_id")]<-paste("tweet",tweet_vars,sep = "_")
    user<- tweet_data$author_id[1]
    user_data<- academictwitteR::bind_user_jsons(json_case_data) %>% 
      filter(id == user) %>% sample_n(size = 1)
    
    user_vars<- colnames(user_data)[-which(colnames(user_data)%in%"author_id")]
    colnames(user_data)
    
    colnames(user_data)[-which(colnames(user_data)%in%"author_id")]<- paste("user",user_vars,sep = "_")
    
    tweet_data<- left_join(tweet_data,user_data,by = "author_id")
    save_dir<-make.dir(file.path = paste(data.path,case,"rds",sep = "/"))
    
    saveRDS(object = tweet_data,file = paste0(save_dir,"/","tweets_and_prof_info_",case_names[i],".RDS"))
    
  }
  }
  
}

#these should still be applied with for loop for memory conservations
twitter_cleaner<- function(file,dim_even = F,prob_dim){
  #if the function will be used with an existing RDS on a local drive,
  #the function can read it now. User needs to provide full path to the file
    
    twitter_data <- file %>% jsonlite::flatten()
  
  
  
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
  
  return(twitter_data)
  
  #finally return the modified dataset.
  
}

twitter_rds_cleaner<-function(case = C("EU","IO","UK"),data_path){
  
  dirty_file_path<- paste0(data_path,case,"/rds/")
  
  files_to_clean<- list.files(dirty_file_path,"*.RDS",full.names = T)
  
  file_names<- list.files(dirty_file_path,"*.RDS") %>%
    gsub("tweets_and_prof_info|.RDS","",x=.)
  
  prob_variables<-map2_dfr(.x = files_to_clean, .y = file_names, .f = meta_extra) %>% 
    group_by(var.names) %>%
    summarise(col_count = n()) %>%
    filter(col_count < max(.$col_count)) %>% 
    pull(var.names)
  
  
  clean_rds_path <- make.dir(paste0(getwd(),"/data/",case,"/rds_clean/"))
  
  for (i in 1:length(files_to_clean)) {
    print(paste0("cleaning and saving: ",file_names[i]))
    clean_rds<-twitter_cleaner(file = files_to_clean[i],
                               dim_even = T,
                               prob_dim = prob_variables)
    
    saveRDS(object = clean_rds,file = paste0(clean_rds_path,file_names[i],".RDS"))
    print(paste0("finished cleaning \n saving the clean file to: ",
                 clean_rds_path,file_names[i],".RDS"))
  }
  
}



#deprecated#
# analysis_vars<- c("tweet_id",
#                   "is_retweet",
#                   "retweet_status_id",
#                   "is_reply",
#                   "reply_to_status_id",
#                   "is_quote",
#                   "quoted_status_id",
#                   "contains_media",
#                   "tweet_mentioned_username",
#                   "tweet_hashtags",
#                   "tweet_created_at",
#                   "tweet_text",
#                   "tweet_lang",
#                   "tweet_public_metrics.retweet_count",
#                   "tweet_public_metrics.reply_count",
#                   "tweet_public_metrics.like_count",
#                   "tweet_public_metrics.quote_count",
#                   "author_id",
#                   "user_username",
#                   "user_name",
#                   "user_description",
#                   "user_created_at",
#                   "user_public_metrics.followers_count",
#                   "user_public_metrics.following_count",
#                   "user_public_metrics.tweet_count",
#                   "user_public_metrics.listed_count")
# 
# 
# 
# twitter_rds_reader<- function(file_path, analysis_vars){
#   data<-readRDS(file_path)
#   #choose the important variables and
#   #make sure that everything is the same class
#   #I shouldn't have done this... only certain things should be character
#   # data <- data %>%
#   #   select(any_of(analysis_vars)) %>%
#   #   mutate(across(everything(),~as.character(.x)))
# 
#   #recode nas in binaries
#   na_vars<- Hmisc::contents(object = data)[[1]] %>%
#     as_tibble(rownames = "var_names") %>% 
#     filter(NAs > 0) %>% 
#     pull(var_names) %>%
#     grep(pattern = "is_*",x = .,perl = T,value = T)
#   
#   data<-data %>% mutate(across(.cols = all_of(na_vars), ~replace_na(.x,0))) %>%
#     mutate(tweet_date = lubridate::as_date(.$tweet_created_at)) %>% 
#     mutate(tweet_year = lubridate::floor_date(tweet_date,unit = "year"),
#            tweet_month = lubridate::floor_date(tweet_date, unit = "month"),
#            tweet_day = lubridate::floor_date(tweet_date, unit = "day"),
#            tweet_calendar_date = lubridate::wday(tweet_date,label = T))
#   return(data)
# }
