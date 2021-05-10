#data scraper (to be used wih academic twitter access v2.0)

devtools::install_github("cjbarrie/academictwitteR")

library(academictwitteR)
library(tidyverse)
dataDIR<-paste0(getwd(),"/metadata")
paste0(dataDIR,"/metadata_14-03-2021.RDS")
metadata = readRDS(file = file.choose())
bearer_token <- "AAAAAAAAAAAAAAAAAAAAANh3NgEAAAAAYpb57mP0saX5D%2BCi4vfCvf%2FWp6Q%3DD6xFxcFGBkLGz0BWs6f4EylxpdxEeLIpLk867py48blqPVwQUm"


# scrape the tweets ------------------------------------------------
#amele işi indirme...

#first scrape tweets:

eu_accounts<- metadata$screen_name

for (i in seq_along(eu_accounts)) {
  #get_creation_date
  creation_date<- metadata %>%
    filter(screen_name == eu_accounts[i]) %>%
    pull(account_created_at) %>%
    as.character() %>%
    str_split(pattern = " ")
  #formulate the start date
  start_date<-  paste0(creation_date[[1]][1],"T",creation_date[[1]][2],"Z")
  #formulate the end date
  end_date <-  end_date<- paste0(Sys.Date(),"T00:00:00Z")
  #formulate the json save directory
  data.path<-paste0(getwd(),"/data/",eu_accounts[i],"/")
  
  #get the tweets and users information
  
  
  tweets<- academictwitteR::get_user_tweets(users = eu_accounts[i],
                                            start_tweets = start_date,
                                            end_tweets = end_date,
                                            bearer_token = bearer_token,
                                            bind_tweets = F,
                                            data_path = data.path)
  
    
}







# bind user data and tweet data together in RDS -------------------------------------------------------------------


for (i in 1:length(eu_accounts)){
  print(eu_accounts[i])
 #some accounts don't exist anymore, so the loop throws an error
  #I was going home before I ran this script and I didn't want it to break down
  #when I was away, thats why I used try(,silent = T)
  try(expr = { data.path<-paste0(getwd(),"/data/",eu_accounts[i],"/")
  
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
  
  if(dir.exists(paste0(getwd(),"/data/rds"))){
  
    saveRDS(tweet_data, file = paste0(getwd(),"/data/rds/tweets_and_prof_info_",eu_accounts[i],".RDS"))
  
    }else{
    
      dir.create(paste0(getwd(),"/data/rds"))
    
      saveRDS(tweet_data, file = paste0(getwd(),"/data/rds/tweets_and_prof_info_",eu_accounts[i],".RDS"))
  }},silent = T)
}

#there is something wrong with EU_EASME, I can't find the twitter page
#and when I try to trace the official page it leads to the page via https://wayback.archive-it.org/
eu_accounts[59]

#found the problem... as of april 1 EU_EASME is shutdown and replace with @EU_EISMEA
#I can get the data for this one but I am not sure if one executive agency worths the trouble
eu_accounts[106]

#similar problem with inea_eu, it ceased to exist on april 1 and replaced by CINEA

# cleaning dfs: ---------------------------------------------------------------
rds.path<- paste0(getwd(),"/data/rds/")
rds.files<- list.files(rds.path,"*.RDS",full.names = T)
rds.file.names<- list.files(rds.path,"*.RDS") %>% gsub("tweets_and_prof_info|.RDS","",x = .)

meta_extra <- function(file_path,file_names){
  a <- readRDS(file = file_path) %>% jsonlite::flatten()
  b <- Hmisc::contents(a)[[1]] %>%
    as_tibble(rownames = "var.names") %>% 
    mutate(screen_name = rep(file_names, times = nrow(.)))
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
  
  if(isTRUE(dim_even)){
    #this simply removes uncommon columns
    twitter_data<- twitter_data %>% select(-any_of(prob_dim))
  }
  
  #referenced_tweets for retweets, quotes and replies----
  
  for (i in 1:nrow(twitter_data)) {
    if(length(twitter_data$tweet_referenced_tweets[[i]])==0){
      twitter_data$tweet_referenced_tweets[i]<-NA
    }
    
  }
  #tweet_referenced_tweets_tweet_referenced_tweets wtf is this?
  
  twitter_data<- twitter_data[!is.na(twitter_data$tweet_referenced_tweets),] %>%
    select(tweet_id,tweet_referenced_tweets) %>%
    group_by(tweet_id) %>%
    unnest(cols = c(tweet_referenced_tweets),names_sep = "_") %>%
    right_join(x = .,y=twitter_data, by = "tweet_id")
  
  #create is_retweet, is_quote, is_reply and reply_to_status_id for later calculations
  #here it breaks for some reason
  
  twitter_data$is_retweet = ifelse(twitter_data$tweet_referenced_tweets_type == "retweeted",1,0)
  twitter_data$retweet_status_id = ifelse(twitter_data$tweet_referenced_tweets_type == "retweeted",twitter_data$tweet_referenced_tweets_id,NA)
  twitter_data$is_reply = ifelse(twitter_data$tweet_referenced_tweets_type == "replied_to",1,0)
  twitter_data$reply_to_status_id = ifelse(twitter_data$tweet_referenced_tweets_type == "replied_to",twitter_data$tweet_referenced_tweets_id,NA)
  twitter_data$is_quote = ifelse(twitter_data$tweet_referenced_tweets_type == "quoted",1,0)
  twitter_data$quoted_status_id = ifelse(twitter_data$tweet_referenced_tweets_type =="quoted",twitter_data$tweet_referenced_tweets_id,NA)

    #entities: hashtags and mentions----
  #hashtags
  twitter_data<- twitter_data %>%
    select(tweet_id, tweet_entities.hashtags) %>%
    group_by(tweet_id) %>%
    unnest(cols = c(tweet_entities.hashtags)) %>% 
    plyr::dlply(.data = . , .variables = "tweet_id",.fun = function(i)i[["tag"]]) %>% 
    tibble(tweet_id = names(.),tweet_hashtags = .) %>%
    right_join(x = .,y = twitter_data,by = "tweet_id")
  
  #mentions
  twitter_data<- twitter_data %>%
    select(tweet_id, tweet_entities.mentions) %>%
    group_by(tweet_id) %>%
    unnest(cols = c(tweet_entities.mentions)) %>% 
    plyr::dlply(.data = . , .variables = "tweet_id",.fun = function(i)i[["username"]]) %>% 
    tibble(tweet_id = names(.),tweet_mentioned_username = .) %>%
    right_join(x = .,y = twitter_data,by = "tweet_id")
  
  
  #contains media ----
  for(i in 1:nrow(twitter_data)){
    if(length(twitter_data$tweet_attachments.media_keys[[i]]) == 0){
      twitter_data$tweet_attachments.media_keys[i]<- NA
    }
  }
  
  twitter_data$contains_media <- ifelse(is.na(twitter_data$tweet_attachments.media_keys),0,1)
  
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
#there is something wrong with is_X variables, if tweet_referenced_tweet_type is NA, they are coded as NA as well...
#test the function

clean_rds_path<- paste0(getwd(),"/data/rds_clean")
for (i in 1:length(rds.files)) {
  twitter_rds_cleaner(file = rds.files[i],
                      read = T,
                      save = T,
                      dim_even = T,
                      prob_dim = prob_variables,
                      saveDIR = clean_rds_path)
  
}

# thrash ------------------------------------------------------------------


eu_accounts<- metadata$screen_name

creation_date<- metadata %>%
  filter(screen_name == eu_accounts[1]) %>%
  pull(account_created_at) %>%
  as.character() %>%
  str_split(pattern = " ")

start_date<-  paste0(creation_date[[1]][1],"T",creation_date[[1]][2],"Z")

end_date <-  end_date<- paste0(Sys.Date(),"T00:00:00Z")

data.path<-paste0(getwd(),"/data/")

tweets<- academictwitteR::get_user_tweets(users = eu_accounts[1],
                                          start_tweets = start_date,
                                          end_tweets = end_date,
                                          bearer_token = bearer_token,
                                          bind_tweets = F,
                                          data_path = data.path)


user_jsons<-list.files(path = data.path,pattern = "^user",full.names = T)

tweet_jsons<-list.files(path = data.path,pattern = "^data",full.names = T)


user_meta<- jsonlite::fromJSON(txt = user_jsons[1],flatten = T)

a<- user_meta[[1]] %>% dplyr::rename(author_id = id)
b<- user_meta[[2]]
c<- left_join(a,b, by = "author_id")

#user_x.json returns gazzilion irrelevant users
native_user<- academictwitteR::bind_user_jsons(data_path = data.path) %>% dplyr::rename(author_id = id) %>% filter(username == eu_accounts[1])
native_tweets<- academictwitteR::bind_tweet_jsons(data_path = data.path)

#there is a function called get_user_profile to look up info on account, I can either look up info from RDS or simply rescrape everything

#test get_user_profile

user_prof_info<- academictwitteR::get_user_profile(x = unique(native_user$author_id),bearer_token = bearer_token)
