#data scraper (to be used wih academic twitter access v2.0)

#devtools::install_github("cjbarrie/academictwitteR")

library(academictwitteR)
library(tidyverse)
accountDIR<- paste0(getwd(),"/data/Accounts")
bearer_token <- "AAAAAAAAAAAAAAAAAAAAANh3NgEAAAAAYpb57mP0saX5D%2BCi4vfCvf%2FWp6Q%3DD6xFxcFGBkLGz0BWs6f4EylxpdxEeLIpLk867py48blqPVwQUm"

# identify the comparable IOs ---------------------------------------------
# for this task I am going to use Hooghe,Lenz,Marks (2019) Measuring Internationa Authority dataset
# 3 indicators are particularly important:delegation,poolings and policy scope
# Policy scope indicate how many policy areas the IO has authority and ranges from 1 to 25
# Delegation indicates to what extend IO can  autonomously excercise authority 
# Pooling indicates the level of collective governance by member states. In other words,
# To what extend member states have agency in the excercise of IO's authority.(chp1, p. 22-23)
library(sjlabelled)
ioauthoritydataDIR<-paste0(accountDIR,"/hooghe_lenz_marks_policy_scope/")

policy_scope <- read_stata(path = paste0(ioauthoritydataDIR,"policy_type_scope_contract_jan2021.dta"))

policy_scope_narrow<- policy_scope %>%
  select(ionumber,ioname,year,acronym,io,scope,core,flank) %>% #scope = core+flank, where core is the number core policy areas where the IO is responsible, flank is the number of policy areas tangent to the core areas
  filter(year >= 2008) #our EU data mostly spans between 2008- 2021

length(unique(policy_scope_narrow$ioname))#75 IOs are bit too much for visualizations
#my aim is to identify IOs who are comparable to the EU in terms of policy scope in a given year.
#By comparable I mean, it has a scope score about (Scope_eu - 1sd)
#So lets create some summary stats (sd) per year and see which IO falls in this category

policy_scope_summary <- policy_scope_narrow %>%
  group_by(year) %>%
  summarise(scope_sd = round(sd(scope)))
#scope indicates number of policy areas, so having decimals don't make much sense

policy_scope_EU <- policy_scope_narrow %>%
  filter(ioname == "EU") %>%
  left_join(.,y = policy_scope_summary,by = "year") %>% 
  mutate(relevance_score_low = scope - scope_sd,
         relevance_score_high = scope + scope_sd)
#I am sure there is a better way of doing this with dplyr,
#I don't wanna waste time looking for that better way
years<- unique(policy_scope_narrow$year)
comparable_ios <- data.frame()

for (i in 1: length(years)){
  ios <- policy_scope_narrow %>% filter(year == years[i])
  eu_year<- policy_scope_EU %>% filter(year == years[i])
  relevant_io_year <- ios %>%
    filter(scope >= eu_year$relevance_score_low & scope <= eu_year$relevance_score_high)
  comparable_ios<- rbind(comparable_ios,relevant_io_year)
  
}

comparable_ios %>%filter(ioname != "EU") %>%  ggplot(.,aes(x = ioname, y= scope, group = ioname)) +
  geom_bar(aes(fill = ioname),stat="identity",position = "dodge") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0))+
  facet_wrap(~year)

# rinse and repeat for delegation and pooling

dp <- read_stata(path = paste0(ioauthoritydataDIR,"authority_level/DP_volIII_june2019_15.dta"))

dp_narrow<- policy_scope %>%
  select(ionumber,ioname,year,acronym,delegation,pooling) %>%
  filter(year >= 2008) #our EU data mostly spans between 2008- 2021

length(unique(dp_narrow$ioname))#76 ios

dp_summary <- dp_narrow %>%
  group_by(year) %>%
  summarise(delegation_sd = sd(delegation),
            pooling_sd = sd(pooling))
#scope indicates number of policy areas, so having decimals don't make much sense

dp_EU <- dp_narrow %>%
  filter(ioname == "EU") %>%
  left_join(.,y = dp_summary,by = "year") %>% 
  mutate(delegation_relevance_score_low = (delegation - delegation_sd),
         delegation_relevance_score_high = (delegation + delegation_sd),
         pooling_relevance_score_low = (pooling - pooling_sd),
         pooling_relevance_score_high = (pooling + pooling_sd))
#delegation and pooling for the EU are measured only for 2008,2009 and 2010
#this makes it really hard to compare IOs with EU for our measure
#So maybe we can make the case that, these IOs are comparable to the EU
#in terms of breadth of authority rather than depth of authority
# I know both deepening and broadening can cause significant politicization
# but breadth is the only reasonable yard stick.


final_io_list<- comparable_ios$ioname
# Identify IO user handles and look up profile information on them --------

io_handles<- readxl::read_excel(path = paste0(accountDIR,"/io_accounts.xlsx"),sheet = 1)

io_relevant_handles<- io_handles %>%
  mutate(io_names_lower = tolower(IO_Name),
         screen_name = gsub("@","",TW_handle)) %>%
  filter(io_names_lower %in% tolower(final_io_list)) %>%
  pull(screen_name)

#lookup profile info:

library(rtweet)

v_1_token<-rtweet::create_token(app = "Functionised_collector",
                     consumer_key = "c0wODdFRKzNDz23l7O9A6GBig",
                     consumer_secret = "uwraekdxEX2BaVZSNLw7sjkVbq3wNRByYYiOolHrYaikqZHKUW",
                     access_token = "1151438784384421888-eas7PxAdeoohUCykI5XeX2Wc2lppjl",
                     access_secret = "96TaBIVuZXlpICK2QAP7IurCFuYPfHSUbXYUTTSRak3gr",
                     set_renv = F)

io_prof_infor<- rtweet::lookup_users(users = io_relevant_handles,parse = T,token = v_1_token)


io_meta_data<- io_relevant_handles<- io_handles %>%
  mutate(io_names_lower = tolower(IO_Name),
         screen_name = gsub("@","",TW_handle)) %>%
  filter(io_names_lower %in% tolower(final_io_list)) %>% 
  left_join(.,y = io_prof_infor, by = "screen_name")

io_meta_data<- io_meta_data[!is.na(io_meta_data$text),]
#we have 55 IO and related accounts.
saveRDS(io_meta_data,file = paste0(accountDIR,"/io_twitter_meta.RDS"))

# scrape the tweets ------------------------------------------------
#
#first scrape tweets:

metadata<- readRDS(paste0(accountDIR,"/io_twitter_meta.RDS"))
accounts<- metadata$screen_name 
#need to look-up accounts first...

#scrape the accounts based on that
for (i in 4:length(accounts)) {
  #get_creation_date
  creation_date<- metadata %>%
    filter(screen_name == accounts[i]) %>%
    pull(account_created_at) %>%
    as.character() %>%
    str_split(pattern = " ")
  #formulate the start date
  start_date<-  paste0(creation_date[[1]][1],"T",creation_date[[1]][2],"Z")
  #formulate the end date
  end_date <-  end_date<- paste0(Sys.Date(),"T00:00:00Z")
  #formulate the json save directory
  data.path<-paste0(getwd(),"/data/IO/json/",accounts[i],"/")
  
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
  try(expr = { data.path<-paste0(getwd(),"/data/IO/json/",accounts[i],"/")
  
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
  
  if(dir.exists(paste0(getwd(),"/data/IO/rds"))){
  
    saveRDS(tweet_data, file = paste0(getwd(),"/data/IO/rds/tweets_and_prof_info_",accounts[i],".RDS"))
  
    }else{
    
      dir.create(paste0(getwd(),"/data/IO/rds"))
    
      saveRDS(tweet_data, file = paste0(getwd(),"/data/IO/rds/tweets_and_prof_info_",accounts[i],".RDS"))
  }},silent = F)
}

# cleaning dfs: ---------------------------------------------------------------
rds.path<- paste0(getwd(),"/data/IO/rds/")
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

#test the function

function_test<- twitter_rds_cleaner(file = rds.files[54],read = T,save = F,dim_even = T)

#run the function on all data

clean_rds_path<- paste0(getwd(),"/data/IO/rds_clean")
for (i in 1:length(rds.files)){
twitter_rds_cleaner(file = rds.files[i],
                    read = T,
                    save = T,
                    dim_even = T,
                    prob_dim = prob_variables,
                    saveDIR = clean_rds_path)
}