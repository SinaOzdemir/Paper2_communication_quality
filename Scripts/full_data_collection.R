#Extra data from twitter profiles:

packs<-c("rtweet","tidyverse")
lapply(packs,library,character.only = T)

dataDIR<-paste(getwd(),"Data",sep = "/")

academic_api<- rtweet::create_token(app = "academic_access",
                                    consumer_key = "IBeCSIhB5tLx5ld6bqsceu8WZ",
                                    consumer_secret = "FtM3n4SvT6wmmWxnljejmpUubWJrqsmLHRPSEzIMGo25HyMuC5",
                                    access_token = "1151438784384421888-UsdBXzmqAEcy6u8uvWf5pCSbB6fBK6",
                                    access_secret = "3vfDMf5wOi5Ul6WI09hY5bPyoCXn07t21tZxmKm34QOGB",
                                    set_renv = F)


eu_accounts<-readRDS(file = paste0(dataDIR,"/qc_paper_accounts.RDS")) %>%
  select(., user_id) %>%
  mutate_if(is.factor,as.character) %>%
  pull(.,var = "user_id") 

user_meta<-lookup_users(users = eu_accounts,parse = T,token = academic_api)

scraper<- function(x,y){
  print(paste0("starting with ", x))
  
  rl<-rate_limits(token = academic_api) %>% 
    filter(.,query%in%c("statuses/user_timeline","application/rate_limit_status"))
    
    if (isTRUE(any(rl$remaining<=1))) {
      print("rate limit reached sleeping now")
      Sys.sleep(15*60)
    }
  
    user <- get_timeline(user = x,
                         n = y,
                         parse = T,
                         check = F,
                         token = academic_api)
    
    saveRDS(object = user,file = paste0(dataDIR,"/tweets_",x,".RDS"))
    return(user)
}

#not a smart way of using map2 i suppose
e_data<- map2(.x = user_meta$user_id,.y = user_meta$statuses_count,.f = scraper)


# validating the data -----------------------------------------------------

tweetDIR<-"C:/Users/sinaoz/OneDrive - NTNU/Projects/Communication_Quality/Data/eutweets"

flist<-list.files(path = tweetDIR,pattern = "*.RDS",full.names = T)

validation<-function(x){
  data<-readRDS(x)
  user_meta<- users_data(data) %>%
    sample_n(tbl = .,size = 1) %>%
    select(.,user_id:verified)
  data_meta = tibble(tweet_count = nrow(data),
                     latest_tweet = max(data$created_at),
                     earliest_tweet = min(data$created_at),
                     account_age = ((round(difftime(max(data$created_at),
                                            max(data$account_created_at),
                                            units = "weeks")))/52)) %>%
    cbind(.,user_meta)
    
  return(data_meta)
}


validation_results<- map_dfr(flist,.f = validation)

saveRDS(object = validation_results,file = paste0(dataDIR,"/metadata/metadata_14-03-2021.RDS"))
