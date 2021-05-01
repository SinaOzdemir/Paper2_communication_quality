#data scraper (to be used wih academic twitter access v2.0)

devtools::install_github("cjbarrie/academictwitteR")

library(academictwitteR)
library(tidyverse)
dataDIR<-paste0(getwd(),"/metadata")

metadata = readRDS(file = paste0(dataDIR,"/metadata_14-03-2021.RDS"))
bearer_token <- "AAAAAAAAAAAAAAAAAAAAANh3NgEAAAAAYpb57mP0saX5D%2BCi4vfCvf%2FWp6Q%3DD6xFxcFGBkLGz0BWs6f4EylxpdxEeLIpLk867py48blqPVwQUm"


twitter_academic_scraper<- function(data,user_name,save_dir, bearer_token){
  account_date<- data %>%
    filter(screen_name%in%user_name) %>%
    pull(account_created_at) %>%
    str_split(string = .,pattern = " ",simplify = T)
  
  start_date <- paste0(creation_date[,1],"T",creation_date[,2],"Z")
  
  end_date<- paste0(Sys.Date(),"T00:00:00Z")
  
  user_tweets<- get_user_tweets(users = user_name,start_tweets = start_date,end_tweets = end_date,bearer_token = bearer_token)
  
  saveRDS(object = user_tweets,file = paste0(saveDIR,"/",user_name,".RDS"))
}




for (i in 1:length(user_names)) {
  twitter_academic_scraper(data = metadata,user_name = user_names[i],save_dir = dataDIR,bearer_token = bearer_token)  
}