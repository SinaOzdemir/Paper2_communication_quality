#data scraper (to be used wih academic twitter access v2.0)

devtools::install_github("cjbarrie/academictwitteR")

library(academictwitteR)
library(tidyverse)
dataDIR<-paste0(getwd(),"/metadata")

metadata = readRDS(file = paste0(dataDIR,"/metadata_14-03-2021.RDS"))
set.seed(28032021)
test_account = filter(metadata, statuses_count >3500) %>% sample_n(tbl = .,size = 1)

creation_date<- test_account$account_created_at %>% str_split(string = .,pattern = " ",simplify = T)
start_date<- paste0(creation_date[,1],"T",creation_date[,2],"Z")
end_date <- paste0(Sys.Date(),"T00:00:00Z")

screen_name<- paste0(test_account$screen_name)
bearer_token <- "AAAAAAAAAAAAAAAAAAAAANh3NgEAAAAAYpb57mP0saX5D%2BCi4vfCvf%2FWp6Q%3DD6xFxcFGBkLGz0BWs6f4EylxpdxEeLIpLk867py48blqPVwQUm"

a<-get_user_tweets(users = screen_name,start_tweets = start_date,end_tweets = end_date,bearer_token = bearer_token)
