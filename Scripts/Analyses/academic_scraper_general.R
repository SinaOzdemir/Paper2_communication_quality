#data scraper (to be used wih academic twitter access v2.0)

source(file = paste0(getwd(),"/Scripts/Data_collection_cleaning/scraping_functions.R"))
data_path<- paste0(getwd(),"/data")
#for twitter api
bearer_token <- "$BEARER"
api_v1_token <- rtweet::create_token(app = "$appname",
                                     consumer_key = "$consumer_key",
                                     consumer_secret = "$consumer_secret_key",
                                     access_token = "$access_token",
                                     access_secret = "$access_secret_key",
                                     set_renv = F)
user_names<- readRDS(file.choose())

# scrape the tweets ------------------------------------------------

#scrape EU tweets
twitter_json_scraper(accounts = user_names,token_v1 = api_v1_token,
                     bearer_token = bearer_token,
                     account_look_up = T,
                     data.folder = data_path,case = "EU")

#Scrape IO tweets
twitter_json_scraper(accounts = user_names,token_v1 = api_v1_token,
                     bearer_token = bearer_token,
                     account_look_up = T,
                     data.folder = data_path,case = "IO")

#Scraoe UK tweets:

twitter_json_scraper(accounts = user_names,token_v1 = api_v1_token,
                     bearer_token = bearer_token,
                     account_look_up = T,
                     data.folder = data_path,case = "UK")

#scrape random tweets:

twitter_json_scraper(accounts = user_names,token_v1 = api_v1_token,
                     bearer_token = bearer_token,
                     account_look_up = T,
                     data.folder = data_path,
                     case = "TWT",
                     stream_days = 7,starting_time ="2021-05-13 11:53:20 CEST")


# bind user data and tweet data together in RDS -------------------------------------------------------------------

data_binder(data.path = data_path,case = "EU")
data_binder(data.path = data_path,case = "IO")
data_binder(data.path = data_path,case = "UK")
data_binder(data.path = data_path, case = "TWT")


# cleaning dfs: ---------------------------------------------------------------
#I didn't fully functionalize rds cleaning to be more flexible
#but wrote the script in a way that would work with "source" command

#EU

rds.path<- paste0(getwd(),"/data/EU/rds/")
rds.files<- list.files(rds.path,"*.RDS",full.names = T)
rds.file.names<- list.files(rds.path,"*.RDS") %>% gsub("tweets_and_prof_info|.RDS","",x = .)

#lets see which columns are problematic:
#if all the columns appear in equal number (i.e all the data is same dimension)
#their count number should be the same

prob_variables<-map2_dfr(.x = rds.files, .y = rds.file.names, .f = meta_extra) %>%
  group_by(var.names) %>%
  summarise(col_count = n()) %>%
  filter(col_count < max(.$col_count)) %>% 
  pull(var.names)


#run the function on all data

clean_rds_path<- paste0(getwd(),"/data/EU/rds_clean")
for (i in 1:length(rds.files)){
  twitter_rds_cleaner(file = rds.files[i],
                      read = T,
                      save = T,
                      dim_even = T,
                      prob_dim = prob_variables,
                      saveDIR = clean_rds_path)
}

#IO

rds.path<- paste0(getwd(),"/data/IO/rds/")
rds.files<- list.files(rds.path,"*.RDS",full.names = T)
rds.file.names<- list.files(rds.path,"*.RDS") %>% gsub("tweets_and_prof_info|.RDS","",x = .)

#lets see which columns are problematic:
#if all the columns appear in equal number (i.e all the data is same dimension)
#their count number should be the same

prob_variables<-map2_dfr(.x = rds.files, .y = rds.file.names, .f = meta_extra) %>%
  group_by(var.names) %>%
  summarise(col_count = n()) %>%
  filter(col_count < max(.$col_count)) %>% 
  pull(var.names)


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

#UK


rds.path<- paste0(getwd(),"/data/UK/rds/")
rds.files<- list.files(rds.path,"*.RDS",full.names = T)
rds.file.names<- list.files(rds.path,"*.RDS") %>% gsub("tweets_and_prof_info|.RDS","",x = .)

#lets see which columns are problematic:
#if all the columns appear in equal number (i.e all the data is same dimension)
#their count number should be the same

prob_variables<-map2_dfr(.x = rds.files, .y = rds.file.names, .f = meta_extra) %>%
  group_by(var.names) %>%
  summarise(col_count = n()) %>%
  filter(col_count < max(.$col_count)) %>% 
  pull(var.names)


#run the function on all data

clean_rds_path<- paste0(getwd(),"/data/UK/rds_clean")
for (i in 1:length(rds.files)){
  twitter_rds_cleaner(file = rds.files[i],
                      read = T,
                      save = T,
                      dim_even = T,
                      prob_dim = prob_variables,
                      saveDIR = clean_rds_path)
}

