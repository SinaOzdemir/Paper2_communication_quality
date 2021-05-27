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

#EU

twitter_rds_cleaner(case = "EU", data_path = "./data/")

#IO

twitter_rds_cleaner(case = "IO", data_path = "./data/")

#UK


twitter_rds_cleaner(case = "UK", data_path = "./data/")

#TWT data is already clean.
