#data scraper (to be used wih academic twitter access v2.0)

source(file = paste0(getwd(),"/Scripts/Data_collection_cleaning/scraping_functions.R"))
data_path<- paste0(getwd(),"/data")
#for twitter api
bearer_token <- "AAAAAAAAAAAAAAAAAAAAANh3NgEAAAAAYpb57mP0saX5D%2BCi4vfCvf%2FWp6Q%3DD6xFxcFGBkLGz0BWs6f4EylxpdxEeLIpLk867py48blqPVwQUm"
api_v1_token <- rtweet::create_token(app = "Functionised_collector",
                                    consumer_key = "c0wODdFRKzNDz23l7O9A6GBig",
                                    consumer_secret = "uwraekdxEX2BaVZSNLw7sjkVbq3wNRByYYiOolHrYaikqZHKUW",
                                    access_token = "1151438784384421888-eas7PxAdeoohUCykI5XeX2Wc2lppjl",
                                    access_secret = "96TaBIVuZXlpICK2QAP7IurCFuYPfHSUbXYUTTSRak3gr",
                                    set_renv = F)


# scrape the tweets ------------------------------------------------

#scrape EU tweets
eu_user_names<- readRDS(file.choose())

twitter_json_scraper(accounts = user_names,token_v1 = api_v1_token,
                     bearer_token = bearer_token,
                     account_look_up = T,
                     data.folder = data_path,case = "EU")

#Scrape IO tweets

io_user_names<- readxl::read_excel("./data/Accounts/io_accounts.xlsx",sheet = 1,na = c(" ",""))
twitter_json_scraper(accounts = user_names,
                     token_v1 = api_v1_token,
                     bearer_token = bearer_token,
                     account_look_up = T,
                     data.folder = data_path,case = "IO")

#Scrape UK tweets:

uk_user_names<- readxl::read_excel(path = "./data/Accounts/uk_accounts.xlsx",sheet = 1, na= c(" ","")) %>%
  select(name:institutional_affiliation) %>% drop_na() %>% 
  pull(twitter_handle)

twitter_json_scraper(accounts = uk_user_names[152:201],
                     token_v1 = api_v1_token,
                     bearer_token = bearer_token,
                     account_look_up = T,
                     data.folder = data_path,
                     case = "UK")

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
