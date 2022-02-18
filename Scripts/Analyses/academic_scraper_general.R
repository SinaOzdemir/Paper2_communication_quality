#data scraper (to be used wih academic twitter access v2.0)

source(file = paste0(getwd(),"/Scripts/Data_collection_cleaning/scraping_functions.R"))
data_path<- paste0(getwd(),"/data")
cases <- c("UK","EU","IO","TWT")

case_selection<- as.integer(readline(prompt = "Which case should I work on? \n 1:UK \n 2:EU \n 3:IO \n 4:Random sample of tweets from Europe \n"))

case<- cases[case_selection]

#for twitter api
bearer_token <- ""
api_v1_token <- rtweet::create_token(app = "",
                                    consumer_key = "",
                                    consumer_secret = "",
                                    access_token = "",
                                    access_secret = "",
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
json_path<- list.dirs(path = file.path(data_path,case,"json"),full.names = T,recursive = F)
accounts<- list.dirs(path = file.path(data_path,case,"json"),full.names = F,recursive = F)

for (i in 118:length(json_path)) {
  cat("working on", accounts[i],".\n")
  
  data_jsons<- list.files(path = json_path[i],pattern = "data_[0-9]+",full.names = T)
  user_json<- list.files(path = json_path[i],pattern = "users_[0-9]+",full.names = T)[1]
  
  if(length(data_jsons) == 0 || length(user_json) == 0){
    cat("there is a problem with user or tweet data\n",
        "tweet data length", length(data_jsons),".\n",
        "user data length", length(user_json),".\n",
        "skipping the user", accounts[i],".\n")
    next
  }
  
  cat("binding data and modifying colnames of", accounts[i],".\n")
  
  tweets<- map_dfr(data_jsons,fromJSON)
  tweets_colnames<- paste("tweet",colnames(tweets),sep = "_")
  colnames(tweets)<- tweets_colnames
  colnames(tweets)[which(names(tweets)=="tweet_author_id")]<-"author_id"
  
  
  cat("appending user profile information for", accounts[i],".\n")
  user<- unique(tweets$author_id)
  
  user_data<- fromJSON(txt = user_json)[["users"]] %>%
    as.data.frame() %>%
    filter(id == user) %>%
    rename(author_id = id)
  
  user_colnames <- paste("user",colnames(user_data),sep = "_")
  
  colnames(user_data)<- user_colnames
  
  colnames(user_data)[which(names(user_data)=="user_author_id")]<- "author_id"
  
  twitter_data<- left_join(x = tweets, y = user_data, by = "author_id")
  
  cat("profile information is appended, converting data to RDS for", accounts[i],".\n")
  
  saveDIR<-make.dir(file.path = file.path(data_path,case,"rds"))
  
  saveRDS(object = twitter_data,file = paste0(saveDIR,"/",accounts[i],".RDS"))
  
}


# dimension evening and cleaning -------------------------------------------------------


rds_files<- list.files(path = file.path(data_path,case,"rds"),pattern = "*.RDS",full.names = T,recursive = F)
rds_names<- list.files(path = file.path(data_path,case,"rds"),pattern = "*.RDS",full.names = F,recursive = F)


prob_dims<- map_dfr(rds_files,meta_extra) %>%
  group_by(var.names) %>%
  dplyr::summarise(col_count = n()) %>%
  filter(col_count < max(.$col_count)) %>% pull(var.names)


#run this individually rds_files[75] and rds_files[118]

for (i in 119:length(rds_files)) {
  
  account<- gsub(".RDS","",rds_names[i])
  
  cat("working on:", account,".\n",sep = " ")
  
  clean_rds<- dim_even(data_path = rds_files[i],dims = prob_dims)
  
  clean_rds<- twitter_cleaner(rds_file = clean_rds)
  
  saveDIR<- make.dir(file.path = file.path(data_path,case,"rds_clean"))
  saveRDS(object = clean_rds,file = paste0(saveDIR,"/",account,".RDS"))
  cat("finished working on", account,"moving on to the next account\n",sep = " ")
  
  remove(clean_rds)
}


# TWT
#detect problem variables to even out dimensions:


# cleaning dfs: ---------------------------------------------------------------



clean_rds_path<- list.files(file.path(data_path,case,"rds_clean"),pattern = "*.RDS",full.names = T)

rds_prob_dim<-map_dfr(.x = clean_rds_path,.f = meta_extra)

prob_dims<- rds_prob_dim %>%group_by(var.names) %>%
  dplyr::summarise(col_count = n()) %>%
  filter(col_count < max(.$col_count)) %>% pull(var.names)



#TWT data is already clean.

twt_files<-list.files(path = file.path(data_path,case),pattern = "*.RDS",full.names = T)



twt_data<- map_dfr(.x = twt_files,.f = readRDS)

a<-twt_data %>%
  group_by(country_code,lang) %>%
  summarise(tweet_count = n(),.groups = "keep") %>%
  drop_na() %>%
  arrange(desc(tweet_count),.by_group = T) %>%
  mutate(country_name = countrycode::countrycode(country_code,"iso2c","country.name")) %>% 
  filter(lang != "und") %>% 
  ggplot(data = .,aes(x = country_name, y = lang)) +
  geom_tile(aes(fill = tweet_count),color = "grey50")+
  scale_fill_distiller(palette = "RdPu")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "Country",
       y= "Language (iso2 code)",
       title = "Linguistic diversity on Twitter in continental Europe",
       subtitle = "Tweets are collected by querying country bbox from API with 5 min window \n (N = 83823)")

saveRDS(twt_data,file = paste0(data_path,"/corpii/","random_tweets.RDS"))
