#data scraper (to be used wih academic twitter access v2.0)

source(file = paste0(getwd(),"/Scripts/Data_collection_cleaning/scraping_functions.R"))
data_path<- paste0(getwd(),"/data")
cases <- c("UK","EU","IO","TWT")
case_selection<- as.integer(readline(prompt = "Which case should I work on? \n 1:UK \n 2:EU \n 3:IO \n 4:Random sample of tweets from Europe \n"))
case<- cases[case_selection]

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


path_for_dim_check<- paste(data_path,case,"json",sep = "/")
account_files<- list.dirs(path = path_for_dim_check,full.names = T,recursive = F)
account_names<- list.dirs(path = path_for_dim_check,full.names = F,recursive = F)

data.dimensions<- data.frame()

for (i in 1:length(account_files)) {
  print(paste0("detecting dimensions for ", account_names[i]))
  json_files<- list.files(path = account_files[i],pattern = "data_[0-9]+",full.names = T)
  json_dimensions<- map_dfr(.x = json_files,.f = meta_extra)
  data.dimensions<- rbind(data.dimensions,json_dimensions)  
}

prob_dims<- data.dimensions %>%group_by(var.names) %>%
  dplyr::summarise(col_count = n()) %>%
  filter(col_count < max(.$col_count)) %>% pull(var.names)


for(i in 1:length(account_files)){
  print(paste0("evening out dimensions and converting jsons to rds for ", account_names[i]))
  json_files<- list.files(path = account_files[i],pattern = "data_[0-9]+",full.names = T)
  if(length(json_files) == 0){
    next
  }
  twitter_data<- map_dfr(.x = json_files, .f = dim_even, dims = prob_dims)
  saveDIR<- make.dir(file.path = paste0(data_path,"/",case,"/","rds/"))
  saveRDS(object = twitter_data, file = paste0(saveDIR,account_names[i],".RDS"))
}
remove(twitter_data)
 

# TWT
#detect problem variables to even out dimensions:


# cleaning dfs: ---------------------------------------------------------------


rds_files<- list.files(file.path(data_path,case,"rds"),pattern = "*.RDS",full.names = T)
accounts<- list.files(file.path(data_path,case,"rds"),pattern = "*.RDS",full.names = F)
for (i in 1:length(rds_files)) {
    
  account<- gsub(".RDS","",accounts[i])
    
    cat("working on:", account,".\n",sep = " ")
    
    clean_rds<- twitter_cleaner(rds_file = rds_files[i])
    
    cat("adding profile information\n")
    user_id<- as.character(unique(clean_rds$author_id))
    
    #this works outside the function but
    #for some reason I get:
    #Error: Problem with `filter()` input `..1`.
    #x Input `..1` must be of size 167 or 1, not size 0.
    #i Input `..1` is `id == user_id`.
    #when i put it in the function so I decided to do it outside the function
    
    prof_info<- list.files(file.path(data_path,case,"json",account),pattern = "users_[0-9]+",full.names = T)[1]
   
    if(isFALSE(length(prof_info)==0)){
      profile_information<- fromJSON(txt = prof_info,flatten = T)[["users"]] %>%
        as.data.frame() %>% 
        filter(id == user_id) %>%
        rename(author_id = id)
      
      profile_colnames<- paste("user",colnames(profile_information),sep = "_")
      
      colnames(profile_information)[which(names(profile_information)=="profile_author_id")]<-"author_id"
      
      clean_rds<- left_join(clean_rds, profile_information, by = "author_id")
      cat("found profile information for ", account,"appending it to the data\n", sep = " ")
    }
    
    saveDIR<- make.dir(file.path = file.path(data_path,case,"rds_clean"))
    saveRDS(object = clean_rds,file = paste0(saveDIR,"/",account,".RDS"))
    cat("finished working on", account,"moving on to the next account\n",sep = " ")
    
    remove(clean_rds)
}


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
