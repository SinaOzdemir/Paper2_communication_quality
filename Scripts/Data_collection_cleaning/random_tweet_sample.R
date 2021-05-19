#Random sample of tweets

# setup -------------------------------------------------------------------


library(pacman)
packs<- c("tidyverse","rtweet","rvest")
pacman::p_load(char = packs,update = F)
data.path<- paste0(getwd(),"/data/")

v_1_token<-rtweet::create_token(app = "Functionised_collector",
                                consumer_key = "c0wODdFRKzNDz23l7O9A6GBig",
                                consumer_secret = "uwraekdxEX2BaVZSNLw7sjkVbq3wNRByYYiOolHrYaikqZHKUW",
                                access_token = "1151438784384421888-eas7PxAdeoohUCykI5XeX2Wc2lppjl",
                                access_secret = "96TaBIVuZXlpICK2QAP7IurCFuYPfHSUbXYUTTSRak3gr",
                                set_renv = F)

#google_maps_api_key <- "AIzaSyCsz6qXAbWh6eglXkq91vebo1NRGY2xaS4"

# get the list of EU countries --------------------------------------------
library(countrycode)
eu_countries <- read_html("https://europa.eu/european-union/about-eu/countries_en") %>%
  rvest::html_elements("#year-entry2 a") %>% html_text2()
eu_countries_df <- tibble(country_name = eu_countries,
                          country_code = countrycode::countrycode(eu_countries,origin = "country.name",destination = "iso2c"))


# look up eu member state coordinates -------------------------------------
#test lookup_coords function
# world_coords<- rtweet::lookup_coords(address = "us")
# eu_coords<-rtweet::lookup_coords(address = "")
# I couldn't get google maps API work but I found a dataset 
#of country bboxes on (https://github.com/sandstrom/country-bounding-boxes)
# last validated on Feb6, 2020

country_coords<-jsonlite::fromJSON(txt = paste0(data.path,"/Accounts/country-bounding-boxes-master/bounding-boxes.json"))

#not the most kosher way of doing it but it works
country_bbox<- do.call("rbind",country_coords) %>%
  as.data.frame %>%
  mutate(country_code = rownames(.)) %>%
  rename(country_name = V1, bbox = V2) %>% 
  mutate(country_code = as.vector(country_code, mode = "character"),
         country_name = as.vector(country_name, mode = "character"))
         

eu_bbox<- left_join(x = eu_countries_df,country_bbox, by = "country_code") %>% drop_na()
#Malta's bbox is missing.


# stream in random tweets -------------------------------------------------

#test streaming function with bboxes
coords<- eu_bbox$bbox[[1]]

aust_random_tweets<- rtweet::stream_tweets(q = eu_bbox$bbox[[1]],
                                           timeout = 60,
                                           parse = T,
                                           token = v_1_token)

aust_random_tweets %>%
  group_by(country_code) %>%
  tally() %>%
  mutate(country_code = countrycode(country_code,origin = "iso2c",destination = "country.name"))
#bbox seems to cover more than Austria along, Germany makes sense but Crotia is a bit weird
#I can filter these out after scraping.

#actual streaming
#stream data with 5 min time-windows for 24 hours
starting_time<- "2021-05-13 11:53:20 CEST"

repeat{

for (i in 1:nrow(eu_bbox)) {
  
  country_name<-eu_bbox$country_name.x[i]
  rl<-rate_limits(token = v_1_token)
  print(paste0(country_name," ",Sys.time()))
  
  if(any(rl$remaining <= 2)){
    print("rate limit reached, sleeping 15 min.")
    Sys.sleep(15*60)
  }else{
  
  country_coords<- eu_bbox$bbox[[i]]
  tweet_stream<- stream_tweets(q = country_coords,
                               timeout = (60*5),
                               language = "en",
                               parse = T,
                               token = v_1_token)
  time<-as.character(Sys.time()) %>% gsub(":|\\s","-",.)
  saveRDS(tweet_stream,file = paste0(data.path,"TWT/",country_name,"_",time,".RDS"))
  }}
end_time<- Sys.time()  

time_diff<- as.numeric(difftime(end_time,starting_time, units = "days"))

if(time_diff >=7){
  break
}
}
