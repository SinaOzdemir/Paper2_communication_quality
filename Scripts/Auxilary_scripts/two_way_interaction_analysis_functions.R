#two-way interaction indicator functions:


# read packages -----------------------------------------------------------



packs<- c("tidyverse","Hmisc","rtweet","igraph","ggraph")
existing_packs<-installed.packages()[,1]

package_loader<- function(packs = character(0)){
  existing_packs<- installed.packages()[,1]
  if(all(packs%in%existing_packs)){
    lapply(packs,library, character.only = T)
  }else{
    missing_packages<- packs[-which(packs%in%existing_packs)]
    install.packages(missing_packages)
    lapply(packs,library(character.only = T))
  }
  
}

package_loader(packs = packs)

# helper functions --------------------------------------------------------

make.dir<- function(file.path){
  if(dir.exists(file.path)){
    return(file.path)
  }else{
    dir.create(path = file.path, recursive = T)
    return(file.path)
  }
}


# variable manipulators ---------------------------------------------------

data_cleaner<- function(data){
  
  clean_data<- data %>%filter(langlang %in% "en") %>% 
    mutate(across(.cols = starts_with("is_"), ~tidyr::replace_na(.x, 0))) %>%
    mutate(across(.cols = starts_with("is_"),~as.numeric(.x))) %>% 
    mutate(tweet_date = lubridate::as_date(.$tweet_created_at)) %>%
    mutate(tweet_year = lubridate::floor_date(tweet_date,unit = "year"),
           tweet_month = lubridate::floor_date(tweet_date,unit = "month"),
           tweet_week = lubridate::floor_date(tweet_date,unit = "week"),
           tweet_day = lubridate::floor_date(tweet_date,unit = "day"),
           tweet_calendar_day = weekdays(tweet_date))
  
  return(clean_data)
    
}


descriptive_plots<- function(data,what = c("reply","quote","retweet"),unit = c("year","month"))