# Cleaning and reshaping the data coming from academic API 2.0
# This will be integrated to data_scraping script later on.
#most variables are list of lists
#there is no convenient way of identifying empty lists in a list
#because of hierarchical data structure, (eg.data$list_of_lists[1] is not null because it contains a list which is empty)
#recursive functions seems to be a way to solve this (https://stackoverflow.com/questions/38247954/find-empty-lists-in-nested-list-of-lists)
#but they are definitely not a one line solution either, so might as well do loop in loop across dataset.

######Tasks######:

#1) Academic API 2.0 returned data with different dimensions they need to be harmonized
#2) Extracting and flattening the important indicators from json
    #is_retweet
    #is_reply
    #mentioned screen name
    #reply_to_status_id
    #reply_to_screen_name
    #contains media
    #media type (if possible)
#################.

# setup -------------------------------------------------------------------
library(pacman)
packs<- c("tidyverse","jsonlite")
#I haven't updated my packages in a while,if you have these packages and they are up to date, ignore install = T, update = T
pacman::p_load(char = packs,install = T,update = T,character.only = T)
rdsdataDIR<- paste0(getwd(),"/Data/rds/")
files<- list.files(path = rdsdataDIR,pattern = ".RDS",full.names = T)
file_names<- list.files(path = rdsdataDIR,pattern = ".RDS",full.names = F) %>% gsub(".RDS","",.)
model_data<- readRDS(file.choose())


# Step 1: even out the dimensions -----------------------------------------


##Task - 1: diagnosing excess columns----


a<-readRDS(file = files[1])
b<-Hmisc::contents(a)[[1]] %>%
  as_tibble(rownames = "var.names") %>%
  mutate(screen_name = rep(file_names[1],times = nrow(.)))

meta_extra <- function(file_path,file_names){
  a <- readRDS(file = file_path)
  b <- Hmisc::contents(a)[[1]] %>%
    as_tibble(rownames = "var.names") %>% 
    mutate(screen_name = rep(file_names, times = nrow(.)))
  return(b)
}


ncol_meta_data<- map2(.x = files, .y = file_names, .f = meta_extra)

#lets see which columns are problematic:
#if all the columns appear in equal number (i.e all the data is same dimension)
#their count number should be the same

ncol_meta_data %>%
  group_by(var.names) %>%
  summarise(col_count = n()) %>% ggplot(data = .,aes(x = var.names,y = col_count))+
  geom_bar(aes(fill = col_count),stat = "identity", position = "dodge")

#so geo and withheld do not appear in all of the dataframes.
#geo field refers to the geo-tag for the tweet. Usually empty, and not necessary for our purpose
#withheld field is more interesting. It refers to 
#"...if we receive a valid and properly scoped request from an authorized entity,
#it may be necessary to withhold access to certain content
#in a particular country from time to time." (https://help.twitter.com/en/rules-and-policies/tweet-withheld-by-country)
#It is interesting but irrelevant right now.
#In any case they are very easy to handle.

##Task - 2: remove the excess dimensions----
#add this to cleaning function then we are done:

twitter_rds_reader<- function(file_path){
  if(!require(tidyverse)){
    library(tidyverse)
  }
  a<- readRDS(file_path) %>% as_tibble()
  
  c_names_a<-colnames(a)
  
  if("geo"%in%acolname | "withheld"%in%acolname){
    
  a<- a %>% select(.,-any_of(c("geo","withheld"))) %>% jsonlite::flatten()
  
  return(a)
  
  }else{
    a<- a %>% jsonlite::flatten()
    
  }
}


# Step2: create important variables --------------------------------------
#read a test data

test_data<- twitter_rds_reader(file_path = files[1])

##Task 1 - unpack entities.referenced tweets and create desired variables:----
twitter_data<- test_data

for (i in 1:nrow(twitter_data)) {
  if(is.null(twitter_data$referenced_tweets[[i]])){
    twitter_data$referenced_tweets[i]<-NA
  }
  
}

twitter_data<- twitter_data[!is.na(twitter_data$referenced_tweets),] %>%
  select(id,referenced_tweets) %>%
  group_by(id) %>%
  unnest(cols = c(referenced_tweets),names_sep = "_") %>%
  right_join(x = .,y=twitter_data, by = "id")

#create is_retweet, is_quote, is_reply and reply_to_status_id for later calculations

twitter_data <- twitter_data %>% mutate(is_retweet = ifelse(.$referenced_tweets_type == "retweeted",1,0),
                                        retweet_status_id = ifelse(.$referenced_tweets_type == "retweeted",.$referenced_tweets_id,NA),
                                        is_reply = ifelse(.$referenced_tweets_type == "replied_to",1,0),
                                        reply_to_status_id = ifelse(.$referenced_tweets_type == "replied_to",.$referenced_tweets_id,NA),
                                        is_quote = ifelse(.$referenced_tweets_type == "quoted",1,0),
                                        quoted_status_id = ifelse(.$referenced_tweets_type =="quoted",.$referenced_tweets_id,NA))

##Task 2 - create mentioned_screen_name variable:----

a<- test_data %>% select(id, entities.mentions)

b<- a %>% group_by(id) %>% unnest(cols = c(entities.mentions))

#this works but I have no clue how it works. I found it on stackoverflow(https://stackoverflow.com/questions/49054737/data-frame-rows-to-nested-list-elements-by-groups)
df.split <-  plyr::dlply(.data = b, .variables = "id",.fun = function(i) i[["username"]])

df_mentions <- data.frame(id = names(df.split))

df_mentions$mentioned_names<- df.split

test_data <- test_data %>% select(-entities.mentions) %>% left_join(x = ., y= df_mentions, by = "id")

##Task 3 - hastags: (will follow the same logic as task2)----

test_data_b<- test_data %>%
  select(id, entities.hashtags) %>%
  group_by(id) %>%
  unnest(cols = c(entities.hashtags)) %>% 
  plyr::dlply(.data = . , .variables = "id",.fun = function(i)i[["tag"]]) %>% 
  tibble(id = names(.),tags = .) %>%
  right_join(x = .,y = test_data,by = "id")
  
##Task 4 - contains media:----
#attachments.media_keys is a list of lists, I need to find a vectorized way to identify empty lists
#in a list of lists so I don't have to loop over the data for every variable...
for(i in 1:nrow(test_data)){
  if(is.null(test_data$attachments.media_keys[[i]])){
    test_data$attachments.media_keys[i]<- NA
  }else{
    test_data$attachments.media_keys[i]<-test_data$attachments.media_keys[i]
  }
}

test_data$contains_media <- ifelse(is.na(test_data$attachments.media_keys),0,1)

##Task 5 - type of media:----

#type of media is another field that needs to be requested which the package I used didn't
#(probably for the sake of convenience)
#While it is interesting in and of itself, it is not in the scope the paper so I am not going to
#try to solve it.

##Task 6 - reply_to_status_id:----

test_data$reply_to_status_id<- ifelse(test_data$ref_type == "replied_to", test_data$ref_twt_id,NA)



##Task 7 - Putting it all together into a single function:----

# function is intended to work with either with an object in the memory
# or a local RDS file. To use the function, convert the native JSON file from the API to RDS

twitter_rds_cleaner<- function(file,read = F,save = F, saveDIR = NULL){
  #if the function will be used with an existing RDS on a local drive,
  #the function can read it now. User needs to provide full path to the file
  if(isTRUE(read)){
  twitter_data<- readRDS(file)
  }else{
    #the function assumes that it will be used with an R object in the global env.
    
    twitter_data <- file
  }
  
  
  #make the dimensions even----
  c_names_a<-colnames(twitter_data)
  
  if("geo"%in%acolname | "withheld"%in%acolname){
    #removes the infrequent columns (geo and withheld) from the dataset
    #to make it competable for bulking
    twitter_data<- twitter_data %>%
      select(.,-any_of(c("geo","withheld"))) %>%
      jsonlite::flatten()
    
   }else{
    twitter_data<- twitter_data %>% jsonlite::flatten()
    
  }
  
  #referenced_tweets for retweets, quotes and replies----
  
  for (i in 1:nrow(twitter_data)) {
    if(is.null(twitter_data$referenced_tweets[[i]])){
      twitter_data$referenced_tweets[i]<-NA
    }
    
  }
  
  twitter_data<- twitter_data[!is.na(twitter_data$referenced_tweets),] %>%
    select(id,referenced_tweets) %>%
    group_by(id) %>%
    unnest(cols = c(referenced_tweets),names_sep = "_") %>%
    right_join(x = .,y=twitter_data, by = "id")
  
  #create is_retweet, is_quote, is_reply and reply_to_status_id for later calculations
  
  twitter_data <- twitter_data %>% mutate(is_retweet = ifelse(.$referenced_tweets_type == "retweeted",1,0),
                                          retweet_status_id = ifelse(.$referenced_tweets_type == "retweeted",.$referenced_tweets_id,NA),
                                          is_reply = ifelse(.$referenced_tweets_type == "replied_to",1,0),
                                          reply_to_status_id = ifelse(.$referenced_tweets_type == "replied_to",.$referenced_tweets_id,NA),
                                          is_quote = ifelse(.$referenced_tweets_type == "quoted",1,0),
                                          quoted_status_id = ifelse(.$referenced_tweets_type =="quoted",.$referenced_tweets_id,NA))
  
  #entities: hashtags and mentions----
    #hashtags
  twitter_data<- twitter_data %>%
    select(id, entities.hashtags) %>%
    group_by(id) %>%
    unnest(cols = c(entities.hashtags)) %>% 
    plyr::dlply(.data = . , .variables = "id",.fun = function(i)i[["tag"]]) %>% 
    tibble(id = names(.),hashtags = .) %>%
    right_join(x = .,y = twitter_data,by = "id")
  
    #mentions
  twitter_data<- twitter_data %>%
    select(id, entities.mentions) %>%
    group_by(id) %>%
    unnest(cols = c(entities.mentions)) %>% 
    plyr::dlply(.data = . , .variables = "id",.fun = function(i)i[["username"]]) %>% 
    tibble(id = names(.),mentioned_username = .) %>%
    right_join(x = .,y = twitter_data,by = "id")
  
  
  #contains media ----
  for(i in 1:nrow(twitter_data)){
    if(is.null(twitter_data$attachments.media_keys[[i]])){
      twitter_data$attachments.media_keys[i]<- NA
    }else{
      twitter_data$attachments.media_keys[i]<-twitter_data$attachments.media_keys[i]
    }
  }
  
  twitter_data$contains_media <- ifelse(is.na(twitter_data$attachments.media_keys),0,1)
  
  #save modified rds----
  if(isTRUE(save)){
    #User needs to provide a save directory if 
    #modified dataset needs to be saved
    #!!!!IMPORTANT!!!
    #The directory should not end with /
    #otherwise the function will not save properly.
    user_name<- twitter_data$author_id[1]
    saveRDS(twitter_data,file = paste0(saveDIR,"/",user_name,".RDS"))
  }else{
    return(twitter_data)
  }
  #finally return the modified dataset.
  
}


# Step 3 - Test the function ----------------------------------------------
#testing is done with dreynders.RDS
##task 1 - test with a dataset in the memory:

func_test_data<- readRDS(file = file.choose())

func_data<- twitter_rds_cleaner(file = func_test_data)

#function works with an object in memory and returns 32 variables.

##task 2 - test with an rds in harddrive

func_data2<- twitter_rds_cleaner(file = files[11],read = T)
#function works with an rds in local and returns 32 variables

## task 3 - test read-write with an rds in memory
dir.create(path = paste0(getwd(),"/Data/test_rds"))
save_test_DIR<- paste0(getwd(),"/Data/test_rds")

twitter_rds_cleaner(file = files[11],read = T,save = T,saveDIR = save_test_DIR)

func_data3<- readRDS(file.choose())

#save attribute works as well!


#function can be used with map, lapply, or a simple for loop