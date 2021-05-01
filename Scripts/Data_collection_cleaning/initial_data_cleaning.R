packs = c("tidyverse","qdapRegex")
lapply(packs,library, character.only = T)

dataDIR = paste0(getwd(),"/data")
#change the directory of social media data and variable names accordingly:
some_data<-readRDS(file = paste0(dataDIR,"/some_data.RDS")) %>% select(., user_id,screen_name,text)

#step 1: remove emojis ( use regex, its a lot faster)
#this is not really smart, there must be a better way of getting these
#but it works reasonably well.
emoji_regex<-"\\p{So}|\\p{Cn}|\U0001f3fb|\U0001f3fc"

some_data$no_emoji<- str_remove_all(string = some_data$text,pattern = emoji_regex)

#step 2: remove links
some_data$no_link<- qdapRegex::rm_twitter_url(text.var = some_data$no_emoji,trim = T,clean = T,pattern = "@rm_twitter_url")

#step 3: remove mentions
#I really need to master this regex business...
some_data$no_mention<- str_remove_all(string = some_data$no_link,pattern = "[@][\\w_-]+")

#step 4: remove hashtags:

some_data$no_hashtag<- str_remove_all(string = some_data$no_mention,pattern = "[#][\\w_-]+")

#step aggregate into a single document: 

some_agg<- some_data %>% group_by(user_id) %>% summarise(full_text = paste0(text, collapse = "."),
                                                         no_emoji = paste0(no_emoji,collapse = ". "),
                                                         no_link = paste0(no_link,collapse = "."),
                                                         no_mention = paste0(no_mention, collapse = "."),
                                                         no_hashtag = paste0(no_hashtag, collapse = "."))

saveRDS(object = some_agg, file = paste0(dataDIR,"/some_corpus_ready.RDS"))
