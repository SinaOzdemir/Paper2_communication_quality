#Analysis data creation:


# setup -------------------------------------------------------------------

library(tidyverse) #tidyverse 1.3.1

data.path<- paste0(getwd(),"/data/")

meta.data<- readRDS(paste0(data.path,"Accounts/EU_handcoded_meta.RDS"))
data.text<- readRDS(paste0(data.path,"corpii/EUTweetLanguageIndicators.RDS"))
data.corpus<- readRDS(paste0(data.path,"corpii/EUcorpus_cleaned.RDS"))

meta.data.imp<- meta.data %>% select(user_id,screen_name,Actor_type:Actor_role)
#text indicator script created duplicated tweets...
data.analysis<- left_join(data.corpus, data.text, by = c("tweet_id"))

data.analysis.unique<- data.analysis[!duplicated(data.analysis$tweet_id),]

meta.data.imp <- meta.data.imp %>%
  rename(author_id = user_id, user_username = screen_name) %>% 
  mutate(author_id = gsub("x","",author_id))


data.analysis.final<- left_join(data.analysis.unique,meta.data.imp, by = c("author_id","user_username"))
#row counts of clean corpus, text indicators and unique merged data don't have the same number of observations...

saveRDS(data.analysis.final, file = "./data/corpii/EU_analysis_data.RDS")
