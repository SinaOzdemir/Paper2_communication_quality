#interactivity indicators

# setup -------------------------------------------------------------------

library(tidyverse)
library(Hmisc)
data.path<-paste0(getwd(),"/data/corpii")
graph.path<- paste0(getwd(),"/plots/")
data.eu<-readRDS(file = paste0(data.path,"/EUcorpus_cleaned.RDS"))
meta.data.eu<-
#lets get a quick data summary-------------

data_summary<- Hmisc::contents(data.eu)[[1]] %>% as_tibble(rownames = "var_names")

#this should be empty now:
na_vars<- data_summary %>%
  filter(NAs >0) %>% pull(var_names) %>%
  grep(pattern = "is_*",ignore.case = T,perl = T,value = T,x = .)
#replace na's in interaction indicators

sample_data_reduced<- sample_data_reduced %>% mutate(across(.cols = all_of(na_vars), ~replace_na(.x,0))) %>%
  mutate(tweet_date = lubridate::as_date(.$tweet_created_at)) %>%
  mutate(tweet_year = lubridate::floor_date(tweet_date,unit = "year"))

#descriptive data eu:
yearly_volume <- data.eu %>% group_by(tweet_year) %>% tally()

#all columns are saved as character to ensure homogeneity in earlier processes
#now we need a few things as numeric

data.eu<- data.eu %>% mutate(across(matches("is_*"),~as.numeric(.x)))

eu_interactions_summary<- data.eu %>%
  group_by(tweet_year) %>%
  summarise(yearly_retweet_share = (sum(is_retweet)),
            yearly_quote_share = (sum(is_quote)),
            yearly_reply_share = (sum(is_reply))) %>%
  left_join(x = .,y = yearly_volume, by = "tweet_year") %>% 
  mutate_at(vars(c("yearly_retweet_share","yearly_quote_share","yearly_reply_share")), ~(./n)) %>%
  pivot_longer(cols = starts_with("yearly_"),names_to = "variable",values_to = "share")

yearly_interactivity<- eu_interactions_summary %>%
  ggplot(data=.,aes(x = tweet_year, y = share)) +
  geom_bar(aes(fill = share),stat = "identity",position = "dodge")+theme_minimal()+
  facet_wrap(~variable)+
  labs(x = "Year",y = "Share",title = "EU verified accounts interactivity rate",subtitle = "N accounts = 115 \n N tweets = 1065203")

ggsave(filename = "yearly_interaction_rate.jpeg",plot = yearly_interactivity,path = graph.path)

######between accounts comparison of interactivity######
eu_acc_vol <- data.eu %>% group_by(user_name) %>% tally(name = "tweet_n")

eu_intr_comp <- data.eu %>%
  group_by(user_name) %>%
  summarise(yearly_retweet_share = (sum(is_retweet)),
            yearly_quote_share = (sum(is_quote)),
            yearly_reply_share = (sum(is_reply))) %>% 
  right_join(x = .,y = eu_acc_vol, by = "user_name") %>% 
  mutate_at(vars(c("yearly_retweet_share","yearly_quote_share","yearly_reply_share")),~(./tweet_n)) 
#####retweet#####
eu_intr_retweet_p<- eu_intr_comp %>% ggplot(aes(x= user_name,y =yearly_retweet_share))+
  geom_point(aes(color = yearly_retweet_share))+
  theme_minimal()+geom_text(aes(label = user_name),hjust = 0, vjust = 0)+
  scale_x_discrete(labels = NULL, breaks = NULL) +
  theme_minimal()+
  labs(x = "",y = "Share", title = "Retweet of EU accounts",subtitle = "Normalized by total number of tweets")
#It feels like I am looking at almost perfect RNG...

ggsave(filename = "inter_account_retweet.jpeg",plot = eu_intr_retweet_p,path = graph.path,units = "cm",width = 20,height = 15)
#this looks ugly as hell in export, need to re-adjust dimensions

####reply########
eu_intr_reply_p<- eu_intr_comp %>% ggplot(aes(x= user_name,y =yearly_reply_share))+
  geom_point(aes(color = yearly_reply_share))+
  theme_minimal()+geom_text(aes(label = user_name),hjust = 0, vjust = 0)+
  scale_x_discrete(labels = NULL, breaks = NULL) +
  theme_minimal()+
  labs(x = "",y = "Share", title = "Reply by EU accounts",subtitle = "Normalized by total number of tweets")
#so they are not big on replying except for EU trade and Commissioner for aggriculture
ggsave(filename = "inter_account_reply.jpeg",plot = eu_intr_reply_p,path = graph.path,units = "cm",width = 20,height = 15)

#####quote#####
eu_intr_quote_p <- eu_intr_comp %>% ggplot(aes(x= user_name,y =yearly_quote_share))+
  geom_point(aes(color = yearly_quote_share))+
  theme_minimal()+geom_text(aes(label = user_name),hjust = 0, vjust = 0)+
  scale_x_discrete(labels = NULL, breaks = NULL) +
  theme_minimal()+
  labs(x = "",y = "Share", title = "Quotes by EU accounts",subtitle = "Normalized by total number of tweets")
#they don't seem to be big on quoting in general either.
ggsave(filename = "inter_account_quote.jpeg",plot = eu_intr_quote_p,path = graph.path,units = "cm",width = 20,height = 15)


#would be great to look at who they reply, retweet and quote with a weighted network graph.
