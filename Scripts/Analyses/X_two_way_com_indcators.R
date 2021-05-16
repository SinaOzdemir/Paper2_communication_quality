#interactivity indicators

# setup -------------------------------------------------------------------

library(tidyverse)
library(Hmisc)
library(academictwitteR)
library(igraph)
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


# network graph of replies ------------------------------------------------
#reply_to_user_id is missing in the clean data.
#lets quickly add it

eu.data.path<- "C:/Users/sinaf/OneDrive - NTNU/Projects/communication_quality_repo/data/EU/rds/"
eu.data.list<- list.files(path = eu.data.path, pattern = "*.RDS",full.names = T)
eu.reply.network<- map_dfr(eu.data.list, readRDS) %>% select(author_id,tweet_in_reply_to_user_id)

eu.edge.list<- eu.reply.network %>%
  group_by(author_id,tweet_in_reply_to_user_id) %>% 
  summarise(weight = n()) %>%
  arrange(desc(weight)) %>%
  rename(from = author_id , to =  tweet_in_reply_to_user_id) %>%
  mutate(to = ifelse(from == to, NA, to)) %>% drop_na(to)


eu.node.list<- eu.edge.list %>%
  pivot_longer(cols = c("from","to"),names_to = "variable", values_to = "labels") %>%
  select(labels) %>%
  distinct(labels)

eu.top5.edge <- eu.edge.list %>% group_by(from) %>% slice_max(order_by = weight, n = 5)

eu.top5.node <- eu.node.list %>% filter(labels %in% eu.top5.edge$from || labels%in%eu.top5.edge$to)

network.dta<-graph_from_data_frame(d = eu.top5.edge,vertices = eu.top5.node,directed = T)

E(network.dta)$width<-eu.top5.edge$weight



#this looks hideous There are some good visualization ideas here https://www.jessesadler.com/post/network-analysis-with-r/?fbclid=IwAR3O0zFbSF0V0UjMXVhQASJy4ft_7InhMtfArPK3AOuylNOUL-noQmV8y5g
plot(network.dta)