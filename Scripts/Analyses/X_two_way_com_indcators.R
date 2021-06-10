#interactivity indicators

# setup -------------------------------------------------------------------
source(file = file.path(getwd(),"Scripts","Auxilary_scripts","two_way_interaction_analysis_functions.R"))
data.path<-file.path(getwd(),"data")

case_data<-data_reader(file_path = data.path,case = "EU")


# data cleaning -----------------------------------------------------------

case_data_clean<- data_cleaner(data = case_data)

# descriptive interactivity -----------------------------------------------

eu_scatter_plot_reply<- descriptive_plots(data = case_data_clean,
                                    case = "the EU",
                                    what = "reply",
                                    unit = "year",
                                    graph_type = "scatter")

eu_scatter_plot_quote<- descriptive_plots(data = case_data_clean,
                                          case = "the EU",
                                          what = "quote",
                                          unit = "year",
                                          graph_type = "scatter")

eu_scatter_plot_retweet<- descriptive_plots(data = case_data_clean,
                                          case = "the EU",
                                          what = "retweet",
                                          unit = "year",
                                          graph_type = "scatter")


(eu_scatter_plot_reply + plot_spacer() + eu_scatter_plot_quote) / (plot_spacer() + eu_scatter_plot_retweet + plot_spacer())

# network graph of interactivity ------------------------------------------------

test_data<- readRDS(file = list.files(file.path(getwd(),"data","EU","rds_clean"),pattern = "*.RDS",full.names = T)[1])

#reply source:

reply_data<- test_data %>% filter(is_reply == 1 & tweet_in_reply_to_user_id != author_id) %>%
  select(tweet_id,author_id,user_username,tweet_in_reply_to_user_id)


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