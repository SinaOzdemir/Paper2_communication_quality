#interactivity indicators

#lets get a quick data summary-------------

data_summary<- Hmisc::contents(sample_data_reduced)[[1]] %>% as_tibble(rownames = "var_names")
na_vars<- data_summary %>% filter(NAs >0) %>% pull(var_names) %>% grep(pattern = "is_*",ignore.case = T,perl = T,value = T,x = .)
#replace na's in interaction indicators

sample_data_reduced<- sample_data_reduced %>% mutate(across(.cols = all_of(na_vars), ~replace_na(.x,0))) %>%
  mutate(tweet_date = lubridate::as_date(.$tweet_created_at)) %>%
  mutate(tweet_year = lubridate::floor_date(tweet_date,unit = "year"))
yearly_volume <- sample_data_reduced %>% group_by(tweet_year) %>% tally()

interactions_summary<- sample_data_reduced %>%
  group_by(tweet_year) %>%
  summarise(yearly_retweet_share = (sum(is_retweet)),
            yearly_quote_share = (sum(is_quote)),
            yearly_reply_share = (sum(is_reply))) %>%
  left_join(x = .,y = yearly_volume, by = "tweet_year") %>% 
  mutate_at(vars(c("yearly_retweet_share","yearly_quote_share","yearly_reply_share")), ~(./n)) %>%
  pivot_longer(cols = starts_with("yearly_"),names_to = "variable",values_to = "share")

yearly_interactivity<- interactions_summary %>%
  ggplot(data=.,aes(x = tweet_year, y = share)) +
  geom_bar(aes(fill = share),stat = "identity",position = "dodge")+facet_wrap(~variable)