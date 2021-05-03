###############################################################
# Project:    EU Tweet
# Task:       Visual descriptive overview  of EU Tweet Sample
# Author:     Christian Rauh (03.05.2021)
##############################################################

# Packages ####
library(tidyverse) # 1.3.0
library(patchwork) # 1.1.1
library(ggridges) # 0.5.2
library(ggtext) # 0.1.1


# The data ####

# Cleaned EUTweet corpus
# as per 1_TextCleaning.R

df <- read_rds("./tweetcorpora/EUtweets_cleaned.RDS") %>% 
  filter(!is_retweet) %>%                                                # Drop mere re-tweets
  select(c(screen_name, name, status_id, created_at, account_created_at, # Keep basic meta
           hashtags, emoji, mentions_screen_name, lang.num,                           # Keep info on non-text content
           media_type, media_expanded_url, ext_media_expanded_url, urls_expanded_url, # Keep infor on media and links
           favorite_count, retweet_count, followers_count)) %>%                        # Keep engagagement info
  rename(date = created_at)

# Questions:

# What is favorite_count as opposed to favourites_count? 
# Per plot very different things 
# ggplot(df, aes(x= favorite_count, y = favourites_count))+
#   geom_point()

# Do we have/get info on quote retweets ?


# Text-based indicators
# As per 2_TextIndicators.R

df2 <- read_rds("./data/EUTweetLanguageIndicators.RDS") %>% 
  select(status_id, flesch, familiarity, verbal, 
         ntoken, n_sentence, n_noun, n_verb, n_namedentities)


# Raw account classification (by Serra Erkenci)
accounts <- read.csv2("./data/EUaccountsCoding_Serra.csv") %>% 
  select(screen_name, commission, account_type)

accounts$account_type <- accounts$account_type %>% 
  recode(`3` = 2L) %>% # Subsume office accounts into institional accounts (effectively Ombudsman and eucopresident)
  recode(`1` = "Personal Accounts",
         `2` = "Institutional Accounts") %>% 
  factor(levels = c("Personal Accounts", "Institutional Accounts"))

accounts$commission <- as.logical(accounts$commission)


# Combine data sets

df <- df %>% 
  left_join(df2, by = "status_id")
rm(df2)

df <- df %>% 
  left_join(accounts, by = "screen_name")
rm(accounts)


# Clean variables ####

# Timestamps
df$day <- df$date %>% 
  as.character() %>% 
  str_extract(".*? ") %>% 
  str_trim()
df$month <- df$day %>% 
  str_remove("-[0-9]{1,2}$")



# Overarching ggplot params ####

theme_set(theme_light() +
            theme(legend.position = "none",
                  axis.text = element_text(color = "black"),
                  plot.title = element_text(size=10)))

accountcolors <- c("#FFCC00", "#003399") # EU, all the way down ...



# Volume ####
#############

# Average monthly n of tweets

monthlyv <- df %>% 
  group_by(account_type, month) %>% 
  summarise(tweets = n())

pl.volume.month <-
  ggplot(monthlyv, aes(x = account_type, y = tweets, colour = account_type))+
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = 1) +
  scale_color_manual(values = accountcolors)+
  coord_flip()+
  labs(title = "Average number of tweets per month",
       x = "",
       y = "")+
  theme(axis.text.y = element_blank())


# Monthly number of tweets

# A bit of a detour here to ensure to have the full list of months
# Independet of whether they are NA in the current EU Tweet sample
months <- seq(min(df$date), max(df$date), by = "months") %>% # Full range of months in the data
  as.character() %>% 
  str_extract("^[0-9]{4}-[0-9]{2}") %>% 
  as.data.frame() %>% 
  rename(month = 1)

months <- months %>% 
  left_join(monthlyv[monthlyv$account_type == "Institutional Accounts", ], by = "month") %>% 
  select(month, tweets) %>% 
  rename(inst = tweets)

months <- months %>% 
  left_join(monthlyv[monthlyv$account_type == "Personal Accounts", ], by = "month") %>% 
  select(-account_type) %>% 
  rename(personal = tweets)

monthlyv2 <- months %>% 
  pivot_longer(cols = 2:3) %>% 
  rename(account_type = name,
         tweets = value)

monthlyv2$account_type <- monthlyv2$account_type %>% 
  str_replace("inst", "Institutional Accounts") %>% 
  str_replace("personal", "Personal Accounts") %>%
  factor(levels = c("Personal Accounts", "Institutional Accounts"))

# time series params

t.breaks <- unique(months$month) %>% # Extract Januaries
  as.data.frame() %>% 
  rename(month = 1) %>% 
  filter(str_detect(month, "-01"))
t.breaks <- t.breaks[,1] # Atomic vector ...
t.labels <- t.breaks %>% # Years in the data
  str_remove("-01") 


# Plot

pl.volume.time <- 
  ggplot(monthlyv2, aes(x=month, y = tweets, fill = account_type, group = 1))+
  geom_bar(stat = "identity", position = "stack", width = .5)+
  # geom_line()+
  scale_x_discrete(breaks = t.breaks, labels = t.labels)+
  scale_fill_manual(values = accountcolors)+
  labs(title = "Number of tweets per month (NOTE: Many months with missing data!!!)",
       x = "",
       y = "")


# Number of words in tweets

pl.words <-
  ggplot(df, aes(x = account_type, y = ntoken, colour = account_type))+
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = 1) +
  scale_color_manual(values = accountcolors)+
  coord_flip()+
  labs(title = "Average number of words per tweet",
       x = "",
       y = "")


# Combine volume plots

pl.volume <- (pl.volume.month + pl.words) / pl.volume.time + 
  plot_annotation(title = "How much do supranational actors tweet?",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          size = 14, face = "bold")))+
  plot_layout(heights = c(1, 2))

ggsave("./plots/DescriptiveOverview/Volume.png", width = 20, height = 15, units = "cm")



# Engagement ####
#################

# Hearts
pl.favs <-
  ggplot(df, aes(y = favorite_count, x = month, color = account_type, group = account_type))+
  # stat_summary(geom = "pointrange", fun.data = "mean_cl_boot") +
  stat_summary(geom = "line", fun = "mean", na.rm = T)+
  # geom_point()+
  scale_x_discrete(breaks = t.breaks, labels = t.labels)+
  scale_color_manual(values = accountcolors)+
  labs(title= "Average number of favorites per tweet and month",
       color = "",
       x= "", y = "")+
  theme(legend.position = c(0.2,.88),
        legend.background = element_blank())

# Retweets
pl.rets <- 
  ggplot(df, aes(y = retweet_count, x = month, color = account_type, group = account_type))+
  # stat_summary(geom = "pointrange", fun.data = "mean_cl_boot") +
  stat_summary(geom = "line", fun = "mean", na.rm = T)+
  # geom_point()+
  scale_x_discrete(breaks = t.breaks, labels = t.labels)+
  scale_color_manual(values = accountcolors)+
  labs(title= "Average number of retweets per tweet and month",
       color = "",
       x= "", y = "")    

# Combine
pl.engagement <-
  pl.favs/pl.rets +
  plot_annotation(title = "How much do users engage with tweets from supranational actors?",
                  caption = "Note that both favorite and retweet counts follow an extremely right-skewed distribution\nand are subject to strong individual outliers.",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          size = 14, face = "bold")))

ggsave("./plots/DescriptiveOverview/Engagement.png", width = 20, height = 15, units = "cm")
