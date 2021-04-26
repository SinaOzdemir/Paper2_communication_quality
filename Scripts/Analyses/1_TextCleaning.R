#############################################################
# Project:  EU Tweet
# Task:     Extract text-based indicators from EUtweet sample
# Author:   @ChRauh (26.04.2021)
#############################################################


# Packages ####
library(tidyverse) # 1.3.0
library(quanteda) # 2.1.2
library(sophistication) # 0.70 https://github.com/kbenoit/sophistication
library(spacyr) # 1.2.1 - requires Python and Spacy environment installed, see package documentation
library(textcat) # 1.0-7


# EUtweet corpus ####
corpus <- read_rds("./tweetcorpora/EUtweets.RDS")

# Keep copy of raw tweet text
corpus <- corpus %>% mutate(text_raw = text)


# Language detection ####

# Twitter language category contains some strange languages ('eu', 'uk'?)
# This here is a cross-check

# corpus$language <- textcat(corpus$text) # takes quite some time
# lang.matrix <- table(corpus$lang, corpus$language, useNA = "ifany") %>%
#   as.data.frame.matrix()
# write_rds(lang.matrix, "./data/LanguageCatMatrix.RDS")
# lang.df <- table(corpus$lang, corpus$language, useNA = "ifany") %>%
#   as.data.frame() %>%
#   rename(twitter.lang = Var1,
#          textcat.lang = Var2)
# write_rds(lang.df, "./data/LanguageCatDF.RDS")
# 
# text.eng <- lang.df %>% 
#   filter(textcat.lang == "english" & Freq > 0) %>% 
#   arrange(desc(Freq))
# 
# twitter.eng <- lang.df %>% 
#   filter(twitter.lang == "en" & Freq > 0) %>% 
#   arrange(desc(Freq))
# 
# discrepancy1 <- corpus %>% 
#   filter(language == "english" & lang != "en") %>% 
#   select(text, lang, language)
# 
# discrepancy2 <- corpus %>% 
#   filter(language != "english" & lang == "en") %>% 
#   select(text, lang, language)

# LESSON: It's actually multi-lingual tweets!

# We need to mark them somehow (check out also cld3::detect_language_multi())
# And then extract only the english-parts, based on sentence tokenization, language detection, and paste(collapse)
# Did something similar for the Irish Press release already


# Clean typical HTML encoding issues ####

# Manual extraction of (possible) HTML characetr references
one.doc <- paste(corpus$text, collapse = " ")
faulty <- str_extract_all(one.doc, "&[a-z0-9].*? ") %>% 
  unlist() %>% 
  str_trim() %>% 
  as.data.frame() %>% 
  rename(pattern = 1) %>% 
  group_by(pattern) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Correct encoding issues
corpus$text <- corpus$text %>% 
  str_replace_all(fixed("&amp;"), " & ") %>% 
  str_replace_all(fixed("&gt;"), " > ") %>% 
  str_replace_all(fixed("&lt;"), " < ") %>% 
  str_replace_all("//s+", " ") #Multiple white spaces (of different type) to one raw whitespace

# Remove URLs from tweet text

# Remove or better disambiguate Hash Tags from text

# Count and remove emoticons


