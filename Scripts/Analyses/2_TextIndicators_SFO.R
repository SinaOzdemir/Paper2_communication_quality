########################################################
# Project:    EU Tweet
# Task:       Extract language-based indicators 
# Author:     Christian Rauh (03.05.2021)
########################################################

# Note:
# Highly contingent on prior text cleaning steps 
# a.o language detection (03.05. debugging needed!)
# Punctuation / HashTag choices
# all generated in: 1_TextCleaning.R

# All text indicators only extracted for english sentences in the tweets (tweet_text.en)


# Packages ####
library(tidyverse) # 1.3.0
library(quanteda) # 2.1.2
library(sophistication) # 0.70 # https://github.com/kbenoit/sophistication
library(spacyr) # 1.2.1 - requires Python and Spacy environment installed, see package documentation
library(textcat) # 1.0-7, n-gram based language detection


# Other tools ####

# Edited functions from sophistication package
# which exclude stopwords from calculating term familiarity
source("./Scripts/Analyses/X_covars_make_baselines_CR.R")



# Corpus ####
corpus <- read_rds("./data/corpii/EUcorpus_cleaned.RDS")


# Sample for testing purposes
# corpus <- corpus %>% sample_n(3000)


# Separate text data (to be gentle to the machine)
# status_id should be unique identifier for mergin later
df <- corpus %>% select(tweet_id, tweet_text.en)


# Quanteda corpus object
qcorp <- quanteda::corpus(df$tweet_text.en, docvars = data.frame(corpus[, "tweet_id"]))
#is this supposed to produce an empty named list?
docids <- docvars(qcorp) %>% # Keep quanteda ids for merging later
  mutate(doc_id = as.character(docid(qcorp)))



# Extract text indicators ####
# 21 mins on my machine, mostly consumed by POS tagging

start <- Sys.time()


# Reading ease scores, based on sophistication package
re <- covars_make(qcorp, readability_measure = "Flesch") 
re$doc_id <- paste0("text",rownames(re))


# Google N-Gram familiarity measures
# Based on modified function from sophistication package
fam <- covars_make_baselines_CR(qcorp, baseline_year = 2010) # Choosing the highest possible year for Goggle NGram Data
fam$doc_id <- rownames(fam)
#this function returned a lot of warnings
#Warning messages:
#1: 'texts.corpus' is deprecated.
#Use 'as.character' instead.
#See help("Deprecated")
#2: In FUN(X[[i]], ...) : no non-missing arguments to min; returning Inf



# Part-of-speech distributions (sophistication/spacyr)
# Puts out doc_id itself, order not necessarily correct
pos <- covars_make_pos(qcorp)
#over an hour and still counting.

# Combine data
indicators <- merge(docids[ ,c("doc_id", "status_id")],
                    re[, c("doc_id", "meanSentenceLength", "Flesch")],
                    by = "doc_id", all.x = T) %>% 
  rename(flesch = Flesch)

indicators <- merge(indicators,
                    fam[, c("doc_id","google_mean_local")], 
                    by = "doc_id", all.x = T) %>% 
  rename(familiarity = google_mean_local)

indicators <- merge(indicators,
                    pos[, c("doc_id","n_namedentities", "n_noun", "n_verb", "n_sentence", "ntoken")],
                    by = "doc_id", all.x = T)

df <- merge(df, indicators, by = "status_id", all.x = T) %>% 
  mutate(nominal = n_noun/n_verb,
         verbal = n_verb/n_noun) %>% 
  select(-doc_id)

# Duration
Sys.time()-start


# Inspection ####


# Export ####
write_rds(df, "./data/EUTweetLanguageIndicators.RDS")