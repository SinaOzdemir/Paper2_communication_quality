#Political responsibiltiy indicators



# setup -------------------------------------------------------------------

packs<- c("tidyverse","spacyr")
pacman::p_load(char = packs)
data.path<- paste0(getwd(),"/analysis_data/")
#reading the sample data 
data<- readRDS(paste0(data.path,"EUcorpus_cleaned_sample.RDS"))
##################################
#Need to track down a policy dictionary!
# CAP and MIA datasets don't have their dictionaries online
# I'll contact the primary authors.
##################################


# Identifying personal pronoun/noun + action verb combinations ------------

# This indicator should be combined with policy related tweets indicator
# later on in order to pin-point political responsibility reporting.
data_eng<- data %>% filter(langlang %in%"en")

#test with spacyR
spacy_initialize()
#first lets remember how spacy worked
data_pos<- spacy_parse(x = data$tweet_text,
                       pos = T,
                       tag = T,
                       entity = T,
                       dependency = T,
                       nounphrase = T)

#document ID is problematic, need to find a way to transfer document IDs from
#the main dataset to POS dataset (we'll pick this up tomorrow)