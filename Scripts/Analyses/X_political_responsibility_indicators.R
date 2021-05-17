#Political responsibiltiy indicators



# setup -------------------------------------------------------------------

packs<- c("tidyverse","spacyr")
pacman::p_load(char = packs)
data.path<- paste0(getwd(),"/analysis_data/")
##################################
#Need to track down a policy dictionary!
# CAP and MIA datasets don't have their dictionaries online
# I'll contact the primary authors.
##################################


# Identifying personal pronoun/noun + action verb combinations ------------

# This indicator should be combined with policy related tweets indicator
# later on in order to pin-point political responsibility reporting.

#test with spacyR
spacy_initialize()
#first lets remember how spacy worked

#sandbox

#spacy is extremely sensitive to typos and mistakes
sandbox<- c(tweet1 = "The EU commission has concluded an investigation against Poland due to recent events and found serious breaches of rule-of-law. Poland will be kicked out of the union",
            tweet2 ="Amazing organization. I attended the first TTNET high-level dialog conference",
            tweet3 ="ECB announced new measures against inflation",
            tweet4 ="After consultation with relevant stakeholders, we decided to stop vaccination purchases",
            tweet5 ="This day coudln't get any better!I received my mission letter to ensure our European way of life #EU#OurWay!! Great honor to be part of vdL")

sandbox_parse<- spacy_tokenize(sandbox[3],"sentence",
                               remove_punct = T,
                               remove_url = T,
                               remove_numbers = T,
                               remove_separators = T,
                               remove_symbols = T,
                               output = "data.frame")
#so here sentences become text if I tokenize tweets into sentences firs
resp_pos_tag<- spacy_parse(x = sandbox,
                      lemma = F,
                      pos = T,
                      tag = T,
                      entity = T,
                      dependency = T,
                      nounphrase = T)
#yayks spacy thinks emojis are nouns... better use twitter tagger.
tweet_grammar <- resp_pos_tag %>%
  group_by(doc_id,sentence_id) %>%
  summarise(grammar_tag = paste(tag, collapse = "+"),
            grammar_pos = paste(pos, collapse = "+"))
#ok so if text is named list, names of the list items are transferred as doc_id
#this turns the data one sentence per line, so its fine if I save the data as a seperate dataset

#test with the data:

#reading the sample data 
data<- readRDS(paste0(data.path,"EUcorpus_cleaned_sample.RDS"))

tweets<- data %>% filter(langlang == "en") %>% pull(tweet_text)
#there are some problems in the preprocessed tweets. Obs 960 still has emojis for some reason. there is also a character with unicode \ufe0f
# lets ignore this for now

names(tweets)<- data %>% filter(langlang == "en") %>% pull(tweet_id)

tweets_pos_tag<- spacy_parse(x = tweets, pos =T,
                             tag = T,
                             lemma = F,
                             entity = F,
                             nounphrase = T) %>% nounphrase_consolidate(concatenator = " ") 
#noun phrase recognition is not very accurate, it recognizes things like "reserachers", "freedom" etc. as nounphrase.
#takes a couple of seconds for 2502 observations

tweets_grammar_df<- tweets_pos_tag %>% filter(pos != "PUNCT") %>% 
  group_by(doc_id,sentence_id) %>%
  summarise(grammar_pos = paste(pos, collapse = "+"),
            grammar_tag = paste(tag, collapse = "+"),
            sentences = paste(token, collapse = " "))


#Naive pos is not enough to recognize responsibility grammar where it identifies agent in an action
#there are some alternative things I can try:
#https://en.wikipedia.org/wiki/Shallow_parsing
#https://en.wikipedia.org/wiki/Semantic_parsing
#https://en.wikipedia.org/wiki/Semantic_role_labeling
#from semantic role labelling: "Mary sold the book to John."
#The agent is "Mary," the predicate is "sold" (or rather, "to sell,")
#the theme is "the book," and the recipient is "John." 
#this could be very interesting for identifying agent in political actions.

