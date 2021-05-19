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

# sandbox experiment ------------------------------------------------------


#spacy is extremely sensitive to typos and mistakes
sandbox<- c(tweet1 = "The EU commission has concluded an investigation against Poland due to recent events and found serious breaches of rule-of-law. Poland will be kicked out of the union",
            tweet2 ="Amazing organization. I attended the first TTNET high-level dialog conference",
            tweet3 ="ECB announced new measures against inflation",
            tweet4 ="After consultation with relevant stakeholders, we decided to stop vaccination purchases",
            tweet5 ="This day coudln't get any beteter!I received my missoin letter to ensure our European way of life #EU#OurWay!! Great honor to be part of vdL")
#hunspell spell checking because spacy is extremely sensitive to typos####
install.packages("hunspell")
library(hunspell)
#is it vectorized?
spell_check_bad<-hunspell(text = sandbox) 
#returns a list for each document as an element
#elements contain only the misspelled words
#however it also recognizes abbreviations and hashtags as misspelling
spell_check_bad[5]
#I can exclude hashtags but abbreviations will be a little bit of a problem
spelling_suggestions<- hunspell_suggest(spell_check_bad[[5]])
a<-spelling_suggestions[[1]]
print(a)
#there seems to be some sort of probability calculation going on under the hood
#both in this example and in the examples online, first suggestions seem to be the most accurate ones

#also differing lengths of lists make it hard to vectorize the operation
#testing a for loop approach
to_ignore<- c("TTNET","ECB","OurWay","vdL")
corrected_tweets<-list()
#there is gotta be a better way...
for(i in length(sandbox)){
  misspelling<- hunspell(text =sandbox[i],format = "text", ignore = to_ignore)
  
  if(length(misspelling)>=1){
  suggestions<- hunspell_suggest(misspelling) %>% unlist()
  corrected_text<- str_replace(string = sandbox[i],pattern = misspelling,replacement = suggestions[1])
  corrected_tweets[i]<-corrected_text
  names(corrected_tweets)[i]<-names(sandbox[i])
  }else{
    corrected_tweets[i]<-sandbox[i]
    names(corrected_tweets)[i]<-names(sandbox[i])
  }
}
#hunspell_suggest

a<-hunspell_find(text = sandbox[5]) %>% unlist()
b<-hunspell_suggest(words = a)
b[[1]][1]

######
hashtags_removed_sandbox<- str_replace_all(string = sandbox,pattern = "#",replacement = " ")
sandbox_parse<- spacy_tokenize(hashtags_removed_sandbox,"word",
                               remove_punct = T,
                               remove_url = T,
                               remove_numbers = T,
                               remove_separators = T,
                               remove_symbols = T,
                               output = "data.frame")
#so here sentences become text if I tokenize tweets into sentences firs
resp_pos_tag<- spacy_parse(x = hashtags_removed_sandbox,
                      lemma = F,
                      pos = T,
                      tag = T,
                      entity = T,
                      dependency = F,
                      nounphrase = T)
#yayks spacy thinks emojis are nouns... better use twitter tagger.
tweet_grammar <- resp_pos_tag %>%
  group_by(doc_id,sentence_id) %>%
  summarise(grammar_tag = paste(tag, collapse = "+"),
            grammar_pos = paste(pos, collapse = "+"),
            sentence = paste(token, collapse = " "))
#ok so if text is named list, names of the list items are transferred as doc_id
#this turns the data one sentence per line, so its fine if I save the data as a seperate dataset


# test with real data -----------------------------------------------------


#test with the data:

#reading the sample data 
data<- readRDS(paste0(data.path,"EUcorpus_cleaned_sample.RDS"))

tweets<- data %>% filter(langlang == "en") %>% pull(tweet_text)
#there are some problems in the reprocessed tweets. Obs 960 still has emojis for some reason. there is also a character with unicode \ufe0f
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


# Notes -------------------------------------------------------------------


#Naive pos is not enough to recognize responsibility grammar where it identifies agent in an action
#there are some alternative things I can try:
#https://en.wikipedia.org/wiki/Shallow_parsing
#https://en.wikipedia.org/wiki/Semantic_parsing
#https://en.wikipedia.org/wiki/Semantic_role_labeling
#from semantic role labelling: "Mary sold the book to John."
#The agent is "Mary," the predicate is "sold" (or rather, "to sell,")
#the theme is "the book," and the recipient is "John." 
#this could be very interesting for identifying agent in political actions.
# WOAH this escalated quick:
#[22:14, 17/05/2021] +1 (757) 232-4858: Things could be improved by using named entity recognition instead of just pos tagging (getting proper noun vs just noun). Spacy has pretrained NER in it. As does nltk. There are some Bert based models that do much better at ner overall, but I imagine proper noun recognition is a bit easier than some tagging.
#[22:20, 17/05/2021] +1 (757) 232-4858: If extracting the proper name from the tweets isn't sufficient for what you need, you could turn it into a question answer problem and throw BERT at it. You could extract the proper names from each tweet using spacy's NER, then use sbert (sbert.net) to embed the sentence "who is the agent of the following passage: <tweet>", embed all the proper names you extracted as the sbert embeddings of "The agent is: <name/noun that appears in tweet>", and pick the one with the highest cosine similarity.  It might give false positives, but there might be some similarity threshold you could filter out false positives with


# function ----------------------------------------------------------------


grammar_extractor<- function(data,text_field,text_id,spell_check = F, emoji = F,emoji_dictionary = NULL){
  
  #make the text a named list####
  if(is.character(text_field) && is.character(text_id)){
  text<- as.vector(data[,text_field],mode = "character")
  text_id<- as.vector(data[,text_id],mode = "character")
  names(text)<-text_id
  }else{
    stop("Please provide text and id fields as strings")
  }

#replace the emojis with their linguistic equivalents####
  if (isTRUE(emoji)){
    if(is.null(emoji_dictionary)){
    
    library(emo)
    
    emoji_decoder <- data.frame(emoji_text = names(ji_name),
                                  emoji_code = as.character(ji_name))
    
    emoji_decoder$emoji_code<-as.character(emoji_decoder$emoji_code)
    
    emoji_decoder_a<-grep(pattern = "*asteri|keycap_star",x = emoji_decoder$emoji_text,ignore.case = T,value = T)
    
    emoji_decoder<- emoji_decoder_2[-which(emoji_decoder$emoji_text%in%emoji_decoder_a),]
    
    for (i in 1:nrow(emoji_decoder)) {
      
      print(paste0("replacing ",emoji_decoder$emoji_text[i],"regex is ", emoji_decoder$emoji_code[i]))
      
      text<- str_replace_all(string = text,
                                       pattern = emoji_decoder$emoji_code[i],
                                       replacement = emoji_decoder$emoji_text[i])
    }
    
    }else{
      
      emoji_decoder<- emoji_dictionary
      
      for (i in 1:nrow(emoji_decoder)) {
        
        print(paste0("replacing ",emoji_decoder$emoji_text[i],"regex is ", emoji_decoder$emoji_code[i]))
        
        text<- str_replace_all(string = text,
                                         pattern = emoji_decoder$emoji_code[i],
                                         replacement = emoji_decoder$emoji_text[i])
      }
      
      
    }
  }
#replace hashtag with white space (only the hashtag leave the rest in)####
  text<- str_replace_all(string = text, pattern = "#", replacement = " ")
# Need to integrate a spell check here
#tagging#####
  
  text_tagged<- spacy_parse(x = text,
                            lemma = F,
                            pos = T,
                            tag = T,
                            entity = T,
                            dependency = F,
                            nounphrase = T,
                            multithread = T)
#Need to shuffle the data around a bit to create a decent grammar string
#to identify responsibility attribution
}
