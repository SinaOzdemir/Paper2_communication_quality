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

#sandbox

sandbox<- c(tweet1 = "The EU commission has concluded an investigation agains Poland due to recent events and found serious breaches agains rule-of-law. Poland will be kicked out of the union",
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
resp_pos_tag<- spacy_parse(x = sandbox_parse$token,
                      lemma = F,
                      pos = T,
                      tag = T,
                      entity = T,
                      dependency = T,
                      nounphrase = T)
#yayks spacy thinks emojis are nouns... better use twitter tagger.
tweet_grammar <- resp_pos_tag %>%
  group_by(doc_id) %>%
  summarise(grammar = paste(tag, collapse = "+")) %>%
  select(-doc_id) %>%  bind_cols(.,sandbox_parse) %>%
  group_by(doc_id) %>%
  summarise(tweet_text = paste(token, collapse = " "),
            tweet_grammar = paste(grammar,collapse = "\n"))#something to indicate end of a sentence here

grammar<- str_split(tweet_grammar$tweet_grammar,pattern = "\n",simplify = T)#ok linebreak works for sentence division.

#if I do one tweet at a time it shouldn't be a problem to get grammar of a tweet per line but doc id system of spacy does not allow 
# merge by id

#function: this is going to be simplified, I overcomplicated things....

grammar_extractor<- function(string,tweet_id,emoji = F,what = c("tag","pos")){
  
  if(isTRUE(emoji)){
    library(emo)
    emoji_decoder <- data.frame(emoji_text = names(ji_name),
                                  emoji_code = as.character(ji_name))
    
    emoji_decoder$emoji_code<-as.character(emoji_decoder_2$emoji_code)
    
    emoji_decoder$emoji_text <- paste0(" [",emoji_decoder_2$emoji_text,"] ")
    
    emoji_decoder_a<-grep(pattern = "*asteri|keycap_star",x = emoji_decoder$emoji_text,ignore.case = T,value = T)
    
    emoji_decoder <- emoji_decoder[-which(emoji_decoder$emoji_text%in%emoji_decoder_a),]
    
    #there must be a better way of doing this...
    for (i in 1:nrow(emoji_decoder)) {
      
      print(paste0("replacing ",emoji_decoder$emoji_text[i],"regex is ", emoji_decoder$emoji_code[i]))
      
      string<- str_replace_all(string = string,
                                       pattern = emoji_decoder$emoji_code[i],
                                       replacement = emoji_decoder$emoji_text[i])
    }
  }

  to_parse<- spacy_tokenize(string, what = "sentence")
  
  if(what == "tag"){
    string_parsed<- spacy_parse(x = to_parse,
                                pos = F,
                                tag = T,
                                lemma = F,
                                entity = F)
    
    string_grammar<- string_parsed %>% 
      group_by(doc_id) %>%
      summarise(grammar = paste(tag, collapes = "+")) %>% 
      select(-doc_id) %>%
      bind_cols(.,string_parsed) %>%
      group_by(doc_id) %>% summarise(tweet_text = paste(token, collapse = "\n"),
                                     tweet_grammar = paste(grammar,collapse = "\n")) %>% 
      mutate(tweet_id = tweet_id)
    
    return(string_grammar)
  }
  
  if(what == "pos"){
    
    string_parsed<- spacy_parse(x = to_parse,
                                pos = T,
                                tag = F,
                                lemma = F,
                                entity = F)
    
    string_grammar<- string_parsed %>% 
      group_by(doc_id) %>%
      summarise(grammar = paste(pos, collapes = "+")) %>% 
      select(-doc_id) %>%
      bind_cols(.,string_parsed) %>%
      group_by(doc_id) %>% summarise(tweet_text = paste(token, collapse = "\n"),
                                     tweet_grammar_pos = paste(grammar,collapse = "\n")) %>% 
      mutate(tweet_id = tweet_id)
  }

  
}
