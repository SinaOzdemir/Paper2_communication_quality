#Political responsibility indicators messing around

packs<- c("tidyverse","spacyr","hunspell","quanteda")
pacman::p_load(char = packs)
data.path<- paste0(getwd(),"/analysis_data/")


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
            tweet5 ="This day couldn't get any better!I received my mission letter to ensure our European way of life #EU#OurWay!! Great honor to be part of vdL")
test_a<- str_remove_all(string = sandbox,pattern = "[#][\\w_-]+")
#gsub doesn't remove the name attribute and does the job if perl = T
test_b<- gsub(pattern = "[#][\\w_-]+",replacement = "",x = sandbox,perl = T)
names(test_a)
names(test_b)
#get a feeling of the data structure after quanteda preprocessing:####
library(quanteda)

sandbox_corpus<- quanteda::corpus(sandbox)
#might as well do some light cleaning here
#still need to remove hashtags
sandbox_dfm<- sandbox %>%
  str_remove_all("[#][\\w_-]+") %>%
  corpus(.,docnames = names(sandbox)) %>%
  tokens(x = .,remove_punct = T,
         remove_symbols = T,
         remove_numbers = T,
         remove_url = T,
         remove_separators = T,) %>%
  tokens_compound(.,
                  phrase(c("European Union","EU commission")),
                  case_insensitive = T) %>%tokens_tolower(.) %>% 
  tokens_remove(.,stopwords("en")) %>%
  tokens_wordstem(.,language = "en") %>% 
  dfm()


utils::download.file(url = "http://www.snsoroka.com/wp-content/uploads/2020/08/LTDjun2013.zip",destfile = "./dictionaries/LTDjun2013.zip")
utils::unzip(zipfile = "./dictionaries/LTDjun2013.zip",exdir = "./dictionaries")
file.remove("./dictionaries/LTDjun2013.zip")

dict<- dictionary(file = "./dictionaries/LTDjun2013/policy_agendas_english.lcd")
dict[["macroeconomics"]]<- c(dict[["macroeconomics"]],"inflat")

sandbox_policy <- sandbox_dfm %>%
  dfm_lookup(x = .,dictionary = dict,case_insensitive = T,capkeys = T,verbose = T) %>% 
  convert(.,to = "data.frame",docid_field = "tweet_id")

# I should probably do some compounding here somewhere like EU commission etc


#hunspell spell checking because spacy is extremely sensitive to typos####
install.packages("hunspell")
library(hunspell)
library(textcat) # 1.0-7, n-gram based language detection
library(cld2) # 1.2
library(cld3) # 1.
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

sandbox_misspell<- c(tweet1 = "The EU commission has concluded an investigation against Poland due to recent events and found serious breaches of rule-of-law. Poland will be kicked out of the union",
                     tweet2 ="Amazing organization. I attended the first TTNET high-seviye dialog conference",
                     tweet3 ="ECB announced newmeasures against inflation",
                     tweet4 ="After consultasion with relevant stakeholders, we decided to stop vaccination purchases",
                     tweet5 ="This day coulnd't get any better!I received my mission letter to ensure our European way of life #EU#OurWay!! Great honor to be part of vdL")

to_ignore<- c("TTNET","ECB","OurWay","vdL")

sandbox_spellcheck<- hunspell(text = sandbox_misspell,format = "text",ignore = to_ignore) %>% unlist()

lang_det_test_textcat<- textcat::textcat(x = sandbox_spellcheck)#this is pretty bullshit
lang_det_test_textcat<- textcat::textcat(x = sandbox_spellcheck,p = ECIMCI_profiles)#slightly better but not a whole lot, at least it recognizes english ones correctly
lang_det_test_cld2<- cld2::detect_language_mixed(text = sandbox_spellcheck[3],plain_text = T)#doesn't work for single word
lang_det_test_cld3<- cld3::detect_language_multi(text = sandbox_spellcheck[3])#also doesn't work on single words
#fastText based on the blog post: https://www.r-bloggers.com/2021/05/language-identification-using-the-fasttext-package-a-benchmark/
install.packages("fastText")
library(fastText)
small_model<-("./dictionaries/lid.176.ftz")
#works extremely well if the pelling is correct, provides lower probability if spelling is incorrect
lang_det_test_ft <- fastText::language_identification(input_obj = sandbox_spellcheck,
                                                      pre_trained_language_model_path = small_model,
                                                      k = 3,
                                                      th = 0.0,
                                                      threads = 1,
                                                      verbose = TRUE) %>% as_tibble(rownames = "word_id")
large_model<- ("./dictionaries/lid.176.bin")
lang_det_test_ft_large<- fastText::language_identification(input_obj = sandbox_spellcheck,
                                                           pre_trained_language_model_path = large_model,
                                                           k = 3,
                                                           th = 0,
                                                           threads = 1,
                                                           verbose = T) %>% as_tibble(rownames = "word_id")

#quickly replicate the language detection on sample data to see which on performs better

tweets<-gsub("http.*?( |$)","",tweets,perl = T)

hashtags<- str_extract_all(string = tweets,pattern = "[#][\\w_-]+|[@][\\w_-]+") %>% unlist() %>% unique()
mentions<- str_extract_all(string = tweets,pattern = "[@][\\w_-]+") %>% unlist() %>% unique()
to_ignore<- c(hashtags,mentions)
#interesting, some mentions are not ignored
tweets_spellcheck <- hunspell(text = tweets, format = "text",ignore = to_ignore) %>% unlist()

#absolutely useless
rl_text_cat<- data.frame(word = tweets_spellcheck,
                         language = textcat::textcat(x = tweets_spellcheck))
#absolutely useless too
rl_text_cat_ext<- data.frame(word = tweets_spellcheck,
                             language = textcat::textcat(x = tweets_spellcheck,p = ECIMCI_profiles))

#works a little better if I can get hunspell to ignore hashtags and mentions...
rl_ft_small<- fastText::language_identification(input_obj = tweets_spellcheck,
                                                pre_trained_language_model_path = small_model) %>% cbind(.,tweets_spellcheck)
#better than the large model but the core problem is still there... hunspell doesn't ignore hashtags and mentions...
rl_ft_large<- fastText::language_identification(input_obj = tweets_spellcheck,
                                                pre_trained_language_model_path = large_model) %>% cbind(.,tweets_spellcheck)

#language detection on word level
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

# try a dictionary approach.

## Replace hashtags with white space so the hunspell is not confused

sandbox_cleaned<- str_replace_all(string = sandbox,pattern = "#",replacement = " ")

#identify misspellings
#twitter data have the mentions and hashtags in seperate columns,
#I can use it create an ignore list before I check for misspellings
#I still have to deal with abbreviations tho.
sb_misspelled_words<- hunspell(sandbox_cleaned,"text") %>% unlist

sb_suggestions<- hunspell_suggest(sb_misspelled_words)

names(sb_suggestions)<-sb_misspelled_words
#so now I need to the suggestions to one word per misspelling, prioritizing
#only the first element

#purr suggestion:
#I love it when elegant solutions work at the first try...
spelling_dictionary<- purrr::map(sb_suggestions,1)

#grammar correction

for (i in 1:length(spelling_dictionary)) {
  print(paste0("looking up ",names(spelling_dictionary)[i]," to replace with ",spelling_dictionary[[i]]))
  sandbox<- str_replace_all(string = sandbox,
                            pattern = names(spelling_dictionary)[i],
                            replacement = spelling_dictionary[[i]])
}


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
data_id<- readRDS(paste0(data.path,"EU_corpus_sample.RDS")) %>% pull(tweet_id)
data<- readRDS(file.choose()) %>% filter(id %in% data_id)

tweets<- data %>% filter(lang == "en") %>% pull(text)
tweet_ids <- data %>% filter(lang == "en") %>% pull(id)

political_respons_example<-data %>% filter(id == "1001044775120916480") %>% pull(text)

#there are some problems in the reprocessed tweets. Obs 960 still has emojis for some reason. there is also a character with unicode \ufe0f
# lets ignore this for now

#replace the emojis with verbal equivalents:-----
emoji_dictionar<- readRDS(file = file.choose())
emoji_dictionar$emoji_text<-paste0(" ",emoji_dictionar$emoji_text," ")
no_emoji_tweets<-tweets

for (i in 1:nrow(emoji_dictionar)) {
  no_emoji_tweets<-str_replace_all(string = no_emoji_tweets,
                                   pattern = emoji_dictionar$emoji_code[i],
                                   replacement = emoji_dictionar$emoji_text[i])
}
###################################################.
#also this process removes names attribute:
#Christian's cleaning did something to encoding
#this works if I use it on raw text data
# also need to add padding around the emoji names otherwise it looks weird in the 
#text eg: "greecehandshakeeu"
#Need to remove links, hashtags and @s before spell checking
###################################################.

#spellcheck-----

#to ignore list needs a rework. Instead of ignoring things in the hashtag and mention columns,
#its better to create a hastag and mention list from the raw text and push it to ignore
hashtags<- str_extract_all(string = no_emoji_tweets,pattern = "[#][\\w_-]+") %>% unlist() %>% unique()
mentions<- str_extract_all(string = no_emoji_tweets,pattern = "[@][\\w_-]+") %>% unlist() %>% unique()
to_ignore<- c(hashtags,mentions)

for_spell_checking_tweets<-no_emoji_tweets %>% 
  str_remove_all("[#][\\w_-]+") %>%
  str_remove_all("[@][\\w_-]+") %>% 
  str_remove_all("http.*?( |$)") %>% 
  str_remove_all("\n") %>%
  str_replace_all(pattern = "&amp;",replacement = "&") %>% #replaced with white space because of consecutive hashtags
  str_replace_all("( )+", " ")# I might as well do all the preprocessing here actually...


spelling_mistakes<- hunspell(text = for_spell_checking_tweets,format = "text",ignore = to_ignore,dict = dictionary("en_GB")) %>% unlist() %>% unique()

spelling_suggestions<- hunspell_suggest(spelling_mistakes) %>% map(.,1) %>% set_names(nm = spelling_mistakes)


not_ignored<-names(spelling_suggestions)[which(names(spelling_suggestions)%in%to_ignore)]

no_emoji_spell_checked_tweets<- no_emoji_tweets
for (i in 1:length(spelling_suggestions)) {
  no_emoji_spell_checked_tweets<-str_replace_all(no_emoji_spell_checked_tweets,
                                                 pattern = names(spelling_suggestions)[i],
                                                 replacement = spelling_suggestions[[i]])
  
}

###########################.
#Spell checking mostly picks up on 
#special names and non-english names
#I think spell checking would introduce more noise at this point
#also regular expression doesn't remove all links.
###########################.


#parsing:-----

##Notes##
#1) spell checking with hunspell didn't work. It mostly recognizes special names thus introduces more noise
#2) Twitter specific communication features (#&@) must be accommodated before POS tagging with some preprocessing. I am not sure how to go about it yet
#3) POS tagging should include dependency true relational grammar parsing.


spacy_initialize()
#need to do a tiny bit of cleaning before parsing to accommodate 
#twitter communication

no_emoji_spell_checked_tweets<- no_emoji_spell_checked_tweets %>% 
  str_remove_all("@") %>% 
  qdapRegex::rm_twitter_url(text.var = .,trim = T,clean = T,pattern = "@rm_twitter_url") %>% 
  str_replace_all(pattern = "#",
                  replacement = " ") %>% #replaced with white space because of consecutive hashtags
  str_replace_all("( )+", " ")

#actually a proper preprocessing where stop words,numbers, etc. removed
#is necessary. The question is how much would it effect the grammar
#extraction? I am still looking forun Proper_nouns+verb as in the example.
names(no_emoji_spell_checked_tweets)<-data %>% filter(tweet_lang == "en") %>% pull(tweet_id)

#more preprocessing:
#tweet ids and order is f*ed up in preprocessing...
tweets_preprocessed<- spacy_tokenize(x = no_emoji_spell_checked_tweets,
                                     what = "word",
                                     remove_punct = T,
                                     remove_url = T,
                                     remove_numbers = T,
                                     remove_separators = T,
                                     remove_symbols = T,
                                     padding = T,
                                     output = "data.frame") %>%
  group_by(doc_id) %>%
  summarise(tweet_text = paste(token,collapse = " ")) %>%
  pull(tweet_text) %>%
  purr::set_names(.,nm = tweet_ids)


parsed_tweets<- spacy_parse(x = tweets_preprocessed,
                            pos = T,
                            tag = T,
                            lemma = F,
                            entity = T,
                            nounphrase = T,
                            multithread = T)

entity_parsed_tweets<- parsed_tweets %>% entity_consolidate()

noun_phrase_tweets<- parsed_tweets %>% nounphrase_consolidate()

#
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


a

