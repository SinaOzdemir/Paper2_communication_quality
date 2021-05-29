#Political responsibiltiy indicators



# setup -------------------------------------------------------------------

packs<- c("tidyverse","spacyr","hunspell","quanteda")
pacman::p_load(char = packs)
data.path<- paste0(getwd(),"/analysis_data/")
##################################
#Found the cap dictionary!
##################################

# function ----------------------------------------------------------------

policy_respons_extractor<- function(data,text_field,
                             text_id,
                             emoji = F,
                             emoji_dictionary = NULL,
                             spell_check = F,
                             preprocess = F,
                             policy = F,
                             grammar = F){
  
  #make the text a named list####
  if(is.character(text_field) && is.character(text_id)){
  cat("converting data to named list object")
  text<- as.vector(data[,text_field],mode = "character")
  text_id<- as.vector(data[,text_id],mode = "character")
  names(text)<-text_id
  #links are never useful anyway
  text<- gsub("http.*?( |$)","",text,perl = T)
  }else{
    stop("Please provide text and id fields as strings")
  }

# spellcheck------
  if(spell_check = T){
    stop("spell checking is not implemented, please choose spellcheck = F")
    cat("spellcheking the tweets")
    
    to_ignore<- str_extract_all(string = tweets,pattern = "[#][\\w_-]+|[@][\\w_-]+") %>% unlist() %>% unique()
    
    text_spell<- hunspell(text = text,format = "text",ignore = to_ignore) %>% unlist()
    
    if(!file.exists("./dictionaries/lid.176.bin")){
      cat("language recognition dictionary is missing, downloading it now")
      cat("this may take a while...")
      utils::download.file(url = "https://dl.fbaipublicfiles.com/fasttext/supervised-models/lid.176.bin",
                           destfile = "./dictionaries/lid.176.bin")
    }
    
    to_correct<- fastText::language_identification(input_obj = text_spell,
                                                        pre_trained_language_model_path = "./dictionaries/lid.176.bin") %>%
      cbind(.,text_spell) %>%
      filter(iso_lang_1 == "en") %>% 
      pull(tweets_spellcheck)
    
    suggestions<- hunspell_suggest(to_correct) %>%
      purrr::set_names(nm = to_correct) %>% 
      purrr::map(.,1)
    
    for (i in 1:length(suggestions)) {
      print(paste0("looking up ",names(suggestions)[i]," to replace with ",suggestions[[i]]))
      text<- str_replace_all(string = text,
                                pattern = names(suggestions)[i],
                                replacement = suggestions[[i]])
    }
    
    
    
  }
  
  
  
  
  
  
#light preprocessing:------
  #remove hashtags and @ so they don't confuse the spacy later
text <- gsub("#|@","",text,perl = T)  
  

#replace the emojis with their linguistic equivalents####
#str_remove removes the names as well. need to do this with gsub or with a less intrusive function
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
  

#extract grammar----

cat("extracting grammar structure of sentences")
spacy_initialize()

text_sentence<- spacy_parse(x = text,pos=T,tag=T,dependency = T)
text_grammar<- text_sentence %>%
  group_by(doc_id,sentence_id) %>%
  summarise(text = paste0(token,collapse = " "),
            grammar_pos = paste0(pos, collapse = "+"),
            grammar_tag = paste0(tag, collapse = "+"),
            dependency = paste0(collapse = "+")) %>% 
  mutate(text_id = paste(doc_id,sentence_id,sep = "_"))


#dictionary lookup----
cat("implementing policy dictionary")
if(!dir.exists("./dictionaries/LTDjun2013")){
  
  utils::download.file(url = "http://www.snsoroka.com/wp-content/uploads/2020/08/LTDjun2013.zip",destfile = "./dictionaries/LTDjun2013.zip")
  utils::unzip(zipfile = "./dictionaries/LTDjun2013.zip",exdir = "./dictionaries")
  file.remove("./dictionaries/LTDjun2013.zip")
  dict<- quanteda::dictionary(file = "./dictionaries/LTDjun2013/policy_agendas_english.lcd")
  
}else{
  dict<- quanteda::dictionary(file = "./dictionaries/LTDjun2013/policy_agendas_english.lcd")
}

dictionary_text_id<- text_grammar %>% pull(text_id)
dictionary_text <- text_grammar %>% pull(text) %>% set_names(nm = dictionary_text_id)



policy_dfm<- dictionary_text %>%
  corpus(.,docnames = names(dictionary_text)) %>%
  tokens(x = .,remove_punct = T,
         remove_symbols = T,
         remove_numbers = T,
         remove_url = T,
         remove_separators = T) %>%
  tokens_compound(.,
                  phrase(c("European Union","EU commission")),
                  case_insensitive = T) %>% 
  tokens_tolower(.) %>% 
  tokens_remove(.,stopwords("en")) %>%
  tokens_wordstem(.,language = "en") %>% 
  dfm() %>%
  dfm_lookup(x = .,
             dictionary = dict,
             case_insensitive = T,
             capkeys = T,
             verbose = T) %>% 
  convert(.,to = "data.frame",docid_field = "text_id")

raw_results<- left_join(policy_dfm,text_grammar, by = "text_id")

return(raw_results)

}
