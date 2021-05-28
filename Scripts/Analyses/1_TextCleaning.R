#############################################################
# Project:  EU Tweet
# Task:    Clean tweets of EUtweet sample
# Author:   @ChRauh (26.04.2021)
#############################################################


# Packages ####
library(tidyverse) # 1.3.0
library(quanteda) # 2.1.2
library(sophistication) # 0.70 https://github.com/kbenoit/sophistication
library(spacyr) # 1.2.1 - requires Python and Spacy environment installed, see package documentation
library(textcat) # 1.0-7, n-gram based language detection
library(cld2) # 1.2
library(cld3) # 1.4.1 - Google's neural network language detection, very bad on abbreviated text, apparently (LESSON!?)
library(kableExtra) # 1.3.1


# Text cleaning function ####

# This functions cleans the text in preparation for automated text analysis later
# meaning that tweet content is reduced to textual information only 
# with proper punctuation and sentence structure and without encoding issues

# Function expects and returns character vector

cleanTweetTexts <- function(tweettexts = character(0)) {
  
  print("Tweet text cleaning initiated ...\nStarting with irregular whitespace and HTML remainders.")
  
  # In text retweet markers
  # Retweets marked in separate column already
  tweettexts <- tweettexts %>% 
    str_remove("^RT ")
  
  # Irregular whitespace 
  
  # New lines
  # often used instead of punctuation, added here (take care of doubled instances later)
  tweettexts <- tweettexts %>% 
    str_replace_all(fixed("\n"), ". ") %>% 
    str_replace_all(fixed(". . "), ". ")
  
  # Harmonize whitespace usage
  tweettexts <- tweettexts %>% 
    str_replace_all("\\s", " ") %>% # all different types of whitespace to regular whitespace
    str_replace_all("( )+", " ") # Multiple whitespaces to one (should be recursive)
  
  
  # Remove URLs from tweet text 
  # Stored in separate variable already 
  tweettexts <- tweettexts %>% 
    str_remove_all("http.*?( |$)") %>% # Anything from 'http' to either a whitespace or the end of the string
    str_replace_all("( )+", " ") # Multiple whitespaces to one (should be recursive)
  
  # Correct common html encoding issues
  tweettexts <- tweettexts %>% 
    str_replace_all(fixed("&amp;"), " & ") %>% 
    str_replace_all(fixed("&gt;"), " > ") %>% 
    str_replace_all(fixed("&lt;"), " < ") %>% 
    str_replace_all("( )+", " ") #Multiple white spaces (of different type) to one raw whitespace
  
  # Remove Emojis
  print("Removing emojis.")
  # We do not want them in the text, as they disturb language detection, readability indices etc
  # There are provided as Unicode in text, I try to capture the whole range of relevant blocks in the Unicode Standard
  # Classical emoticons 1F300 to 1F64F: https://en.wikipedia.org/wiki/Emoticons_(Unicode_block)
  # Extended pictographs 1F300 to 1F5FF: https://en.wikipedia.org/wiki/Miscellaneous_Symbols_and_Pictographs (nested?)
  # Supplemental pictographs 1F900 to 1F9FF: https://en.wikipedia.org/wiki/Supplemental_Symbols_and_Pictographs
  # Dingbats 2700 to 27BF: https://en.wikipedia.org/wiki/Dingbat#Dingbats_Unicode_block
  # Alphanumeric Supplement 1F100 to 1F1FF: https://en.wikipedia.org/wiki/Enclosed_Alphanumeric_Supplement
  # Miscellaneous_Symbols 2600 to 26FF: https://en.wikipedia.org/wiki/Miscellaneous_Symbols
  # More ???  \u2695️️
  
  # Country flags are a tricky business ... https://en.wikipedia.org/wiki/Regional_indicator_symbol
  # Sequences of unicode characters, EU flag is U+1F1EA (=E) U+1F1FA (=U), e.g.
  
  tweettexts <- tweettexts %>% 
    str_replace_all("[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]", 
                    " ") %>% 
    str_replace_all("( )+", " ")
  
  # Hash tags ####
  print("Cleaning hashtags and mentions.")
  # Stored in seperate variable already
  # But also textual information that should be kept
  # We presume that camel case Hastags are read as seprate words
  # and split accordingly (respecting up to five Words concatened 'words')
  
  tweettexts <- tweettexts %>% 
    str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7 \\8\\9 \\10\\11\\12") %>% # 5 Camel Cased words in Hashtag
    str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7 \\8\\9\\10") %>% # 4 Camel Cased words in Hashtag
    str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7\\8") %>% # 3 Camel Cased words in Hashtag
    str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5\\6") %>% # 2 Camel Cased words in Hashtag
    str_replace_all("#", " ") %>% 
    str_replace_all("( )+", " ")
  
  # @ Mentions
  # Proceed as for Hashtags
  tweettexts <- tweettexts %>% 
    str_replace_all("(@)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7 \\8\\9 \\10\\11\\12") %>% # 5 Camel Cased words in Mention
    str_replace_all("(@)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7 \\8\\9\\10") %>% # 4 Camel Cased words in Mention
    str_replace_all("(@)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7\\8") %>% # 3 Camel Cased words in Mention
    str_replace_all("(@)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5\\6") %>% # 2 Camel Cased words in Mention
    str_replace_all("@", " ") %>% 
    str_replace_all("( )+", " ")
  
  # Final cleaning steps
  print("Final cleaning steps ...")
  
  # The fanciest whitespace you have never seen
  tweettexts <-tweettexts %>% 
    str_remove_all(" ️")# Not even matched by /s
  
  # Reduce multiple whitespaces again (you never know)
  tweettexts <- tweettexts %>% 
    str_replace_all("( )+", " ")
  
  # Offset punctuation
  tweettexts <- tweettexts %>% 
    str_replace_all(fixed(" . "), ". ") %>% 
    str_replace_all(fixed(" . "), ". ") %>% 
    str_replace_all(fixed(". . "), ". ") %>% 
    str_replace_all(fixed(".. "), ". ") %>%
    str_replace_all(fixed(" . "), ". ")
  
  # Dots not followed by whitespace
  tweettexts <- tweettexts %>% 
    str_replace_all("(\\.)([A-Z])", "\\1 \\2")
  
  # Doubled punctuation
  tweettexts <- tweettexts %>% 
    str_replace_all("(\\.|\\?|!)(\\.)", "\\1 ") %>% 
    str_replace_all("(\\.|\\?|!)([ ]{0,2})(\\.)", "\\1 ") %>% 
    str_squish()
  
  # remove whitespace left and right
  tweettexts <- tweettexts %>% 
    str_trim(side = "both")
  
  # Colon at end of string (removed links...)
  tweettexts <- tweettexts  %>% 
    str_trim(side="both") %>% 
    str_remove_all(":$")
  
  # Make sure tweet ends with sentence endmarker (punctuation)
  tweettexts <- tweettexts %>% 
    str_trim(side = "both") %>% 
    paste0(".") %>% 
    str_replace("(\\.|\\?|!)(\\.)", "\\1 ")
  
  # Output
  print("Tweet text cleaning completed!")
  return(tweettexts)
}


# Sandbox illustration of text cleaning

sandbox <- c("I fell asleep hoping to wake up from a bad dream.Europe is full of wonders that no one will bring us back. Preserving with #digitization is important for us &amp; for future generations. Close to the Parisians. With #NotreDame we've lost a piece of our history https://t.co/hQRqMGSsq3 https://t.co/CPLs1DqEcl",
             "\U0001f91d Sharing risk.\n\U0001f30d Maximising impact.\n\nToday we’ve signed 4 new guarantee agreements under the EU External Investment Plan to create more \U0001f4a1 opportunities for people in countries near the EU and in Africa. \n\nRead more ➡️https://t.co/YY3zPWSti4\n\U0001f4c8 #InvestGlobal #EIP https://t.co/HvYWuoVEOC",
             "We call on all countries to bring to justice those responsible for crimes against journalists. #EndImpunity \n\nWe’ll continue to defend freedom of expression and protect those who make use of it to keep us informed. \n\nMy statement with @JosepBorrellF: \nhttps://t.co/8bfPJ5ChsY https://t.co/5MCHSSE8No",
             "Full room \U0001f44f for the workshop on “Advancing the Creation of Regional #Bioeconomy Clusters in #Europe” organised by @BBI2020 &amp; #SCAR #Bioeconomy Group. Great to see the participants commitment in creating a wider and more robust bioeconomy network in the #EU! \U0001f91d https://t.co/mzuX4a03aT",
             "At #SyriaConf2020, the international community pledged a total of €6.9 bn for #Syria &amp; the main countries hosting Syrian refugees for 2020 and beyond\U0001f30d.\n\nOut of this amount, the\U0001f1ea\U0001f1fa#EU pledged 71% (€4.9 bn) with €2.3 bn coming from the\U0001f1ea\U0001f1fa@EU_Commission \n\n➡️https://t.co/cvcAngTUk1 https://t.co/Obgz2rel3g",
             "Happy Birthday! Bonne anniversaire! Alles Gute zum Geburtstag. Wszystkiego najlepszego z okazji urodzin! feliç aniversari! feliz cumpleaños! doğum günün kutlu olsun.",
             "This tweet is clearly mostly english text as it will be often the case. Mais français est possible aussi! \U0001F1EB\U0001F1F7") %>% 
  as.data.frame() %>% 
  rename(text = 1)

sandbox$textclean <- cleanTweetTexts(sandbox$text)

# # Sandbox as initially set up above
# # UNICOde and new lines converted manually (hmpf) to HTML entities to see them in outout
# pre <- c("I fell asleep hoping to wake up from a bad dream.Europe is full of wonders that no one will bring us back. Preserving with #digitization is important for us &amp; for future generations. Close to the Parisians. With #NotreDame we've lost a piece of our history https://t.co/hQRqMGSsq3 https://t.co/CPLs1DqEcl",
#          "&#x0001f91d; Sharing risk.<br>&#x0001f30d; Maximising impact.<br><br>Today we’ve signed 4 new guarantee agreements under the EU External Investment Plan to create more &#x0001f4a1; opportunities for people in countries near the EU and in Africa. <br><br>Read more &#x27a1;https://t.co/YY3zPWSti4<br>&#x01f4c8; #InvestGlobal #EIP https://t.co/HvYWuoVEOC",
#          "We call on all countries to bring to justice those responsible for crimes against journalists. #EndImpunity <br><br>We’ll continue to defend freedom of expression and protect those who make use of it to keep us informed. <br><br>My statement with @JosepBorrellF: <br>https://t.co/8bfPJ5ChsY https://t.co/5MCHSSE8No",
#          "Full room &#x0001f44f; for the workshop on “Advancing the Creation of Regional #Bioeconomy Clusters in #Europe” organised by @BBI2020 &amp; #SCAR #Bioeconomy Group. Great to see the participants commitment in creating a wider and more robust bioeconomy network in the #EU! &#x0001f91d; https://t.co/mzuX4a03aT",
#          "At #SyriaConf2020, the international community pledged a total of €6.9 bn for #Syria &amp; the main countries hosting Syrian refugees for 2020 and beyond&#x1f30d;.<br><br>Out of this amount, the&#x1f1ea;&#x1f1fa;#EU pledged 71% (€4.9 bn) with €2.3 bn coming from the&#x1f1ea;&#x1f1fa;@EU_Commission <br><br>&#x27a7;️https://t.co/cvcAngTUk1 https://t.co/Obgz2rel3g")
# 
# # Comparison table
# comp <- data.frame(pre = pre,
#                    post = sandbox)
# 
# # Export
# comp.out <- comp %>% 
#   kable(col.names = c("Pre-Cleaning", "Post-Cleaning"), escape =F) %>% 
#   kable_styling(bootstrap_options = c("striped", "hover"))
# save_kable(comp.out, "./tables/TextCleaningComparison.html")



# Emoji detection function ####

# Expects text vector of raw tweet texts as input
# If countEmojis == T (default), returns count of detected emojis, 
# if countEmojis == F, returns character vector of all detected emojis
# Emoji RegEx should match cleaning function above

detectTweetEmojis <- function(tweettexts = character(0), countEmojis = T) {
  
  # Emojis are provided as Unicode in text, I try to capture the whole range of relevant blocks in the Unicode Standard
  # Classical emoticons 1F300 to 1F64F: https://en.wikipedia.org/wiki/Emoticons_(Unicode_block)
  # Extended pictographs 1F300 to 1F5FF: https://en.wikipedia.org/wiki/Miscellaneous_Symbols_and_Pictographs (nested?)
  # Supplemental pictographs 1F900 to 1F9FF: https://en.wikipedia.org/wiki/Supplemental_Symbols_and_Pictographs
  # Dingbats 2700 to 27BF: https://en.wikipedia.org/wiki/Dingbat#Dingbats_Unicode_block
  # Alphanumeric Supplement 1F100 to 1F1FF: https://en.wikipedia.org/wiki/Enclosed_Alphanumeric_Supplement
  # Miscellaneous_Symbols 2600 to 26FF: https://en.wikipedia.org/wiki/Miscellaneous_Symbols
  # More ???  \u2695️️
  
  if (countEmojis == T ) {
    emojis <- tweettexts %>% 
      str_count("[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]")
  } else {
    emojis <- tweettexts %>% 
      str_extract_all("[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]")
  }
}

# Sandbox illustration
sandbox$emojicount <- detectTweetEmojis(sandbox$text, countEmojis = T)
sandbox$emoji <- detectTweetEmojis(sandbox$text, countEmojis = F)


  

# Language detection and cleaning function ####

# EU actors likely to tweet in multiple languages (partially also within the same tweet)
# Function should detect language(s) of tweets and keep only english parts for later text analysis in separate variable

# Language detection, especially on short strings, is a challenge, however. Pretesting shows:

# sandbox <- "Happy Birthday! Bonne anniversaire! Alles Gute zum Geburtstag. Wszystkiego najlepszego z okazji urodzin! feliç aniversari! feliz cumpleaños! doğum günün kutlu olsun."
# df <- spacy_tokenize(sandbox, what = "sentence") %>% 
#   data.frame() %>% 
#   rename(sentences = 1)
# df$tc.simple <- textcat(df$sentences)
# df$tc.ecimci <- textcat(df$sentences, p = ECIMCI_profiles) # Larger n-gram sample, no regional dialects, but only 26 languages (see: names(ECIMCI_profiles))
# df$cld2 <- cld2::detect_language(df$sentences) # Google's old model driving Chrome, the best according to different source
# df$cld3 <- cld3::detect_language(df$sentences) # Google's neuronal network model

# Textcat simple has too many misclassifications with irrelvant languages
# Textcat ECMIMCI is muhc better, but limited language choices available
# Googles neuronal network has high error rate 
# CLD2 most robust in quick samples, and also honest if no lang detetcted - go for this now

# In the future, one may think about an ensemble approach

# For now, function expects data frame that contains id and cleaned text (fixed column names for now)
# and returns a data frame indicating languages and 'english only' texts (sentence level)


detectTweetLanguages <- function(tweetcorpus = data.frame(0)) {
  
  # Keep only id and text variables
  tweets <- tweetcorpus %>%  select(id, textclean)
  
  # Detect language of whole tweet
  # Fallback if no language is detected for individual samples
  tweets$tweetlanguage <- cld2::detect_language(tweets$textclean)
  
  # Detect language of individual sentences
  
  tweets$tweetlanguages <- "NA" # Target column to store all detected languages
  tweets$texten <- "NA" # Target column to store only english text
  
  for (i in 1:nrow(tweets)){
    
    # Progress
    print(paste0("Detect languages in tweet ", i, " of ", nrow(tweets)))

    # Sentence tokenizer
    df <- spacy_tokenize(tweets$textclean[i], what = "sentence") %>% 
      data.frame() %>% 
      rename(sentences = 1)
    
    # Language detection
    df$lang <- cld2::detect_language(df$sentences)
    df$lang[is.na(df$lang)] <- tweets$tweetlanguage[i] # Fallback: If language of sentence cannot be detected, use language detected for overall tweet
    
    # Store all detected languages
    langs <- unique(df$lang) %>% 
      as.character() %>% 
      sort() %>% 
      paste(collapse = ", ")
    tweets$tweetlanguages[i] <- langs
    
    # Drop non-english sentences
    df <- df %>% filter(lang == "en") 
    
    # Rebuild and store text
    en.text <- paste(df$sentences, collapse = " ")
    tweets$texten[i] <- en.text
  }
  
  # Clean up
  rm(list = c('en.text','i', 'langs'))

  # Output
  tweets <- tweets %>% select(-textclean) # Drop original texts, still in original corpus
  return(tweets)
  
}


# Sandbox illustration #

sandbox$id <- 1:nrow(sandbox) # Establish unique id
langinfo <- detectTweetLanguages(sandbox) # Extract language info
sandbox <- left_join(sandbox, langinfo, by = "id")



# EU Tweets ####

# Corpus
corpus <- read_rds("./data/corpii/EU_corpus.RDS")

# Sample for testing purposes
# corpus <- corpus %>% sample_n(3000)

# Clean texts
corpus$textclean <- cleanTweetTexts(corpus$text)

# Emojis
corpus$emojicount <- detectTweetEmojis(corpus$text, countEmojis = T)
corpus$emojis <- detectTweetEmojis(corpus$text, countEmojis = F)

# Language detection and cleaning
corpus$id <- 1:nrow(corpus) # Establish unique id
langinfo <- detectTweetLanguages(corpus) # Extract language info
corpus <- left_join(corpus, langinfo, by = "id")

# Export 
write_rds(corpus, "./data/corpii/EU_corpus_cleaned.RDS")



# IO Tweets ####

# Corpus
corpus <- read_rds("./data/corpii/IO_corpus.RDS")

# Sample for testing purposes
# corpus <- corpus %>% sample_n(3000)

# Clean texts
corpus$textclean <- cleanTweetTexts(corpus$text)

# Emojis
corpus$emojicount <- detectTweetEmojis(corpus$text, countEmojis = T)
corpus$emojis <- detectTweetEmojis(corpus$text, countEmojis = F)

# Language detection and cleaning
corpus$id <- 1:nrow(corpus) # Establish unique id
langinfo <- detectTweetLanguages(corpus) # Extract language info
corpus <- left_join(corpus, langinfo, by = "id")

# Export 
write_rds(corpus, "./data/corpii/IO_corpus_cleaned.RDS")



# # UK Tweets ####
# 
# # Corpus
# corpus <- read_rds("./data/corpii/UK_corpus.RDS")
# 
# # Sample for testing purposes
# # corpus <- corpus %>% sample_n(3000)
# 
# # Clean texts
# corpus$textclean <- cleanTweetTexts(corpus$text)
# 
# # Emojis
# corpus$emojicount <- detectTweetEmojis(corpus$text, countEmojis = T)
# corpus$emojis <- detectTweetEmojis(corpus$text, countEmojis = F)
# 
# # Language detection and cleaning
# corpus$id <- 1:nrow(corpus) # Establish unique id
# langinfo <- detectTweetLanguages(corpus) # Extract language info
# corpus <- left_join(corpus, langinfo, by = "id")
# 
# # Export 
# write_rds(corpus, "./data/corpii/UK_corpus_cleaned.RDS")
# 
# 
# 
# # TWT Tweets ####
# 
# # Corpus
# corpus <- read_rds("./data/corpii/TWT_corpus.RDS")
# 
# # Sample for testing purposes
# # corpus <- corpus %>% sample_n(3000)
# 
# # Clean texts
# corpus$textclean <- cleanTweetTexts(corpus$text)
# 
# # Emojis
# corpus$emojicount <- detectTweetEmojis(corpus$text, countEmojis = T)
# corpus$emojis <- detectTweetEmojis(corpus$text, countEmojis = F)
# 
# # Language detection and cleaning
# corpus$id <- 1:nrow(corpus) # Establish unique id
# langinfo <- detectTweetLanguages(corpus) # Extract language info
# corpus <- left_join(corpus, langinfo, by = "id")
# 
# # Export 
# write_rds(corpus, "./data/corpii/TWT_corpus_cleaned.RDS")