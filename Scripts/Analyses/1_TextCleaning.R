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


# EUtweet corpus ####
corpus <- read_rds("./tweetcorpora/EUtweets.RDS")

# Sample for testing purposes
# corpus <- corpus %>% sample_n(3000)

# Keep copy of raw tweet text
corpus <- corpus %>% mutate(text_raw = text)

# Sandbox of real tweets
# Random, from corpus$text[sample(1:3000, 1)]
sandbox <- c("I fell asleep hoping to wake up from a bad dream.Europe is full of wonders that no one will bring us back. Preserving with #digitization is important for us &amp; for future generations. Close to the Parisians. With #NotreDame we've lost a piece of our history https://t.co/hQRqMGSsq3 https://t.co/CPLs1DqEcl",
             "\U0001f91d Sharing risk.\n\U0001f30d Maximising impact.\n\nToday we’ve signed 4 new guarantee agreements under the EU External Investment Plan to create more \U0001f4a1 opportunities for people in countries near the EU and in Africa. \n\nRead more ➡️https://t.co/YY3zPWSti4\n\U0001f4c8 #InvestGlobal #EIP https://t.co/HvYWuoVEOC",
             "We call on all countries to bring to justice those responsible for crimes against journalists. #EndImpunity \n\nWe’ll continue to defend freedom of expression and protect those who make use of it to keep us informed. \n\nMy statement with @JosepBorrellF: \nhttps://t.co/8bfPJ5ChsY https://t.co/5MCHSSE8No",
             "Full room \U0001f44f for the workshop on “Advancing the Creation of Regional #Bioeconomy Clusters in #Europe” organised by @BBI2020 &amp; #SCAR #Bioeconomy Group. Great to see the participants commitment in creating a wider and more robust bioeconomy network in the #EU! \U0001f91d https://t.co/mzuX4a03aT",
             "At #SyriaConf2020, the international community pledged a total of €6.9 bn for #Syria &amp; the main countries hosting Syrian refugees for 2020 and beyond\U0001f30d.\n\nOut of this amount, the\U0001f1ea\U0001f1fa#EU pledged 71% (€4.9 bn) with €2.3 bn coming from the\U0001f1ea\U0001f1fa@EU_Commission \n\n➡️https://t.co/cvcAngTUk1 https://t.co/Obgz2rel3g")


# Irregular whitespace ####

# New lines
# often used instead of punctuation, added here (take care of doubled instances later)
sandbox <- sandbox %>% 
  str_replace_all(fixed("\n"), ". ") %>% 
  str_replace_all(fixed(". . "), ". ")

corpus$text <- corpus$text %>% 
  str_replace_all(fixed("\n"), ". ") %>% 
  str_replace_all(fixed(". . "), ". ")


# Harmonize whitespace usage
sandbox <- sandbox %>% 
  str_replace_all("\\s", " ") %>% # all different types of whitespace to regular whitespace
  str_replace_all("( )+", " ") # Multiple whitespaces to one (should be recursive)

corpus$text <- corpus$text %>% 
  str_replace_all("\\s", " ") %>% # all different types of whitespace to regular whitespace
  str_replace_all("( )+", " ") # Multiple whitespaces to one (should be recursive)


# Remove URLs from tweet text ####
# They are separately stored in API output 
# and disturbed language detection, text complexity measures, etc. ...

sandbox <- sandbox %>% 
  str_remove_all("http.*?( |$)") %>% # Anything from 'http' to either a whitespace or the end of the string
  str_replace_all("( )+", " ") # Multiple whitespaces to one (should be recursive)

corpus$text <- corpus$text %>% 
  str_remove_all("http.*?( |$)") %>% # Anything from 'http' to either a whitespace or the end of the string
  str_replace_all("( )+", " ") # Multiple whitespaces to one (should be recursive)



# HTML encoding issues ####

# Manual extraction of (possible) HTML character references
# one.doc <- paste(corpus$text, collapse = " ")
# faulty <- str_extract_all(one.doc, "&[a-z0-9].*? ") %>% 
#   unlist() %>% 
#   str_trim() %>% 
#   as.data.frame() %>% 
#   rename(pattern = 1) %>% 
#   group_by(pattern) %>% 
#   summarise(count = n()) %>% 
#   arrange(desc(count))

# Correct HTML encoding issues
sandbox <- sandbox %>% str_replace_all(fixed("&amp;"), " & ") %>% 
  str_replace_all(fixed("&gt;"), " > ") %>% 
  str_replace_all(fixed("&lt;"), " < ") %>% 
  str_replace_all("( )+", " ") 
  
corpus$text <- corpus$text %>% 
  str_replace_all(fixed("&amp;"), " & ") %>% 
  str_replace_all(fixed("&gt;"), " > ") %>% 
  str_replace_all(fixed("&lt;"), " < ") %>% 
  str_replace_all("( )+", " ") #Multiple white spaces (of different type) to one raw whitespace



# Emoticons ####

# We do not want them in the text, as they disturb language detection, readability indices etc
# But we want to preserve the info to possibly control for their effect on engagement
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

sandbox %>% str_extract_all("[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]")
sandbox %>% str_count("[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]")
sandbox <- sandbox %>% 
  str_replace_all("[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]", 
                            " ") %>% 
  str_replace_all("( )+", " ")

corpus$emoji <- str_extract_all(corpus$text, "[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]")
corpus$emoji.count <- str_count(corpus$emoji, ",")+1
corpus$text <- corpus$text %>% 
  str_replace_all("[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]", 
                  " ") %>% 
  str_replace_all("( )+", " ")



# Hash tags ####

# Stored in separate column in Twitter API output anyway
# So we can clean them in the text

# I presume that camel case Hastags are read as seprate words
# and split accordingly (respecting up to five Words, see toy example)

# c("bla bla #EndImpunity bla bla",
#   "bla bla #EndImpunityForever bla bla",
#   "bla bla #EndImpunityForeverAndEver bla bla") %>% 
#   str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7 \\8\\9 \\10\\11\\12") %>% # 5 Camel Cased words in Hashtag
#   str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7 \\8\\9\\10") %>% # 4 Camel Cased words in Hashtag
#   str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7\\8") %>% # 3 Camel Cased words in Hashtag
#   str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5\\6") %>% # 2 Camel Cased words in Hashtag
#   str_replace_all("#", " ") %>% 
#   str_replace_all("( )+", " ")

sandbox <- sandbox %>% 
  str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7 \\8\\9 \\10\\11\\12") %>% # 5 Camel Cased words in Hashtag
  str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7 \\8\\9\\10") %>% # 4 Camel Cased words in Hashtag
  str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7\\8") %>% # 3 Camel Cased words in Hashtag
  str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5\\6") %>% # 2 Camel Cased words in Hashtag
  str_replace_all("#", " ") %>% 
  str_replace_all("( )+", " ")

corpus$text <- corpus$text %>% 
  str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7 \\8\\9 \\10\\11\\12") %>% # 5 Camel Cased words in Hashtag
  str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7 \\8\\9\\10") %>% # 4 Camel Cased words in Hashtag
  str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7\\8") %>% # 3 Camel Cased words in Hashtag
  str_replace_all("(#)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5\\6") %>% # 2 Camel Cased words in Hashtag
  str_replace_all("#", " ") %>% 
  str_replace_all("( )+", " ")



# @ Mentions ####

# Proceed as for Hashtags

sandbox <- sandbox %>% 
  str_replace_all("(@)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7 \\8\\9 \\10\\11\\12") %>% # 5 Camel Cased words in Mention
  str_replace_all("(@)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7 \\8\\9\\10") %>% # 4 Camel Cased words in Mention
  str_replace_all("(@)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7\\8") %>% # 3 Camel Cased words in Mention
  str_replace_all("(@)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5\\6") %>% # 2 Camel Cased words in Mention
  str_replace_all("@", " ") %>% 
  str_replace_all("( )+", " ")

corpus$text <- corpus$text %>% 
  str_replace_all("(@)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7 \\8\\9 \\10\\11\\12") %>% # 5 Camel Cased words in Mention
  str_replace_all("(@)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7 \\8\\9\\10") %>% # 4 Camel Cased words in Mention
  str_replace_all("(@)([A-Z])([a-z]+?)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5 \\6\\7\\8") %>% # 3 Camel Cased words in Mention
  str_replace_all("(@)([A-Z])([a-z]+?)([A-Z])([a-z]+?)( )", "\\1\\2\\3 \\4\\5\\6") %>% # 2 Camel Cased words in Mention
  str_replace_all("@", " ") %>% 
  str_replace_all("( )+", " ")



# Final cleaning and inspection ####

# The fanciest whitespace you have never seen
sandbox <- sandbox %>% 
  str_remove_all(" ️")# Not even matched by /s
corpus$text <-corpus$text %>% 
  str_remove_all(" ️")# Not even matched by /s

# Reduce multiple whitespaces again (you never know)
sandbox <- sandbox %>% 
  str_replace_all("( )+", " ")
corpus$text <- corpus$text %>% 
  str_replace_all("( )+", " ")

# Offset punctuation
sandbox <- sandbox %>% 
  str_replace_all(fixed(" . "), ". ") %>% 
  str_replace_all(fixed(" . "), ". ") %>% 
  str_replace_all(fixed(". . "), ". ") %>% 
  str_replace_all(fixed(".. "), ". ") %>% 
  str_replace_all(fixed(" . "), ". ")
corpus$text <- corpus$text %>% 
  str_replace_all(fixed(" . "), ". ") %>% 
  str_replace_all(fixed(" . "), ". ") %>% 
  str_replace_all(fixed(". . "), ". ") %>% 
  str_replace_all(fixed(".. "), ". ") %>%
  str_replace_all(fixed(" . "), ". ")

# Dots not followed by whitespace
sandbox <- sandbox %>% 
  str_replace_all("(\\.)([A-Z])", "\\1 \\2")
corpus$text <- corpus$text %>% 
   str_replace_all("(\\.)([A-Z])", "\\1 \\2")

# Doubled punctuation
sandbox <- sandbox %>% 
  str_replace_all("(\\.|\\?|!)(\\.)", "\\1 ") %>% 
  str_replace_all("(\\.|\\?|!)([ ]{0,2})(\\.)", "\\1 ") %>% 
  str_squish()
corpus$text <- corpus$text %>% 
  str_replace_all("(\\.|\\?|!)(\\.)", "\\1 ") %>% 
  str_replace_all("(\\.|\\?|!)([ ]{0,2})(\\.)", "\\1 ") %>% 
  str_squish()

# remove Whitespace left and right
sandbox <- sandbox %>% 
  str_trim(side = "both")
corpus$text <- corpus$text %>% 
  str_trim(side = "both")

# Colon at end of string (removed links...)
sandbox <- sandbox %>% 
  str_trim(side="both") %>% 
  str_remove_all(":$")
corpus$text <- corpus$text  %>% 
  str_trim(side="both") %>% 
  str_remove_all(":$")

# Make sure tweet ends with sentence endmarker (punctuation)
sandbox <- sandbox %>% 
  str_trim(side = "both") %>% 
  paste0(".") %>% 
  str_replace("(\\.|\\?|!)(\\.)", "\\1 ")
corpus$text <- corpus$text %>% 
  str_trim(side = "both") %>% 
  paste0(".") %>% 
  str_replace("(\\.|\\?|!)(\\.)", "\\1 ")

# Inspect random tweet text
corpus$text[sample(nrow(corpus), 1)]



# Exemplary comparison of sandbox ####

# Sandbox as initially set up above
# UNICOde and new lines converted manually (hmpf) to HTML entities to see them in outout
pre <- c("I fell asleep hoping to wake up from a bad dream.Europe is full of wonders that no one will bring us back. Preserving with #digitization is important for us &amp; for future generations. Close to the Parisians. With #NotreDame we've lost a piece of our history https://t.co/hQRqMGSsq3 https://t.co/CPLs1DqEcl",
         "&#x0001f91d; Sharing risk.<br>&#x0001f30d; Maximising impact.<br><br>Today we’ve signed 4 new guarantee agreements under the EU External Investment Plan to create more &#x0001f4a1; opportunities for people in countries near the EU and in Africa. <br><br>Read more &#x27a1;https://t.co/YY3zPWSti4<br>&#x01f4c8; #InvestGlobal #EIP https://t.co/HvYWuoVEOC",
         "We call on all countries to bring to justice those responsible for crimes against journalists. #EndImpunity <br><br>We’ll continue to defend freedom of expression and protect those who make use of it to keep us informed. <br><br>My statement with @JosepBorrellF: <br>https://t.co/8bfPJ5ChsY https://t.co/5MCHSSE8No",
         "Full room &#x0001f44f; for the workshop on “Advancing the Creation of Regional #Bioeconomy Clusters in #Europe” organised by @BBI2020 &amp; #SCAR #Bioeconomy Group. Great to see the participants commitment in creating a wider and more robust bioeconomy network in the #EU! &#x0001f91d; https://t.co/mzuX4a03aT",
         "At #SyriaConf2020, the international community pledged a total of €6.9 bn for #Syria &amp; the main countries hosting Syrian refugees for 2020 and beyond&#x1f30d;.<br><br>Out of this amount, the&#x1f1ea;&#x1f1fa;#EU pledged 71% (€4.9 bn) with €2.3 bn coming from the&#x1f1ea;&#x1f1fa;@EU_Commission <br><br>&#x27a7;️https://t.co/cvcAngTUk1 https://t.co/Obgz2rel3g")

# Comparison table
comp <- data.frame(pre = pre,
                   post = sandbox)

# Export
comp.out <- comp %>% 
  kable(col.names = c("Pre-Cleaning", "Post-Cleaning"), escape =F) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))
save_kable(comp.out, "./tables/TextCleaningComparison.html")
  

# Language detection ####

# Twitter language category contains some strange languages ('eu', 'uk'?) - we neeed to cross-check
# I use the ngram-profile (character level) based detection from textcat()
# Google'S cld3 hardly worked on such short pieces in my experince

# Initial exercise: tweet level

# corpus$language <- textcat(corpus$text) 
# 
# # Compare textcat to Twitter indicator
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

# textcat looks more reliable in the examples but main LESSON:
# It's actually mostly multi-lingual tweets!
# We need to mark them and asses their frequency
# And then extract only the english-parts and/or translate the others


# Language detection: sentence level
# Tokenize tweets into sentences, detect language along ngram-profile, 
# Note all languages used in tweet, cut-out the english parts

# Testing

sandbox <- "Happy Birthday! Bonne anniversaire! Alles Gute zum Geburtstag. Wszystkiego najlepszego z okazji urodzin! feliç aniversari! feliz cumpleaños! doğum günün kutlu olsun."
df <- spacy_tokenize(sandbox, what = "sentence") %>% 
  data.frame() %>% 
  rename(sentences = 1)
df$tc.simple <- textcat(df$sentences)
df$tc.ecimci <- textcat(df$sentences, p = ECIMCI_profiles) # Larger n-gram sample, no regional dialects, but only 26 languages (see: names(ECIMCI_profiles))
df$cld2 <- cld2::detect_language(df$sentences) # Google's old model driving Chrome, the best according to different source
df$cld3 <- cld3::detect_language(df$sentences) # Google's neuronal network model

# Textcat simple to many misclassifications with irrelvant languages
# Textcat ECMIMCI better, but limited language choice
# CLD2 most rebuts in quick sample, and honest if no lang detetcted - go for this now

# LESSON: Language detection is a challenge

# In future iterations we may think about an ensemble approach here
# Possible including external APIs ...


# Classify and clean tweet sentences

corpus$tweetlanguage <- cld2::detect_language(corpus$text) # Fallback if lang of individual sentences cannot be detected
corpus$text.en <- "NA" # Target column to store only english text
corpus$langlang <- "NA" # Target column to store all detected languages

for (i in 1:nrow(corpus)){
  
  # Progress
  print(i)
  print(round((i/nrow(corpus))*100, 2))
  
  # Sentence tokenizer
  df <- spacy_tokenize(corpus$text[i], what = "sentence") %>% 
    data.frame() %>% 
    rename(sentences = 1)
  
  # Language detection
  df$lang <- cld2::detect_language(df$sentences)
  df$lang[is.na(df$lang)] <- corpus$tweetlanguage[i] # Fallback: If language of sentence cannot be detected, use language detected for overall tweet
  
  # Store all detected languages
  langs <- unique(df$lang) %>% 
    as.character() %>% 
    sort() %>% 
    paste(collapse = ", ")
  corpus$langlang[i] <- langs
  
  # Drop non-english sentences
  df <- df %>% filter(lang == "en") 
  
  # Rebuild and store text
  en.text <- paste(df$sentences, collapse = " ")
  corpus$text.en[i] <- en.text
}

# To do
# Afrikaans -> NL !
# Galician GL -> ES?
# bs, hr
# xx-Qaai? 


# Cross-checks
sum(corpus$text.en == "") # Number of tweets without english content
sum(corpus$text.en == "" & corpus$tweetlanguage == "en", na.rm = T) # Should be empty
no.eng <- corpus %>%  filter(text.en =="") %>% # Inspect cases w/out english sentences
  select(text, tweetlanguage, langlang)

# Frequency of detected languages on tweet level
detected.langs <- as.data.frame(table(corpus$langlang, useNA = "ifany")) 
detected.langs <- detected.langs %>% arrange(desc(Freq))

# Frequency on language level
lang.freq <- data.frame(lang = character(0))
for (i in 1:nrow(corpus)) {
  languages <- str_split(corpus$langlang[i], ", ") %>% 
    as.data.frame() %>% 
    rename(lang = 1)
  lang.freq <- rbind(lang.freq, languages)
}
rm(languages)
lang.freq <- lang.freq %>% 
  group_by(lang) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

ggplot(lang.freq, aes(x=count, y=reorder(lang, count))) + 
  geom_col()+
  labs(title = "Frequency of detected languages in EUtweet sample",
       y= "Detected Language\n(Google Compact Language Detector 2 on sentence level)\n",
       x = "Frequency of language occurence in Tweets")+
  theme_minimal()

ggsave("./plots/LanguageFrequency.png", width = 16, height = 18, units = "cm")
  
# Average number of languages per tweet   
# which((str_count(corpus$langlang, ",")>2))

corpus$lang.num <- str_count(corpus$langlang, ",") + 1

ggplot(corpus, aes(x=lang.num)) + 
  geom_histogram() +
  # coord_flip() +
  labs(title = "Multilingual Tweets in EUTweet sample",
       y= "Absolute frequency\n",
       x = "Number of languages in Tweet\n(Google Compact Language Detector 2\non sentence level)")+
  theme_minimal()

ggsave("./plots/MultilingualTweetFrequency.png", width = 10, height = 10, units = "cm")



# Export cleaned corpus ####
write_rds(corpus, "./tweetcorpora/EUtweets_cleaned.RDS")
