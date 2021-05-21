

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