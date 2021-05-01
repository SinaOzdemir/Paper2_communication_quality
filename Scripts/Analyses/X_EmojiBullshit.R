library(tidyverse) # 1.3.0
library(systemfonts) # 1.0.1
library(ragg) # 1.1.2



# Emoji fun ####

# Most frequent emojies - but country flags ... external list of regional indicators followed by each other ...

# Corpus of only Commission (main account) tweets
ecemo <- corpus %>% filter(screen_name == "EU_Commission") %>%  # Not many?
  select(text_raw) 

# Extract all regional indicators (flag sequences)
# From 1F1E6 to 1F1FF: https://en.wikipedia.org/wiki/Regional_indicator_symbol

ecemo$regional <- "NA"

for (i in 1:nrow(ecemo)) { # Might be a more lightweigth apply approach to this
  
  # Progress
  print(i)
  
  # Extract all region codes
  hits <- str_extract_all(ecemo$text_raw[i], "[\U{1F1E6}-\U{1F1FF}]") %>% 
    unlist() %>% # Flatten
    paste(collapse = " ")
  
  # If empty, leave untouched
  if(length(hits)<=0) {next}
  
  # Store flat version 
  ecemo$regional[i] <- hits
}

# See what's in there
# ris <- ecemo %>% 
#   group_by(regional) %>% 
#   summarise(count = n()) %>% 
#   arrange(desc(count))


# Count flags 
flags <- ecemo %>% 
  filter(regional != "") %>% 
  select(regional)

flags$regional <- flags$regional %>% 
  str_replace_all("(.*? .*?)( )", "\\1SPLIT") # Should be every second occurence of whitespace

flags2 <- data.frame(flag = character(0))

for (i in 1:nrow(flags)) {
  
  # Split on marker
  indflags <- str_split(flags$regional[i], "SPLIT") %>% 
    as.data.frame() %>% 
    rename(flag = 1) %>% 
    mutate(flag = str_remove_all(flag, "\\s")) #Drop all white space, should be flag sequences then
  
  # Append to target
  flags2 <- rbind(flags2, indflags)
}

flagcounts <- flags2 %>% 
  group_by(flag) %>% 
  summarise(count = n()) %>% 
  rename(emoji = flag) %>% 
  arrange(desc(count))


# Count other emojis
# Exploiting the UNICODE blocks specified above
# Here we don't have to care about seuqences (I hope)

ecemo$emoji <- "NA"

for (i in 1:nrow(ecemo)) { # Might be a more lightweigth apply approach to this
  
  # Progress
  print(i)
  
  # Extract all region codes
  hits <- str_extract_all(ecemo$text_raw[i], "[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]") %>% 
    unlist() %>% # Flatten
    paste(collapse = " ") %>% 
    str_remove_all("[\U{1F1E6}-\U{1F1FF}]") %>% # Drop region block, was within range of bigger block
    str_replace_all("\\s+", " ") # Multiple whitespaces to one
  
  # If empty, leave untouched
  if(length(hits)<=0) {next}
  
  # Store flat version 
  ecemo$emoji[i] <- hits
}

emojis <- ecemo %>% 
  filter(emoji != "") %>% 
  filter(emoji != " ") %>% 
  select(emoji) %>% 
  mutate(emoji = str_squish(emoji))

emojis2 <- data.frame(emoji = character(0))
for (i in 1:nrow(emojis)) {
  
  # Split on marker
  indflags <- str_split(emojis$emoji[i], " ") %>% 
    as.data.frame() %>% 
    rename(emoji = 1) %>% 
    mutate(emoji = str_remove_all(emoji, "\\s")) #Drop all white space, should be flag sequences then
  
  # Append to target
  emojis2 <- rbind(emojis2, indflags)
}

emojiscount <- emojis2 %>% 
  group_by(emoji) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


# Combine counts of both type of symbols
emocount <- flagcounts %>% 
  rbind(emojiscount) %>% 
  arrange(desc(count))



# And plot 

ggplot(head(emocount, 25), aes(x=count, y=reorder(emoji, count))) + 
  # geom_col(width = .5)+
  geom_text(
    aes(label = emoji), 
    family = "Segoe UI Emoji")+
  labs(title = "The Commission's most favorite Emojis",
       x = paste0("Number of occurrences\nin ", nrow(ecemo), " Tweets from @EU_Commission"),
       y = "")+
  theme_minimal()+
  theme(axis.text.y = element_blank())

ggsave(file = "./plots/CommEmojis.png", width = 16, height = 16, units = "cm")