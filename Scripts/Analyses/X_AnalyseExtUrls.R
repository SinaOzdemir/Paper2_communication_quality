

corpus <- read_rds("./data/corpii/EU_corpus_cleaned.RDS") %>% 
  select(entities.urls) 

# Extract URLS of tweets
# Stored in list, to flat

# Target variables
corpus$urls <- NA

# create progress bar
pb <- txtProgressBar(min = 0, max = nrow(corpus), style = 3)

# Loop over obs and extract urls
for (i in 1:nrow(corpus)) {
  
  # Check whether the tweet data contain url entity information 
  # (first element of respective list must contain something),
  # jump to next iteration if not
  
  if(length(corpus$entities.urls[i][[1]]) == 0) {next}
  
  # Urls stored for each obs
  urls <- corpus$entities.urls[i][[1]]
  
  # Store urls as charater string, separate by ', '
  corpus$urls[i] <- paste(unique(urls$expanded_url), collapse = ", ")
  
  # Only URLS refering to media stored on dedicated twitter servers #
  # CHECK TWIMG!!!
  media <- paste(unique(urls$expanded_url[str_detect(urls$display_url, "(pic\\.twitter\\.com)|(twimg)")]),
                 collapse = ", ")
  corpus$mediaurls[i] <- ifelse(length(media) > 0 , media, NA)
  
  # Only URLS not referring to media stored on dedicated twitter servers
  ext <- paste(unique(urls$expanded_url[!str_detect(urls$display_url, "(pic\\.twitter\\.com)|(twimg)")]),
               collapse = ", ")
  corpus$exturls[i] <- ifelse(length(ext) > 0 , ext, NA)
  
  # update progress bar
  setTxtProgressBar(pb, i)
  
}

# Long list of all URLS

urls <- paste(corpus$exturls, collapse = ", ") %>%  # All extracted exteral URLS
  str_split(", ") %>% # Atomic representation
  as.data.frame() %>% 
  rename(url = 1) %>% 
  filter(url != "") %>% 
  filter(!is.na(url)) %>% 
  group_by(url) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Main Servers

urls$server <- urls$url %>% 
  str_remove(fixed("http://")) %>% 
  str_remove(fixed("https://")) %>%
  str_remove("^www\\.") %>% 
  str_remove("/.*$")

servers <- urls %>%  select(server, count) %>% 
  filter(!str_detect(server, "twit")) %>% 
  group_by(server) %>% 
  summarise(count = sum(count)) %>% 
  mutate(share = (count/sum(count)) * 100) %>% 
  arrange(desc(count))

servers$eudomain <- str_detect(servers$server, fixed("europa.eu"))
servers$shortener <-  servers$server %in% c("bit.ly", "ow.ly", "goo.gl", "tinyurl.com", "tiny.cc", "buff.ly", "j.mp")

write_rds(servers, "./analysis_data/EU-ExternalDomainsReferredTo.RDS")

# Shorteners
sum(servers$share[servers$shortener])

# europe.eu domains
eu <- servers %>%  filter(eudomain)
sum(eu$share)
head(eu, 30)

# Other links
ext <- servers %>%  filter(!eudomain & !shortener)
head(ext, 30)

# Other social media (roughly)
ext$social <- ext$server %in% c("dlvr.it", "facebook.com", "fb.me", "on.fb.me", "lnkd.in", "linkedin.com", "instagram.com", "flickr.com")
sum(ext$share[ext$social])




