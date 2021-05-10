#################################################################
# Project:  EU Tweet
# Task:     Identify archival documents that may contain UK gov
#           Twitter accounts
# Authors:  Christian Rauh (30.04.2021)
################################################################

# Packages ####
library("rvest")
library("httr")
library("tidyverse")
library("lubridate")


# Game plan:
# Search gov.uk for all documents containing "Twitter" and store their links

# https://www.gov.uk/search/all?keywords=Twitter&order=relevance

# 12,742 results on April 30 2021
# 638 results pages


# Extract links to individual docs ####
links = data.frame()

for (pages in seq(from = 1, to = 638, by = 1)) {
  
  # Show iteration
  print(paste("Page:", pages))
  
  # Construct link to search results page
  link = paste0("https://www.gov.uk/search/all?keywords=Twitter&order=relevance&page=",pages,"")
  
  # Check whether it works
  if(http_error(link)) {next}
  
  # Parse html
  page = read_html(link)
  
  # Extract items
  # up_date = page %>% html_nodes ("time") %>% html_text() #  Date last modified according to Serra's research
  # headline = page %>% html_nodes (".gem-c-document-list__item-link") %>% html_text()
  pr_links = page %>% html_nodes(".gem-c-document-list__item-link") %>% html_attr("href") %>% paste("https://www.gov.uk", ., sep="")
  
  
  # Append to target DF
  links = rbind(links, data.frame(pr_links, stringsAsFactors = FALSE))
  
  # Probably pause for a second
  Sys.sleep(sample(0:1, 1))
  
}

# Intermediary saving
write_rds(links, "UK-TwitterDocLinks.rds")
# links <- read_rds("UK-TwitterDocLinks.rds")


# Extract handles and Twitter links ####

t.handles <- data.frame()
t.links <- data.frame()

for (i in 1:nrow(links)) {
  
  # Progress
  print(i)
  print((i/nrow(links))*100)
  
  # Check whether page works
  if(http_error(links$pr_links[i])) {next}
  
  # Read page
  page <- read_html(links$pr_links[i])
  
  # Collapse all paragraphs to one chunk of text
  text <- html_nodes(page, "p") %>% 
    html_text() %>% 
    paste(collapse = " ")
  
  # extract @ Handles
  handles <- str_extract_all(text, " @[A-Za-z0-9_-]+?( |\\.)") %>% 
    as.data.frame() %>% 
    rename(handle = 1)
  
  handles$handle <- substr(handles$handle, start = 1, stop = nchar(handles$handle)-1) %>% # Drop last charcetr
    str_trim()
  
  if(nrow(handles>0)) {t.handles <- rbind(t.handles, handles)}
  
  # extract Twitter links
  tls <- str_extract_all(text, " https://twitter.com/[A-Za-z0-9_-]+?( |\\.)") %>% 
    as.data.frame() %>% 
    rename(handle = 1)
  tls$handle <- substr(tls$handle, start = 1, stop = nchar(tls$handle)-1) %>% # Drop last character
    str_trim() # Trim whitespace
  
  if(nrow(tls>0)) {t.links <- rbind(t.links, tls)}
}

# Combine @ handles and Twitter links
accounts <- t.links %>% 
  mutate(handle = str_replace(handle, fixed("https://twitter.com/"), "@")) %>% 
  rbind(t.handles) %>% 
  group_by(handle) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  mutate(link = str_replace(handle, "@", "https://twitter.com/"))

hist(accounts$freq)

# Export ####
write_rds(accounts, "ProbableUKgovAccounts.rds")


# Next steps ####
# Check whether accounts are 'verified'
# maybe apply frequency filter
# Filter out embassies @UKin...
# Few faulty ones spotted ...
