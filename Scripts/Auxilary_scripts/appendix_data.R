packs<- c("tidyverse","rtweet","kableExtra","here")
lapply(packs,library, character.only = T)

data_path<- here("data")

# analysis_data<- readRDS(file = file.path(data_path,"corpii","AnalyticData_AllSamples.RDS"))
analysis_data<- readRDS(file = file.path(data_path, "AnalyticData_AllSamples.RDS"))

eu_accounts<- analysis_data %>%
  filter(str_detect(tweetsample, fixed("EU "))) %>% 
  pull(screen_name) %>% unique()

uk_accounts<-  analysis_data %>%
  filter(str_detect(tweetsample, fixed("UK "))) %>% 
  filter(screen_name != "HMRCcustomers") %>%  
  filter(screen_name != "DVLAgovuk") %>%  
  filter(screen_name != "nsandihelp")  %>% 
  pull(screen_name) %>%
  unique()

io_accounts<- analysis_data %>%
  filter(str_detect(tweetsample, fixed("IO"))) %>% 
  pull(screen_name) %>% unique()
remove(analysis_data)


# EU appendix -------------------------------------------------------------


list.files(path = file.path(data_path,"EU","rds_clean"),
                       full.names = T,recursive = F,pattern = "*.RDS") %>% 
  map_dfr(.x = .,.f = readRDS) %>%
  filter(user_username%in%eu_accounts) %>% 
  select(user_name,
         user_description,
         user_username,
         user_public_metrics.tweet_count,
         user_public_metrics.followers_count,
         user_created_at) %>% distinct() %>% 
  mutate(user_username = paste0("@",.$user_username),
         user_name = str_remove_all(string = .$user_name,pattern = "[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]"),
         user_description = str_remove_all(string = .$user_description,pattern = "[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]"),
         user_description = str_replace_all(string = .$user_description,pattern = "\\n", " ")) %>% 
  write.table(x = .,
              file = file.path(data_path,"EU_analysis_sample.tsv"),
              sep = "\t",
              col.names = c("Actor Name",
                            "Account Description",
                            "Handle",
                            "n tweets",
                            "Follower count",
                            "Account Creation"),
              row.names = F,quote = T,fileEncoding = "UTF-8")



# UK appendix -------------------------------------------------------------


uk_data_list<-list.files(path = file.path(data_path,"UK","rds_clean"),
           full.names = T,recursive = F,pattern = "*.RDS") %>%
  grep(pattern = paste(uk_accounts,collapse = "|"),x = .,value = T,perl = T)

uk_appendix<- data.frame()

for (i in 1:length(uk_data_list)) {
  
uk_actor<-readRDS(uk_data_list[i]) %>%
  select(user_name,
         user_description,
         user_username,
         user_public_metrics.tweet_count,
         user_public_metrics.followers_count,
         user_created_at) %>% distinct() %>% 
  mutate(user_username = paste0("@",.$user_username),
         user_name = str_remove_all(string = .$user_name,pattern = "[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]"),
         user_description = str_remove_all(string = .$user_description,pattern = "[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]"),
         user_description = str_replace_all(string = .$user_description,pattern = "\\n", " "))
  
uk_appendix<-rbind(uk_appendix,uk_actor)
}

write.table(x = uk_appendix,
            file = file.path(data_path,"uk_analysis_sample.tsv"),
            sep = "\t",
            col.names = c("Actor Name",
                          "Account Description",
                          "Handle",
                          "n tweets",
                          "Follower count",
                          "Account Creation"),
            row.names = F,
            quote = T,
            fileEncoding = "UTF-8")

# IO appendix -------------------------------------------------------------


list.files(path = file.path(data_path,"IO","rds_clean"),
           full.names = T,recursive = F,pattern = "*.RDS") %>% 
  map_dfr(.x = .,.f = readRDS) %>%
  filter(user_username%in%io_accounts) %>% 
  select(user_name,
         user_description,
         user_username,
         user_public_metrics.tweet_count,
         user_public_metrics.followers_count,
         user_created_at) %>% distinct() %>% 
  mutate(user_username = paste0("@",.$user_username),
         user_name = str_remove_all(string = .$user_name,pattern = "[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]"),
         user_description = str_remove_all(string = .$user_description,pattern = "[\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}]"),
         user_description = str_replace_all(string = .$user_description,pattern = "\\n", " ")) %>% 
  write.table(x = .,
              file = file.path(data_path,"IO_analysis_sample.tsv"),
              sep = "\t",
              col.names = c("Actor Name",
                            "Account Description",
                            "Handle",
                            "n tweets",
                            "Follower count",
                            "Account Creation"),
              row.names = F,quote = T,fileEncoding = "UTF-8")


