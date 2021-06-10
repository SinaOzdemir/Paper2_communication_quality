#two-way interaction indicator functions:

print("Important: Functions are written based on data structure of a dataset in rds_clean in case folder!")

req_colnames<-paste(c("tweet_id",
                "author_id",
                "user_username",
                "is_quote",
                "is_reply",
                "is_retweet",
                "reply_to_status_id",
                "retweet_status_id",
                "quoted_status_id",
                "tweet_in_reply_to_user_id",
                "tweet_date"),collapse =  " ")

print(paste0("Functions require following columns: ",req_colnames))


# read packages -----------------------------------------------------------



packs<- c("tidyverse","Hmisc","rtweet","ggraph","patchwork")
existing_packs<-installed.packages()[,1]
#Function works
package_loader<- function(packs = character(0)){
  existing_packs<- installed.packages()[,1]
  if(all(packs%in%existing_packs)){
    lapply(packs,library, character.only = T)
  }else{
    missing_packages<- packs[-which(packs%in%existing_packs)]
    install.packages(missing_packages)
    lapply(packs,library(character.only = T))
  }
  
}

package_loader(packs = packs)

# helper functions --------------------------------------------------------

#Function works!
make.dir<- function(file.path){
  if(dir.exists(file.path)){
    return(file.path)
  }else{
    dir.create(path = file.path, recursive = T)
    return(file.path)
  }
}

#Function works!
data_reader<- function(file_path, case = c("UK","EU","IO")){
  
  req_columns<- c("tweet_id",
                      "author_id",
                      "user_username",
                      "is_quote",
                      "is_reply",
                      "is_retweet",
                      "reply_to_status_id",
                      "retweet_status_id",
                      "quoted_status_id",
                      "tweet_in_reply_to_user_id",
                      "tweet_date")
  
  check<- readline(prompt = "This function is intended to read final analysis data.\n Are you reading the preprocessed data? \n 1: Yes\n 2: No")
  
  if(check == 1){
  
  cat("reading the preprocessed data.","Due to file size this may take a minute.",sep = "\n")
    
  if(case == "UK"){
    data_path<- file.path(file_path,"corpii","UK_corpus.RDS")
  }else if(case == "IO"){
    data_path <- file.path(file_path,"corpii","IO_corpus.RDS")
  }else if(case == "EU"){
    data_path <- file.path(file_path,"corpii","EU_analysis_data.RDS")
  }else{
    stop("please choose one of the following cases:\n UK\n EU \n IO")
  }
    
  data<- readRDS(data_path)

  if(all(req_columns%in%colnames(data))){
    cat("all required columns are present!")
    return(data)
  }
  else{
    missing_columns<- req_columns[-which(req_columns%in%colnames(data))]
    cat("following columns are missing:", missing_columns, sep = "\n")
    stop("aborting the data read")
  }
  
  }
  else{
    stop("you need to import the preprocessed data for other functions to work")
  }
}

# variable manipulators ---------------------------------------------------

#Function works!
data_cleaner<- function(data){
  
  clean_data<- data %>%filter(langlang %in% "en") %>% 
    mutate(across(.cols = starts_with("is_"), ~tidyr::replace_na(.x, 0))) %>%
    mutate(across(.cols = starts_with("is_"),~as.numeric(.x))) %>% 
    mutate(tweet_date = lubridate::as_date(.$tweet_created_at)) %>%
    mutate(tweet_year = lubridate::year(lubridate::floor_date(tweet_date,unit = "year")),
           tweet_month = lubridate::month(lubridate::floor_date(tweet_date,unit = "month"),label = T),
           tweet_week = lubridate::floor_date(tweet_date,unit = "week"),
           tweet_calendar_day = base::weekdays(tweet_date))
  
  return(clean_data)
    
}

#Function works
descriptive_plots<- function(data,
                             case = c("the EU","the UK","IO"),
                             what = c("reply","quote","retweet"),
                             unit = c("year","month","week","calendar_day"),
                             graph_type = c("bar_stack","bar_dodge","scatter")){
  
  trimmed_data<- data %>% select(tweet_id,
                                 author_id,
                                 user_username,
                                 tweet_in_reply_to_user_id,
                                 matches("is_"),
                                 tweet_date,
                                 tweet_year,
                                 tweet_month,
                                 tweet_calendar_day)
  
  if(what == "reply"){
    cat("removing threads pretending to be replies")
    trimmed_data<- trimmed_data %>% 
      filter(tweet_in_reply_to_user_id != author_id)
  }
  
  aggregation_data<- trimmed_data %>%
    select(tweet_id,author_id,user_username, matches(what),matches(unit))
  
  colnames(aggregation_data)<-c("tweet_id","user_id","screen_name","filtering_unit","aggregation_unit")
  
  #reading from github didn't work, read the local file.Right now it only works for the EU, need to make 
  #manual data reading more flexible to accommodate other cases.
  
  
  # TODO: There are some missing accounts in the manually coded categories. It should be added there at some point
  manual_categories<-readRDS(file = file.path(getwd(),"data","Accounts","EU_handcoded_categories.RDS"))
  
  
  aggregation_data<- manual_categories %>%
    filter(user_id%in%aggregation_data$user_id & screen_name %in%aggregation_data$screen_name) %>% 
    select(user_id,screen_name,Actor_type) %>% left_join(x = aggregation_data,y = .,by = c("user_id","screen_name"))
  

  aggregated_data<- aggregation_data %>%
    group_by(aggregation_unit,Actor_type) %>% 
    summarise(absolute_freq = sum(filtering_unit),
              percentage_share = (sum(filtering_unit)/n()))
  
  if(graph_type == "bar_stack"){
  desc_plot<- aggregated_data %>%drop_na() %>% 
    ggplot(.,aes(x = aggregation_unit,y = percentage_share)) +
    geom_bar(aes(fill = Actor_type),color = "black",position = "stack",stat = "identity")+
    scale_x_continuous(breaks = min(aggregated_data$aggregation_unit):max(aggregated_data$aggregation_unit))+
    #ylim(0,1)+
    # geom_text(aes(label = absolute_freq),
    #           color = "white",
    #           size = 5,
    #           fontface = "bold",
    #           position = position_stack(vjust = .5))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(x = unit,
         y = "Percentage share",
         title = paste0(case," verified accounts ", what," share"),
         subtitle = paste0("N accounts = ",
                           length(unique(aggregation_data[,"screen_name"])),
                           "\n",
                           "N tweets =", nrow(aggregation_data)))
    
  return(desc_plot)}
  
  if(graph_type == "bar_dodge"){
    desc_plot<- aggregated_data %>%drop_na() %>% 
      ggplot(.,aes(x = aggregation_unit,y = percentage_share)) +
      geom_bar(aes(fill = Actor_type),color = "black",position = "dodge",stat = "identity")+
      scale_x_continuous(breaks = min(aggregated_data$aggregation_unit):max(aggregated_data$aggregation_unit))+
      #ylim(0,1)+
      # geom_text(aes(label = absolute_freq),
      #           color = "white",
      #           size = 5,
      #           fontface = "bold",
      #           position = position_stack(vjust = .5))+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(x = unit,
           y = "Percentage share",
           title = paste0(case," verified accounts ", what," share"),
           subtitle = paste0("N accounts = ",
                             length(unique(aggregation_data[,"screen_name"])),
                             "\n",
                             "N tweets =", nrow(aggregation_data)))
    
    return(desc_plot)}
  
  if(graph_type == "scatter"){
    warning("!!Scatter plot is still very primitive!!")
    
    desc_plot<- aggregated_data %>%drop_na() %>% 
      ggplot(.,aes(x = aggregation_unit,y = percentage_share)) +
      geom_point(aes(color = Actor_type,size = percentage_share ,alpha = percentage_share))+
      scale_x_continuous(breaks = min(aggregated_data$aggregation_unit):max(aggregated_data$aggregation_unit))+
      theme_minimal()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(x = unit,
           y = "Percentage share",
           title = paste0(case," verified accounts ", what," share"),
           subtitle = paste0("N accounts = ",
                             length(unique(aggregation_data[,"screen_name"])),
                             "\n","N tweets =",
                             nrow(aggregation_data)))
    return(desc_plot)
  }
}
