#twitter json parsing

# packages ----------------------------------------------------------------


library(pacman)
packs<- c("academictwitteR","tidyverse","tidyjson","rjson")
#devtools::install_github("cjbarrie/academictwitteR")

pacman::p_load(char = packs)


# directories -------------------------------------------------------------

meta_dataDIR<- paste0(getwd(),"/metadata/")

json_dataDIR<- paste0(getwd(),"/Data/json/")

rds_dataDIR<- paste0(getwd(),"/Data/rds/")


# tidyjson tutorial -------------------------------------------------------

#some example json:

people <- c('{"age": 32, "name": {"first": "Bob",   "last": "Smith"}}',
            '{"age": 54, "name": {"first": "Susan", "last": "Doe"}}',
            '{"age": 18, "name": {"first": "Ann",   "last": "Jones"}}')

#parsing the sample json:

tidypeople <- people %>% spread_all()

head(tidypeople)

#a more complex example:

wb_data<- worldbank

wb_data_tidy<- wb_data %>% spread_all()#nested variables are not spread properly in this method

#handling arrays in json
# find out which variables array
#in this case, array elements in json are like nested datasets (datasets stored in datasets)
#they need to be unpacked and added to the high level dataset

#this pipe identifies which elements in json are arrays

wb_tidy_array<-wb_data %>%
  gather_object() %>% # identifies value-name pairs in each document/json entry
  json_types() %>% # this identifies the type of json object 
  count(name, type) #this function is from dplyr. Summarizes value-name pair types

#this pipe pulls out the array out of json and unnests it  

wb_unnested_array <- wb_data %>%
  enter_object(majorsector_percent) %>% #navigates to a certain value-name in json
  gather_array() %>%  # I am not really sure what this does. Vignette says it collapses a json array into index-value pairs, creating a new column "array.index" to store the index of the array and values in the "Json" attribute
  spread_all()#this converts arrays into long format dataframe. With some manipulation this can be turned into wide-format and added to the main data


#example of aggregating:
#I am not really sure what he is doing here
#vignette says aggregate funding by sector and region
#but I am not familiar with the data so can't tell
test_aggregation<- wb_data %>%
  spread_all %>% select(document.id,region = regionname, funding = totalamt) %>%
  enter_object(majorsector_percent) %>% gather_array %>% 
  spread_all %>% rename(sector = Name, percent = Percent) %>%
  group_by(region, sector) %>%
  mutate(nominal_funding = sum(funding * percent))


#some additional common uses of tidyjson:

##Filter for a specific type:
comp <- companies
#identifying and gathering arrays

comp_arrays<-comp[1] %>%
  gather_object() %>%
  json_types() %>% 
  count(name, type) %>%
  filter(type == "array")#there are 13 arrays in comp[1] json

#unnesting all arrays  
comp_arrays_unnest<-comp[1] %>% gather_object %>% 
  filter(is_json_array(.)) %>% gather_array() %>% spread_all()#but there are 6 arrays missing 


# some test with actual twitter json -------------------------------------

meta_data<- readRDS(file = paste0(meta_dataDIR,"metadata_14-03-2021.RDS")) %>% filter(user_id == "415555178")



# first try with jsonlite -------------------------------------------------
#from: https://developer.twitter.com/en/docs/tutorials/getting-started-with-r-and-v2-of-the-twitter-api
#need to dig deeper into this blog post, it seems the way it communicates with Twitter API seems to return a different data structure
#than academictwitteR, returns it.

test_jsonlist1<- jsonlite::fromJSON(txt = paste0(json_dataDIR,"metadatadata_141787589999984640.json"),flatten = T) %>% as_tibble()
#this turns everything into neat dataset, some columns are still lists which needs to be unlisted and turned into dataframe

json_meta<- Hmisc::contents(test_jsonlist1) 

json_content_meta<- json_meta[[1]] %>% as_tibble(.,rownames = "variable.names")
#honestly this method is superbly easy to parse data, academictwitteR is a retard in handling returned data...



# flattening rds ----------------------------------------------------------

test_rds1<- readRDS(file = paste0(rds_dataDIR,meta_data$screen_name[1],".RDS"))

test_flatt_rds1<- jsonlite::flatten(test_rds1)#everything is flattened, now its time to unlist the lists

test_flatt_rds1_meta<- Hmisc::contents(test_flatt_rds1)[[1]] %>% as_tibble(rownames = "variable_names") %>% filter(Storage == "list")

to_unlist <- test_flatt_rds1 %>% select(id,all_of(test_flatt_rds1_meta$variable_names))

a<- do.call("rbind",to_unlist$entities.mentions)

#ok I need to find a way to parse dfs that has more than 1 rows.

entities.mention.meta<-tibble()

for (i in 1:nrow(to_unlist)) {
  if(class(to_unlist$entities.mentions[[i]]) != "NULL"){
  b<-tibble(twt_id = to_unlist$id[i],
            nrow = Hmisc::contents(to_unlist$entities.mentions[[i]])[2][[1]][1],
            ncol = Hmisc::contents(to_unlist$entities.mentions[[i]])[2][[1]][2])
  
  entities.mention.meta <- rbind(entities.mention.meta,b)
  }else{
    b<- tibble(twt_id = to_unlist$id[i],
               nrow = 0,
               ncol = 0)
    entities.mention.meta<- rbind(entities.mention.meta,b)
    
  }
}

#unnest test
#ok this unnests if I go one at a time.
to_unlist_names<-colnames(to_unlist)[-1]

c<- to_unlist %>%  unnest(cols = all_of(to_unlist_names[1:2]),names_sep = ".") 

#I don't need to unnest everything!
#.annotations may not be necessary
#absolutely necessary ones: referenced_tweets, entities mentioned,entities hashtag(its a list), 
