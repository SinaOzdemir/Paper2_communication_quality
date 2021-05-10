#Putting the data together

library(tidyverse)

dataDIR<-paste0(getwd(),"/Data")

sourceDIR<-"C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/phd data/Phd_data_collection/Scripts/extended_list_collection/Actor_tweets"

data.files<-list.files(path = sourceDIR,pattern = "*.RDS",full.names = T)[which(str_detect(string = data.files,pattern = "_ascii",negate = T))]
agency.data<-readRDS(file = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/phd data/Phd_data_collection/Scripts/extended_list_collection/agencies_and_other_inst/actor_twt_.RDS")
eu_accounts<-readRDS(file = paste0(dataDIR,"/qc_paper_accounts.RDS"))
#test

a<-readRDS(file = data.files[1]) %>% filter(.,user_id%in%eu_accounts$user_id)
imp_vars<-colnames(a)[-1]

b<-select(a, matches(match = imp_vars))
c<-readRDS(data.files[283])
#reader

paper_data<- lapply(data.files,
                    function(x){readRDS(file = x) %>% filter(.,user_id%in%eu_accounts$user_id)})

paper_data_df<- do.call("rbind",paper_data[1:282])

janusz<-paper_data[[283]]

paper_data_df$date<-NULL

paper_data_df<-rbind(paper_data_df,janusz,agency.data)

paper_data_df<-paper_data_df[!duplicated(paper_data_df$status_id),]

saveRDS(object = paper_data_df,file = paste0(dataDIR,"/eu_tweets_raw.RDS"))


# readability paper data --------------------------------------------------


dataa<-read_csv(file = file.choose())
datab<-read_csv(file=file.choose())
dup_cols<-intersect(x = colnames(dataa),y=colnames(datab))
dup_cols<-dup_cols[3:12]

datac<-select(datab,!all_of(dup_cols))

datad<-inner_join(dataa,datac, by =c("screen_name","created_at")) %>%
  filter(.,screen_name%in%eu_accounts$screen_name)

saveRDS(object = datad,file = paste(dataDIR,"readability_paper_data_eu_only.RDS",sep = "/"))
