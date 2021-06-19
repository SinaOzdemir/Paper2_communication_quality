#############################################################
# Project:  EU Tweet
# Task:     Collect past follower counts for UK Tweet accounts
# Author:   @ChRauh (18.06.2021)
#############################################################


# Packages ####
library(tidyverse) # 1.3.0
library(rvest)

# Tools ####
source("./Scripts/EUfollowerCounts/PastTwitter.R") # See https://github.com/ChRauh/PastTwitter



# List of UK accounts ####
# created by 0_BuildUKtweetCorpus.R
accounts <- read_rds("./analysis_data/UK_account_list.RDS") 

accounts$snapshots <- as.numeric(NA)


# Filter accounts ####

# We do not use parliamentary accounts in the main analysis
# and also dropped 'helpdesk' accounts from UK agencies
# I aplly the same filters here to save downloading time

ukinfo <- read.csv2("./data/uk_accounts_CRedit.csv") %>% 
  mutate(twitter_handle = str_trim(twitter_handle)) %>% 
  rename(screen_name = twitter_handle) %>% 
  select(screen_name, account_type, institutional_affiliation)

parl <- ukinfo %>% filter(str_detect(institutional_affiliation, "House of "))
parl <- parl[,1]

accounts <- accounts %>% 
  filter(!(screen_name %in% parl)) %>% 
  filter(screen_name != "HMRCcustomers") %>% 
  filter(screen_name != "nsandihelp") 

rm(ukinfo)
rm(parl)



# Extract archive.org data #####

for (i in 1:nrow(accounts)) {

  # Progress
  print(paste0("Account ", i, " of ", nrow(accounts),"; ", accounts$screen_name[i]))
  
  # Establish connection for saving
  filename <- paste0("./data/FollowerCounts/UK/", accounts$screen_name[i], ".RDS")
  
  # Check wehther file has already been created
  # if yes continue with next profile
  # (when aborting in between, remove latest file - may be incomplete)
  if(file.exists(filename)) {
    
    # Store number of snapshots from earlier download
    accounts$snapshots[i] <- nrow(read_rds(filename))
  }
  
  else {
    
    # Get archive.org snapshots for Twitter profile
    snapshots <- handleSnapshots(accounts$screen_name[i])
    
    # Store number of snapshots available for profile
    accounts$snapshots[i] <- nrow(snapshots)
    
    # If no snapshots are available for profile, go to the next one
    if(nrow(snapshots)==0) {next}
    
    # Extract info from avialable snapshots
    df <- extractAccountInfo(snapshots)
    
    # Save to disk
    write_rds(df, filename)
    
  }
}



# Store info on avaliable snapshots ####
archive <- accounts %>% select(screen_name, snapshots)
write_rds(archive, "./analysis_data/UK-snapshots.RDS")


