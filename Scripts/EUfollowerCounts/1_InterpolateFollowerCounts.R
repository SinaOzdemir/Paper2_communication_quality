#####################################################################
# Project:  EU Tweet
# Task:     Interpolate follower counts (collected from archive.org)
# Author:   @ChRauh (25.052021)
#####################################################################


# Packages ####
library(tidyverse) # 1.3.0
library(zoo) # Time series stuff 1.8-8



# Required lists ####
files <- paste0("./data/FollowerCounts/EU/",
                list.files("./data/FollowerCounts/EU/")) # Files with info on individual profiles
accounts <- read_rds("./analysis_data/EU_account_list.RDS") # EU account info
snapshots <- read_rds("./analysis_data/EU-snapshots.RDS") # Info on avilable archive.org snapshots
accounts <- left_join(accounts, snapshots, by = "screen_name")
rm(snapshots)



# Interpolation of web.archive.org snapshot data ####
# Linear, between all dates for which we have a measurement

followercounts <- data.frame() # target data frame

for (i in 1:length(files)) {
  
  # Current Twitter profile
  profile <- files[i] %>% 
    str_extract("[^/]*?RDS") %>% 
    str_remove(fixed(".RDS"))
  
  # Progress
  print(paste0("File ", i, " of ", length(files), "; ", profile))
  
  # Read info collected from archive.org
  snaps <- read_rds(files[i]) %>% 
    mutate(date = as.Date(date, format = "%Y%m%d")) %>% 
    select(screen_name, date, follower_count) %>% 
    filter(!is.na(follower_count))
  
  
  # Add information on the day the profile was created
  # Everybody should start with 0 followers
  creadate <- accounts$user_created_at[accounts$screen_name == profile] %>% 
    str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}") %>% 
    as.Date(format = "%Y-%m-%d")
  snaps <- snaps %>% filter(date != creadate) # In case it's in there already
  snaps <- rbind(snaps,
                 data.frame(screen_name = profile,
                            date = creadate,
                            follower_count = 0))
  
  # Add information on follower counts for the day
  # we have scraped our tweets (should be May 5 2021)
  snaps <- snaps %>% filter(date != as.Date("20210505", format = "%Y%m%d")) # In case it's in there already
  snaps <- rbind(snaps,
                 data.frame(screen_name = profile,
                            date = as.Date("20210505", format = "%Y%m%d"),
                            follower_count = accounts$user_public_metrics.followers_count[accounts$screen_name == profile]))
  
  # Span the ful range of days between the earliest and latest date 
  # for which we have follower counts
  series <- as.data.frame(seq.Date(min(snaps$date), max(snaps$date), by = "days")) %>% 
    rename(date = 1) %>% 
    mutate(screen_name = unique(snaps$screen_name))
  
  # Add actually observed follower counts to this series
  series <- left_join(series, snaps[, c("date", "follower_count")], by = "date") %>% 
    arrange(date)
  
  # Linearily interpolate follower_counts (integers!)
  # In between all actually observations ... (with the help of zoo)
  series.z <- read.zoo(series[, c("date", "follower_count")])
  interpolation <- na.approx(series.z, xout = seq(start(series.z), end(series.z), "day")) %>% 
    fortify.zoo() %>% 
    rename(date = Index,
           follower_count_interpolated = 2) %>% 
    mutate(follower_count_interpolated = as.integer(follower_count_interpolated))
  
  # ... and add it to the series ...
  series <- left_join(series, interpolation, by = "date")
  
  # ... and append to target data frame
  followercounts <- rbind(followercounts, series)
  
}

# Clean up
rm(list = c("snaps", "creadate", "series", "series.z", "interpolation"))

# Cross-checks
length(unique(followercounts$screen_name)) == length(files)



# Interpolate followers foe accounts w/out archive.org snapshots ####
# Between account creation and scraping date

nosnaps <- accounts %>% 
  filter(snapshots == 0) %>% 
  select(screen_name, user_created_at, user_public_metrics.followers_count) %>% 
  rename(follower_count = user_public_metrics.followers_count,
         creadate = user_created_at) %>% 
  mutate(creadate = creadate %>% 
           str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}") %>% 
           as.Date(format = "%Y-%m-%d"))

for (i in 1:nrow(nosnaps)) {
  

  # Establish full range of days between account creation and scraping date
  series <- as.data.frame(seq.Date(nosnaps$creadate[i], as.Date("20210505", format = "%Y%m%d"), by = "days")) %>% 
    rename(date = 1) %>% 
    mutate(screen_name = nosnaps$screen_name[i],
           follower_count = as.integer(NA))
  
  # Start and end values
  series$follower_count[1] <- 0
  series$follower_count[nrow(series)] <- nosnaps$follower_count[i]
  
  # Linearily interpolate follower_counts (integers!)
  # In between all actually observations ... (with the help of zoo)
  series.z <- read.zoo(series[, c("date", "follower_count")])
  interpolation <- na.approx(series.z, xout = seq(start(series.z), end(series.z), "day")) %>% 
    fortify.zoo() %>% 
    rename(date = Index,
           follower_count_interpolated = 2) %>% 
    mutate(follower_count_interpolated = as.integer(follower_count_interpolated))
  
  # ... and add it to the series ...
  series <- left_join(series, interpolation, by = "date")
  
  # ... and append to target data frame
  followercounts <- rbind(followercounts, series)
  
}

# Clean up
rm(list = c("series", "series.z", "interpolation"))

# Cross-cehck
length(unique(followercounts$screen_name)) == 115


# Export follower counts ####
write_rds(followercounts, "./analysis_data/FollowerCountsInterpolated_EU.RDS")
