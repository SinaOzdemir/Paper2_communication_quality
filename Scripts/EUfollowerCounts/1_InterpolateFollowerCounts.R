#####################################################################
# Project:  EU Tweet
# Task:     Interpolate follower counts (collected from archive.org)
# Author:   @ChRauh (25.052021)
#####################################################################


# Packages ####
library(tidyverse) # 1.3.0
library(zoo)


# Required lists ####
files <- paste0("./data/FollowerCounts/EU/",
                list.files("./data/FollowerCounts/EU/")) # Files with info on individual profiles
accounts <- read_rds("./analysis_data/EU_account_list.RDS") # EU account info
snapshots <- read_rds("./analysis_data/EU-snapshots.RDS") # Info on avilable archive.org snapshots
accounts <- left_join(accounts, snapshots, by = "screen_name")
rm(snapshots)


# Interpolation ####
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
  
  # ... and appedn to target data frame
  followercounts <- rbind(followercounts, series)
  
}

# Clean up
rm(list = c("snaps", "series", "series.z", "interpolation"))

# Cross-checks
length(unique(followercounts$screen_name)) == length(files)



# Export ####
write_rds(followercounts, "./analysis_data/FollowerCountsInterpolated_EU.RDS")




