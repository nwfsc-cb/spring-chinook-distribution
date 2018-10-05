library(dplyr)
library(ggplot2)

# Load release data
release = read.csv("data/chinook/all_releases.txt", header=T, stringsAsFactors = FALSE) 
release = dplyr::select(release, tag_code_or_release_id, run, brood_year, first_release_date,
  release_location_code, stock_location_code, cwt_1st_mark_count, cwt_2nd_mark_count,
  non_cwt_1st_mark_count, non_cwt_2nd_mark_count, release_location_name,
  stock_location_name, release_location_state, release_location_rmis_region, 
  release_location_rmis_basin) %>% 
  dplyr::rename(tag_code = tag_code_or_release_id)

# Sum up the total releases, drop unneccessary columns
release$total_release = sum(c(as.numeric(release$cwt_1st_mark_count), as.numeric(release$cwt_2nd_mark_count), 
                              as.numeric(release$non_cwt_1st_mark_count), as.numeric(release$non_cwt_2nd_mark_count)))
release = dplyr::select(release, -cwt_1st_mark_count, -cwt_2nd_mark_count,
  -non_cwt_1st_mark_count, -non_cwt_2nd_mark_count)

# pull out relase year
release$release_year = substr(release$first_release_date, 1, 4)
release = dplyr::select(release, -first_release_date)

# recoveries
recover = read.csv("data/chinook/recoveries_1973.csv", stringsAsFactors = FALSE)
recover = dplyr::select(recover, tag_code, recovery_id, recovery_date, fishery, gear, 
  recovery_location_code, recovery_location_name, estimated_number)
for(y in 1974:2016) {
  #  names change slightly in 2015,
  temp = read.csv(paste0("data/chinook/recoveries_",y,".csv"), 
    stringsAsFactors = FALSE)
  temp = dplyr::select(temp, tag_code, recovery_id, recovery_date, fishery, gear, 
    recovery_location_code, recovery_location_name, estimated_number)
  recover = rbind(recover, temp)
}

recover = dplyr::filter(recover, !is.na(estimated_number)) %>% 
  filter(tag_code != "")

# Join by tag code
release = dplyr::rename(release, tag_code=tag_code_or_release_id)
dat = left_join(recover, release) # join by tag code

# pull in location data from RMIS
locations = read.csv("data/locations.txt", stringsAsFactors = FALSE)
locations = locations[,c("location_code","rmis_latitude","rmis_longitude", "description")]
locations = rename(locations, recovery_location_code = location_code,
  recovery_description = description, latitude=rmis_latitude, longitude = rmis_longitude)
dat = left_join(dat, locations)

# At this point, 'dat' is the complete release-recovery dataset for all years, coastwide

# Example of filtering
# location codes: 2(BC), 3 (WA), 5 (OR), 6 (CA), etc.
dat = mutate(dat, recovery_state = substr(recovery_location_code, 1,2)) %>%
  mutate(recovery_year = substr(recovery_date, 1,4),
    recovery_month = substr(recovery_date, 5,6))