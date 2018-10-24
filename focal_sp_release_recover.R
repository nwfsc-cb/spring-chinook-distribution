library(dplyr)
library(ggplot2)


#load recoveries
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

#load release data
release = read.csv("data/chinook/all_releases.txt", header=T, stringsAsFactors = FALSE) 
release = dplyr::select(release, tag_code_or_release_id, run, brood_year, first_release_date,
                        release_location_code, stock_location_code, cwt_1st_mark_count, cwt_2nd_mark_count,
                        non_cwt_1st_mark_count, non_cwt_2nd_mark_count, release_location_name,
                        stock_location_name, release_location_state, release_location_rmis_region, 
                        release_location_rmis_basin) %>% 
  dplyr::rename(tag_code = tag_code_or_release_id)

#left_join combines the two data frames into dat
dat = left_join(recover, release) 
##dat should have all releases and recoveries 1973-2016

##then filter out stocks that we are not interested in...
#load focal species data that Ole created, this file just has the stocks we are interested in
focal = read.csv("data/focal_spring_chinook_releases_summary.csv", header=T, stringsAsFactors = FALSE) 
#focal now just has one column, stock_location code
focal=dplyr::select(focal, stock_location_code)
#combine dat and focal by stock_code_location, filters out stocks we are not interested in
dat_focal=semi_join(dat, focal)

#now to sum total releases across rows using coded wire tag columns
dat_focal$total_release=rowSums(dat_focal[,c('cwt_1st_mark_count', 'cwt_2nd_mark_count', 'non_cwt_1st_mark_count', 'non_cwt_2nd_mark_count')], na.rm=TRUE)
#drop unnecessary columns after summing total releases, you only care about total releases of tagged fish
dat_focal=dplyr::select(dat_focal, -cwt_1st_mark_count, -cwt_2nd_mark_count,
                -non_cwt_1st_mark_count, -non_cwt_2nd_mark_count)
#dat_focal now has all releases and recoveries, sum of CWT as total releases, *should be* ready for plotting [will need to add in location info at some point!]

##save your data as a csv
    write.csv(dat_focal,'dat_focal.csv')

###having issues with adding in locations to dat focal....the left join on this so skipping it for now...observations nearly double because something isnt matching up
#use 'by' command in the left join ie: left_join(dat, locations, by="recovery_location_code") but still get obs doubling. IDK why.
#    next add in location recovery from RMIS
#    locations = read.csv("data/locations.txt", stringsAsFactors = FALSE)
#    locations = locations[,c("location_code","rmis_latitude","rmis_longitude", "description")]
#    locations = rename(locations, recovery_location_code = location_code,
#                      recovery_description = description, latitude=rmis_latitude, longitude = rmis_longitude)
#    dat_focal = left_join(dat_focal, locations)
    ##after doing above... ^ dat_focal should have all releases and recoveries 1973-2016 and the associated locations from RMIS
    