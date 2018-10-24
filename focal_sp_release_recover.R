library(dplyr)
library(ggplot2)

##Load recovery data
  #combine all recoveries from 1974-2016 into one data sheet, having an issue because the names change in 2015 so 2015 and 2016 wont load
recover = read.csv("data/chinook/recoveries_1973.csv", stringsAsFactors = FALSE)
for(y in 1974:2016) {
  #names change slightly in 2015,
  temp = read.csv(paste0("data/chinook/recoveries_",y,".csv"), 
                  stringsAsFactors = FALSE)
    recover = rbind(recover, temp)
}

df <- filter(recover, run_year == '1992')


df <- substr(recover,regexpr("[^0]",tag_code),char(tag_code),stopifnot(apply(ncT,length(unique(.)) == 6)))


df$tag_code1 <- str_remove(df$tag_code, "^0+", for y in length(unique(tag_code == '5')))

fun1 <- function(x) substr(x, 1 + (1 * as.numeric(substr(x,1,1)=='0')), nchar(x) - (1 * as.numeric(substr(x, nchar(x), nchar(x)) == '.')))



recover = dplyr::filter(recover, !is.na(estimated_number)) %>% 
  filter(tag_code != "")

#to combine 2015 and 2016 with recover (goes until 2014) need to delete first column from recover data set 
recover= select(recover, -c("X"))
#now load 2015 2016 data sets so you can combine recover with 2015 and 2016 data sets
year_2015 = temp
year_2016 = read.csv("data/chinook/recoveries_2016.csv", stringsAsFactors = FALSE, colClasses=c("tag_code"="character"))
#combine all 3 then you will have 1974 to 2016 in one set
all_recovery= rbind(recover, year_2015, year_2016)
#now all_recovery = recoveries from 1974 to 2016

## now need to combine recoveries and releases
#load release data
release = read.csv("data/chinook/all_releases.txt", header=T, stringsAsFactors = FALSE) 
release = dplyr::select(release, tag_code_or_release_id, run, brood_year, first_release_date,
                        release_location_code, stock_location_code, cwt_1st_mark_count, cwt_2nd_mark_count,
                        non_cwt_1st_mark_count, non_cwt_2nd_mark_count, release_location_name,
                        stock_location_name, release_location_state, release_location_rmis_region, 
                        release_location_rmis_basin) %>% 
  dplyr::rename(tag_code = tag_code_or_release_id)

#join releases and recoveries together by tag code
release = dplyr::rename(release, tag_code=tag_code_or_release_id) #figure out if this is necessary what is release id? how is it different than tag code and is tag_code_release_id a thing? if so what year is it used in? look at rmis documentation info to figure that out
#left_join combines the two data frames into dat
dat = left_join(all_recovery, release) 
##dat should have all releases and recoveries 1973-2016


##then filter out stocks that we are not interested in...
#load focal species data that Ole created, this file just has the stocks we are interested in
focal = read.csv("data/focal_spring_chinook_releases_summary.csv", header=T, stringsAsFactors = FALSE) 
#focal now just has one column, stock_location code
focal=select(focal, stock_location_code)
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
    