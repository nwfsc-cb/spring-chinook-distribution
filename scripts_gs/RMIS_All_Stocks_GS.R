library(tidyverse)
library(here)
#this script joins releases and recoveries for ALL STOCKS not just focal stocks. this was created because JW and folks at AFSC were asking for bycatch plots from all stocks. 

#################################################################################################################################################################
# STEP 1: Join Release, Recoveries, and Location information. 
################################################################################################################################################################

#Eric has already created this file and it includes the descriptions - Now just need to get focal to match in. 
rmisdat<- here::here("data","joined_releases_recoveries_locations.rds") %>%
  readRDS( )


#now to sum total releases across rows using coded wire tag columns
rmisdat$total_release=rowSums(rmisdat[,c('cwt_1st_mark_count', 'cwt_2nd_mark_count', 'non_cwt_1st_mark_count', 'non_cwt_2nd_mark_count')], na.rm=TRUE)
#drop unnecessary columns after summing total releases, you only care about total releases of tagged fish
rmisdat=dplyr::select(rmisdat, -cwt_1st_mark_count, -cwt_2nd_mark_count,
                        -non_cwt_1st_mark_count, -non_cwt_2nd_mark_count)
#rmisdat now has all releases and recoveries, sum of CWT as total releases

#take out freshwater recoveries from dat_recovery
dat_recovery= rmisdat %>% 
  separate(recovery_location_code, into = c("state_code", "rest_of_rec_code"), sep = 1, remove = FALSE) %>%
  separate(rest_of_rec_code, into = c("marine_fw", "rest_of_rec_code"), sep = 1) %>%
  filter(marine_fw == 'M') %>% 
  separate(recovery_date, into = c("rec_year", "rest_of_rec_date"), sep = 4) %>%
  separate(rest_of_rec_date, into = c("rec_month", "rec_day"), sep= 2) %>%
  mutate(rec_year=as.numeric(rec_year))

#################################################################################################################################################################
#STEP 2: Assign recoveries into regions 
################################################################################################################################################################

rec_codes_lookup = read_csv("data/recovery codes-wietkamp+shelton 12-2018 two PUSO.csv") %>%
  rename(region=Rec.area.Sullaway.Shelton.NMFSstat) %>%
  select(recovery_location_code, region)  

# pull in location data from RMIS
locations = read_csv("data/locations.txt")
locations = locations[,c("location_code","rmis_latitude","rmis_longitude", "description")]
locations = rename(locations, recovery_location_code = location_code,
                   recovery_description = description, latitude=rmis_latitude, longitude = rmis_longitude)
#fix locations file before left joining then may not experience as many duplicates 
locations <- locations %>%
  filter(!is.na(latitude)) %>% 
  distinct(recovery_location_code, .keep_all = TRUE)   #took out duplicate recovery codes, keep.all = keeps the first row of values for each recovery code
#i think this is ok because alot of the duplicate data were just off my a 0.001 degree in the lat and long, not huge differences 
all_rec_codes_locations <- left_join(rec_codes_lookup ,locations)

df_recovery <- left_join(dat_recovery, all_rec_codes_locations, by = "recovery_location_code")

#################################################################################################################################################################
#STEP 4: SNOUTBASE - DATA PARCE AND JOIN INTO RMIS 
################################################################################################################################################################
#SNOUTBASE- Use to match lat and long into to RMIS Highseas to get better recovery information so the recoveries dont snap to a grid

setwd("~/Documents/GitHub/Chinook_Bycatch/data/ASHOP/snoutbase")
snout = read.csv("A-SHOP_Snoutbase_122118.csv", stringsAsFactors = FALSE)
snout$tag_code<- str_pad(snout$CWTCode, width= 6, pad = "0", side="left") #add leading zero that r removes

snout <- snout %>% 
  filter(!CWTCode == "-" & !CWTCode == " ") %>% #remove blanks in snout data set
  separate(Date, c("rec_month", "rec_day", "delete"), sep= '/') %>%
  mutate(rec_month = as.numeric(rec_month)) %>% 
  mutate(rec_day = as.numeric(rec_day)) %>% 
  mutate(rec_year= as.numeric(Year)) %>%
  mutate(Long= as.numeric(Long)) %>%
  mutate(Long = ifelse(rec_year == 2009, Long * -1, Long)) %>%#Fixes this --> ALL RECOVERIES FROM 2009 ARE PROBABLY MISSING A (-) IN LONGITUDE
  unite("id", c("rec_year", "rec_month", "rec_day", "tag_code") )

#is there fish in snoutbase not in rmis? #ANSWER = NO ALL SNOUTBASE FISH ARE IN RMIS
#get unique list of tag codes for each
#          rmis_u <- as.data.frame(unique(dat_short$tag_code))
#          snout_u <-as.data.frame(unique(snout$tag_code))

#          colnames(snout_u)[which(names(snout_u) == "unique(snout$tag_code)")] <- "name"
#          colnames(rmis_u)[which(names(rmis_u) == "unique(dat_short$tag_code)")] <- "name"

#         b <- left_join(snout_u, rmis_u)


#this script matches lat and long into to RMIS Highseas to get better recovery information so they recoveries dont snap to a grid

#use dat from rmis_base file. 
#dat_region = read.csv("dat_region.csv", stringsAsFactors = FALSE)

#keep just highseas fisheries
hs_dat <- df_recovery %>% 
  filter(fishery_type == "high_seas") %>%
  filter(rec_year > 2005) %>% #snoutbase starts in 2005
  mutate(rec_month = as.numeric(rec_month)) %>% 
  mutate(rec_day = as.numeric(rec_day)) %>% 
  mutate(rec_year= as.numeric(rec_year)) %>%
  unite("id", c("rec_year", "rec_month", "rec_day", "tag_code"), remove = FALSE )

all_dat <- left_join(hs_dat, snout, by =  "id") #combine so that rmis lat and longs can be replaced by snout 

all_dat1 <- all_dat %>%
  filter(!is.na(Year)) %>%
  mutate(Lat = as.numeric(Lat)) %>%
  mutate(Long = as.numeric(Long))
#add spatial bounds to these based on lat long
all_dat1$region<-  as.vector(cut(all_dat1$Lat, breaks=c(Inf,46.63611,45.76666,44.015, 42.6725, 42, 38.958333, -Inf), 
                                 labels=c(  
                                   "NCA",
                                   "MEN",
                                   "SOR",
                                   "COR",
                                   "NOR",
                                   "COL",
                                   "WAC")))

all_dat2 <- all_dat1 %>% 
  select(id,rec_year, rec_month, rec_day, tag_code, recovery_id, fishery, fishery_type, gear, region, latitude, longitude, Lat, Long, region, recovery_location_code, recovery_location_name, estimated_number, estimation_level, brood_year, first_release_date, release_location_code, release_location_name, release_location_state, release_location_rmis_basin,
         release_loc_domain, stock_location_code, total_release, detection_method )

df_recovery$Haul <- NA
df_recovery$Lat <- NA
df_recovery$Long <- NA
df_recovery$Ln <- NA
df_recovery$Wt <- NA
df_recovery$Sex <- NA

dat_region_filter <- df_recovery %>%
  unite("id", c("rec_year", "rec_month", "rec_day", "tag_code"), remove =FALSE ) %>%
  filter(!fishery_type == "high_seas" | !rec_year > 2005) %>% #get rid of the data that is in hs_dat so then they can be bound later and duplicates wont be counted
  select(id, rec_year, rec_month, rec_day, tag_code, recovery_id, fishery, fishery_type, gear, region, latitude, longitude, Lat, Long, region, recovery_location_code, recovery_location_name, estimated_number, estimation_level,  brood_year, first_release_date, release_location_code, release_location_name, release_location_state, release_location_rmis_basin,
         release_loc_domain, stock_location_code, total_release,detection_method)

everything <- rbind(dat_region_filter, all_dat2, deparse.level = 1, make.row.names = TRUE)  

# conditionally replace latitude and longitude with NA if there is a value in the snoutbase lat and long
everything1 <- everything %>%
  mutate(Latitude = case_when(Lat > 0 ~ Lat,       
                              TRUE ~ latitude)) %>%  
  mutate( Longitude = case_when(Lat > 0 ~ Long, 
                                TRUE ~longitude)) %>%
  dplyr::select(-c(longitude, Long, latitude, Lat)) #now this should have all snout and rmis data with combined lat and longs

dat_everything <- everything1 

#################################################################################################################################################################
# SAVE TIDY DATA FILES 
################################################################################################################################################################

saveRDS(df_recovery, "data/rmis_allstocks_June2020.RDS" )
 
 