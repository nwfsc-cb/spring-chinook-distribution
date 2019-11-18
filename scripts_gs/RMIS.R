library(tidyverse)
library(here)
#this script joins releases and recoveries
#Creates a file that has all release/recovery info assigned into regions 
here()
#################################################################################################################################################################
                                                                  # STEP 1: Join Release, Recoveries, and Location information. 
################################################################################################################################################################
#   recover = read.csv("data/chinook/recoveries_1973.csv", stringsAsFactors = FALSE)
#   recover = dplyr::select(recover, species,
#     tag_code, recovery_id, 
#     recovery_date, fishery, gear, sex, length, length_type, length_code,
#     recovery_location_code, recovery_location_name, 
#     estimation_level, estimated_number, detection_method, recovery_description)
#   for(y in 1974:2017) {
#     #  names change slightly in 2015,
#     temp = read.csv(paste0("data/chinook/recoveries_",y,".csv"), 
#                     stringsAsFactors = FALSE)
#     temp = dplyr::select(temp, species, tag_code, recovery_id, recovery_date, fishery, gear, 
#       sex, length, length_type, length_code,
#       recovery_location_code, recovery_location_name, estimation_level, 
#       estimated_number, detection_method, recovery_description)
#     recover = rbind(recover, temp)
#   }
#   
#   recover = dplyr::filter(recover, !is.na(estimated_number)) %>% 
#     filter(tag_code != "")
#   
#   #load release data
#   release = read.csv("data/chinook/all_releases.txt", header=T, stringsAsFactors = FALSE) 
#   release = dplyr::select(release, tag_code_or_release_id, run, brood_year, first_release_date,
#                           release_location_code, stock_location_code, cwt_1st_mark_count, cwt_2nd_mark_count,
#                           non_cwt_1st_mark_count, non_cwt_2nd_mark_count, release_location_name,
#                           stock_location_name, release_location_state, release_location_rmis_region, 
#                           release_location_rmis_basin) %>% 
#     dplyr::rename(tag_code = tag_code_or_release_id)
# 
# #left_join combines the two data frames into dat
# dat = left_join(recover, release) 
# ##dat should have all releases and recoveries


               #START HERE

#Eric has already created this file and it includes the descriptions - Now just need to get focal to match in. 
rmisdat <- readRDS("data/joined_releases_recoveries_locations.rds") 

rmisdat <- readRDS(rmisdat)

##then filter out stocks that we are not interested in...
#load focal species data that Ole created, this file just has the stocks we are interested in
focal = read.csv("data/focal_spring_chinook_releases_summary.csv", header=T, stringsAsFactors = FALSE)%>%
  select(stock_location_code)
#combine dat and focal by stock_code_location, filters out stocks we are not interested in
dat_focal=semi_join(rmisdat, focal)

#now to sum total releases across rows using coded wire tag columns
dat_focal$total_release=rowSums(dat_focal[,c('cwt_1st_mark_count', 'cwt_2nd_mark_count', 'non_cwt_1st_mark_count', 'non_cwt_2nd_mark_count')], na.rm=TRUE)
#drop unnecessary columns after summing total releases, you only care about total releases of tagged fish
dat_focal=dplyr::select(dat_focal, -cwt_1st_mark_count, -cwt_2nd_mark_count,
                        -non_cwt_1st_mark_count, -non_cwt_2nd_mark_count)
#dat_focal now has all releases and recoveries, sum of CWT as total releases

############################################################################################################################################################
############################################################################################################################################################

#take out freshwater recoveries from dat_recovery
dat_recovery= dat_focal %>% 
  separate(recovery_location_code, into = c("state_code", "rest_of_rec_code"), sep = 1, remove = FALSE)%>%
  separate(rest_of_rec_code, into = c("marine_fw", "rest_of_rec_code"), sep = 1) %>%
  filter(marine_fw == 'M') %>%
#add names to fishery gear in dat_recovery_state to make it more interprable
#create categories by gear based on Github/RMIS/RMIS Documentation/PSC_V41_Specification 
#new variable created is called 'gear_type'
 mutate(fishery_type = as.vector(cut(fishery, breaks=c(-Inf,19,29,39,49,59,69,79,89,99,Inf), 
                                            labels=c("troll",
                                                     "net_seine",
                                                     "aboriginal",
                                                     "sport",
                                                     "escapement",
                                                     "test_fishery",
                                                     "juv_sampling",
                                                     "high_seas",
                                                     "misc",
                                                     "high_seas")))) %>%
#rename state code #s to state names (ie 1-> AK) used info from locaiton.txt
# mutate(recovery_state = recode_factor(state_code, '1' = "AK",
#                                              '2' = "BC",
#                                              '3' = "WA",
#                                              '4'="ID",
#                                              '5'="OR",
#                                              '6'="CA",
#                                              '7'="HS")) %>%
  
                mutate(fishery_name = recode_factor(fishery, '80' = "Hake Trawl At Sea (CA OR WA)",
                                           '800' = "Hake Trawl Shoreside (OR WA)",
                                           '802' = "Rockfish Trawl (CA OR WA)",
                                           '803'="Groundfish Trawl (CA OR WA)",
                                           '804'="Sablefish Fixed Gear (CA OR WA)",
                                           '805'="Nearshore Groundfish (CA OR)",
                                           '81'="Groundfish Observer (Gulf AK)",
                                           '812'="Rockfish Fishery (Gulf of AK)",
                                           '82'="Groundfish Observer (Bering Sea)",
                                           '83'="Foreign Research Vessels",
                                           '84'="Foreign Mothership",
                                           '85'="Ocean Trawl By-catch",
                                           '87'="Squid Gillnet By-catch", 
                                           '88'="Juv Sampling",
                                           '89'="Other",
                                           '40' = "Ocean Sport",
                                           '41' = "Charter Sport",
                                           '42' = "Private Sport",
                                           '43'= "Jetty Sport",
                                           '44'="Columbia River Sport",
                                           '45'="Estuary Sport",
                                           '48'="Terminal Sport",
                                           '20' = "Ocean Gillnet",
                                           '21' = "Columbia Gillnet",
                                           '22' = "Coastal Gillnet",
                                           '23'= "Mixed Net and Seine",
                                           '24'="FW Net",
                                           '25'="Commercial Seine",
                                           '26'="Terminal Seine",
                                           '27'="FW Seine",
                                           '28'="Other Net",
                                           '29'="Other Seine",
                                           '10' = "Ocean Troll (non-treaty)",
                                           '11' = "Ocean Troll- Day Boat",
                                           '12' = "Ocean Troll- Trip",
                                           '13'= "Ocean Troll- Freezer Boat",
                                           '14'="Ocean Troll- Ice Boat",
                                           '15'="Treaty Troll",
                                           '16'="Terminal Troll",
                                           '17'="Non-treaty/treaty Troll",
                                           '18'="Aboriginal Troll",
                                           '19'="Other Troll")) %>%
  separate(recovery_date, into = c("rec_year", "rest_of_rec_date"), sep = 4) %>%
  separate(rest_of_rec_date, into = c("rec_month", "rec_day"), sep= 2) %>%
  mutate( rec_year = as.numeric(rec_year))

#################################################################################################################################################################
                                                    #STEP 2: Assign recoveries into regions 
################################################################################################################################################################
#codes updated to use the NMFS stat areas in AK 
rec_codes_lookup = read.csv("data/nmfs_areas_recovery codes-wietkamp+shelton 12-2018 two PUSO.csv", stringsAsFactors = FALSE) %>%
  mutate(recovery_location_code =location_code) %>%
  mutate(region = Rec.area.GS.nmfs) %>% 
  select(c(recovery_location_code, region)) #change so that RMIS has same column labels 

#pretty sure we dont need the location file because all the codes on this with lat long are already supplied in the RMIS data base, codes have been matched with a recovery code in the recovery codes csv
# locations = read.csv("data/locations.txt", stringsAsFactors = FALSE) %>%
# select(c("location_code","rmis_latitude","rmis_longitude", "description")) %>%
# rename(recovery_location_code = location_code,
#                    recovery_description = description, latitude=rmis_latitude, longitude = rmis_longitude) %>%
# filter(!is.na(latitude)) %>% 
# distinct(recovery_location_code, .keep_all = TRUE)   #took out duplicate recovery codes, keep.all = keeps the first row of values for each recovery code
# all_rec_codes_locations <- left_join(rec_codes_lookup, locations)

df_recovery <- left_join(dat_recovery, rec_codes_lookup, by = "recovery_location_code")

#600 individuals still come up with NA's...

#na is a list of recovery codes that did not match up to Oles look up table - went and added it back in to the lookup data base
na<- df_recovery %>% filter(is.na(region)) #all the recoveries that do not match up with a recovery location code in look up table // will want to use this later for when add in regions

recovery_location_code <- as.data.frame(unique(na$recovery_location_code)) %>%
  rename(recovery_location_code = 'unique(na$recovery_location_code)') %>%
  mutate(id = 1)

# c<- left_join(locations, recovery_location_code) #list of unique codes that are not in look up table (includes with and without lat long)
# id<- c %>% filter(id == 1) #list of unique codes that are not in look up table (includes with and without lat long)
# write.csv(id, "missing_codes.csv")

#z <- id %>% filter(is.na(latitude)) # list of recovery codes that are not in lookup tanble AND that dont have lat and long - need to look up in RMIS data base?
#lat_long <- id %>% filter(!is.na(latitude)) #list of recovery codes that are not in table but DO have lat long- will use for convex hull stuff
#lat_long <- lat_long %>%
#  dplyr::select(-c(id))
#x <- left_join(dat_recovery, z) 
#v <- x %>% filter(!is.na(id)) #list of # of recoveries that dont have lat long in recovery codes

#################################################################################################################################################################
#STEP 4: SNOUTBASE - DATA PARCE AND JOIN INTO RMIS 
################################################################################################################################################################
#SNOUTBASE- Use to match lat and long into to RMIS Highseas to get better recovery information so the recoveries dont snap to a grid

setwd("~/Documents/GitHub/Chinook_Bycatch/data/ASHOP/snoutbase")
snout = read.csv("A-SHOP_Snoutbase_122118.csv", stringsAsFactors = FALSE) %>%
        mutate(tag_code = str_pad(CWTCode, width= 6, pad = "0", side="left")) %>% #add leading zero that r removes
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


#now match lat and long into to RMIS Highseas to get better recovery information so they recoveries dont snap to a grid

#snoutbase is just for highseas fisheries 
hs_dat <- df_recovery %>% 
  filter(fishery_type == "high_seas") %>%
  filter(rec_year > 2005) %>% #snoutbase starts in 2005
  mutate(rec_month = as.numeric(rec_month)) %>% 
  mutate(rec_day = as.numeric(rec_day)) %>% 
  mutate(rec_year= as.numeric(rec_year)) %>%
  unite("id", c("rec_year", "rec_month", "rec_day", "tag_code"), remove = FALSE )

all_dat <- left_join(hs_dat, snout, by =  "id") %>% #combine so that rmis lat and longs can be replaced by snout 
filter(!is.na(Year)) %>%
mutate(Lat = as.numeric(Lat)) %>%
mutate(Long = as.numeric(Long)) %>%
#add spatial bounds to these based on lat long
mutate(region = as.vector(cut(Lat, breaks=c(Inf,46.63611,45.76666,44.015, 42.6725, 42, 38.958333, -Inf), 
                                 labels=c(  
                                   "NCA",
                                   "MEN",
                                   "SOR",
                                   "COR",
                                   "NOR",
                                   "COL",
                                   "WAC"))))%>% 
  select(id,rec_year, rec_month, rec_day, tag_code, recovery_id, fishery, fishery_type, gear, region, latitude, longitude, Lat, Long, region, recovery_location_code, recovery_location_name, estimated_number, estimation_level, brood_year, first_release_date, release_location_code, release_location_name, release_location_state, release_location_rmis_basin,
         stock_location_code, total_release, detection_method )
 
df_recovery$Haul <- NA
df_recovery$Lat <- NA
df_recovery$Long <- NA
df_recovery$Ln <- NA
df_recovery$Wt <- NA
df_recovery$Sex <- NA

dat_everything <- df_recovery %>%
  unite("id", c("rec_year", "rec_month", "rec_day", "tag_code"), remove =FALSE ) %>%
  filter(!fishery_type == "high_seas" | !rec_year > 2005) %>% #get rid of the data that is in hs_dat so then they can be bound later and duplicates wont be counted
  select(id, rec_year, rec_month, rec_day, tag_code, recovery_id, fishery, fishery_type, gear, region, latitude, longitude, Lat, Long, region, recovery_location_code, recovery_location_name, estimated_number, estimation_level,  brood_year, first_release_date, release_location_code, release_location_name, release_location_state, release_location_rmis_basin,
          stock_location_code, total_release,detection_method)%>%
  rbind(all_dat, deparse.level = 1, make.row.names = TRUE)  %>%
  mutate(Latitude = case_when(Lat > 0 ~ Lat,       
                             TRUE ~ latitude)) %>%  
  mutate( Longitude = case_when(Lat > 0 ~ Long, 
                               TRUE ~longitude)) %>%
 select(-c(longitude, Long, latitude, Lat)) #now this should have all snout and rmis data with combined lat and longs

View(dat_everything)

#test <- dat_everything %>% filter(is.na(region)) #still labout 600 NA but none of them are high seas 
#################################################################################################################################################################
                                                                  # SAVE TIDY DATA FILES 
################################################################################################################################################################
#
#write.csv(dat_everything, "RMIS.csv" )
#getwd()

 #CHECK TO SEE IF REGION ASSIGNMENTS PLOT CORRECTLY
# dat <- dat_everything %>%
#   filter(!Latitude == 0) 
# world <- map_data("world")
# north_america <- subset(world, region %in% c("USA", "Canada"))
# 
# north_america %>%
#   filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
#   filter(!long > -115) %>%
#   filter(!lat < 46) %>%
#   filter(!lat > 62) %>%
#   filter(!long < -173) %>%
#   ggplot( ) +
#   geom_polygon(data = dat, aes(x = Longitude, y = Latitude, group = region), fill = "white", color = "black") +
#   coord_fixed(1.3) +
#   geom_segment(data = df, colour="orange", aes(x = as.numeric(line.start.lon), 
#                                                y = as.numeric(line.start.lat), 
#                                                xend = as.numeric(line.end.lon), 
#                                                yend = as.numeric(line.end.lat))) +
#   facet_wrap(~fishery) +
#   #  geom_text(data=df, colour="darkgreen",aes(x=label_long1, y= label_lat1, label= region), size=2)+ #smaller labels for CA, OR, and WA so they fit
#   theme_classic()

  