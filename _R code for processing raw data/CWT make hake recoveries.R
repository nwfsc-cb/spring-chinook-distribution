library(tidyverse)
library(here)
#this script joins releases and recoveries
#Creates a file that has all release/recovery and locations when applicable

#################################################################################################################################################################
                 # STEP 1: Join Release, Recoveries, and Location information. 
################################################################################################################################################################
#
# recoveries for the tag code of interest are already present in the data.frame
      #recover.marine 

# These are needed for the end of this script to match up with ASHOP and SHORESIDE DATA
shoreside_chinook = read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Recoveries/Shoreside_CWT/Final_Dataset/shoreside_chinook_tagcode_FTID.csv"))
ashop_all_salmon = read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Hake Trawl/ASHOP/final_datasets/ashop_chinook.csv"))
ashop_chinook_CWT = read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Recoveries/ASHOP/snoutbase/A-SHOP_Snoutbase_122118.csv"))
ashop_sample_frac = read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Hake Trawl/ASHOP/final_datasets/ashop_sample_fraction.csv"))

#add names to fishery gear in dat_recovery_state to make it more interprable
#create categories by gear based on Github/RMIS/RMIS Documentation/PSC_V41_Specification 
#new variable created is called 'gear_type'
#take out freshwater recoveries from dat_recovery
dat_recovery= recover.marine %>%
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
  filter(fishery_type == "high_seas") %>%
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
                                      '89'="Other")) %>%
  separate(recovery_date, into = c("rec_year", "rest_of_rec_date"), sep = 4) %>%
  separate(rest_of_rec_date, into = c("rec_month", "rec_day"), sep= 2) %>%
  mutate(rec_year = as.numeric(rec_year))

#################################################################################################################################################################
      #STEP 2: Assign recoveries into regions 
################################################################################################################################################################

# pull in location data from RMIS
locations = read.csv("/Users/ole.shelton/Github/rmis-data/data/locations.txt", stringsAsFactors = FALSE)
locations = locations[,c("location_code","rmis_latitude","rmis_longitude", "description")]
locations = rename(locations, recovery_location_code = location_code,
                   recovery_description = description, latitude=rmis_latitude, longitude = rmis_longitude)
#fix locations file before left joining then may not experience as many duplicates 
locations <- locations %>%
  filter(!is.na(latitude))

locations <- locations %>% 
  distinct(recovery_location_code, .keep_all = TRUE)   #took out duplicate recovery codes, keep.all = keeps the first row of values for each recovery code
#i think this is ok because alot of the duplicate data were just off my a 0.001 degree in the lat and long, not huge differences 
df_recovery <- left_join(dat_recovery, locations)

### Cull recoveries to only include the US west coast
df_recovery <- df_recovery %>% filter(latitude<49)



#df_recovery <- left_join(dat_recovery, all_rec_codes_locations, by = "recovery_location_code")
#na is a list of recovery codes that did not match up to Oles look up table
#229 recovery location codes did not match up
#na<- everything1 %>% filter(is.na(region)) #all the recoveries that do not match up with a recovery location code in look up table // will want to use this later for when add in regions
#recovery_location_code <- unique(na$recovery_location_code)
#recovery_location_code <- as.data.frame(recovery_location_code)
#recovery_location_code$id <- 1
#c<- left_join(locations, recovery_location_code) #list of unique codes that are not in look up table (includes with and without lat long)
#id<- c %>% filter(id == 1) #list of unique codes that are not in look up table (includes with and without lat long)

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

snout = ashop_chinook_CWT %>%
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

#snoutbase is just for highseas fisheries South of AK
hs_dat <- df_recovery %>% 
  filter((fishery_name %in% c("Hake Trawl At Sea (CA OR WA)",
                              "Ocean Trawl By-catch",
                              "Groundfish Trawl (CA OR WA)", 
                              "Rockfish Trawl (CA OR WA)", 
                              "Nearshore Groundfish (CA OR)", 
                              "Hake Trawl Shoreside (OR WA)") & rec_year > 2005)) %>% #get rid of the data that is in hs_dat so then they can be bound later and duplicates wont be counted
  mutate(rec_month = as.numeric(rec_month)) %>% 
  mutate(rec_day = as.numeric(rec_day)) %>% 
  mutate(rec_year= as.numeric(rec_year)) %>%
  unite("id", c("rec_year", "rec_month", "rec_day", "tag_code"), remove = FALSE )

all_dat <- left_join(hs_dat, snout, by =  "id") %>% #combine so that rmis lat and longs can be replaced by snout 
  filter(!is.na(Year)) %>%
  mutate(Lat = as.numeric(as.character(Lat))) %>%
  mutate(Long = as.numeric(as.character(Long))) %>%
  #add an updated region to these based on lat long
  mutate(rec.area.code = as.vector(cut(Lat, breaks=c(Inf,46.63611,45.76666,44.015, 42.6725, 42, 38.958333, -Inf), 
                                labels=c(  
                                  "NCA",
                                  "MEN",
                                  "SOR",
                                  "COR",
                                  "NOR",
                                  "COL",
                                  "WAC"))))%>% 
  dplyr::select(id,rec_year, rec_month, rec_day, tag_code, recovery_id,rec.area.code,
        fishery, fishery_type, fishery_name, gear, latitude, longitude, recovery_location_name,
        Lat, Long, estimated_number, estimation_level, detection_method, recovery_description)

df_recovery$Haul <- NA
df_recovery$Lat  <- NA
df_recovery$Long <- NA
df_recovery$Ln   <- NA
df_recovery$Wt   <- NA
df_recovery$Sex  <- NA

dat_everything <- df_recovery %>%
  unite("id", c("rec_year", "rec_month", "rec_day", "tag_code"), remove =FALSE ) %>%
  filter(!(fishery_name %in% 
             c("Hake Trawl At Sea (CA OR WA)",
               "Ocean Trawl By-catch",
               "Groundfish Trawl (CA OR WA)",
               "Rockfish Trawl (CA OR WA)",
               "Nearshore Groundfish (CA OR)",
               "Hake Trawl Shoreside (OR WA)") & rec_year > 2005)) %>% #get rid of the data that is in hs_dat so then they can be bound later and duplicates wont be counted
  dplyr::select(id,rec_year, rec_month, rec_day, tag_code, recovery_id,rec.area.code,
                fishery, fishery_type, fishery_name, gear, latitude, longitude, recovery_location_name,
                Lat, Long, estimated_number, estimation_level, detection_method, recovery_description) %>%
  rbind(all_dat, deparse.level = 1, make.row.names = TRUE)  %>%
  mutate(Latitude = case_when(Lat > 0 ~ Lat,       
                              TRUE ~ latitude)) %>%  
  mutate( Longitude = case_when(Lat > 0 ~ Long, 
                                TRUE ~longitude)) %>%
  dplyr::select(-c(longitude, Long, latitude, Lat)) #now this should have all snout and rmis data with combined lat and longs

#######################################################################################
##### Need to re-match each of these recoveries that are still "HSEA" to an area
#######################################################################################

ashop_rmis_recovery<- dat_everything %>%  
  dplyr::filter(fishery == 80) %>% #SNOUTBASE INFO HAS ALREADY BEEN INCORPORATED HERE
  dplyr::mutate(rec.area.code = cut(Latitude, breaks=c(-Inf, 
                                                       37.1833, 
                                                       38.958333,
                                                       40.08333,
                                                       41.99, # this forces effort to SOR which is appropriate (no fishery in NCA pors 1991)
                                                       42.6725,
                                                       44.015,
                                                       45.76666,
                                                       46.64,
                                                       49), labels=c("MONT",
                                                                     "SFB",
                                                                     "MEN",
                                                                     "NCA",
                                                                     "SOR",
                                                                     "COR",
                                                                     "NOR",
                                                                     "COL",
                                                                     "WAC"))) %>%
  dplyr::mutate(tag.code = tag_code) %>%  
  dplyr::mutate(fishery = "ashop") %>%
  dplyr::mutate(rec.year = rec_year) %>%
  dplyr::mutate(rec.month = rec_month) %>%  
  dplyr::mutate(estimation.level = estimation_level) %>%  #supplied by RMIS - but not much information later than 2001, only 30% of info has an estimation level. If the estimation level is NA, then the est.num is 0, even when count is > 0. 
  dplyr::mutate(count =  1) %>% #in RMIS 1 line == 1 fish will summarize later
  #dplyr::select(8, 13, 26:31) %>%
  dplyr::group_by(fishery, rec.year, rec.month, tag.code, rec.area.code, estimation.level, estimated_number) %>%
  dplyr::summarise(count = sum(as.numeric(count)), est.numb.temp= sum(as.numeric(estimated_number))) %>%
  dplyr::mutate(est.numb = case_when(est.numb.temp == 0 ~  as.character(count), #if there is no est. number bc no estimated level, use the count since you know that is the minimum. 
                                     TRUE ~ as.character(est.numb.temp))) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c("est.numb.temp", "estimated_number")) %>%
  dplyr::filter(!is.na(rec.area.code)) %>%
  dplyr::mutate(est.numb = as.numeric(est.numb)) %>%
  dplyr::mutate(rec.month =str_pad(rec.month, 2, side= c("left"), pad = "0")) 

#add sample fractions
ashop_sample_frac1 <- ashop_sample_frac %>%
  mutate(rec.month = as.character(Month)) %>% 
  mutate(rec.month =str_pad(rec.month, 2, side= c("left"), pad = "0")) %>%
  mutate(rec.area.code = region) %>% 
  mutate(rec.year = as.integer(year)) %>%  
  mutate(median.frac.samp = fraction) %>% 
  select(rec.area.code, rec.year, rec.month, median.frac.samp) 

ashop_chinook <- left_join(ashop_rmis_recovery, ashop_sample_frac1, by = c("rec.year", "rec.month", "rec.area.code"))

# only 6% of the recoveries don't have a smaple fraction accounted for - 
na <- ashop_chinook %>% filter(is.na(median.frac.samp))

#searching for weird tag code month and day in rmis - looks like it was never entered  
#test<- rmis %>%
#  filter(tag_code == '0601140605')
#######################################################################################################################################################################################################################################################################################################
#shoreside
############################################################################################################################################################################################################################################################################
#pull out shoreside recoveries from RMIS - it is coded as fishery == 800 and fishery gear type == 800 (there is also Ocean Trawl Bycatch labeled 85 just from ODFW) 
#shoreside_rmis_recovery<- rmis %>% filter(fishery%in%c(800)) #doesnt return many...

# I think for shoreside I dont need to pull out from RMIS (there really arent any... Kayliegh supplied data 2011 on and that is really all we have)
#check to see if hauls had more than 40 chinook
#NOPE, GREATEST CATCH HAD 15 FISH/HAUL
# test <- shoreside_chinook %>%
#  dplyr::group_by(LANDING_YEAR, LANDING_MONTH, LANDING_DAY, VESSEL_ID ) %>%
#  dplyr::count(Tag.Code)

#estimated.numb
#observer coverage is nearly 100% so this will be the same as the count [except for 2014]. 

#estimation.level
#how do observers expand? level of resolution by which estimation was made.There is info in RMIS from 2015, only 60 observations but they all use estimation level: 2 - means sector level. kayleigh confirms this makes sense

# Shoreside_median_sample_frac for 2011-2016. (info on fish recoveries into 2018 but we don't have the effort data)

# Shoreside Fisheries: Winter = January to April, November to December; Summer = May to October

#There's also 2 levels of "expansion" - expansion at the haul level (observers sampling half a haul or a representative sample 
#and then expanding proportionally to the entire haul) and expansion at the fleet level (to deal with the very small amount of catch unsampled due to observer illness or incorrect sampling).

#MEDIAN.SAMPLE.FRAC
#Fleet level 
#2011 - present: 100% of vessels have an observer onboard. THIS IS CONSISTENT THROUGHOUT REGIONS 
#nearly all tows were surveyed (just 99.9% in 2014, all others had 100%)

#Haul level: they whole-haul survey for salmon. so 100% of hauls are surveyed for salmon 


library(tidyverse)
#set up df with all years, months and regions for sample frac
temp.year <- c(2011:2016)
temp.month <- c(01:12)
temp.region <- c("WAC", "PUSO", "COL", "NOR", "COR", "SOR", "NCA", "MEN", "SFB", "MONT")
shoreside_sample_frac <- expand.grid(temp.year, temp.month, temp.region) #fully factorial match between month and year - will filter this out later for combos that only have recoveries when it gets joined in. 
shoreside_sample_frac <- shoreside_sample_frac %>%
  mutate(rec.year = Var1, rec.month=Var2, rec.area.code=Var3) %>%
  dplyr::select(-c(Var1, Var2, Var3)) %>%
  mutate(fleet.level.frac = case_when(rec.year == 2014 ~ 0.999, #2014 was the only year where less than 100% was sampled 
                                      TRUE ~ 1)) %>%
  mutate(haul.level.frac =  1) %>% #
  mutate(median.frac.samp = fleet.level.frac*haul.level.frac) %>% #since everything is nearly 1 except for 2014, median is the same as the actual sample frac... 
  dplyr::select(-c(fleet.level.frac, haul.level.frac))





#shoreside 
# Get rid of duplicates.
shoreside_chinook_reduced <- shoreside_chinook %>% group_by(ID_UNIQUE,DRVID,sector,gear,FTID,LANDING_MONTH,LANDING_DAY,
                                    Port, AGENCY_CODE,Tag.Code,SampleSex,ForkLength..cm.,SampleWeight..lbs.,
                                    AdiposePresent,assign_region,LANDING_YEAR,VESSEL_ID,PORT_NAME) %>%
                            summarise(Lat=mean(AVG_LAT),Long=mean(AVG_LONG))
shoreside_chinook_new_area <- shoreside_chinook_reduced %>% group_by(ID_UNIQUE) %>% summarise(N=length(ID_UNIQUE)) %>% filter(N>=2)

# Figure out which ones still have more than one area assignment.  Assign to the region with the average latitude.
A <- shoreside_chinook_reduced %>% filter(ID_UNIQUE %in% shoreside_chinook_new_area$ID_UNIQUE)
B <- A %>% group_by(ID_UNIQUE,DRVID,sector,gear,FTID,LANDING_MONTH,LANDING_DAY,
                Port, AGENCY_CODE,Tag.Code,SampleSex,ForkLength..cm.,SampleWeight..lbs.,
                AdiposePresent,LANDING_YEAR,VESSEL_ID,PORT_NAME) %>%
            summarise(Lat.fin=mean(Lat),Long.fin=mean(Long)) %>%
            dplyr::mutate(assign_region = cut(Lat.fin, breaks=c(-Inf, 
                                                     37.1833, 
                                                     38.958333,
                                                     40.08333,
                                                     41.99, # this forces effort to SOR which is appropriate (no fishery in NCA pors 1991)
                                                     42.6725,
                                                     44.015,
                                                     45.76666,
                                                     46.64,
                                                     49), labels=c("MONT",
                                                                   "SFB",
                                                                   "MEN",
                                                                   "NCA",
                                                                   "SOR",
                                                                   "COR",
                                                                   "NOR",
                                                                   "COL",
                                                                   "WAC"))) %>%
            rename(Lat=Lat.fin,Long=Long.fin)
            
shoreside_chinook_reduced <- shoreside_chinook_reduced %>% filter(!ID_UNIQUE %in% shoreside_chinook_new_area$ID_UNIQUE) %>% bind_rows(.,B)

shoreside_chinook1 <- shoreside_chinook_reduced %>%  #this is data that kayleigh gave me. 
  mutate(tag.code = as.character(Tag.Code)) %>%  
  mutate(fishery = "shoreside") %>%
  mutate(rec.year = as.integer(LANDING_YEAR)) %>%
  mutate(rec.month = as.integer(LANDING_MONTH)) %>%  
  mutate(rec.area.code = as.character(assign_region)) %>% 
  mutate(estimation.level = "2") %>%  #estimation.level     -->        how do observers expand? there is info in RMIS from 2015, only 60 observations but they all use estimation level: 2
  group_by(fishery,estimation.level, rec.year, rec.month, rec.area.code, tag.code) %>%
  count(tag.code) %>% #fish caught/region/month 
  mutate(count=n, est.numb = count) %>% # they only subsample if it is >40 fish/haul. Our data shows that they didnt have more than 15/haul. So they did not subsample, here the count = est.numb (unless est.numb is also based on unobserved effort-then we should change it) 
  select(-c(n))

shoreside_recoveries <- left_join(shoreside_chinook1, shoreside_sample_frac, by=c("rec.year", "rec.month", "rec.area.code"))

shoreside_recoveries <- shoreside_recoveries %>%
  ungroup() %>%
  mutate(rec.month=as.character(rec.month), estimation.level= as.integer(estimation.level)) 

#BIND BOTH SETS OF RECOVERIES
recoveries <- bind_rows(shoreside_recoveries, ashop_chinook)

#pad the tag codes because leading zero gets dropped in merge. 
recoveries <- recoveries %>% 
  dplyr::mutate(tag.code =str_pad(tag.code, 6, side= c("left"), pad = "0"))

