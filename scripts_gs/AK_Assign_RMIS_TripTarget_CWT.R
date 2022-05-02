library(here)
library(tidyverse)
library(zoo)
#this script uses data from afsc (akfin data) to try to add trip targets (pollock or non-pollock) to RMIS trawl recoveries. 
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}
#if you need data for the script and it is not on your computer you need to download it from the data sharing SFTP site.  
#######################################################################################################
# LOAD OBSERVER DATA FROM AFSC  
#######################################################################################################
# Run this scipts to create pollock_fishing_dates.RDS if you need it: #source("scripts_gs/AK_tidy_pollock_fishing_dates.R")
dates<-readRDS("data/AK_CWT_Trawl/pollock_fishing_dates/pollock_fishing_dates.RDS") #%>%
#already did this in tidy pollock file  filter(ocean_region == "GOA") 

observer.load <- readRDS("data/AK_CWT_Trawl/Observer_data_base/comp_obs_haul_v3.rds") 

#observer.first.ten <- readRDS("data/AK_CWT_Trawl/Observer_data_base/comp_obs_haul_v2_toptenrows.rds") 
#results are the same if I match on haul date or retrieval date. 
observer<- observer.load %>% #this has cruise number which, in combo with other columns can be used to match. Cruise # is specific to an observer and their deployment so it can not be the only thing used to match. 
  rename(trip_target_code=TARGET_FISHERY_CODE,week_end_date=WEEK_END_DATE,
         cruise_no=CRUISE, Haul_or_Delivery_no=HAUL_NUMBER, processing_sector=PSCNQ_PROCESSING_SECTOR,
         vessel_name=VES_AKR_NAME, ves_akr_adfg=CATCHER_BOAT_ADFG ) %>%
#~2% of data does not have a retrieval date, I think this is worth it because that is a small fraction and also more accurate than week end date 
  separate(RETRIEVAL_DATE, into = c("year", "month", "day"), sep="-") %>%
  separate(day, into = c("day", "delete"), sep=" ") %>%
  mutate(year=as.numeric(year), 
          month = as.numeric(month), 
          day = as.numeric(day)) %>%
  filter(case_when(year > 2012 ~ !trip_target_code == "K",
                   TRUE ~ trip_target_code == trip_target_code),
         processing_sector == "S" ) %>% # RMIS starts coding rockfish specific recoveries in 2013, remove them from this data set so we dont get duplicates. 
  mutate(trip_target_code= case_when(trip_target_code %in% c("P","B") ~ "pollock",
                                     TRUE ~ "other")) %>%
  group_by(week_end_date, year,month, day, cruise_no, AKR_VESSEL_ID,OBS_VESSEL_ID, ves_akr_adfg, processing_sector, REPORTING_AREA_CODE,vessel_name) %>% 
  summarise(trip_target_code = paste(trip_target_code, collapse=",")) %>% # one line/all fisheries associated with that permit number 
  # simplify so it is either pollock or other - will help remove duplicates for matching 
  mutate(trip_target_code =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "pollock, other",
                                       grepl("pollock", trip_target_code) ~ "pollock",
                                       grepl("other", trip_target_code) ~ "other",
                                       TRUE ~ "FIX")) %>%
  data.frame() 

unique_vessel_names <- unique(observer[c("OBS_VESSEL_ID", "AKR_VESSEL_ID","vessel_name", "ves_akr_adfg")])  

unique_vessel_names_ID=unique_vessel_names %>% 
  mutate(REAL_vessel_name = vessel_name , vessel_name = OBS_VESSEL_ID) %>%
  select(vessel_name,REAL_vessel_name)

#######################################################################################################
# LOAD AND CORRECT VESSEL NAMES IN CWT DATA 
#######################################################################################################
#setwd("~/Documents/GitHub/spring-chinook-distribution")
rec_codes_lookup = read.csv("data/recovery codes-wietkamp+shelton 12-2018 two PUSO.csv", stringsAsFactors = FALSE) %>%
  mutate(recovery_location_code =location_code) %>%
  mutate(region = Rec.area.Sullaway.Shelton.NMFSstat) %>% 
  select(c(recovery_location_code, region)) %>% #change so that RMIS has same column labels 
  distinct(recovery_location_code, .keep_all = TRUE) #remove duplicates 
 
#ASK OLE TO UPDATE THE GIT SO I CAN PULL FINAL RMIS DATA! -- Then I wont need to match with the rec code lookup because it will already be in there. 
#load RMIS data  
load("/Users/genoa.sullaway/Documents/GitHub/rmis-data/data/final_data/all_chinook.Rdata")
all_chinook<-data.frame(all_chinook) %>%
  separate(recovery_date,c("year","rest"), sep = 4)%>%
  separate(rest, c("month","day"), sep = 2)%>%
  filter(year >1995) %>%
  filter(fishery %in% c(81, 82))
#
match <- all_chinook %>%
  left_join(rec_codes_lookup, by = "recovery_location_code") %>%
  mutate(region = case_when(recovery_location_name == "HIGH SEAS 3 55N 154W" ~ "E.APEN",
                            recovery_location_name == "HIGH SEAS 3 51N 172W" ~ "ALEUT",
                            recovery_location_name %in% c("HIGH SEAS 4 56N 162W", "HIGH SEAS 4 57N 162W") ~ "BER",
                                                          TRUE ~ region))

  
#bring in the CWT recovery data that Michele Masuda sent, change vessel names so they match with norpac records, and observer database etc, match with our CWT recoveries so I can add on our ocean regions, filter out Bering and Aleutian Recoveries 
#this data is tricky because there are a decent amount of NAs in fields that we would use to get unique trip IDs. 
#Cruise_no has 671/1593 na's
#PERMIT (not na's but "blanks") 679/1593
#VESSEL OR PLANT CODE 679/1593
#haul delivery 70/1593 na's
#vessel_name 46/1593 
#the mis matches with the codes and vessel names and inconsistent NAs make it so I cant do a unique trip ID, so I am only able to match on dates and vessel names. 
cwt <- read.csv("data/AK_CWT_Trawl/HighSeasCWT2006a_for_Jordan.csv") %>%
  separate(recovery_date, into = c("year", "restofdate"), sep = 4) %>%
  separate(restofdate, into = c("month", "day"), sep = 2) %>%
  mutate(Vessel_name = as.character(Vessel_name), 
         year=as.numeric(year), 
         month = as.numeric(month), 
         day = as.numeric(day)) %>%
  filter(!year == 2018) %>%
  rename(vessel_name= Vessel_name) %>% #Vessel code is the equivalent to PERMIT in Norpac database. A lot of boat names in CWT are some sort of permit #, and match with permit number. Ex. A0825
  select(recovery_id, tag_code, Observer, vessel_name,Vessel_or_Plant_code, Haul_or_Delivery_no, year, month, day, Lat, latmin, Long, Longmin) %>%
  #filter(!is.na(Cruise_no) & !is.na(PERMIT)) %>% #remove entries that dont have any info 
  #mutate(VESSEL_CODE = as.character(PERMIT), PERMIT= as.character(PERMIT)) %>%#some CWT recoveries have the Vessel_Code classified as PERMIT #, Will use that in to match to norpac too
  #vessel_names are messed up, fix them so matching works better.
  mutate(PERMIT = Vessel_or_Plant_code, 
         vessel_name = case_when(vessel_name %in% c("Ak Beauty", "A_BEAUTY", "Alaska Beauty") ~ "ALASKA BEAUTY",
                                 vessel_name %in% c("A_DAWN","A366") ~ "ALASKA DAWN",
                                 vessel_name %in% c("ALASKA_ROSE") ~ "ALASKA ROSE",
                                 vessel_name %in% c("A_J") ~ "AJ", 
                                 vessel_name %in% c("AK_COMM") ~ "ALASKAN COMMAND",              
                                 vessel_name %in% c("AK SPIRIT") ~ "ALASKA SPIRIT", 
                                 vessel_name %in% c("A_NO1") ~ "ALEUTIAN NO 1",
                                 vessel_name %in% c("A_FJORD","ARCTIC_FJORD") ~ "ARCTIC FJORD",
                                 vessel_name %in% c("A_VICTORY","ALASKA_VICTORY") ~ "ALASKA VICTORY",
                                 vessel_name %in% c("AM_1","AMERICAN_NOI","AMERICAN_NO1","AMERICAN NO. 1") ~ "AMERICAN NO I",
                                 vessel_name %in% c("AMERICAN_CHALLENGER") ~ "AMERICAN CHALLENGER",    
                                 vessel_name %in% c("ARCTIC_6", "ARCTIC_VI", "ARCTIC") ~ "ARCTIC III",
                                 vessel_name %in% c("A_OCEAN") ~ "ALASKA OCEAN",
                                 vessel_name %in% c("Anthem; A714", "Anthem") ~ "ANTHEM",
                                 vessel_name %in% c("Alderbaren") ~ "ALDEBARAN",  
                                 vessel_name %in% c("VIKING_EXPLORER", "Viking Explorer") ~ "VIKING EXPLORER",
                                 vessel_name == "C_KIWANDA" ~ "CAPE KIWANDA",
                                 vessel_name %in% c("C_BROTHERS", "COLLIER BROS", "Collier Bros", "Collier Brothers") ~ "COLLIER BROTHERS",
                                 vessel_name %in% c("Caravelle", "CORAVILLE", "Caravelle/A322", "CARAVILLE", "Cavavelle" )~ "CARAVELLE",     
                                 vessel_name %in% c("Commodore","COMMODORG") ~ "COMMODORE",
                                 vessel_name %in% c("Destination") ~ "DESTINATION",
                                 vessel_name %in% c("BROWNS_POINT","BROWNS POINT") ~ "BROWNS POINT",
                                 vessel_name %in% c("BLUE_FOX") ~ "BLUE FOX",
                                 vessel_name %in% c("B_EXPLORER") ~ "BRISTOL EXPLORER", 
                                 vessel_name %in% c("BERING_ROSE","BERING ROSE") ~ "BERING ROSE",  
                                 vessel_name %in% c("BAY_ISLANDER", "Bay Islander") ~ "BAY ISLANDER",
                                 vessel_name %in% c("CAL_HORIZON") ~ "CALIFORNIA HORIZON",
                                 vessel_name %in% c("CHELSEA_K") ~ "CHELSEA K",
                                 vessel_name %in% c("CAPE_KINNDER","CAPE KINNDER") ~ "CAPE KINNDER",
                                 vessel_name %in% c("CATLIN_ANN") ~ "CATLIN ANN",
                                 vessel_name %in% c("CHRISTI_ANN") ~ "CHRISTI ANN", 
                                 vessel_name %in% c("Constellation") ~ "CONSTELLATION",
                                 vessel_name %in% c("DONA_MARTITA","D_MARTITA") ~ "DONA MARTITA",  
                                 vessel_name %in% c("DONA_PAULITA") ~ "DONA PAULITA", 
                                 vessel_name %in% c("Elizabeth") ~ "ELIZABETH",
                                 vessel_name %in% c("Elizabeth F","ELIZABETH_F","ELIZBETH F","ELIZEBETH J") ~ "ELIZABETH F",  
                                 vessel_name %in% c("ESKIMO_PRINCESS") ~ "ESKIMO PRINCESS", 
                                 vessel_name %in% c("F_ALLEGIANCE") ~ "FIERCE ALLEGIANCE", 
                                 vessel_name %in% c("FLYING_CLOUD") ~ "FLYING CLOUD",
                                 vessel_name %in% c("Elizabeth F","ELIZABETH_F","ELIZBETH F","ELIZEBETH J", "ELIZABETH F", "ELIZABETH") ~ "ELIZABETH F",  
                                 vessel_name %in% c("ESKIMO_PRINCESS") ~ "ESKIMO PRINCESS", 
                                 vessel_name %in% c("G_MAR","GUN-MAR") ~ "GUNMAR",
                                 vessel_name %in% c("Golden Fleece")~"GOLDEN FLEECE",
                                 vessel_name %in% c("EXCALIBUR_II","EXCALIBER II","EXCALLIBUR II","EX_II") ~ "EXCALIBUR II",
                                 vessel_name %in% c("FIERCE_ALLIGIANCE","FIERCE ALLIANCE") ~  "FIERCE ALLEGIANCE",
                                 vessel_name %in% c("GOLDEN_ALASKA","GOLDEN_ALASKA","GOLDEN_ALASKA", "G_A_(GOLDEN_ALASKA?)","G_ALASKA","GOLDEN AK","M/V_GA") ~ "GOLDEN ALASKA",  
                                 vessel_name %in% c("G_DAWN", "GOLDEN_DAWN")~"GOLDEN DAWN",
                                 vessel_name %in% c("G_PACIFIC", "GREAT_PACIFIC")~ "GREAT PACIFIC",
                                 vessel_name %in% c("GOLD_RUSH","G_RUSH","Gold Rush")~ "GOLD RUSH", 
                                 vessel_name %in% c("GRUMPY_J")~ "GRUMPY J",
                                 vessel_name %in% c("JAMIE_MARIE")~ "JAMIE MARIE",
                                 vessel_name %in% c("H_LORRAINE","HAZEL_LORRAINE","Hazel Lorraine") ~ "HAZEL LOUISE",
                                 vessel_name %in% c("HARVESTER ENTERPRSEI", "HARVESTER ENTERPRSE") ~ "HARVESTER ENTERPRISE",
                                 vessel_name %in% c("Hickory Wind", "H_WIND","HICKORY") ~ "HICKORY WIND",
                                 vessel_name %in% c("H LIGHT","H_LIGHT") ~ "HIGHLAND LIGHT",
                                 vessel_name %in% c("ISLAND_ENTERPRISE","I_ENT") ~ "ISLAND ENTERPRISE",
                                 vessel_name %in% c("KODIAK_ENTERPRISE", "K_ENTERPRISE" ) ~ "KODIAK ENTERPRISE",
                                 vessel_name %in% c("KAREN_EVICH")~"KAREN EVICH",
                                 vessel_name %in% c("KAREN K") ~ "KAREN KAY",
                                 vessel_name %in% c("LISA_MARIE") ~ "LISA MARIE",
                                 vessel_name %in% c("L_LEE", "LESLIE_LEE","Leslie Lee; A493") ~ "LESLIE LEE",  
                                 vessel_name %in% c("L_Melinda", "Lisa Melinda", "LISA_MELINDA", "Lisa Melinda; A313") ~ "LISA MELINDA",  
                                 vessel_name %in% c("Majesty; A315") ~ "MAJESTY",
                                 vessel_name %in% c("MAR_PACIFICO","Mar Pacifico","MARPACIFICO") ~ "MAR PACIFICO", 
                                 vessel_name %in% c("MARCY_J","Marcy J") ~ "MARCY J",
                                 vessel_name %in% c("MICHELLE_RENE","Michelle Renee","M_RENE","MICHELLE RENE") ~ "MICHELLE RENE", 
                                 vessel_name %in% c("MILKY_WAY") ~ "MILKY WAY", 
                                 vessel_name %in% c("MISS_SARAH","Miss Sarah; A479","M_SARAH","MS_SARAH") ~ "MISS SARAH",
                                 vessel_name %in% c("MORNING_STAR","MORNINGSTAR") ~ "MORNING STAR",
                                 vessel_name %in% c("NEW_LIFE","N_LIFE", "NEWLIFE") ~ "NEW LIFE",   
                                 vessel_name %in% c("N_WATCH") ~ "NIGHTWATCH",  
                                 vessel_name %in% c("N_GLACIER") ~ "NORTHERN GLACIER", 
                                 vessel_name %in% c("NORTHERN_EAGLE") ~"NORTHERN EAGLE", 
                                 vessel_name %in% c("Northern Jaeger") ~"NORTHERN JAEGER",
                                 vessel_name %in% c("O_EXPLORER","Ocean Explorer") ~"OCEAN EXPLORER",
                                 vessel_name %in% c("Ocean Hope", "OCEAN HOPE III", "OCEAN HOPE", "Ocean Hope 3", "O_HOPE") ~ "OCEAN HOPE 3", 
                                 vessel_name %in% c("O_PHOENIX","OCEAN_PHOENIX") ~"OCEAN PHOENIX",
                                 vessel_name %in% c("OCEAN_ROVER") ~"OCEAN ROVER", 
                                 vessel_name %in% c("OCEAN_PEACE") ~"OCEAN PEACE", 
                                 vessel_name %in% c("OCEAN_STORM") ~"OCEAN STORM", 
                                 vessel_name %in% c("OCEAN_ENTERPRISE") ~"OCEAN ENTERPRISE",  
                                 vessel_name %in% c("P_EXPLORER") ~"PACIFIC EXPLORER", 
                                 vessel_name %in% c("P_GLACIER") ~"PACIFIC GLACIER", 
                                 vessel_name %in% c("Pacific Star","P_STAR","PACIFIC_STAR","PAC_STAR") ~"PACIFIC STAR", 
                                 vessel_name %in% c("P_VIKING","PACIFIC_VIKING") ~"PACIFIC VIKING",
                                 vessel_name %in% c("P_RAM","PAC_RAM","PACIFIC_RAM") ~"PACIFIC RAM",
                                 vessel_name %in% c("PACIFIC_KNIGHT") ~"PACIFIC KNIGHT",
                                 vessel_name %in% c("PACIFIC_MONARCH") ~"PACIFIC MONARCH",
                                 vessel_name %in% c("PACIFIC_PRINCE","P_PRINCE") ~"PACIFIC PRINCE",
                                 vessel_name %in% c("PACIFIC_CHALLENGER","PAC_CHALLENGER") ~"PACIFIC CHALLENGER",
                                 vessel_name %in% c("PEGGY_JO","P_JO","PEGGY JOE") ~"PEGGY JO",
                                 vessel_name %in% c("Progress") ~"PROGRESS",
                                 vessel_name %in% c("R_AMERICAN") ~"ROYAL AMERICAN",
                                 vessel_name %in% c("R_ATLANTIC") ~"ROYAL ATLANTIC", 
                                 vessel_name %in% c("ROYARK KING") ~"ROYAL KING",
                                 vessel_name %in% c("S_DAWN") ~"SEA DAWN",
                                 vessel_name %in% c("S_ENTERPRISE") ~"SEATTLE ENTERPRISE",
                                 vessel_name %in% c("S_FREEZE") ~"SEAFREEZE ALASKA",
                                 vessel_name %in% c("S_MAC","SEA_MAC") ~"SEA MAC", 
                                 vessel_name %in% c("S_PETREL","STORM PETREL/NORTHERN VICTOR") ~"STORM PETREL",
                                 vessel_name %in% c("S_WOLF","SEA_WOLF") ~"SEA WOLF",
                                # vessel_name %in% c("SEAFISHER") ~"SEA FISHER",
                                 vessel_name %in% c("SUNSET_BAY") ~"SUNSET BAY", 
                                 vessel_name %in% c("SHAWNA_RAZ") ~"SHAWNA RAZ",
                                 vessel_name %in% c("S_V") ~"SEA VENTURE",
                                 vessel_name %in% c("T_ANNE") ~"TRACEY ANNE",
                                 vessel_name %in% c("TOPAZ A234","F/V TOPAZ","APS TOPAZ") ~"TOPAZ",
                                 vessel_name %in% c("TRAVELLER") ~"TRAVELER",
                                 vessel_name %in% c("UNIMAK ENTERPRISE","UNIMAK_ENTERPRISE","UNIMAX") ~"UNIMAK", 
                                 vessel_name %in% c("Vaerdal") ~"VAERDAL",
                                 vessel_name %in% c("Vanguard","Vanguard; A012") ~"VANGUARD",
                                 vessel_name %in% c("W_DAWN") ~"WESTERN DAWN",
                                 vessel_name %in% c("Walter N","WALTER_N","WALTER_W") ~"WALTER N",
                                 vessel_name %in% c("WESTWARD_1","WESTWARD_I") ~"WESTWARD I",
                                 TRUE ~ vessel_name)) %>%
  unite("Lat", c("Lat", "latmin"), sep =".") %>%
  unite("Long", c("Long", "Longmin"), sep = ".") %>%
  mutate(longitude = as.numeric(Long) * -1, latitude = as.numeric(Lat)) %>%
  filter(year > 1995) %>%
  mutate(recovery_id = as.character(recovery_id)) %>%
  left_join(match, by = c("recovery_id", "tag_code")) %>%
  select(recovery_id, tag_code, Observer, vessel_name, Vessel_or_Plant_code, PERMIT, Haul_or_Delivery_no, latitude, longitude, 
         year.x, month.x, day.x, region) %>% #what do we get if we use RMIS dates instead of cwt dates 
  rename(year=year.x, month=month.x, day=day.x) %>% #only 25 without ocean region, some of them have lat and long so use that to match in 
  mutate(region = case_when(is.na(region) & longitude > -140 & latitude > 55.5   ~  "NSEAK", #"X650",
                            is.na(region) & longitude < -140 & longitude > -147 | longitude == -140 ~ "NE.GOA", #"X640",
                            is.na(region) & longitude < -147 & longitude > -154 | longitude == -147 ~ "NW.GOA", #"X630",
                            is.na(region) & longitude < -154 & longitude > -159 | longitude == -154 ~ "E.APEN", #"X620",
                            is.na(region) & latitude > 54 & longitude < -161 ~ "BER",
                            is.na(region) & latitude < 55 & longitude > -164 ~ "W.APEN",
                            is.na(region) & longitude < -159 & longitude > -170 | longitude == -159 & latitude < 55 ~ "W.APEN",
                            is.na(region) & longitude < -159 & longitude > -161 & latitude < 54 ~ "W.APEN",
                            is.na(region) & longitude < -170 & latitude < 54 ~ "ALEUT",
                            is.na(region) & longitude > -136 ~ "SSEAK",
                            TRUE ~ region)) %>%
  filter(!region %in% c("ALEUT", "BER"))  %>%
  left_join(unique_vessel_names_ID) %>% #some RMIS vessel names are actually Vessel ID's, use observer database to decode these and correct them in the RMIS so joins are easier later.
  mutate(vessel_name = case_when(is.na(REAL_vessel_name) ~ vessel_name,
                                 TRUE~ REAL_vessel_name)) %>%
  select(-c(REAL_vessel_name))

cwt.temp <- cwt %>% filter(!vessel_name == "") #take these out so I can match by Vessel_or_Plant_code and then put them back in

unique_vessel_names_plantcode<- unique_vessel_names %>%
  mutate(Vessel_or_Plant_code = as.character(AKR_VESSEL_ID)) %>%
  select(Vessel_or_Plant_code, vessel_name)

cwt <- cwt %>% filter(vessel_name == "") %>%
  mutate(Vessel_or_Plant_code = as.character(Vessel_or_Plant_code))%>%
  left_join(unique_vessel_names_plantcode, by = c("Vessel_or_Plant_code")) %>%
  mutate(vessel_name = case_when(is.na(vessel_name.y) ~ vessel_name.x,
                                 TRUE~ vessel_name.y)) %>%
  select(-c(vessel_name.x,vessel_name.y)) %>%
  rbind(cwt.temp)

# tried also matching with THE OBSERVER DATABASE ADFG NUMBERS ONTO THE VESSEL_OR_PLANT_CODE but it didnt work. I think the leftover are adfg numbers but 
  #thsoe numbers aren't in the observer data
 
#######################################################################################################
# MATCH CWT AND OBSERVER TO GET TRIP TARGET INFORMATION
#######################################################################################################
cwt_observer_join<- left_join(cwt, observer, by = c("year", "month","day","vessel_name")) %>%
  select(recovery_id, tag_code, Observer, vessel_name, Vessel_or_Plant_code, PERMIT, Haul_or_Delivery_no, year, month,day,
         longitude, latitude, region,trip_target_code,REPORTING_AREA_CODE) 
 
#how many match? 751
cwt_observer_join %>%
  filter(!is.na(trip_target_code)) %>%
  summarise(sum(n()))

#pull out the matched data and tidy it so it can be bound later 
join1<- cwt_observer_join %>%
  filter(!is.na(trip_target_code))  

observer_week_end_date <- observer %>%
  separate(week_end_date, into = c("year", "month", "day"), sep="-")  %>%
  mutate(year = as.numeric(year), month = as.numeric(month), day = as.numeric(day))
  
#TRY MATCHING ON WEEK END DATE AND VESSEL NAMES -- only adds 26. 
cwt_observer_join1<- cwt_observer_join %>%
    filter(is.na(trip_target_code)) %>%
    select(recovery_id, tag_code, Observer, vessel_name, PERMIT, Haul_or_Delivery_no, year, month,day,
           longitude, latitude, region,Vessel_or_Plant_code ) %>%
    left_join(observer_week_end_date, by = c("year", "month","day","vessel_name")) %>% 
    select(recovery_id, tag_code, Observer, vessel_name, PERMIT, Haul_or_Delivery_no, year, month,day,
           longitude, latitude, region,REPORTING_AREA_CODE,Vessel_or_Plant_code,trip_target_code)  

cwt_observer_join1 %>%
    filter(!is.na(trip_target_code)) %>%
    summarise(sum(n()))
    
    #pull out the matched data and tidy it so it can be bound later 
join2<- cwt_observer_join1 %>%
    filter(!is.na(trip_target_code)) 
 
#keep these to try and figure out which ones dont match 
na_join<-cwt_observer_join1 %>%
  filter(is.na(trip_target_code))  

cwt_matches<- rbind(join1, join2) 

#######################################################################################################
#the left over NA's, 50% of the data set are matched by fishing dates, if they are within the pollock fishing season we assume they are fishing for pollock. (~800)
#######################################################################################################
#based on dates this just says if people were fishing for pollock or not. Doesnt say anything about processing sector, but I think we are assuming shroeside bc there are not many CP's for pollock in the GOA 
na_join<- na_join %>% 
  unite("fishing_date", c("month", "day","year"),sep = "-", remove=FALSE) %>%
  mutate(fishing_date= as.Date(fishing_date, "%m-%d-%Y")) %>%
  left_join(dates, by = c("fishing_date", "region"))  %>%
  mutate(trip_target_code = case_when(fishing == "pollock" ~"pollock",
                                      TRUE ~ "other")) %>%
  select(-c(fishing, ocean_region, fishing_date))
 
all_cwt<- rbind(na_join, cwt_matches) %>%  
  mutate(
  #processing_sector = case_when(vessel_name %in% c("TRIDENT",  "TRIDENT AKUTAN", "TRIDENT SEAFOODS SAND POINT","UNISEA","WESTERN_AK_FISHERIES", "SANDPOINT PLANT-TRIDENT SEAFOODS",
  #                                                         "NEW_WEST_FISHERIES") ~ "CP", #all of these are catcher processors 
  #                                      is.na(processing_sector) ~ "S",
  #                                      TRUE ~ processing_sector),
#there are 5 recoveries with NA for region, lat and long and REPORTING AREA CODE. There are 3 that have a reporting area code and I can add a region to those. 
         region = case_when(is.na(region) & REPORTING_AREA_CODE == 509 ~ "BER",
                            is.na(region) & REPORTING_AREA_CODE == 620 ~ "E.APEN",
                            is.na(region) & REPORTING_AREA_CODE == 630 ~ "NW.GOA",
                            TRUE ~ region)) %>%
  filter(!is.na(region), !region == "BER")

#all_cwt is the complete version.  
pollock_cwt <- all_cwt %>%
  filter(trip_target_code == "pollock") 

#save file: 
saveRDS(pollock_cwt, "data/AK_CWT_Trawl/CWT_Recovery_Pollock_Shoreside_trip_assignments.RDS")

#######################################################################################################
#MAKE SOME PLOTS
#######################################################################################################
#how does observed pollock effort compare to CWT recoveries?
join_data_shoreside<-readRDS("data/shoreside_pollock_effort.RDS") %>%
  filter(year > 1995)
 
#first plot just cwt recoveries pollock vs other fisheries
 all_cwt  %>%
  filter(#processing_sector == "S",
         !trip_target_code == "pollock, other", !region %in% c("NE.GOA NW.GOA W.APEN E.APEN BER", "NSEAK", "SSEAK")) %>%
  mutate(month = str_pad(month, "left", pad=0, width = 2),
         region = factor(region, levels = c("W.APEN", "E.APEN", "NW.GOA", "NE.GOA","SSEAK"))) %>%
  unite("year.month",c("year","month"), sep = ".") %>% 
  group_by(year.month, region) %>%
  count(trip_target_code) %>%
  ggplot(aes(x=year.month, y = n)) +
  geom_bar(aes(fill = trip_target_code, group = trip_target_code), stat="identity") +
# scale_fill_manual(values = c(pollock = "skyblue", other="grey")) +
  facet_wrap(~region, scales = "free") +
# facet_grid(processing_sector~region,scales="free") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_discrete(breaks = every_nth(n = 10))

cwt_plot<- all_cwt  %>% 
  #filter(processing_sector == "S") %>%
  mutate(month = str_pad(month, "left", pad=0, width = 2)) %>%
  unite("year.month",c("year","month"), sep = ".") %>% 
  group_by(year.month, region) %>%
  count(trip_target_code) %>%
  filter(trip_target_code == "pollock") %>%
  dplyr::rename(pollock_recovery = n)

#how do pollock recoveries line up with observed effort? only a small handful of instances where there are pollock recoveries and no effort...? 
join_data_shoreside %>%  
  ungroup() %>%
  select(year,month,region,observed_boat_days) %>%   
  mutate(month = str_pad(month, "left", pad=0, width = 2)) %>%
  unite("year.month",c("year","month"), sep = ".") %>% 
  full_join(cwt_plot, by= c("year.month","region")) %>%
  select(-trip_target_code) %>%
  gather(3:4, key = "category", value = "number") %>%
  mutate(number= replace_na(number,0), 
         region = factor(region, levels = c("W.APEN", "E.APEN", "NW.GOA", "NE.GOA","SSEAK")),
         category=factor(category, levels=c("pollock_recovery", "observed_boat_days"))) %>%
  filter(!region %in% c("NE.GOA NW.GOA W.APEN E.APEN BER", "NSEAK")) %>%
  ggplot() +
  geom_bar(aes(x=year.month,y=number, fill = category),position="stack", stat="identity",alpha = 0.8) +
  facet_wrap(~region, scales="free_y",ncol = 1) +
  ggtitle("Compare Observed Data and RMIS Pollock Recoveries") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))  +
  scale_x_discrete(breaks = every_nth(n = 20))











