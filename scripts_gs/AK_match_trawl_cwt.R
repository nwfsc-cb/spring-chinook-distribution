library(here)
library(tidyverse)
library(zoo)
#if you need data for the script and it is not on your computer you need to download it from the shared site SFTP (if you are not Genoa then ask Genoa)
#######################################################################################################
# LOAD BYCATCH AND OBSERVER DATA FROM AFSC - MATCH SO MAX INFO CAN BE MATCHED WITH CWT AND MINIMIZE DUPES. 
#######################################################################################################
# RUN THIS TO GET THE POLLOCK FISHING DATES THAT ARE MATCHED IN LATER source(tidy_pollock_fishing_dates.R)
dates<-readRDS("data/pollock_fishing_dates/pollock_fishing_dates.RDS") #%>%
 #already did this in tidy pollock file  filter(ocean_region == "GOA") 

observer.load <- readRDS("data/AK_recovery_data_trawl/Observer_data_base/comp_obs_haul_v3.rds") 

observer.first.ten <- readRDS("data/AK_recovery_data_trawl/Observer_data_base/comp_obs_haul_v2_toptenrows.rds") 
#results are the same if I match on haul date or retrieval date. 
observer<- observer.load %>% #this has cruise number which, in combo with other columns can be used to match. Cruise # is specific to an observer and their deployment so it can not be the only thing used to match. 
  rename(trip_target_code=TARGET_FISHERY_CODE,week_end_date=WEEK_END_DATE,cruise_no=CRUISE, Haul_or_Delivery_no=HAUL_NUMBER, processing_sector=PSCNQ_PROCESSING_SECTOR,vessel_name=VES_AKR_NAME, ves_akr_adfg=CATCHER_BOAT_ADFG,vessel_id=AKR_VESSEL_ID) %>%
  filter(!trip_target_code == "K") %>% # our recovery data for rockfish are seperate, so dont need them in this database
  mutate(trip_target_code= case_when(trip_target_code %in% c("P","B") ~ "pollock",
                                                                 TRUE ~ "other")) %>%
  separate(RETRIEVAL_DATE, into = c("year", "month", "day"), sep="-") %>%
  separate(day, into = c("day", "delete"), sep=" ") %>%
  group_by(week_end_date, year,month, day, cruise_no, vessel_name, vessel_id, ves_akr_adfg, processing_sector,OBS_VESSEL_ID, REPORTING_AREA_CODE) %>% 
  summarise(trip_target_code = paste(trip_target_code, collapse=",")) %>% # one line/all fisheries associated with that permit number 
  # simplify so it is either pollock or other - will help remove duplicates for matching 
  mutate(trip_target_code =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "pollock, other",
                                       grepl("pollock", trip_target_code) ~ "pollock",
                                       grepl("other", trip_target_code) ~ "other",
                                       TRUE ~ "FIX")) %>%
  data.frame() 
 
bycatch.load <-  readRDS("data/AK_recovery_data_trawl/Observer_data_base/bycatch_data_update.RDS") 
#deal with these seperately because info before 1991 isnot in observer database 

bycatch<- bycatch.load %>% #this data doesnt specify anything within a week. date is the end of week date, cant get specific trip info past that. 
  dplyr::select(week_end_date, reporting_area_code, year.1,trip_target_code, vessel_id, pscnq_processing_sector,trip_target_name, ves_akr_name, ves_akr_length, ves_akr_adfg, 
                ves_akr_cg_num, catcher_vessel_id, processor_permit_id) %>% #right now I am going to trust the _akr_ data columns because they align with NORPAC. If the vessel names are the same for both columns but lengths differ, the akr column has the same info as norpac
  # but in some cases vessel names differ between columns... for not just going to be consistent and use AKR here. 
  # not sure what vessel ID type to use out of: vessel_id, catcher_vessel_id, processor_permit_id
  # select(-c(pscnq_rate_precedence, psc_fishery_id, adfg_stat_area_code,ves_akr_homeport_state, ves_akr_net_tonnage, ves_akr_gross_tonnage, ves_akr_horsepower,ves_cfec_net_tonnage, 
  #           ves_cfec_gross_tonnage, ves_cfec_horsepower))
  #alot of trips target pollock and something else, combine trip target column to remove duplicate that arise from multiple targets during a trip. 
  rename(vessel_name=ves_akr_name, processing_sector=pscnq_processing_sector) %>% 
  filter(!trip_target_code == "K") %>% #our recovery data for rockfish are seperate, so dont need them in this database
  mutate(trip_target_code= case_when(trip_target_code %in% c("P","B") ~ "pollock",
                                     #   trip_target_code == "K" ~ "rockfish", 
                                     TRUE ~ "other")) %>%
  select(-c(trip_target_name)) %>%
  # separate(week_end_date, into = c("year", "month", "day"),sep="-") %>%
  #for the bycatch data, the date provided is "week end date" not recovery date, that is not helpful when matching to CWT, so I condensed for bycatch/month. Fleet will either be pollock, other or pollock/other. 
  group_by(year.1,week_end_date, vessel_id, reporting_area_code,processing_sector,ves_akr_cg_num, ves_akr_adfg, #catcher_vessel_id, 
           #processor_permit_id,
           vessel_name, ves_akr_length) %>% 
  summarise(trip_target_code = paste(trip_target_code, collapse=",")) %>% #one line/all fisheries associated with that permit number 
  #simplify so it is either pollock, other, or pollock&other
  mutate(trip_target_code =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "pollock, other",
                                       grepl("pollock", trip_target_code) ~ "pollock",
                                       grepl("other", trip_target_code) ~ "other",
                                       TRUE ~ "FIX")) %>%
  data.frame()   

bycatch.early <- bycatch %>% filter(year.1 < 1996)
bycatch <- bycatch %>% filter(year.1 > 1995)
#rm(bycatch.load)
#akr_vessel_id in the observer data == the vessel.id in the bycatch data
#######################################################################################################
# MATCH BYCATCH AND OBSERVER INFORMATION
#######################################################################################################
#able to join ~75% of them. this gives me more complete info-- but also adds duplicates... 
#this doesnt actually matter that much now that I have more complete observer info--> 
join <- left_join(observer, bycatch, by = c("week_end_date", "vessel_name", "ves_akr_adfg")) %>% #observer has more complete record and 
  #sometimes the bycatch data doesn match up, using that to get vessel_lengths. since they wont change/boat name I am just filling in below within a group
  group_by(vessel_name) %>%
  fill(ves_akr_length, .direction = "down") %>%
  fill(ves_akr_length, .direction = "up")   %>%
  mutate(REPORTING_AREA_CODE= case_when(is.na(REPORTING_AREA_CODE) ~ reporting_area_code,
                                        TRUE ~ REPORTING_AREA_CODE)) %>%
  select(-c(reporting_area_code))

#how many dont match?
# join %>%
#   filter(is.na(ves_akr_length)) %>%
#   summarise(sum(n()))
#######################################################################################################
# MAP PREP 
#######################################################################################################
df = read.csv("Maps_and_Charts_of_Regions/spatial_bounds_gs_new.csv", stringsAsFactors = FALSE) %>%
  filter(state == 'ak')

#This map plots the spatial boxes that we assign effort and recovery too. 
world <- map_data("world")
north_america <- subset(world, region %in% c("USA", "Canada"))

map<-north_america %>%
  filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
  filter(!long > -115) %>%
  filter(!lat < 46) %>%
  filter(!lat > 62) %>%
  filter(!long < -173) %>%
  ggplot( ) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  coord_fixed(1.3) +
  geom_segment(data = df, colour="gray", aes(x = as.numeric(line.start.lon), 
                                             y = as.numeric(line.start.lat), 
                                             xend = as.numeric(line.end.lon), 
                                             yend = as.numeric(line.end.lat))) +
  #  geom_text(data=df, colour="darkgreen",aes(x=label_long1, y= label_lat1, label= region), size=2)+ #smaller labels for CA, OR, and WA so they fit
  geom_text(data=df, colour="darkgreen",aes(x=as.numeric(label_long2), y=as.numeric(label_lat2), label= region), size=2) +
  theme_classic()
#map

#######################################################################################################
# LOAD AND CORRECT VESSEL NAMES IN CWT DATA 
#######################################################################################################
#setwd("~/Documents/GitHub/spring-chinook-distribution")
rec_codes_lookup = read.csv("data/recovery codes-wietkamp+shelton 12-2018 two PUSO.csv", stringsAsFactors = FALSE) %>%
  mutate(recovery_location_code =location_code) %>%
  mutate(region = Rec.area.Sullaway.Shelton.NMFSstat) %>% 
  select(c(recovery_location_code, region)) %>% #change so that RMIS has same column labels 
  distinct(recovery_location_code, .keep_all = TRUE) #remove duplicates 

#dir <- ("~/Documents/GitHub/rmis-data")
#load("data/final_data/all_chinook.Rdata")
all_chinook<-data.frame(all_chinook) %>%
  separate(recovery_date,c("year","rest"), sep = 4)%>%
  separate(rest, c("month","day"), sep = 2)%>%
  filter(year >1995) %>%
  filter(fishery %in% c(81, 82))

match <- all_chinook %>%
  left_join(rec_codes_lookup, by = "recovery_location_code")

cwt <- read.csv("data/AK_recovery_data_trawl/HighSeasCWT2006a_for_Jordan.csv") %>%
  separate(recovery_date, into = c("year", "restofdate"), sep = 4) %>%
  separate(restofdate, into = c("month", "day"), sep = 2) %>%
  mutate(
    #year = as.numeric(year),month = as.numeric(month),day = as.numeric(day), 
    Vessel_name = as.character(Vessel_name)) %>%
  filter(!year == 2018) %>%
  rename(vessel_name= Vessel_name,cruise_no=Cruise_no) %>% #Vessel code is the equivalent to PERMIT in Norpac database. A lot of boat names in CWT are some sort of permit #, and match with permit number. Ex. A0825
  select(recovery_id, tag_code, Observer, vessel_name, cruise_no, Vessel_or_Plant_code, Haul_or_Delivery_no, year, month, day, Lat, latmin, Long, Longmin) %>%
  #filter(!is.na(Cruise_no) & !is.na(PERMIT)) %>% #remove entries that dont have any info 
  #mutate(VESSEL_CODE = as.character(PERMIT), PERMIT= as.character(PERMIT)) %>%#some CWT recoveries have the Vessel_Code classified as PERMIT #, Will use that in to match to norpac too
  #vessel_nameS ARE MESSED UP, FIX THEM HERE SO MATCHING IS BETTER 
  mutate(PERMIT = Vessel_or_Plant_code, 
    vessel_name = case_when(vessel_name %in% c("Ak Beauty", "A_BEAUTY", "Alaska Beauty") ~ "ALASKA BEAUTY",
                                 vessel_name %in% c("A_DAWN","A366") ~ "ALASKA DAWN",
                                 vessel_name %in% c("ALASKA_ROSE") ~ "ALASKA ROSE",
                                 vessel_name %in% c("A_J") ~ "AJ", 
                                 vessel_name %in% c("A_NO1") ~ "ALEUTIAN NO 1",
                                 vessel_name %in% c("A_FJORD","ARCTIC_FJORD") ~ "ARCTIC FJORD",
                                 vessel_name %in% c("A_VICTORY","ALASKA_VICTORY") ~ "ALASKA VICTORY",
                                 vessel_name %in% c("AM_1","AMERICAN_NOI","AMERICAN_NO1","AMERICAN NO. 1") ~ "AMERICAN NO I",
                                 vessel_name %in% c("AMERICAN_CHALLENGER") ~ "AMERICAN CHALLENGER",    
                                 vessel_name %in% c("ARCTIC_6", "ARCTIC_VI", "ARCTIC") ~ "ARCTIC III",
                                 vessel_name %in% c("A_OCEAN") ~ "ALASKA OCEAN",
                                 vessel_name %in% c("Anthem; A714", "Anthem") ~ "ANTHEM",
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
                                 vessel_name %in% c("MICHELLE_RENE","Michelle Renee","M_RENE") ~ "MICHELLE RENE", 
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
                                 vessel_name %in% c("SEAFISHER") ~"SEA FISHER",
                                 vessel_name %in% c("SUNSET_BAY") ~"SUNSET BAY", 
                                 vessel_name %in% c("SHAWNA_RAZ") ~"SHAWNA RAZ",
                                 vessel_name %in% c("S_V") ~"SEA VENTURE",
                                 vessel_name %in% c("T_ANNE") ~"TRACEY ANNE",
                                 vessel_name %in% c("TOPAZ A234","F/V TOPAZ") ~"TOPAZ",
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
  mutate(longitude = as.numeric(Long) * -1, latitude = as.numeric(Lat))  
 
################ plot existing locations on a map to see if assignments worked
map + geom_point(data= cwt, mapping = aes(x= longitude, y=latitude, color =region), alpha=0.5) 

#use norpac to fill in some of the blank or coded CWT vessel names  - it fills in ~ 700 names 
norpac <- read.csv("data/AK_recovery_data_trawl/Norpac Vessel Table_2020.csv")%>%
  rename(vessel_name = "NAME",ADFG = "ADFG_NUMBER", USCG="COAST_GUARD_NUMBER") %>%
  mutate(PERMIT = as.character(PERMIT),USCG = as.character(USCG))  %>% 
  select(vessel_name, PERMIT,LENGTH,ADFG, USCG,VESSEL_CODE) %>%
  mutate(USCG = as.numeric(USCG))

#this one will match into vessel names 
norpac_vessel_code <- norpac %>% 
  select(vessel_name, VESSEL_CODE) %>%
  rename(actual_ves_name=vessel_name, vessel_name =VESSEL_CODE) %>%
  mutate(vessel_name = as.character(vessel_name))

cwt <- left_join(cwt, norpac_vessel_code, by=c("vessel_name")) %>%
  mutate(actual_ves_name=as.character(actual_ves_name), 
         vessel_name = case_when(is.na(actual_ves_name) ~ vessel_name,
                                 TRUE ~ actual_ves_name)) %>%
  select(-c(actual_ves_name)) %>%
  data.frame()
cwt.early <- cwt %>% filter(year < 1996)

#JOIN RMIS CWT DATA TO GET LOCATIONS FOR RECOVERIES 
cwt.late <- cwt %>% 
  data.frame() %>%
  filter(year > 1995) %>%
  mutate(recovery_id = as.character(recovery_id)) %>%
  left_join(match, by = c("recovery_id", "tag_code")) %>%
  select(recovery_id, tag_code, Observer, vessel_name, cruise_no, Vessel_or_Plant_code, PERMIT, Haul_or_Delivery_no, latitude, longitude, 
         year.x, month.x, day.x, region) %>% #what do we get if we use RMIS dates instead of cwt dates 
  rename(year=year.x, month=month.x, day=day.x) %>% #only 25 without ocean region, some of them have lat and long so use that to match in 
  mutate(region = case_when(is.na(region) & longitude > -140 & latitude > 55.5   ~  "NSEAK", #"X650",
                                is.na(region) & longitude < -140 & longitude > -147 | longitude == -140 ~ "NE.GOA", #"X640",
                                is.na(region) & longitude < -147 & longitude > -154 | longitude == -147 ~ "NW.GOA", #"X630",
                                is.na(region) & longitude < -154 & longitude > -159 | longitude == -154 ~ "W.APEN", #"X620",
                                is.na(region) & latitude > 54 & longitude < -161 ~ "BER",
                                is.na(region) & latitude < 55 & longitude > -164 ~ "E.APEN",
                                is.na(region) & longitude < -159 & longitude > -170 | longitude == -159 & latitude < 55 ~ "E.APEN",
                                is.na(region) & #latitude < 55 & longitude > -164 ~ "E.APEN",
                                is.na(region) & longitude < -159 & longitude > -161 & latitude < 54 ~ "E.APEN",
                                is.na(region) & longitude < -170 & latitude < 54 ~ "ALEUT",
                                is.na(region) & longitude > -136 ~ "SSEAK",
                                                            TRUE ~ region)) %>%
  filter(!region %in% c("ALEUT", "BER")) #FOR NOW JUST FOCUS ON GOA 
#######################################################################################################
# MATCH CWT AND OBSERVER  -- it doesnt work to match on any of the permit numbers.....
#######################################################################################################
#ABOUT 100 DUPLICATES ADDED HERE....
cwt_observer_join<- left_join(cwt.late, join, by = c("year", "month","day","vessel_name")) %>%
  select(recovery_id, tag_code, Observer, vessel_name, cruise_no.x, PERMIT, Haul_or_Delivery_no, year, month,day,
         longitude, latitude, region,processing_sector.x,trip_target_code.x,REPORTING_AREA_CODE, ves_akr_length,Vessel_or_Plant_code) %>%
  rename(trip_target_code=trip_target_code.x, processing_sector=processing_sector.x, cruise_no = cruise_no.x)

#how many match? 
cwt_observer_join %>%
  filter(!is.na(trip_target_code)) %>%
  summarise(sum(n()))

#pull out the matched data and tidy it so it can be bound later 
join1<- cwt_observer_join %>%
  filter(!is.na(trip_target_code))  

#this adds 104 more matches 
cwt_observer_join1<- cwt_observer_join %>%
  filter(is.na(trip_target_code)) %>%
  select(recovery_id, tag_code, Observer, vessel_name, cruise_no, PERMIT, Haul_or_Delivery_no, year, month,day,
         longitude, latitude, region,Vessel_or_Plant_code) %>%
  left_join(join, by = c("year", "month","day","cruise_no")) %>%
  select(recovery_id, tag_code, Observer, vessel_name.x,vessel_name.y,cruise_no, PERMIT, Haul_or_Delivery_no, year, month,day,
         longitude, latitude, region,processing_sector.x,trip_target_code.x,REPORTING_AREA_CODE, ves_akr_length,Vessel_or_Plant_code) %>%
  rename(trip_target_code=trip_target_code.x, processing_sector=processing_sector.x) %>%
  mutate(vessel_name = case_when(vessel_name.x == "" ~ vessel_name.y,      #this match adds some vessel names 
                                 TRUE ~ vessel_name.x)) %>%
  select(-c(vessel_name.x,vessel_name.y))

cwt_observer_join1 %>%
  filter(!is.na(trip_target_code)) %>%
  summarise(sum(n()))

#pull out the matched data and tidy it so it can be bound later 
join2<- cwt_observer_join1 %>%
  filter(!is.na(trip_target_code))  
 
#keep these to try and figure out which ones dont match 
na_join<-cwt_observer_join1 %>%
  filter(is.na(trip_target_code))  

#AVOID DOING THIS FOR NOW BC IT IS KIND OF JANKY-- SEE IF THERE ARE BETTER WAYS TO MATCH THE UNKNOWNS BASED ON DATE AND SECTOR/VESSEL NAME
#now match on year, month,vessel name 
#set up observer data so you can get fishing targets and sectors within a month for each vessel
# obs_year_month <- join %>%
#   rename(trip_target_code=trip_target_code.x, processing_sector=processing_sector.x) %>% 
#   group_by(vessel_name, year,month,ves_akr_length) %>%
#   summarise(trip_target_code = paste(trip_target_code, collapse=","),processing_sector=paste(processing_sector, collapse= ",")) %>% # one line/all fisheries associated with that permit number 
#   # simplify so it is either pollock or other - will help remove duplicates for matching 
#   mutate(trip_target_code =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "pollock, other",
#                                        grepl("pollock", trip_target_code) ~ "pollock",
#                                        grepl("other", trip_target_code) ~ "other",
#                                        TRUE ~ "FIX"))  %>%
#   mutate(processing_sector =  case_when(grepl("CP", processing_sector) & grepl("M", processing_sector) ~ "CP,M",
#                                            grepl("CP", processing_sector) & grepl("S", processing_sector) ~ "CP,S",
#                                            grepl("CP", processing_sector) ~ "CP",
#                                            grepl("M", processing_sector) ~ "M",
#                                            grepl("S", processing_sector) ~ "S",
#                                            TRUE ~ "FIX")) 
# cwt_observer_join2<- cwt_observer_join1 %>%
#   filter(is.na(trip_target_code)) %>%
#   select(recovery_id, tag_code, Observer, vessel_name, cruise_no, PERMIT, Haul_or_Delivery_no, year, month,day,
#          longitude, latitude, region,REPORTING_AREA_CODE) %>%
#   left_join(obs_year_month, by = c("year", "month", "vessel_name"))
# 
# #this adds 653 more matches 
# cwt_observer_join2 %>%
#   filter(!is.na(trip_target_code)) %>%
#   summarise(sum(n()))
# 
# join3<- cwt_observer_join2 %>%
#   filter(!is.na(trip_target_code)) 
# #1537 total == 79.7 % of the late cwt recoveries. 
# na_from_join<-  cwt_observer_join2 %>%
#   filter(is.na(trip_target_code)) 
#rbind - this give the total CWT recoveries that matched after 1995.
cwt_matches_late<- rbind(join1, join2) 
#######################################################################################################
# MATCH CWT AND POLLOCK SEASON FISHING DATES  
#######################################################################################################
#set up date so it matches 
cwt_matches_late <- cwt_matches_late %>%
  unite("fishing_date", c("month", "day","year"),sep = "-", remove=FALSE) %>%
  mutate(fishing_date= as.Date(fishing_date, "%m-%d-%Y")) %>%
  left_join(dates, by = c("fishing_date", "region")) %>%
  mutate(trip_target_code = case_when(processing_sector == "CP" ~ "pollock", #under assumption that everything is pollock
                                      trip_target_code == "pollock, other" & is.na(fishing) ~ "other", 
                                      trip_target_code == "pollock, other" & fishing=="pollock" ~ "pollock", 
                                      TRUE~ trip_target_code)) %>%
  select(-c(REPORTING_AREA_CODE))
# cwt_matches_late %>%
#   filter(is.na(region)) %>%
#   summarise(sum(n()))
# unique(cwt_matches_late$region)
#######################################################################################################
#FIGURE OUT WHAT IS HAPPENING WITH BOATS THAT DON'T JOIN ABOVE ~ 884 BOATS 
#######################################################################################################
#get unique combinations of boat names, sector, and length match in here ----
unique_boat_sector_length <- unique(join[c("year", "month", "vessel_name", "processing_sector.x", "ves_akr_length")])
unique_boat_sector_length <- unique_boat_sector_length %>% 
  group_by(year,month,vessel_name,ves_akr_length) %>%
  summarise(processing_sector = paste(processing_sector.x, collapse=","))

#this adds 125
na_join <- na_join %>% 
  unite("fishing_date", c("month", "day","year"),sep = "-", remove=FALSE) %>%
  mutate(fishing_date= as.Date(fishing_date, "%m-%d-%Y")) %>%
  left_join(dates, by = c("fishing_date", "region")) %>%
  mutate(trip_target_code = case_when(processing_sector == "CP" ~ "pollock", #under assumption that everything is pollock
                                      trip_target_code == "pollock, other" & is.na(fishing) ~ "other", 
                                      trip_target_code == "pollock, other" & fishing=="pollock" ~ "pollock", 
                                      TRUE~ trip_target_code)) %>%
  select(-c(14:17)) %>% 
  left_join(unique_boat_sector_length, by = c("vessel_name", "year", "month")) %>%
  mutate(trip_target_code = case_when(processing_sector == "CP" ~ "pollock", #under assumption that everything is pollock 
                                    TRUE ~ fishing)) %>%
  mutate(trip_target_code=case_when(is.na(trip_target_code) ~ "other",
                                    TRUE ~ trip_target_code)) 

na_join %>%
     filter(!is.na(trip_target_code)) %>%
     summarise(sum(n()))

all_cwt_late<- rbind(na_join, cwt_matches_late) %>% #add a few more in. 
  mutate(processing_sector = case_when(vessel_name %in% c("TRIDENT",  "TRIDENT AKUTAN", "TRIDENT SEAFOODS SAND POINT","UNISEA","WESTERN_AK_FISHERIES", "SANDPOINT PLANT-TRIDENT SEAFOODS",
                                                             "NEW_WEST_FISHERIES") ~ "CP", #all of these are catcher processors 
    TRUE ~ processing_sector))




#######################################################################################################
# ISOLATE RECOVERIES THAT DONT HAVE SECTOR INFO AND USE OTHER DATA SOURCES TO MATCH THEM 
#######################################################################################################
# NOTES FROM JW: "catcher vessel" means "catcher only vessel" and is synonymous with "shoreside" because they have to 
#deliver to a shoreside vessel or to a mother ship. So a vessel that is a catcher vessel could deliver 
#either to a shoreside processor or to a mothership. That's where my uncertainty on the CV / mothership 
#comes in. I need to dive in a little more there. So a "catcher/processor" is always going to be in the 
#CP sector. If a vessel says catcher vessel and never also says mothership than it is in the shoreside sector. 
#But those that have both....well, I'm not exactly sure. That could be a trip-by-trip dealie, unfortunately, so there may be some ambiguity there.....

#datasource: North Pacific Groundfish Observer Program database  - https://inport.nmfs.noaa.gov/inport/item/7290

###################### TIDY THE DATA JORDAN GAVE ME AND SUMMARIZE IT SO IT CAN BE MATCHED INTO CWT DATA. 

#these are vessel names that Jordan matched from NPGOP, some have vessel name and some dont. need to combine both datasets he gave me 
jw_unid<- read.csv("data/AK_recovery_data_trawl/vessels_matched_prelim.csv") %>%
filter(!permit %in% c(3703, 3396,2943, 3362,4056,3664), !vessel_type_desc == "Catcher Vessel - Fish Discarded")  # remove M records based on bycatch data which has these vessels as CV/Shoreside only so going to keep them consistent here.#isolate the vessels that are both CV and Mothership and use that list to filter the bycatch data to see if I can get more specific info like dates. 
#THROUGH DOING THIS NEXT PART I LEARNED THAT EVEN THOUGH SOME BOATS SHOWED UP IN TWO SECTORS DID THIS DIDNT HAPPEN ON THE SAME YEAR. YEARS WE HAD CWT RECOVERIES, THE BOATS WERE CONSISTENTLY IN ONE SECTOR. THUS I COULD JUST DELETE SECTOR CODES THAT DIDNT APPLY TO THIS DATA. 


#create DF that you can semi join to remove boats that appear as motherships and CV's
semi <- jw_unid %>%
  filter(!is.na(vessel_type_desc)) %>%
  group_by(vessel_name) %>%
  count(vessel_type_desc) %>%
  count(vessel_name) %>%
  filter(!n>1) %>%
  select(-c(n))

unid_matched_ves_name <- semi_join(jw_unid, semi) %>%
  filter(!is.na(vessel_type_desc)) %>%
  group_by(vessel_name) %>%
  count(vessel_type_desc) %>%
  select(-c(n)) %>%
  #code these to align with the CWT data
  mutate(processing_sector_add = case_when(vessel_type_desc == "Catcher Vessel" ~ "S",
                                          vessel_type_desc == "Mothership - Unsorted Catch" ~ "M",
                                          vessel_type_desc == "Catcher/Processor Vessel" ~ "CP", 
                                          TRUE ~ "FIX")) %>%
  select(-c(vessel_type_desc))

#add wonky matches to unid_matched and then deal with the other ones seperately. 
jw_unid_2<- read.csv("data/AK_recovery_data_trawl/Wonky_vessel_trips_sector2_fromJW.csv") %>%
#these are trips that Jordan matched from NPGOP that didnt have a vessel name 
  rename(vessel_name = "Vessel_name", processing_sector_add= "pscnq_processing_sector") %>%
  filter(!vessel_name == "") %>% #annoying, blank vessel names arent NA they are just spaces. 
  group_by(vessel_name) %>%
  count(processing_sector_add) %>%
  select(-c(n)) %>%
  rbind(unid_matched_ves_name) %>%
  group_by(vessel_name) %>%
  count(processing_sector_add) %>% #remove duplicates 
 select(-c(n))

############# combine additions to the CWT Late data. 
cwt_late_temp <- left_join(all_cwt_late, jw_unid_2, by = c("vessel_name")) %>%
  mutate(processing_sector= case_when(is.na(processing_sector) ~ processing_sector_add,
                                             TRUE ~ processing_sector),
         PERMIT = as.character(PERMIT)) %>%
  select(-c(processing_sector_add)) 

 #now deal with ones he gave me that dont have vessel names but we could figure out sector using other info. 
#same data as above just filtering out vessels that dont have names 

#also add in the length category 
cwt_late_matched<- read.csv("data/AK_recovery_data_trawl/Wonky_vessel_trips_sector2_fromJW.csv") %>%
   rename(cruise_no="Cruise_no", vessel_name = "Vessel_name", processing_sector_NEW= "pscnq_processing_sector") %>%
   filter(vessel_name == "") %>% #annoying, blank vessel names arent NA they are just spaces. 
   group_by(cruise_no, vessel_code, plant_code) %>%
   count(processing_sector_NEW) %>%
   filter(!is.na(cruise_no)) %>%
   mutate(PERMIT = case_when(is.na(vessel_code) ~ plant_code,
                             TRUE ~ vessel_code), PERMIT = as.character(PERMIT), processing_sector_NEW = as.character(processing_sector_NEW)) %>%
   right_join(cwt_late_temp, by = c("PERMIT", "cruise_no")) %>%
   mutate(processing_sector= case_when(is.na(processing_sector) ~ processing_sector_NEW,
                                       TRUE ~ processing_sector)) %>%
   select(-c(processing_sector_NEW, n)) %>%
#add some of the remaining ones in by hand because I could find info online...
    mutate(processing_sector= case_when(vessel_name %in% c("HAZEL LORRAINE", "CAITLIN ANN", "MICHELLE RENEE", "ALASKA BEAUTY", "COLLIER BROTHERS")  ~ "S",
                                          vessel_name %in% c("HARVESTER ENTERPRISE") ~ "CP",
                                        TRUE ~ processing_sector))%>%
  # mutate(LENGTH = as.integer(ves_akr_length)) %>%
  mutate(length_category = case_when(processing_sector == "CP" ~ "Large",
                                     ves_akr_length < 125 & year < 2013 ~ "Small",
                                     ves_akr_length > 124 & year < 2013 ~ "Large",
                                     ves_akr_length > 59 & year > 2012 ~ "Large",
                                     ves_akr_length < 60 & year > 2012 ~ "Small",
                                    TRUE ~ "NOT_MATCHED" )) 

#100 that we couldnt match. that's ok! 
na_sector <- cwt_late_matched %>%
      filter(is.na(processing_sector))    

na_length <- cwt_late_matched %>%
  filter(length_category == "NOT_MATCHED")    

#Almost there, some of the additions dont have vessel lengths-adding those in.  
#RE SUMMARIZE NORPAC TO MATCH WITH VESSELS THAT DONT MATCH ABOVE -- ALL OF THESE VESSELS *SHOULD* BE IN NORPAC...
    #this one will match into vessel names 
cwt_late_matched1 <- norpac %>% 
     group_by(vessel_name, VESSEL_CODE) %>%
     summarise(LENGTH = paste(LENGTH, collapse=",")) %>%
     mutate(Vessel_or_Plant_code = VESSEL_CODE) %>%
     right_join(cwt_late_matched, by = c("vessel_name","Vessel_or_Plant_code")) %>%
     mutate(ves_akr_length=as.character(ves_akr_length)) %>%
     mutate(ves_akr_length= case_when(is.na(ves_akr_length) ~ LENGTH,
                                        TRUE ~ ves_akr_length)) %>%
     select(-c(LENGTH, VESSEL_CODE))

     #join by vessel name, not code and be done with it. 
cwt_late_matched2 <- norpac %>% 
  group_by(vessel_name) %>%
  summarise(LENGTH = paste(LENGTH, collapse=",")) %>%
  right_join(cwt_late_matched1, by = c("vessel_name")) %>%
  mutate(LENGTH=as.character(LENGTH)) %>%
  mutate(ves_akr_length= case_when(is.na(ves_akr_length) ~ LENGTH,
                                TRUE ~ ves_akr_length)) %>%
  select(-c(LENGTH))%>% #some vessel lengths are doubled up because of more than one boat with that name in the norpac system.so i went through those manually. 
mutate(ves_akr_length=case_when(ves_akr_length=="29,42,42,92,32" ~ "92",  
                                ves_akr_length=="29,76,32,55" ~ "55",  
                                ves_akr_length=="31,40,38,71" ~ "71",  
                                ves_akr_length=="32,53,45" ~ "104",  
                                ves_akr_length%in% c("37,58", "38,58") ~ "58",  
                                ves_akr_length%in% c("41,86") ~ "86",  
                                ves_akr_length%in% c("41,90,90") ~ "90",  
                                ves_akr_length%in% c("43,41,30,40,193") ~ "193",   
                                ves_akr_length%in% c("46,32,43,58") ~ "58",
                                ves_akr_length%in% c("52,73") ~ "73",  
                                ves_akr_length%in% c("56,114") ~ "114", 
                                ves_akr_length%in% c("57,87,32,57") ~ "57",
                                ves_akr_length%in% c("58,185", "58,43" , "58,87") ~ "58",
                                TRUE ~ ves_akr_length), 
       ves_akr_length = as.numeric(ves_akr_length)) %>%
  mutate(processing_sector = case_when(processing_sector %in% c("M,S", "S,M") ~ "S",
                                        TRUE ~ processing_sector),
         length_category = case_when(processing_sector %in% c("M","CP") ~ "Large",
                                     ves_akr_length < 125 & year < 2013 ~ "Small",
                                     ves_akr_length > 124 & year < 2013 ~ "Large",
                                     ves_akr_length > 59 & year > 2012 ~ "Large",
                                     ves_akr_length < 60 & year > 2012 ~ "Small",
                                     TRUE ~ as.character(ves_akr_length)))
                               
#103 left unmatched
na <- cwt_late_matched2 %>%
   filter(length_category == "NOT_MATCHED" | is.na(processing_sector))
   


all_cwt_late  <- cwt_late_matched2













#OLD CODE- LOOKS AT HOW OFTEN BOATS TARGET POLLOCK VS OTHER / YEAR AND MONTH. ALSO PARCES EARLY CWT. NOT USING THIS FOR NOW. 
#######################################################################################################
# Do boats fish for the same target within a trip? - generally yes. 
#######################################################################################################
#first, do boats switch targets with in a day & among hauls? 
# observer_target <- observer %>%
#   ungroup() %>%
#   group_by(year,month, day, cruise_no, vessel_name, vessel_id, ves_akr_adfg, processing_sector,OBS_VESSEL_ID, GF_HARVEST_SECTOR) %>%
#   summarise(trip_target_code = paste(trip_target_code, collapse=",")) %>% #one line/all fisheries associated with that permit number 
#   #simplify so it is either pollock, other, or pollock&other
#   mutate(trip_target_code_day =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "switch", #if this category comes up it means the boat switched targets within a day
#                                            TRUE ~ "no_switch")) %>%
#   ungroup() %>%
#   count(trip_target_code_day) %>%
#   data.frame()   ## only 1.8% of boats switch targets between pollock and other within a day 
# 
# 
# #what about within a month?  ## 11% of boats switch targets between pollock and other within a month 
# observer_target <- observer %>%
#   ungroup() %>%
#   group_by(year,month, cruise_no, vessel_name, vessel_id, ves_akr_adfg, processing_sector,OBS_VESSEL_ID, GF_HARVEST_SECTOR) %>%
#   summarise(trip_target_code = paste(trip_target_code, collapse=",")) %>% #one line/all fisheries associated with that permit number 
#   #simplify so it is either pollock, other, or pollock&other
#   mutate(trip_target_code_day =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "switch", #if this category comes up it means the boat switched targets within a day
#                                            TRUE ~ "no_switch")) %>%
#   ungroup() %>%
#   count(trip_target_code_day) %>%
#   data.frame()   

#######################################################################################################
# EARLY ----- CAN I MATCH PRE-1996 DATA BASED ON BOATS THAT WERE FISHING BEFORE AND AFTER 1995? -- able to match 57 (out of 430) of these using the bycatch data
#Also tried this with the observer data base after it was matched with the late cwt, to see if it informs any of the early CWT. able to match 32
#good news is the matches between observer and bycatch are unique, so matched a total of 89 early recoveries to fleet (but not all of these have processing sector info)
#######################################################################################################
#get unique data from bycatch that could be used to match with CWT data. IE- if a boat has only ever fished for pollock then it would be pretty likely they only fished for pollock before 95. 
# unique_bycatch  <- bycatch.early %>%
#   group_by(vessel_id, ves_akr_adfg,vessel_name, ves_akr_length) %>% 
#   summarise(trip_target_code = paste(trip_target_code, collapse=","),processing_sector=paste(processing_sector, collapse= ",")) %>% # one line/all fisheries associated with that permit number 
#   # simplify so it is either pollock or other - will help remove duplicates for matching 
#   mutate(trip_target_code =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "pollock, other",
#                                        grepl("pollock", trip_target_code) ~ "pollock",
#                                        grepl("other", trip_target_code) ~ "other",
#                                        TRUE ~ "FIX"))  %>%
#   mutate(processing_sector =  case_when(grepl("CP", processing_sector) & grepl("M", processing_sector) ~ "CP,M",
#                                            grepl("CP", processing_sector) & grepl("S", processing_sector) ~ "CP,S",
#                                            grepl("CP", processing_sector) ~ "CP",
#                                            grepl("M", processing_sector) ~ "M",
#                                            grepl("S", processing_sector) ~ "S",
#                                            TRUE ~ "FIX")) 
# 
# #Match these on to the CWT data that is prior to 1995 and see if any join in. 
# early_join_bycatch <- left_join(cwt.early, unique_bycatch) %>%
#   select(-c(vessel_id, ves_akr_adfg)) %>% 
#   filter(!is.na(trip_target_code))
# 
# #########  WITH OBSERVER X CWT DATA
# unique_observer  <- join %>%
#   group_by(vessel_name, ves_akr_length) %>%
#   summarise(trip_target_code = paste(trip_target_code.x, collapse=","),processing_sector=paste(processing_sector.x, collapse= ",")) %>%  # one line/all fisheries associated with that permit number 
#   # simplify so it is either pollock or other - will help remove duplicates for matching 
#   mutate(trip_target_code =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "pollock, other",
#                                        grepl("pollock", trip_target_code) ~ "pollock",
#                                        grepl("other", trip_target_code) ~ "other",
#                                        TRUE ~ "FIX"))   %>%
#   mutate(processing_sector =  case_when(grepl("CP", processing_sector) & grepl("M", processing_sector) ~ "CP,M",
#                                            grepl("CP", processing_sector) & grepl("S", processing_sector) ~ "CP,S",
#                                            grepl("CP", processing_sector) ~ "CP",
#                                            grepl("M", processing_sector) ~ "M",
#                                            grepl("S", processing_sector) ~ "S",
#                                            TRUE ~ "FIX")) 
# 
# early_join_observer <- left_join(cwt.early, unique_observer) %>%
#   filter(!is.na(trip_target_code))  
# 
# #filter out duplicate matches from the bycatch data  
# early_join_bycatch<-anti_join(early_join_bycatch,early_join_observer)
# cwt_matches_early<- rbind(early_join_bycatch,early_join_observer)
