library(here)
library(tidyverse)
library(zoo)
#######################################################################################################
# LOAD BYCATCH AND OBSERVER DATA FROM AFSC - MATCH SO MAX INFO CAN BE MATCHED WITH CWT AND MINIMIZE DUPES. 
#######################################################################################################
observer.load <- readRDS("data/AK_recovery_data_trawl/Observer_data_base/comp_obs_haul_v3.rds") 

observer.first.ten <- readRDS("data/AK_recovery_data_trawl/Observer_data_base/comp_obs_haul_v2_toptenrows.rds") 
#results are the same if I match on haul date or retrieval date. 
observer<- observer.load %>% #this has cruise number which, in combo with other columns can be used to match. Cruise # is specific to an observer and their deployment so it can not be the only thing used to match. 
  rename(trip_target_code=TARGET_FISHERY_CODE,week_end_date=WEEK_END_DATE,cruise_no=CRUISE, Haul_or_Delivery_no=HAUL_NUMBER, gf_processing_sector=GF_PROCESSING_SECTOR,vessel_name=VES_AKR_NAME, ves_akr_adfg=CATCHER_BOAT_ADFG,vessel_id=AKR_VESSEL_ID) %>%
  filter(!trip_target_code == "K") %>% # our recovery data for rockfish are seperate, so dont need them in this database
  mutate(trip_target_code= case_when(trip_target_code %in% c("P","B") ~ "pollock",
                                     # trip_target_code == "K" ~ "rockfish", 
                                     TRUE ~ "other")) %>%
  separate(RETRIEVAL_DATE, into = c("year", "month", "day"), sep="-") %>%
  separate(day, into = c("day", "delete"), sep=" ") %>%
  group_by(week_end_date, year,month, day, cruise_no, vessel_name, vessel_id, ves_akr_adfg, gf_processing_sector,OBS_VESSEL_ID, GF_HARVEST_SECTOR,REPORTING_AREA_CODE) %>% 
  summarise(trip_target_code = paste(trip_target_code, collapse=",")) %>% # one line/all fisheries associated with that permit number 
  # simplify so it is either pollock or other - will help remove duplicates for matching 
  mutate(trip_target_code =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "pollock, other",
                                       grepl("pollock", trip_target_code) ~ "pollock",
                                       grepl("other", trip_target_code) ~ "other",
                                       TRUE ~ "FIX")) %>%
  data.frame() 

bycatch.load <-  readRDS("data/AK_recovery_data_trawl/Observer_data_base/bycatch_data.RDS") 
#deal with these seperately because info before 1991 isnot in observer database 

bycatch<- bycatch.load %>% #this data doesnt specify anything within a week. date is the end of week date, cant get specific trip info past that. 
  dplyr::select(week_end_date, year.1,trip_target_code, vessel_id, gf_processing_sector,trip_target_name, ves_akr_name, ves_akr_length, ves_akr_adfg, 
                ves_akr_cg_num, catcher_vessel_id, processor_permit_id) %>% #right now I am going to trust the _akr_ data columns because they align with NORPAC. If the vessel names are the same for both columns but lengths differ, the akr column has the same info as norpac
  # but in some cases vessel names differ between columns... for not just going to be consistent and use AKR here. 
  # not sure what vessel ID type to use out of: vessel_id, catcher_vessel_id, processor_permit_id
  # select(-c(pscnq_rate_precedence, psc_fishery_id, adfg_stat_area_code,ves_akr_homeport_state, ves_akr_net_tonnage, ves_akr_gross_tonnage, ves_akr_horsepower,ves_cfec_net_tonnage, 
  #           ves_cfec_gross_tonnage, ves_cfec_horsepower))
  #alot of trips target pollock and something else, combine trip target column to remove duplicate that arise from multiple targets during a trip. 
  rename(vessel_name=ves_akr_name) %>% 
  filter(!trip_target_code == "K") %>% #our recovery data for rockfish are seperate, so dont need them in this database
  mutate(trip_target_code= case_when(trip_target_code %in% c("P","B") ~ "pollock",
                                     #   trip_target_code == "K" ~ "rockfish", 
                                     TRUE ~ "other")) %>%
  select(-c(trip_target_name)) %>%
  # separate(week_end_date, into = c("year", "month", "day"),sep="-") %>%
  #for the bycatch data, the date provided is "week end date" not recovery date, that is not helpful when matching to CWT, so I condensed for bycatch/month. Fleet will either be pollock, other or pollock/other. 
  group_by(year.1,week_end_date, vessel_id, gf_processing_sector,ves_akr_cg_num, ves_akr_adfg, #catcher_vessel_id, 
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
# LOAD AND CORRECT VESSEL NAMES IN CWT DATA 
#######################################################################################################
cwt <- read.csv("data/AK_recovery_data_trawl/HighSeasCWT2006a_for_Jordan.csv") %>%
  separate(recovery_date, into = c("year", "restofdate"), sep = 4) %>%
  separate(restofdate, into = c("month", "day"), sep = 2) %>%
  mutate(
    #year = as.numeric(year),month = as.numeric(month),day = as.numeric(day), 
    Vessel_name = as.character(Vessel_name)) %>%
  filter(!year == 2018) %>%
  rename(PERMIT = "Vessel_or_Plant_code", vessel_name= Vessel_name,cruise_no=Cruise_no) %>% #Vessel code is the equivalent to PERMIT in Norpac database. A lot of boat names in CWT are some sort of permit #, and match with permit number. Ex. A0825
  select(recovery_id, tag_code, Observer, vessel_name, cruise_no, PERMIT, Haul_or_Delivery_no, year, month, day, Lat, Long) %>%
  #filter(!is.na(Cruise_no) & !is.na(PERMIT)) %>% #remove entries that dont have any info 
  #mutate(VESSEL_CODE = as.character(PERMIT), PERMIT= as.character(PERMIT)) %>%#some CWT recoveries have the Vessel_Code classified as PERMIT #, Will use that in to match to norpac too
  #vessel_nameS ARE MESSED UP, FIX THEM HERE SO MATCHING IS BETTER 
  mutate(vessel_name = case_when(vessel_name == "Viking Explorer" ~ "VIKING EXPLORER",
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
                                 vessel_name %in% c("CAPE_KINNDER") ~ "CAPE KINNDER",
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
                                 vessel_name %in% c("Elizabeth F","ELIZABETH_F","ELIZBETH F","ELIZEBETH J") ~ "ELIZABETH F",  
                                 vessel_name %in% c("ESKIMO_PRINCESS") ~ "ESKIMO PRINCESS", 
                                 vessel_name %in% c("G_MAR","GUN-MAR") ~ "GUNMAR",
                                 vessel_name %in% c("Golden Fleece")~"GOLDEN FLEECE",
                                 vessel_name %in% c("EXCALIBUR_II","EXCALIBER II","EXCALLIBUR II","EX_II") ~ "EXCALIBUR II",
                                 vessel_name %in% c("FIERCE_ALLIGIANCE","FIERCE ALLIANCE") ~  "FIERCE ALLEGIANCE",
                                 vessel_name %in% c("GOLDEN_ALASKA","GOLDEN_ALASKA","GOLDEN_ALASKA", "G_A_(GOLDEN_ALASKA?)","G_ALASKA","GOLDEN AK","M/V_GA") ~ "GOLDEN_ALASKA",  
                                 vessel_name %in% c("G_DAWN", "GOLDEN_DAWN")~"GOLDEN DAWN",
                                 vessel_name %in% c("G_PACIFIC", "GREAT_PACIFIC")~ "GREAT PACIFIC",
                                 vessel_name %in% c("GOLD_RUSH","G_RUSH","Gold Rush")~ "GOLD RUSH",
                                 vessel_name %in% c("H_LORRAINE","HAZEL_LORRAINE","Hazel Lorraine") ~ "HAZEL LORRAINE",
                                 vessel_name %in% c("HARVESTER ENTERPRSEI") ~ "HARVESTER ENTERPRSE",
                                 vessel_name %in% c("Hickory Wind", "H_WIND","HICKORY") ~ "HARVESTER ENTERPRSE",
                                 vessel_name %in% c("H LIGHT","H_LIGHT") ~ "HIGHLAND LIGHT",
                                 vessel_name %in% c("ISLAND_ENTERPRISE","I_ENT") ~ "ISLAND ENTERPRISE",
                                 vessel_name %in% c("KODIAK_ENTERPRISE", "K_ENTERPRISE" ) ~ "KODIAK ENTERPRISE",
                                 vessel_name %in% c("KAREN_EVICH")~"KAREN EVICH",
                                 vessel_name %in% c("KAREN K") ~ "KAREN KAY",
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
                                 TRUE ~ vessel_name))

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
  select(-c(actual_ves_name))
cwt.early <- cwt %>% filter(year < 1996)
cwt.late <- cwt %>% filter(year > 1995)
#######################################################################################################
# MATCH BYCATCH AND OBSERVER INFORMATION
#######################################################################################################
#able to join ~75% of them. this gives me more complete info-- but also adds duplicates... 
#this doesnt actually matter that much now that I have more complete observer info--> 
join <- left_join(observer, bycatch, by = c("week_end_date", "vessel_name", "ves_akr_adfg")) %>% #observer has more ocmplete record and 
  #sometimes the bycatch data doesn match up, using that to get vessel_lengths. since they wont hcange/boat name I am just filling in below within a group
  group_by(vessel_name) %>%
  fill(ves_akr_length, .direction = "down") %>%
  fill(ves_akr_length, .direction = "up")   

#how many dont match?
# join %>%
#   filter(is.na(ves_akr_length)) %>%
#   summarise(sum(n()))

#######################################################################################################
# MATCH CWT AND OBSERVER  -- it doesnt work to match on any of the permit numbers.....
#######################################################################################################
# 771 match.  
cwt_observer_join<- left_join(cwt.late, join, by = c("year", "month","day","vessel_name")) %>%
  select(c(1:12,17,20,25)) %>%
  rename(trip_target_code=trip_target_code.x, gf_processing_sector=gf_processing_sector.x, cruise_no = cruise_no.x)

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
  select(c(1:12)) %>%
  left_join(join, by = c("year", "month","day","cruise_no")) %>%
  select(c(1:12,14,17,20,25)) %>%
  rename(trip_target_code=trip_target_code.x, gf_processing_sector=gf_processing_sector.x) %>%
  mutate(vessel_name = case_when(vessel_name.x == "" ~ vessel_name.y,      #this match adds some vessel names 
                                 TRUE ~ vessel_name.x)) %>%
  select(-c(vessel_name.x,vessel_name.y))

cwt_observer_join1 %>%
  filter(!is.na(trip_target_code)) %>%
  summarise(sum(n()))

#pull out the matched data and tidy it so it can be bound later 
join2<- cwt_observer_join1 %>%
  filter(!is.na(trip_target_code))  
#875 match ~ 45% of the late cwt recoveries. 

#now match on year, month,vessel name 
#set up observer data so you can get fishing targets and sectors within a month for each vessel
obs_year_month <- join %>%
  rename(trip_target_code=trip_target_code.x, gf_processing_sector=gf_processing_sector.x) %>% 
  group_by(vessel_name, year,month,ves_akr_length) %>%
  summarise(trip_target_code = paste(trip_target_code, collapse=","),gf_processing_sector=paste(gf_processing_sector, collapse= ",")) %>% # one line/all fisheries associated with that permit number 
  # simplify so it is either pollock or other - will help remove duplicates for matching 
  mutate(trip_target_code =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "pollock, other",
                                       grepl("pollock", trip_target_code) ~ "pollock",
                                       grepl("other", trip_target_code) ~ "other",
                                       TRUE ~ "FIX"))  %>%
  mutate(gf_processing_sector =  case_when(grepl("CP", gf_processing_sector) & grepl("M", gf_processing_sector) ~ "CP,M",
                                           grepl("CP", gf_processing_sector) & grepl("S", gf_processing_sector) ~ "CP,S",
                                           grepl("CP", gf_processing_sector) ~ "CP",
                                           grepl("M", gf_processing_sector) ~ "M",
                                           grepl("S", gf_processing_sector) ~ "S",
                                           TRUE ~ "FIX")) 
cwt_observer_join2<- cwt_observer_join1 %>%
  filter(is.na(trip_target_code)) %>%
  select(c(1:11,15)) %>%
  #rename(vessel_name = vessel_name.x) %>%
  left_join(obs_year_month, by = c("year", "month", "vessel_name"))

#this adds 653 more matches 
cwt_observer_join2 %>%
  filter(!is.na(trip_target_code)) %>%
  summarise(sum(n()))
join3<- cwt_observer_join2 %>%
  filter(!is.na(trip_target_code)) 
#1537 total == 79.7 % of the late cwt recoveries. 

#rbind - this give the total CWT recoveries that matched after 1995.
cwt_matches_late<- rbind(join1, join2, join3)
#I can up this number by fixing more of the CWT names manually. 

#######################################################################################################
# Do boats fish for the same target within a trip? - generally yes. 
#######################################################################################################
#first, do boats switch targets with in a day & among hauls? 
observer_target <- observer %>%
  ungroup() %>%
  group_by(year,month, day, cruise_no, vessel_name, vessel_id, ves_akr_adfg, gf_processing_sector,OBS_VESSEL_ID, GF_HARVEST_SECTOR) %>%
  summarise(trip_target_code = paste(trip_target_code, collapse=",")) %>% #one line/all fisheries associated with that permit number 
  #simplify so it is either pollock, other, or pollock&other
  mutate(trip_target_code_day =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "switch", #if this category comes up it means the boat switched targets within a day
                                           TRUE ~ "no_switch")) %>%
  ungroup() %>%
  count(trip_target_code_day) %>%
  data.frame()   ## only 1.8% of boats switch targets between pollock and other within a day 


#what about within a month?  ## 11% of boats switch targets between pollock and other within a month 
observer_target <- observer %>%
  ungroup() %>%
  group_by(year,month, cruise_no, vessel_name, vessel_id, ves_akr_adfg, gf_processing_sector,OBS_VESSEL_ID, GF_HARVEST_SECTOR) %>%
  summarise(trip_target_code = paste(trip_target_code, collapse=",")) %>% #one line/all fisheries associated with that permit number 
  #simplify so it is either pollock, other, or pollock&other
  mutate(trip_target_code_day =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "switch", #if this category comes up it means the boat switched targets within a day
                                           TRUE ~ "no_switch")) %>%
  ungroup() %>%
  count(trip_target_code_day) %>%
  data.frame()   

#######################################################################################################
# CAN I MATCH PRE-1996 DATA BASED ON BOATS THAT WERE FISHING BEFORE AND AFTER 1995? -- able to match 57 (out of 430) of these using the bycatch data
#Also tried this with the observer data base after it was matched with the late cwt, to see if it informs any of the early CWT. able to match 32
#good news is the matches between observer and bycatch are unique, so matched a total of 89 early recoveries to fleet (but not all of these have processing sector info)
#######################################################################################################
#get unique data from bycatch that could be used to match with CWT data. IE- if a boat has only ever fished for pollock then it would be pretty likely they only fished for pollock before 95. 
unique_bycatch  <- bycatch.early %>%
  group_by(vessel_id, ves_akr_adfg,vessel_name, ves_akr_length) %>% 
  summarise(trip_target_code = paste(trip_target_code, collapse=","),gf_processing_sector=paste(gf_processing_sector, collapse= ",")) %>% # one line/all fisheries associated with that permit number 
  # simplify so it is either pollock or other - will help remove duplicates for matching 
  mutate(trip_target_code =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "pollock, other",
                                       grepl("pollock", trip_target_code) ~ "pollock",
                                       grepl("other", trip_target_code) ~ "other",
                                       TRUE ~ "FIX"))  %>%
  mutate(gf_processing_sector =  case_when(grepl("CP", gf_processing_sector) & grepl("M", gf_processing_sector) ~ "CP,M",
                                           grepl("CP", gf_processing_sector) & grepl("S", gf_processing_sector) ~ "CP,S",
                                           grepl("CP", gf_processing_sector) ~ "CP",
                                           grepl("M", gf_processing_sector) ~ "M",
                                           grepl("S", gf_processing_sector) ~ "S",
                                           TRUE ~ "FIX")) 

#Match these on to the CWT data that is prior to 1995 and see if any join in. 
early_join_bycatch <- left_join(cwt.early, unique_bycatch) %>%
  select(-c(vessel_id, ves_akr_adfg)) %>% 
  filter(!is.na(trip_target_code))

#########  WITH OBSERVER X CWT DATA
unique_observer  <- join %>%
  group_by(vessel_name, ves_akr_length) %>%
  summarise(trip_target_code = paste(trip_target_code.x, collapse=","),gf_processing_sector=paste(gf_processing_sector.x, collapse= ",")) %>%  # one line/all fisheries associated with that permit number 
  # simplify so it is either pollock or other - will help remove duplicates for matching 
  mutate(trip_target_code =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "pollock, other",
                                       grepl("pollock", trip_target_code) ~ "pollock",
                                       grepl("other", trip_target_code) ~ "other",
                                       TRUE ~ "FIX"))   %>%
  mutate(gf_processing_sector =  case_when(grepl("CP", gf_processing_sector) & grepl("M", gf_processing_sector) ~ "CP,M",
                                           grepl("CP", gf_processing_sector) & grepl("S", gf_processing_sector) ~ "CP,S",
                                           grepl("CP", gf_processing_sector) ~ "CP",
                                           grepl("M", gf_processing_sector) ~ "M",
                                           grepl("S", gf_processing_sector) ~ "S",
                                           TRUE ~ "FIX")) 

early_join_observer <- left_join(cwt.early, unique_observer) %>%
  filter(!is.na(trip_target_code))  

#filter out duplicate matches from the bycatch data  
early_join_bycatch<-anti_join(early_join_bycatch,early_join_observer)
cwt_matches_early<- rbind(early_join_bycatch,early_join_observer)

#######################################################################################################
# BIND EARLY AND LATE RECOVERIES AND GROUP THEM BASED ON SIZE ETC .
#######################################################################################################
cwt_matches_all<- rbind(cwt_matches_early, cwt_matches_late) %>%
  # mutate(LENGTH = as.integer(ves_akr_length)) %>%
  mutate(length_category = case_when(ves_akr_length < 125 & year < 2013 ~ "Small",
                                     ves_akr_length < 60 & year > 2013 ~ "Small",
                                     is.na(ves_akr_length) ~ "NA",
                                     TRUE ~ "Large" ))


write_csv(cwt_matches_all, "cwt_ak_matches.csv")
#770 dont have lat and long 
saveRDS(cwt_matches_all,"cwt_ak_matches.RDS")