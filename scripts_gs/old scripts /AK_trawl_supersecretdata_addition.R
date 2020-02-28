 library(here)
 library(tidyverse)

#######################################################################################################
# LOAD BYCATCH AND OBSERVER DATA FROM AFSC - MATCH SO MAX INFO CAN BE MATCHED WITH CWT AND MINIMIZE DUPES. 
#######################################################################################################
 observer<- readRDS("data/AK_recovery_data_trawl/Observer_data_base/comp_obs_haul.rds") %>% #this has cruise number which, in combo with other columns can be used to match. Cruise # is specific to an observer and their deployment so it can not be the only thing used to match. 
   rename(trip_target_code=TARGET_FISHERY_CODE) %>%
   filter(!trip_target_code == "K") %>% # our recovery data for rockfish are seperate, so dont need them in this database
   mutate(trip_target_code= case_when(trip_target_code %in% c("P","B") ~ "pollock",
                                      # trip_target_code == "K" ~ "rockfish", 
                                      TRUE ~ "other")) %>%
   separate(RETRIEVAL_DATE, into = c("year", "month", "day"), sep="-") %>%
   separate(day, into = c("day", "delete"), sep=" ") %>%
   group_by(year,month,day, CRUISE, AKR_VESSEL_ID, CATCHER_BOAT_ADFG) %>% 
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
            dplyr::select(week_end_date, trip_target_code, vessel_id, gf_processing_sector,trip_target_name, ves_akr_name, ves_akr_length, ves_akr_adfg, 
                  ves_akr_cg_num, catcher_vessel_id, processor_permit_id) %>% #right now I am going to trust the _akr_ data columns because they align with NORPAC. If the vessel names are the same for both columns but lengths differ, the akr column has the same info as norpac
    # but in some cases vessel names differ between columns... for not just going to be consistent and use AKR here. 
    # not sure what vessel ID type to use out of: vessel_id, catcher_vessel_id, processor_permit_id
    # select(-c(pscnq_rate_precedence, psc_fishery_id, adfg_stat_area_code,ves_akr_homeport_state, ves_akr_net_tonnage, ves_akr_gross_tonnage, ves_akr_horsepower,ves_cfec_net_tonnage, 
    #           ves_cfec_gross_tonnage, ves_cfec_horsepower))
    #alot of trips target pollock and something else, combine trip target column to remove duplicate that arise from multiple targets during a trip. 
  filter(!trip_target_code == "K") %>% #our recovery data for rockfish are seperate, so dont need them in this database
  mutate(trip_target_code= case_when(trip_target_code %in% c("P","B") ~ "pollock",
                               #   trip_target_code == "K" ~ "rockfish", 
                                  TRUE ~ "other")) %>%
  select(-c(trip_target_name)) %>%
  separate(week_end_date, into = c("year", "month", "day"),sep="-") %>%
  #for the bycatch data, the date provided is "week end date" not recovery date, that is not helpful when matching to CWT, so I condensed for bycatch/month. Fleet will either be pollock, other or pollock/other. 
  group_by(year, month, vessel_id, gf_processing_sector,ves_akr_cg_num, ves_akr_adfg, #catcher_vessel_id, 
           #processor_permit_id,
           ves_akr_name, ves_akr_length) %>% 
  summarise(trip_target_code = paste(trip_target_code, collapse=",")) %>% #one line/all fisheries associated with that permit number 
#simplify so it is either pollock, other, or pollock&other
  mutate(trip_target_code =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "pollock, other",
                                             grepl("pollock", trip_target_code) ~ "pollock",
                                             grepl("other", trip_target_code) ~ "other",
                                                     TRUE ~ "FIX")) %>%
                  data.frame()   

bycatch.early <- bycatch %>% filter(year < 1996)
bycatch <- bycatch %>% filter(year > 1995)
rm(bycatch.load)

#akr_vessel_id in the observer data == the vessel.id in the bycatch data

#######################################################################################################
# LOAD AND CORRECT VESSEL NAMES IN CWT DATA 
#######################################################################################################
cwt <- read.csv("data/AK_recovery_data_trawl/HighSeasCWT2006a_for_Jordan.csv") %>%
  separate(recovery_date, into = c("year", "restofdate"), sep = 4) %>%
  separate(restofdate, into = c("month", "day"), sep = 2) %>%
  mutate(year = as.numeric(year),month = as.numeric(month),day = as.numeric(day), Vessel_name = as.character(Vessel_name)) %>%
  filter(!year == 2018) %>%
  rename(PERMIT = "Vessel_or_Plant_code") %>% #Vessel code is the equivalent to PERMIT in Norpac database. A lot of boat names in CWT are some sort of permit #, and match with permit number. Ex. A0825
  select(recovery_id, tag_code, Observer, Vessel_name, Cruise_no, PERMIT, Haul_or_Delivery_no, year, month, day, Lat, Long) %>%
  #filter(!is.na(Cruise_no) & !is.na(PERMIT)) %>% #remove entries that dont have any info 
  #mutate(VESSEL_CODE = as.character(PERMIT), PERMIT= as.character(PERMIT)) %>%#some CWT recoveries have the Vessel_Code classified as PERMIT #, Will use that in to match to norpac too
  #VESSEL_NAMES ARE MESSED UP, FIX THEM HERE SO MATCHING IS BETTER 
  mutate(Vessel_name = case_when(Vessel_name == "Viking Explorer" ~ "VIKING EXPLORER",
                                 Vessel_name == "C_KIWANDA" ~ "CAPE KIWANDA",
                                 Vessel_name %in% c("C_BROTHERS", "COLLIER BROS", "Collier Bros", "Collier Brothers") ~ "COLLIER BROTHERS",
                                 Vessel_name %in% c("Caravelle", "CORAVILLE", "Caravelle/A322", "CARAVILLE", "Cavavelle" )~ "CARAVELLE",     
                                 Vessel_name %in% c("Commodore","COMMODORG") ~ "COMMODORE",
                                 Vessel_name %in% c("Destination") ~ "DESTINATION",
                                 Vessel_name %in% c("BROWNS_POINT","BROWNS POINT") ~ "BROWNS POINT",
                                 Vessel_name %in% c("BLUE_FOX") ~ "BLUE FOX",
                                 Vessel_name %in% c("B_EXPLORER") ~ "BRISTOL EXPLORER", 
                                 Vessel_name %in% c("BERING_ROSE","BERING ROSE") ~ "BERING ROSE",  
                                 Vessel_name %in% c("BAY_ISLANDER", "Bay Islander") ~ "BAY ISLANDER",
                                 Vessel_name %in% c("CAL_HORIZON") ~ "CALIFORNIA HORIZON",
                                 Vessel_name %in% c("CHELSEA_K") ~ "CHELSEA K",
                                 Vessel_name %in% c("CAPE_KINNDER") ~ "CAPE KINNDER",
                                 Vessel_name %in% c("CATLIN_ANN") ~ "CATLIN ANN",
                                 Vessel_name %in% c("CHRISTI_ANN") ~ "CHRISTI ANN", 
                                 Vessel_name %in% c("Constellation") ~ "CONSTELLATION",
                                 Vessel_name %in% c("DONA_MARTITA","D_MARTITA") ~ "DONA MARTITA",  
                                 Vessel_name %in% c("DONA_PAULITA") ~ "DONA PAULITA", 
                                 Vessel_name %in% c("Elizabeth") ~ "ELIZABETH",
                                 Vessel_name %in% c("Elizabeth F","ELIZABETH_F","ELIZBETH F","ELIZEBETH J") ~ "ELIZABETH F",  
                                 Vessel_name %in% c("ESKIMO_PRINCESS") ~ "ESKIMO PRINCESS", 
                                 Vessel_name %in% c("F_ALLEGIANCE") ~ "FIERCE ALLEGIANCE", 
                                 Vessel_name %in% c("FLYING_CLOUD") ~ "FLYING CLOUD",
                                 Vessel_name %in% c("Elizabeth F","ELIZABETH_F","ELIZBETH F","ELIZEBETH J") ~ "ELIZABETH F",  
                                 Vessel_name %in% c("ESKIMO_PRINCESS") ~ "ESKIMO PRINCESS", 
                                 Vessel_name %in% c("G_MAR","GUN-MAR") ~ "GUNMAR",
                                 Vessel_name %in% c("Golden Fleece")~"GOLDEN FLEECE",
                                 Vessel_name %in% c("EXCALIBUR_II","EXCALIBER II","EXCALLIBUR II","EX_II") ~ "EXCALIBUR II",
                                 Vessel_name %in% c("FIERCE_ALLIGIANCE","FIERCE ALLIANCE") ~  "FIERCE ALLEGIANCE",
                                 Vessel_name %in% c("GOLDEN_ALASKA","GOLDEN_ALASKA","GOLDEN_ALASKA", "G_A_(GOLDEN_ALASKA?)","G_ALASKA","GOLDEN AK","M/V_GA") ~ "GOLDEN_ALASKA",  
                                 Vessel_name %in% c("G_DAWN", "GOLDEN_DAWN")~"GOLDEN DAWN",
                                 Vessel_name %in% c("G_PACIFIC", "GREAT_PACIFIC")~ "GREAT PACIFIC",
                                 Vessel_name %in% c("GOLD_RUSH","G_RUSH","Gold Rush")~ "GOLD RUSH",
                                 Vessel_name %in% c("H_LORRAINE","HAZEL_LORRAINE","Hazel Lorraine") ~ "HAZEL LORRAINE",
                                 Vessel_name %in% c("HARVESTER ENTERPRSEI") ~ "HARVESTER ENTERPRSE",
                                 Vessel_name %in% c("Hickory Wind", "H_WIND","HICKORY") ~ "HARVESTER ENTERPRSE",
                                 Vessel_name %in% c("H LIGHT","H_LIGHT") ~ "HIGHLAND LIGHT",
                                 Vessel_name %in% c("ISLAND_ENTERPRISE","I_ENT") ~ "ISLAND ENTERPRISE",
                                 Vessel_name %in% c("KODIAK_ENTERPRISE", "K_ENTERPRISE" ) ~ "KODIAK ENTERPRISE",
                                 Vessel_name %in% c("KAREN_EVICH")~"KAREN EVICH",
                                 Vessel_name %in% c("KAREN K") ~ "KAREN KAY",
                                 Vessel_name %in% c("L_LEE", "LESLIE_LEE","Leslie Lee; A493") ~ "LESLIE LEE",  
                                 Vessel_name %in% c("L_Melinda", "Lisa Melinda", "LISA_MELINDA", "Lisa Melinda; A313") ~ "LISA MELINDA",  
                                 Vessel_name %in% c("Majesty; A315") ~ "MAJESTY",
                                 Vessel_name %in% c("MAR_PACIFICO","Mar Pacifico","MARPACIFICO") ~ "MAR PACIFICO", 
                                 Vessel_name %in% c("MARCY_J","Marcy J") ~ "MARCY J",
                                 Vessel_name %in% c("MICHELLE_RENE","Michelle Renee","M_RENE") ~ "MICHELLE RENE", 
                                 Vessel_name %in% c("MILKY_WAY") ~ "MILKY WAY", 
                                 Vessel_name %in% c("MISS_SARAH","Miss Sarah; A479","M_SARAH","MS_SARAH") ~ "MISS SARAH",
                                 Vessel_name %in% c("MORNING_STAR","MORNINGSTAR") ~ "MORNING STAR",
                                 Vessel_name %in% c("NEW_LIFE","N_LIFE", "NEWLIFE") ~ "NEW LIFE",   
                                 Vessel_name %in% c("N_WATCH") ~ "NIGHTWATCH",  
                                 Vessel_name %in% c("N_GLACIER") ~ "NORTHERN GLACIER", 
                                 Vessel_name %in% c("NORTHERN_EAGLE") ~"NORTHERN EAGLE", 
                                 Vessel_name %in% c("Northern Jaeger") ~"NORTHERN JAEGER",
                                 Vessel_name %in% c("O_EXPLORER","Ocean Explorer") ~"OCEAN EXPLORER",
                                 Vessel_name %in% c("Ocean Hope", "OCEAN HOPE III", "OCEAN HOPE", "Ocean Hope 3", "O_HOPE") ~ "OCEAN HOPE 3", 
                                 Vessel_name %in% c("O_PHOENIX","OCEAN_PHOENIX") ~"OCEAN PHOENIX",
                                 Vessel_name %in% c("OCEAN_ROVER") ~"OCEAN ROVER", 
                                 Vessel_name %in% c("OCEAN_PEACE") ~"OCEAN PEACE", 
                                 Vessel_name %in% c("OCEAN_STORM") ~"OCEAN STORM", 
                                 Vessel_name %in% c("OCEAN_ENTERPRISE") ~"OCEAN ENTERPRISE",  
                                 Vessel_name %in% c("P_EXPLORER") ~"PACIFIC EXPLORER", 
                                 Vessel_name %in% c("P_GLACIER") ~"PACIFIC GLACIER", 
                                 Vessel_name %in% c("Pacific Star","P_STAR","PACIFIC_STAR","PAC_STAR") ~"PACIFIC STAR", 
                                 Vessel_name %in% c("P_VIKING","PACIFIC_VIKING") ~"PACIFIC VIKING",
                                 Vessel_name %in% c("P_RAM","PAC_RAM","PACIFIC_RAM") ~"PACIFIC RAM",
                                 Vessel_name %in% c("PACIFIC_KNIGHT") ~"PACIFIC KNIGHT",
                                 Vessel_name %in% c("PACIFIC_MONARCH") ~"PACIFIC MONARCH",
                                 Vessel_name %in% c("PACIFIC_PRINCE","P_PRINCE") ~"PACIFIC PRINCE",
                                 Vessel_name %in% c("PACIFIC_CHALLENGER","PAC_CHALLENGER") ~"PACIFIC CHALLENGER",
                                 Vessel_name %in% c("PEGGY_JO","P_JO","PEGGY JOE") ~"PEGGY JO",
                                 Vessel_name %in% c("Progress") ~"PROGRESS",
                                 Vessel_name %in% c("R_AMERICAN") ~"ROYAL AMERICAN",
                                 Vessel_name %in% c("R_ATLANTIC") ~"ROYAL ATLANTIC", 
                                 Vessel_name %in% c("ROYARK KING") ~"ROYAL KING",
                                 Vessel_name %in% c("S_DAWN") ~"SEA DAWN",
                                 Vessel_name %in% c("S_ENTERPRISE") ~"SEATTLE ENTERPRISE",
                                 Vessel_name %in% c("S_FREEZE") ~"SEAFREEZE ALASKA",
                                 Vessel_name %in% c("S_MAC","SEA_MAC") ~"SEA MAC", 
                                 Vessel_name %in% c("S_PETREL","STORM PETREL/NORTHERN VICTOR") ~"STORM PETREL",
                                 Vessel_name %in% c("S_WOLF","SEA_WOLF") ~"SEA WOLF",
                                 Vessel_name %in% c("SEAFISHER") ~"SEA FISHER",
                                 Vessel_name %in% c("SUNSET_BAY") ~"SUNSET BAY", 
                                 Vessel_name %in% c("SHAWNA_RAZ") ~"SHAWNA RAZ",
                                 Vessel_name %in% c("S_V") ~"SEA VENTURE",
                                 Vessel_name %in% c("T_ANNE") ~"TRACEY ANNE",
                                 Vessel_name %in% c("TOPAZ A234","F/V TOPAZ") ~"TOPAZ",
                                 Vessel_name %in% c("TRAVELLER") ~"TRAVELER",
                                 Vessel_name %in% c("UNIMAK ENTERPRISE","UNIMAK_ENTERPRISE","UNIMAX") ~"UNIMAK", 
                                 Vessel_name %in% c("Vaerdal") ~"VAERDAL",
                                 Vessel_name %in% c("Vanguard","Vanguard; A012") ~"VANGUARD",
                                 Vessel_name %in% c("W_DAWN") ~"WESTERN DAWN",
                                 Vessel_name %in% c("Walter N","WALTER_N","WALTER_W") ~"WALTER N",
                                 Vessel_name %in% c("WESTWARD_1","WESTWARD_I") ~"WESTWARD I",
                                  TRUE ~ Vessel_name))

#######################################################################################################
# MATCH CWT AND OBSERVER -- matches 415, post-1995 recoveries (415/1927) 
#######################################################################################################
obs_renamed <- observer %>% 
  rename(Cruise_no= CRUISE)  %>%
  mutate(id = "observer", year= as.numeric(year), month=as.numeric(month), day=as.numeric(day)) 
  
test_obs_join <- cwt %>%
#  filter(year > 1995) %>% 
  left_join(obs_renamed, by = c("Cruise_no", "year", "month", "day")) %>%
  filter(!is.na(id)) %>%
  select(-c(id))

#######################################################################################################
# HOW OFTEN DOES A BOAT FISH FOR SOMETHING DIFFERENT THAN POLLOCK? Happens annually, not monthly. Means this amy be able to match to CWT- when this gets matched it only matches 495, similiar to above. IDK if the matches are the same or differnt... 
      #only 142 duplicates between the observer matches and the bycatch matches, if I can make sure that I get rid of the duplicates than I will get more than 500 matches. 
#######################################################################################################
bycatch_forjoin <- bycatch %>%
  rename(Vessel_name = ves_akr_name) %>%
  mutate(year= as.numeric(year), month=as.numeric(month))
  
#25% join post 1995 (413 recoveries)
test_bycatch_join <- cwt %>%
#  filter(!year < 1996) %>%
  left_join(bycatch_forjoin, by=c("year","month", "Vessel_name")) %>%
  filter(!is.na(gf_processing_sector))

#see if they overlap
#these are the ones that overlap, filter these out of the other two, rbind all three - That will get me somewhere. 
overlapping_inner_join <- inner_join(test_obs_join,test_bycatch_join) 
obs_individ <- anti_join(test_obs_join,overlapping_inner_join) %>% #filter out the redundant ones. 
  mutate(gf_processing_sector = NA, ves_akr_length= NA,ves_akr_cg_num=NA,ves_akr_adfg=NA, vessel_id = NA)
bycatch_individ <- anti_join(test_bycatch_join,overlapping_inner_join) %>% #filter out the redundant ones. 
  mutate(AKR_VESSEL_ID = NA, CATCHER_BOAT_ADFG= NA) 

#723 MATCHES HERE 
cwt_matches <- rbind(overlapping_inner_join,obs_individ,bycatch_individ)

#now this is what I have left to match to. filter out the ones I already matched.  
cwt_needmatch <- anti_join(cwt, cwt_matches) 

#######################################################################################################
# CAN I MATCH PRE-1996 DATA BASED ON BOATS THAT WERE FISHING BEFORE AND AFTER 1995? -- able to match 57 (out of 430) of these using the bycatch data
    #Also tried this with the observer data base after it was matched with the late cwt, to see if it informs any of the early CWT. able to match 32
    #good news is the matches between observer and bycatch are unique, so matched a total of 89 early recoveries to fleet (but not all of these have processing sector info)
#######################################################################################################
#get unique data from bycatch that could be used to match with CWT data. IE- if a boat has only ever fished for pollock then it would be pretty likely they only fished for pollock before 95. 
unique_bycatch  <- bycatch.early %>%
  group_by(vessel_id, ves_akr_adfg,ves_akr_name, ves_akr_length) %>%
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
                                       TRUE ~ "FIX"))  %>%
  rename(Vessel_name = ves_akr_name) 

#get a list of boat names that never targeted Pollock 
cwt.early <- cwt %>% filter(year <1996)
#Match these on to the CWT data that is prior to 1995 and see if any join in. 
early_join_bycatch <- left_join(cwt.early, unique_bycatch) %>%
 select(c(1:12, 16,17)) %>% #may want to adjust this later
filter(!is.na(trip_target_code))

#########  WITH OBSERVER X CWT DATA
unique_observer  <- test_obs_join %>%
  group_by(Vessel_name, PERMIT,AKR_VESSEL_ID, CATCHER_BOAT_ADFG) %>%
  summarise(trip_target_code = paste(trip_target_code, collapse=",")) %>% # one line/all fisheries associated with that permit number 
  # simplify so it is either pollock or other - will help remove duplicates for matching 
  mutate(trip_target_code =  case_when(grepl("pollock", trip_target_code) & grepl("other", trip_target_code) ~ "pollock, other",
                                       grepl("pollock", trip_target_code) ~ "pollock",
                                       grepl("other", trip_target_code) ~ "other",
                                       TRUE ~ "FIX"))   

early_join_observer <- left_join(cwt.early, unique_observer) %>%
  filter(!is.na(trip_target_code)) %>%
  mutate(gf_processing_sector=NA) %>%
  select(c(1:12,15,16))  #may want to adjust this later

early_matched<-rbind(early_join_observer, early_join_bycatch)
 

#######################################################################################################
# try and match vessel name, AKR VESSEL ID, CATCHER adfg number and year /month - this is kind of useless because it has a lot of duplicates. 
#######################################################################################################
#how many of the columns with similiar names have different info?? need to check ---> Looks like you should use the AKR column  
# unique_bycatch <- unique(bycatch[c("ves_cfec_name", "ves_cfec_length","ves_akr_name","ves_akr_length")])
# #unique_bycatch_ves_name <- unique(bycatch[c("ito_vname", "ves_cfec_name","ves_akr_name")])
# 
# unique_bycatch<-unique_bycatch %>%
#   mutate(ves_cfec_length-ves_akr_length) %>%
#   mutate(source = "bycatch")
# 
# # unique_bycatch_ves_name <- unique_bycatch_ves_name %>%
# #   group_by(ves_cfec_name,ves_akr_name) %>%
# #   count(ito_vname)
# 
# #check vessel lengths to see which is the most reliable. 
# norpac_load <- read.csv("data/AK_recovery_data_trawl/Norpac Vessel Table_2020.csv") 
# norpac_akr<- norpac_load %>%
#   rename(ves_akr_name = "NAME", ves_akr_cg_num = "COAST_GUARD_NUMBER", ves_akr_length = "LENGTH") %>%
#   mutate(ves_akr_cg_num = as.character(ves_akr_cg_num)) %>%
#   select(ves_akr_name, ves_akr_cg_num,ves_akr_length)
# 
# norpac_cfec<- norpac_load %>%
#   rename(ves_cfec_name = "NAME", cg = "COAST_GUARD_NUMBER", ves_cfec_length = "LENGTH") %>%
#  # mutate(ves_akr_cg_num = as.character(ves_akr_cg_num)) %>%
#   select(ves_cfec_name, cg, ves_cfec_length)
# 
# test <- left_join(unique_bycatch,norpac_cfec)

#what about vessel ID's? 
#vessel_id, catcher vessel_id and processor_permit_id
# unique_vessel_id <- unique(bycatch[c("vessel_id", "catcher_vessel_id","processor_permit_id")]) #use processor permit ID- the most complete, all match
# 
# #filter out matches to see which ones dont match, may be it is just NAs make sure if a number is rpesent that they match 
# test <- filter(unique_vessel_id, vessel_id != processor_permit_id)
# 
# test2 <- filter(unique_vessel_id, vessel_id != catcher_vessel_id)

















