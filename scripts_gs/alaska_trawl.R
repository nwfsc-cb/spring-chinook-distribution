library(here)
library(tidyverse)

#goal is to match the CWT recoveries to boat names and permit names to figure out what the boats were fishing for when they recovered a fish in the CWT Trawl dataset. 
##########################################################################################
#Load NORPAC 
##########################################################################################
#norpac and CWT can match on vessel names and some permit #'s. NORPAC has mulitple permit #'s with the same boat name and different boat lengths, indicating they are seperate boats. 
#cwt and the permit # database I pulled from online can match by more 
norpac_load <- read.csv("data/Norpac Vessel Table_2020.csv")%>%
  rename(Vessel_name = "NAME",ADFG = "ADFG_NUMBER", USCG="COAST_GUARD_NUMBER") %>%
  mutate(PERMIT = as.character(PERMIT),USCG = as.character(USCG)) %>%
  filter(!LENGTH < 25)  #filter out boats smaller than 25 because that seems to be the threshold for matching (no diff between # of matches between 20 and 25), and it is unlikely boats smaller than that are trawling. 

norpac <- norpac_load %>% 
  select(Vessel_name, PERMIT,LENGTH,ADFG, USCG,VESSEL_CODE) %>%
  mutate(USCG = as.numeric(USCG))

norpac_vessel_code <- norpac_load %>% 
  select(Vessel_name, VESSEL_CODE) %>%
  rename(PERMIT ="VESSEL_CODE") %>%
  mutate(PERMIT = as.character(PERMIT))

norpac_vessel_SEQ <- norpac_load %>% 
  select(Vessel_name, VESSEL_SEQ) %>%
  rename(PERMIT ="VESSEL_SEQ") %>%
  mutate(PERMIT = as.character(PERMIT))

norpac_AKR_VESSEL_ID <- norpac_load %>% 
  select(Vessel_name, AKR_VESSEL_ID) %>%
  rename(PERMIT ="AKR_VESSEL_ID") %>%
  mutate(PERMIT = as.character(PERMIT))

norpac_ADFG <- norpac_load %>%
  select(Vessel_name,ADFG) %>%
  rename(PERMIT = "ADFG") %>%
  mutate(PERMIT = as.character(PERMIT))

norpac_USCG <- norpac_load %>%
  select(Vessel_name, USCG) %>%
  rename(PERMIT = "USCG") %>%
  mutate(PERMIT = as.character(PERMIT))

##########################################################################################
#Load Permit Number and Vessel Name Documents for Each Fishery
##########################################################################################
#I skipped all Crab permits, GOA rockfish (seperate dataset, Masuda removed those from this dataset), Halibut/Sablefish (Caught on hook and line not trawl)

### ALEUTIAN POLLOCK 
#probably dont need this - we arent going to have effort data for this fishery.
# Aleut_pollock<- read.csv("data/ak_permits/ai-pollock-2015-2005/Aleutian_pollock_05_15.csv") %>%
#   rename(AFA = "AFA.Permit.Number.") %>%
#   separate(ADF.G...USCG., into = c("ADFG","temp", "USCG")) %>%
#   mutate(USCG = case_when(USCG == "" ~ temp,
#                           TRUE ~ USCG)) %>%
#   select(c("Fishery", "Year","Vessel_name", "AFA", "ADFG", "USCG"))%>%
#   mutate(PERMIT = NA)

### BERING SEA POLLOCK
  #INSHORE COOP MEMBERS
Inshore_Coop1<- read.csv("data/ak_permits/Pollock_AFA_Inshore_Coop_Members/pollock_afa_coop_mbrs_2000-2004.csv") %>%
  mutate(Fishery = "Inshore_Coop_Pollock") %>%
  rename(AFA = "AFA.Permit")
  #2005-2017 [csv's were provided]
temp=list() 
for(y in 5:17) {
  temp[[y]] = read.csv(paste0("data/ak_permits/Pollock_AFA_Inshore_Coop_Members/",y,"afa_list_coop_mbrs.csv"))
  temp[[y]] = temp[[y]] %>% 
    dplyr::select(c(1:6)) %>%
    mutate(Fishery = "Inshore_Coop_Pollock") 
}
Inshore_Coop2 <- map_df(temp,~as.data.frame(x=.x), .id="delete") %>%
  rename(Vessel_name = "VESSEL.NAME", USCG = "CG.NUM", AFA = "VESSEL.AFA.PERMIT") %>%
 select(-c(delete, OWNER.NAME, NMFS.ID, AFA.COOP.ID, COOP.NAME)) 
##### FINAL INSHORE COOP MEMBERS DATAFRAME 
Inshore_Coop_Pollock = rbind(Inshore_Coop1, Inshore_Coop2) %>% #remove duplicates 
  group_by(Fishery,Vessel_name,ADFG,AFA) %>%
  count(USCG)  %>%
  filter(!n<6) %>%
  select(-c(n)) %>%
  data.frame()
rm(Inshore_Coop1,Inshore_Coop2)

#CATCHER PROCESSORS 
Catcher_Processor1<- read.csv("data/ak_permits/Pollock_AFA_Catcher Processors/Pollock_afa_catcher_processor_2000_2004.csv") %>%
  mutate(Fishery = "Catcher_Processor") %>%
  rename( USCG = "USCG.", AFA = "AFA.", ADFG="ADFG.") 
#2005-2017 [csv's were provided]
temp=list() 
for(y in 5:17) {
  temp[[y]] = read.csv(paste0("data/ak_permits/Pollock_AFA_Catcher Processors/",y,"afa_list_cp.csv"))
  temp[[y]] = temp[[y]] %>% 
    dplyr::select(c(1:5)) %>%
    mutate(Fishery = "Catcher_Processor") 
}
Catcher_Processor2 <- map_df(temp,~as.data.frame(x=.x), .id="delete") %>%
  rename(Vessel_name = "VESSEL.NAME", USCG = "CG.NUM", AFA = "AFA.PERMIT") %>%
  select(-c(delete, OWNER.NAME, NMFS.ID,  BSAI.POLLOCK.RESTRICTED)) 
##### FINAL INSHORE COOP MEMBERS DATAFRAME 
Catcher_Processor = rbind(Catcher_Processor1, Catcher_Processor2)  %>%
  group_by(Fishery,Vessel_name,ADFG,USCG) %>%
  count(AFA) %>%
  ungroup() %>%
  filter(!n<6) %>%
  select(-c(n))

rm(Catcher_Processor1, Catcher_Processor2)

#INSHORE PROCESSORS 
Inshore_Processor1<- read.csv("data/ak_permits/Pollock_AFA_inshore-processors/Pollock_inshore_processors_2000-2004.csv") %>%
  mutate(Fishery = "Inshore_Processor") %>%
  select(-c(4,5))
#2005-2017 [csv's were provided]
temp=list() 
for(y in 5:17) {
  temp[[y]] = read.csv(paste0("data/ak_permits/Pollock_AFA_inshore-processors/",y,"afa_list_inshore_proc.csv"))
  temp[[y]] = temp[[y]] %>% 
   # dplyr::select(c(1:5)) %>%
    mutate(Fishery = "Inshore_Processor") 
}
Inshore_Processor2 <- map_df(temp,~as.data.frame(x=.x), .id="delete") %>%
  rename(Vessel_name = "PROCESSOR.NAME", USCG = "CG.NUM", AFA = "AFA.PERMIT") %>%
 select(Vessel_name,USCG,AFA, Fishery)

##### FINAL INSHORE COOP MEMBERS DATAFRAME 
Inshore_Processor1 = rbind(Inshore_Processor1, Inshore_Processor2) 
rm(Inshore_Processor1, Inshore_Processor2)

#MOTHERSHIPS
Mothership1<- read.csv("data/ak_permits/Pollock_AFA_Motherships/afa_motherships_2000_2004.csv") %>%
  mutate(Fishery = "Mothership") 
#2005-2017 [csv's were provided]
temp=list() 
for(y in 5:17) {
  temp[[y]] = read.csv(paste0("data/ak_permits/Pollock_AFA_Motherships/",y,"afa_list_mothership.csv"))
  temp[[y]] = temp[[y]] %>% 
    # dplyr::select(c(1:5)) %>%
    mutate(Fishery = "Mothership") 
}
Mothership2 <- map_df(temp,~as.data.frame(x=.x), .id="delete") %>%
  rename(Vessel_name = "VESSEL.NAME", USCG = "CG.NUM", AFA = "AFA.PERMIT") %>%
  mutate(ADFG = NA) %>%
  select(Vessel_name,USCG,AFA, ADFG,Fishery)
# select(-c(delete, BSAI.POLLOCK.RESTRICTED, COOP.PROCESSOR)) 
##### FINAL INSHORE COOP MEMBERS DATAFRAME 
Mothership = rbind(Mothership1, Mothership2) %>%
  filter(!is.na(ADFG))
rm(Mothership1, Mothership2)
  
#CATCHER VESSEL POLLOCK
      #2000-2004 [copied from PDF]
Catcher_Vessel_Pollock1<- read.csv("data/ak_permits/Pollock_AFA_Catcher_Vessels/afa_bering_sea_pollock_00_04.csv")%>%
  select(c(3:6)) %>%
  mutate(Fishery = "Catcher_Vessel_Pollock") %>%
  rename(Vessel_name="Vessel.Name.", ADFG = "ADFG.", USCG = "USCG.", AFA = "AFA.")
  #2005-2017 [csv's were provided]
temp=list() 
for(y in 5:17) {
  temp[[y]] = read.csv(paste0("data/ak_permits/Pollock_AFA_Catcher_Vessels/",y,"afa_list_cv.csv"))
  temp[[y]] = temp[[y]] %>% 
    dplyr::select(c(1:8)) %>%
    mutate(Fishery = "Catcher_Vessel_Pollock")
}
Catcher_Vessel_Pollock2 <- map_df(temp,~as.data.frame(x=.x), .id="delete") %>%
  select(c(2:5,10)) %>%
  rename(Vessel_name = "VESSEL.NAME", USCG = "CG.NUM",AFA = "AFA.PERMIT") 
##### FINAL CATCHER VESSEL POLLOCK DATAFRAME 
Catcher_Vessel_Pollock = rbind(Catcher_Vessel_Pollock1, Catcher_Vessel_Pollock2)  
rm(Catcher_Vessel_Pollock1,Catcher_Vessel_Pollock2)

### AMENDMENT 80 fisheries (non-pollock-- yellowfin sole, flathead sole, rock sole, Atka mackerel, and Aleutian Islands Pacific ocean perch)
#this data sheet has the year, species, permit #. ---- if I can just match on permit # then I dont necessarily need boat names.. 
Amend_80<- read.csv("data/ak_permits/AMD80/AMD80.csv")  %>%
  mutate(PERMIT=as.character(X.PERMIT..)) %>%
  rename(species = "X") %>%
  select(c(1:2,4,6,8))

#this data sheet has the year, owner, permit #, and vessel name. 
#both of these should be matched so I have permit #, species, and vessel name. 
Amend_80_owners<- read.csv("data/ak_permits/AMD80/amd80_owner_vessel_match.csv") %>%
  rename(Vessel_name = "VESSEL") %>%
  mutate(PERMIT=as.character(X.PERMIT..)) %>%
  select(-c("X.PERMIT..")) %>%
  mutate(Fishery = "AMD80")

AMD80<-Amend_80 %>%
  left_join(Amend_80_owners, by =c("PERMIT", "Fishery", "Year")) %>% #so that I have species caught for each permit #
  left_join(norpac, by =c("PERMIT")) %>% #add on vessel names by the permit number
  rename(Vessel_name = Vessel_name.y #, ADFG = "ADFG_NUMBER", USCG="COAST_GUARD_NUMBER"
         ) %>%
  select(c(Fishery, Year, species, PERMIT, Vessel_name)) #two permit numbers: 37442, 3217 dont have vessel names. 
#AMD80 IS NOW A DATA SHEET WITH SPECIES CAUGHT FOR EACH PERMIT NUMBER AND HAS ASSOCIATED VESSEL NAME AND YEAR. 
#Unique set of permit, vessel and year that can be matched onto AMD80 to get species. Vessels are not permited for all the same species, amount of species permited ranges from 3-8.  
AMD80_unique <- unique(AMD80[c("Vessel_name","PERMIT","Fishery")])

AMD80_unique <- AMD80_unique %>%
  mutate(Vessel_name = as.character(Vessel_name)) %>%
  group_by(Fishery,Vessel_name) %>%
  count(PERMIT) %>%
  mutate(ADFG = NA, USCG = NA, AFA = NA)%>%
  dplyr::select(-c(n)) %>%
  data.frame()
  
rm(temp,y)

#Do I need these? Not sure if they give us any extra info... MAY BE JHELPFUL TO GORUP INTO OTHER? SEE WHAT POLLOCK GIVES THAN CIRCLE BACK
#Federal Fisheries Permits (FFP):This permit is required for US vessels used to fish for groundfish in the Gulf of Alaska or Bering Sea and Aleutian Islands. 
    #This permit is also required for vessels used to fish for any non-groundfish species and that are required to retain any bycatch of groundfish:

#Federal Processor Permits (FPP):Federal Processor Permits Issued to Shoreside Processors - Federal Processor Permits are also required for shoreside processors that receive and/or process groundfish harvested from Federal waters (or from any Federally-permitted vessels). 
  #FPPs are non-transferable, one-year permits, issued to owners on request and without charge. Also-- Federal Processor Permits Issued to Stationary Floating Processors - This permit is required for stationary floating processors (processing vessels that operate solely within Alaska State waters). 

#######################################################################################################
  #RBIND EACH FISHERY SO THEY CAN BE MATCHED WITH CWT AND NORPAC TO LINK ACTUAL RECOVERIES TO FISHERY
#######################################################################################################

permit_numbers<- data.frame(rbind(Mothership,Inshore_Coop_Pollock,Catcher_Processor,Catcher_Vessel_Pollock)) %>%
   group_by(Fishery,Vessel_name,ADFG,AFA) %>%
   count(USCG) %>%
   ungroup() %>%
   mutate(Vessel_name = as.character(Vessel_name)) %>%
   dplyr::select(-c(n)) %>%
   mutate(PERMIT = NA) %>%
   rbind(AMD80_unique)%>%
  select(Vessel_name, Fishery)

#######################################################################################################
#LOAD CWT 
#######################################################################################################
#LOAD CWT
cwt <- read.csv("data/HighSeasCWT2006a_for_Jordan.csv" ) %>%
  separate(recovery_date, into = c("Year", "restofdate"), sep = 4) %>%
  separate(restofdate, into = c("Month", "Day"), sep = 2) %>%
  mutate(Year = as.numeric(Year),Month = as.numeric(Month),Day = as.numeric(Day), Vessel_name = as.character(Vessel_name)) %>%
  filter(!Year == 2018) %>%
  rename(PERMIT = "Vessel_or_Plant_code") %>% #Vessel code is the equivalent to PERMIT in Norpac database. A lot of boat names in CWT are some sort of permit #, and match with permit number. Ex. A0825
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
                              #   Vessel_name %in% c("W_PLANT") ~"SUNSET BAY", 
                                 Vessel_name %in% c("Walter N","WALTER_N","WALTER_W") ~"WALTER N",
                                 Vessel_name %in% c("WESTWARD_1","WESTWARD_I") ~"WESTWARD I",
                                 TRUE ~ Vessel_name))


#######################################################################################################
# START TO MATCH AND FIGURE OUT WHAT FLEETS ARE ASSOCIATED WITH SPECIFIC RECOVERIES 
#######################################################################################################
# #START MATCHING for recoveries without a PERMIT name. 
# match_blank_permit<- cwt %>%
#   filter(PERMIT =="") %>% #avoids duplicate matches 
# left_join(permit_numbers, by = c("Vessel_name")) 
# 
# 
#try matching permit numbers and norpac to have 1 reference database. then add on to the CWT data....
reference <- left_join(norpac, permit_numbers, by=c("Vessel_name")) #permit,ADFG,USCG from norpac and from online are not usually same #'s... 

reference_no_na <- reference %>% 
  filter(!is.na(Fishery)) %>%
  group_by(Vessel_name, VESSEL_CODE, LENGTH,PERMIT, USCG,ADFG) %>% 
  mutate(Fishery = paste(Fishery, collapse=",")) #one line/all fisheries associated with that permit number 

Unique_Ref<-  unique(reference_no_na[c("LENGTH","VESSEL_CODE", "Vessel_name","Fishery", "PERMIT", "ADFG", "USCG")])

#MAKE A UNIQUE_REF DATAFRAME FOR EACH PERMIT TYPE. JUST DO SELECT AND FILTER OUT NA'S
#LEFT OFF - NEED TO MATCH IN UNIQUE REF TO CWT FOR EACH PERMIT TYPE, DO WHAT I DO BELOW. HOPE THAT THIS WAY IT AVOIDS ALL THE DUPLICATES AND WEIRD MIS-MATCHES. 
Unique_Ref_ADFG <- Unique_Ref %>%
  ungroup() %>%
  select(Vessel_name, Fishery, LENGTH, ADFG) %>%
  filter(!is.na(ADFG))
Unique_Ref_VESSEL_CODE <- Unique_Ref %>%
  ungroup() %>%
  select(Vessel_name, Fishery, LENGTH, VESSEL_CODE) %>%
  filter(!is.na(VESSEL_CODE))
Unique_Ref_PERMIT <- Unique_Ref %>%
  ungroup() %>%
  select(Vessel_name, Fishery, LENGTH, PERMIT) %>%
  filter(!is.na(PERMIT))
Unique_Ref_USCG<- Unique_Ref %>%
  ungroup() %>%
  select(Vessel_name, Fishery, LENGTH, USCG,PERMIT) %>%
  filter(!is.na(USCG)) 

# Match in what you can from NORPAC and permit database
match <- cwt %>% 
  mutate(VESSEL_CODE = PERMIT #useless, I dontt match on these,USCG = as.integer(PERMIT),ADFG = as.integer(PERMIT)
         )%>% 
  left_join(Unique_Ref_VESSEL_CODE, by = c("Vessel_name","VESSEL_CODE")) %>%
  left_join(Unique_Ref_PERMIT, by = c("Vessel_name","PERMIT")) %>%
 #nothing matches here---  left_join(Unique_Ref_ADFG, by = c("Vessel_name", "ADFG")) %>%
  mutate(Fishery = (case_when(is.na(Fishery.y) ~ Fishery.x,
                                           is.na(Fishery.x) ~ Fishery.y,
                                           TRUE ~ "FIX_ME" ))) %>%
  mutate(LENGTH.y = as.character(LENGTH.y), LENGTH.x = as.character(LENGTH.x),
    LENGTH = (case_when(is.na(LENGTH.y) ~ LENGTH.x,
                                          is.na(LENGTH.x) ~ LENGTH.y,
                                          TRUE ~ "FIX_ME" ))) %>%
  select(-c(LENGTH.y,LENGTH.x, Fishery.y,Fishery.x)) %>%
    #some vessel_names in CWT are actually vessel codes. match those here.
  mutate(VESSEL_CODE=Vessel_name) %>%
  left_join(Unique_Ref_VESSEL_CODE, by = "VESSEL_CODE") %>%
    #mutate the Vessel names for the CWT vessels that start with A
  mutate(Vessel_name = case_when(str_detect(Vessel_name.x, "^A") ~ Vessel_name.y,
                                 TRUE~ Vessel_name.x),
         Fishery = (case_when(is.na(Fishery.y) ~ Fishery.x,
                                     is.na(Fishery.x) ~ Fishery.y,
                                     TRUE ~ "FIX_ME")),
         LENGTH.y = as.character(LENGTH.y), LENGTH.x = as.character(LENGTH.x),
         LENGTH = (case_when(is.na(LENGTH.y) ~ LENGTH.x,
                                      is.na(LENGTH.x) ~ LENGTH.y,
                                      TRUE ~ "FIX_ME" )))  %>%
  select(-c(LENGTH.y,LENGTH.x, Fishery.y,Fishery.x,Vessel_name.x,Vessel_name.y,VESSEL_CODE))
 
#Some boat names in "match" are known, but the associated length and fishery data doesnt transfer because we dont have an associated PERMIT number. Here, I take a list of boats where we do have info (based on match)
  #and simplify it so it can be joined in on itself and fill in some of the NA's for Length and fishery. 
  #Some boat names appear in Norpac twice, those have been excluded because they dont match correctly and I dont know which length to use. 

cwt_unique <- unique(match[c("Vessel_name","LENGTH", "Fishery")])
cwt_unique_na<-cwt_unique%>% #if the LENGTH IS NA FROM ABOVE, SEE IF YOU CAN RE-JOIN WITH NORPAC, JUST ON VESSEL NAME TO INFORM THE LENGTH INFO. 
  filter(is.na(LENGTH)) %>%
  select(Vessel_name) %>%
  left_join(norpac, by = c("Vessel_name")) %>%
  filter(!is.na(LENGTH)) %>%
  select(Vessel_name,LENGTH) %>%
  mutate(Fishery = NA)
cwt_unique_no_na <- cwt_unique %>%
  filter(!is.na(LENGTH)) 
#Vessels that appear twice are removed, will have to look those up in observer database, I cant reliably join them in
cwt_length_addition <- rbind(cwt_unique_na, cwt_unique_no_na) %>% #BIND THEM BACK TOGETHER SO MAXIMUM JOINING OPPORTUNITY
  filter(!is.na(LENGTH)) %>%
  group_by(Vessel_name) %>%
  filter(n() == 1)

match <- match %>%
  left_join(cwt_length_addition, by = c("Vessel_name")) %>%
  mutate(Fishery = (case_when(is.na(Fishery.y) ~ Fishery.x,
                              is.na(Fishery.x) ~ Fishery.y,
                              TRUE ~ Fishery.x)),
         LENGTH.y = as.character(LENGTH.y), LENGTH.x = as.character(LENGTH.x),
         LENGTH = (case_when(is.na(LENGTH.y) ~ LENGTH.x,
                             is.na(LENGTH.x) ~ LENGTH.y,
                             TRUE ~LENGTH.x))) %>%
  select(-c(LENGTH.y,LENGTH.x, Fishery.y,Fishery.x)) 

#~50% still dont match, but now all boat lengths were added. 
No_Match <- match %>%
  filter(is.na(Fishery))

## Make the boats big or small based on year for Pollock fishery (not all these boats are Pollock but that is ok, will filter out later)
# BEFORE 2013, SMALL <125 FT
# 2013 AND AFTER, SMALL < 60FT
match <- match %>%
  mutate(LENGTH = as.integer(LENGTH)) %>%
  mutate(length_category = case_when(LENGTH < 125 & Year < 2013 ~ "Small",
                                     LENGTH < 60 & Year > 2013 ~ "Small",
                                     is.na(LENGTH) ~ "NA",
                                     TRUE ~ "Large" ))

#List of recoveries without Length or Fishery
  #for these, vessel names are blank, numbers, or duplicates in the NorPac database, some of these are not in the Norpac database. 
Recoveries_NA_Length_Fishery <- match %>%
  filter(#is.na(LENGTH) & 
    is.na(Fishery))
#write_csv(Recoveries_NA_Length_Fishery, "Recoveries_NA_Length_Fishery.csv")

#######################################################################################################
#PLOT
#######################################################################################################

#plot out known and unknown Fishery info by year
match %>%
  mutate(data = case_when(is.na(Fishery) ~ "Unknown",
                           TRUE~ "Known")) %>%
  ungroup() %>%
  group_by(Year) %>%
  count(data) %>%
  ggplot(aes(x= Year, y = n, fill = data, group=data)) +
    geom_bar(stat = "identity")+
  theme_classic() +
  ylab("Recovery Count")

#######################################################################################################
# Get unique data to pair with observer database
#######################################################################################################

Duplicate_vessel_names <- rbind(cwt_unique_na, cwt_unique_no_na) %>% #duplicate vessel names from Norpac that are also present in CWT recovery data
  select(-c(Fishery)) %>%
  filter(!is.na(LENGTH)) %>%
  group_by(Vessel_name,LENGTH) %>%
  filter(!n() > 1)

# create unique observer, vessel name and permit number list to match to observer database 
unique_list <- unique(Recoveries_NA_Length_Fishery[c("Observer", "Year", "Month", "Day","Cruise_no", "Vessel_name", "PERMIT")])
# replacing empty string with NA
unique_list <- unique_list %>% mutate_each(funs(sub("^$", NA, .)), PERMIT)           

write_csv( unique_list,"unique_list.csv")

# filling missing PERMIT values if boat with same name has a permit # elsewhere ... 
df <- unique_list %>% 
  group_by(Vessel_name) %>%
  fill(PERMIT,.direction = "down")%>%
  fill(PERMIT,.direction = "up") %>%
  mutate_each(funs(sub("^$", NA, .)), PERMIT)     
 
#plott when recoveries are from the unique list 

unique_list %>%
  group_by(Year) %>%
  count(Vessel_name) %>%
  ggplot(aes(x= Year, y = n)) +
  geom_bar(stat = "identity")+
  theme_classic() +
  ylab("Recovery Count")




#######################################################################################################
# ASSIGN FISHING BASED ON POLLOCK FLEET FISHING DATES (??)
#######################################################################################################
#first look at distribution of fishing - can I assign to pollock based on that??? 
cwt %>% 
  group_by(Year,Month, Day) %>%
  count(recovery_id) %>%
  ggplot(aes(x=Day, y=n)) +
  geom_bar(stat= "identity") +
  facet_grid(Year~Month)


