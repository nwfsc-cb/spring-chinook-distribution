library(here)
library(tidyverse)

#goal is to match the CWT recoveries to boat names and permit names to figure out what the boats were fishing for when they recovered a fish in the CWT Trawl dataset. 

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

### BS POLLOCK
############################### <----- NEED TO GO INTO THE EARLY YEARS HERE AND COMBINE THE PDFS' FOR EACH GROUP HERE, LOAD SEPERATELY THEN RBIND.

  #INSHORE COOP MEMBERS
# Inshore_Coop1<- read.csv("data/ak_permits/Pollock_AFA_Inshore_Coop_Members/ "     #####  # COMBINE THE PDFS HERE") %>%
#   select(c(1:7)) %>%
#   mutate(Fishery = "Inshore_Coop_Pollock") %>%
#   rename(Vessel_name="Vessel.Name.", ADFG = "ADFG.", USCG = "USCG.", AFA = "AFA.")
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
Inshore_Coop_Pollock = rbind(Inshore_Coop1, Inshore_Coop2) 
#  mutate(PERMIT = NA)


#CATCHER PROCESSORS 
# Catcher_Processor1<- read.csv("data/ak_permits/Pollock_AFA_Inshore_Coop_Members/ "     #####  # COMBINE THE PDFS HERE") %>%
#   select(c(1:7)) %>%
#   mutate(Fishery = "Catcher_Processor") %>%
#   rename(Vessel_name="Vessel.Name.", ADFG = "ADFG.", USCG = "USCG.", AFA = "AFA.")
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
Catcher_Processor = rbind(Catcher_Processor1, Catcher_Processor2) 
#  mutate(PERMIT = NA)


#INSHORE PROCESSORS 
# Inshore_Processor1<- read.csv("data/ak_permits/Pollock_AFA_Inshore_Coop_Members/ "     #####  # COMBINE THE PDFS HERE") %>%
#   select(c(1:7)) %>%
#   mutate(Fishery = "Inshore_Processor") %>%
#   rename(Vessel_name="Vessel.Name.", ADFG = "ADFG.", USCG = "USCG.", AFA = "AFA.")
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
  # select(-c(delete, BSAI.POLLOCK.RESTRICTED, COOP.PROCESSOR)) 
##### FINAL INSHORE COOP MEMBERS DATAFRAME 
Catcher_Processor = rbind(Inshore_Processor1, Inshore_Processor2) 
#  mutate(PERMIT = NA)

#MOTHERSHIPS
# Mothership1<- read.csv("data/ak_permits/Pollock_AFA_Inshore_Coop_Members/ "     #####  # COMBINE THE PDFS HERE") %>%
#   select(c(1:7)) %>%
#   mutate(Fishery = "Mothership") %>%
#   rename(Vessel_name="Vessel.Name.", ADFG = "ADFG.", USCG = "USCG.", AFA = "AFA.")
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
  select(Vessel_name,USCG,AFA, Fishery)
# select(-c(delete, BSAI.POLLOCK.RESTRICTED, COOP.PROCESSOR)) 
##### FINAL INSHORE COOP MEMBERS DATAFRAME 
Mothership = rbind(Mothership1, Mothership2) 
#  mutate(PERMIT = NA)

  #CATCHER VESSEL POLLOCK
      #2000-2004 [copied from PDF]
Catcher_Vessel_Pollock1<- read.csv("data/ak_permits/Pollock_AFA_Catcher_Vessels/afa_bering_sea_pollock_00_04.csv")%>%
  select(c(1:6)) %>%
  mutate(Fishery = "Catcher_Vessel_Pollock") %>%
  rename(Vessel_name="Vessel.Name.", ADFG = "ADFG.", USCG = "USCG.", AFA = "AFA.")
  #2005-2017 [csv's were provided]
temp=list() 
for(y in 5:17) {
  temp[[y]] = read.csv(paste0("data/ak_permits/afa-catcher-vessels-bering_pollock_2017-2000/",y,"afa_list_cv.csv"))
  temp[[y]] = temp[[y]] %>% 
    dplyr::select(c(1:8)) %>%
    mutate(Year = y) %>%
    mutate(Fishery = "Catcher_Vessel_Pollock")
}
Catcher_Vessel_Pollock2 <- map_df(temp,~as.data.frame(x=.x), .id="delete") %>%
  select(c(2:5,10:11)) %>%
  rename(Vessel_name = "VESSEL.NAME", USCG = "CG.NUM",AFA = "AFA.PERMIT") 
##### FINAL CATCHER VESSEL POLLOCK DATAFRAME 
Catcher_Vessel_Pollock = rbind(Catcher_Vessel_Pollock1, Catcher_Vessel_Pollock2)  %>%
  mutate(PERMIT = NA)
  
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

norpac <- norpac %>%
   mutate(PERMIT = as.character(PERMIT))

AMD80<-Amend_80 %>%
  left_join(Amend_80_owners, by =c("PERMIT", "Fishery", "Year")) %>% #so that I have species caught for each permit #
  left_join(norpac, by =c("PERMIT")) %>% #add on vessel names by the permit number
  rename(Vessel_name = Vessel_name.y #, ADFG = "ADFG_NUMBER", USCG="COAST_GUARD_NUMBER"
         ) %>%
  select(c(Fishery, Year, species, PERMIT, Vessel_name)) #two permit numbers: 37442, 3217 dont have vessel names. 
#AMD80 IS NOW A DATA SHEET WITH SPECIES CAUGHT FOR EACH PERMIT NUMBER AND HAS ASSOCIATED VESSEL NAME AND YEAR. 
#Unique set of permit, vessel and year that can be matched onto AMD80 to get species. Vessels are not permited for all the same species, amount of species permited ranges from 3-8.  
AMD80_unique <- unique(AMD80[c("Vessel_name","PERMIT","Fishery","Year")])

AMD80_unique <- AMD80_unique %>%
  mutate(ADFG = NA, USCG = NA, AFA = NA)


#Do I need these? Not sure if they give us any extra info... MAY BE JHELPFUL TO GORUP INTO OTHER? SEE WHAT POLLOCK GIVES THAN CIRCLE BACK
#Federal Fisheries Permits (FFP):This permit is required for US vessels used to fish for groundfish in the Gulf of Alaska or Bering Sea and Aleutian Islands. 
    #This permit is also required for vessels used to fish for any non-groundfish species and that are required to retain any bycatch of groundfish:

#Federal Processor Permits (FPP):Federal Processor Permits Issued to Shoreside Processors - Federal Processor Permits are also required for shoreside processors that receive and/or process groundfish harvested from Federal waters (or from any Federally-permitted vessels). 
  #FPPs are non-transferable, one-year permits, issued to owners on request and without charge. Also-- Federal Processor Permits Issued to Stationary Floating Processors - This permit is required for stationary floating processors (processing vessels that operate solely within Alaska State waters). 

  
#######################################################################################################
  #RBIND EACH FISHERY SO THEY CAN BE MATCHED WITH CWT AND NORPAC TO LINK ACTUAL RECOVERIES TO FISHERY
#######################################################################################################
permit_numbers<- rbind(Mothership,Inshore_Coop_Pollock,Catcher_Processor,Catcher_Vessel_Pollock,AMD80_unique) %>%
   group_by(Fishery,Vessel_name) %>%
   count(PERMIT) %>%
   ungroup() %>%
   mutate(Vessel_name = as.character(Vessel_name)) %>%
   dplyr::select(-c(4)) 

#try matching these ones seperately---- doesnt really help. 
# permit_numbers_PERMIT<- permit_numbers %>%
#   filter(!is.na(PERMIT)) %>%
# group_by(Fishery)%>%
#   count(PERMIT)%>%
#   ungroup() %>%
#  # mutate(Vessel_name = as.character(Vessel_name)) %>%
#   select(-c(n))
#  
# permit_numbers_ADFG<- permit_numbers %>%
#   group_by(Fishery) %>%
#   count(ADFG) %>% ##something may be weird here with the copy and paste or transformation?! It is reading things that are the same as unique and that is confusing. 
# #so I am going to filter the bering pollock ones that add up to 1, they are the duplicates
#   ungroup() %>%
#   mutate(
#     #Vessel_name = as.character(Vessel_name), 
#     PERMIT = ADFG) %>% 
#   filter(!Fishery=="Bering Pollock" | !n==1)
#permit_numbers_USCG_ADFG <- unique(permit_numbers_USCG_ADFG[c("Fishery", "Vessel_name", "ADFG","USCG")]) #something may be weird here with the copy and paste or transformation?! It is reading things that are the same as unique and that is confusing. 

#######################################################################################################
#LOAD CWT AND NORPAC, START TO MATCH AND FIGURE OUT WHAT FLEETS ARE ASSOCIATED WITH SPECIFIC RECOVERIES 
#######################################################################################################
#LOAD CWT
cwt <- read.csv("data/HighSeasCWT2006a_for_Jordan.csv" ) %>%
  separate(recovery_date, into = c("Year", "restofdate"), sep = 4) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!Year == 2018) %>%
  rename(PERMIT = "Vessel_or_Plant_code") %>% #Vessel code is the equivalent to PERMIT in Norpac database. A lot of boat names in CWT are some sort of permit #, and match with permit number. Ex. A0825
#  filter(!is.na(Cruise_no) & !is.na(PERMIT)) %>% #remove entries that dont have any info 
  mutate(VESSEL_CODE = as.character(PERMIT)) #some CWT recoveries have the Vessel_Code classified as PERMIT #, Will use that in to match to norpac too

#LOAD NORPAC
#norpac and CWT can match on vessel names and some permit #'s. NORPAC has mulitple permit #'s with the same boat name and different boat lengths, indicating they are seperate boats. 
#cwt and the permit # database I pulled from online can match by more 
norpac <- read.csv("data/Norpac Vessel Table_2020.csv") %>% 
  rename(Vessel_name = "NAME",ADFG = "ADFG_NUMBER", USCG="COAST_GUARD_NUMBER") %>%
  mutate(PERMIT = as.character(PERMIT),USCG = as.character(USCG)) %>%
  filter(!LENGTH < 25) %>% #filter out boats smaller than 25 because that seems to be the threshold for matching (no diff between # of matches between 20 and 25), and it is unlikely boats smaller than that are trawling. 
  select(Vessel_name, PERMIT,ADFG, USCG, LENGTH) 

norpac_ADFG <- norpac %>%
  select(Vessel_name, LENGTH,ADFG) %>%
  mutate(PERMIT = ADFG)

norpac_USCG <- norpac %>%
  select(Vessel_name, LENGTH,USCG) %>%
  mutate(PERMIT = USCG)

norpac_vessel_code <- read.csv("data/Norpac Vessel Table_2020.csv") %>% 
  rename(Vessel_name = "NAME",ADFG = "ADFG_NUMBER", USCG="COAST_GUARD_NUMBER") %>%
  mutate(PERMIT = as.character(PERMIT),USCG = as.character(USCG)) %>%
  filter(!LENGTH < 25) %>% #filter out boats smaller than 25 because that seems to be the threshold for matching (no diff between # of matches between 20 and 25), and it is unlikely boats smaller than that are trawling. 
  select(Vessel_name, VESSEL_CODE,LENGTH) 

#what are the unique combinations of vessel name and permit number that I need to match?
unique_cwt <- filter(cwt, !PERMIT =="")
unique_cwt<-unique(unique_cwt[c("Vessel_name", "PERMIT")]) 
unique_cwt<-mutate(unique_cwt, CWT =1)

#START MATCHING. 
  #FIRST MATCH NORPAC AND CWT BY PERMIT #. BUT, THE PERMIT NUMBERS IN THE CWT DATABASE ARENT ACTUALLY ALL PERMIT NUMBERS, SOME ARE VESSEL CODES. SO THAT IS WHY THERE ARE TWO SERIES OF MATCHES
match<- cwt %>%
        filter(!PERMIT =="") %>% #avoids duplicate matches 
        left_join(norpac, by=c("PERMIT")) %>%  #if you match just on permit and not vessel name you get more matches and it clarifies some of the vessel names in the CWT data set. 
        left_join(norpac_vessel_code, by=c("VESSEL_CODE")) %>% # some of the permit numbers in CWT data are also vessel codes, now match by vessel code. 
        left_join(norpac_ADFG, by=c("PERMIT")) %>% #joining on USCG doesnt add anything extra
  mutate(Vessel_name.x = as.character(Vessel_name.x),Vessel_name.x.x = as.character(Vessel_name.x.x), Vessel_name.y.y=as.character(Vessel_name.y.y), Vessel_name.y = as.character(Vessel_name.y)
         ) %>% #use all the matches to fill in Vessel name as a new column so vessel names are uniform. 
  mutate(Vessel_name = as.character(case_when(is.na(Vessel_name.y) ~ Vessel_name.x.x,
                                               is.na(Vessel_name.x.x) | is.na(Vessel_name.y.y) ~ Vessel_name.y,
                                               is.na(Vessel_name.y.y)| is.na(Vessel_name.x.x) | is.na(Vessel_name.y) ~ Vessel_name.x,
                                                  TRUE ~ Vessel_name.y))) %>%
  mutate(Vessel_name = as.character(case_when(is.na(Vessel_name) ~ Vessel_name.x, # a few weird ones didnt get transfered- not sure what to do
                                              TRUE ~ Vessel_name))) %>%
  mutate(LENGTH = as.character(case_when(is.na(LENGTH.x) ~ LENGTH.y,
                                              TRUE ~ LENGTH.x))) %>% #also combine lengths 
  select(-c(Vessel_name.y,Vessel_name.x,
            Vessel_name.x.x,Vessel_name.y.y,LENGTH.x,LENGTH.y)) # Now, Vessel names, all permit #'s, and vessel codes should be better organized. Try to match on permit #'s I got online to determine what each permit # means and link the fisheries. 

#Now, match Permit # dataframes so fleets can be matched with recoveries  
#match on permit numbers rather than vessel name

fishery_match<- match %>%
  #left_join(permit_numbers_PERMIT, by = c("PERMIT")) %>%  
  #left_join(permit_numbers_ADFG, by = c("PERMIT")) %>%
  left_join(permit_numbers, by = c("Vessel_name")) 

#fishery_match<- match %>%
#  left_join(permit_numbers, by = c("Vessel_name")) 
#######################################################################################################
#TRYING TO TROUBLE SHOOT WHY CERTAIN BOATS DONT MATCH EITHER THE NORPAC OR PERMIT DATA FROM ONLINE 
#######################################################################################################
#PROBLEM: is that permit info starts in 2005 - CWT starts in 1989, so there are a whole lot of recoveries we are missing. 
#PROBLEM: 40% of the CWT Recoveries are not matching to a fishery (from online).
  #Is it because I didnt get all the permit #'s from the online database? 
  #Could also be the text on Vessel names, different capitilization etc. --> Likely the case for some because I can see them in the online AK database. But some are not there, indicating I missed them when downloading. 

#troubleshoot things that didnt match between Norpac and CWT
#about 40 Recoveries that are not in NorPac, Unsure what they are from. 
  #All have NA for Vessel name.  
  na_match <- match %>% filter(is.na(Vessel_name) | Vessel_name == "") 
  unique(na_match[c("PERMIT")]) #list of permit numbers that dont match, there are 10 unique permit numbers. 
  
  #Now troubleshoot when trying to match fishery to recovery
#40% of these dont match. is it because I didnt get all the permit #'s from the online database? 
  # Could also be the text on Vessel names, different capitilization etc. 
  na_fishery_match <- filter(fishery_match, is.na(Fishery))


  #check to get unique names and sizes for boats that dont match 
  na_unique <-  unique(na_fishery_match[c("PERMIT.x")])
  
  
#####CHECK TO SEE IF FISHERY NA'S THAT DONT MATCH TO ANYTHING ARE IN NORPAC OR THE ONLINE PERMITS THAT I DOWNLOADED
  #these 76 vessels do not match with the any category (AFA, USCG, ADFG, PERMIT) in the online permit downloads. 
  na_unique_test <- na_unique %>%
    mutate(from_NA_data_set = 1) %>%
    left_join(norpac) %>%
    filter(!is.na(Vessel_name)) #PERMIT #'s that start with A do not match. 
  
  na_unique_permit_numbers <- na_unique %>%
    mutate(from_NA_data_set = 1) %>%
    left_join(permit_numbers)  

  
  #I have 76 permit numbers that account for half of the dataset and IDK how to match them. 
