library(here)
library(tidyverse)

#goal is to match the CWT recoveries to boat names and permit names to figure out what the boats were fishing for when they recovered a fish in the CWT Trawl dataset. 


#LEFT OFF: NEXT NEED TO LEFT_JOIN THE "JOIN" DATAFRAME WITH CWT DATAFRAME AND SEE HOW MANY BOATS MATCH AND WHAT IT CAN TELL YOU ABOUT THE FLEETS. IF A LOT MATCH- YAY. IF NOT MANY MATCH THEN NEED TO TALK WITH OLE-ASK ABOUT AMENDMENT 80. ALSO NEED TO GO BACK ON WEBSITE AND SEE IF
    #THERE ARE OTHER PERMIT #S TO PULL, ALSO MAKE SURE FORMATTING WORKS, THAT COULD BE THE REASON SOME STUFF DOESNT MATCH IN. 
    #SHOULD ALEUTIAN POLLOCK BE IN NORPAC??


##########################################################################################
#Load Permit Number and Vessel Name Documents for Each Fishery
##########################################################################################
### ALEUTIAN POLLOCK 
Aleut_pollock<- read.csv("data/ak_permits/ai-pollock-2015-2005/Aleutian_pollock_05_15.csv") %>%
  rename(AFA = "AFA.Permit.Number.") %>%
  separate(ADF.G...USCG., into = c("ADFG","temp", "USCG")) %>%
  mutate(USCG = case_when(USCG == "" ~ temp,
                          TRUE ~ USCG)) %>%
  select(c("Fishery", "Year","Vessel_name", "AFA", "ADFG", "USCG"))%>%
  mutate(PERMIT = NA)

###BERING POLLOCK
  #2000-2004 [copied from PDF]
BS_pollock1<- read.csv("data/ak_permits/afa-catcher-vessels-bering_pollock_2017-2000/afa_bering_sea_pollock_00_04.csv")%>%
  select(c(1:6)) %>%
  mutate(Fishery = "Bering Pollock") %>%
  rename(Vessel_name="Vessel.Name.", ADFG = "ADFG.", USCG = "USCG.", AFA = "AFA.")
  #2005-2017 [csv's were provided]
temp=list() 
for(y in 5:17) {
  temp[[y]] = read.csv(paste0("data/ak_permits/afa-catcher-vessels-bering_pollock_2017-2000/",y,"afa_list_cv.csv"))
  temp[[y]] = temp[[y]] %>% 
    dplyr::select(c(1:8)) %>%
    mutate(Year = y) %>%
    mutate(Fishery = "Bering Pollock")
}
BS_pollock2 <- map_df(temp,~as.data.frame(x=.x), .id="delete") %>%
  select(c(2:5,10:11)) %>%
  rename(Vessel_name = "VESSEL.NAME", USCG = "CG.NUM",AFA = "AFA.PERMIT") 
##### FINAL POLLOCK DATAFRAME 
BS_pollock = rbind(BS_pollock1, BS_pollock2)  %>%
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
  rename(Vessel_name = Vessel_name.y, ADFG = "ADFG_NUMBER", USCG="COAST_GUARD_NUMBER") %>%
  select(c(Fishery, Year, species, PERMIT, Vessel_name)) #two permit numbers: 37442, 3217 dont have vessel names. 
#AMD80 IS NOW A DATA SHEET WITH SPECIES CAUGHT FOR EACH PERMIT NUMBER AND HAS ASSOCIATED VESSEL NAME AND YEAR. 
#Unique set of permit, vessel and year that can be matched onto AMD80 to get species. Vessels are not permited for all the same species, amount of species permited ranges from 3-8.  
AMD80_unique <- unique(AMD80[c("Vessel_name","PERMIT","Fishery","Year")])

AMD80_unique <- AMD80_unique %>%
  mutate(ADFG = NA, USCG = NA, AFA = NA)
  
#######################################################################################################
  #RBIND EACH FISHERY SO THEY CAN BE MATCHED WITH CWT AND NORPAC TO LINK ACTUAL RECOVERIES TO FISHERY
#######################################################################################################
permit_numbers<- rbind(Aleut_pollock,BS_pollock,AMD80_unique)

#try matching these ones seperately
permit_numbers_PERMIT<- permit_numbers %>%
group_by(Fishery, Vessel_name)%>%
  filter(!is.na(PERMIT)) %>%
  count(PERMIT)%>%
  select(-c(n))
 
permit_numbers_USCG_ADFG<- permit_numbers %>%
  group_by(Fishery, Vessel_name, USCG )%>%
  filter(!is.na(USCG)) %>%
  count(ADFG) %>% ##something may be weird here with the copy and paste or transformation?! It is reading things that are the same as unique and that is confusing. 
#so I am going to filter the bering pollock ones that add up to 1, they are the duplicates
filter(!Fishery=="Bering Pollock" | !n==1)
#permit_numbers_USCG_ADFG <- unique(permit_numbers_USCG_ADFG[c("Fishery", "Vessel_name", "ADFG","USCG")]) #something may be weird here with the copy and paste or transformation?! It is reading things that are the same as unique and that is confusing. 

cwt <- read_csv("data/HighSeasCWT2006a_for_Jordan.csv") %>%
  separate(recovery_date, into = c("Year", "restofdate"), sep = 4) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!Year == 2018)

#norpac and CWT can only match on vessel names, which is not very helpful. Bc there are mulitple permit #'s with the same boat name and different boat lengths, indicating they are seperate boats. 
#cwt and the permit # database I pulled from online can match by more 
norpac <- read_csv("data/Norpac Vessel Table_2020.csv") %>%
  rename(Vessel_name = "NAME",ADFG = "ADFG_NUMBER", USCG="COAST_GUARD_NUMBER") %>%
  mutate(PERMIT = as.character(PERMIT),USCG = as.character(USCG)) %>%
  filter(!LENGTH < 25) %>% #filter out boats smaller than 25 becausethat seems to be the threshold for matching (no diff between # of matches between 20 and 25), and it is unlikely boats smaller than that are trawling. 
  select(Vessel_name, PERMIT,ADFG, USCG, LENGTH) 

#join <- cwt %>%
#  left_join(norpac, by = c("Vessel_name"))%>%
 # left_join(permit_numbers, by = c("Vessel_name", "Year")) # I cant match by permit unless I also add Norpac in. But there are a lot of duplicate boat names here so it is harder to match. 
join <- norpac %>%
  #left_join(permit_numbers_ADFG, by = c("Vessel_name", "ADFG")) %>%
  left_join(permit_numbers_USCG_ADFG, by = c("Vessel_name", "USCG","ADFG")) %>%  # no aleutian pollock fleets match on.. are those not included in NorPac or is something wrong with the format?? 
  left_join(permit_numbers_PERMIT, by = c("Vessel_name", "PERMIT")) %>%
  mutate(Fishery.x=as.character(Fishery.x), Fishery.y=as.character(Fishery.y)) %>%
  mutate(Fishery = case_when(Fishery.y == "AMD80" & is.na (Fishery.x)  ~ Fishery.y,
                             is.na(Fishery.y ) & Fishery.x == 'Bering Pollock' ~ Fishery.x, #amendment 80 may just mean that they were targeting pollock and then also could keep other fish. so if I get a "both" category than it doesnt really matter, it should just be pollock
                             TRUE ~ "FIX ME")) %>%
  filter(!Fishery == "FIX ME") #check which ones you are removing but a lot of norpac fishingboats wobt match and that is OK. 

    # mutate(Fishery = case_when(Fishery.y == "AMD80" & is.na (Fishery.x)  ~ Fishery.y,
  #                            Fishery.y == "AMD80" & Fishery.x == "Bering Pollock" ~ "Pollock_and_non_pollock", #amendment 80 may just mean that they were targeting pollock and then also could keep other fish. so if I get a "both" category than it doesnt really matter, it should just be pollock
  #                            TRUE ~ "FIX ME"))
