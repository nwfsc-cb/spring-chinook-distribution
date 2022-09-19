#THIS SCRIPT GRABS THE FTIDS ASSOCIATED WITH LOGBOOK MDT HAKE HAULS 
#MERGES THEM WITH THE LOG BOOK FTID HAKE HAULS TO COMPARE NUMBER OF DAYS FISHED.

#THEN USES THE DIFFERENCE BETWEEN DAYS TO ESTIMATE OFFSET BETWEEN THE LOGBOOK AND PACFIN DATA.
#MEAN OFFSET IS APPLIED TO EACH PORT. 
#NEW COLUMN CREATED CALLED TRIP_LENGTH_ALL. THIS CONDITIONALLY APPLIES TRIP LENGTHS USING DATA WE ARE MOST CONFIDENT IN FIRST (IE LOG BOOKS) THEN TRIPS WITH CONSECUTIVE LANDINGS, THEN ESTIMATES SUBTRAACTED FROM OFFSETS.
#FINAL DF IS SHORESIDE_MDT_TRIP_LENGTH

library(plyr)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)
library(gplots)
###################################################################################################################################################################################
                                                                                      #STEP 1 --> IDENTIFY THE HAKE TRIPS WITHIN PACFIN DATASET 
###################################################################################################################################################################################

#library(plyr)
#dat comes from ole's script: 

base.dir <- "/Users/ole.shelton/GitHub/"
data.dir <- paste0(base.dir,"Orca_Salmon_DATA/Effort info/Trawl-US/Shoreside_PacFin_Fish_Ticket")
write.dir <- paste0(base.dir,"spring-chinook-distribution/Processed Data/Effort Data")

setwd(base.dir)
source("spring-chinook-distribution/_R code for processing raw data/Process trawl fish tickets.R")

C <- dat %>% 
  filter(PACFIN_GEAR_CODE %in% c("GFT","MDT","OTW","RLT")) %>%
  dplyr::select(c(LANDING_YEAR, 
                  LANDING_MONTH, 
                  LANDING_DAY, 
                  VESSEL_ID, 
                  VESSEL_NUM, 
                  PORT_NAME, 
                  FTID,
                  AGENCY_CODE,
                  ORIG_PACFIN_CATCH_AREA_CODE,
                  PACFIN_CATCH_AREA_CODE,
                  PACFIN_CATCH_AREA_DESCRIPTION,
                  INPFC_AREA_TYPE_CODE,
                  NUM_OF_DAYS_FISHED, 
                  SPECIES_CODE_NAME, 
                  LANDED_WEIGHT_LBS)) %>%
  dplyr::group_by(LANDING_YEAR, 
                  LANDING_MONTH, 
                  LANDING_DAY, 
                  VESSEL_ID, 
                  VESSEL_NUM, 
                  PORT_NAME, 
                  FTID,
                  AGENCY_CODE,
                  ORIG_PACFIN_CATCH_AREA_CODE,
                  PACFIN_CATCH_AREA_CODE,
                  PACFIN_CATCH_AREA_DESCRIPTION,
                  INPFC_AREA_TYPE_CODE,
                  NUM_OF_DAYS_FISHED, 
                  SPECIES_CODE_NAME) %>% 
  dplyr::summarise(species_wt_mt = sum(LANDED_WEIGHT_LBS))

#remove huge df so R runs faster --> 
rm(dat)

#create new data frames that get total hake weight, from tows that caught hake, and total haul weight
temp_haul <- C %>%
  group_by(LANDING_YEAR, 
           LANDING_MONTH, 
           LANDING_DAY, 
           VESSEL_ID, 
           VESSEL_NUM, 
           PORT_NAME, 
           FTID,
    
           AGENCY_CODE,
           ORIG_PACFIN_CATCH_AREA_CODE,
           PACFIN_CATCH_AREA_CODE,
     
           PACFIN_CATCH_AREA_DESCRIPTION,
        
           INPFC_AREA_TYPE_CODE,
           NUM_OF_DAYS_FISHED 
         ) %>%
  dplyr::summarise(haul_wt_lbs = sum(species_wt_mt)) 

C$SPECIES_CODE_NAME <- as.character(C$SPECIES_CODE_NAME)

temp_hake <- C %>%  
  dplyr::filter(SPECIES_CODE_NAME %in% c("PACIFIC WHITING", "PACIFIC WHITING ", "PACIFIC WHITING (ANIMAL FOOD)", "PACIFIC WHITING (REDUCTION)", "WHITING, PACIFIC")) %>%
  dplyr::group_by(LANDING_YEAR, 
                  LANDING_MONTH, 
                  LANDING_DAY, 
                  VESSEL_ID, 
                  VESSEL_NUM, 
                  PORT_NAME, 
                  FTID,
                
                  AGENCY_CODE,
                  ORIG_PACFIN_CATCH_AREA_CODE,
                  PACFIN_CATCH_AREA_CODE,
     
                  PACFIN_CATCH_AREA_DESCRIPTION,
                 
                  INPFC_AREA_TYPE_CODE,
                  NUM_OF_DAYS_FISHED) %>%              #dont group by species code name because multiple hake categories may be on one FTID and that messes it up 
  dplyr::summarise(hake_wt_lbs = sum(species_wt_mt))   

#this merge creates a data frame of hake and haul weights, only for landings that caught hake 
hake <- merge(temp_haul, temp_hake, all.x=TRUE) 
# vhake <- left_join(temp_haul, temp_hake, by = c("LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY", "PORT_NAME", "VESSEL_NUM", "FTID", "NUM_OF_DAYS_FISHED")) 

dat.hake <- hake %>% 
  mutate(proportion_hake = hake_wt_lbs/haul_wt_lbs) %>% #find proportion of hake/trip            
  filter(!proportion_hake < 0.5)                     #remove hake proportion less than 50%, which means: 25,000 trips out of 400,000 are greater than 50% hake, this is 7% of trips that recorded any hake take  
  
dat.hake <- dat.hake %>% mutate(id = rownames(dat.hake)) %>%
unite("trip_id", c("LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY", "VESSEL_NUM"), remove = FALSE)
# write_csv(dat.hake, "hake_.csv") #this is before fishing days are added in though. 

ftid_reference <- dat.hake %>%
  dplyr::select(LANDING_YEAR, LANDING_MONTH, LANDING_DAY, VESSEL_NUM, VESSEL_ID, FTID, trip_id) %>%
  mutate(individ_ticket_id = rownames(dat.hake))
#write_csv(ftid_reference, "FTID_ref.csv")

###################################################################################################################################################################################
                                                                        #STEP 2: ESTIMATE MAXIMUM TRIP LENGTH POSSIBLE FROM PACFIN FISH TICKET DATA
###################################################################################################################################################################################

#estimate maximum trip length days based on vessel id number and time in between trips 
Fa <- dat.hake %>%   
 dplyr::select(-FTID) %>%
   filter(is.na(NUM_OF_DAYS_FISHED)) %>%
  unite("landing.date", c(LANDING_MONTH, LANDING_DAY, LANDING_YEAR), sep = "/", remove=FALSE) 

F1 <- Fa %>%   #this has just MDT trips with estimated fishing days 
  dplyr::mutate(date = as.Date(landing.date, format = "%m/%d/%Y")) %>%
  dplyr::group_by(date, VESSEL_NUM, PORT_NAME, trip_id) %>%
  dplyr::count(date) %>%
  dplyr::ungroup()

effort_est_pacfin <- F1 %>%
  dplyr::group_by(VESSEL_NUM) %>%
  dplyr::arrange(date, .by_group = TRUE) %>%                   #use arrange to make sure it does operations based on correct order/group
  dplyr::mutate(max_trip_length = difftime(date, lag(date), units = "days")) %>% #subtract time from previous row (this is done with lag) and then produce new column called max_trip_length in the unit days
  #the first trip by every vessel has an unknown trip length (NA)
  dplyr::mutate(max_trip_length = max_trip_length) %>% 
  dplyr::ungroup() %>%
  dplyr::filter(max_trip_length < 10)  %>% #filter out max trips greater than 10 days, no trips were greater than 10 days from data we actually have in dat.hake
  dplyr::filter(!is.na(max_trip_length)) %>%
  dplyr::filter(!max_trip_length < -1) %>%
  dplyr::mutate(max_trip_length= as.numeric(max_trip_length)) %>%
  dplyr::mutate(PORT_NAME= as.character(PORT_NAME)) %>%
  dplyr::select(-c("n", "VESSEL_NUM"))

###################################################################################################################################################################################
                                                                          #STEP 2 CONT. GET OFFSET ESTIMATES [X PORT]
###################################################################################################################################################################################
#MDT DATAFRAME COMES FROM: 
setwd(data.dir)
MDT = read.csv("LBK_MDT_98-19_Distinct.csv", stringsAsFactors = FALSE)

MDT<- MDT %>%
  mutate(departure_date = as.Date(DDATE, format = "%d-%b-%y")) %>%
  mutate(landing_date = as.Date(RDATE, format = "%d-%b-%y")) %>%
  mutate(FISH_DAYS = difftime(landing_date, departure_date, units = "days")) %>%
  mutate(FISH_DAYS = FISH_DAYS + 1) %>% #adding one so that 0's dont come in when they departed fished and landed all in the same day. 
  #also assumes that fishing occurs on days that they departed and days that they landed. 
  # mutate(AGENCY_CODE = AGID)%>%
  mutate(VESSEL_NUM = DRVID) %>%
  dplyr::select(-c(AGID, DTIME, RTIME, DDATE, RDATE, DRVID, FTID)) %>%
  separate(landing_date, into = c("LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY"), sep = "-", remove= FALSE) %>%
  unite("trip_id", c("LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY", "VESSEL_NUM"), remove = FALSE)

      #LOGBOOK DF IS THE MDT DATA THAT ARE ONLY ASSOCIATED WITH HAKE HAULS - FILTERING JOIN
      logbook <- semi_join(MDT, dat.hake, by = c("trip_id"))

#OFFSET ESTIMATE
logbook$LANDING_YEAR <- as.integer(logbook$LANDING_YEAR)
logbook$LANDING_MONTH <- as.integer(logbook$LANDING_MONTH)
logbook$LANDING_DAY <- as.integer(logbook$LANDING_DAY) 
     
     effort <- left_join(effort_est_pacfin, logbook, by = c("trip_id")) #do the join so the difference between known and estimated can be quantified, and match FTIDs with ports 
     
effort1a<- effort %>% 
  dplyr::select(-c("departure_date", "landing_date", "date")) %>%
  dplyr::filter(!is.na(FISH_DAYS)) %>%
  dplyr::filter(!is.na(max_trip_length)) %>%
  dplyr::filter(!max_trip_length < 0) %>%
  dplyr::mutate(mean_difference = max_trip_length - FISH_DAYS ) %>%
dplyr::filter(!mean_difference < 0) 

# library(plyr)

    #need to add port name back in. 
    #list of trip id's and the associated ports

trip_id_port_list <- dat.hake %>%
  dplyr::mutate(PORT_NAME = as.character(PORT_NAME)) %>%
  dplyr::group_by(trip_id, PORT_NAME) %>%
  dplyr::count(PORT_NAME) %>%
  dplyr::select(-n)

effort1<- left_join(effort1a, trip_id_port_list, by=c("trip_id", "PORT_NAME"))

effort2 <-ddply(effort1, c("PORT_NAME"), summarise, #included month but may need to think about this more if they differ in month- estimates may be off?
                               N    = length(mean_difference),
                               mean_diff = mean(mean_difference),
                               sd   = sd(mean_difference),
                               se_diff   = sd / sqrt(N),
                               N    = length(max_trip_length),
                               mean_est = mean(max_trip_length),
                               sd   = sd(max_trip_length),
                               se_est   = sd / sqrt(N),
                               N    = length(FISH_DAYS),
                               mean_logbook = mean(FISH_DAYS),
                               sd   = sd(FISH_DAYS),
                               se_logbook   = sd / sqrt(N))

#offset varies the most by port in coos, crescent, and eureka x year. 
#fairly constant in astoria and newport
#if you look at it by month you have more variability

#NOW WANT TO LEFT JOIN EFFORT DIFFERENCE SUMMARY INTO THE ESTIMATED FISH RANGE DATAFRAME AND SUBTRACT THE DIFF FROM ALL max trip lengths --> if I do this will have to compare performance to known locgbook fish days when that applies
#(or do we want to use logbook data when we can and just substitute when there is nothing?)
effort3<- effort2 %>%
  dplyr::select(PORT_NAME, mean_diff, se_diff) #mean diff column is the average offset between known days and estimate for that month and port 


df_all <- left_join(effort, effort3, by = c("PORT_NAME")) #joining effort DF- which has a column for pacfin estimates and logbook known trip length

df_all <- df_all %>%
  dplyr::mutate(new_est = ifelse(max_trip_length > 1, max_trip_length - mean_diff, 1)) %>% #SUBTRACT THE OFFSET CONDITIONALLY, SO WE DONT SUBTRACT OFFSETS WHEN WE KNOW THE MAX FISH TRIP LENGTH IS ALREADY 0. 
  dplyr::mutate(trip_length_logbook = FISH_DAYS) %>%
  dplyr::mutate(trip_length_estPacFin = max_trip_length) %>%
 dplyr::mutate(trip_length_all = case_when(FISH_DAYS > -1 ~ FISH_DAYS,       #create a new column that conditionally mutates- uses log book trip length when new_est is na, 
                                   new_est < 0 ~ max_trip_length,   #uses max trip length when new est is negative,
                                   TRUE ~ new_est)) %>%              #and uses new est if those two dont apply
  dplyr::select(-c(max_trip_length, FISH_DAYS, departure_date, landing_date, LANDING_DAY, LANDING_MONTH, LANDING_YEAR))

     write_csv(df_all, "trip_length_trip_id.csv")

trip_length <- df_all %>%
  dplyr::select(trip_id, trip_length_all)

dat.hake1 <- left_join(dat.hake, trip_length, by = "trip_id") #dat.hake1 has all info for hake and the matched trip length - about 10,000 trips where we could not estimate length

#look at where we do and do not have trip length data 
#test<- dat.hake1 %>% 
#  group_by(PACFIN_CATCH_AREA_DESCRIPTION, trip_length_all) %>%
# count(trip_length_all)

###################################################################################################################################################################################
                                                                                        #STEP 3: ASSIGN OUR REGIONS TO PACFIN DESCRIPTIONS 
###################################################################################################################################################################################

sb = read.csv("../spatial_bounds_gs.csv", stringsAsFactors = FALSE)

sb <- sb %>% filter(state %in% c("or", "wa", "ca", "bc"))

# test<- dat.hake1 %>% 
#  dplyr::group_by(PACFIN_CATCH_AREA_DESCRIPTION, ORIG_PACFIN_CATCH_AREA_CODE, PACFIN_CATCH_AREA_CODE)%>%
#  dplyr::count(PACFIN_CATCH_AREA_DESCRIPTION) %>%
#  unite("area_code", c("ORIG_PACFIN_CATCH_AREA_CODE", "PACFIN_CATCH_AREA_CODE"))
        # write.csv(test, "pac_fin_codes.csv")   

        #MANUALLY ASSIGN CODES TO THE DIFFERENT PACFIN DESCRIPTIONS AND THEN BRING IT BACK IN.
region_assignments = read.csv("pac_fin_codes.csv", stringsAsFactors = FALSE)

region_assignments1 <-  region_assignments %>% 
  unite("area_code", c("LANDING_YEAR", "PORT_NAME", "area_code")) 

dat.hakea<- dat.hake1 %>% 
  unite("area_code", c("LANDING_YEAR", "PORT_NAME", "ORIG_PACFIN_CATCH_AREA_CODE", "PACFIN_CATCH_AREA_CODE"), remove = FALSE) %>%
  unite("pacfin_code", c("ORIG_PACFIN_CATCH_AREA_CODE", "PACFIN_CATCH_AREA_CODE"))

dat.hake2 <- left_join(dat.hakea, region_assignments1, by = "area_code" ) #regions assigned

dat.hake3 <- dat.hake2 %>% 
  mutate(PACFIN_CATCH_AREA_DESCRIPTION = PACFIN_CATCH_AREA_DESCRIPTION.x) %>%
  dplyr::select(-c(PACFIN_CATCH_AREA_DESCRIPTION.x, PACFIN_CATCH_AREA_DESCRIPTION.y, X, n, id)) %>%
  separate(area_code, c("LANDING_YEAR", "area_code"), sep= 5)

####################################################################################################################################################################################
                                                  #STEP 3A: ASSIGN MDT LOGBOOK DATA TO REGION TO DE-MYSTIFY SOME AMBIGOUS CATEGORIES
###################################################################################################################################################################################
mdt_lat_long = read.csv("LBK_MDT_98-19_Distinct_InclLatLongs.csv", stringsAsFactors = FALSE)

mdt_lat_long1 <- mdt_lat_long %>%
  mutate(LANDING_DATE = as.Date(RDATE, format = "%d-%b-%Y")) %>%
  separate(LANDING_DATE, into = c("LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY"), sep = "-", remove= FALSE) %>%
  mutate(LANDING_MONTH = as.numeric(LANDING_MONTH))%>%
  unite("trip_id", c("LANDING_YEAR", "LANDING_MONTH", "LANDING_DAY", "DRVID"), remove = FALSE) %>%
  dplyr::select(trip_id, FTID,  SET_LAT, SET_LONG ) %>%
  mutate(SET_LONG = -1 * SET_LONG) %>%
  filter(!SET_LAT < 1)
# separate_rows(FTID, SET_LONG, SET_LAT)


#NO FISHING IN SGEO OR PUSO IN THIS DATA SET SO JUST CAN ASSIGN BY LAT AND LONG 
mdt_lat_long1$assign_region <- cut(mdt_lat_long1$SET_LAT, breaks=c(-Inf, 
                                                                   36, 
                                                                   37.1833, 
                                                                   38.958333,
                                                                   40.08333,
                                                                   42,
                                                                   42.6725,
                                                                   44.015,
                                                                   45.76666,
                                                                   46.6,
                                                                   Inf), labels=c("SCAL",
                                                                                  "MONT",
                                                                                  "SFB",
                                                                                  "MEN",
                                                                                  "NCA",
                                                                                  "SOR",
                                                                                  "COR",
                                                                                  "NOR",
                                                                                  "COL",
                                                                                  "WAC")) 

B<- left_join(dat.hake3, mdt_lat_long1, by = "trip_id")

B1 <- B %>% 
  mutate(assign_region.y = as.character(assign_region.y)) %>%
  mutate(assign_region.x = as.factor(assign_region.x))

B2 <- B1 %>% 
  dplyr::mutate(new_region = case_when(
    assign_region.y %in% c('SCAL', 'MONT', 'SFB', 'MEN', 'NCA', 'SOR', 'COR', 'NOR', 'COL', 'WAC') ~ as.character(assign_region.y),
    is.na(assign_region.y) ~ as.character(assign_region.x),
    TRUE ~ "oops")) %>%
  mutate(assign_region = new_region) %>%
  dplyr::select(-c(assign_region.y, assign_region.x, new_region)) 

####################################################################################################################################################################################

####################################################################################################################################################################################

#doing this to figure out what is going on with the georgia strait fishery. 
# dat.hake.sgeo <- dat.hake3 %>%
#  filter(assign_region =="SGEO") %>%
#  dplyr::group_by(PACFIN_CATCH_AREA_DESCRIPTION.x, LANDING_YEAR, PORT_NAME, area_code, assign_region)%>%
#  dplyr::count(PACFIN_CATCH_AREA_DESCRIPTION.x)
  
        #DO THIS TO CALCULATE PROPORTIONS TO THE MIXED LABELS 
# region.partiallist <- data.frame(assign_region=c("SCAL.MONT.SFB.MEN.NCA", "NCA.SOR", "SOR.NOR", "SOR.NOR.COL.WAC", "NOR.COL.WAC", "WAC.PUSO","SOR.NOR", "MONT.SFB.MEN.NCA.SOR.NOR.COL.WAC.PUSO"))
# dat.hake4 <- semi_join(dat.hake3, region.partiallist) #filter out the data that are already correctly assigned. 

# looking at distribution of vessel numbers for the un-identifiable descriptions, it looks like it is multiple vessel numbers that are included each year in the poor descriptions 
#  dat_vessel_num <- dat.hake4 %>% group_by(PACFIN_CATCH_AREA_DESCRIPTION, LANDING_YEAR, PORT_NAME, VESSEL_NUM) %>%
#    count(VESSEL_NUM)

#make table of pacfin description codes and years associated with each partial code
#test_codes_years<- B2 %>% 
#  dplyr::group_by(PACFIN_CATCH_AREA_DESCRIPTION, LANDING_YEAR, PORT_NAME, area_code, assign_region)%>%
#  dplyr::count(PACFIN_CATCH_AREA_DESCRIPTION) %>%
#   arrange(LANDING_YEAR, .by_group = TRUE)
#      View(test) <- unique(test_codes_years$assign_region)

# list_area_code<- test %>% 
#  dplyr::group_by(PACFIN_CATCH_AREA_DESCRIPTION, area_code, assign_region)%>%
#   dplyr::count(area_code) 
# write.csv(list_area_code, "pac_fin_codes2.csv") 

#NOTES:
 # 47 30' N TO 50 30' N; CANADIAN CATCH ONLY --> ALL PORTS IN WA, INCLUDING BELLINGHAM BAYM, EVERETT, SEATTLE, ANACORTES, BLAINE, WESTPORT -- 1984 - 1999 CURRENTLY ASSIGNED TO:
 
 # 36 00' N TO 40 30' N; PIEDRAS BLANCAS TO CAPE MENDOCINO --> 1981- 2006 [ALL HAVE 1 CT EXCEPT 2006 ASTORIA N = 189] DELETE ALL EXCEPT ASTORIA? CURRENTLY ASSIGNED TO: SCAL.MONT.SFB.MEN.NCA

 # UNKNOWN PSMFC AREA WITHIN PACIFIC COUNCIL REGION (W-O-C) --> 1981- 2013 ALL PORTS ARE IN OREGON. EXCEPT FOR 1 IN EVERETT.  CURRENTLY ASSIGNED TO: MONT.SFB.MEN.NCA.SOR.COR.NOR.COL.WAC

 # SOUTHERN PORTION OF AREA 3C (UNITED STATES ONLY) --> 1991 - 2016 --> ALL PORTS IN OREGON EXCEPT WESTPORT AND BLAINE (MINORITY). CURRENTLY ASSIGNED TO: MONT.SFB.MEN.NCA.SOR.COR.NOR.COL.WAC

# 47 20' N TO 220(T.) --> WE STILL DONT KNOW WHAT 220(T.) IS. CURRENTLY ASSIGNED TO: WAC

#	 GEORGIA STRAIT --> Looks like fishing occured 1983 -1997 all landings were in WA ports mostly seattle, everett, bellingham bay
 
#THIS DOESNT QUITE WORK, BUT IT SHOULD FILTER FOR JUST CODES THAT HAVE A "." IN THEM
# library(stringr)
#test<- B2 %>% 
#  filter(!assign_region %in% c("MONT",
 #        "SFB",
  #       "MEN",
   #      "NCA",
   #      "SOR",
    #     "NOR",
     #    "COL",
      #   "WAC",
       #  "PUSO",
        # "SGEO",
         #"SCAL"))
  
#filter(str_detect(assign_region, ".")) %>%
#  count(assign_region)

dat.hake_TEMP_TEST <- B2 %>% 
    dplyr::mutate(region_ct = case_when(
    assign_region == 'MONT.SFB.MEN.NCA' ~ "4",
    assign_region == 'NCA.SOR.COR' ~ "3",
    assign_region == 'SOR.COR.NOR.COL.WAC' ~ "5", 
    assign_region == 'NCA.SOR.COR.NOR.COL.WAC' ~"6", 
    assign_region == 'NOR.COL.WAC' ~ "3",
    assign_region == 'COL.WAC' ~ "2",   
    assign_region == 'SOR.COR.NOR' ~ "3",   
    assign_region == 'MONT.SFB.MEN.NCA.SOR.COR.NOR.COL.WAC' ~ "9",   
    TRUE ~ "1")) %>%
  mutate(assign_region = strsplit(assign_region, "[.]")) %>% 
  unnest(assign_region) 

#for some reason this doesnt work whenit is piped....
#ASSIGN PROPORTIONS TO EACH AREA CODE/REGION. 
        
G <-  dat.hake_TEMP_TEST %>% 
  filter(!area_code %in% c("1A_1A", "CL_CL")) %>%  #removing 1A_1A because there are 3 codes total through southern CA.  removed CL_CL bc there are two instances of fishing and span 4 regions 
  dplyr::mutate(proportion_effort = case_when(
                                              pacfin_code %in% c("UP_2B", "2A_2B", "2B_2B", "2E_2B", "2F_2B", "3A_2B", "3S_2B", "1C_2B") & assign_region == "SOR" ~ "0.101190476",    #proportion is related to the proportion of catch that should be assigned to that area based on the overlap in latitudes based on our regions and pacfin fishing area 
                                              pacfin_code %in% c("UP_2B", "2A_2B", "2B_2B", "2E_2B", "2F_2B", "3A_2B", "3S_2B", "1C_2B") & assign_region == "COR" ~ "0.797619048",
                                              pacfin_code %in% c("UP_2B", "2A_2B", "2B_2B", "2E_2B", "2F_2B", "3A_2B", "3S_2B", "1C_2B") & assign_region == "NOR" ~ "0.101190476",
                                              pacfin_code %in% c("1B_1B") & assign_region == "MONT" ~ "0.275116279",
                                              pacfin_code %in% c("1B_1B") & assign_region == "SFB" ~ "0.410930233",
                                              pacfin_code %in% c("1B_1B") & assign_region == "MEN" ~ "0.262790698",          
                                              pacfin_code %in% c("1B_1B") & assign_region == "NCA" ~ "0.051162791", 
                                              pacfin_code %in% c("EK_EK") & assign_region == "NCA" ~ "0.62962963",    #proportion is related to the proportion of catch that should be assigned to that area based on the overlap in latitudes based on our regions and pacfin fishing area 
                                              pacfin_code %in% c("EK_EK") & assign_region == "SOR" ~ "0.248148148",
                                              pacfin_code %in% c("EK_EK") & assign_region == "COR" ~ "0.122222222", 
                                              area_code %in% c("NEWPORT_2A_3S", "NEWPORT_2B_3S", "NEWPORT_2E_3S","NEWPORT_3A_3S", "NEWPORT_UP_UP",  
                                                              "ASTORIA_UP_UP") & assign_region == "NCA" ~ "0.228028504",
                                              area_code %in% c("NEWPORT_2A_3S", "NEWPORT_2B_3S", "NEWPORT_2E_3S","NEWPORT_3A_3S", "NEWPORT_UP_UP",
                                                               "ASTORIA_UP_UP") & assign_region == "SOR" ~ "0.079572447",
                                              area_code %in% c("NEWPORT_2A_3S", "NEWPORT_2B_3S", "NEWPORT_2E_3S","NEWPORT_3A_3S", "NEWPORT_UP_UP",
                                                               "ASTORIA_UP_UP") & assign_region == "COR" ~ "0.159738717",
                                              area_code %in% c("NEWPORT_2A_3S", "NEWPORT_2B_3S", "NEWPORT_2E_3S","NEWPORT_3A_3S", "NEWPORT_UP_UP",
                                                               "ASTORIA_UP_UP") & assign_region == "NOR" ~ "0.207957245",
                                              area_code %in% c("NEWPORT_2A_3S", "NEWPORT_2B_3S", "NEWPORT_2E_3S","NEWPORT_3A_3S", "NEWPORT_UP_UP",
                                                               "ASTORIA_UP_UP") & assign_region == "COL" ~ "0.102612827",
                                              area_code %in% c("NEWPORT_2A_3S", "NEWPORT_2B_3S", "NEWPORT_2E_3S","NEWPORT_3A_3S", "NEWPORT_UP_UP",
                                                               "ASTORIA_UP_UP") & assign_region == "WAC" ~ "0.222090261",
                                              area_code %in% c("CHARLESTON (COOS BAY)_UP_UP") & assign_region == "SOR" ~ "0.177907594",
                                              area_code %in% c("CHARLESTON (COOS BAY)_UP_UP") & assign_region == "COR" ~ "0.357142857",                 
                                              area_code %in% c("CHARLESTON (COOS BAY)_UP_UP") & assign_region == "NOR" ~ "0.464949549",
                                              area_code %in% c("ILWACO_VN_3S") & assign_region == "NOR" ~ "0.390412486",
                                              area_code %in% c("ILWACO_VN_3S") & assign_region == "COL" ~ "0.19264214",
                                              area_code %in% c("ILWACO_VN_3S") & assign_region == "WAC" ~ "0.416945373",
                                              area_code %in% c("WESTPORT_OC_3S") & assign_region == "COL" ~ "0.316020483",
                                              area_code %in% c("WESTPORT_OC_3S") & assign_region == "WAC" ~ "0.683979517", 
                                              area_code %in% c("NEAH BAY_VN_3S") & assign_region == "SOR" ~ "0.103076923",
                                              area_code %in% c("NEAH BAY_VN_3S") & assign_region == "COR" ~ "0.206923077",                                
                                              area_code %in% c("NEAH BAY_VN_3S") & assign_region == "NOR" ~ "0.269384615",
                                              area_code %in% c("NEAH BAY_VN_3S") & assign_region == "COL" ~ "0.132923077",
                                              area_code %in% c("NEAH BAY_VN_3S") & assign_region == "WAC" ~ "0.287692308",
                                              TRUE ~ "1")) #IF CODES ARE JUST SINGLE CODES THEY WILL NOT BE DIVIDED

#NOW MULTIPLY THE CATCH/THAT REGION BASED ON THE PROPORTION. 
G1 <- G %>% dplyr::mutate(total_haul_region =  as.numeric(proportion_effort) * haul_wt_lbs, 
                    total_hake_region = as.numeric(proportion_effort) * hake_wt_lbs,
                    total_effort = as.numeric(trip_length_all)/as.numeric(region_ct)) %>% #trip length divided by the regions to get a fraction of fishing days that we can allot to that region
  dplyr::select(-c(region_ct, proportion_effort, proportion_hake, NUM_OF_DAYS_FISHED, hake_wt_lbs, haul_wt_lbs))  %>%
  filter(!is.na(trip_length_all))

##########################################################################################################################################################################################################
                                                                                      #STEP 4: ASSIGN CHINOOK BYCATCH DATA TO REGION
##########################################################################################################################################################################################################

chinook = read.csv(paste0(base.dir,"Orca_Salmon_DATA/Hake Trawl/Shoreside_CWT/Chinook_CWT_HaulData_2019-03-25.csv"), stringsAsFactors = FALSE)

#USE THIS FOR REFERENCE TO UNDERSTAND WHAT TAG CODES ARE CORRESPONDED WITH WHICH FTID'S
#shoreside_chinook_tagcode_FTID <- chinook %>%
#  mutate(LANDING_YEAR = YEAR) %>%
#  mutate(VESSEL_ID = DRVID) %>%
#  mutate(PORT_NAME = Port) %>%
#  group_by(LANDING_YEAR, FTID, PORT_NAME, AVG_LAT, AVG_LONG, Tag.Code) %>%
#  dplyr::count(Tag.Code)

#DIDNT ASSIGN TO PUSO OR SGEO BC NO BYCATCH THERE, ALSO NO FISHING THERE BETWEEN 2011- 2016
chinook$assign_region <- cut(chinook$AVG_LAT, breaks=c(-Inf,
                                                       36, 
                                                       37.1833, 
                                                       38.958333,
                                                       40.08333,
                                                       42,
                                                       42.6725,
                                                       44.015,
                                                       45.76666,
                                                       46.6,
                                                       Inf), labels=c("SCAL",
                                                                      "MONT",
                                                                      "SFB",
                                                                      "MEN",
                                                                      "NCA",
                                                                      "SOR",
                                                                      "COR",
                                                                      "NOR",
                                                                      "COL",
                                                                      "WAC")) 
chinook_save<- chinook %>%
  separate("LANDING_DATE", c("LANDING_MONTH", "LANDING_DAY", "delete"), sep = "/") %>%
  dplyr::mutate(LANDING_YEAR = as.factor(YEAR)) %>%
  dplyr::mutate(VESSEL_ID = DRVID) %>%
  dplyr::mutate(PORT_NAME = Port) %>%
  dplyr::select(-c(delete, YEAR, Port))

chinook_save$LANDING_MONTH <- str_pad(chinook_save$LANDING_MONTH, width= 2, pad = "0", side="left") #add a

chinook_save$LANDING_DAY <- str_pad(chinook_save$LANDING_DAY, width= 2, pad = "0", side="left") #add a

##########################################################################################################################################################################################################
                                                                            #STEP 4 cont. CHECK REGION ASSIGNMENTS PLOT BYCATCH 
##########################################################################################################################################################################################################

# check to make sure they were correctly assigned 
#sb = read.csv("Maps_and_Charts_of_Regions/spatial_bounds_gs.csv", stringsAsFactors = FALSE)
library(mapdata)

world <- map_data("world")
north_america <- subset(world, region %in% c("USA", "Canada"))

north_am_filter <- north_america %>%
  filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
  filter(!long > -115) %>%
  filter(!lat < 35) %>%
  filter(!lat > 48.5) %>%
  filter(!long < -173) 

#base map
p_north_am <- ggplot(data = north_am_filter) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3)+
  theme_bw()
p_north_am

df<- sb 

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p<- p_north_am +
  #geom_segment(data = df, colour="orange", aes(x = as.numeric(line.start.lon), 
  #                                            y = as.numeric(line.start.lat), 
  #                                           xend = as.numeric(line.end.lon), 
  #                                          yend = as.numeric(line.end.lat))) +
  # geom_text(data=df, colour="darkgreen",aes(x=label_long1, y= label_lat1, label= region), size=2)+ #smaller labels for CA, OR, and WA so they fit
  #  geom_text(data=df, colour="darkgreen",aes(x=label_long2, y= label_lat2, label= region), size=3) +
  geom_point(data= chinook, aes(x= as.numeric(AVG_LONG), y = as.numeric(AVG_LAT), color = assign_region, size = n), alpha = 0.5) +
  facet_wrap(~LANDING_YEAR) +
  scale_color_brewer(palette="Spectral")

p

##########################################################################################################################################################################################################
                                                                                        #PLOT FISHING EFFORT IN A HEATMAP 
##########################################################################################################################################################################################################
#do year.month combo to merge in
temp.year <- as.character(c(1981:2016))
temp.month <- as.character(c(01:12))
year.month<- expand.grid(temp.year, temp.month) #fully factorial match between month and year 
year.month$Var2<- str_pad(year.month$Var2, width= 2, pad = "0", side="left") #add a zero before month 1-9 so that I can keep months in order
year.month <- unite(year.month, "year.month", c("Var1", "Var2"), sep = ".", remove= TRUE) #join month and year in its own dataframe 

#create year.month for DF 
G1$LANDING_MONTH<- str_pad(G1$LANDING_MONTH, width= 2, pad = "0", side="left") #add a zero before month 1-9 so that I can keep months in order
G1 <- G1 %>%
  tidyr::unite("year.month", c("LANDING_YEAR", "LANDING_MONTH"), sep = ".", remove= FALSE) #create year.month so this can be merged with fully factorial year.month and create blanks

#merge the two to get fully factorial year.months
merge <- merge(year.month, G1, all = TRUE)
merge$region.use <- recode(merge$assign_region, 'SGEO' = 1, 'WAC'= 2, 'PUSO'=3, 'COL'= 4, 'NOR'=5, 'COR' =6, 'SOR'=7, 'NCA'= 8, 'MEN'=9, 'SFB' =10, 'MONT' = 11, 'SCAL'=12)

#create an label for x axis with blanks so you can see years 
temp.lab = as.data.frame(c(1981:2016)) #create df with all years
temp <- as.data.frame(temp.lab[rep(1:nrow(temp.lab),1,each=12),]) #duplicate each row 12 times
temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`[duplicated(temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`)] <- " " #fill duplicates with a space
temp$label<-  temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`  #change column name and remove old column
temp <- temp %>% 
  dplyr::select(-c(`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`))
#recode regions so they are numbered with North = 1, so north is on top. 
temp.region <- c('SGEO', 'WAC', 'PUSO', 'COL','NOR', 'COR', 'SOR', 'NCA', 'MEN', 'SFB', 'MONT', 'SCAL')

count <- merge %>% group_by(year.month, region.use) %>%         #filtering out the NA's for effort decreases the available data by A LOT. (~1/2)
   summarise(sum_effort =sum(total_effort))

spread <- count %>%
   tidyr::spread(year.month, sum_effort) 

#set up matrix for heatmap 
#spread<- spread[1:11,] #these numbers will need to be edited based on how big columns/rows are 
region<- spread$region.use
hake.spread<- spread[,2:433] 
matrix <- data.matrix(hake.spread)
row.names(matrix) <- region
matrix[matrix < 0.1] <- NA #turn 0 to NA so that it doesnt get messed up in log function

matrix <- log(matrix) #log matrix values so that scale 

col.br= colorRampPalette(c("white", "blue","purple", "red"))
# year_list <- as.list()
# par(mar=c(10,4,4,2))
heatmap.2(matrix, Rowv=FALSE, Colv=FALSE, 
          xlab = "Year", ylab = "Region",
          scale="none", margins=c(4,6), 
          dendrogram="none", trace="none",
          col=col.br(20), #breaks=pairs.breaks,
          na.color = "black",
          key =TRUE,
          keysize = 1.5, #labRow = year_list,
          key.title = "log scale",
          adjCol = c(0.5,1.1),
          labCol = temp$label, #tell it to use dummy variable for X axis years
          labRow = temp.region, #use dummy variable for Y axis regions so they can be north to south 
          cexCol = 0.6,
          sepcolor = "black",
          main = "Shoreside Hake Estimated Fishing Effort\n(log boat days)", 
          density.info = ("none"),
          #( "bottom.margin", "left.margin", "top.margin", "right.margin" )
          key.par=list(mar=c(2,1, 1,0.1)))


##########################################################################################################################################################################################################
                                                                                        #PLOT EFFORT AND CHINOOK BYCATCH
##########################################################################################################################################################################################################

#get bycatch/unit effort
# summarize effort/region and summarize bycatch per region
F1<- G1 %>% 
  separate(LANDING_YEAR ,c("LANDING_YEAR", "delete"), sep = 4) %>%
  dplyr::select( -c(delete)) %>%
  group_by(LANDING_YEAR, assign_region) %>%
  summarise(effort_sum =sum(total_effort)) %>%
  unite("id", c("LANDING_YEAR", "assign_region"))

F2 <- chinook %>%
  group_by(YEAR) %>%
  count(assign_region) %>%
  unite("id", c("YEAR", "assign_region"))

F3 <- left_join(F1, F2, by = "id" )

F4 <- F3 %>%
  mutate(bycatch_effort_ratio = n / effort_sum ) %>%
  separate("id", c("LANDING_YEAR", "FISHING_REGION"), sep = "_")  %>%
  filter(!is.na(bycatch_effort_ratio))


ggplot(F4, aes(LANDING_YEAR, bycatch_effort_ratio, size = n)) +
  geom_point() +
  facet_grid(FISHING_REGION~., scales = "free_y") +
  theme_bw()
 

F4$FISHING_REGION <- ordered(F4$FISHING_REGION, levels = c("WAC", "COL", "NOR", "SOR", "NCA"))

ggplot(F4, aes(effort_sum, n)) +
  geom_smooth()

##########################################################################################################################################################################################################
                                                                                    #BYCATCH HEATMAP
##########################################################################################################################################################################################################
#year.month combo to merge in
temp.year <- as.character(c(2011:2016))
temp.month <- as.character(c(01:12))
year.month<- expand.grid(temp.year, temp.month) #fully factorial match between month and year 
year.month$Var2<- str_pad(year.month$Var2, width= 2, pad = "0", side="left") #add a zero before month 1-9 so that I can keep months in order
year.month <- unite(year.month, "year.month", c("Var1", "Var2"), sep = ".", remove= TRUE) #join month and year in its own dataframe 

#create year.month for DF 
chinook1 <- chinook1 %>%
  tidyr::unite("year.month", c("LANDING_YEAR", "LANDING_MONTH"), sep = ".", remove= FALSE) #create year.month so this can be merged with fully factorial year.month and create blanks

#merge the two to get fully factorial year.months
merge <- merge(year.month, chinook1, all = TRUE)
merge$region.use <- recode(merge$assign_region, 'SGEO' = 1, 'WAC'= 2, 'PUSO'=3, 'COL'= 4, 'NOR'=5, 'COR' =6, 'SOR'=7, 'NCA'= 8, 'MEN'=9, 'SFB' =10, 'MONT' = 11, 'SCAL'=12)

#create an label for x axis with blanks so you can see years 
temp.lab = as.data.frame(c(2011:2016)) #create df with all years
temp <- as.data.frame(temp.lab[rep(1:nrow(temp.lab),1,each=12),]) #duplicate each row 12 times
temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`[duplicated(temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`)] <- " " #fill duplicates with a space
temp$label<-  temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`  #change column name and remove old column
temp <- temp %>% 
  dplyr::select(-c(`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`))
#recode regions so they are numbered with North = 1, so north is on top. 
temp.region <- c('SGEO', 'WAC', 'PUSO', 'COL','NOR', 'COR', 'SOR', 'NCA', 'MEN', 'SFB', 'MONT', 'SCAL')

count <- merge %>% group_by(year.month, region.use) %>%       
  summarise(sum_fish =sum(n))

year.month <- c('2011.01', '2011.01', '2011.01', '2011.01')                     
region.use <- c(1, 3, 10, 11) 
sum_fish <- c(0, 0, 0, 0)

add <- cbind.data.frame(year.month, region.use, sum_fish)

count <- rbind.data.frame(count, add)

spread <- count %>%
  tidyr::spread(year.month, sum_fish) 

#set up matrix for heatmap 
spread<- spread[1:11,] #these numbers will need to be edited based on how big columns/rows are 
region<- spread$region.use
cwt.spread<- spread[,2:73] 
matrix <- data.matrix(cwt.spread)
#row.names(matrix) <- region
matrix[matrix < 0.1] <- NA #turn 0 to NA so that it doesnt get messed up in log function

matrix <- log(matrix) #log matrix values so that scale 

col.br= colorRampPalette(c("white", "blue","purple", "red"))
# year_list <- as.list()
# par(mar=c(10,4,4,2))
heatmap.2(matrix, Rowv=FALSE, Colv=FALSE, 
          xlab = "Year", ylab = "Region",
          scale="none", margins=c(4,6), 
          dendrogram="none", trace="none",
          col=col.br(20), #breaks=pairs.breaks,
          na.color = "black",
          key =TRUE,
          keysize = 1.5, #labRow = year_list,
          key.title = "log scale",
          adjCol = c(0.5,1.1),
          labCol = temp$label, #tell it to use dummy variable for X axis years
          labRow = temp.region, #use dummy variable for Y axis regions so they can be north to south 
          cexCol = 0.6,
          sepcolor = "black",
          main = "CWT Bycatch (log boat days)", 
          density.info = ("none"),
          #( "bottom.margin", "left.margin", "top.margin", "right.margin" )
          key.par=list(mar=c(2,1, 1,0.1)))

###################################################################################################################################################################################
                                                                                            #SAVE CSV'S OF TIDY DATA
###################################################################################################################################################################################
#shoreside fleet effort/trip
G2 <- G1 %>%
  mutate(FTID = FTID.x) %>% 
  mutate(fishing_region = assign_region) %>%
  mutate(hake_catch = total_hake_region) %>%
  mutate(total_catch = total_haul_region) %>%
  mutate(boat_days = total_effort) %>%
  separate(LANDING_YEAR, c("LANDING_YEAR", "delete"), sep = 4) %>%
  dplyr::select(-c(INPFC_AREA_TYPE_CODE, total_hake_region, total_haul_region, assign_region, area_code, AGENCY_CODE, trip_length_all, FTID.x, FTID.y, delete, year.month)) 

#EFFORT SUMMARY
G3 <- G2 %>%
  group_by(LANDING_YEAR, LANDING_MONTH, fishing_region) %>%
dplyr::summarise(sum_boat_days = sum(total_effort))  

setwd(write.dir)
write.csv(G2, "shoreside_hake_all.csv")
write.csv(G3 , "shoreside_hake_effort.csv")
write_csv(chinook_save, "shoreside_chinook_tagcode_FTID.csv")

###################################################################################################################################################################################
                               #GET OFFSET ESTIMATES [PORT X  MONTH] CAN BE USED TO DECIDE IF YOU WANT TO INCORPORATE MONTH. FOR NOW, WE WILL NOT
###################################################################################################################################################################################

effort_difference_summary2 <-ddply(effort1, c("PORT_NAME", "LANDING_MONTH.x"), summarise, #included month but may need to think about this more if they differ in month- estimates may be off?
                                   N    = length(mean_difference),
                                   mean_diff = mean(mean_difference),
                                   sd   = sd(mean_difference),
                                   se_diff   = sd / sqrt(N),
                                   N    = length(max_trip_length),
                                   mean_est = mean(max_trip_length),
                                   sd   = sd(max_trip_length),
                                   se_est   = sd / sqrt(N),
                                   N    = length(FISH_DAYS),
                                   mean_logbook = mean(FISH_DAYS),
                                   sd   = sd(FISH_DAYS),
                                   se_logbook   = sd / sqrt(N))

#month and port offset
ggplot(effort_difference_summary2, aes(x= mean_diff, y=LANDING_MONTH.x)) +
  geom_point(aes(), size = 2)+
  facet_grid(~PORT_NAME) +
  # scale_y_continuous(breaks=seq(0,12,2)) +
  theme_minimal() +
  #  scale_color_manual(values = c("mean_est" = "darkslategray3", "mean_logbook"= "darkslategray3", "mean_diff" = "#E69F00"))+
  geom_errorbarh(aes(xmax = mean_diff + se_diff, xmin = mean_diff - se_diff, height = .3)) +
  ggtitle("Mean Offset (Days) [PacFin Max Trip Length - Logbook Trip Length]")+
  labs(xlab = "Mean Offset (Days)")+
  theme(plot.title = element_text(hjust = 0.5))
#this plot shows that if we look at offset by month and port we see the lowest variability (averaged thorugh many years) and that pacfin fish tickets are usually
#overestimated by  1-2 days, with some ports being slightly more vairable than others. 