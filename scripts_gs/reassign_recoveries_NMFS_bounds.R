library(tidyverse)
library(here)
#this file creates a new column in our lookup code file that has Alaska assignments for the NMFS stat areas and then saves it at the end--this only needs to be run one time and then the Weitkamp file is updated.
## was last run to update the NMFS stat area names in Alaska and should not need to be called again! 

#write.csv(dat_recovery, "dat_recovery.csv")
#reassigning spatial bounds 
codes = read_csv("data/recovery codes-wietkamp+shelton 12-2018 two PUSO.csv") %>%
 # select(c(1:10, 17)) %>%
 # filter(state.prov == 1) %>%
  mutate(recovery_location_code = location_code) %>%
  select(-c(Rec.area.Sullaway.Shelton.NMFSstat)) #remove previous versions for joining


location = read.csv("data/locations.txt") %>%
 select(c("location_code","rmis_latitude","rmis_longitude", "description")) %>%
 rename(recovery_location_code = location_code, recovery_description = description,
                latitude=rmis_latitude, longitude = rmis_longitude) %>%
 distinct(recovery_location_code, .keep_all = TRUE)   #took out duplicate recovery codes, keep.all = keeps the first row of values for each recovery code
 

#easier to assign codes that have lat/longs - start with those. Join on the location file to the codes to get the descriptions and lat/longs when applicable. 
temp_lat_long <- left_join(codes, location, by=c("recovery_location_code")) %>%
  filter(`state/prov` %in% c(1,7)) %>%
  #filter(str_detect(tolower(recovery_location_code), "^1")) %>% #Just pulls AK recoveries 
  filter(!is.na(latitude)) %>% 
  separate(recovery_description, into = c("sea", "sector", "hemi", "Bearing", "stat_area"), sep = ",") %>%
  separate(stat_area, into = c("area_info", "stat_area"), sep = 24) %>%
  mutate(longitude = case_when(longitude > 100 ~ longitude * -1,
                               TRUE ~ longitude)) %>% # (-)'s left off of some of the longitudes 
  filter(latitude > 50) %>%
  mutate(Rec.area.Sullaway.Shelton.NMFSstat = case_when(
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "NSEAK SSEAK" ~ "NSEAK SSEAK",
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "NSEAK" ~ "NSEAK",
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "SSEAK" ~ "SSEAK",
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "YAK" ~ "NE.GOA", #Ole already assigned these, so they will just carry over
    sector== " SECTOR 4"~ "BER",
    longitude > -140 & latitude > 55.5   ~  "NSEAK", #"X650",
    longitude < -140 & longitude > -147 | longitude == -140 ~ "NE.GOA", #"X640",
    longitude < -147 & longitude > -154 | longitude == -147 ~ "NW.GOA", #"X630",
    longitude < -154 & longitude > -159 | longitude == -154 ~ "W.APEN", #"X620",
    latitude > 59 & longitude < -160 ~ "BER",
    longitude < -159 & longitude > -170 | longitude == -159 & latitude < 55 ~ "E.APEN",  #"X610",
    longitude == -172 & latitude < 55 ~ "E.APEN", #an odd ball on the south side of aluetian islands in sector 3
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "ALEUT" ~ "ALEUT",
    latitude < 55.5 & longitude > -140 ~ Rec.area.Sullaway.Shelton.YAK_SOR_NCA,
         TRUE ~ Rec.area.Sullaway.Shelton.YAK_SOR_NCA)) %>%
  mutate(Rec.area.Sullaway.Shelton.NMFSstat = case_when(Rec.area.Sullaway.Shelton.NMFSstat == "BER" & sector == " Region 1" ~ "NE.GOA",
                                                        Bearing == "District 186" ~ "NE.GOA",
                                                        Rec.area.Sullaway.Shelton.NMFSstat == "YAK" & longitude > -137 & latitude < 58  ~ "NSEAK", #SOME OF THE YAK'S ARENT ASSIGNING CORRECLTY THIS WILL FIX IT. 
                                                        Rec.area.Sullaway.Shelton.NMFSstat %in% c("APEN", "HSEA") & latitude < 52 & longitude < -170 ~ "ALEUT",
                                                        Rec.area.Sullaway.Shelton.NMFSstat == "APEN" & latitude > 57 ~ "BER",
                                                        Rec.area.Sullaway.Shelton.NMFSstat == "BER" & latitude < 54 & longitude < -170 ~ "ALEUT",
                                                        Rec.area.Sullaway.Shelton.NMFSstat == "BER" & latitude < 55 ~ "E.APEN",
                                                        Rec.area.Sullaway.Shelton.NMFSstat == "HSEA" & latitude == 55 ~ "SSEAK",
                                                        Rec.area.Sullaway.Shelton.NMFSstat == "HSEA" & longitude > -160 & longitude < -155 ~ "W.APEN",
                                                        Rec.area.Sullaway.Shelton.NMFSstat == "NSEAK" & latitude == 56 ~ "SSEAK",
                                                        Rec.area.Sullaway.Shelton.NMFSstat == "YAK" & longitude > - 136.5 ~ "NSEAK",
                                                        Rec.area.Sullaway.Shelton.NMFSstat == "BER" & longitude > -140 ~ "NE.GOA",
                                                        recovery_location_code %in% c("1M112MB SE10720", "1M1NW") ~ "SSEAK",
                                                        hemi %in% c(" WRANGELL", " PETERSBURG") ~ "SSEAK",
                                                        Rec.area.Sullaway.Shelton.NMFSstat == "NSEAK" & longitude < -137.69 ~ "NE.GOA",
                                                        Rec.area.Sullaway.Shelton.NMFSstat =="NE.GOA" & longitude > -135.6990 ~ "NSEAK",
                                                        TRUE ~ Rec.area.Sullaway.Shelton.NMFSstat))#TWO Yak and one SSEAK is out of line - will come back to those later. 

test <- temp_lat_long %>%
     filter(Rec.area.Sullaway.Shelton.NMFSstat == "NE.GOA")
#plot and check to see if assignment worked
base <- world <- map_data("world") %>%  subset(region %in% c("USA", "Canada")) %>%
    filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
    filter(!long > -115) %>%
    filter(!lat < 34) %>%
    filter(!lat > 62) %>%
    filter(!long < -173) %>% 
    ggplot( ) +
    geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    coord_fixed(1.3)

base +  geom_point(data = temp_lat_long, mapping = aes(x = longitude, y = latitude, color = Rec.area.Sullaway.Shelton.NMFSstat)) +
    theme_bw()
    
#seperate process for codes without a latitude 
temp_NA <- left_join(codes, location, by=c("recovery_location_code")) %>%
  filter(`state/prov` %in% c(1,7)) %>%
  filter(is.na(latitude)) %>% 
  mutate(recovery_description = as.character(recovery_description)) %>%
  mutate(recovery_description = case_when(is.na(recovery_description) ~ description,
                                           TRUE ~ recovery_description)) %>%
  separate(recovery_description, into = c("sea", "sector", "hemi", "Bearing", "stat_area"), sep = ",", remove = FALSE) %>%
  separate(stat_area, into = c("area_info", "stat_area"), sep = 24, remove = FALSE) %>%
  mutate(Rec.area.Sullaway.Shelton.NMFSstat = case_when(
    name == "HIGH SEAS 3 N" ~ "NE.GOA NW.GOA W.APEN E.APEN BER",  
    name == "HIGH SEAS 3 N W" ~ "NE.GOA NW.GOA W.APEN E.APEN BER",
    sector== " SECTOR 4"~ "BER",
    stat_area == "610" ~ "E.APEN",#"X610",
    stat_area == "620" ~ "W.APEN", #"X620",
    stat_area == "621" ~ "W.APEN", #621 = typo? put it into KOD bc that is 620 
    stat_area == "630" ~ "NW.GOA", #"X630", 
    stat_area == "640" ~ "NE.GOA", #"X640",
    stat_area == "649" ~ "NE.GOA", #north part of PWS on the YAK side. 
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "YAK" ~ "NE.GOA",  #"X640", Yakutat assignments will stay the same, keeping the cut past -140. 
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "NSEAK" ~ "NSEAK", #"X650", these assignments didnt change
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "SSEAK" ~ "SSEAK", #These assignments didnt change 
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "CISS" ~ "NW.GOA", #CISS = Cook Inlet
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "BER" ~ "BER", #These assignments didnt change 
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "NSEAK SSEAK" ~ "NSEAK SSEAK",
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "APEN" ~ "E.APEN",
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "PWS CISS KOD AKPEN BER" ~ "NW.GOA W.APEN E.APEN BER",
      #PWS/YAK AREA
    Bearing %in% c(" District 225" , " District 222",  " District 223", " District 224", " District 226", " District 227", " District 229") ~ "NW.GOA", #"X630",
    recovery_description == "Alaska marine, Region 2, Quadrant PW" ~ "NE.GOA",  
      ## ^^ there are 186 with this vague code: "AK M 2 PW" assigned to YAK. all from net/siene fishery. 
    Bearing %in% c(" District 221", " District 212", " District 228")  ~ "NE.GOA", #"X640",
      #KOD AREA I looked over all the descriptions and districts, they are all well within the 630 area, thus we can broadly assign them into it without parcing. 
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "KOD" ~ "NW.GOA",  #"X630"
      #APEN - already assigned using stat area
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "PWS" ~ "NW.GOA",  
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "ALEUT" ~ "ALEUT",  
    Rec.area.Sullaway.Shelton.YAK_SOR_NCA == "HSEA" ~ "HSEA",  
    sector== " SECTOR 5"~ "BER",
    hemi %in% c(" JUNEAU") ~ "NSEAK",    
    hemi %in% c(" WRANGELL" ) ~ "SSEAK", 
    recovery_location_code == "1M" ~ "AK",
    TRUE ~ "FIX_ME")) 


test <- temp_NA %>% filter(Rec.area.Sullaway.Shelton.NMFSstat == "FIX_ME" )
 #BIND AND then create your NEW column in code assignment. 
temp1 <-temp_lat_long %>% 
   select(recovery_location_code, Rec.area.Sullaway.Shelton.NMFSstat)
temp2 <- temp_NA %>%
   select(recovery_location_code, Rec.area.Sullaway.Shelton.NMFSstat)

#COMBINE ALL CODES AND REMOVE DUPLICATES 
codes_stat_area<-rbind(temp1, temp2) %>% 
  unite(temp, c(recovery_location_code, Rec.area.Sullaway.Shelton.NMFSstat)) 
codes_stat_area<-  as.data.frame(unique(codes_stat_area$temp)) %>%
  rename(temp= "unique(codes_stat_area$temp)") %>%
  separate(temp, into = c("recovery_location_code", "Rec.area.Sullaway.Shelton.NMFSstat"), sep = "_") 

#CREATE NEW COLUMN AND BRING IN NON-ALASKA CODES 
join<- left_join(codes, codes_stat_area, by = c("recovery_location_code")) %>%
  mutate(Rec.area.Sullaway.Shelton.NMFSstat = case_when(is.na(Rec.area.Sullaway.Shelton.NMFSstat) ~ Rec.area.Sullaway.Shelton.YAK_SOR_NCA,
                                                        Rec.area.Sullaway.Shelton.NMFSstat == "FIX_ME" ~ Rec.area.Sullaway.Shelton.YAK_SOR_NCA, 
                                                        TRUE~Rec.area.Sullaway.Shelton.NMFSstat)) %>%
  select(-c(X1)) %>%
  filter(!is.na(Rec.area.Sullaway.Shelton.NMFSstat)) #I deal with assigning the NA's below. 

####################
#in script RMIS.R after codes area assigned, some still do not get assigned, this can be used to add in those assignments. 
na <- read.csv("data/missing_codes.csv")

# base <- world <- map_data("world") %>%  subset(region %in% c("USA", "Canada")) %>%
#   filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
#   filter(!long > -115) %>%
#   filter(!lat < 34) %>%
#   filter(!lat > 62) %>%
#   filter(!long < -173) %>% 
#   ggplot( ) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
#   coord_fixed(1.3)
# 
# base +  geom_point(data = bc_na, mapping = aes(x = longitude, y = latitude, color = Rec.area.Sullaway.Shelton.NMFSstat)) +
#   theme_bw()

ak_na<-na %>% 
  filter(str_detect(tolower(recovery_location_code), "^1")) %>% #Just pulls AK recoveries 
  separate(recovery_description, into = c("sea", "sector", "hemi", "Bearing", "stat_area"), sep = ",") %>%
  separate(stat_area, into = c("area_info", "stat_area"), sep = 24) %>%
  mutate(Rec.area.Sullaway.Shelton.NMFSstat = case_when(
    longitude > -155 & longitude < -145 ~ "NW.GOA",
    longitude < -170 ~ "ALEUT", 
    hemi == " HOMER" ~ "W.APEN",
    hemi %in% c(" GUSTAVUS"," ELFIN COVE", " JUNEAU", " SITKA" ) ~ "NSEAK",    
    hemi %in% c(" PETERSBURG", " WRANGELL" ) ~ "SSEAK",  
    TRUE ~ "FIX_ME")) 


bc_na<-na %>% 
  filter(str_detect(tolower(recovery_location_code), "^2")) %>% #Just pulls AK recoveries 
  separate(recovery_description, into = c("sea", "rest"), sep = ":") %>%
  separate(rest, into = c("delete", "area_code", "location", "location_cont", "another_code"), sep = " ") %>%
  separate(sea, into = c("stat_area", "delete"), sep = " ") %>%
  select(-c(delete)) %>%
  mutate(na_code = str_sub(recovery_location_code, start = 4, end = 5)) %>% #pulls out specific parts of the code useful for assignments 
  mutate(Rec.area.Sullaway.Shelton.NMFSstat = case_when(
    stat_area %in% c("NN", "NSPT") ~ "NBC", 
    stat_area %in% c("CNG", "CSPT") ~ "CBC", 
    stat_area %in% c("GSPTS", "GSRESSPT", "GSSPTC", "JSSSPT", "GSPTN") ~ "SGEO", 
    stat_area %in% c("NWTR") ~ "NWVI", 
    location %in% c("GILLAM") ~ "NWVI",
    na_code == "27" & latitude > 49.382966 ~ "NWVI",
    na_code == "27" & latitude < 49.382966 ~ "SWVI",
    stat_area %in% c("JFSPT") ~ "SWVI",   
    na_code %in% c("09", "25") ~ "NBC",
    na_code %in% c("50", "26") ~ "CBC",
    na_code == "24" ~ "SWVI",
    na_code %in% c("22","23", "61", "62","85") ~ "SGEO",
    na_code %in% c("01", "27") ~ "NWVI", # THERE ARE three codes left with no descriptions etc but the codes have similiar regions to the NWVI codes 27's and 25's so it is very likely that is where they belong. 
    na_code %in% c("08", "SK") ~ "NWVI", # rec loc code below, but for some reason that didnt work to assign it, so I did it using the NA_code, which is unique for both of these
    #recovery_location_code %in% c("2MNSKP004 000", "2MS08M302 000") ~ "NWVI", #These codes have no information, but they sort close to NWVI codes...so I put them there 
    TRUE ~ stat_area)) 

# base +  geom_point(data = bc_na, mapping = aes(x = longitude, y = latitude, color = Rec.area.Sullaway.Shelton.NMFSstat)) +
#   theme_bw()
ak_na<-ak_na %>%
  select(c(recovery_location_code, latitude, longitude, Rec.area.Sullaway.Shelton.NMFSstat ))
bc_na<-bc_na %>%
  select(c(recovery_location_code, latitude, longitude, Rec.area.Sullaway.Shelton.NMFSstat ))

na_assigned <- rbind(ak_na, bc_na) %>%
  mutate(id= NA, `state/prov` = str_sub(recovery_location_code, start = 1, end = 1)) %>%
  mutate(`F/M` = "M") %>%
  mutate(location_code = recovery_location_code) %>%
  mutate(location_type = NA, name= NA, psc_basin = NA, psc_region = NA, description = NA, rmpc_region = NA, rmpc_basin = NA, `Rec.area-W`= NA, Rec.area.Shelton = NA, Rec.area.Shelton2=NA, Rec.area.Shelton3=NA,Rec.area.Sullaway.Shelton=NA, Rec.area.Sullaway.Shelton.two.Oregon =NA,Rec.area.Sullaway.Shelton.SOR_NCA=NA, Rec.area.Sullaway.Shelton.YAK_SOR_NCA=NA) %>%
  select(-c(latitude, longitude))

#add these codes to the bottom of the master code file. 
all_codes<-rbind(join, na_assigned) %>%
  select(-c(id)) %>%
  mutate(id = row_number())   

#write.csv(all_codes, "data/recovery codes-wietkamp+shelton 12-2018 two PUSO.csv")
 