library(tidyverse)
library(here)

#this file creates a new column in our lookup code file that has Alaska assignments for the NMFS stat areas and then saves it at the end. 

#write.csv(dat_recovery, "dat_recovery.csv")
#reassigning spatial bounds 
codes = read_csv("data/recovery codes-wietkamp+shelton 12-2018 two PUSO.csv") %>%
 # select(c(1:10, 17)) %>%
 # filter(state.prov == 1) %>%
  mutate(recovery_location_code = location_code)

#bring in RMIS data to figure out what codes need to be re-assigned 
rmis = read.csv("data/rmis_release_recovery.csv", stringsAsFactors = FALSE) %>%
  filter(region %in% c("NSEAK", "YAK", "PWS", "KOD", "APEN", "ALEUT","BER")) 

rec_codes_reassign <- as.data.frame(unique(rmis$recovery_location_code)) %>%
  rename(recovery_location_code= "unique(rmis$recovery_location_code)" )

#### RAN "STEP 1" OF RMIS.R TO GET TO THE DAT_RECOVERY FILE ####
#assign recoveries to new STAT areas based on their lat and long. 
temp_lat_long <- semi_join(dat_recovery, rec_codes_reassign) %>% #temp should be only the alaska recovery codes + associated info 
  filter(!is.na(latitude)) %>% 
  separate(recovery_description, into = c("sea", "sector", "hemi", "Bearing", "stat_area"), sep = ",") %>%
  separate(stat_area, into = c("area_info", "stat_area"), sep = 24) %>%
  select(3:8,13,17,31:32, 34:38) %>%
  mutate(gs_nmfs_region = case_when(
    longitude > -140 & latitude > 55.5   ~  "NSEAK", #"X650",
    longitude < -140 & longitude > -147 | longitude == -140 ~ "YAK", #"X640",
    longitude < -147 & longitude > -154 | longitude == -147 ~ "PWS", #"X630",
    longitude < -154 & longitude > -159 | longitude == -154 ~ "KOD", #"X620",
    longitude < -159 & longitude > -170 | longitude == -159 ~ "APEN",  #"X610",
    longitude == -172 ~ "X541", #an odd ball on the south side of aluetian islands in sector 3
    sector== " SECTOR 4"~ "BER",
    latitude < 55.5   ~ "SSEAK",
        TRUE ~ "FIX_ME"))  
#df <- df %>%
#  filter(state =="ak")

#plot and check to see if assignment worked
# base <- world <- map_data("world") %>%  subset(region %in% c("USA", "Canada")) %>%
#     filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
#     filter(!long > -115) %>%
#     filter(!lat < 34) %>%
#     filter(!lat > 62) %>%
#     filter(!long < -173) 
#     ggplot(base) +
#     geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
#     coord_fixed(1.3)+
#     geom_point(data = temp_3, mapping = aes(x = longitude, y = latitude, color = gs_nmfs_region)) +
#     geom_segment(data = df, colour="orange", aes(x = as.numeric(line.start.lon), 
#                                                y = as.numeric(line.start.lat), 
#                                                xend = as.numeric(line.end.lon), 
#                                                yend = as.numeric(line.end.lat))) +
#     geom_text(data=df, colour="darkgreen",aes(x=as.numeric(label_long2), y=as.numeric(label_lat2), label= region), size=2) +
#     theme_bw()

    #LEFT OFF HERE
#ONCE YOU GET ASSIGNMENTS - MAKE AN RMD THAT SUMMARIZES EVERYTING - PLOT THEM IN BINS BY FISHERY (THIS WONT BE A MAP BC A LOT DONT HAVE LAT LONGS) AND MAKE A TABLE LIKE THE ONE YOU SENT TO JORDAN 
    #AND ADD THEM TO THE RECOVERY CODE DATA BASE AS A NEW COLUMN - DO THIS IN R THEN DOWNLOAD THE DATA SHEET ADD TO RMIS 
    #ONCE YOU UPDATE THE CODE DATABASE, GO BACK INTO RMIS.R RE-RUN THE SCRIPT, MAKE SURE EVERYRHING MAKES SENSE AND SAVE NEW RMIS CSV. 
  
#seperate process for codes with out a latitude 
 temp_NA <- semi_join(dat_recovery, rec_codes_reassign) %>% #temp should be only the alaska recovery codes + associated info 
  filter(is.na(latitude)) %>% 
  separate(recovery_description, into = c("sea", "sector", "hemi", "Bearing", "stat_area"), sep = ",", remove = FALSE) %>%
  separate(stat_area, into = c("area_info", "stat_area"), sep = 24, remove = FALSE) %>%
 select(c(3:7,13,17,33,33:44)) %>%
   left_join(codes) %>%
  mutate(gs_nmfs_region = case_when(
    recovery_location_name == "HIGH SEAS 3 N" ~ "GOA", # LOOK BACK INTO THESE **
    recovery_location_name == "HIGH SEAS 3 N W" ~ "GOA",
    sector== " SECTOR 4"~ "BER",
    stat_area == "610" ~ "APEN",  #"X610",
    stat_area == "620" ~ "KOD",  #"X620",
    stat_area == "621" ~ "X621",
    stat_area == "630" ~ "PWS",  #"X630", 
    stat_area == "640" ~ "YAK", #"X640",
    stat_area == "649" ~ "X649",
   # these should already be assigned as NSEAK- hemi %in% c(" HAINES", " ELFIN COVE", " SITKA", " SKAGWAY", " GUSTAVUS", " MEDVEJIE", " JUNEAU", " YAKUTAT") ~ "X650", #these are all sport fisheries 
    Rec.area.Sullaway.Shelton == "YAK" ~ "YAK",  #"X640", #Yakutat assignments will stay the same, keeping the cut past -140. 
    Rec.area.Sullaway.Shelton == "NSEAK" ~ "NSEAK", #  "X650", #These assignments didnt change
    Rec.area.Sullaway.Shelton == "SSEAK" ~ "SSEAK", #These assignments didnt change 
    #PWS/YAK AREA
    Bearing == " District 225" ~ "PWS", #"X630",
    recovery_description == "Alaska marine, Region 2, Quadrant PW" ~ "YAK",  #"X640", #630 and 640 pretty much go straight down the middle of PWS. this description is vague to start with... show Ole.
    ## ^^ there are 186 with this vague code: "AK M 2 PW" assigned to 640/YAK. all from net/siene fishery. 
    Bearing %in% c(" District 221", " District 212")  ~ "YAK", #"X640",
    #KOD AREA I looked over all the descriptions and districts, they are all well within the 630 area, thus we can broadly assign them into it without parcing. 
    Rec.area.Sullaway.Shelton == "KOD" ~ "PWS",  #"X630", #Distrcits are in cook inlet-area 630
    #APEN - already assigned using stat area
    TRUE ~ "FIX_ME")) 
 
 #join the two together and then create your own column in code assignment. 
temp1 <-temp_lat_long %>% 
   select(recovery_location_code, gs_nmfs_region)
temp2 <- temp_NA %>%
   select(recovery_location_code, gs_nmfs_region)

temp_3 <- read.csv("data/missing_codes.csv") %>% #assign these and then **PASTE THEM IN BY HAND** 
    filter(str_detect(tolower(recovery_location_code), "^1")) %>% #just going to focus on AK for now- filter out all others 
    mutate(gs_nmfs_region = case_when(
    longitude > -140 & latitude > 55.5   ~  "NSEAK", #"X650",
    longitude < -140 & longitude > -147 | longitude == -140 ~ "YAK", #"X640",
    longitude < -147 & longitude > -154 | longitude == -147 ~ "PWS", #"X630",
    longitude < -154 & longitude > -159 | longitude == -154 ~ "KOD", #"X620",
    longitude < -159 & longitude > -170 | longitude == -159 ~ "APEN",  #"X610",
   # longitude == -172 ~ "X541", #an odd ball on the south side of aluetian islands in sector 3
    #sector== " SECTOR 4"~ "BER",
    latitude < 55.5   ~ "SSEAK",
    TRUE ~ "FIX_ME")) %>%
  select(recovery_location_code, gs_nmfs_region)


codes_stat_area<-rbind(temp1, temp2) %>% #now combine all codes, select unique combinations so it can be added into the database of our assignments. 
  unite(temp, c(recovery_location_code, gs_nmfs_region)) 
codes_stat_area<-  as.data.frame(unique(codes_stat_area$temp)) %>%
  rename(temp= "unique(codes_stat_area$temp)") %>%
  separate(temp, into = c("recovery_location_code", "gs_nmfs_region"), sep = "_") 
 
join<- left_join(codes, codes_stat_area, by = c("recovery_location_code")) %>% #create a new column and save the csv. 
 # mutate(Rec.area.GS.nmfs = gs_nmfs_region) %>%
  mutate(Rec.area.GS.nmfs = case_when(is.na(gs_nmfs_region) ~ Rec.area.Sullaway.Shelton,
                                       TRUE ~ gs_nmfs_region)) %>%
  select(-c(gs_nmfs_region,recovery_location_code))
 
# write.csv(join, "nmfs_areas_recovery codes-wietkamp+shelton 12-2018 two PUSO.csv")
 
 
 
 
 
 
 
 
 
 
 
 
 
 
#PLOT ALL
 temp_NA %>% 
 filter(fishery_type == "high_seas") %>%
   group_by(rec_year, fishery, stat_area1) %>%
      count(stat_area1)  %>%
   ggplot(aes(x=rec_year, y=n)) +
   geom_bar(position="dodge", stat="identity") +
   facet_grid(stat_area1 ~ fishery, scales = "free_y", labeller=label_wrap_gen(width=.1)) +
   ggtitle('AK Region Recoveries') +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   labs(x='Recovery Year', y= 'Instance of Fish recovery')+
   theme_bw()+
   ggtitle("Recoveries AK")
 
 
 