library(here)
library(tidyverse)
#a significant amount of the CWT recoveries reported in the rockfish dataframe that look like they never made it to RMIS. 
#spike in recoveries in 2013 that are not in RMIS

#this data frame is modified, doesnt have the sensitive info. 
# rockfish<- read_csv("data/Rockfish CWT mod trim.csv") %>%
#   mutate(tag_code= str_pad(tag_code, pad = "0", width = 6, side = c("left"))) %>%
#   separate(recovery_date, into =c("rec_year", "rec_monthrec_day"), sep = 4) %>%
#   filter(!rec_year %in% c("2018","2019")) %>%
#   mutate(source="rockfish") %>%
#   select(-c(fishery, detection_method))

#####################################################################################
#####################################################################################
#LOAD DATA SETS
# this dataframe has lat and long info for recoveries
rockfish <- read_csv("data/Rockfish CWT.csv") %>%
  mutate(Long=-1*Long) %>%
  mutate(tag_code= str_pad(tag_code, pad = "0", width = 6, side = c("left"))) %>%
  separate(recovery_date, into =c("rec_year", "rec_monthrec_day"), sep = 4) %>%
  separate(rec_monthrec_day, into =c("rec_month", "rec_day"), sep = 2) %>%
  filter(!rec_year %in% c("2018","2019")) %>%
  mutate(source="rockfish") %>%
  select(-c(fishery, detection_method)) %>%
  ## assign recoveries to our spatial boxes. Some latitudes dont have associated longitudes, but they do have an associated stat area 
  mutate(rockfish_recovery_region = case_when(Area == 620 ~ "E.APEN",   
                                              Area == 630 ~ "NW.GOA",
                                              Long <= -147 & Long > -154  ~ "NW.GOA", #"X630",
                                              Long <= -154 & Long > -159  ~ "E.APEN", #"X620",
                                              TRUE~ "FIX" )) %>%  # 5 fish from 2013 from ship Cape Kiwanda that do not have any location info. 
    filter(!Missing_Fins %in% c("None", "none")) #filter out recoveries where the adipose fin wasn't clipped 

#need to have WD set to RMIS_data repo...
load("data/final_data/all_chinook.Rdata")

all_chinook<-as.data.frame(all_chinook) %>%
  separate(recovery_date, into = c("rec_year", "rest_of_rec_date"), sep = 4) %>%
  separate(rest_of_rec_date, into = c("rec_month", "rec_day"), sep= 2) %>%
  mutate(rec_year = as.character(rec_year), recovery_id = as.numeric(recovery_id)) %>%
  filter(fishery %in% c(81,82,812)) %>%
  mutate(source="rmis") %>%
  select(-c("rec_month")) 

#Old RMIS - dont use. 
# highseas<- read_csv("data/RMIS.csv") %>%
#   mutate(rec_year = as.character(rec_year)) %>%
#   filter(fishery_type == "high_seas") %>%
#   mutate(source="rmis") %>%
#   select(-c("rec_month")) 

#####################################################################################
#####################################################################################
#for the rest of the script I have filtered out 2018/2019 data, fish that dont have adipose clips, and  tag code na's out of the rockfish data set. 

#how many rockfish recoveries dont have a tag code? (by year and month) 
df_NA <- rockfish %>%
  filter(is.na(tag_code))%>%
  select(c("rec_year","rec_month")) %>%
  group_by(rec_year) %>%
  count(rec_month) %>%
  filter(!is.na(rec_year))
#sum(df_NA$n) # ---> 24 rockfish recoveries without a tagcode and a year

#some of the rockfish recovery tag codes have NA's (~30 of them), remove those.
#also only working with 2013-2017, filtered out 2018 2019 data because they arent in RMIS. 
rockfish_tagcode <- rockfish %>%
  filter(!is.na(tag_code)) %>%
  filter(!is.na(rec_year))

#####################################################################################
#####################################################################################
# Below I just use the rockfish recoveries that are associated with a tag code. 
# Next, summarise rockfish recoveries across both data sets to figure out what did and did not match into RMIS (see summary1)
#only 40 rockfish CWT match in to RMIS
recovery_summary <-left_join(rockfish_tagcode,all_chinook,by = c("recovery_id","rec_year", "tag_code")) %>%
  mutate(source = case_when(source.x == "rockfish" & source.y=="rmis" ~ "both",
                            source.x == "rockfish" & is.na(source.y) ~ "rockfish",
                            TRUE ~ "rmis")) 

summary1<- recovery_summary %>%
  select(c("source", "rec_year","rec_month", "recovery_id")) %>%
  group_by(rec_year) %>%
  count(source) %>%
  filter(!is.na(rec_year)) #%>%
  # spread(source,n) %>%
  # mutate(fraction = both/rockfish) %>%
  # gather(2:4, key = "source", value = "count") %>%
  # filter(!source == "fraction") #comment this out if you want to look at fraction of overlap/year

#are there RMIS recoveries that are not in this CWT database? 
#COME BACK TO THIS 

# rmis_summary <-left_join(all_chinook,rockfish,by = c("recovery_id","rec_year", "tag_code")) %>%
#   mutate(source = case_when(source.y == "rockfish" & source.x=="rmis" ~ "both",
#                             source.y == "rockfish" & is.na(source.x) ~ "rockfish",
#                             TRUE ~ "rmis")) %>%
#   filter(!is.na(source.y))
# 
# test <- left_join(rockfish, rmis_summary,by = c("recovery_id","rec_year", "tag_code")) %>%
#   filter(is.na(Observer.y))
######################################################

# How many rockfish recoveries from the rockfish CWT data set are not entered in RMIS? (organized by year) -- only 1. 
summary2<- recovery_summary %>%
  filter(is.na(source.y))%>%
  select(c("rec_year", "recovery_id", "source")) %>%
  group_by(rec_year) %>%
  count(source)
#sum(summary2$n) #total 1 that were not accounted for in RMIS 
#####################################################################################
#####################################################################################
#Nest, try and see if there are there any patterns for the recoveries that dont match into RMIS 
#patterns to check:
    #location (ocean region) -- not a whole lot of diversity here, only NW.GOA in 2013. Not going to group by this 
    #time (month and year) -- most months have some rmis data and then more rockfish data, some months, only have rockfish data but if that is the case it is just a small amount. 
    #ship name -- Some ships are in both some are just rockfish, no huge pattern here.  

ship_recoveries<- recovery_summary %>%
  group_by(
    #rec_year, 
  source) %>% #grouping by rockfish recovery region because it is mre inclusive than the RMIS regions, but when they do match recoveries they have the same information. 
  count(Vessel_name)

################# THIS PLOT [BELOW] IS HELPFUL, OTHER ONES NOT SO MUCH. 
year_month<- recovery_summary %>%
  group_by(rec_year, rec_month) %>% #grouping by rockfish recovery region because it is mre inclusive than the RMIS regions, but when they do match recoveries they have the same information. 
  count(source)

ggplot(year_month, aes(x=rec_month, y = n)) +
  geom_bar(aes(fill=factor(source, levels=c("rockfish","both"))), stat= "identity")+
  labs(fill=" ") +
  facet_wrap(~rec_year) +
  theme_classic() #most of the entries that arent in RMIS are in 2013- what happened this year? 

#plot all counts/year  
recovery_summary %>%
  group_by(rec_year) %>% #grouping by rockfish recovery region because it is mre inclusive than the RMIS regions, but when they do match recoveries they have the same information. 
  count(source) %>% 
  filter(!source == "rockfish") %>%
  ggplot(aes(x=rec_year, y=n)) +
  geom_bar(
    #aes(fill=factor(source, levels=c("rockfish","both"))), 
    stat= "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Recovery Year', y= 'Instance of Fish recovery', title = 'Rockfish CWT Recoveries') +
  theme_bw()

#plot all counts/year just from rockfish data
## There is only 1- this plot isnt super useful anymore. 
# recovery_summary %>%
#   filter(source =="rockfish") %>%
#   group_by(rec_year) %>% #grouping by rockfish recovery region because it is mre inclusive than the RMIS regions, but when they do match recoveries they have the same information. 
#   count(source) %>%
# ggplot(aes(x=rec_year, y=n)) +
#   geom_bar(stat= "identity") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   labs(x='Recovery Year', y= 'Instance of Fish recovery', title = 'Rockfish CWT Recoveries- Data from jordan') +
#   theme_bw()


#####################################################################################
#####################################################################################
#plot map of recoveries in rmis and in extra rockfish data
# ----> THERE ARENT THAT MANY RECOVERIES WITH LAT AND LONG, SO THIS PLOT IS MILDLY INFORMATIVE
# world <- map_data("world")
# north_america <- subset(world, region %in% c("USA", "Canada"))
# 
# north_america %>%
#   filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
#   filter(!long > -115) %>%
#   filter(!lat < 46) %>%
#   filter(!lat > 62) %>%
#   filter(!long < -173) %>%
#   ggplot( ) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
#   coord_fixed(1.3) +
#   geom_point(data=recovery_summary, aes(x=Long, y = Lat,color=source),alpha=0.5) +
#   theme_classic() +
#   facet_wrap(~rec_year)
