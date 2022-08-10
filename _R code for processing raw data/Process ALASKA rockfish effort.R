#This document matches the CWT recoveries from the Rockfish trawl fleet to RMIS to see if they can clarify specific trawl fleets in Alaska. The new data all matches into RMIS, but it doesnt give us new info in terms of matching fleets because all the RMIS matches are already coded for the rockfish fleet. It will add more specific lats and longs to RMIS if we want those. 

knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(here)
library(tidyverse)
library(ggplot2)
library(StratigrapheR)

data.dir <- paste0(base.dir,"/Orca_Salmon_DATA/Effort info/Trawl-ALASKA/")
data.cwt.dir <- paste0(base.dir,"/Orca_Salmon_DATA/Recoveries/Alaska Trawl/")
write.dir <- paste0(base.dir,"/spring-chinook-distribution/Processed Data/Effort Data/")

# ### Unobserved Data (2007-2012 Shoreside only)
# 2007-2012: Shoreside-- Use management code to ID trips.
# 2013-2020: [see observed data bc 100% census] 
# 
# ### Observed Data 
# This data uses "Unique trips observed.RDS".
# 2007-2012: Shoreside: The data exists but having a hard time making it useful due to how trips are ID'ed. 
#           If I use management code to ID trips, I just get CP data. 
#           If I use management code and trip target code to ID trips I get SHoreside and CP, 
#           but then there is a mismatch between observed and total effort, 
#           where observed days dont seem to be accounted for in the fish ticket data in the way we are using it. 
#               CP: Because they dont use elandings fish ticket stuff I do not have info on unobserved effort for these, are they fully observed??
# 2013-2020: ID trips using RPP and target trip code, 100% observed. (CP and Shoreside, comes from "Unique trips observed.RDS")

rockfish_observed<-readRDS(paste0(data.dir,"/Unique_trips_Observed.RDS")) %>%
  rename_all(tolower) %>% 
  mutate(month=as.numeric(format(haul_date,"%m")),
           rec.area.code = case_when(reporting_area_code == "610" ~ "WAPEN", 
                            reporting_area_code == "620" ~ "EAPEN",
                            reporting_area_code %in% c("630") ~ "NWGOA",
                            reporting_area_code %in% c("640", "640 ") ~ "NEGOA",
                            TRUE ~ "MISSING_LOCATION_INFO")) %>% 
  group_by(year,management_program_code,
           catcher_boat_adfg,pscnq_processing_sector,trip_target_code, month,akr_gear_code,rec.area.code, reporting_area_code) %>%
  dplyr::summarise(haul_days=length(unique(haul_date[!is.na(haul_date)])),
            vlength=ves_cfec_length[1],
            gf_catch=sum(official_total_catch,na.rm=TRUE)) %>%
    filter(trip_target_code=="K" | management_program_code == "RPP") %>%
    filter(year > 2012,
   # filter(case_when(year >2006 & year <2013 ~ management_program_code == "RPP", #pilot program, not 100% coverage. 
  #                  year>2012 ~ trip_target_code=="K" | management_program_code == "RPP"), #100% coverage
         !reporting_area_code == "650") %>%  #only 4 entries with 650 in mid 1990s
  ungroup() %>%
  mutate(year = as.numeric(year)) %>%  
  group_by(year,pscnq_processing_sector,month,rec.area.code) %>%
  dplyr::summarise(observed_boat_days = sum(haul_days)) %>%  
  mutate(observed_boat_days=(replace_na(observed_boat_days, 0))) 
 

### 2013- 2020 Plot Effort (All Observed, 100% Census)
#```{r stacked bar plots new, echo=FALSE, fig.height=5, fig.width=9}
temp.year <- as.character(c(2013:2020))
temp.month <- as.character(c(01:12))
temp.rec.area.code <-  as.character(c("EAPEN", "WAPEN",  "NWGOA", "NEGOA"))
category <-  as.character(c("S", "CP"))
year.month<- expand.grid(temp.year, temp.month, pscnq_processing_sector=category,rec.area.code= temp.rec.area.code)  #fully factorial match between all categories 
year.month$Var2<- str_pad(year.month$Var2, width= 2, pad = "0", side="left") #add a zero before month 1-9 so that I can keep months in order
year.month <- unite(year.month, "year.month", c("Var1", "Var2"), sep = ".", remove= TRUE) #join month and year in its own dataframe 
#general plot of all effort. 
rockfish_observed %>%  
  filter(year>2012) %>%
  ungroup() %>%
  dplyr::select(year,month,rec.area.code,pscnq_processing_sector,observed_boat_days) %>%   
  mutate(month = str_pad(month, "left", pad=0, width = 2)) %>%
  unite("year.month",c("year","month"), sep = ".") %>%
 # gather(4:5, key = "category", value = "boat_days") %>%
  merge(year.month, all = TRUE) %>% # add in labels
  mutate(observed_boat_days= replace_na(observed_boat_days,0)) %>%
#  filter(!boat_days<0) %>%
    ggplot(.) +
      geom_bar(aes(x=year.month,y=observed_boat_days), position="dodge", stat="identity",alpha = 0.8) +
      facet_grid(pscnq_processing_sector~rec.area.code, scales = "free") +
      ggtitle("Observed Boat Days") +  
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7)) # +
      #scale_x_discrete(breaks = every_nth(n = 12))
 
#### Now check seasonal coverage 
temp<- expand.grid(year=temp.year, rec.area.code=temp.rec.area.code, pscnq_processing_sector=category) #fully factorial match between all categories 
  
#####
#SPRING
rockfish_observed %>%  
  filter(year>2012) %>%
  ungroup() %>%
  dplyr::select(year,month,rec.area.code,pscnq_processing_sector,observed_boat_days) %>%   
  filter(month %in% c(4,5)) %>%
  group_by(year,rec.area.code,pscnq_processing_sector) %>%
  dplyr::summarise(observed_boat_days=sum(observed_boat_days))%>%
  ungroup() %>%
  merge(temp, all = TRUE) %>%
  mutate(observed_boat_days= replace_na(observed_boat_days,0)) %>%
  ggplot() +
  geom_bar(aes(x=year,y=observed_boat_days),stat="identity",alpha = 0.8) +
  facet_grid(pscnq_processing_sector~rec.area.code, scales = "free") +
  ggtitle("Spring Boat Days (Rockfish)") + 
  theme_classic() +
  #  geom_vline(aes(xintercept="2013"),color="magenta")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))  #+
  #scale_x_discrete(breaks = every_nth(n = 5))

#SUMMER
rockfish_observed %>%  
  filter(year>2012) %>%
  ungroup() %>%
  filter(!pscnq_processing_sector=="M") %>%
  dplyr::select(year,month,rec.area.code,pscnq_processing_sector,observed_boat_days) %>%   
  filter(month %in% c(6,7)) %>%
  group_by(year,rec.area.code,pscnq_processing_sector) %>%
  dplyr::summarise(observed_boat_days=sum(observed_boat_days))%>%
  ungroup() %>%
  merge(temp, all = TRUE) %>%
  mutate(observed_boat_days= replace_na(observed_boat_days,0)) %>%
  ggplot() +
  geom_bar(aes(x=year,y=observed_boat_days),stat="identity",alpha = 0.8) +
  facet_grid(pscnq_processing_sector~rec.area.code, scales = "free") +
  ggtitle("Summer Boat Days (Rockfish)") + 
  theme_classic() +
  #  geom_vline(aes(xintercept="2013"),color="magenta") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7)) # +
  #scale_x_discrete(breaks = every_nth(n = 5))

#FALL
rockfish_observed %>%  
  filter(year>2012) %>%
  ungroup() %>%
  filter(!pscnq_processing_sector=="M") %>%
  dplyr::select(year,month,rec.area.code,pscnq_processing_sector,observed_boat_days) %>%   
  filter(month %in% c(8,9,10)) %>%
  group_by(year,rec.area.code,pscnq_processing_sector) %>%
  dplyr::summarise(observed_boat_days=sum(observed_boat_days))%>%
  ungroup() %>%
  merge(temp, all = TRUE) %>%
  mutate(observed_boat_days= replace_na(observed_boat_days,0)) %>%
  ggplot() +
  geom_bar(aes(x=year,y=observed_boat_days),stat="identity",alpha = 0.8) +
  facet_grid(pscnq_processing_sector~rec.area.code, scales = "free") +
  ggtitle("Fall Boat Days (Rockfish)") + 
  theme_classic() +
  #  geom_vline(aes(xintercept="2013"),color="magenta") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))  #+
  #scale_x_discrete(breaks = every_nth(n = 5))

#WINTER
rockfish_observed %>%  
  filter(year>2012) %>%
  ungroup() %>%
  filter(!pscnq_processing_sector=="M") %>%
  dplyr::select(year,month,rec.area.code,pscnq_processing_sector,observed_boat_days) %>% 
  filter(month %in% c(1,2,3,11,12)) %>%
  mutate(season.temp = case_when(month %in% c(11,12) ~ "winter.1",
                                 month %in% c(1:3) ~ "winter.2",
                                 TRUE ~ "NA")) %>%
  group_by(year, season.temp,rec.area.code,pscnq_processing_sector) %>%
  dplyr::summarise(observed_boat_days=sum(observed_boat_days)) %>%
  ungroup() %>%
  mutate(year=as.numeric(year), year.new = case_when(season.temp == "winter.2" ~ year -1,
                                                     TRUE ~ year)) %>%
  group_by(year.new, rec.area.code, pscnq_processing_sector) %>%
  dplyr::summarise(observed_boat_days=sum(observed_boat_days)) %>%
  dplyr::rename(year=year.new) %>%
  merge(temp) %>%
  mutate(observed_boat_days= replace_na(observed_boat_days,0)) %>%
  ggplot() +
  geom_bar(aes(x=year,y=observed_boat_days), stat="identity",alpha = 0.8) +
  facet_grid(pscnq_processing_sector~rec.area.code, scales = "free") +
  ggtitle("Winter Boat Days ( Rockfish)") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))# +
# geom_vline(aes(xintercept=2013),color="magenta")


### Observed CP 2007-2012 but we have no info about total effort in 2010-2012, were CP's fully observed? Can we get the total effort for these years? 
#2010 - 2012
# temp.year <- as.character(c(2010:2012))
# temp.month <- as.character(c(01:12))
# temp.rec.area.code <-  as.character(c("W.APEN", "NW.GOA"))
# year.month<- expand.grid(temp.year, temp.month, rec.area.code=temp.rec.area.code) #fully factorial match between all categories 
# year.month$Var2<- str_pad(year.month$Var2, width= 2, pad = "0", side="left") #add a zero before month 1-9 so that I can keep months in order
# year.month <- unite(year.month, "year.month", c("Var1", "Var2"), sep = ".", remove= TRUE) #join month and year in its own dataframe 

# observed_rockfish_2010_2012 %>%  
#   ungroup() %>%
#   dplyr::select(year,month,rec.area.code,observed_boat_days) %>%   
#   mutate(month = str_pad(month, "left", pad=0, width = 2)) %>%
#   unite("year.month",c("year","month"), sep = ".") %>%
#   # gather(4:5, key = "category", value = "boat_days") %>%
#   merge(year.month, all = TRUE) %>% # add in labels
#   mutate(observed_boat_days= replace_na(observed_boat_days,0)) %>%
#   ggplot() +
#   geom_bar(aes(x=year.month,y=observed_boat_days), position="dodge", stat="identity",alpha = 0.8) +
#   facet_wrap(~rec.area.code, scales = "free") +
#   ggtitle("Observed Boat Days CP") +  
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))  +
#   scale_x_discrete(breaks = every_nth(n = 12))

###############################################################################
### Unobserved and Observed Effort Shoreside 2007-2012
###############################################################################

#this is shoreside only!! 
rockfish_fish_ticket<-readRDS(paste0(data.dir,"fishtix_snippet.RDS")) %>% 
  filter(adfg_h_mgt_program_id == "RPP") %>%
  mutate(fishing_start_date=as.Date(ft_date_fishing_began,format="%Y%m%d"), # the dates are wonky - convert to acutal dates
         date_landed=as.Date(ft_date_landed,format="%Y%m%d"),
         rec.area.code = case_when(reporting_area_code == "610" ~ "WAPEN", 
                            reporting_area_code == "620" ~ "EAPEN",
                            reporting_area_code %in% c("630") ~ "NWGOA",
                            reporting_area_code %in% c("640", "640 ") ~ "NEGOA",
                            TRUE ~ "MISSING_LOCATION_INFO")) %>% 
  group_by(vessel_id,adfg_number,date_landed,year,rec.area.code) %>% # group by vessel-date_landed-year,reporting_area_code
  dplyr::summarise(ft_observers_onboard=ft_observers_onboard[1], # this observers_onboard field seems pretty useless.
            observed=observed[1], #  this flag for whether a vessel is observed seems somewhat more useful. Maybe you'll find it helpful?
            fishing_start_date=min(fishing_start_date), # for a particular landed date there may be multiple start dates. Pick the earliest one.
            pounds=sum(pounds[earnings>0],na.rm=TRUE)) %>%  # summarise pounds of only those species for which they got paid
  data.frame %>% 
  rowwise() %>% 
  mutate(fish_days=as.vector(difftime(date_landed,fishing_start_date,units="days")), # determine number of days between start and landed. Note that if landed the same day it will be a zero. Maybe you want to add 1? [line below]
         fish_days = case_when(fishing_start_date==date_landed ~ 1,
                               TRUE ~ fish_days),
         month=as.numeric(format(date_landed,"%m")),
         year=as.numeric(format(date_landed,"%Y")),
         # season = case_when(month %in% c(1,2,3,11,12) ~ "winter",
         #                        month %in% c(4, 5) ~ "spring",
         #                        month %in% c(6,7) ~ "summer",
         #                        month %in% c(8,9,10) ~ "fall",
         #                        TRUE~"SEASON_NA")
  ) %>% # extract a month and year field
  group_by(year,month,rec.area.code) %>% #  group by vessel-date_landed-year,reporting_area_code
  dplyr::summarise(total_boat_days = sum(fish_days) #,
            # all_catch_MT=sum(pounds*0.000453592)
  )%>% #convert to MT, 0.000453592 MT = 1 LB 
  filter(!rec.area.code == "MISSING_LOCATION_INFO", year >2006 & year < 2013) 


temp.year <- as.character(c(2007:2012))
temp.month <- as.character(c(01:12))
temp.rec.area.code <-  as.character(c("WAPEN",  "NWGOA"))
year.month<- expand.grid(temp.year, temp.month, rec.area.code=temp.rec.area.code)   #fully factorial match between all categories 
year.month$Var2<- str_pad(year.month$Var2, width= 2, pad = "0", side="left") #add a zero before month 1-9 so that I can keep months in order
year.month <- unite(year.month, "year.month", c("Var1", "Var2"), sep = ".", remove= TRUE) #join month and year in its own dataframe 
#general plot of all effort. 
rockfish_fish_ticket %>%  
  ungroup() %>%
  dplyr::select(year,month,rec.area.code,total_boat_days) %>%   
  mutate(month = str_pad(month, "left", pad=0, width = 2)) %>%
  unite("year.month",c("year","month"), sep = ".") %>%
  merge(year.month, all = TRUE) %>% # add in labels
  mutate(total_boat_days= replace_na(total_boat_days,0)) %>%
  ggplot() +
  geom_bar(aes(x=year.month,y=total_boat_days), position="dodge", stat="identity",alpha = 0.8) +
  facet_wrap(~rec.area.code, scales = "free") +
  ggtitle("Total Effort") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))  #+
  #scale_x_discrete(breaks = every_nth(n = 12))


## Seasonal for total effort 
temp<- expand.grid(year=temp.year, rec.area.code=temp.rec.area.code)  #fully factorial match between all categories 
  
#SPRING
rockfish_fish_ticket %>%  
  ungroup() %>%
  dplyr::select(year,month,rec.area.code,total_boat_days) %>%   
  filter(month %in% c(4,5)) %>%
  group_by(year,rec.area.code) %>%
  dplyr::summarise(total_boat_days=sum(total_boat_days))%>%
  ungroup() %>%
  merge(temp, all = TRUE) %>%
  mutate(total_boat_days= replace_na(total_boat_days,0)) %>%
  ggplot() +
  geom_bar(aes(x=year,y=total_boat_days),stat="identity",alpha = 0.8) +
  facet_wrap(~rec.area.code, scales = "free") +
  ggtitle("Spring Total Boat Days (Rockfish)") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7)) # +
  #scale_x_discrete(breaks = every_nth(n = 5))

#SUMMER
rockfish_fish_ticket %>%  
  # ungroup() 
  dplyr::select(year,month,rec.area.code,total_boat_days) %>%   
  filter(month %in% c(6,7)) %>%
  group_by(year,rec.area.code) %>%
  dplyr::summarise(total_boat_days=sum(total_boat_days))%>%
  ungroup() %>%
  merge(temp, all = TRUE) %>%
  mutate(total_boat_days= replace_na(total_boat_days,0)) %>%
  ggplot() +
  geom_bar(aes(x=year,y=total_boat_days),stat="identity",alpha = 0.8) +
  facet_wrap(~rec.area.code, scales = "free") +
  ggtitle("Summer Boat Days (Rockfish)") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7)) # +
  #scale_x_discrete(breaks = every_nth(n = 5))

#FALL
rockfish_fish_ticket %>%  
  ungroup() %>%
  dplyr::select(year,month,rec.area.code,total_boat_days) %>%   
  filter(month %in% c(8,9,10)) %>%
  group_by(year,rec.area.code) %>%
  dplyr::summarise(total_boat_days=sum(total_boat_days))%>%
  ungroup() %>%
  merge(temp, all = TRUE) %>%
  mutate(total_boat_days= replace_na(total_boat_days,0)) %>%
  ggplot() +
  geom_bar(aes(x=year,y=total_boat_days),stat="identity",alpha = 0.8) +
  facet_wrap(~rec.area.code, scales = "free") +
  ggtitle("Fall Boat Days (Rockfish)") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7)) # +
  #scale_x_discrete(breaks = every_nth(n = 5))

#WINTER
rockfish_fish_ticket %>%  
  ungroup() %>%
  dplyr::select(year,month,rec.area.code,total_boat_days) %>% 
  filter(month %in% c(1,2,3,11,12)) %>%
  mutate(season.temp = case_when(month %in% c(11,12) ~ "winter.1",
                                 month %in% c(1:3) ~ "winter.2",
                                 TRUE ~ "NA")) %>%
  group_by(year, season.temp,rec.area.code) %>%
  dplyr::summarise(total_boat_days=sum(total_boat_days)) %>%
  ungroup() %>%
  mutate(year=as.numeric(year), year.new = case_when(season.temp == "winter.2" ~ year -1,
                                                     TRUE ~ year)) %>%
  group_by(year.new, rec.area.code,) %>%
  dplyr::summarise(total_boat_days=sum(total_boat_days)) %>%
  dplyr::rename(year=year.new) %>%
  merge(temp, all=TRUE) %>%
  mutate(total_boat_days= replace_na(total_boat_days,0)) %>%
  ggplot() +
  geom_bar(aes(x=year,y=total_boat_days), stat="identity",alpha = 0.8) +
  facet_wrap(~rec.area.code, scales = "free") +
  ggtitle("Winter Boat Days ( Rockfish)") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7)) 

## How does RMIS classify a rockfish trip? See last figure, RMIS recoveries and effort match better when I classify Rockfish trips using the target code rather than the management code
# Does effort generally line up with recoveries? [Using Rockfish recovery data from JW and AFSC] Shoreside looks good, what is the deal with no recoveries for CP?  This is better for trip target code than management code.... 

#load RMIS data 
rockfish <- read.csv(paste0(data.cwt.dir,"/Rockfish CWT.csv")) %>%
  dplyr::mutate(Long=-1*Long, tag_code= str_pad(tag_code, pad = "0", width = 6, side = c("left"))) %>%
  tidyr::separate(recovery_date, into =c("rec_year", "rec_monthrec_day"), sep = 4) %>%
  tidyr::separate(rec_monthrec_day, into =c("rec_month", "rec_day"), sep = 2) %>%
  #dplyr::filter(!rec_year %in% c("2018","2019")) %>%
  dplyr::mutate(source="rockfish") %>%
  dplyr::select(-c(fishery, detection_method)) %>%
  ## assign recoveries to our spatial boxes. Some latitudes dont have associated longitudes, but they do have an associated stat area 
  dplyr::mutate(rec.area.code = case_when(Area == 620 ~ "EAPEN",   
                                   Area == 630 ~ "NWGOA",
                                   Long <= -147 & Long > -154 ~ "NWGOA", #"X630",
                                   Long <= -154 & Long > -159  ~ "WAPEN", #"X620",
                                   TRUE~ "FIX" )) %>%  # 5 fish from 2013 from ship Cape Kiwanda that do not have any location info. 
  filter(!Missing_Fins %in% c("None", "none"), !rec.area.code=="FIX", !rec_year<2013) %>%  #filter out recoveries where the adipose fin wasn't clipped, also filter out recoveries with no lcoatino info, there are only 4.   
  group_by(rec.area.code, rec_year) %>%
  dplyr::count(rec_month) %>%
  unite("year.month",c("rec_year","rec_month"), sep = ".")  %>%
  dplyr::rename(recoveries=n)

ggplot(rockfish) +
  geom_bar(aes(x=year.month,y=recoveries),stat="identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))  +
  facet_wrap(~rec.area.code)

rockfish_observed %>%  
  ungroup() %>%
  dplyr::select(year,month,rec.area.code,observed_boat_days) %>%
    mutate(month = str_pad(month, "left", pad=0, width = 2) ) %>%
  unite("year.month",c("year","month"), sep = ".") %>% 
  full_join(rockfish, by= c("year.month","rec.area.code")) %>%
  gather(3:4, key = "category", value = "number") %>%
  mutate(number= replace_na(number,0), category=factor(category, levels=c("recoveries", "observed_boat_days"))) %>%
  filter(!is.na(rec.area.code), !rec.area.code %in% c("NEGOA NWGOA WAPEN EAPEN BER", "NSEAK")) %>%
  group_by(year.month,rec.area.code,category) %>%
  summarise(number=sum(number)) %>%
  ggplot() +
  geom_bar(aes(x=year.month,y=number, fill = category),position="stack", stat="identity",alpha = 0.8) +
  facet_wrap(~rec.area.code, scales="free_y") +
  ggtitle("Compare Observed Data and RMIS Recoveries") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))  #+
  #scale_x_discrete(breaks = every_nth(n = 20)) 


saveRDS(rockfish_fish_ticket, paste0(write.dir,"rockfish_shoreside_2007_2012_summarized.RDS"))
saveRDS(rockfish_observed, paste0(write.dir,"rockfish_shoreside_CP_2013_2020_summarized.RDS")) 

