library(tidyverse)
library(here)
library(gmodels)
#bring in obs and unobs compiled effort data and summarize to boat days
all_effort <- readRDS("data/AK_Effort_Trawl/Gasper_data/Chinooky_Rates_merged.RDS") #ELLR == NO OBSERVER, OBS == OBSERVER
# data <- all_effort %>%
#   filter(fmp_sub_area_code == "GOA")
# 
# table(data$year[!is.na(data$el_report_id)],data$processing_sector[!is.na(data$el_report_id)],exclude=NULL)

all_effort_boat_days<- all_effort %>%
  filter(fmp_sub_area_code == "GOA") %>%
  mutate(length_category = case_when(ves_akr_length < 125 & year < 2013 ~ "Small",
                                     ves_akr_length > 124 & year < 2013 ~ "Large",
                                     ves_akr_length > 59 & year > 2012 ~ "Large",
                                     ves_akr_length < 60 & year > 2012 ~ "Small",
                                     TRUE ~ as.character(ves_akr_length)),
         region = case_when(reporting_area_code == "610" ~ "E.APEN", 
                            reporting_area_code == "620" ~ "W.APEN",  
                            reporting_area_code %in% c("630") ~ "NW.GOA", 
                            reporting_area_code %in% c("640", "640 ") ~ "NE.GOA", 
                            reporting_area_code %in% c("649") ~ "NE.NW.GOA", #there are about 600 records from this stat area- need to divide them in half between NE and NW GOA, can do this based on ADFG numbers. check w ole before doing this.  
                            TRUE ~ "TBD")) %>% #there are none of these. 
        separate(trip_target_date, into = c("year", "month", "day"), sep = "-") %>%
        group_by(year,month,region,length_category,processing_sector) %>%
        dplyr::summarise(
          all_boat_days=n() ,
          all_vessel_count = n_distinct(catcher_vessel_id),
          #retained_catch_MT=sum(retained_gf_weight), #retained catch only starts in 2009 for some reason
          all_catch_MT=sum(total_gf_basis_weight))

#summarized observed effort data
    
#first just summarize observed effort off of the total effort 
obs_effort_boat_days <- all_effort %>%
  filter(catch_report_type_code=="OBS") %>%
  filter(fmp_sub_area_code == "GOA") %>%
  mutate(length_category = case_when(ves_akr_length < 125 & year < 2013 ~ "Small",
                                     ves_akr_length > 124 & year < 2013 ~ "Large",
                                     ves_akr_length > 59 & year > 2012 ~ "Large",
                                     ves_akr_length < 60 & year > 2012 ~ "Small",
                                     TRUE ~ as.character(ves_akr_length)),
         region = case_when(reporting_area_code == "610" ~ "E.APEN", 
                            reporting_area_code == "620" ~ "W.APEN",  
                            reporting_area_code %in% c("630") ~ "NW.GOA", 
                            reporting_area_code %in% c("640", "640 ") ~ "NE.GOA", 
                            reporting_area_code %in% c("649") ~ "NE.NW.GOA", #there are about 600 records from this stat area- need to divide them in half between NE and NW GOA, can do this based on ADFG numbers. check w ole before doing this.  
                            TRUE ~ "TBD")) %>% #there are none of these. 
  separate(trip_target_date, into = c("year", "month", "day"), sep = "-") %>%
  group_by(year,month,region,length_category,processing_sector) %>%
  dplyr::summarise(
    obs_boat_days=n(),
    obs_vessel_count=n_distinct(catcher_vessel_id),
    #retained_catch_MT=sum(retained_gf_weight), #retained catch only starts in 2009 for some reason
    obs_catch_MT=sum(total_gf_basis_weight))

effort <- left_join(all_effort_boat_days, obs_effort_boat_days) %>%
 mutate(obs_boat_days= replace_na(obs_boat_days,0),
        obs_vessel_count= replace_na(obs_vessel_count,0),
        obs_catch_MT= replace_na(obs_catch_MT,0))


