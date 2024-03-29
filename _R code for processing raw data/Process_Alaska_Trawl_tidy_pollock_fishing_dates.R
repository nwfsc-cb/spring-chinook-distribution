library(here)
library(tidyverse)
library(data.table)
#load pollock fihsing dates and tidy for matching into match_ak_trawl.R 
#RIGHT NOW THIS IS SET FOR "LATE CWT'S" FILTERS OUT ANY FISHING BEFORE 1995! 

#basically just ignoring sector here because most fishing is in the "inshore sector" 
  #and the variability comes from regions (which I can match on) not sector 

dates <- read_csv(paste0(base.dir,"/Orca_Salmon_DATA/Recoveries/Alaska Trawl/AK_CWT_Trawl/pollock_fishing_dates/POLLOCK_USE.csv")) %>%
  dplyr::select(c(1:7)) %>%
  filter(!open_date == "Closed") %>%
  filter(year>1995) %>%
  rename(ocean_region = region) %>%
  #assign to regions for cwt matching. 
  mutate(rec.area.code = case_when(ocean_region == "GOA" & stat_area == "610" ~ "WAPEN",#"X610",
                            ocean_region == "GOA" & stat_area == "620" ~ "EAPEN", #"X620",
                            ocean_region == "GOA" & stat_area %in% c("630", "Shelikof") ~ "NWGOA", #"X630", 
                            ocean_region == "GOA" & is.na(stat_area) & sector == "Shelikof" ~ "NWGOA", #"X630", 
                            ocean_region == "GOA" & stat_area %in% c("640", "640 ") ~ "NEGOA", #"X630",  
                            ocean_region == "GOA" & stat_area %in% c("650") ~ "NEGOA", #"X650",
                            ocean_region == "BS" ~ "BER",
                            ocean_region == "AI" ~ "ALEUT",
                            TRUE ~ "ZZFIX"))  %>%
  #mutate(region=strsplit(region, ",")) %>% 
  filter(ocean_region == "GOA") %>%
  #unnest(region) %>%
  dplyr::select(-c(sector)) %>%
  mutate(open_date= as.Date(open_date, "%d-%b"), close_date = as.Date(close_date, "%d-%b"), year_close = year) %>%
  separate(open_date, c("delete", "open_date"), sep = 5) %>% #now open date is month-day
  separate(close_date, c("delete1", "close_date"), sep = 5) %>%
  unite("open_date", c("open_date","year"),sep = "-") %>%
  unite("close_date", c("close_date","year_close"),sep = "-") %>%
  mutate(open_date= as.Date(open_date, "%m-%d-%Y"), close_date = as.Date(close_date, "%m-%d-%Y")) %>%
  mutate(open_date= as.Date(open_date, "%Y-%m-%d"), close_date = as.Date(close_date, "%Y-%m-%d")) %>%
  dplyr::select(-c(delete, delete1)) %>%
  filter(!is.na(open_date)) #%>% #na's show up when the season is closed
#expand dates 
dates<-setDT(dates)[ , list(ocean_region = ocean_region, 
                            rec.area.code=rec.area.code, 
                            fishing_date = seq(open_date, close_date, by = "1 day")), 
                            by = 1:nrow(dates)]

dates<- dates %>%
  group_by(ocean_region,rec.area.code) %>%
  count(fishing_date) %>%
  dplyr::select(-c(n)) %>%
  mutate(fishing = "pollock") #%>%
  # mutate(sector = case_when(sector %in% c("Inshore", "Inshore\n", "Shelikof") ~ "Shoreside",
  #                           sector == "Offshore" ~ "Catcher Processor",
  #                           TRUE ~ sector))
  # 

saveRDS(dates, paste0(base.dir,"/Orca_Salmon_DATA/Recoveries/Alaska Trawl/AK_CWT_Trawl/pollock_fishing_dates/pollock_fishing_dates.RDS"))
