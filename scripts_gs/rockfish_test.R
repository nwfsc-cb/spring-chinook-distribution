library(here)
library(tidyverse)
#40 of these match onto CWT
#figure out what the deal is for the ones that dont match- are these just a bunch of CWT recoveries that were not entered into RMIS? are there patterns by location or year? 

#map out the ones that do and do not match see if there is a pattern either way 

rockfish<- read_csv("data/Rockfish CWT mod trim.csv")
rockfish_all <- read_csv("data/Rockfish CWT.csv")
rmis<- read_csv("data/RMIS.csv")
rmis <- rmis %>%
  mutate(rec_year = as.character(rec_year))

rockfish1 <- rockfish %>%
mutate(tag_code= str_pad(tag_code, pad = "0", width = 6, side = c("left"))) %>%
separate(recovery_date, into =c("rec_year", "rec_monthrec_day"), sep = 4) %>%
 # separate(rec_monthrec_day, into =c("rec_month", "rec_day"), sep = 2) %>%
filter(!rec_year %in% c("2017","2018","2019")) %>%
select(-c(fishery, detection_method, rec_year))
# rename(tag_code_rockfishcwt = "tag_code")%>%
   
test<-left_join(rockfish1,rmis,by = c("tag_code", "recovery_id"))

test <- test %>%
  filter(!is.na(fishery.y))

highseas <- rmis %>%
  filter(fishery_type == "high_seas") 
