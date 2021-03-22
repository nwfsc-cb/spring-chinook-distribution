library(here)
library(tidyverse)

#GS To Do: 
# 1) dig through the tag codes and figure out if we need to add month start and stop dates for each release. For example, are there releases from a single ID_name code that are released brood_year +1 and others at brood_year +2? If so we may need to split the releases into _small and _large release groups so as not to mix them.  See "river releases fall FRAM_v1.csv" for some examples on how Ole has done this before.
        # Working on this

# 2) Are there reasonable numbers of releases and years from each "Ocean.Region"? Or do we need to go back and find and include additional releases from particular location and years.  It may be that they don't exist.  But we can add to the river releases file.
        # Region_na keeps the regions and years where we don't have release data  
        # Count_data shows how many years of releases we havd for each ocean region 
        # Ocean regions with less than 10 years: CHIG_spr, KOD, JUAN_spr. 
        # Depending on cutoff for how many years we want to make sure are in release data we can filter count_data to those 
# 3) Are there some releases that are actually very very small and need to be excluded for some reason?
        # A small handful - in the low_data dataframes 

# 4) Do the groupings of ID_name and ocean.region seem ok?  Or are there some mistakes?
        # Yep! 
# 5) Ensure each tag code only occurs once in the "Tag codes" file.
        # Yep, each tag_code only occurs once

# NEXT TASKS:
#   After we feel good about the releases, we can use the rest of the
# Github⁩/⁨Salmon-Climate/⁨_R code for processing raw data⁩/CWT identify, fetch releases, match to recover.R
# file to match targeted releases to recoveries.
  
# Naming conventions for ID_name.  Generally, I use the name of the hatchery first, then the name of the river (if there are multiple river releases from a hatchery) and followed by _spr, _wint, or _sum for the run type (fall is the default if there is no label at the end).

# data.dir <- "/Users/ole.shelton/GitHub/Orca_Salmon_Data/Releases/CLIMATE"

fall_releases <- read.csv("data_gitignore/river releases fall chinook FRAM_v1 CLIMATE.csv")
spring_releases <- read.csv("data_gitignore/river releases spring-summer chinook FRAM_v2 CLIMATE.csv")
tag_codes <- read.csv("data_gitignore/Tag codes spring-summer chinook FRAM_v2 CLIMATE AK_NCA_SOR_PUSO.csv")

# fall_releases <- read.csv(paste0(data.dir,"/river releases fall chinook FRAM_v1 CLIMATE.csv"))
# spring_releases <- read.csv(paste0(data.dir,"/river releases spring-summer chinook FRAM_v2 CLIMATE.csv"))
# tag_codes <- read.csv(paste0(data.dir,"/Tag codes spring-summer chinook FRAM_v2 CLIMATE AK_NCA_SOR_PUSO.csv"))

######################################################
######################################################
# 1. figure out is there are releases of multiple ages 
df.ad.cwt <- tag_codes %>% 
  mutate(release_age =release.year-brood_year) %>%
  group_by(ID, release_age,stock_location_code, hatchery_location_code, release_stage, first.release.month, last.release.month) %>%
  summarise(cwt.sum = sum(cwt.released)) %>% 
  #count(release_age) %>%
  ungroup() %>%
  mutate(ID = as.character(ID), release_age = as.numeric(release_age), cwt.sum = as.numeric(cwt.sum)) %>%
  spread(release_age, cwt.sum) %>%
  dplyr::select(-c("7")) %>% # this one is data entry error or something, recorded as 0 cwt.released 
  na_if(0)  #a lot of NAs, change these to 0 so they get filtered out

# Ole made this second df so we could look at releases that have CWT but no ad-clip... seems to be increasingly common in BC and WA
df.noad.cwt <- tag_codes %>% 
  mutate(release_age =release.year-brood_year) %>%
  group_by(ID, release_age,stock_location_code, hatchery_location_code, release_stage, first.release.month, last.release.month) %>%
  summarise(cwt.all.sum=sum(cwt.all.released)) %>%
  #count(release_age) %>%
  ungroup() %>%
  mutate(ID = as.character(ID), release_age = as.numeric(release_age),cwt.all.sum=as.numeric(cwt.all.sum)) %>%
  spread(release_age, cwt.all.sum) %>%
  dplyr::select(-c("7")) %>% # this one is data entry error or something, recorded as 0 cwt.released 
  na_if(0) #a lot of zeros, probably data entry problem. change these to 0 so they get filtered out

#function to remove rows that have more than x NA's across a row
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

df.noad.cwt <- delete.na(df.noad.cwt, 2) #remove rows that have more than 3 columns with NA-because that means most fish were released within a single age. 
#this is the dataframe to use when splitting months. 

#I think this is what can be joined in to change the groupings, but check with Ole on how the fall ones were divided. 
df.noad.cwt1 <- df.noad.cwt %>%
  gather(7:10, key = release.age, value = count) %>%
  mutate(release.age = as.numeric(release.age)) %>%
  filter(!is.na(count)) %>%
  mutate(join = case_when(release.age > 1 ~ "large",
                          TRUE ~ "small")) %>%
  unite("new_ID", c("ID", "join"), sep = "_", remove = FALSE) 
#  rename(ID_name = "ID")
spring_releases <- spring_releases %>%
  rename(ID = "ID_name")

#this should be the final df, added in months and small/large names. double check all things and clean df a little more to look like fall release df
#also right now just used the all.cwt column should include the ad clipped fish? 
spring_release_with_months <- tag_codes %>%
  group_by(ID, river, ocean.region, run, stock_location_code, hatchery_location_code, first.release.month, last.release.month) %>%
  count(release_stage) %>%
  left_join(df.noad.cwt1) %>%
  ungroup() %>%
  mutate(ID = case_when(is.na(new_ID) ~ ID,
                        TRUE~ new_ID))%>%
  dplyr::select(-c(n, join, new_ID, count,release.age)) %>%
  dplyr::rename(month.start="first.release.month", month.stop="last.release.month")

######################################################
######################################################
  #2. Are there reasonable numbers of releases and years from each "Ocean.Region"?
# below is a plot of total releases by year and region 
df<- tag_codes %>%
  group_by(release.year,ocean.region) %>%
  summarise(cwt.sum = sum(cwt.released),cwt.all.sum = sum(cwt.all.released))

ggplot(df, aes(x= release.year, y= cwt.sum)) +
  geom_bar(stat= "identity") +
  facet_wrap(~ocean.region, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_classic()

ggplot(df, aes(x= release.year, y= cwt.all.sum)) +
  geom_bar(stat= "identity") +
  facet_wrap(~ocean.region, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# looks like there are some lacking- making a data frame that shows where we are missing years or have little information for certain regions. 
tag_codes <- tag_codes %>%
  mutate(ocean.region = as.character(ocean.region))

code_list <- data.frame(rep(unique(tag_codes$ocean.region), each=43))

regions <- code_list %>%
  rename(ocean.region =rep.unique.tag_codes.ocean.region...each...43.) %>%
  dplyr::select(ocean.region) %>%
  group_by(ocean.region) %>%
  mutate(release.year= 1975:2017) %>%
  left_join(df, by = c("ocean.region", "release.year")) 

regions_na <- regions %>% 
              filter(is.na(cwt.sum))

#summarize the amount of years we do have data for each region
count_data <- regions %>% 
  filter(!is.na(cwt.sum)) %>%
 # group_by(ocean.region) %>%
 count(ocean.region) %>%
  rename(years_of_data = n)
       
#### OLE: This is super helpful.  I'll dig in and make an additional file of tag codes to try and fill in these gaps. 


######################################################
######################################################
# 3) Are there some releases that are actually very very small and need to be excluded for some reason?

low_data_cwt <- tag_codes %>%
  filter(!cwt.released==0) %>%
  group_by(release.year, ID, hatchery_location_code) %>%
  #filter(cwt.released<100) %>%
  summarise(sum= sum(cwt.released)) %>%
  filter(sum<5000)  

low_data_all <- tag_codes %>%
  filter(!cwt.released==0) %>%
  group_by(release.year, ID, hatchery_location_code) %>%
  #filter(cwt.released<100) %>%
  summarise(sum= sum(cwt.all.released)) %>%
  filter(sum<5000) 

######################################################
######################################################
# 4) Do the groupings of ID_name and ocean.region seem ok?  Or are there some mistakes?
groupings<- tag_codes %>%
            group_by(ocean.region,ID) %>%
            count(ocean.region)

######################################################
######################################################
# 5) Ensure each tag code only occurs once in the "Tag codes" file.
tag_codes <- tag_codes %>%
  mutate(tag_code = as.character(tag_code))

unique_tag_codes <- data.frame(code=c(unique(tag_codes$tag_code)))


