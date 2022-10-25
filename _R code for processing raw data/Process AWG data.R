#### This file is for exploring the AWG data.
library(tidyverse)
library(lubridate)

rmis.base.dir <- "/Users/ole.shelton/GitHub/rmis-data"
base.dir <- "/Users/ole.shelton/GitHub"

dat <- readRDS(paste0(base.dir,"/Orca_Salmon_DATA/AWG chinook CTC data/2022-09/CWDBRecovery.rds"))

# Go get the tag codes that I use 
tag.codes.fall <- read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Releases/CLIMATE/Tag codes fall chinook FRAM_v2 CLIMATE _two_OR_PUSO.csv"))
tag.codes.spr  <- read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Releases/SPR-SUM/Tag codes spring-summer chinook FRAM_2022_05 Spr-Sum_two_OR_PUSO_AK.csv"))
tag.codes <- bind_rows(tag.codes.fall,tag.codes.spr)  

# Go get all releases for later use
all.rel <- read.csv(paste0(rmis.base.dir,"/data/Chinook/all_releases.csv"))

# Go get the recovery codes from weitkamp and shelton
dat.loc.key	<- read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Recoveries/recovery codes-wietkamp+shelton 04-2022 two PUSO.csv"),header=T)

# add month and day columns.
dat <- dat %>% mutate(RecDate = as.Date(RecoveryDate,"%m/%d/%Y"),
                      month=month(RecDate),year=year(RecDate), day=day(RecDate))
dat <- dat %>% mutate(F.M = substr(RecoverySite,2,2))

# merge in the origins and IDs 
dat <- dat %>% rename(tag_code=TagCode) %>%
        left_join(., tag.codes %>% dplyr::select(ID, ocean.region,tag_code))

all <- dat %>% distinct(tag_code,ID,ocean.region) %>% arrange(tag_code)
all.na <- all %>% filter(is.na(ID))
 
all.na <- left_join(all.na,
                    all.rel %>% rename(tag_code=tag_code_or_release_id))
write.csv(all.na, file="all_na.csv")
# Check to see what fraction of information comes from 
# Freshwater vs. Marine for Aux data.
# A <- dat %>% group_by(Auxiliary,F.M) %>% summarise(N = length(F.M),Sum=sum(EstimatedNumber,na.rm=T))
# A
# A tibble: 4 × 4
# Groups:   Auxiliary [2]
# Auxiliary F.M        N      Sum
# <int>     <chr>   <int>    <dbl>
#   0       F     732594 1319038.
#   0       M     386364 1254643.
#   1       F      25686  759843.
#   1       M       1731   45840.
 

# Pull out the auxiliary data
dat.aux   <- dat %>% filter(Auxiliary==1) 
dat.aux.M <- dat %>% filter(Auxiliary==1,F.M =="M") 
dat.aux.F <- dat %>% filter(Auxiliary==1,F.M =="F") 

# Marine... These are mostly net fisheries but there are 
# some legit marine fisheries (mostly rec in St. of Georgia)
# This gets most of the locations.  
# There are a few mystery DFO locations that need parsing.
dat.aux.M$rec.area.code <- dat.loc.key$Rec.area.Sullaway.Shelton.TWO_OR_PUSO_AK[match(dat.aux.M$RecoverySite, dat.loc.key$location_code)]
# A <- dat.aux.M %>% filter(is.na(rec.area.code))
# > unique(A$RecoverySite)
# [1] "2MS99GSMNR0026"      "2MS99GSMNR2732"     
# [3] "2MS7023A"            "2MN36M056       000"
# [5] "2MN09H004"           "2MN12H008"          
# [7] "2MS51H008"           "3M10107" 

# Freshwater is where the action is.

head(dat.aux.F)

dat.aux.F %>% distinct(Agency) #CWDBFishery
dat.aux.F <- dat.aux.F %>% mutate(state.numb = substr(RecoverySite,1,1))

A <- dat.aux.F %>% group_by(state.numb,year) %>% summarise(length(year))
