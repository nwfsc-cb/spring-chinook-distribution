## SCRIPT FOR IMPORTING ALASKA POLLOCK SURVEY DATA, CREATING EFFORT MAPS AND SAMPLING FRACTION CALCULATIONS

knitr::opts_chunk$set(echo = FALSE, message=FALSE, warning = FALSE)
library(tidyverse)
library(here)
library(gmodels)
library(gplots)
library(readr)
col.br= colorRampPalette(c("white", "blue","purple","red")) #colors for heatmap
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

data.dir <- paste0(base.dir,"/Orca_Salmon_DATA/Effort info/Trawl-ALASKA/")
data.rec.dir <- paste0(base.dir,"/Orca_Salmon_DATA/Recoveries/Alaska Trawl/")
write.dir <- paste0(base.dir,"/spring-chinook-distribution/Processed Data/Effort Data/")
plot.dir <- paste0(base.dir,"/spring-chinook-distribution/Heatmaps/")

observed <- readRDS(paste0(data.dir,"Unique_trips_Observed.RDS"))  %>% mutate(haul_date1=haul_date) %>%
  separate(haul_date1, into = c("year", "month", "day"), sep = "-") %>%
  rename_all(tolower) %>% 
  mutate(month =as.numeric(month),
         trip_target_code2=ifelse(akr_gear_code=="NPT","Not-P",trip_target_code),
         rec.area.code = case_when(reporting_area_code == "610" ~ "WAPEN", 
                            reporting_area_code == "620" ~ "EAPEN",
                            reporting_area_code %in% c("630") ~ "NWGOA",
                            reporting_area_code %in% c("640", "640 ") ~ "NEGOA",
                            TRUE ~ "MISSING_LOCATION_INFO")) %>% 
  group_by(year,catcher_boat_adfg,pscnq_processing_sector,
           trip_target_code,trip_target_code2,
           month, day, haul_date, cruise, 
           akr_gear_code,rec.area.code, reporting_area_code,obs_vessel_id,haul_join) %>%
  dplyr::summarise(haul_days=length(unique(haul_date[!is.na(haul_date)])),
                   vlength=ves_cfec_length[1],
                   gf_catch=sum(official_total_catch,na.rm=TRUE)) %>%
  filter(case_when(year == 1996 ~ !akr_gear_code == "NPT",
                   TRUE ~ trip_target_code == "P"), !reporting_area_code == "650") %>% #1996 trips are classified slightly differently, tyhere is no trip target code so remove NPT for 1996 only. #only 1 entry from 650 dont know if it is SSEAK or NSEAK
  ungroup() %>%
  mutate(
    # length_category = case_when(vlength < 125 & year < 2013 ~ "Small",
    #                                  vlength > 124 & year < 2013 ~ "Large",
    #                                  vlength > 59 & year > 2012 ~ "Large",
    #                                  vlength < 60 & year > 2012 ~ "Small",
    #                                  TRUE ~ as.character(vlength)),
    # season = case_when(month %in% c(1,2,3,11,12) ~ "winter",
    #                       month %in% c(4, 5) ~ "spring",
    #                       month %in% c(6,7) ~ "summer",
    #                       month %in% c(8,9,10) ~ "fall",
    #                       TRUE~"SEASON_NA"),
    year = as.numeric(year)) %>%  #add season for plotting purposes- season structure = what ole uses in 2020 paper
  group_by(year,pscnq_processing_sector,trip_target_code, month,rec.area.code
           #,length_category, #can add this in later if fish ticket data for obs and unobs has v length?
  ) %>%
  dplyr::summarise(observed_boat_days = sum(haul_days),
            obs_catch_MT=sum(gf_catch)) #THIS IS IN MT--CONVERT TO LBS OR FISH TICKET CONVERT TO MT

#all, observed and unobserved trips ***shoreside only
fish_ticket<-readRDS(paste0(data.dir,"fishtix_snippet.RDS")) %>% 
  mutate(fishing_start_date=as.Date(ft_date_fishing_began,format="%Y%m%d"), # the dates are wonky - convert to acutal dates
         date_landed=as.Date(ft_date_landed,format="%Y%m%d"),
         rec.area.code = case_when(reporting_area_code == "610" ~ "WAPEN", 
                            reporting_area_code == "620" ~ "EAPEN",
                            reporting_area_code %in% c("630") ~ "NWGOA",
                            reporting_area_code %in% c("640", "640 ") ~ "NEGOA",
                            TRUE ~ "MISSING_LOCATION_INFO")) %>% 
  filter(!adfg_h_mgt_program_id %in% c("RPP")) %>% #remove fish tickets from the rockfish program. 
  group_by(vessel_id,adfg_number,date_landed,year,rec.area.code) %>% # group by vessel-date_landed-year,reporting_area_code
  dplyr::summarise(ft_observers_onboard=ft_observers_onboard[1], # this observers_onboard field seems pretty useless.
            observed=observed[1], #  this flag for whether a vessel is observed seems somewhat more useful. Maybe you'll find it helpful?
            fishing_start_date=min(fishing_start_date), # for a particular landed date there may be multiple start dates. Pick the earliest one.
            pounds=sum(pounds[earnings>0],na.rm=TRUE)) %>%  # summarise pounds of only those species for which they got paid
  data.frame %>% 
  rowwise() %>% 
  mutate(fish_days=as.numeric(difftime(date_landed,fishing_start_date,units="days"))+1) %>%
    # determine number of days between start and landed. 
    # Note that if landed the same day it will be a zero. Added 1 here.
  mutate(
        # fish_days = case_when(fishing_start_date==date_landed ~ 1,
        #                         TRUE ~ fish_days),
         month=as.numeric(format(date_landed,"%m")),
         year=as.numeric(format(date_landed,"%Y"))) %>%
         # season = case_when(month %in% c(1,2,3,11,12) ~ "winter",
         #                        month %in% c(4, 5) ~ "spring",
         #                        month %in% c(6,7) ~ "summer",
         #                        month %in% c(8,9,10) ~ "fall",
         #                        TRUE~"SEASON_NA")
   # extract a month and year field
  group_by(year,month,rec.area.code) %>% #  group by vessel-date_landed-year,reporting_area_code
  dplyr::summarise(total_effort = sum(fish_days,na.rm=T),
            all_catch_MT=sum(pounds*0.000453592)) %>% #convert to MT, 0.000453592 MT = 1 LB 
  filter(!rec.area.code == "MISSING_LOCATION_INFO") 

shoreside_observed <- observed %>%
  filter(pscnq_processing_sector == "S") %>% 
  mutate(year=as.numeric(year))

#merge data and get the fraction of obs and unobserved. 
join_data_shoreside <- full_join(fish_ticket, shoreside_observed, by=c("year", "month", "rec.area.code")) %>%
  dplyr::mutate(observed_boat_days = as.numeric(replace_na(observed_boat_days,0)),
                total_effort = replace_na(total_effort,0),
                total_effort = ifelse(total_effort < observed_boat_days,observed_boat_days,total_effort),
                unobs_boat_days = total_effort - observed_boat_days)  %>%
  # This gets rid of the rare times when observed effort > total effort (almost always within one or two)
  #mutate(total_effort = ifelse(total_effort < observed_boat_days,observed_boat_days,total_effort)) %>%
  filter(!rec.area.code == "MISSING_LOCATION_INFO")  %>%
  ungroup() %>%
   mutate(#observed_boat_days = case_when(year<1996 ~ total_effort*0.3, #0.3 based on oberver coverage chart when we dont have data for 1991-1996.
  #                                       TRUE ~observed_boat_days),
         unobs_boat_days = case_when(year<1996 ~ total_effort - observed_boat_days, TRUE ~ unobs_boat_days),
         #unobs_boat_days = case_when(year<1996 ~ unobs_boat_days-observed_boat_days, TRUE ~ unobs_boat_days),
         frac_boat_days_obs = observed_boat_days/total_effort,
         frac_boat_days_obs = replace_na(frac_boat_days_obs, 0))

#############3 MAKE SOME PLOTS

temp.year <- as.character(c(1991:2020))
temp.month <- as.character(c(01:12))
temp.rec.area.code <-  as.character(c("WAPEN", "EAPEN",  "NWGOA", "NEGOA"))
category <-  as.character(c("unobs_boat_days", "observed_boat_days"))
year.month<- expand.grid(temp.year, temp.month,category=category,rec.area.code=temp.rec.area.code) #fully factorial match between all categories 
year.month$Var2<- str_pad(year.month$Var2, width= 2, pad = "0", side="left") #add a zero before month 1-9 so that I can keep months in order
year.month <- unite(year.month, "year.month", c("Var1", "Var2"), sep = ".", remove= TRUE) #join month and year in its own dataframe 

#general plot of all effort. 
join_data_shoreside %>%  
  ungroup() %>%
  dplyr::select(year,month,rec.area.code,pscnq_processing_sector,unobs_boat_days,observed_boat_days,frac_boat_days_obs) %>%   
  mutate(month = str_pad(month, "left", pad=0, width = 2)) %>%
  unite("year.month",c("year","month"), sep = ".") %>%
  gather(4:5, key = "category", value = "boat_days") %>%
  merge(year.month, all = TRUE) %>% # add in labels
  mutate(boat_days= replace_na(boat_days,0)) %>%
  filter(!boat_days<0) %>%
  ggplot() +
  geom_bar(aes(x=year.month,y=boat_days, fill = category), position="stack", stat="identity",alpha = 0.8) +
  facet_wrap(~rec.area.code, scales = "free", ncol=1) +
  ggtitle("Boat Days (Shoreside Pollock)") + # no small CP's
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))  +
  scale_x_discrete(breaks = every_nth(n = 12))


### Stacked bar plots summarized by season

#make specific x-axes
temp.rec.area.code <-  as.character(c("WAPEN", "EAPEN",  "NWGOA", "NEGOA"))
category <-  as.character(c("unobs_boat_days", "observed_boat_days"))

temp<- expand.grid(year=temp.year, category=category,rec.area.code=temp.rec.area.code) #fully factorial match between all categories 
  #rename(year="Var1",category="Var2", rec.area.code="Var3") #%>%
#mutate(Var2= str_pad(Var2, width= 2, pad = "0", side="left")) #%>%
#  unite("year.month", c("Var1", "Var2"), sep = ".", remove= TRUE) #join month and year in its own dataframe 


#SPRING
join_data_shoreside %>%  
  ungroup() %>%
  filter(month %in% c(3,4,5)) %>%
  dplyr::select(1:3,8,10) %>%
  gather(4:5, key = "category", value = "boat_days") %>%
  group_by(year,rec.area.code,category) %>%
  dplyr::summarise(boat_days=sum(boat_days))%>%
  ungroup() %>%
  merge(temp, all = TRUE) %>%
  mutate(boat_days= replace_na(boat_days,0)) %>%
  ggplot() +
  geom_bar(aes(x=year,y=boat_days, fill = category), position="stack", stat="identity",alpha = 0.8) +
  facet_grid(~rec.area.code, scales = "free") +
  ggtitle("Spring Boat Days (Shoreside Pollock)") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))  +
  scale_x_discrete(breaks = every_nth(n = 5))

## SUMMER
join_data_shoreside %>%  
  ungroup() %>%
  filter(month %in% c(6,7)) %>%
  dplyr::select(1:3,8,10) %>%
  gather(4:5, key = "category", value = "boat_days") %>%
  group_by(year,rec.area.code,category) %>%
  dplyr::summarise(boat_days=sum(boat_days))%>%
  ungroup() %>%
  merge(temp, all = TRUE) %>%
  mutate(boat_days= replace_na(boat_days,0)) %>%
  ggplot() +
  geom_bar(aes(x=year,y=boat_days, fill = category), position="stack", stat="identity",alpha = 0.8) +
  facet_grid(~rec.area.code, scales = "free") +
  ggtitle("Summer Boat Days (Shoreside Pollock)") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))  +
  scale_x_discrete(breaks = every_nth(n = 5))

#FALL
join_data_shoreside %>%  
  ungroup() %>%
  filter(month %in% c(8,9,10)) %>%
  dplyr::select(1:3,8,10) %>%
  gather(4:5, key = "category", value = "boat_days") %>%
  group_by(year,rec.area.code,category) %>%
  dplyr::summarise(boat_days=sum(boat_days))%>%
  ungroup() %>%
  merge(temp, all = TRUE) %>%
  mutate(boat_days= replace_na(boat_days,0)) %>%
  ggplot() +
  geom_bar(aes(x=year,y=boat_days, fill = category), position="stack", stat="identity",alpha = 0.8) +
  facet_grid(~rec.area.code, scales = "free") +
  ggtitle("Fall Boat Days (Shoreside Pollock)") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))  +
  scale_x_discrete(breaks = every_nth(n = 5))

#WINTER
join_data_shoreside %>%  
  ungroup() %>%
  dplyr::select(1:3,8,10) %>%
  filter(month %in% c(1,2,11,12)) %>%
  mutate(season.temp = case_when(month %in% c(11,12) ~ "winter.1",
                                 month %in% c(1:2) ~ "winter.2",
                                 TRUE ~ "NA")) %>%
  gather(4:5, key = "category", value = "boat_days") %>%
  group_by(year, season.temp,rec.area.code,category) %>%
  dplyr::summarise(boat_days=sum(boat_days)) %>%
  ungroup() %>%
  mutate(year=as.numeric(year), year.new = case_when(season.temp == "winter.2" ~ year -1,
                                                     TRUE ~ year)) %>%
  group_by(year.new, rec.area.code, category) %>%
  dplyr::summarise(boat_days=sum(boat_days)) %>%
  dplyr::rename(year=year.new) %>%
  merge(temp) %>%
  mutate(boat_days= replace_na(boat_days,0)) %>%
  ggplot() +
  geom_bar(aes(x=year,y=boat_days, fill = category), position="stack", stat="identity",alpha = 0.8) +
  facet_grid(~rec.area.code, scales = "free") +
  ggtitle("Winter Boat Days ( Shoreside Pollock)") + # no small CP's
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7)) 

#####################################
#How long do trips tend to be?  
#####################################
all_data_trip_length<-readRDS(paste0(data.dir,"fishtix_snippet.RDS")) %>% 
  mutate(fishing_start_date=as.Date(ft_date_fishing_began,format="%Y%m%d"), # the dates are wonky - convert to acutal dates
         date_landed=as.Date(ft_date_landed,format="%Y%m%d"),
         rec.area.code = case_when(reporting_area_code == "610" ~ "WAPEN", 
                            reporting_area_code == "620" ~ "EAPEN",
                            reporting_area_code %in% c("630") ~ "NWGOA",
                            reporting_area_code %in% c("640", "640 ") ~ "NEGOA",
                            TRUE ~ "MISSING_LOCATION_INFO")) %>% 
  group_by(vessel_id,adfg_number,date_landed,year,rec.area.code) %>% #  group by vessel-date_landed-year,reporting_area_code
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
         season = case_when(month %in% c(1,2,11,12) ~ "winter",
                            month %in% c(3,4,5) ~ "spring",
                            month %in% c(6,7) ~ "summer",
                            month %in% c(8,9,10) ~ "fall",
                            TRUE~"SEASON_NA")) %>%
  group_by(year, rec.area.code) %>%
  dplyr::summarise(mean_trip_length = mean(fish_days),
            sd_fish_days=sd(fish_days)) %>%
  filter(!rec.area.code == "MISSING_LOCATION_INFO")

all_data_trip_length %>% 
  ungroup() %>%
  # mutate(month = str_pad(month, "left", pad=0, width = 2)) %>%
  #unite("year.month",c("year","month"), sep = ".") %>%
  ggplot(aes(x=year,y=mean_trip_length)) +
  geom_line()+ 
  geom_point(alpha = 0.8) +
  #geom_errorbar(aes(ymin=mean_trip_length-sd_fish_days, ymax=mean_trip_length+sd_fish_days))+
  facet_wrap(~rec.area.code, scales = "free", ncol=1) +
  ggtitle("Spring Boat Days (Shoreside Pollock)") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90#, hjust = 1,size=7
  ))  


############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################


#Fraction of Boat Days with an observer onboard.
#Black = no effort, white = no observed effort

spread.total <- join_data_shoreside %>%
  ungroup() %>%
  dplyr::select(c(1:3,11)) %>%
  mutate(month = str_pad(month, "left", pad=0, width = 2)) %>%
  unite("year.month",1:2, remove=TRUE, sep = ".") %>%
  # group_by(year.month, rec.area.code) %>%
  # summarise(total_effort = sum(total_effort),
  #           observed_boat_days = sum(observed_boat_days)) %>%  
  merge(year.month %>% dplyr::select(-c(category)) %>% group_by(year.month) %>% dplyr::count(rec.area.code) %>% dplyr::select(-c(n)), all = TRUE) %>%
  # mutate(obs_frac= observed_boat_days/total_effort) %>%
  filter(!frac_boat_days_obs >1) %>% #handful of errors as JW about. 
  dplyr::select(year.month,rec.area.code, frac_boat_days_obs) %>%
  tidyr::spread(year.month, frac_boat_days_obs) 

#set up matrix for heatmap 
#spread.total<- spread.total[1:5,] 
rec.area.code<- spread.total$rec.area.code
spread.total<- spread.total[,2:237]
row.names(spread.total) <- rec.area.code
matrix= as.matrix(spread.total)
#matrix[matrix < 0.1] <- NA #turn 0 to NA so that it doesnt get messed up in log function

# FOR X AXIS LABELS
temp.lab = as.data.frame(temp.year) #create df with all years
temp <- as.data.frame(temp.lab[rep(1:nrow(temp.lab),1,each=12),]) #duplicate each row 12 times
temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`[duplicated(temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`)] <- " " #fill duplicates with a space
temp$label<-  temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`  #change column name and remove old column

heatmap.2(matrix, Rowv=FALSE, Colv=FALSE, 
          xlab = "Year.Month",  
          scale="none", margins=c(4,6), 
          dendrogram="none", trace="none",
          col=col.br(20), #breaks=pairs.breaks,
          na.color = "black",
          key =TRUE,
          keysize = 1.5, #labRow = year_list,
          key.title = "log scale",
          adjCol = c(0.5,1.1),
          labCol = temp$label, #tell it to use dummy variable for X axis years
          labRow = rec.area.code, #use dummy variable for Y axis rec.area.codes so they can be north to south 
          cexCol = 0.6,
          sepcolor = "black",
          main = "Fraction of Observed Boat Days", 
          density.info = ("none"),
          #( "bottom.margin", "left.margin", "top.margin", "right.margin" )
          key.par=list(mar=c(2,1, 1,0.1)))

####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################

# If there is an observer on board- how much of the catch are they surveying?
# Do this by looking at the observed trips,

# This involves three data frames from AKFIN and the observer program.
# 
# Observed trips "Unique Trips..."
# "Ole_Squash", All of the chinook ever sampled by the observer program.
# - we trying to merge observed and squash to id pollock, shoreside trips.
# "Chinooky...", how AFSC expands groundfish catches to estimate total bycatch. 
   # Derived from J. Watson's pulls from the AK databases.
# 
# Squash gives us how many fish were actually seen.
# Chinooky gives expansion rates and estimated number of fish caught, but not actually seen fish.
# If we merge squash and chinooky, we can suss out the fraction actually sampled.
# 
# Caveats-
#   Chinooky doesn't exist pre-2003.
#   Observed and Squash start in 1996
#   For 1996-2002, we have observed trips. 2003-2007 had the same protocol as 1996-2002.
#   Make a guess at sampling fraction, from just observed fraction.
#     - only reason to do this is 1997,98,99 have lots of recoveries.


options(digits = 21)
setwd(data.dir)
observed.raw <- readRDS(paste0(data.dir,"Unique_trips_Observed.RDS")) %>% mutate(haul_date2 =haul_date) %>%
  separate(haul_date2, into = c("year", "month", "day"), sep = "-")
observed.summary<- observed.raw  %>% 
  mutate(year=as.numeric(substr(haul_date,1,4))) %>%
  mutate(trip_target_code2=ifelse(akr_gear_code=="NPT","Not-P",trip_target_code)) %>%
  dplyr::select(year,
                haul_join,
                cruise,
                #haul_date,
                obs_vessel_id,
                nmfs_area=reporting_area_code,
                akr_gear_code,
                trip_target_code2,
                pscnq_processing_sector) %>%
  mutate(nmfs_area=as.numeric(nmfs_area)) %>% 
  # only keep observations targeting pollock
  #filter(trip_target_code %in% c("B","P")|akr_gear_code=="PTR") %>%
  mutate(trip_target_code2 = ifelse(trip_target_code2 == "B","P",trip_target_code2)) %>%
  group_by(year,
           haul_join,
           cruise,
           #haul_date,
           obs_vessel_id,
           nmfs_area,
           #akr_gear_code,
           #trip_target_code,
           trip_target_code2,
           pscnq_processing_sector) %>%
  dplyr::summarise(n=length(year)) %>% dplyr::select(-n)
  # get rid of catcher processors right off the bat.
  

##### SUMMARY Here is a table for Pollock in the GOA.  The vast, vast majority of fishing is
#####  from Shoreside in the Gulf of Alaska.  CP and motherships are ignorable.
observed.raw %>% 
  group_by(reporting_area_code,pscnq_processing_sector, trip_target_code) %>% 
  tally() %>% filter(trip_target_code %in% c("P","B")) %>% as.data.frame()


# how many observations are we looking for  
observed.S.P <- observed.summary %>% filter(trip_target_code2=="P",pscnq_processing_sector=="S")  
count.ob.S.P <- observed.S.P %>% group_by(cruise,year,haul_join) %>% summarise(N=length(year)) 

dups <- count.ob.S.P %>% filter(N>1) # these are are the observations that are duplicates.
A <- observed.S.P %>% filter(cruise %in% dups$cruise, haul_join %in% dups$haul_join)

dim(A); sum(dups$N) # got em

# why are they duplicated?
# because the observers had multiple rec.area.codes associated with each trip
head(A %>% as.data.frame())

# dat.squash is a list of all of the chinook sampled in the observer program.
# we need to merge in the observed data from above to assign target and managment group to this
# dat.squash includes all of the Bering and lots of fleets, so we need to filter those out.

# here is a table of where and when they have samples from:
options(digits = 21)
setwd(data.rec.dir)
dat.squash.raw <- readRDS(paste0(data.rec.dir,"Ole_Squash.RDS")) %>%
    mutate(haul_date_sq =HAUL_OFFLOAD_DATE) %>%
    separate(HAUL_OFFLOAD_DATE, into = c("year", "month", "day"), sep = "-")

dat.squash.raw %>% arrange(NMFS_AREA) %>% group_by(year,NMFS_AREA) %>% summarise(N=length(year)) %>%
  pivot_wider(.,values_from = "N",names_from = c("NMFS_AREA")) %>% as.data.frame()

# We assume that if they made in into this database they were checked for an adipose
# fin and therefore for CWT.
# We are going to use this database to figure out what fraction of the total Chinook
# captured during the fishery.

# First we have to pull out the trips that can be ascribed to the shoreside pollock fleet
# in the Gulf of Alaska.
dat.squash.goa <- dat.squash.raw %>%
    dplyr::select(year,month,day,haul_date_sq,
                    cdq_code=CDQ_CODE,gear=GEAR,permit=PERMIT,
                    cruise=CRUISE,
                    port_join =PORT_JOIN,
                    haul_join=HAUL_JOIN,
                    vessel_code =VESSEL_CODE,
                    lat_end=LATDD_END,lat_start=LATDD_START,
                    lon_end=LONDD_END,lon_start=LONDD_START,
                    nmfs_area=NMFS_AREA,
                    TYPE_1_OTOLITH, TYPE_2_SCALE, TYPE_3_SEX_LENGTH_WEIGHT,
                    TYPE_4_FIN_CLIPS, TYPE_5_VERTEBRAE, TYPE_6_SPINES,
                    TYPE_7_MATURITY_SCAN, TYPE_8_MATURITY, TYPE_9_STOMACHS,
                    TYPE_10_ISOTOPES,TYPE_11_OTHER_TISSUE,  TYPE_12_SNOUT,
                    TYPE_13_ADIPOSE_PRESENT,
                    weight=WEIGHT,
                    sex=SEX,
                    ad_pres = ADIPOSE_PRESENT,
                    SAMPLE_SYSTEM) %>% 
    filter(nmfs_area>600,nmfs_area<670) %>% # include only GOA
    mutate(haul_join_short = floor(haul_join*0.00001)) %>% ## This is a way to cut off the last 5 digits of haul join 
    mutate(port_join_short = floor(port_join*0.00001)) %>% ## This is a way to cut off the last 5 digits of haul join
    #mutate(haul_join_short = substr(haul_join_1,1,15)) %>%
    mutate(year=as.numeric(year))

head(dat.squash.goa)

dat.squash.goa.obs.shore.early <- left_join(dat.squash.goa,observed.S.P %>% filter(year<=2007,year>=1997),
                                            by = c("haul_join"= "haul_join",
                                                   "cruise"="cruise",
                                                   "nmfs_area"="nmfs_area",
                                                   "year"="year")) %>% 
                                      filter(pscnq_processing_sector=="S")

dim(dat.squash.goa %>% filter(year<=2007,year>=1997)) # 4388
dim(dat.squash.goa.obs.shore.early) # 3706
# 3706/4388 =  84.4% from the trawl shoreside.

# join based on two different fields for 2014 and on.
dat.squash.goa.obs.shore.late1  <- left_join(dat.squash.goa,observed.S.P %>% filter(year>=2014),
                                            by = c("port_join"= "haul_join",
                                                    "cruise"="cruise",
                                                    "nmfs_area"="nmfs_area",
                                                    "year"="year")) %>% 
                                    filter(pscnq_processing_sector=="S")

dat.squash.goa.obs.shore.late2  <- left_join(dat.squash.goa,observed.S.P %>% filter(year>=2014),
                                          by = c("haul_join"= "haul_join",
                                                  "cruise"="cruise",
                                                  "nmfs_area"="nmfs_area",
                                                  "year"="year")) %>%
                                     filter(pscnq_processing_sector=="S")

dat.squash.goa.obs.shore.late <- bind_rows(dat.squash.goa.obs.shore.late1,dat.squash.goa.obs.shore.late2)

dim(dat.squash.goa %>% filter(year>=2014)) # 21646
dim(dat.squash.goa.obs.shore.late) # 19539
# 90.2% from the trawl shoreside.

### Make up my own join for 2008-2011
yr.start <- 2008
yr.stop  <- 2011
dat.squash.goa.mid <- dat.squash.goa %>% filter(year>=yr.start,year<=yr.stop) 

# make a new join key using only part of haul_join and port_join
observed.S.P.mid <- observed.raw  %>% 
  mutate(year=as.numeric(substr(haul_date,1,4))) %>%
  mutate(trip_target_code2=ifelse(akr_gear_code=="NPT","Not-P",trip_target_code)) %>%
  dplyr::select(year,
                haul_join,
                cruise,
                haul_date,
                obs_vessel_id,
                nmfs_area=reporting_area_code,
                akr_gear_code,
                trip_target_code2,
                pscnq_processing_sector) %>%
  mutate(nmfs_area=as.numeric(nmfs_area)) %>% 
  # only keep observations targeting pollock
  mutate(trip_target_code2 = ifelse(trip_target_code2 == "B","P",trip_target_code2)) %>%
  filter(trip_target_code2 %in% c("P")|akr_gear_code=="PTR") %>%
  filter(year>=yr.start,year<=yr.stop) %>%
  group_by(year,
           cruise,
           haul_join,
           haul_date,
           obs_vessel_id,
           nmfs_area,
           #akr_gear_code,
           #trip_target_code,
           trip_target_code2,
           pscnq_processing_sector) %>%
  dplyr::summarise(n=length(year)) %>% dplyr::select(-n) %>%
  mutate(haul_join_short = floor(haul_join*0.00001)) 
  

# JOIN 
dat.squash.goa.obs.shore.mid1 <- semi_join(dat.squash.goa.mid,observed.S.P.mid,
                    by=c("year",
                         "port_join_short" = "haul_join_short",
                         "cruise",
                         #"vessel_code"="obs_vessel_id",
                         "nmfs_area")) %>%
  mutate(obs_vessel_id=NA,trip_target_code2="P",pscnq_processing_sector="S")

dat.squash.goa.obs.shore.mid2 <- semi_join(dat.squash.goa.mid,observed.S.P.mid,
                                           by=c("year",
                                                "haul_join_short" = "haul_join_short",
                                                "cruise",
                                                #"vessel_code"="obs_vessel_id",
                                                "nmfs_area")) %>%
  mutate(obs_vessel_id=NA,trip_target_code2="P",pscnq_processing_sector="S")

dim(dat.squash.goa.obs.shore.mid1)
dim(dat.squash.goa.obs.shore.mid2)

# and there are no rows where haul_join_short == port_join_short, so there are no duplicates
dat.squash.goa %>% filter(haul_join_short==port_join_short) %>% nrow()

dat.squash.goa.obs.shore.mid = bind_rows(dat.squash.goa.obs.shore.mid1,
                                         dat.squash.goa.obs.shore.mid2)

dat.squash.goa.mid %>% nrow() # 1396
dat.squash.goa.obs.shore.mid %>% nrow() # 1204
# 1204/1396 = 86% from shoreside pollock. 

### LOOK at 2012-2013 separately
# I can't get these records to match to anything reasonable.  I've tried a lot of things.
# So I went digging in the literature.

# year.start <- 2012
# year.stop <- 2013
# dat.squash.goa.mid2 <- dat.squash.goa %>% filter(year>=year.start,year<=year.stop) 

# Some shit I dug up from AFSC tech report 291
# 2013.  Total samples collected = 737, 698 of these worked for genetics.
# Genetic samples that worked (sum=698) by area 
# early (week 4-13): 
#   610, 5
#   620, 256
#   630, 105
#   640, 107
# late (week 35-44): 
#   610, 0
#   620, 111
#   630, 114
#   640, 0
  
# For comparison, there are 858 entries in dat.squash for these areas in 2013.
# if only the 737 chinook examined for genetics were examined for tags that 
# would be 86% of the bycatch in the observed fish (737/858 = 0.859). 
# That's right about in line with the sampling fraction from adjacent years.  But which ones are they?

# AFSC tech report # 289.
# There were a large number of samples collected in 2013 from industry groups... (>2000)
# in the rockfish and arrowtooth fishery.  These should not be in the observer database
  
# AFSC tech report 288 Table 3
# Fraction of trips observed for bycatch.
# dockside sampling 2012 jan-june = 0.888, July-Dec = 0.688
# dockside sampling 2013 jan-june = 0.764, July-Dec = 0.775
# Fraction of chinook sampled from the observed bycatch
# all 2012 and 2013 = 0.10

### THIS IS SOME OF THE JUNK I TRIED.
# make a new join key using 
# observed.S.P.mid2 <- observed.raw  %>% 
#   mutate(year=as.numeric(substr(haul_date,1,4))) %>%
#   mutate(trip_target_code2=ifelse(akr_gear_code=="NPT","Not-P",trip_target_code)) %>%
#   dplyr::select(year,
#                 haul_join,
#                 cruise,
#                 haul_date,
#                 obs_vessel_id,
#                 nmfs_area=reporting_area_code,
#                 akr_gear_code,
#                 trip_target_code2,
#                 pscnq_processing_sector) %>%
#   mutate(nmfs_area=as.numeric(nmfs_area)) %>% 
#   # only keep observations targeting pollock
#   mutate(trip_target_code2 = ifelse(trip_target_code2 == "B","P",trip_target_code2)) %>%
#   filter(trip_target_code2 %in% c("P")|akr_gear_code=="PTR") %>%
#   filter(year>=year.start,year<=year.stop) %>%
#   group_by(year,
#            cruise,
#            haul_join,
#            haul_date,
#            obs_vessel_id,
#            nmfs_area,
#            #akr_gear_code,
#            #trip_target_code,
#            trip_target_code2,
#            pscnq_processing_sector) %>%
#   dplyr::summarise(n=length(year)) %>% dplyr::select(-n) %>%
#   mutate(haul_join_short = floor(haul_join*0.00001)) 
#   
# #mutate(key=paste(cruise,obs_vessel_id,haul_date,sep="."))
# 
# # dat.squash.goa.mid <- dat.squash.goa.mid %>%
# #                         mutate(key=paste(cruise,vessel_code,haul_date_sq,sep="."))
# 
# dat.squash.goa.obs.shore.mid2 <- semi_join(dat.squash.goa.mid2),
#                                            observed.S.P.mid2 %>% filter(year>=yr.start,year<=yr.stop),
#                                           by=c("year",
#                                                "cruise",
#                                                "vessel_code"="obs_vessel_id",
#                                                "nmfs_area")) %>%
#   mutate(obs_vessel_id=NA,trip_target_code2="P",pscnq_processing_sector="S")

#########
#########
######### add 2012 and 2013 in manually at the end because the sampling is clearly messed up.
#########
#########


# Make a plot of the proportion of sampled Chinook from shoreside pollock.
## OK COMBINE...
dat.squash.goa.obs.shore <- bind_rows(dat.squash.goa.obs.shore.early,
                                      dat.squash.goa.obs.shore.mid,
                                      dat.squash.goa.obs.shore.late)

dat.squash.goa.obs.shore %>% distinct() %>% dim()

TWO <-left_join(dat.squash.goa %>% group_by(year) %>% tally ,
          dat.squash.goa.obs.shore %>% group_by(year) %>% tally %>% rename(n.samp = n)) %>% 
          mutate(prop.samp = round(n.samp/n,3) )
    
  ggplot(TWO,aes(x=year,y=prop.samp)) + 
          geom_point() +
          geom_line()

fish.sampled.tot.v.in.pollock <-  ggplot(TWO) +
    geom_point(aes(x=year,y=n),color="blue") +
    geom_line(aes(x=year,y=n),color="blue") +
    geom_point(aes(x=year,y=n.samp),color="red") +
    geom_line(aes(x=year,y=n.samp), color= "red") +
    ggtitle("Blue is total fish sampled, Red are from pollock fleet")

# It sure looks like it worked ok.
  # and it sure looks like those are just fish multiple fish that happen to be the same size and sex in a single observer trip.
  # So we're done.
  # Let's assign our rec.area.codes to this data file.
  dat.squash.goa.obs.shore <- dat.squash.goa.obs.shore %>%
    mutate(rec.area.code = case_when(nmfs_area == "610" ~ "WAPEN",
                              nmfs_area == "620" ~ "EAPEN",
                              nmfs_area %in% c("630") ~ "NWGOA",
                              nmfs_area %in% c("640", "640 ") ~ "NEGOA",
                              nmfs_area == "649" ~ "PWS",
                              #reporting_area_code == "649" & adfg_stat_area_code %in% c("466032","466033","466003") ~ "NE.GOA",
                              #reporting_area_code == "649" & adfg_stat_area_code %in% c("476006","486001","476035","476005","476007","476003","476004","476034", "475933","476031","476008","485932") ~ "NW.GOA", #there are about 600 records from NMFS AREA 649, which is the most northern part of PWS stat area- going to divide these based on in the 630/640 line continued into PWS and assign via ADFG #'s
                              TRUE ~ "MISSING_LOCATION_INFO")) 
  
    # summary tables
  dat.squash.goa.obs.shore %>% group_by(year,nmfs_area) %>% tally() %>% 
        pivot_wider(.,values_from = n,names_from = nmfs_area) %>% as.data.frame()
  
# THIS IS THE DATA FRAME WHICH HAS SHORESIDE DATA FOR OBSERVED CHINOOK IN GOA, 1997 on
dat.squash.goa.obs.shore <- dat.squash.goa.obs.shore %>%
                    mutate(rec.area.code = case_when(nmfs_area == "610" ~ "WAPEN",
                                              nmfs_area == "620" ~ "EAPEN",
                                              nmfs_area %in% c("630") ~ "NWGOA",
                                              nmfs_area %in% c("640", "640 ") ~ "NEGOA",
                                              nmfs_area == "649" ~ "PWS",
                          #reporting_area_code == "649" & adfg_stat_area_code %in% c("466032","466033","466003") ~ "NE.GOA",
                          #reporting_area_code == "649" & adfg_stat_area_code %in% c("476006","486001","476035","476005","476007","476003","476004","476034", "475933","476031","476008","485932") ~ "NW.GOA", #there are about 600 records from NMFS AREA 649, which is the most northern part of PWS stat area- going to divide these based on in the 630/640 line continued into PWS and assign via ADFG #'s
                          TRUE ~ "MISSING_LOCATION_INFO")) 

############### OK. NOW LET'S CONNECT THESE TO THE CHINOOKY DATA FRAME which contains all of the 
############### information that allows you to expand to total bycatch.
############### 

#options(digits=15)
chinooky_rates <- readRDS(paste0(data.dir,"Chinooky_Rates_merged.RDS")) %>%
  filter(fmp_sub_area_code == "GOA", target =="pollock") %>% #  23438 observations
  mutate(rec.area.code = case_when(reporting_area_code == "610" ~ "WAPEN",
                            reporting_area_code == "620" ~ "EAPEN",
                            reporting_area_code %in% c("630") ~ "NWGOA",
                            reporting_area_code %in% c("640", "640 ") ~ "NEGOA",
                            reporting_area_code == "649" ~ "PWS",
                            #reporting_area_code == "649" & adfg_stat_area_code %in% c("466032","466033","466003") ~ "NE.GOA",
                            #reporting_area_code == "649" & adfg_stat_area_code %in% c("476006","486001","476035","476005","476007","476003","476004","476034", "475933","476031","476008","485932") ~ "NW.GOA", #there are about 600 records from NMFS AREA 649, which is the most northern part of PWS stat area- going to divide these based on in the 630/640 line continued into PWS and assign via ADFG #'s
                            TRUE ~ "MISSING_LOCATION_INFO")) %>% #there are 14 of these because they are in area 649 but have missing ADFG #'s, all from a few days  in 2003
  separate(trip_target_date, into = c("year", "month", "day"), sep = "-") %>%
  rename(nmfs_area = reporting_area_code) %>% #  23438 observations
  filter(processing_sector=="S") %>% #23363 observations
  filter(!management_program_code %in% c("SMO"),!nmfs_area==649)
  # this gets the data down to about 22940 observations. 
  # includes a small number of RPP and AFA management codes.


options(digits=15)
tot.chin <- chinooky_rates %>% 
              group_by(year,
                       month,
                       rec.area.code,nmfs_area) %>% 
              summarise(N.deliveries=length(year),est.chinook = sum(pscnq_estimate)) %>%
              as.data.frame() %>%
              mutate(year=as.numeric(year),nmfs_area=as.numeric(nmfs_area))

obs.chin <- dat.squash.goa.obs.shore %>% group_by(year,month,nmfs_area,rec.area.code) %>%
                        summarise(obs.chinook=length(year))

all.chin <- full_join(tot.chin,obs.chin) %>% arrange(year,month) %>% 
              mutate(obs.chinook = ifelse(is.na(obs.chinook)==T,0,obs.chinook),
                     est.chinook = ifelse(is.na(est.chinook)==T,0,est.chinook),
                      sampling.fraction = round(obs.chinook / est.chinook,4),
                     sampling.fraction =ifelse(is.infinite(sampling.fraction)==T,NA,sampling.fraction))

all.chin <- all.chin %>% mutate(month.numb = as.numeric(month))

samp.frac.raw <- ggplot(all.chin,aes(x=year,y=sampling.fraction,color=month),alpha=0.5) + 
  geom_point(alpha=0.5) +
  geom_line(alpha=0.2) +
  facet_wrap(~rec.area.code) +
  #lims(y=c(0,1)) +
  theme_bw()

## There is one observation from October 2009 where sampling fraction is >1.  
# This occurs because there were observations that show up in one database as occurring on Sept 30 and another on Oct 1
# That's irritating.  To smooth this out this produces sampling fractions that integrate over several months
# and weights infomation by the number of chinook estimated and observed. (i.e. not a simple average of sampling fraction)

all.chin <- all.chin %>% mutate(est.chin.smooth = as.numeric(NA),
                                obs.chin.smooth = as.numeric(NA),
                                samp.frac.smooth= as.numeric(NA))

YR <- sort(unique(all.chin$year))
AREA <- sort(unique(all.chin$rec.area.code))
for (i in 1:length(YR)){
  for(j in 1:length(AREA)){
    for(k in 1:12){
      temp <- all.chin %>% filter(year==YR[i],rec.area.code==AREA[j]) %>% 
                      filter(month.numb %in% c((k-1):(k+1)))
      if(nrow(temp)>0){
        est.chin <- sum(temp$est.chinook)
        obs.chin <- sum(temp$obs.chinook)
        all.chin <- all.chin %>% 
              mutate(est.chin.smooth = case_when(year==YR[i] & rec.area.code==AREA[j] & month.numb==k ~ est.chin,
                                        TRUE ~ est.chin.smooth),
                    obs.chin.smooth = case_when(year==YR[i] & rec.area.code==AREA[j] & month.numb==k ~ obs.chin,
                                        TRUE ~ obs.chin.smooth)
                    )
      }    
    }
  }
}
      
all.chin <- all.chin %>% mutate(samp.frac.smooth = ifelse(est.chin.smooth>0, obs.chin.smooth/est.chin.smooth,NA))
all.chin$rec.area.code <- factor(all.chin$rec.area.code,levels=c("WAPEN","EAPEN","NWGOA","NEGOA"))

### Add in the values for 2012 and 2013 from AFSC tech memo 

# AFSC tech report 288 Table 3
# Fraction of trips observed for bycatch.
# dockside sampling 2012 jan-june = 0.888, July-Dec = 0.688
# dockside sampling 2013 jan-june = 0.764, July-Dec = 0.775
# Fraction of chinook sampled from the observed bycatch
# all 2012 and 2013 = 0.10

MONTH.early <- c("01","02","03","04","05","06")
MONTH.late  <- c("07","08","09","10","11","12")

all.chin <- all.chin %>% 
  mutate(samp.frac.smooth= 
           case_when(year == 2012 & month %in% c(MONTH.early)~ 0.0888,
                     year == 2012 & month %in% c(MONTH.late)~ 0.0688,
                     year == 2013 & month %in% c(MONTH.early)~ 0.0764,
                     year == 2013 & month %in% c(MONTH.late)~ 0.0775,
                     TRUE ~ samp.frac.smooth))

# there are a small number of rec.area.code-month combinations that have NA sampling fractions.
# almost all of these are incidents in 2008 and 2009 where there are very small amounts of fishing and sampling
# for these we are just going to us the annual average for these sampling fractions

temp <- all.chin %>% filter(year>=2003) %>% 
                group_by(year,rec.area.code) %>% summarise(ann.avg.frac=sum(obs.chinook)/sum(est.chinook))
all.chin <- left_join(all.chin,temp) %>% 
            mutate(samp.frac.smooth=ifelse(is.na(samp.frac.smooth)==T,ann.avg.frac,samp.frac.smooth))

samp.frac.smooth <- ggplot(all.chin,aes(x=year,y=samp.frac.smooth,color=month),alpha=0.5) + 
  geom_point(alpha=0.5) +
  geom_line(alpha=0.2) +
  facet_wrap(~rec.area.code) +
  #lims(y=c(0,1)) +
  theme_bw()

### OK. Plot of Boat Days observed from the very top of this document
join_data_shoreside$rec.area.code <- factor(join_data_shoreside$rec.area.code,levels=c("WAPEN","EAPEN","NWGOA","NEGOA"))

# just like for the observed sampling fraction, smooth out the observed boat days fraction 
join_data_shoreside <- join_data_shoreside %>% mutate(total_effort_smooth = as.numeric(NA),
                                observed_boat_days_smooth = as.numeric(NA))

YR <- sort(unique(join_data_shoreside$year))
AREA <- sort(unique(join_data_shoreside$rec.area.code))
for (i in 1:length(YR)){
  for(j in 1:length(AREA)){
    for(k in 1:12){
      temp <- join_data_shoreside %>% filter(year==YR[i],rec.area.code==AREA[j]) %>% 
        filter(month %in% c((k-1):(k+1)))
      if(nrow(temp)>0){
        tot.days <- sum(temp$total_effort)
        obs.days <- sum(temp$observed_boat_days)
        join_data_shoreside <- join_data_shoreside %>% 
          mutate(total_effort_smooth = case_when(year==YR[i] & rec.area.code==AREA[j] & month ==k ~ tot.days,
                                             TRUE ~ total_effort_smooth),
                 observed_boat_days_smooth = case_when(year==YR[i] & rec.area.code==AREA[j] & month ==k ~ obs.days,
                                             TRUE ~ observed_boat_days_smooth)
          )
      }    
    }
  }
}
join_data_shoreside <- join_data_shoreside %>% mutate(frac_boat_days_obs_smooth = observed_boat_days_smooth /total_effort_smooth)


###Regress the fraction of observed trips against the fraction of observed chinook for 2003-2007

YR <- c(2003:2007)

temp <- left_join(join_data_shoreside %>% filter(year %in% YR),
              all.chin %>% filter(year %in% YR) %>% mutate(month=as.numeric(as.character(month)))) %>% 
          dplyr::select(year,month,rec.area.code,nmfs_area,total_effort,observed_boat_days,frac_boat_days_obs, frac_boat_days_obs_smooth,
                        N.deliveries,est.chinook,obs.chinook,sampling.fraction,samp.frac.smooth)

#temp %>% dplyr::select(year,month,rec.area.code,nmfs_area,frac_boat_days_obs,sampling.fraction) %>% ungroup()

# plot of raw, monthly data
ggplot(join_data_shoreside %>% filter(year>=1997),aes(x=year,y=frac_boat_days_obs,color=as.factor(month)),alpha=0.5) + 
  geom_point(alpha=0.5) +
  geom_line(alpha=0.2) +
  facet_wrap(~rec.area.code) +
  lims(y=c(0,1)) +
  theme_bw()

# plot of smoothed data.
ggplot(join_data_shoreside %>% filter(year>=1997,year<2020),aes(x=year,y=frac_boat_days_obs_smooth,color=as.factor(month)),alpha=0.5) + 
  geom_point(alpha=0.5) +
  geom_line(alpha=0.2) +
  facet_wrap(~rec.area.code) +
#  lims(y=c(0,1)) +
  theme_bw()

# plot of bivariate relationship, raw.
ggplot(temp) +
  geom_point(aes(x=frac_boat_days_obs,y=sampling.fraction,color=as.factor(rec.area.code))) +
  lims(y=c(0,0.4))

# plot of bivariate relationship, smooth
ggplot(temp) +
  geom_point(aes(x=frac_boat_days_obs_smooth,y=samp.frac.smooth,color=as.factor(rec.area.code))) +
  lims(y=c(0,0.4))

# Well that doesn't seem to work.
# Fall back on using the month-rec.area.code average from 2003-2007 for 1997-2002.  
# Average of the smoothed data

int.samp.frac <- all.chin %>% filter(year %in% c(2003:2007)) %>% 
                      group_by(rec.area.code,month) %>% 
                      summarise(samp.frac.smooth.mean=mean(samp.frac.smooth,na.rm = T),
                                samp.frac.smooth.median=median(samp.frac.smooth,na.rm = T))


all.sampling.fraction <- left_join(all.chin,int.samp.frac) %>%
                mutate(final.samp.fraction = ifelse(is.na(samp.frac.smooth)==T,
                                                    samp.frac.smooth.mean,
                                                    samp.frac.smooth))

### Get rid of NA from early years using annual averages just like for 2003 and later
temp <- all.sampling.fraction %>% filter(year<=2002) %>% 
          group_by(year,rec.area.code) %>% summarise(ann.avg.frac.early=mean(samp.frac.smooth.mean,na.rm=T))
all.sampling.fraction <- left_join(all.sampling.fraction,temp) %>% 
  mutate(final.samp.fraction =ifelse(is.na(final.samp.fraction)==T,ann.avg.frac.early,final.samp.fraction))

# simplify the data frame for later use 

sampling.fraction <- all.sampling.fraction %>% 
                            dplyr::select(year,month,month.numb,rec.area.code,nmfs_area,final.samp.fraction)

# One manual adjustment becasue there were CWT reported by no fish purported sampled..
sampling.fraction <- sampling.fraction %>% mutate(final.samp.fraction =
                               case_when(year==2011 & month.numb %in% c(2,3) & rec.area.code=="WAPEN" ~
                                                sampling.fraction %>% filter(year==2011, month.numb %in% c(2,3) ,rec.area.code=="EAPEN") %>% pull(final.samp.fraction) %>% mean(),
                                         TRUE ~ final.samp.fraction))
# MAke a plot
samp.frac.final <-  ggplot(sampling.fraction,aes(x=year,y=final.samp.fraction,color=month),alpha=0.5) + 
  geom_point(alpha=0.5) +
  geom_line(alpha=0.2) +
  facet_wrap(~rec.area.code) +
  #lims(y=c(0,1)) +
  theme_bw()


fish.sampled.tot.v.in.pollock 
samp.frac.raw 
samp.frac.smooth
samp.frac.final

##########################################################################
##########################################################################
##########################################################################
##########################################################################
##### ---- WRITE TO FILE.
##########################################################################
##########################################################################
##########################################################################
##########################################################################

#shoreside only 
#save effort data
 final_shoreside <- join_data_shoreside %>%
  ungroup() %>%
  dplyr::select(year,month,rec.area.code,unobs_boat_days,observed_boat_days,total_effort)  

saveRDS(final_shoreside, paste0(write.dir,"Shoreside_Pollock_GOA_Effort_Summarized.RDS"))

#if there is an observer onboard** [so this is filtered for just observed trips] they sample 1/10 1996 and later, unless specific tender deliver stuff happens 
#save sample fraction data 
saveRDS(sampling.fraction, paste0(write.dir,"Sample_Fraction_Pollock_GOA_Summarized.RDS"))

#################################### MAKE A PDF WITH SOME OF THE FIGURES
# Function stolen from make heatmap.
library(fields)
library(RColorBrewer)
library(viridis)
library(cowplot)
library(pdftools)

plot.heatmap <- function(temp,Title,all.month=TRUE){
  temp <- as.matrix(temp[,3:(ncol(temp))])
  z.lim	<- c(-0.01,max(temp))
  if(max(temp) < -0.01){z.lim	<- c(-0.01,0)}
  
  lab.new <- levels(final_shoreside$rec.area.code)
  x.lab	=	lab.new
  col.br<- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  par(mfrow=c(1,1),oma=c( 0,1,0,4) )
  image(z=temp,x=1:nrow(temp),y=1:length(lab.new),axes=FALSE,ylab="",xlab="",
        col=1,zlim=c(-9999,0))
  image(z=temp,x=1:nrow(temp),y=1:length(lab.new),axes=FALSE,ylab="",xlab="",
        col=col.br(32),zlim=z.lim,add=T)
  box(bty="o",lwd=2)
  axis(2,las=2,at=1:ncol(temp),labels=x.lab)
  if(all.month==TRUE){axis(1,las=2,at=seq(1,nrow(temp),by=12),labels=YRS)}
  #if(all.seas==FALSE){axis(1,las=2,at=seq(1,length(YEARS.RECOVER),by=2),labels=YEARS.RECOVER[seq(1,length(YEARS.RECOVER),by=2)])}
  title(main=Title)
  par(mfrow=c(1,1),oma=c( 0,1,0,0) )
  ticks <- seq(min(z.lim),max(z.lim),length.out=6)
  
  image.plot(temp,legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list( at= ticks    , labels=round(ticks,0)))
}
YRS <- unique(final_shoreside$year) %>% sort()
MTH <- 1:12
temp.eff <- final_shoreside %>% 
  pivot_wider(.,names_from=rec.area.code,values_from = total_effort,id_cols = c("year","month")) %>%
  full_join(.,expand_grid(year=YRS,month=MTH)) %>% arrange(year,month)
temp.eff[is.na(temp.eff)==T] <- -99



###


plot.heatmap.samp.frac <- function(temp,Title){
  
  temp <- as.matrix(temp[,3:(ncol(temp))])
  
  z.lim	<- c(1e-6,1)
  lab.new <- levels(final_shoreside$rec.area.code)
  x.lab	=	lab.new
  col.br<- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  par(mfrow=c(1,1),oma=c( 0,1,0,4) )
  image(z=temp,x=1:nrow(temp),y=1:length(lab.new),axes=FALSE,ylab="",xlab="",
        col=1,zlim=c(-9999,0))
  image(z=temp,x=1:nrow(temp),y=1:length(lab.new),axes=FALSE,ylab="",xlab="",
        col=col.br(32),zlim=z.lim,add=T)
  box(bty="o",lwd=2)
  axis(2,las=2,at=1:ncol(temp),labels=x.lab)
  axis(1,las=2,at=seq(1,nrow(temp),by=12),labels=YRS)
  title(main=Title)
  par(mfrow=c(1,1),oma=c( 0,1,0,0) )
  ticks <- seq(min(z.lim),max(z.lim),length.out=6)
  
  image.plot(temp,legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list( at= ticks, labels=round(ticks,2),0))
}
# YRS <- unique(sampling.fraction$year) %>% sort()
# MTH <- 1:12
temp.samp <- sampling.fraction %>% 
  pivot_wider(.,names_from=rec.area.code,values_from = final.samp.fraction,id_cols = c("year","month")) %>%
  mutate(month=as.numeric(month)) %>%
  full_join(.,expand_grid(year=YRS,month=MTH)) %>% arrange(year,month)
temp.samp[is.na(temp.samp)==T] <- -99



pdf(file=paste0(plot.dir,"Shoreside Pollock GOA.pdf"),onefile=T,height=8.5,width=11)
  
  plot.heatmap( temp=temp.eff,Title="Shoreside Pollock GOA Effort (Boat Days)")
  plot.heatmap.samp.frac( temp=temp.samp,Title="Shoreside Pollock GOA Sample Fraction")
  print(fish.sampled.tot.v.in.pollock)
  #print(samp.frac.raw )
  print(samp.frac.smooth)
  print(samp.frac.final)
dev.off()