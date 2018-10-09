library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
#this script creates dat_recovery which is used in the 'plotting recoveries' script

##Load focal species data
dat_focal = read.csv("dat_focal.csv", stringsAsFactors = FALSE)

#summarize dat_focal to plot recoveries through time by fishery
#summarize the recoveries by tag code (to get amt of individuals recovered?), run_year (reovery year- x axis of time), fishery
recovery_fishery= group_by(dat_focal, run_year)
counts=count(recovery_fishery, fishery) 
#returns the data with column name n, rename to recovery_cts
counts=rename(counts, recovery_cts=n)
#you can use the "counts" dataframe for plotting fish recoveryxfishery through time

#using the 'counts' dataframe, need to categorize 'fishery' variable to make it more interprable
#create categories by gear based on Github/RMIS/RMIS Documentation/PSC_V41_Specification 
#new variable created is called 'gear_type'
counts$gear_type <- as.vector(cut(counts$fishery, breaks=c(-Inf,19,29,39,49,59,69,79,89,Inf), labels=c("troll","net_seine","aboriginal","sport","escapement","test_fishery","juv_sampling","high_seas","misc")))

#lets create a new df with broader categories for initial plotting
#'cts_gear' is the fish counts summed by year for each gear type
cts_gear= counts %>%
  group_by(run_year,gear_type) %>%
  summarise(cts_gear=sum(recovery_cts))


#######create dat_recovery which will be used for further plotting
#it will have states labeled by state abbreviation, fishery gear labeled, and Freshwater filtered out
#first need to get recovery location data extracted from recovery_location_code 
#taking the first # into a seperate variable
dat_recovery= dat_focal %>% 
  separate(recovery_location_code, into = c("state_code", "rest_of_rec_code"), sep = 1)
#take out freshwater recoveries from dat_recovery_state
dat_recovery= dat_recovery %>%
  separate(rest_of_rec_code, into = c("marine_fw", "rest_of_rec_code"), sep = 1) %>%
  filter(marine_fw == 'M')
#add names to fishery gear in dat_recovery_state to make it more interprable
#create categories by gear based on Github/RMIS/RMIS Documentation/PSC_V41_Specification 
#new variable created is called 'gear_type'
dat_recovery$gear_type <- as.vector(cut(dat_recovery$fishery, breaks=c(-Inf,19,29,39,49,59,69,79,89,Inf), 
                                        labels=c("troll",
                                                 "net_seine",
                                                 "aboriginal",
                                                 "sport",
                                                 "escapement",
                                                 "test_fishery",
                                                 "juv_sampling",
                                                 "high_seas",
                                                 "misc")))
#rename state code #s to state names (ie 1-> AK) used info from locaiton.txt
dat_recovery$recovery_state <- recode_factor(dat_recovery$state_code, '1' = "AK",
                                             '2' = "BC",
                                             '3' = "WA",
                                             '4'="ID",
                                             '5'="OR",
                                             '6'="CA",
                                             '7'="HS")
#Split WA and OR so Columbia river is included 
#new column is called release_loc_domain
#codes from Chap 13 in PSC_V41, RMIS documentation, using region codes
dat_recovery$release_loc_domain <- dat_recovery$release_location_rmis_region %>%
  recode(CECR= "COL", CRGN= "COL", LOCR= "COL", UPCR= "COL", SNAK= "COL") %>%
  recode(SEAK= "AK", CNAK= "AK", NOAK= "AK", CEAK= "AK", WEAK= "AK", AKGN= "AK") %>%
  recode(FRTH= "BC", NASK= "BC", GST= "BC", WCVI= "BC", JNST= "BC", COBC= "BC", QCI= "BC", TRAN= "BC", BCGN= "BC") %>%
  recode(GRAY= "WA", HOOD= "WA", JUAN= "WA", MPS= "WA", NOWA= "WA", NWC= "WA", SKAG= "WA", SPS= "WA", NPS= "WA", WILP= "WA", WAGN= "WA") %>%
  recode(NOOR= "OR", SOOR= "OR", ORGN= "OR") %>%
  recode(NOCA= "CA", CECA= "CA",SOCA= "CA",KLTR= "CA", SAFA= "CA", SJOA= "CA", CAGN= "CA") %>%
  recode(LOYR="YR", UPYR= "YR", YRGN= "YR") %>%
  recode(ALSR= "TR", CHIL= "TR", STUN= "TR", TAWH= "TR", TRGN= "TR") 
#BC has a lot of regions that are left blanks, so had to tell it to put BC in those blank areas
dat_recovery$release_loc_domain[dat_recovery$release_loc_domain == ""] <- NA
dat_recovery$release_loc_domain <- dat_recovery$release_loc_domain %>%
  recode(NA= "BC")
#filter out data points that are not 'sample type'== 1
dat_recovery<- dat_recovery %>%
  filter(sample_type=="1")
#only filters out 46,013 data points (which are sample types 2-6, relies on voluntary data where catch may be unknown, likely less reliable estimates)
#dat_recovery has only sample type 1, state, fishery gear, and FW filtered out, domains like columbia river included

write.csv(dat_recovery, "dat_recovery.csv")

#code used to decide if sample type should be filtered
#####################################################
#deciding how sample type should be filtered...
count_sample_type <-dat_recovery %>%
  select(release_loc_domain, sample_type)

p8 = ggplot(data=count_sample_type) + 
  geom_bar(aes(x=sample_type)) + 
  ggtitle("Exploring sample type use and which ones to filter out") + 
  facet_wrap(~release_loc_domain, scales = "free")
p8
#looks like sample type 1 is used the most, and that is the only one that is strictly non-voluntary, so has better data anyway
#going to filter out everything that isnt sample type 1
#only filters out 46,013 data points
####################################################

#combine locations into dataset
# pull in location data from RMIS
locations = read.csv("data/locations.txt", stringsAsFactors = FALSE)
locations = locations[,c("location_code","rmis_latitude","rmis_longitude", "description")]
locations = rename(locations, recovery_location_code = location_code,
                   recovery_description = description, latitude=rmis_latitude, longitude = rmis_longitude)
dat_recovery_loc <- dat_recovery %>% 
  unite(recovery_location_code, state_code, marine_fw, rest_of_rec_code, sep = "")
dat_recovery_loc = left_join(dat_recovery_loc, locations)
#works but duplicates for some reason, need to delete duplicates (they have NA's)
dat_recovery_loc <- dat_recovery_loc %>%
                filter(!is.na(latitude))

#dat_recovery_loc has the recovery location in it now but rows increased a bit so there may still be some duplicates? not sure if this is an issue

