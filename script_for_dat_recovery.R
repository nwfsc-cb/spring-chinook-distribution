library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
#this script creates dat_recovery which is used in the 'plotting recoveries' script

##Load focal species data
dat_focal = read.csv("dat_focal.csv", stringsAsFactors = FALSE)

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
  recode('NA' = "BC")
#filter out data points that are not 'sample type'== 1
#dat_recovery<- dat_recovery %>%
#  filter(sample_type=="1")
#only filters out 46,013 data points (which are sample types 2-6, relies on voluntary data where catch may be unknown, likely less reliable estimates)


#dat_recovery has state, fishery gear, and FW filtered out, domains like columbia river included

write.csv(dat_recovery, "dat_recovery.csv")

