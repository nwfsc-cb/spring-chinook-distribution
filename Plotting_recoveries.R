library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)

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
      
# Now graph p1, which is chinook recoveries through time X fishery gear type
p = ggplot(data=cts_gear) + geom_line(aes(x=run_year, y=cts_gear, group=gear_type, linetype=gear_type, color=gear_type)) + labs(x= "Run Year", y= "Salmon Recovery Counts") + ggtitle("Salmon Recovery Counts through Time x Gear Type")
p




#############################################
#now lets get more detail and plot recoveries by state and gear type
#first need to get recovery location data extracted from recovery_location_code 
#taking the first # into a seperate variable
dat_recovery_state= dat_focal %>% 
  separate(recovery_location_code, into = c("state_code", "rest_of_rec_code"), sep = 1)
#summarize the recoveries by tag code (to get amt of individuals recovered?), run_year (reovery year- x axis of time), fishery, state code
counts_gear_state= dat_recovery_state %>%
  group_by(run_year, state_code) %>%
  count(fishery) 
#returns the data with column name n, rename to recovery_cts
counts_gear_state=rename(counts_gear_state, recovery_cts=n)

#using the 'counts_gear_state' dataframe, need to categorize 'fishery' variable to make it more interprable
#create categories by gear based on Github/RMIS/RMIS Documentation/PSC_V41_Specification 
#new variable created is called 'gear_type'
counts_gear_state$gear_type <- as.vector(cut(counts_gear_state$fishery, breaks=c(-Inf,19,29,39,49,59,69,79,89,Inf), labels=c("troll","net_seine","aboriginal","sport","escapement","test_fishery","juv_sampling","high_seas","misc")))

#need to sum by 'gear type' category, right now it is summed by 'fishery' numbers
counts_gear_state= counts_gear_state %>%
  group_by(run_year,state_code, gear_type) %>%
  summarise(recovery_cts=sum(recovery_cts))
#rename state code #s to state names (ie 1-> AK) used Github/RMIS/RMIS Documentation/PSC_V41_Specification page 73 in document "Geographic Coding" for codes and abbreviations
counts_gear_state$state <- recode_factor(counts_gear_state$state_code, '1' = "AK",
                          '2' = "YR",
                          '3' = "BC",
                          '4'="WA",
                          '5'="CR",
                          '6'="OR",
                          '7'="CA",
                          '8'="TR")

#Now graph p2 which is chinook recoveries through time X fishery gear type X wrapped by state
p2 = ggplot(data=counts_gear_state) + 
  geom_line(aes(x=run_year, y=recovery_cts, group=gear_type, linetype=gear_type, color=gear_type)) + 
  labs(x= "Run Year", y= "Salmon Recovery Counts") + 
  ggtitle("Salmon Recovery Counts through Time x Gear Type X State") + 
  facet_wrap(~state, scales = "free")
p2
#what is happening in 1995 in Alaska and Columbia River 
#what does escapement mean?



#dont need here but this is how to change numeric --> character
##dat_recovery_state$state_code <- as.numeric(as.character(dat_recovery_state$state_code))
