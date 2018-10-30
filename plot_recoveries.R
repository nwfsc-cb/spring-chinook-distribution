library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)

#use dat_recovery for these graphs, already created

######################
#summarize the recoveries by tag code, run_year (reovery year- x axis of time), fishery, state code
counts_gear_state= dat_recovery %>%
  group_by(run_year, recovery_state) %>%
  count(gear_type) 
#returns the data with column name n, rename to recovery_cts
counts_gear_state=rename(counts_gear_state, recovery_cts=n)
#need to sum by 'gear type' category, right now it is summed by 'fishery' numbers
counts_gear_state= counts_gear_state %>%
  group_by(run_year,recovery_state, gear_type) %>%
  summarise(recovery_cts=sum(recovery_cts))
#################    

#Now graph p1, which is chinook recoveries through time X fishery gear type
p1 = ggplot(data=cts_gear) + 
          geom_line(aes(x=run_year, y=cts_gear, group=gear_type, linetype=gear_type, color=gear_type)) + 
          labs(x= "Run Year", y= "Salmon Recovery Counts") + 
          ggtitle("Salmon Recovery Counts through Time x Gear Type")
p1
#escapement has largest amount of recoveries, then generally troll, sport and net seine

#Now graph p2 which is chinook recoveries through time X fishery gear type X wrapped by state
p2 = ggplot(data=counts_gear_state) + 
  geom_line(aes(x=run_year, y=recovery_cts, group=gear_type, linetype=gear_type, color=gear_type)) + 
  labs(x= "Run Year", y= "Salmon Recovery Counts") + 
  ggtitle("Salmon Recovery Counts through Time x Gear Type X State") + 
  facet_wrap(~recovery_state, scales = "free")
p2
#sharp drop in 1995 in every state aside from high seas (no data from that time)? something missing in data?? or was there some weird event?
########################


#######################
#Now plot salmon recovery by estimation instead of counts
#first summarize estimation similiar to above
#use dat_recovery, should already have state code applied and freshwater filtered out if you ran code from above

#summarize the recoveries by sums of estimated number code (to get haul level estimations of recovery), run_year (reovery year- x axis of time), fishery, state code
estimated_gear_state= dat_recovery %>%
  group_by(run_year, recovery_state, gear_type) %>%
  summarise(estimated_number=sum(estimated_number))
estimated_gear_state$gear_type <- as.factor(estimated_gear_state$gear_type)

#same as P2 but now use estimated recovery counts
p3 = ggplot(data=estimated_gear_state) + 
  geom_line(aes(x=run_year, y=estimated_number, group=gear_type, linetype=gear_type, color=gear_type)) + 
  labs(x= "Run Year", y= "Estimated Recovery Numbers") + 
  ggtitle("Estimated Recovery Numbers through Time x Gear Type X State") + 
  facet_wrap(~recovery_state, scales = "free")
p3
#p3 doesnt show hugely different patterns from p2 but highseas has a different pattern
#CA and WA variance is greater for 90's in estimated plots than in counts plot

#should also make a graph that shows how estimation levels vary by state or something to indicate if all levels should be included or not 
#looking at how estimation levels used vary by state and also by fishery
    #first look at how they vary by state
    #use count command to get frequency estimation level use
dat_recovery$estimation_level <- as.factor(dat_recovery$estimation_level)
est_level_count= dat_recovery %>%
  group_by(run_year, recovery_state, gear_type) %>%
  count(estimation_level) 
#graph it
p4 = ggplot(data=est_level_count) + 
  geom_line(aes(x=run_year, y=n, group=estimation_level, linetype=estimation_level, color=estimation_level)) + 
  labs(x= "Run Year", y= "frequency of est level occurence") + 
  ggtitle("how do estimation levels vary by state?") + 
  facet_wrap(~recovery_state, scales = "free")
p4
#the estimation levels used are different by the state the fish was recovered in
#high seas use really broad estimates (level 2)
#BC and CA generally use 3
#WA and OR are a little more specific using levels 4 and 5
#AK used to use mainly 3, now uses a mix of 3,4,5

#now look at how they vary by gear type
#graph it
p5 = ggplot(data=est_level_count) + 
  geom_line(aes(x=run_year, y=n, group=estimation_level, linetype=estimation_level, color=estimation_level)) + 
  labs(x= "Run Year", y= "frequency of est level occurence") + 
  ggtitle("how do estimation levels vary by gear type?") + 
  facet_wrap(~gear_type, scales = "free")
p5
# I think state information is more important than gear type....
#troll is mainly 3,4,5 mix
#sport is 6

##############could filter out level 2, this would filter out the high seas mainly
#graph recoveries by filter


######################
#now want to look at how recovery estiamtes change based on release and recovery locations (state level)
#first need to summarize recovery estimates and group by locations
#release_loc_domain looks at release locations by state and also columbia river seperately
#removed High Seas recovery location because that is estimation level 2 (the only one)
recover_release_estimation= dat_recovery %>%
  group_by(run_year, recovery_state, release_loc_domain) %>%
  summarise(estimated_number=sum(estimated_number))
recover_release_estimation = recover_release_estimation %>%
  filter(!is.na(release_loc_domain)) %>%
  filter(!release_loc_domain=="TR") %>%
  filter(!release_loc_domain=="YR") %>%
  filter(!recovery_state=="HS")
  
#graph it
p7 = ggplot(recover_release_estimation) + 
  geom_line(aes(x=run_year, y=estimated_number, group=recovery_state, linetype=recovery_state, color=recovery_state)) + 
  labs(x= "Run Year", y= "Estimated Recovery Numbers") + 
  ggtitle("Where are salmon being recovered based on release location?") + 
  facet_wrap(~release_loc_domain, scales = "free")
p7

#looks like a majoirty of fish are being recovered in the same state they are being released in, especially alaska
#if its not in their state, then they go North by a state or 2


#################################HAVENT FINISHED THIS...COME BACK TO IT?
#looking at how filtering by different estimation levels changes p3
#not going to include est 2 because that is just high seas.
estimated_gear_state_fltr_3 <- dat_recovery %>%
  filter(estimation_level == "3")
#summarize the recoveries by sums of estimated number code (to get haul level estimations of recovery), run_year (reovery year- x axis of time), fishery, state code
estimated_gear_state_fltr_3= estimated_gear_state_fltr_3 %>%
  group_by(run_year, recovery_state, gear_type) %>%
  summarise(estimated_number=sum(estimated_number))
estimated_gear_state_fltr_3$gear_type <- as.factor(estimated_gear_state_fltr_3$gear_type)

#graph recoveries for each estimation level to see how that changes results
p3 = ggplot(data=estimated_gear_state_fltr_3) + 
  geom_line(aes(x=run_year, y=estimated_number, group=gear_type, linetype=gear_type, color=gear_type)) + 
  labs(x= "Run Year", y= "Estimation Level 3 Recovery Numbers") + 
  ggtitle("Estimation Level 3 Recovery Numbers through Time x Gear Type X State") + 
  facet_wrap(~recovery_state, scales = "free")
p3
###################################

#looking at fish recovery locaiton based on age
#maybe older fish get recovered farther from their release location
#to get fish age at recovery subtract brood_year from run_year
dat_recovery$recovery_age <- (dat_recovery$run_year - dat_recovery$brood_year)
dat_recovery <- dat_recovery %>% 
            filter(recovery_age >0)

#release_loc_domain looks at release locations by state and also columbia river seperately
recover_release_estimation= dat_recovery %>%
  group_by(run_year, recovery_state, release_loc_domain) %>%
  summarise(estimated_number=sum(estimated_number))
recover_release_estimation = recover_release_estimation %>%
  filter(!is.na(release_loc_domain)) %>%
  filter(!release_loc_domain=="TR") %>%
  filter(!release_loc_domain=="YR")

#graph it
p7 = ggplot(recover_release_estimation) + 
  geom_line(aes(x=run_year, y=estimated_number, group=recovery_state, linetype=recovery_state, color=recovery_state)) + 
  labs(x= "Run Year", y= "Estimated Recovery Numbers") + 
  ggtitle("Where are salmon being recovered based on release location?") + 
  facet_wrap(~release_loc_domain, scales = "free")
p7


#######  
#work on better spatial graphs. 
#do recoveries by release areas (p7) but smaller spatial scales...
#try filtering out by estimation level- definitely 2, maybe not the others?? 
#to figure this out maybe make p7 or p3 (probably p3) over again but one for each estimation level



