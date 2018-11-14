library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(maps)
library(mapdata)
#this script isolates different fleets and plots how many fish they have recovered by fishery within a fleet

dat_recovery = read.csv("dat_recovery.csv", stringsAsFactors = FALSE)

dat_recovery$fishery <- as.numeric(dat_recovery$fishery)
dat_recovery<- dat_recovery %>% 
  separate(recovery_date, into = c("rec_year", "rest_of_rec_code"), sep = 4) 

############# HIGH SEAS #############
highseas <- dat_focal %>%
  filter(fishery %in% c(80, 800, 802, 803, 804, 805, 81, 812, 82, 83, 84, 85, 87, 88, 89))

#rename highseas
#based on RMIS Documentation 
highseas$fishery_name <- recode_factor(highseas$fishery, '80' = "Hake Trawl At Sea",
                                       '800' = "Hake Trawl Shoreside",
                                       '802' = "Rockfish Trawl",
                                       '803'="Groundfish Trawl",
                                       '804'="Sablefish Fixed Gear",
                                       '805'="Nearshore Groundfish",
                                       '81'="Groundfish Observer",
                                       '812'="Rockfish Fishery",
                                       '82'="Groundfish Observer",
                                       '83'="Foreign Research Vessels",
                                       '84'="Foreign Mothership",
                                       '85'="Ocean Trawl By-catch",
                                       '87'="Squid Gillnet By-catch", 
                                       '88'="Juv Sampling",
                                       '89'="Other")

#sort high seas to get recovery #s by year and fishery to look at available data through time (NOT space)
highseas_group <- highseas %>%
  group_by(rec_year, fishery_name) 
highseas_count=count(highseas_group, fishery_name) 
highseas_spread <- highseas_count %>%
  spread(fishery_name, n)
highseas_spread[is.na(highseas_spread)] <- 0

hs_bar <- ggplot(highseas_count, aes(x=rec_year, y=n)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(fishery_name ~ .,scales = "free_y",labeller=label_wrap_gen(width=.1)) +
  ggtitle('High Seas Recoveries') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Recovery Year', y= 'Instance of Fish recovery')
hs_bar

write.csv(highseas_spread, "highseas.csv")
