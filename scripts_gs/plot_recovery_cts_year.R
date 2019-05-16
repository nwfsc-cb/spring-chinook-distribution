library(tidyverse)
library(RColorBrewer)
library(maps)
library(mapdata)
library(gridExtra)
#this script isolates different fleets and plots how many fish  have recovered by a fleet within a fishery category

dat_recovery = read.csv("dat_recovery.csv", stringsAsFactors = FALSE)

#____________________________________________________________________________________________________
############# HIGH SEAS #############

highseas<- dat_recovery %>% filter(fishery_type=='high_seas')

#sort high seas to get recovery #s by year and fishery to look at available data through time 
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


#just AK highseas fisheries
ak_highseas <- highseas_count %>%
  filter(fishery_name %in% c("Groundfish Observer (Bering Sea)", "Groundfish Observer (Gulf AK)", "Rockfish Fishery (Gulf of AK)" ))

ak_bar <- ggplot(ak_highseas, aes(x=rec_year, y=n)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(fishery_name ~ .,scales = "free_y", labeller=label_wrap_gen(width=.1)) + 
  ggtitle('AK High Seas Recoveries') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Recovery Year', y= 'Instance of Fish recovery') +
  theme_bw()
ak_bar

#__________________________________________________________________________________________
########## SPORT FISHERY #############

sport<- dat_recovery %>% filter(fishery_type=='sport')

#sort to get recovery #s by year and fishery to look at available data through time 
sport_group <- sport %>%
  group_by(rec_year, fishery_name) 
sport_count=count(sport_group, fishery_name) 
sport_spread <- sport_count %>%
  spread(fishery_name, n)
sport_spread[is.na(sport_spread)] <- 0

sp_bar <- ggplot(sport_count, aes(x=rec_year, y=n)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(fishery_name ~ .,scales = "free_y",labeller=label_wrap_gen(width=.1)) +
  ggtitle('Sport Fishery Recoveries') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Recovery Year', y= 'Instance of Fish recovery')
sp_bar

#__________________________________________________________________________________________
###################### NET ################

net <- dat_recovery %>%
  filter(fishery_type=='net_seine')

#sort to get recovery #s by year and fishery to look at available data through time 
net_group <- net %>%
  group_by(rec_year, fishery_name) 
net_count=count(net_group, fishery_name) 
net_spread <- net_count %>%
  spread(fishery_name, n)
net_spread[is.na(net_spread)] <- 0

n_bar <- ggplot(net_count, aes(x=rec_year, y=n)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(fishery_name ~ .,scales = "free_y",labeller=label_wrap_gen(width=.1)) +
  ggtitle('Net Fishery Recoveries') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Recovery Year', y= 'Instance of Fish recovery')
n_bar

#__________________________________________________________________________________________
############# TROLL ################
troll <- dat_recovery %>%
  filter(fishery_type=="troll")

#sort to get recovery #s by year and fishery to look at available data through time 
troll_group <- troll %>%
  group_by(rec_year, fishery_name) 
troll_count=count(troll_group, fishery_name) 
troll_spread <- troll_count %>%
  spread(fishery_name, n)
troll_spread[is.na(troll_spread)] <- 0

t_bar <- ggplot(troll_count, aes(x=rec_year, y=n)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(fishery_name ~ .,scales = "free_y",labeller=label_wrap_gen(width=.1)) +
  ggtitle('Troll Fishery Recoveries') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Recovery Year', y= 'Instance of Fish recovery')
t_bar


g<-grid.arrange(hs_bar, sp_bar, n_bar, t_bar, nrow(2), ncol(2))
g
pdf(file="fleetcts_year.pdf", width=13, height=8.5); print(g); dev.off()







