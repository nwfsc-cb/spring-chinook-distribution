library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(dplyr)

#work on tile plots to get a better of idea of missing months.
#also work on spatial stuff 

dat_recovery = read.csv("dat_recovery.csv", stringsAsFactors = FALSE)

#use this to get an idea of how data varies seasonally for each fishery

highseas<- dat_recovery %>% filter(fishery_type=='high_seas')

highseas_group <- highseas %>%
  group_by(rec_year, rec_month, fishery_name)
highseas_count=count(highseas_group, fishery_name) 

highseas_count$n <- as.numeric(highseas_count$rec_month)


#get counts of sampling instances/month/fishery
highseas_group <- highseas %>%
  group_by(rec_month)
highseas_count=count(highseas_group, fishery_name) 
highseas_count <- filter(highseas_count, !is.na(rec_month))
highseas_count$rec_month <- as.factor(highseas_count$rec_month)


h_szn_bar <- ggplot(highseas_count, aes(x=rec_month, y=n)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(fishery_name ~ .,scales = "free_y",labeller=label_wrap_gen(width=.1)) +
  ggtitle('Highseas Recovery by Month (counts through all years)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Recovery Month', y= 'Instance of Fish Recovery (Counts)')+
  theme_bw()
h_szn_bar

#_________________________________________________________________________________________
    #exploring specific fleets by year
          #GROUNDFISH GOA#
groundfish_goa<- highseas %>%
  filter(fishery_name == 'Groundfish Observer (Gulf AK)')

groundfish_goa_group <- groundfish_goa %>%
  group_by(rec_year, rec_month)
groundfish_goa_ct=count(groundfish_goa_group, fishery_name) 
groundfish_goa_ct <- filter(groundfish_goa_ct, !is.na(rec_month))
groundfish_goa_ct$rec_month <- as.factor(groundfish_goa_ct$rec_month)

gf_goa <- ggplot(groundfish_goa_ct, aes(x=rec_month, y=n)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(rec_year ~ .,scales = "free_y",labeller=label_wrap_gen(width=1)) +
  ggtitle('Groundfish GOA') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Recovery Month', y= 'Instance of Fish Recovery (Counts)')+
  theme_bw()
gf_goa

#_________________________________________________________________________________________
        #exploring specific fleets by year
                #GROUNDFISH BSAI
groundfish_bsai<- highseas %>%
  filter(fishery_name == 'Groundfish Observer (Bering Sea)')

groundfish_bsai_group <- groundfish_bsai %>%
  group_by(rec_year, rec_month)
groundfish_bsai_ct=count(groundfish_bsai_group, fishery_name) 
groundfish_bsai_ct <- filter(groundfish_bsai_ct, !is.na(rec_month))
groundfish_bsai_ct$rec_month <- as.factor(groundfish_bsai_ct$rec_month)

gf_bsai <- ggplot(groundfish_bsai_ct, aes(x=rec_month, y=n)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(rec_year ~ .,scales = "free_y",labeller=label_wrap_gen(width=1)) +
  ggtitle('Groundfish BSAI') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Recovery Month', y= 'Instance of Fish Recovery (Counts)')+
  theme_bw()
gf_bsai

#_________________________________________________________________________________________
      #exploring specific fleets by year
            #HAKE TRAWL AT SEA
hake<- highseas %>%
  filter(fishery_name == 'Hake Trawl At Sea (CA OR WA)')

hake_group <- hake %>%
  group_by(rec_year, rec_month)
hake_ct=count(hake_group, fishery_name) 
hake_ct <- filter(hake_ct, !is.na(rec_month))
hake_ct$rec_month <- as.factor(hake_ct$rec_month)

hake <- ggplot(hake_ct, aes(x=rec_month, y=n)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(rec_year ~ .,scales = "free_y",labeller=label_wrap_gen(width=1)) +
  ggtitle('Hake At Sea') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Recovery Month', y= 'Instance of Fish Recovery (Counts)')+
  theme_bw()
hake

p <- hake_ct %>%
  ggplot(aes(rec_month, rec_year)) + 
  geom_tile(aes(fill = n), colour = "white") +
  ggtitle('Hake') +
  labs(x=' ', y= ' ') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

#_________________________________________________________________________________________
              #SPORT

sport<- dat_recovery %>% filter(fishery_type=='sport')

sport_group <- sport %>%
  group_by(rec_month)
  sport_count=count(sport_group, fishery_name) 
  sport_count <- filter(sport_count, !is.na(rec_month))
  sport_count$rec_month <- as.factor(sport_count$rec_month)


s_bar <- ggplot(sport_count, aes(x=rec_month, y=n)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(fishery_name ~ .,scales = "free_y",labeller=label_wrap_gen(width=.1)) +
  ggtitle('Sport Recovery by Month (counts through all years)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Recovery Month', y= 'Instance of Fish Recovery (Counts)')+
  theme_bw()
s_bar

#_________________________________________________________________________________________
              #TROLL
troll <- dat_recovery %>%
  filter(fishery_type=='troll')

troll_group <- troll %>%
  group_by(rec_month)
troll_count=count(troll_group, fishery_name) 
troll_count <- filter(troll_count, !is.na(rec_month))
troll_count$rec_month <- as.factor(troll_count$rec_month)

t_bar <- ggplot(troll_count, aes(x=rec_month, y=n)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(fishery_name ~ .,scales = "free_y",labeller=label_wrap_gen(width=.1)) +
  ggtitle('Troll Recovery by Month (counts through all years)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Recovery Month', y= 'Instance of Fish Recovery (Counts)')+
  theme_bw()
t_bar
#_________________________________________________________________________________________
#NET
net <- dat_recovery %>%
  filter(fishery_type=='net_seine')

net_group <- net %>%
  group_by(rec_month)
net_count=count(net_group, fishery_name) 
net_count <- filter(net_count, !is.na(rec_month))
net_count$rec_month <- as.factor(net_count$rec_month)
n_bar <- ggplot(net_count, aes(x=rec_month, y=n)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(fishery_name ~ .,scales = "free_y",labeller=label_wrap_gen(width=.1)) +
  ggtitle('Net Recovery by Month (counts through all years)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Recovery Month', y= 'Instance of Fish Recovery (Counts)')+
  theme_bw()
n_bar
