library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(maps)
library(mapdata)
#this script isolates different fleets and plots locations where they have recovered fish on map
#uses dat_recovery_loc

#General Notes
    #HIGH SEAS
#no high seas recovery in BC
#Groundfish observer (orange) pollock pretty consistent through time until 2008, very few recoveries until 2012, sill low recoveries
#hake fairly consistent, At Sea contracts a fair bit 1992- 1999
#hake shoreside is pretty minimal through time, only shows up in 2015
    #SPORT
#most of the sport is Ocean Sport, mostly in BC, CAN. 
#sparse in BC until 1982
#sparse to non-existent everywhere before 1973
#none in OR south in 1993, some in 1994, 2008, 2009 sparse
    #NET
#Nothing south of the Columbia
#big decrease in BC in 1999 (a few sparse recoveries[2000-2008], virtually none 2008-2016)
  #TROLL
#more recoveries appear in South of SF after 1980s
#day boat ocean troll only in BC

dat_recovery_loc = read.csv("dat_recovery_loc.csv", stringsAsFactors = FALSE)

########### Recoveries through time and space
#base map
#had to take the world map and parce it down by filtering out lats and longs I didnt want since subregions were not in map_data 
world <- map_data("world")
north_america <- subset(world, region %in% c("USA", "Canada"))

north_am_filter <- north_america %>%
  filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
  filter(!long > -115) %>%
  filter(!lat < 36) %>%
  filter(!lat > 62) %>%
  filter(!long < -173) 

#plot base map
p_north_am <- ggplot(data = north_am_filter) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3)+
  theme_bw()
p_north_am

######################
############# HIGH SEAS #############
highseas <- dat_recovery_loc %>%
  filter(fishery_type == 'high_seas')
  
## COLORS ##
#########create color scale so fisheries always have same color
highseas$fishery_name <- as.factor(highseas$fishery_name)

library(RColorBrewer)
myColors <- brewer.pal(9,"Set3")
names(myColors) <- levels(highseas$fishery_name)
colScale <- scale_colour_manual(name = "Fishery",values = myColors)

#plot points onto base map
df <- highseas

h <-  p_north_am +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = fishery_name)) +
  scale_alpha(guide = 'none')+
  colScale+
  ggtitle("High Seas Recovery (1981-2016)") +
# labs(caption = "N") +
theme_bw() 
h

h_yr <-  p_north_am +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = fishery_name)) +
  scale_alpha(guide = 'none')+
  colScale+
  facet_wrap(~rec_year)+
  ggtitle("High Seas Recovery X Time") +
  theme_bw() 
h_yr

h_5yr <-  p_north_am +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = fishery_name)) +
  scale_alpha(guide = 'none')+
  colScale+
  facet_wrap(~run_year_five)+
  ggtitle("High Seas Recovery (Five Year Interval)") +
  theme_bw() 
h_5yr

#no high seas recovery in BC
#Groundfish observer (orange) pollock pretty consistent through time until 2008, very few recoveries until 2012, sill low recoveries
#hake fairly consistent, At Sea contracts a fair bit 1992- 1999
#hake shoreside is pretty minimal through time, only shows up in 2015

########## SPORT FISHERY #############
sport <- dat_recovery_loc %>%
  filter(fishery_type == 'sport')
## COLORS ##
#########create color scale so fisheries always have same color
sport$fishery_name <- as.factor(sport$fishery_name)

library(RColorBrewer)
myColors <- brewer.pal(6,"Set3")
names(myColors) <- levels(sport$fishery_name)
colScale <- scale_colour_manual(name = "Fishery",values = myColors)

#plot points onto base map
df <- sport

s <-  p_north_am +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = fishery_name)) +
  scale_alpha(guide = 'none')+
  colScale+
  ggtitle("Sport Recovery (1973-2016)") +
  # labs(caption = "N") +
  theme_bw() 
s

s_5yr <-  p_north_am +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = fishery_name)) +
  scale_alpha(guide = 'none')+
  colScale+
  facet_wrap(~run_year_five)+
  ggtitle("Sport Recovery (Five Year Interval)") +
  theme_bw() 
s_5yr

s_yr <-  p_north_am +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = fishery_name)) +
  scale_alpha(guide = 'none')+
  colScale+
  facet_wrap(~rec_year)+
  ggtitle("Sport Recovery X Year") +
  theme_bw() 
s_yr
#most of the sport is Ocean Sport, mostly in BC, CAN. 
#sparse in BC until 1982
#sparse to non-existent everywhere before 1973
#none in OR south in 1993, some in 1994, 2008, 2009 sparse


############# NET ################
net <- dat_recovery_loc %>%
  filter(fishery_type == 'net')

### COLORS 
net$fishery_name <- as.factor(net$fishery_name)

library(RColorBrewer)
myColors <- brewer.pal(7,"Set3")
names(myColors) <- levels(net$fishery_name)
colScale <- scale_colour_manual(name = "Fishery",values = myColors)

#plot points onto base map
df <- net

n <-  p_north_am +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = fishery_name)) +
  scale_alpha(guide = 'none')+
  colScale+
  ggtitle("Net Recovery (1974-2016)") +
  # labs(caption = "N") +
  theme_bw() 
n

n_5yr <-  p_north_am +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = fishery_name)) +
  scale_alpha(guide = 'none')+
  colScale+
  facet_wrap(~run_year_five)+
  ggtitle("Net Recovery (Five Year Interval)") +
  theme_bw() 
n_5yr

n_yr <-  p_north_am +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = fishery_name)) +
  scale_alpha(guide = 'none')+
  colScale+
  facet_wrap(~rec_year)+
  ggtitle("Net Recovery X Year") +
  theme_bw() 
n_yr

#Nothing south of the Columbia
#stops in BC in 1999 (a few sparse recoveries[2000-2003] but virtually none 1999-2016)

#_________________________________________________________________________________________________________________
############# TROLL ################
troll <- dat_recovery_loc %>%
  filter(fishery_type == 'troll')

### COLORS 
troll$fishery_name <- as.factor(troll$fishery_name)

library(RColorBrewer)
myColors <- brewer.pal(10,"Set3")
names(myColors) <- levels(troll$fishery_name)
colScale <- scale_colour_manual(name = "Fishery",values = myColors)

#plot points onto base map
df <- troll

t <-  p_north_am +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = fishery_name)) +
  scale_alpha(guide = 'none')+
  colScale+
  ggtitle("Troll Recovery (1974-2016)") +
  # labs(caption = "N") +
  theme_bw() 
t

t_5yr <-  p_north_am +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = fishery_name)) +
  scale_alpha(guide = 'none')+
  colScale+
  facet_wrap(~run_year_five)+
  ggtitle("Troll Recovery (Five Year Interval)") +
  theme_bw() 
t_5yr

t_yr <-  p_north_am +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = fishery_name)) +
  scale_alpha(guide = 'none')+
  colScale+
  facet_wrap(~rec_year)+
  ggtitle("Troll Recovery X Year") +
  theme_bw() 
t_yr

#print all plots to a PDF

pall <- list(h, h_5yr, h_yr, s, s_5yr, s_yr, n, n_5yr, n_yr, t, t_5yr, t_yr) 
pdf(file="fleets_maps.pdf", width=13, height=8.5); print(pall); dev.off()




