library(tidyverse)
library(here)
#11-14 NOTES:
#Check with Ole about location of Bering. if it stays vs if we move it to be 90 degrees with X610 doesnt matter in terms of recoveries 


df = read.csv(paste0(base.dir,"/spring-chinook-distribution/Maps_and_Charts_of_Regions/spatial_bounds_gs_new.csv"), stringsAsFactors = FALSE) %>%
      filter(!region %in% c("ALEUT","BER","COR"))

#This map plots the spatial boxes that we assign effort and recovery too. 

world <- map_data("world")
north_america <- subset(world, region %in% c("USA", "Canada"))

map<-north_america %>%
    # filter(!group== 1511, 
    #        !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
    filter(!long > -120) %>%
    filter(!lat < 34) %>%
    filter(!lat > 62) %>%
    filter(!long < -170) %>%
    ggplot( ) +
    geom_polygon(aes(x = long, y = lat, group = group), fill = "lightgray") +
    coord_fixed(1.3) +
    geom_segment(data = df, colour="gray", aes(x = as.numeric(line.start.lon), 
                                               y = as.numeric(line.start.lat), 
                                               xend = as.numeric(line.end.lon), 
                                               yend = as.numeric(line.end.lat))) +
#  geom_text(data=df, colour="darkgreen",aes(x=label_long1, y= label_lat1, label= region), size=2)+ #smaller labels for CA, OR, and WA so they fit
  geom_text(data=df, colour="darkgreen",aes(x=as.numeric(label_long2), y=as.numeric(label_lat2), label= region), size=2) +
  scale_x_continuous("Longitude",expand=c(0.0,0),limits=c(-170.1,-119.7)) +
  scale_y_continuous("Latitude",expand=c(0.0,0),limits=c(35.,62)) +
  theme_classic()
map

pdf( paste0(base.dir,"/spring-chinook-distribution/Maps_and_Charts_of_Regions/All_regions_map.pdf"), 
      width=8, height=6); print(map); dev.off()

# 
# geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
#   coord_fixed(1.3)+
#   geom_point(data = trawl_lat_long, mapping = aes(x = longitude, y = latitude, color = sector)) +
#   theme_bw()
