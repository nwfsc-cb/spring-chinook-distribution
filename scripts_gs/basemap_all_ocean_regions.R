library(tidyverse)
library(here)

df = read.csv("Maps_and_Charts_of_Regions/spatial_bounds_gs_new.csv", stringsAsFactors = FALSE) 

#This map plots the spatial boxes that we assign effort and recovery too. 

world <- map_data("world")
north_america <- subset(world, region %in% c("USA", "Canada"))

WC_map<-north_america %>%
  filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
  filter(!long > -118) %>%
  filter(!lat < 35) %>%
  filter(!lat > 63) %>%
  filter(!long < -173) %>%
  ggplot( ) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "lightgray") +
  coord_fixed(1.3) +
  geom_segment(data = df, colour="black", size =.1, 
               aes(x = as.numeric(line.start.lon), 
                                             y = as.numeric(line.start.lat), 
                                             xend = as.numeric(line.end.lon), 
                                             yend = as.numeric(line.end.lat))) +
  #  geom_text(data=df, colour="darkgreen",aes(x=label_long1, y= label_lat1, label= region), size=2)+ #smaller labels for CA, OR, and WA so they fit
  geom_text(data=df, colour="darkgreen",aes(x=as.numeric(label_long2), y=as.numeric(label_lat2), label= region), size=2) +
  geom_text(data=df, colour="darkgreen",aes(x=as.numeric(label_long1), y=as.numeric(label_lat1), label= region), size=1.2) +
  
  theme(#plot.margin = margin(0,2,0,.01, "inch"), #(top,right,bottom,left)
    plot.background = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    legend.text = element_text(size = 7),
    legend.title.align=0.5, 
    legend.title=element_text(size=7),
    legend.key = element_blank(), 
    legend.background=element_blank(),
    legend.box.background = element_blank(),
    legend.position = c(1.2, 0.3),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    #axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    #axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')
  )
WC_map

pdf("output/all_ocean_regions_map.pdf", width=13, height=8.5); print(WC_map); dev.off()

