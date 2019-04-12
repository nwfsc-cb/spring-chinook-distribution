
library(tidyverse)
library(maps)
library(mapdata)

#had to take the world map and parce it down by filtering out lats and longs I didnt want since subregions were not in data set
    world <- map_data("world")
    north_america <- subset(world, region %in% c("USA", "Canada"))

    north_am_filter <- north_america %>%
      filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
      filter(!long > -115) %>%
      filter(!lat < 34) %>%
      filter(!lat > 62) %>%
      filter(!long < -173) 
      
#base map
      p_north_am <- ggplot(data = north_am_filter) + 
        geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
        coord_fixed(1.3)+
        theme_bw()
      p_north_am
