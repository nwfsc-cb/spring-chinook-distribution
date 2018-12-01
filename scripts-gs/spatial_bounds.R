library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(plyr)
library(dplyr)
library(maps)
library(mapdata)

#THIS SCRIPT WILL CREATE A TABLE SHOWING COUNTS OF HOW MANY FISH APPEAR IN EACH STATISTICAL AREA PER YEAR
      #IDEALLY THIS WILL HELP ESTABLISH SPATIAL BOUNDS
#use blakes file: bycatch_location_points_with_stat_areas

df<- bycatch_location_points_with_statistical_area_attributes

df$REC_YEAR<- as.character(df$REC_YEAR)

df<- df %>%
  group_by(REC_YEAR, FISH_TYPE, REGISTRATI, REGISTRA_3) 

df_ct <-count(df,FISH_NAME) 


#wating to get map from blake before doing anything else that will help me organize stat areas from east to west on a geom tile 