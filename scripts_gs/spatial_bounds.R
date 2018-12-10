library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(plyr)
library(dplyr)
library(maps)
library(mapdata)

#THIS SCRIPT WILL CREATE A TILE PLOT SHOWING COUNTS OF HOW MANY FISH APPEAR IN EACH STATISTICAL AREA PER YEAR AND SEASON
      #IDEALLY THIS WILL HELP ESTABLISH SPATIAL BOUNDS
#use blakes file: bycatch_location_points_with_stat_areas

gis <- dat_recovery_loc
gis$ID_NO <- 1:nrow(gis) 

gis<- gis %>% 
  dplyr:: select(ID_NO, rec_season) #JOIN SO YOU CAN HAVE REC_SEASON IN THERE TOO

df<-  inner_join(bycatch_location_points_with_statistical_area_attributes, gis, by = "ID_NO")

##__________________________________________________________________________________________________
#PARCING FOR TILE PLOT

df$REC_YEAR<- as.character(df$REC_YEAR)

df<- df %>%
  group_by(REC_YEAR, FISH_TYPE, REGISTRATI, REGISTRA_3) 

df_ct <-count(df,FISH_NAME) 

#recode factor
df_ct$Region <- recode_factor(df_ct$REGISTRA_3, 
'KOD' = "KOD",
'CIFW'= "KOD",
'NGSW'="KOD",
'CISW'="KOD", #combine cook inlet with kodiak
'PWSE' ="PWS", #combine all prince william sound 
'PWSF'="PWS",
'PWSI'="PWS",
'IBS'="YAK",
'EYKT' = "YAK",
'CSEO'="YAK",
'NSEI'= "YAK",
'NSEO' = "YAK",
'BSEA' ="BER",
'AISD' = "ALEUT",
'MSAPW' = "ALEUT",
'MSAPE'="PEN",
'LCHIG'="PEN",
'SSEI'= "NSEAK",
'SSEO'="SSEAK") #change IBS to WYAK, for W of Yakatat

#Make levels from NW to SE so they plot better
df_ct <- df_ct %>% mutate(Region = factor(Region, levels = c("BER",
                                                                    "ALEUT",
                                                                    "PEN", 
                                                                    "KOD",
                                                                    "PWS",
                                                                    "YAK",
                                                                    "NSEAK",
                                                             "SSEAK"
                                                                    )))

df_ct$Region = with(df_ct, factor(Region, levels = rev(levels(Region)))) #reverse factor levels so it plots NE to SW

df_ct$Region <- as.factor(df_ct$Region)
p<- ggplot(df_ct) + geom_tile(aes(x=REC_YEAR, y= Region, fill= FISH_TYPE)) +
  theme_bw() +
theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

#GRAPH BY SEASON

df<- df %>%
  group_by(rec_season, FISH_TYPE, REGISTRATI, REGISTRA_3) 

df_ct <-count(df,FISH_NAME) 
df_ct <- df_ct %>% filter(!is.na(rec_season))
p<- ggplot(df_ct) + geom_tile(aes(x=rec_season, y= Region, fill= FISH_TYPE)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
##__________________________________________________________________________________________________
            #PLOT Bounds and labels on Map 
#had to take the world map and parce it down by filtering out lats and longs I didnt want since subregions were not in data set
    #CALL BASE MAP
setwd("~/Documents/GitHub/rmis/scripts_gs")
source("base_map_script.R") #create base map for this script to continue to run



#load dataset called spatial_bounds_gs.xlsx in chinook_bycatch/maps
spatial_bounds_gs = read.csv("spatial_bounds_gs.csv", stringsAsFactors = FALSE)

df<- spatial_bounds_gs
#df<- df %>%
#  filter(state == 'ak')

p<- p_north_am +
  geom_segment(data = df, colour="orange", aes(x = as.numeric(line.start.lon), 
                                            y = as.numeric(line.start.lat), 
                                            xend = as.numeric(line.end.lon), 
                                            yend = as.numeric(line.end.lat))) +
  geom_text(data=df, colour="darkgreen",aes(x=label_long1, y= label_lat1, label= region), size=2)+ #smaller labels for CA, OR, and WA so they fit
  geom_text(data=df, colour="darkgreen",aes(x=label_long2, y= label_lat2, label= region), size=3)
p


##__________________________________________________________________________________________________
#ALASKA ONLY

world <- map_data("world")
north_america <- subset(world, region %in% c("USA", "Canada"))
ak <- subset(north_america, subregion %in% c("Alaska"))

#filtered out some points in +longitude because map shape gets really small- likely not many recoveries out on W Aleutians
ak <- ak %>%
  filter(!long > 0)
 
#base map
a <- ggplot(data = ak) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3)+
  theme_bw()
a

#load dataset called spatial_bounds_gs.xlsx in chinook_bycatch/maps
spatial_bounds_gs = read.csv("spatial_bounds_gs.csv", stringsAsFactors = FALSE)

  df<- spatial_bounds_gs %>% filter(state == "ak") 

ak_base<- a +
  geom_segment(data = df, colour="orange", aes(x = as.numeric(line.start.lon), 
                                               y = as.numeric(line.start.lat), 
                                               xend = as.numeric(line.end.lon), 
                                               yend = as.numeric(line.end.lat))) +
  #geom_text(data=df, colour="darkgreen",aes(x=label_long1, y= label_lat1, label= region), size=2)+ #smaller labels for CA, OR, and WA so they fit
  geom_text(data=df, colour="darkgreen",aes(x=label_long2, y= label_lat2, label= region), size=3)
ak_base
