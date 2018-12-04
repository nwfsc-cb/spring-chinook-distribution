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

#just look for highseas in AK
highseas$REGISTRATI <- as.factor(highseas$REGISTRATI)
highseas$REGISTRA_3 <- as.factor(highseas$REGISTRA_3)
highseas$n <- as.numeric(highseas$n)

#recode factor
df_ct$Region <- recode_factor(df_ct$REGISTRA_3, 
'KOD' = "KOD",
'CIFW'= "KOD",
'NGSW'="KOD",
'CISW'="KOD", #combine cook inlet with kodiak
'PWSE' ="PWS", #combine all prince william sound 
'PWSF'="PWS",
'PWSI'="PWS",
'IBS'="WYAK",
'EYKT' = "EYAK",
'CSEO'="EYAK",
'NSEI'= "EYAK",
'NSEO' = "EYAK") #change IBS to WYAK, for W of Yakatat

#Make levels from NW to SE so they plot better
df_ct <- df_ct %>% mutate(Region = factor(Region, levels = c("BSEA",
                                                                    "AISD",
                                                                    "MSAPW", 
                                                                    "MSAPE",
                                                                    "LCHIG",
                                                                    "KOD",
                                                                    "PWS",
                                                                    "WYAK",
                                                                    "EYAK",
                                                             "SSEI",
                                                             "SSEO"
                                                                    )))

df_ct$Region = with(df_ct, factor(Region, levels = rev(levels(Region)))) #reverse factor levels so it plots NE to SW

df_ct$Region <- as.factor(df_ct$Region)
p<- ggplot(df_ct) + geom_tile(aes(x=REC_YEAR, y= Region, fill= FISH_TYPE)) +
  theme_bw() +
theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
