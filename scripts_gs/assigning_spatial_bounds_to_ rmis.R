#CODE SPATIAL BOUND CATEGORIES
setwd("~/Documents/GitHub/rmis/data")
df = read.csv("recovery codes-wietkamp+shelton 12-2018 two PUSO.csv", stringsAsFactors = FALSE)
dat = read.csv("rmis_data.csv", stringsAsFactors = FALSE)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
df$recovery_location_code <- df$location_code
df <- df%>% 
  select(recovery_location_code, Rec.area.Sullaway.Shelton)

df$region <- df$Rec.area.Sullaway.Shelton
df <- df %>% select(-c(Rec.area.Sullaway.Shelton)) 
#df = version of lookup table with necessary columns

#___________________________________________________________________________________________________________________________________________________
#join DF recovery with the weitkamp updated codes 
dat_region <- left_join(dat, df)
    #this join only adds 109 rows, small amount of duplicates going to ignore

#write.csv(dat_region, "dat_region.csv") #SAVE DAT REGION FOR FURTHER USE       THIS FILE IS GOOD TO GO. NOW NEEDS SNOUTBASE CORRECTIONS 


#PLOT dat_region on map and make sure they land in areas that make sense
world <- map_data("world")
north_america <- subset(world, region %in% c("USA"))
north_america <- north_america %>%
  filter(!long > -120) %>%
  filter(!lat < 30)

n <- ggplot(data = north_america) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3)+
  theme_bw()
n

setwd("~/Documents/GitHub/Chinook_Bycatch/Maps_and_Charts_of_Regions")
spatial_bounds_gs = read.csv("spatial_bounds_gs.csv", stringsAsFactors = FALSE)

df<- spatial_bounds_gs 

p_nolab<- n +
  geom_segment(data = df, colour="orange", aes(x = as.numeric(line.start.lon), 
                                               y = as.numeric(line.start.lat), 
                                               xend = as.numeric(line.end.lon), 
                                               yend = as.numeric(line.end.lat))) 

w <-  p_nolab +
  geom_point(data = dat_region, mapping = aes(x = longitude, y = latitude, color= region, alpha= 0.5))  +
  scale_alpha(guide = 'none')+
  theme_bw() 

w
#_______________________________________________________________________________________________________________________________________ 
#ALL SUMMARY PLOTS

#SUMMARY PLOTS OF SPATIAL BOUNDS BY SEASON

#dat_region$Region = with(dat_region, factor(Region, levels = rev(levels(Region)))) #reverse factor levels so it plots NE to SW
#JUST AK

df_ct <- dat_region %>%
  filter(region == "BER" | region == "ALEUT" |region == "APEN" | region == "KOD" | region == "PWS" | region == "YAK" | region == "NSEAK" |
           region == "SSEAK")
df_ct$region <- as.factor(df_ct$region)

df_ct <- df_ct %>% 
  mutate(region = factor(region, levels = c("BER",
                                            "ALEUT",
                                            "APEN", 
                                            "KOD",
                                            "PWS",
                                            "YAK",
                                            "NSEAK",
                                            "SSEAK"
  )))

df_ct$region = with(df_ct, factor(region, levels = rev(levels(region)))) #reverse factor levels so it plots NE to SW

df_ct_sum <- df_ct %>%
  dplyr::group_by(fishery_type, rec_season) %>%
  dplyr::count(region) 

df_ct <- df_ct %>%
  filter(!fishery_type == "misc" & !fishery_type == "aboriginal" & !fishery_type == "test_fishery")

p<- ggplot(df_ct) + geom_tile(aes(x=rec_season, y= region), color= "white") +
 theme_bw() +
  theme(plot.title = element_text(hjust=0, size=16)) +
 # theme_bw(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~fishery_type) +
  labs(title = "Alaska Regions and Recovery Presence Absence", caption = "Removed aboriginal, misc, and test fishery data, season specifications in google drive ak_area notes, a few more regions need to be reclassified but this will only add a recovery for highseas ALEUT in fall, and YAK in summer") +
  xlab("Season") +
  ylab("Region")
p

pdf("AK_region_tile.pdf", width=13, height=8.5); print(p); dev.off()


            

p = df %>%
  ggplot(aes(month_name, tax)) + geom_tile(aes(fill =pres_abs)) +
  scale_fill_manual(values = col.plot) 

 #_______________________________________________________________________________________________________________________________________ 
  #SUMMARY PLOTS OF SPATIAL BOUNDS BY YEAR - JUST AK, FACET BY FISHERY
df_ct <- dat_region %>%
  filter(region == "BER" | region == "ALEUT" |region == "APEN" | region == "KOD" | region == "PWS" | region == "YAK" | region == "NSEAK" |
           region == "SSEAK")
df_ct$region <- as.factor(df_ct$region)

df_ct <- df_ct %>% 
  mutate(region = factor(region, levels = c("BER",
                                            "ALEUT",
                                            "APEN", 
                                            "KOD",
                                            "PWS",
                                            "YAK",
                                            "NSEAK",
                                            "SSEAK"
  )))


df_cta <- df_ct %>%
  dplyr::filter(!fishery_type == "misc" & !fishery_type == "aboriginal" & !fishery_type == "test_fishery") %>%
 dplyr::group_by(rec_year, region, fishery_type) %>%
   dplyr::count(fishery_type)

df <- df_cta
col.filters <- unique(df_cta$fishery_type) 

lapply(seq_along(col.filters), function(x) {
  filter(df, fishery_type == col.filters[x])
}
) -> df_list

names(df_list) <- col.filters

list2env(df_list, .GlobalEnv)

#plot by fishery list     
plotdf=list()

for(i in 1: length(df_list)) 
{
  df = as.data.frame(df_list[[i]])
  NAME <- unique(df$fishery_type)
  
  plotdf[[i]] <- ggplot(df, aes(x=rec_year, y=n)) +
    geom_bar(position="dodge", stat="identity") +
   facet_grid(region ~ ., labeller=label_wrap_gen(width=.1)) +
    ggtitle('AK Region Recoveries') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x='Recovery Year', y= 'Instance of Fish recovery')+
    theme_bw()+
    ggtitle(paste(as.character(NAME))) 
  
}

plotdf[[2]]


p <- list()
for(i in 1:4){
  p[[i]] <- qplot(1:10,10:1,main=i)
}
do.call(grid.arrange, p)

g <-grid.arrange(plotdf[[1]], plotdf[[2]], plotdf[[3]], plotdf[[4]], nrow(2), ncol(2))
g
pdf(file="ak_recovery_freq_region", width=13, height=8.5); print(g); dev.off()



pall <- list(plotdf[[1]], plotdf[[2]], plotdf[[3]], plotdf[[4]]) 
pdf(file="ak_recovery_freq_region.pdf", width=13, height=8.5); print(pall); dev.off()


#SAME AS ABOVE JUST WITH ALL FISHERIES
df_ctb <- df_ct %>%
  dplyr::filter(!fishery_type == "misc" & !fishery_type == "aboriginal" & !fishery_type == "test_fishery") %>%
  dplyr::group_by(rec_year, region) %>%
  dplyr::count(fishery_type)

df <- df_ctb

p <- ggplot(df, aes(x=rec_year, y=n)) +
    geom_bar(position="dodge", stat="identity") +
    facet_grid(region ~ ., scales = "free_y", labeller=label_wrap_gen(width=.1)) +
    ggtitle('AK Region Recoveries') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x='Recovery Year', y= 'Instance of Fish recovery')+
    theme_bw()+
    ggtitle("Recoveries AK")

p

                    #PLOT RECOVERIES BY REGION AND SEASON FACETED BY FISHERY - SEPERATE PLOTS FOR EACH RECOVERY STATE MAKE IT EASIER TO READ 
#THIS NEEDS WORK______________________________________________________________________________________________________________________________________

#summarize to indicate amount of recoveries in each area
dat_region_sum <- dat_region %>%
  dplyr::group_by(rec_season, fishery_type) %>%
  dplyr::count(region)

df <- dat_region
col.filters <- unique(dat_region$recovery_state) 

lapply(seq_along(col.filters), function(x) {
  filter(dat_region, recovery_state == col.filters[x])
}
) -> df_list

names(df_list) <- col.filters

list2env(df_list, .GlobalEnv)

#plot by fishery list     
plotdf=list()

for(i in 1: length(df_list)) 
{
  df = as.data.frame(df_list[[i]])
  NAME <- unique(df$recovery_state)
  
  plotdf[[i]] <- ggplot(df) + 
    geom_tile(aes(x=rec_season, y= region), color= "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    facet_wrap(~fishery_type)
    ggtitle(paste(as.character(NAME))) 
}

print(plotdf[[5]])




#___________________________________________________________________________________________________________________

#FIGURING OUT WHERE WE ARE MISSING INFO ON LOCATION CODES
#  df_recovery <- left_join(dat_recovery, df, by = "recovery_location_code")
#na is a list of recovery codes that did not match up to Oles look up table
#331 recovery location codes did not match up
          na<- df_recovery %>% filter(is.na(`Rec area.Shelton3`)) #all the recoveries that do not match up with a recovery location code in look up table // will want to use this later for when add in regions
          recovery_location_code <- unique(na$recovery_location_code)
          recovery_location_code <- as.data.frame(recovery_location_code)
          recovery_location_code$id <- 1
          c<- left_join(locations, recovery_location_code) #list of unique codes that are not in look up table (includes with and without lat long)
          id<- c %>% filter(id == 1) #list of unique codes that are not in look up table (includes with and without lat long)
          
          z <- id %>% filter(is.na(latitude)) # list of recovery codes that are not in lookup tanble AND that dont have lat and long - need to look up in RMIS data base?
          lat_long <- id %>% filter(!is.na(latitude)) #list of recovery codes that are not in table but DO have lat long- will use for convex hull stuff
          lat_long <- lat_long %>%
            dplyr::select(-c(id))
          x <- left_join(dat_recovery, z) 
          v <- x %>% filter(!is.na(id)) #list of # of recoveries that dont have lat long in recovery codes
#write_csv(lat_long,"missing_rec_code_lat_long.csv")




