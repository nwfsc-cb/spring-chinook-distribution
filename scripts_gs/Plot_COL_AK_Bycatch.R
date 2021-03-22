#create CWT recovery map for slide 
library(tidyverse)
library(maps)
library(mapdata)
library(scales)
library(raster)
library(RColorBrewer)
library(wesanderson)

#This script codes the base map for other spatial graphing, 
#also has broader graphing of the fishery types through year, 5 years, and all years

dat = readRDS("data/rmis_allstocks_June2020.RDS") %>%
  mutate(fishery_name =recode_factor(fishery, '80' = "Hake Trawl At Sea (CA OR WA)",
                                             '800' = "Hake Trawl Shoreside (OR WA)",
                                             '802' = "Rockfish Trawl (CA OR WA)",
                                             '803'="Groundfish Trawl (CA OR WA)",
                                             '804'="Sablefish Fixed Gear (CA OR WA)",
                                             '805'="Nearshore Groundfish (CA OR)",
                                             '81'="Groundfish Observer (Gulf AK)",
                                             '812'="Rockfish Fishery (Gulf of AK)",
                                             '82'="Groundfish Observer (Bering Sea)",
                                             '83'="Foreign Research Vessels",
                                             '84'="Foreign Mothership",
                                             '85'="Ocean Trawl By-catch",
                                             '87'="Squid Gillnet By-catch", 
                                             '88'="Juv Sampling",
                                             '89'="Other",
                                             '40' = "Ocean Sport",
                                             '41' = "Charter Sport",
                                             '42' = "Private Sport",
                                             '43'= "Jetty Sport",
                                             '44'="Columbia River Sport",
                                             '45'="Estuary Sport",
                                             '48'="Terminal Sport",
                                             '20' = "Ocean Gillnet",
                                             '21' = "Columbia Gillnet",
                                             '22' = "Coastal Gillnet",
                                             '23'= "Mixed Net and Seine",
                                             '24'="FW Net",
                                             '25'="Commercial Seine",
                                             '26'="Terminal Seine",
                                             '27'="FW Seine",
                                             '28'="Other Net",
                                             '29'="Other Seine",
                                             '10' = "Ocean Troll (non-treaty)",
                                             '11' = "Ocean Troll- Day Boat",
                                             '12' = "Ocean Troll- Trip",
                                             '13'= "Ocean Troll- Freezer Boat",
                                             '14'="Ocean Troll- Ice Boat",
                                             '15'="Treaty Troll",
                                             '16'="Terminal Troll",
                                             '17'="Non-treaty/treaty Troll",
                                             '18'="Aboriginal Troll",
                                             '19'="Other Troll"), 
                        color = recode_factor(fishery_name, 'Ocean Troll- Trip' = "Troll",
                                               'Treaty Troll' = "Troll",
                                               'Non-treaty/treaty Troll'= "Troll",
                                               'Ocean Troll (non-treaty)' = "Troll",
                                               'Ocean Troll- Day Boat' = "Troll",
                                               'Terminal Troll' = "Troll", 
                                               'Columbia Gillnet' = "Net",
                                               'Private Sport' = "Sport",
                                               'Ocean Sport' = "Sport",
                                               'Charter Sport' = "Sport",
                                               'Ocean Gillnet' = "Net",
                                               'Commercial Seine' = "Seine",
                                               'Aboriginal Troll'= "Troll"),
                        facet = recode_factor(color,'Groundfish Observer (Gulf AK)' = "High Seas", 
                                             'Hake Trawl Shoreside (OR WA)' = "High Seas",
                                             'Rockfish Trawl (CA OR WA)' = "High Seas",
                                             'Groundfish Trawl (CA OR WA)' = "High Seas",
                                             'Rockfish Fishery (Gulf of AK)' = "High Seas", 
                                             'Groundfish Observer (Bering Sea)' = "High Seas",
                                             'Hake Trawl At Sea (CA OR WA)' = "High Seas",
                                             'Ocean Trawl By-catch'= "High Seas",
                                             'Troll' = "Troll")) %>%
  rename(Latitude = "latitude.y", Longitude = longitude.y)  
 
north_america <- map_data("world") %>% filter(region=="USA")

ak <- subset(north_america, subregion %in% c("Alaska")) %>%
  filter(!long > 0) %>% 
  filter(!lat>62)

#base map
# p_ak <- ggplot(data = ak) + 
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.5) + 
#   coord_fixed(1)+
#   theme_void()
# p_ak

# 
# ggplot( ) + 
#   geom_polygon(data = ak, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.5) + 
#   coord_fixed(1.3)+
#   theme_void() +
#   facet_grid(~year_group) +
#   geom_point(data= high_seas, mapping = aes(x= Longitude, y=Latitude, size= n, color = fishery_name), alpha=0.5) +
#   scale_size_continuous(range=c(1,12), name = "CWT Recovery Counts") + 
#   ggtitle("Alaska CWT Recoveries") +
#   scale_color_manual(name = " ", values=wes_palette(name="Cavalcanti1")) +
#  theme( #legend.position = c(1, 1),
# #text = element_text(color = "#22211d"),
# plot.title = element_text(hjust = 0.5),
# plot.background = element_rect(fill = "#f5f5f2", color = NA), 
# panel.background = element_rect(fill = "#f5f5f2", color = NA), 
# #legend.background = element_rect(fill = "#f5f5f2", color = NA), 
# legend.box.margin = margin(.6,.1,1.1,.8, "cm"),
# plot.margin = margin(0.9,.1,1.1,.1, "cm")
# #plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))
# )

##################################### PLOT JUST COL RELEASES AND AK RECOVERIES FOR ALL YEARS  #########################################

dat_col_ak <- dat %>%
  filter(release_location_rmis_region %in% c("LOCR", "CECR","UPCR", "SNAK","CRGN"), 
         region %in% c("NE.GOA","NSEAK","SSEAK","NSEAK SSEAK","NW.GOA","AK", "W.APEN","E.APEN")) %>%
 # filter(!fishery_name %in% c("Mixed Net and Seine")) %>%
  filter(!is.na(fishery_name)) %>%
#  dplyr::mutate(year_group=cut(rec_year, breaks=c(-Inf, 1980, 1990, 2000, Inf), labels=c("1973-1980","1980-1990", "1990-2000", "2000-2016")))  %>%
  # dplyr::mutate(year_group=cut(rec_year, breaks=c(-Inf, 1994, Inf), labels=c("1973-1993","1994-2016"))) %>%
 # filter(year_group %in% c("1980-1990", "2000-2016")) %>%
  filter(facet == "High Seas", !is.na(Latitude)) %>%
  group_by(fishery_name, Latitude, Longitude) %>%
  dplyr::count(release_location_rmis_region) 

plot_col_ak<- ggplot( ) + 
  geom_polygon(data=ak,aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.5) + 
  coord_fixed(1)+
  theme_void()+
  geom_point(data= dat_col_ak, mapping = aes(x= Longitude, y=Latitude, size= n, color = release_location_rmis_region), alpha=0.5) +
  scale_size_continuous(name = "CWT Recovery Count") + 
  scale_color_manual(guide=FALSE,name = " ", values=wes_palette(name="Cavalcanti1")) +
  facet_grid(release_location_rmis_region ~fishery_name)+
  ggtitle("Recovery Locations for\nColumbia Origin CWT Fish") +
  theme(#legend.position = "none",
    #text = element_text(color = "#22211d"),
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    #legend.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.box.margin = margin(.6,.1,1.1,.8, "cm"),
    plot.margin = margin(0.9,.1,1.1,.1, "cm")
    #plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))
  )
############
dat_ak <- dat %>%
  filter(#release_location_rmis_region %in% c("LOCR", "CECR","UPCR", "SNAK","CRGN"), 
         region %in% c("NE.GOA","NSEAK","SSEAK","NSEAK SSEAK","NW.GOA","AK", "W.APEN","E.APEN")) %>%
  # filter(!fishery_name %in% c("Mixed Net and Seine")) %>%
  filter(!is.na(fishery_name)) %>%
  #  dplyr::mutate(year_group=cut(rec_year, breaks=c(-Inf, 1980, 1990, 2000, Inf), labels=c("1973-1980","1980-1990", "1990-2000", "2000-2016")))  %>%
  # dplyr::mutate(year_group=cut(rec_year, breaks=c(-Inf, 1994, Inf), labels=c("1973-1993","1994-2016"))) %>%
  # filter(year_group %in% c("1980-1990", "2000-2016")) %>%
  filter(facet == "High Seas", !is.na(Latitude)) %>%
  group_by(fishery_name, Latitude, Longitude) %>%
  dplyr::count(fishery_name)
 
dat_ak_filter <- dat %>% 
  filter(release_location_rmis_region %in% c("LOCR", "CECR","UPCR", "SNAK","CRGN"),
    region %in% c("NE.GOA","NSEAK","SSEAK","NSEAK SSEAK","NW.GOA","AK", "W.APEN","E.APEN")) %>%
  # filter(!fishery_name %in% c("Mixed Net and Seine")) %>%
  filter(!is.na(fishery_name)) %>%
  #  dplyr::mutate(year_group=cut(rec_year, breaks=c(-Inf, 1980, 1990, 2000, Inf), labels=c("1973-1980","1980-1990", "1990-2000", "2000-2016")))  %>%
  # dplyr::mutate(year_group=cut(rec_year, breaks=c(-Inf, 1994, Inf), labels=c("1973-1993","1994-2016"))) %>%
  # filter(year_group %in% c("1980-1990", "2000-2016")) %>%
  filter(facet == "High Seas", !is.na(Latitude)) %>%
  group_by(fishery_name, Latitude, Longitude) %>%
  dplyr::count(release_location_rmis_region)

plot_ak_highlight_col<-ggplot( ) + 
  geom_polygon(data=ak,aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.5) + 
  coord_fixed(1)+
  theme_void()+
  geom_point(data= dat_ak, mapping = aes(x= Longitude, y=Latitude, size= n),# color="gray", 
             alpha=0.3) +
  geom_point(data= dat_ak_filter, mapping = aes(x= Longitude, y=Latitude, size= n, color= release_location_rmis_region)) +
  scale_size_continuous(name = "CWT Recovery Count") + 
  scale_color_manual(guide=FALSE,name = " ", values=wes_palette(name="Cavalcanti1")) +
  facet_grid(release_location_rmis_region~fishery_name)+
  ggtitle("Recovery Locations for CWT Chinook") +
  theme(#legend.position = "none",
    #text = element_text(color = "#22211d"),
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    #legend.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.box.margin = margin(.6,.1,1.1,.8, "cm"),
    plot.margin = margin(0.9,.1,1.1,.1, "cm")
    #plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))
  )
plot_ak_highlight_col
# 
# pdf(paste0("plots/AK_COL_CWT.pdf")) #save abundance plots of each scenario
# plot(plot_col_ak)
# plot(plot_ak_highlight_col)
# dev.off()
# 





##################################### PLOT JUST COLUMBIA RELEASES IN EARLY AND LATE RECOVERY YEARS BY FISHERY #########################################
north_america <- map_data("world") %>% filter(region %in% c("USA", "Canada"))

west_coast <- north_america %>%
   filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
 filter(!long > -113) %>%
  filter(!lat > 62) %>%
  filter(!long < -179) %>%
  filter(!lat < 32) 

#base map
p_west_coast <- ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.5) + 
  coord_fixed(1.3) +
  theme_void()
p_west_coast

columbia <- dat_everything %>% filter(release_loc_domain == "COL") %>%
  filter(!fishery_type %in% c("net_seine", "sport")) %>%
  filter(!Longitude > -115) %>%
  filter(!is.na(fishery_name)) %>%
dplyr::mutate(year_group=cut(rec_year, breaks=c(-Inf, 1980, 1990, 2000, Inf), labels=c("1973-1980","1980-1990", "1990-2000", "2000-2016")))  %>%
  dplyr::mutate(color = recode_factor(fishery_name, 
                                      'Ocean Troll- Trip' = "Troll",
                                      'Treaty Troll' = "Troll",
                                      'Non-treaty/treaty Troll'= "Troll",
                                      'Ocean Troll (non-treaty)' = "Troll",
                                      'Ocean Troll- Day Boat' = "Troll",
                                      'Terminal Troll' = "Troll", 
                                      'Aboriginal Troll'= "Troll")) %>%
  dplyr::mutate(facet = recode_factor(color,'Groundfish Observer (Gulf AK)' = "High Seas", 
                                      'Hake Trawl Shoreside (OR WA)' = "High Seas",
                                      'Rockfish Trawl (CA OR WA)' = "High Seas",
                                      'Groundfish Trawl (CA OR WA)' = "High Seas",
                                      
                                      'Rockfish Fishery (Gulf of AK)' = "High Seas", 
                                      'Groundfish Observer (Bering Sea)' = "High Seas",
                                      'Hake Trawl At Sea (CA OR WA)' = "High Seas",
                                      'Ocean Trawl By-catch'= "High Seas",
                                      'Troll' = "Troll")) %>%
  # dplyr::mutate(year_group=cut(rec_year, breaks=c(-Inf, 1994, Inf), labels=c("1973-1993","1994-2016"))) %>%
  filter(year_group %in% c("1980-1990", "2000-2016")) %>%
  group_by(year_group, facet, color, Latitude, Longitude) %>%
  dplyr::count(color) 



ggplot( ) + 
  geom_polygon(data = west_coast, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.5) + 
  coord_fixed(1.3)+
  theme_void() +
  geom_point(data= columbia, mapping = aes(x= Longitude, y=Latitude, size= n, color = color), alpha=0.5) +
  scale_size_continuous(name = "CWT Recovery Count") + 
# scale_color_brewer(name = " ", palette = rev("Spectral")) +
  facet_grid(year_group ~ facet)+
  ggtitle("Recovery Locations for\nColumbia River CWT Fish") +
  theme( #legend.position = "bottom",
   #text = element_text(color = "#22211d"),
   plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    #legend.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.box.margin = margin(.6,.1,1.1,.8, "cm"),
    plot.margin = margin(0.9,.1,1.1,.1, "cm")
    #plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))
  )


# how to find color: display.brewer.all(n=9, type = "div")

##################################### PLOT PUGET SOUND RELEASES IN EARLY AND LATE RECOVERY YEARS BY FISHERY #########################################
north_america <- map_data("world") %>% filter(region %in% c("USA", "Canada"))

west_coast <- north_america %>%
  filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
  filter(!long > -113) %>%
  filter(!lat > 62) %>%
  filter(!long < -179) %>%
  filter(!lat < 32) 

#base map
p_west_coast <- ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.5) + 
  coord_fixed(1.3) +
  theme_void()
p_west_coast

wa <- dat_everything %>% filter(release_loc_domain == "WA") %>%
  filter(!fishery_type %in% c("net_seine", "sport")) %>%
  filter(!Longitude > -115) %>%
  filter(!is.na(fishery_name)) %>%
  filter(!release_location_rmis_region %in% c("WILP", "GRAY", "NWC", "NOWA")) %>%
  dplyr::mutate(year_group=cut(rec_year, breaks=c(-Inf, 1980, 1990, 2000, Inf), labels=c("1973-1980","1980-1990", "1990-2000", "2000-2016")))  %>%
  dplyr::mutate(color = recode_factor(fishery_name, 
                           'Treaty Troll' = "Troll",
                           'Non-treaty/treaty Troll'= "Troll",
                                      'Ocean Troll (non-treaty)' = "Troll",
                                      'Ocean Troll- Day Boat' = "Troll",
                                      'Terminal Troll' = "Troll", 
                                      'Aboriginal Troll'= "Troll")) %>%
  dplyr::mutate(facet = recode_factor(color,'Groundfish Observer (Gulf AK)' = "High Seas", 
                                      'Rockfish Fishery (Gulf of AK)' = "High Seas", 
                                      'Groundfish Observer (Bering Sea)' = "High Seas",
                                      'Hake Trawl At Sea (CA OR WA)' = "High Seas",
                                      'Ocean Trawl By-catch'= "High Seas",
                                      'Troll' = "Troll")) %>%
  # dplyr::mutate(year_group=cut(rec_year, breaks=c(-Inf, 1994, Inf), labels=c("1973-1993","1994-2016"))) %>%
  filter(year_group %in% c("1980-1990", "2000-2016")) %>%
  group_by(year_group, facet, color, Latitude, Longitude) %>%
  dplyr::count(color) 

ggplot( ) + 
  geom_polygon(data = west_coast, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.5) + 
  coord_fixed(1.3)+
  theme_void() +
  geom_point(data= wa, mapping = aes(x= Longitude, y=Latitude, size= n, color = color), alpha=0.5) +
  scale_size_continuous(name = "CWT Recovery Count") + 
    scale_color_manual(name = " ", values=wes_palette(name="Cavalcanti1")) +
  facet_grid(year_group ~ facet)+
  ggtitle("Recovery locations of fish\nreleased from Puget Sound") +
  theme( #legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        #text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        #legend.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.box.margin = margin(.6,.1,1.1,.8, "cm"),
        plot.margin = margin(0.9,.1,1.1,.1, "cm")
        #plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))
  )








