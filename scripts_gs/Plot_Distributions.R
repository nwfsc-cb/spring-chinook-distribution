#This script plots Spring Chinook Ocean Distributions on map
library(here)
library(sf)
library(tidyverse)
library(viridis)
library(maps)
library(mapdata)
library(ggpubr)
library(scales)
library(PNWColors)
library(wesanderson)
library(gridExtra)
library(grid)
library(lwgeom) 
library(raster)
library(here)
#############################################################################################################################
#set colors 
color16<- c( "#F2AD00",
             "#FF0000", 
             "#AA352D", 
             "#7F5045",
             "#556A5B",
             "#75B2B2",
             "#00A08A",
             "#79A645",
             "#C9AA17",
             "#F3A600",
             "#DE8D23",
             "#AAA06B",
             "#8FA98E",
             "#5BBCD6",
             "#2A8572",
             "#000000")
#library(scales)
#show_col(color16)

#############################################################################################################################
######################################################## LOAD DATA ##########################################################

#Load Stock Geo-coords -- this should hae stock names and stock lat and longs for map triangles 
stock_info <- read.csv( )  

#LOAD THE OCEAN REGION LABELS 
#base_map_labels = read.csv("data/base_map_labels.csv", stringsAsFactors = FALSE) %>%
  #mutate(label_long1 = label_long1 - 15)

AK_ocean_regions <- read.csv("Maps_and_Charts_of_Regions/AK_regions.csv",
                            stringsAsFactors = FALSE)

ocean_regions <- read.csv("Maps_and_Charts_of_Regions/Ocean_Region_Coords.csv",
                            stringsAsFactors = FALSE) %>%
   mutate(line.end.lon = case_when(line.end.lon == -152 ~ line.end.lon - 16,
                                   TRUE~ line.end.lon))

#Lat and Longs for ocean distribution proportional points -- create this 
# evenness_locations <- read_csv("data/Ocean_Dist_Points_on_Map.csv") %>%
#   mutate(stock = as.factor(stock), 
#          sum = 1.8) %>%
#   group_by(location.name) %>%
#   mutate(cummulative = cumsum(sum),
#          map_point_long = -cummulative + map_point_long)

#############################################################################################################################
######################################################## CREATE PROPORTIONAL DISTRIBUTION DF ##############################################################################
#make small fractions 0.
# df<-theta_distribute
# df[df < 0.01] <- 0
# df <- df %>%
#   data.frame( ) %>%
#   mutate(location.number = 1:17) %>%
#   left_join(ocean_names) %>%
#   gather(1:16, key = stock_number_ole ,value = distribution) %>%
#   separate(stock_number_ole, c("delete", "stock_number_ole" ),sep=1) %>%
#   dplyr::select(-c("delete")) %>%
#   mutate(stock_number_ole = as.numeric(stock_number_ole)) %>%
#   left_join(stock_info) %>%
#   dplyr::mutate(location.number = as.numeric(location.number))  %>%  
#   filter(!location.name %in% c("PUSO", "SGEO", "PUSO_out")) %>%
#   dplyr::select(c(location.number, location.name,stock.name, distribution)) %>%
#   arrange(location.number) %>%
#   left_join(evenness_locations)  %>%
#   dplyr::select(-c(stock)) %>%
#   dplyr::mutate(stock.name  = factor(stock.name, level=c("SFB", "NCA","SOR","COR","NOR", "SAB", "LCOL",#order according to latitude
#                                                          "MCOL", "URB", "SNAK",  "WAC", "PUSO_S",
#                                                          "PUSO_N","SWVI","SGEO_S", "SGEO_N")))
#################################################################################   
#create AK map
ak <- sf::st_as_sf(maps::map('world','USA:Alaska',
                             # ylim = c(55,60),
                             # xlim=c(-140,-130), 
                             plot=FALSE, fill=TRUE))

#create bounds to trim AK map 
bounds_ak <- extent(-170,-130, 50,62) 
extent_ak <- st_set_crs(st_as_sf(as(bounds_ak, "SpatialPolygons")), 4326)
ak <- st_intersection(ak, extent_ak) #trim map by intersections 

#plot transformed cropped data 
ak_plot<-ggplot(ak) +
  geom_sf() +
  coord_sf(crs=4326, #transforms it 
           #xlim = mapRange[c(1:2)], 
           #ylim = mapRange[c(3:4)], # notice what happens when y
           # ylim = c(40,45),
           # xlim=c(100,110),
           expand = TRUE,
           clip = "off")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
#################################################################################  
#create BC map
#PLOT<- map("world", "Canada",  plot=TRUE, fill=TRUE)
bc<-sf::st_as_sf(map("worldHires", "Canada",  plot=FALSE, fill=TRUE))
#create bounds to trim AK map 
bounds_bc <- extent(-140,-120,45,60)
extent_bc <- st_set_crs(st_as_sf(as(bounds_bc, "SpatialPolygons")), 4326)
bc_crop <- st_intersection(extent_bc,st_make_valid(bc)) #trim map by intersections 

# bc_plot<-ggplot(bc_crop) +
#   geom_sf() +
#   coord_sf(crs=aea.proj, #transforms it 
#            #xlim = mapRange[c(1:2)], 
#            #ylim = mapRange[c(3:4)], # notice what happens when y
#            # ylim = c(40,45),
#            # xlim=c(100,110),
#            expand = TRUE,
#            clip = "off")+
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# bc_plot
#################################################################################  
#create WA OR CA mapmap("worldHires", "Canada",
wa_or_ca<-sf::st_as_sf(maps::map('state', region = c('washington','california','oregon'), 
                                 plot=FALSE, fill=TRUE))

#create bounds to trim map 
bounds_lowerUS <- extent(-130,-120,36,60)
extent_lowerUS <- st_set_crs(st_as_sf(as(bounds_lowerUS, "SpatialPolygons")), 4326)
wa_or_ca <- st_intersection(st_make_valid(wa_or_ca), extent_lowerUS) #trim map by intersections 

map1<-  ggplot( ) +
  geom_sf(data=wa_or_ca) +  
  geom_sf(data=ak )+
  geom_sf(data=bc_crop)+
  coord_sf(crs=4326,#aea.proj, #transforms it
           expand = TRUE)+
 # scale_x_continuous(#name="Longitude",breaks=seq(-140,-122,5), 
 #    limits = c(-168,-120),                
 #    expand = c(0,0))+
 #  scale_y_continuous(#name="Latitude",breaks=seq(36,56,5), 
 #    expand = c(0,0)) +
 #  #add box around salish sea 
 #  geom_rect(aes(xmin = -122, xmax = -128.5, ymin = 46.8, ymax = 51.5), 
 #            fill = NA, colour = "black", alpha = 0.5, size = .4) +
  #add lines for the different mainland ocean regions 
  geom_segment(data = AK_ocean_regions, colour="black", size = 0.1, linetype = 2, alpha = 0.7, aes(x = as.numeric(line.start.lon),
                                                                                                      y = as.numeric(line.start.lat),
                                                                                                      xend = as.numeric(line.end.lon),
                                                                                                      yend = as.numeric(line.end.lat))) +
  #add lines for the AK ocean regions 
   geom_segment(data = ocean_regions, colour="black", size = 0.1, linetype = 2, alpha = 0.7, aes(x = as.numeric(line.start.lon),
                                                                                                      y = as.numeric(line.start.lat),
                                                                                                      xend = as.numeric(line.end.lon),
                                                                                                      yend = as.numeric(line.end.lat))) 



map1
    #add points in the ocean to show proportional stock distributions
  geom_point(data = df, aes(x=map_point_long, y=map_point_lat,size=ifelse(distribution==0, NA, distribution), colour = stock.name)) +   
  scale_color_manual(values = color16, guide = guide_legend(reverse = TRUE), name = "Stock")+
  guides(color = FALSE) +
  #add triangles for each stock origin
  geom_point(data = stock_info_triangles, aes(x=SITELONG, y=SITELAT, color=stock.name),show.legend = FALSE, shape=17, size = 2) +
  #remove legends for each aesthetic 
  guides(fill = FALSE, 
         #size = FALSE, 
         shape = FALSE, alpha = FALSE,
         size=guide_legend(title = "Stock-specific Proportional\nOcean Distribution" 
                           #label.position = "center"
         ),
         color=guide_legend(ncol=2, override.aes = list(size = 4),
                            keywidth=0.1,
                            keyheight=0.1,
                            default.unit="inch")) +
  #add text for ocean region labels
  geom_text(data=base_map_labels, colour="black",aes(x=label_long1, y= label_lat1, label= region), #angle = 45, hjust = -0.25, 
            size=2) +
  #only want a partial x y axis 
  # geom_segment(aes(x=-140, xend=-122,y=-Inf,yend=-Inf))+
  # geom_segment(aes(y=36,yend=58,x=-Inf,xend=-Inf)) +
  theme(#plot.margin = margin(0,2,0,.01, "inch"), #(top,right,bottom,left)
    plot.background = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA, size=.5),
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

map1

################################################################################################MAKE INSET
################ Make Base map. 
salish_seaWA <-sf::st_as_sf(maps::map('state', region = c('washington'), 
                                      plot=FALSE, fill=TRUE))
bounds_lowerUS <- extent(-130,-121.5,47,58.2)
extent_lowerUS <- st_set_crs(st_as_sf(as(bounds_lowerUS, "SpatialPolygons")), 4326)
salish_seaWA_crop <- st_intersection(st_make_valid(salish_seaWA), extent_lowerUS) #trim map by intersections 

salish_seaBC<-sf::st_as_sf(maps::map("worldHires", "Canada",  plot=FALSE, fill=TRUE))
#create bounds to trim AK map 
bounds_bc <- extent(-140,-121.5,45,52)
extent_bc <- st_set_crs(st_as_sf(as(bounds_bc, "SpatialPolygons")), 4326)
salish_seaBC_crop <- st_intersection(extent_bc,st_make_valid(salish_seaBC)) #trim map by intersections 


#LOAD STOCK NAMES WITH GEOCORDS FOCAL SPECIES 
stock_info_triangles <- read.csv("data/Hatcheries_List_Focal_GS.csv") %>%
  dplyr::mutate(STOCK_NAME_GENOAADD  = factor(STOCK_NAME_GENOAADD, level=c("SFB", "NCA","SOR","COR","NOR", "SAB", "LCOL",#order according to latitude
                                                                           "MCOL", "URB", "SNAK", "WAC","PUSO_S",
                                                                           "PUSO_N","SWVI","SGEO_S", "SGEO_N"))) %>%
  dplyr::rename(stock.name = STOCK_NAME_GENOAADD) %>%
  filter(stock.name %in% c("WAC","PUSO_S",
                           "PUSO_N","SWVI","SGEO_S", "SGEO_N"))

#LOAD STOCK NAMES AND ORDER 
stock_info <- read.csv("data/stock_name_info.csv") %>%
  dplyr::mutate(stock.name  = factor(stock.name, level=c("SFB", "NCA","SOR","COR","NOR", "SAB", "LCOL",#order according to latitude
                                                         "MCOL", "URB", "SNAK", "WAC","PUSO_S",
                                                         "PUSO_N","SWVI","SGEO_S", "SGEO_N"))) 
#LOAD THE OCEAN REGION LABELS 
SS_base_map_labels = read.csv("data/SS_base_map_labels.csv", stringsAsFactors = FALSE)  %>%
  mutate(label_long2 = label_long2 - 7.75,
         label_lat2 = label_lat2 + 0.5,
         region = case_when(region == "PUSO_out" ~ "SJDF",
                            TRUE ~ region))  

#LOAD COORDINATES FOR LINES IN OCEAN REGIONS 
SS_ocean_region_bounds = read.csv("data/SS_Base_Map_Coords.csv", stringsAsFactors = FALSE) 

#LOAD LAT AND LONGS FOR OCEAN DISTRIBUTION POINTS  
SS_evenness_locations <- read_csv("data/SS_Points_on_Map_Locations.csv") %>%
  mutate(stock = as.factor(stock),
         long3 = long3 - 0.5,
         lat3 =lat3 + 0.5) %>%
  dplyr::mutate(location.number = as.numeric(location.number),
                location.name = case_when(location.name == "PUSO_out" ~ "SJDF",
                                          TRUE ~ location.name))   

#CREATE DATA FRAME USED FOR PLOTTING 
df<-theta_distribute
df[df < 0.01] <- 0
df <- df %>%
  data.frame( ) %>%
  mutate(location.number = 1:17) %>%
  left_join(ocean_names) %>%
  gather(1:16, key = stock_number_ole ,value = distribution) %>%
  separate(stock_number_ole, c("delete", "stock_number_ole" ),sep=1) %>%
  dplyr::select(-c("delete")) %>%
  mutate(stock_number_ole = as.numeric(stock_number_ole)#,
         # location.name = case_when(location.name == "PUSO_out" ~ "SJDF",
         #                   TRUE ~ location.name)
  )  %>% 
  left_join(stock_info) %>%
  dplyr::mutate(location.number = as.numeric(location.number),
                location.name = case_when(location.name == "PUSO_out" ~ "SJDF",
                                          TRUE ~ location.name))  %>%  
  filter(location.name %in% c("PUSO", "SJDF", "SGEO")) %>%
  dplyr::select(c(location.number, location.name,stock.name, distribution)) %>%
  arrange(location.number) %>%
  left_join(SS_evenness_locations) %>%
  dplyr::select(-c(stock)) %>%
  dplyr::mutate(stock.name  = factor(stock.name, level=c("SFB", "NCA","SOR","COR","NOR", "SAB", "LCOL",#order according to latitude
                                                         "MCOL", "URB", "SNAK",  "WAC", "PUSO_S",
                                                         "PUSO_N","SWVI","SGEO_S", "SGEO_N")))

inset <- ggplot( ) +
  geom_sf(data=salish_seaWA_crop)+
  geom_sf(data=salish_seaBC_crop)+
  coord_sf(crs=4326,#aea.proj, #transforms it
           expand = TRUE)+
  # geom_polygon(data= wa_salish, aes(x = long, y = lat, group = group), color = "white", fill="lightgray",size = 0.25) +
  # geom_polygon(data = bc_salish, aes(x = long, y = lat,group = group), color = "white", fill="lightgray",size = 0.25) +
  scale_x_continuous(#name="Longitude", breaks=seq(-150,-120,5), 
    limits = c(-147,-121.5),
    expand = c(0,0)) +
  scale_y_continuous(
    expand = c(0,0)) +
  # geom_segment(data = SS_ocean_region_bounds, colour="black", 
  #              size = 0.2, linetype = 2, 
  #              aes(x = as.numeric(line.start.lon),
  #                  y = as.numeric(line.start.lat),
  #                  xend = as.numeric(line.end.lon),
  #                  yend = as.numeric(line.end.lat))) +
  #add points in the ocean to show proportional stock distributions
  geom_point(data = df, aes(x=long3, y=lat3,size=ifelse(distribution==0, NA, distribution), 
                            # position = position_dodge(width = 0.5),
                            colour = stock.name)) +   
  scale_color_manual(values = color16, guide = guide_legend(reverse = TRUE), name = "Stock")+
  #add triangles for each stock origin
  geom_point(data = stock_info_triangles, aes(x=SITELONG, y=SITELAT, color=stock.name), shape=17, size = 2) +
  #remove legends for each aesthetic 
  guides(fill = FALSE, size = FALSE, color = FALSE) + 
  #add text for ocean region labels
  geom_text(data=SS_base_map_labels, colour="black",aes(x=label_long2, y= label_lat2, label= region), #angle = 45, hjust = -0.25,
            size=2) +
  #only want a partial x y axis 
  # geom_segment(aes(x=-135, xend=-120,y=-Inf,yend=-Inf)) +
  # geom_segment(aes(y=46,yend=51,x=-Inf,xend=-Inf)) +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    # legend.title.align=0.25, 
    legend.key = element_rect(fill = "white", color = "white"), 
    legend.box.background = element_rect(color="white"),# size=2
    # plot.margin = margin(0.2,5,.2,.2, "inch"), #(top,right,bottom,left)
    plot.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=.5),
    # panel.background = element_blank(),    
    legend.text = element_text(size = 7),
    legend.title=element_text(size=7),
    # legend.key = element_blank(), 
    #legend.background=element_blank(),
    #legend.box.background = element_blank(),
    #legend.position = c(0.8, 0.95),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    # axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    # axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')
  )                                     

inset
######################################################################################################
############Arrange stuff for plot   
#PUT INSET ON TOP OF BIG MAP 
# Fig1_Map_Inset <-  ggplot() +
#   coord_equal(xlim = c(0, 10), ylim = c(0, 10), expand = TRUE) +
#   annotation_custom(ggplotGrob(map1), xmin =0, xmax = 10, ymin = -4,
#                     ymax = 14) +
#   annotation_custom(ggplotGrob(inset), xmin = 7, xmax = 13, ymin = 6, 
#                     ymax = 10)   +
#   theme_void()
# Fig1_Map_Inset


#arrange figures on the same page 
Fig_1<- ggarrange(
  # Fig1_Map_Inset_fix,   
  Fig1_Map_Inset,
  nrow = 1, 
  #align = "v",
  #ncol = 2,
  #  widths = c(1.5,1),
  labels =c("A", "B")#,
  #vjust= -0.75 #adjust vertical label position
) 
#Fig_1

# 
# pdf( file = "output/Figure_1.pdf",width = 17 , height = 7)
# Fig_1
# dev.off()

