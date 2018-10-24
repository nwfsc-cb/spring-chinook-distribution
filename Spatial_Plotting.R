library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(mapdata)
library(scales)
library(raster)
##Load focal species data
dat_recovery = read.csv("dat_recovery.csv", stringsAsFactors = FALSE)

#create dat_recover_loc used for spatial plotting
#joins locations.txt with dat_recovery

# pull in location data from RMIS
locations = read.csv("data/locations.txt", stringsAsFactors = FALSE)
locations = locations[,c("location_code","rmis_latitude","rmis_longitude", "description")]
locations = rename(locations, recovery_location_code = location_code,
                   recovery_description = description, latitude=rmis_latitude, longitude = rmis_longitude)
dat_recovery_ <- dat_recovery %>% 
  unite(recovery_location_code, state_code, marine_fw, rest_of_rec_code, sep = "")
dat_recovery_loc = right_join(locations, dat_recovery_)
#Delete duplicates (they have NA's)
dat_recovery_loc <- dat_recovery_loc %>%
  filter(!is.na(latitude))

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
        coord_fixed(1.3)
      p_north_am

#GROUP GEAR TYPE MORE BROADLY 
    dat_recovery_loc$gear_group <- recode_factor(dat_recovery_loc$gear_type, 'high_seas' = "other",
                                                 'misc' = "other",
                                                 'troll' = "troll",
                                                 'juv_sampling'="other",
                                                 'net_seine'="net_seine",
                                                 'sport'="sport",
                                                 'aboriginal'="other",
                                                 'test_fishery'="other")
    
        ## COLORS ##
    #########create color scale so fisheries always have same color
    dat_recovery_loc$gear_type <- as.factor(dat_recovery_loc$gear_type)
    
    library(RColorBrewer)
    myColors <- brewer.pal(4,"RdYlGn")
    names(myColors) <- levels(dat_recovery_loc$gear_group)
    colScale <- scale_colour_manual(name = "gear_group",values = myColors)
    
    ################ REGIONS GROUPED IN 5 YEAR INTERVALS #################
    
    #create for loop to go through region_list and create 5 year categories 
    #go into region list
    #dplyr stuff group and count create new column and new df
    #region_list_5
    #put region list 5 into a list using same code that created region_liust
   dat_recovery_loc$run_year <-as.numeric(dat_recovery_loc$run_year)
    
  dat_recovery_loc <- dat_recovery_loc %>% 
       separate(recovery_year, into = c("run_year", "rest_rec_date"), sep = 4)
     
    dat_recovery_loc$run_year_five <- as.vector(cut(dat_recovery_loc$run_year, breaks=c(-Inf,1977,1982,1987,1992,1997,2002,2007,2012,Inf), labels=c("1973-1977","1978-1982","1983-1987","1988-1992","1993-1997","1998-2002","2003-2007","2008-2012","2013-2016")))
    dat_recovery_loc <-  dat_recovery_loc %>% unite(lat_long, latitude, longitude, remove=FALSE) 
    dat_recovery_five <- dat_recovery_loc %>%
      group_by(release_loc_domain, release_location_rmis_region, run_year_five, latitude, longitude, lat_long, gear_group) %>%
      summarise(est_sum_five_yr=sum(estimated_number))
    
    #if interested in Male vs Female locations
    #dat_recovery_five$sex <- as.factor(dat_recovery_five$sex)
    #dat_recovery_five_MF <- dat_recovery_five %>% filter(!sex == "")

    
###### CREATE LIST BASED ON DAT_RECOVER_FIVE SO A PLOTING LOOP CAN BE CREATED #######
    
    col.filters <- unique(dat_recovery_five$release_location_rmis_region) 
    
    lapply(seq_along(col.filters), function(x) {
      filter(dat_recovery_five, release_location_rmis_region == col.filters[x])
    }
    ) -> region_list_5
    
    # names(region_list_5) <- col.filters
    col.filters_dat <- as.data.frame(col.filters)
    # list2env(region_list_5, .GlobalEnv)    
    
    #remove first row because regions werent named so it shows up blank
    region_list_5<- list.remove(region_list_5, c(1))
  
    
      ###########################################
    ###########################################
    ###########################################
    #plot by region list 5    
    plotdf=list()
    
    for(i in 1: length(region_list_5)) 
    {
      df = as.data.frame(region_list_5[[i]])
      NAME <- unique(df$release_location_rmis_region)
      
      plotdf[[i]] <-  p_north_am +
        geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = gear_group, alpha=0.05)) +
        scale_alpha(guide = 'none')+
        colScale+
        facet_wrap(~run_year_five)+
        ggtitle(paste(as.character(NAME))) +
        labs(caption = "Recovery locations X gear type (title indicates associated release area)")
        theme_bw() 
      
    }
    
    print(plotdf[[1]])    
    
    #print all above graphs to one PDF
    pdf("Recoveries_region_5.pdf", width=13, height=8.5)
    for (i in 1:33) {
      print(plotdf[[i]])
    }
    dev.off()
    
    
    ###########################################
    ###########################################
    ######### RMIS DOMAINS ######## Grouped in 5 years  
    
    col.filters <- unique(dat_recovery_five$release_loc_domain) 
    
    lapply(seq_along(col.filters), function(x) {
      filter(dat_recovery_five, release_loc_domain == col.filters[x])
    }
    ) -> domain_list_5
    
    #remove last row because regions werent named so it shows up blank
    domain_list_5<- list.remove(domain_list_5, c(9))
    
    # names(domain_list_5) <- col.filters
    col.filters_dat <- as.data.frame(col.filters)
    # list2env(region_list_5, .GlobalEnv)    
    

    #plot by domain list 5    
    plotdf=list()
    
    for(i in 1: length(domain_list_5)) 
    {
      df = as.data.frame(domain_list_5[[i]])
      NAME <- unique(df$release_loc_domain)
      
      plotdf[[i]] <-  p_north_am +
        geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = gear_group, alpha=0.05)) +
        scale_alpha(guide = 'none')+
        colScale+
        facet_wrap(~run_year_five)+
        ggtitle(paste(as.character(NAME))) + 
        labs(caption = "Recovery locations X gear type (title indicates associated release area)")
        theme_bw() 
      
    }
    
    print(plotdf[[3]])    
    
    #print all above graphs to one PDF
    pdf("Recoveries_domain_5.pdf", width=13, height=8.5)
    for (i in 1:33) {
      print(plotdf[[i]])
    }
    dev.off()
    
    ###########################################
    ###########################################
    ######### RMIS REGIONS ######## single years
#seperate out different RMIS regions's so you can plot by basin
    #creates a list of data frames by basin
    #list can be used to plot in a loop later
    
    col.filters <- unique(dat_recovery_loc$release_location_rmis_region) 
    
    lapply(seq_along(col.filters), function(x) {
      filter(dat_recovery_loc, release_location_rmis_region == col.filters[x])
    }
    ) -> region_list
    
    names(region_list) <- col.filters
    
    list2env(region_list, .GlobalEnv)
   
  #plot by region list     
    plotdf=list()
    rn<- as.data.frame(region_names)
    
    rn_gather <- rn %>%
      gather(c(1:34)) 
      
      rn_gather <- rn_gather[, -1]
    
    for(i in 1:length(region_list)) {
    for(k in names(rn_gather))
    {
      df = as.data.frame(region_list[[i]])
      plotdf[[i]] <-  p_north_am +
        geom_point(data = df, mapping = aes(x = longitude, y = latitude, color = gear_group, alpha=0.05)) +
        colScale +
        facet_wrap(~run_year)+
        ggtitle(as.character(k))+
        theme_bw()
      
    }}
    
    
    print(plotdf[[2]])
  
##################################################
#EXTRA MAPS TRYING TO FIND THE BEST MAP

#not sure if this will be helpful
#how to get BC on a map
library(rgdal)

if (!file.exists("./src/ref/ne_50m_admin_1_states_provinces_lakes/ne_50m_admin_1_states_provinces_lakes.dbf")){
  download.file(file.path('http://www.naturalearthdata.com/http/',
                          'www.naturalearthdata.com/download/50m/cultural',
                          'ne_50m_admin_1_states_provinces_lakes.zip'), 
                f <- tempfile())
  unzip(f, exdir = "./src/ref/ne_50m_admin_1_states_provinces_lakes")
  rm(f)
}

region <- readOGR("./src/ref/ne_50m_admin_1_states_provinces_lakes", 'ne_50m_admin_1_states_provinces_lakes', encoding='UTF-8')

can_region <- subset(region, name %in% c("British Columbia"))
ggplot(can_region) + 
  aes(long,lat, group = group) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() + 
  guides(fill = FALSE)+
  theme_bw()



#could do this with WA, CA, OR
counties <- map_data("county")
ca_county <- subset(counties, region == "california")

ca_base + theme_nothing() + 
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top





#below works but is very slow
#create vectors with areas on interest
library(raster)
states    <- c('California', 'Alaska', 'Oregon', 'Washington')
provinces <- c("British Columbia", "Alberta")

us <- getData("GADM",country="USA",level=1)
canada <- getData("GADM",country="CAN",level=1)

us.states <- us[us$NAME_1 %in% states,]
ca.provinces <- canada[canada$NAME_1 %in% provinces,]

library(ggplot2)
base_map <- ggplot(us.states,aes(x=long,y=lat,group=group))+
  geom_path()+
  geom_path(data=ca.provinces)+
  coord_map()+
  theme_bw()
base_map

# ggmap for these maps
library(ggmap)
#make bbox for 1999 data
 #having issues with the bbox becuase it doesnt extend to the full west coast even though i know there are points there
 wbbox <- make_bbox(lon = dat_1999$longitude, lat = dat_1999$latitude, f = 1)

 w_map <- get_map(location = wbbox, maptype = "satellite", source = "google")
 ggmap(w_map) 

ggmap(w_map) + geom_point(data = dat_1999, mapping = aes(x = longitude, y = latitude, size= estimated_number, color = release_location_rmis_basin, alpha=0.05 ))
 

