library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(mapdata)
library(scales)
library(raster)

#This script codes the base map for other spatial graphing, 
                #also has broader graphing of the fishery types through year, 5 years, and all years

dat_recovery_loc = read.csv("dat_recovery_loc.csv", stringsAsFactors = FALSE)

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

#adding spatial bounds
      #load dataset called spatial_bounds_gs.xlsx in chinook_bycatch/maps
      df<- ocean_area
      df<- df %>%
        filter(state == 'ak')

d<- p_north_am +
    geom_segment(data = df, colour="red", aes(x = as.numeric(line.start.lon), 
                                 y = as.numeric(line.start.lat), 
                                 xend = as.numeric(line.end.lon), 
                                 yend = as.numeric(line.end.lat)))
d

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
    #I MAY HAVE MESSED THIS UP...MAY NEED TO BE FIXED#####
    
    
    #create for loop to go through region_list and create 5 year categories 
    #go into region list
    #dplyr stuff group and count create new column and new df
    #region_list_5
    #put region list 5 into a list using same code that created region_liust
   dat_recovery_loc$run_year <-as.numeric(dat_recovery_loc$run_year)

    dat_recovery_loc <-  dat_recovery_loc %>% unite(lat_long, latitude, longitude, remove=FALSE) 
    dat_recovery_five <- dat_recovery_loc %>%
      group_by(release_loc_domain, release_location_rmis_region, run_year_five, latitude, longitude, lat_long, gear_group) %>%
      summarise(est_sum_five_yr=sum(estimated_number))

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
   