# Plots of temperature associations

# /////Calculate average temperature for each stock weighted by distribution.

# Go get temperatures in each year.
ocean_temp
ocean_temp_dev 
ocean_temp_avg
# Note that there are a few NA in Puget Sound in the raw data, but not in the deviation data. 
# The deviations have beeen linearly interpolated.  The next section adds the mean back to the deviations to 
# get the interpolated prediction for the missing seasons

# First, get rid of the early seasons of temperature (pre-1982) because they are predictions, not data. 
  ocean_temp_mod <- ocean_temp 

  # Next, fill in the NA from the ocean temperatures for PUSO. There are two at present (4-2020)
  ocean_temp_long <- pivot_longer(ocean_temp_mod,cols = as.character(LOCATIONS$location.name),names_to = "location.name",values_to="C")
  #ocean_temp_long$C <- ocean_temp_long$C# adjust the values to deg.C
  MISS <- ocean_temp_long %>% filter(is.na(C))
  ocean_temp_long <- ocean_temp_long %>% filter(is.na(C)==F)
  ocean_temp_dev_long <- pivot_longer(ocean_temp_dev %>% mutate(id=rownames(ocean_temp_dev)),cols = as.character(LOCATIONS$location.name),names_to = "location.name",values_to="dev.C.0.1")
  ocean_temp_dev_long <- ocean_temp_dev_long %>% mutate(year=as.numeric(substr(id,1,4)),season=substr(id,6,8)) %>% dplyr::select(-id)
  ocean_temp_dev_long$dev.C <- ocean_temp_dev_long$dev.C.0.1 * 10
  
  MISS <- left_join(MISS, ocean_temp_avg) %>% left_join(.,ocean_temp_dev_long) %>% 
            mutate(C=MEAN.trim+dev.C) %>% dplyr::select(-dev.C.0.1,-SD.trim,-MEAN.trim,-dev.C)
  
  ocean_temp_long <- full_join(ocean_temp_long,MISS)
  ocean_dist_long <- mat_flat
  
  equal.weight.temp <- ocean_temp_long %>% filter(year>=1982) %>% group_by(year,season) %>%
                          summarise(W.temp = mean(C)) %>% mutate(ocean.region="EQUAL",origin=99)
  
  
# Calculate spatial weights for each origin, season, and year
  # use the frame "mat_flat" from "Spatial location plots SS Climate.r".These are
  # the mean estimates of distribution for each season and year and origin.
  
  # USE ONE OF THE EARLY YEARS WHICH IS DEFINED AS THE AVERAGE DISTRIBUTION (dev.C= 0)
  ocean_temp_long_avg <- ocean_temp_avg %>% mutate(year.plot="MEAN") %>% 
                            mutate(C=MEAN.trim,sd.C=SD.trim) %>% dplyr::select(-MEAN.trim,-SD.trim) 
  ocean_dist_long_avg <- ocean_dist_long %>% filter(year==1979) %>% dplyr::select(-year.season,-year) 
  
  # This is the data.frame for calculating weighted temperature pretending each year had the time-series average temperatures.
  dist.avg.and.temp <- full_join(ocean_dist_long_avg, ocean_temp_long) %>% filter(year>=1982)
  dist.avg.and.temp.summary <- dist.avg.and.temp %>% filter(quant=="Mean") %>% 
                              mutate(temp.weight=C*prop) %>% 
                              group_by(year,season,ocean.region) %>%
                              summarise(W.temp.mean.prop=sum(temp.weight)) 
  
    
  # cull ocean_temp_long to only include 1982 and later.
  ocean_temp_long <- ocean_temp_long %>% filter(year>=1982) 
  
  ### SOME EXTRA STUFF TO DO HERE TO ADD IN INFORMATION ABOUT THE DISTRIBUTION UNDER dev.C=0 conditions
  #                    %>% bind_rows(.,ocean_temp_long_avg) 
  
  # dist.and.temp.summary is the weighted mean temperature for each stock-season-year
  dist.and.temp <- left_join(ocean_temp_long, ocean_dist_long)
  
  dist.and.temp.summary <- dist.and.temp %>% filter(quant=="Mean") %>% 
                      mutate(temp.weight=C*prop) %>% group_by(year,season,ocean.region,origin) %>%
                      summarise(W.temp=sum(temp.weight)) %>%
                      filter(!season=="Win")

  dist.and.temp.summary <- full_join(dist.and.temp.summary,equal.weight.temp) %>% filter(!season=="Win")
  
  dist.and.temp.summary <- dist.and.temp.summary %>% as.data.frame() %>%
                              mutate(ocean.region = 
                                       factor(ocean.region, 
                                              levels=c(as.character(spawn_loc$ocean.region),"EQUAL")))
  
  # merge in the weighted average Temp under average distribution
  dist.and.temp.summary <- left_join(dist.and.temp.summary, dist.avg.and.temp.summary) %>% as.data.frame() %>%
                            mutate(ocean.region = factor(ocean.region, 
                                    levels=c(as.character(spawn_loc$ocean.region),"EQUAL")))
  dist.and.temp.summary.long <- dist.and.temp.summary %>% pivot_longer(col=contains("W"),names_to="type",values_to = "C")
  
  ## Exploratory plots
  dist.and.temp.summary <- dist.and.temp.summary %>% 
                                      mutate(season.plot=case_when(season=="Spr"~"Spring",
                                                                    season=="Sum"~"Summer",
                                                                    season=="Fal"~"Fall")) %>%
                                      mutate(season.plot=factor(season.plot,levels=c("Spring","Summer","Fall")))
                                                
  
   Weighted.temp.all <- ggplot(dist.and.temp.summary) +
     geom_boxplot(aes(y=W.temp,x=ocean.region),outlier.shape=NA,fill="transparent") +
     geom_jitter(aes(y=W.temp,x=ocean.region),alpha=0.2,height=0,width=0.2) + 
     scale_y_continuous("Weighted mean temperature (C)",breaks=c(6:16))+
     xlab("Stock")+
     facet_wrap(~season.plot,nrow=3) + 
     theme_bw() +
      theme(axis.text.x=element_text(size=6))
   
  Weighted.temp.all
  
  # focal 6.
  STOCKS <- c("SFB","NCA","LCOL","MCOL","URB","SNAK","EQUAL")
  Weighted.temp.focal <- ggplot(dist.and.temp.summary %>% filter(ocean.region %in% STOCKS)) +
    geom_jitter(aes(y=W.temp,x=ocean.region),alpha=0.1,height=0,width=0.2) + 
    facet_wrap(~season,nrow=3) + 
    theme_bw()
  Weighted.temp.focal
  
  
  set.seed(991)
  Weighted.temp.focal.sum <- ggplot(dist.and.temp.summary %>% filter(ocean.region %in% STOCKS,season=="Sum")) +
    geom_boxplot(aes(y=W.temp,x=ocean.region),outlier.shape=NA,fill="transparent") +
    geom_jitter(aes(y=W.temp,x=ocean.region),alpha=0.2,height=0,width=0.2) + 
    scale_y_continuous("Weighted mean temperature (C)",breaks=c(8:14))+
    xlab("Stock") +
    #facet_wrap(~season,nrow=3) + 
    theme_bw()
  Weighted.temp.focal.sum
  
  quartz(file=paste0(base.dir,"/Salmon-Climate/Output plots/Weighted Mean Temperature Six Summer.jpeg",sep=""),
         height=3.25,width=5,dpi=600,type="jpeg")
  print(Weighted.temp.focal.sum)
  dev.off()
  
  quartz(file=paste0(base.dir,"/Salmon-Climate/Output plots/Weighted Mean Temperature All Stocks.jpeg",sep=""),
         height=7,width=7,dpi=600,type="jpeg")
  print(Weighted.temp.all)
  dev.off()
  
  
  
  ## Change in weighted temperature relative to average distribution
  weighted.temp.change <- ggplot(dist.and.temp.summary.long %>% filter(ocean.region %in% STOCKS)) +
      geom_point(aes(y=C,x=type),alpha=0.4) +
      geom_line(aes(y=C,x=type,group=year),alpha=0.4)+
    facet_grid(season~ocean.region)
  
  weighted.temp.change.sum <- ggplot(dist.and.temp.summary.long %>% filter(ocean.region %in% STOCKS,season=="Sum")) +
    geom_point(aes(y=C,x=type),alpha=0.4) +
    geom_line(aes(y=C,x=type,group=year),alpha=0.4)+
    facet_grid(season~ocean.region) +
    theme_bw()
  
  
  dist.and.temp.summary %>% group_by(ocean.region,season) %>% 
    summarise(median(W.temp),mean(W.temp),MAX=max(W.temp),MIN=min(W.temp)) %>% 
    mutate(DIFF=MAX-MIN) %>% filter(ocean.region%in%STOCKS,season=="Sum") %>% as.data.frame()
  
  