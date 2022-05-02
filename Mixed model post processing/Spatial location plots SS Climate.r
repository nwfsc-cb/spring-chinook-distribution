### process the distribution estimates
library(tidyr)
library(fields)
library(RColorBrewer)
library(viridis)
library(lemon)

Probs =  c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)

### THIS IS FOR THE AVERAGE DISTRIBUTION
MEAN  <- origin_loc_mean
SD    <- origin_loc_sd
MEDIAN  <- origin_loc_median
QUANT <- apply(origin_loc,c(2,3,4),quantile,probs=Probs)
rownames(QUANT) <- Probs

### THIS IS FOR THE YEAR-SEASON DISTRIBUTION
MEAN.mat  <- apply(samp$origin_mat,c(2,3,4),mean)
QUANT.mat <- apply(samp$origin_mat,c(2,3,4),quantile,probs=Probs)
rownames(QUANT.mat) <- Probs

mat_flat <- melt(QUANT.mat,varnames=c("quant","origin","year.season","loc"),value.name="prop") 
mat_flat$quant <- as.character(mat_flat$quant)
mat_flat_mean <- melt(MEAN.mat,varnames=c("origin","year.season","loc"),value.name="prop") %>% mutate(quant="Mean") %>% 
                      dplyr::select(quant,origin,year.season,loc,prop)

mat_flat <- bind_rows(mat_flat_mean,mat_flat)


lab <- data.frame(year.season=1:nrow(ocean_temp_dev),
                  year = as.numeric(substr(rownames(ocean_temp_dev),1,4)),
                  season =  substr(rownames(ocean_temp_dev),6,8))
lab <- lab %>% mutate(year=case_when(season=="Win"~year-1,season!="Win"~year))

mat_flat <- left_join(mat_flat,lab) %>% 
            left_join(.,spawn_loc_plot %>% rename(origin=loc.spawn.idx) %>% dplyr::select(ocean.region,origin)) %>% 
            left_join(.,LOCATIONS %>% rename(loc=location.number))

mat_flat$ocean.region <- factor(mat_flat$ocean.region,
                                  levels = c(as.character(spawn_loc$ocean.region)))
mat_flat$season <- factor(mat_flat$season,
                            levels = c("Spr", "Sum","Fal","Win"))
mat_flat$location.name <- factor(mat_flat$location.name,
                            levels = LOCATIONS$location.name)

########################################################################
#########################################################################

# ocean_temp_agg <- ocean_temp %>% melt(id.vars =c("year","season")) %>%
#                       mutate("Site"=variable) %>% dplyr::select(-variable) %>%
#                       group_by(season,Site) %>% summarise(MEAN=mean(value))

SEAS <- c("Spr","Sum","Fal")
origin_loc_long <- NULL
origin_all <- NULL
for(i in 1: N_season){
  A <- melt(origin_loc_mean[i,,]) %>% as.data.frame()
  colnames(A) <- c("origin","loc","prop")
  A$season = SEAS[i]
  origin_loc_long <- bind_rows(origin_loc_long,A)
  

  B <- melt(QUANT[,i,,],varnames=c("quant","origin","loc"),value.name="prop") %>% as.data.frame()
  B$season = SEAS[i]
  origin_all <- bind_rows(origin_all,B)
}

origin_loc_long <- merge(origin_loc_long,LOCATIONS, by.x="loc",by.y="location.number")
origin_all <- merge(origin_all,LOCATIONS, by.x="loc",by.y="location.number")

colnames(origin_loc_long)[grep("location.name",colnames(origin_loc_long))] <- "Site"
colnames(origin_all)[grep("location.name",colnames(origin_all))] <- "Site"

origin_loc_long <- merge(origin_loc_long,spawn_loc_plot[,c("number","ocean.region","loc.spawn.idx")],by.x=c("origin"),by.y=c("loc.spawn.idx"),all=F)
origin_loc_long$number <- floor(origin_loc_long$number)

origin_all <- merge(origin_all,spawn_loc_plot[,c("number","ocean.region","loc.spawn.idx")],by.x=c("origin"),by.y=c("loc.spawn.idx"),all=F)
origin_all$number <- floor(origin_all$number)

####
origin_all$ocean.region <- factor(origin_all$ocean.region,
                                       levels = c(as.character(spawn_loc$ocean.region)))
origin_all$season <- factor(origin_all$season,
                                 levels = c("Spr", "Sum","Fal"))

#################

### OK .
## origin_all is the average distribution with uncertainty.
## mat_flat is the estimated distribution for each season-year

ALP = 0.4
SUM.PLOT <- list()
FALL.PLOT <- list()

for(i in 1:nrow(spawn_loc_plot)){

  DAT <- mat_flat %>% 
            filter(quant=="Mean",ocean.region==spawn_loc_plot$ocean.region[i],year>=1982, 
                    season %in% c("Spr","Sum","Fal")) 
  DAT.sum<- DAT %>% filter(season=="Sum")

  DAT.MEAN <- origin_loc_long %>% filter(ocean.region==spawn_loc_plot$ocean.region[i], 
                                         season %in% c("Spr","Sum","Fal")) 
  DAT.MEAN.sum <- DAT.MEAN %>% filter(season=="Sum")
  
# COMMON PLOTTING SETTINGS
END.COL <- 0.85
AT = sort(unique(DAT$loc))
LABS = sort(unique(DAT$location.name))
XLAB <- ""
YLAB = "Proportion"
grob <- grobTree(textGrob(paste0(spawn_loc_plot$ocean.region[i],", Summer"), x=0.01,  y=0.95, hjust=0))
                          #gp=gpar(col="red", fontsize=13, fontface="italic")))

SUM.PLOT[[paste0(spawn_loc_plot$ocean.region[i],".all.reg")]] <- 
  ggplot(DAT.sum) +
  #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
  geom_line(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
  geom_line(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.2,alpha=0.8) +
  geom_point(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.5,alpha=0.8) +
  #facet_wrap(~season) +
  scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL) +
  scale_x_continuous(breaks=AT,labels=LABS) +
  xlab(XLAB)+
  ylab(YLAB)+
  scale_y_continuous(expand = expand_scale(mult=c(0.001,0.1)))+
  annotation_custom(grob) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))

# This takes out the Puget Sound / SOG areas
DAT1.sum      <- DAT.sum %>% filter(loc<=8|loc>=12) %>% arrange(loc)
DAT.MEAN1.sum <- DAT.MEAN.sum %>% filter(loc<=8|loc>=12) %>% arrange(loc)

SUM.PLOT[[paste0(spawn_loc_plot$ocean.region[i],".coast")]] <- 
  ggplot() +
  #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
  geom_line(data=DAT1.sum,aes(y=prop,x=location.name,group=year,color=year),alpha=ALP) +
  geom_line( data=DAT.MEAN1.sum, aes(y=prop,x=Site,group=number),color="black",size=1.2,alpha=0.8) +
  geom_point(data=DAT.MEAN1.sum, aes(y=prop,x=Site),color="black",size=1.5,alpha=0.8) +
  #facet_wrap(~season) +
  scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL) +
  #scale_x_discrete(breaks=AT,labels=LABS) +
  xlab(XLAB)+
  ylab(YLAB)+
  scale_y_continuous(expand = expand_scale(mult=c(0.001,0.1)))+
  annotation_custom(grob) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) 

###########################################
###########################################  
##### FALL PLOTS
###########################################
###########################################
grob <- grobTree(textGrob(paste0(spawn_loc_plot$ocean.region[i],", Fall"), x=0.01,  y=0.95, hjust=0))

  DAT.fall<- DAT %>% filter(season=="Fal")
  DAT.MEAN.fall <- DAT.MEAN %>% filter(season=="Fal")
  
  FALL.PLOT[[paste0(spawn_loc_plot$ocean.region[i],".all.reg")]] <- 
    ggplot(DAT.fall) +
    #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
    geom_line(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
    geom_line(data=DAT.MEAN.fall, aes(y=prop,x=loc),color="black",size=1.2,alpha=0.8) +
    geom_point(data=DAT.MEAN.fall, aes(y=prop,x=loc),color="black",size=1.5,alpha=0.8) +
    #facet_wrap(~season) +
    scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL) +
    scale_x_continuous(breaks=AT,labels=LABS) +
    xlab(XLAB)+
    ylab(YLAB)+
    scale_y_continuous(expand = expand_scale(mult=c(0.001,0.1)))+
    annotation_custom(grob) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
  
  # This takes out the Puget Sound / SOG areas
  DAT1.fall      <- DAT.fall %>% filter(loc<=8|loc>=12) %>% arrange(loc)
  DAT.MEAN1.fall <- DAT.MEAN.fall %>% filter(loc<=8|loc>=12) %>% arrange(loc)
  
  FALL.PLOT[[paste0(spawn_loc_plot$ocean.region[i],".coast")]] <- 
    ggplot() +
    #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
    geom_line(data=DAT1.fall,aes(y=prop,x=location.name,group=year,color=year),alpha=ALP) +
    geom_line( data=DAT.MEAN1.fall, aes(y=prop,x=Site,group=number),color="black",size=1.2,alpha=0.8) +
    geom_point(data=DAT.MEAN1.fall, aes(y=prop,x=Site),color="black",size=1.5,alpha=0.8) +
    #facet_wrap(~season) +
    scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL) +
    #scale_x_discrete(breaks=AT,labels=LABS) +
    xlab(XLAB)+
    ylab(YLAB)+
    scale_y_continuous(expand = expand_scale(mult=c(0.001,0.1)))+
    annotation_custom(grob) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
}

###############################

pdf(file=paste(base.dir,"/Salmon-Climate/Output plots/Spatial Dist all years ",NAME,".pdf",sep=""),
       width=7,height=5,onefile = T)
  print(SUM.PLOT)
  print(FALL.PLOT)
dev.off()



#############################################################
#############################################################
#############################################################
###----- This section pulls out info for 1980s, 1990s, 2000s, 2010s,
###----- plots the average for each decade x season 
#############################################################
#############################################################
#############################################################
### THIS DEFINES THE YEARS TO AVERAGE OVER. 
YRS <- data.frame(start=c(1982,1990,2000,2010),stop=c(1989,1999,2009,max(mat_flat$year)))
SEAS <- c("Spr","Sum","Fal")

lab <- data.frame(year.season=1:nrow(ocean_temp_dev),
                  year = as.numeric(substr(rownames(ocean_temp_dev),1,4)),
                  season =  substr(rownames(ocean_temp_dev),6,8))
lab <- lab %>% mutate(year=case_when(season=="Win"~year-1,season!="Win"~year))

mat_flat_by_dec <- NULL
for( i in 1:nrow(YRS)){
  for(j in 1:length(SEAS)){
      THESE <- which(lab$year >= YRS$start[i] & lab$year <= YRS$stop[i] & lab$season == SEAS[j])
  
      temp.dat <- samp$origin_mat[,,THESE,]
      
      MEAN.dec  <- apply(temp.dat,c(2,4),mean)
      QUANT.dec <- apply(temp.dat,c(2,4),quantile,probs=Probs)
      rownames(QUANT.dec) <- Probs

      mat_flat_dec <- melt(QUANT.dec,varnames=c("quant","origin","loc"),value.name="prop") 
      mat_flat_dec$quant <- as.character(mat_flat_dec$quant)
      mat_flat_mean_dec <- melt(MEAN.dec,varnames=c("origin","loc"),value.name="prop") %>% mutate(quant="Mean") %>% 
                            dplyr::select(quant,origin,loc,prop)

      mat_flat_by_dec <- bind_rows(mat_flat_mean_dec,mat_flat_dec) %>% 
                            mutate(start=YRS$start[i],stop=YRS$stop[i],season=SEAS[j]) %>% 
                            bind_rows(mat_flat_by_dec,.)
  }
}

      mat_flat_by_dec <-   left_join(mat_flat_by_dec,spawn_loc_plot %>% rename(origin=loc.spawn.idx) %>% dplyr::select(ocean.region,origin)) %>% 
                            left_join(.,LOCATIONS %>% rename(loc=location.number))

      mat_flat_by_dec$ocean.region <- factor(mat_flat_by_dec$ocean.region,
                                          levels = c(as.character(spawn_loc$ocean.region)))
      mat_flat_by_dec$season <- factor(mat_flat_by_dec$season,
                                      levels = c("Spr", "Sum","Fal","Win"))
      mat_flat_by_dec$location.name <- factor(mat_flat_by_dec$location.name,
                                            levels = LOCATIONS$location.name)

      mat_flat_by_dec <- mat_flat_by_dec %>% mutate(year.group = case_when(stop < 1990 ~ "1980s",
                                                                           stop > 1990 & stop < 2000 ~ "1990s",
                                                                           stop > 2000 & stop < 2010 ~ "2000s",
                                                                           stop > 2010 & stop < 2020 ~ "2010s"))

#################  PLOTTING BY DECADE.

      ### OK .
      ## mat_flat_dec is the estimated distribution for each season-decade.
      
      SUM.PLOT.dec <- list()
      FALL.PLOT.dec <- list()
      
      for(i in 1:nrow(spawn_loc_plot)){
        
        DAT <- mat_flat_by_dec %>% 
          filter(quant=="Mean",ocean.region==spawn_loc_plot$ocean.region[i],season %in% c("Spr","Sum","Fal")) 
        DAT.sum<- DAT %>% filter(season=="Sum")
        
        DAT.ribbon <-  mat_flat_by_dec %>% 
                              filter(quant %in% c("0.05","0.95"),ocean.region==spawn_loc_plot$ocean.region[i],season %in% c("Spr","Sum","Fal")) %>%
                              pivot_wider(names_from=quant,values_from = prop,id_cols=c("ocean.region","season","origin","loc","location.name","year.group")) %>%
                              rename(X.05 ='0.05',X.95='0.95')

        DAT.sum <- left_join(DAT.sum,DAT.ribbon)
        
        DAT.MEAN <- origin_loc_long %>% filter(ocean.region==spawn_loc_plot$ocean.region[i], 
                                               season %in% c("Spr","Sum","Fal")) 
        DAT.MEAN.sum <- DAT.MEAN %>% filter(season=="Sum")
        
        # COMMON PLOTTING SETTINGS
        ALP = 0.8
        ALP.ribbon = 0.1
        END.COL <- 0.85
        LINE.WT <- 1.2
        AT = sort(unique(DAT$loc))
        LABS = sort(unique(DAT$location.name))
        XLAB <- ""
        YLAB = "Proportion"
        grob <- grobTree(textGrob(paste0(spawn_loc_plot$ocean.region[i],", Summer"), x=0.01,  y=0.95, hjust=0))
        #gp=gpar(col="red", fontsize=13, fontface="italic")))
        
        SUM.PLOT.dec[[paste0(spawn_loc_plot$ocean.region[i],".all.reg")]] <- 
          ggplot(DAT.sum) +
          #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
          geom_line(aes(y=prop,x=loc,group=year.group,color=year.group),alpha=ALP,size=LINE.WT) +
          geom_ribbon(aes(x=loc,ymin = X.05, ymax = X.95,group=year.group,fill=year.group),alpha=ALP.ribbon) +
          #geom_line(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.2,alpha=0.8) +
          #geom_point(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.5,alpha=0.8) +
          #facet_wrap(~season) +
          scale_color_viridis_d("Year",alpha=ALP,begin=0,end=END.COL) +
          scale_fill_viridis_d("Year",alpha=ALP,begin=0,end=END.COL) +
          scale_x_continuous(breaks=AT,labels=LABS) +
          xlab(XLAB)+
          ylab(YLAB)+
          scale_y_continuous(expand = expand_scale(mult=c(0.001,0.1)))+
          annotation_custom(grob) +
          theme_bw()+
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
        
        # This takes out the Puget Sound / SOG areas
        DAT1.sum      <- DAT.sum %>% filter(loc<=8|loc>=12) %>% arrange(loc)
        DAT.MEAN1.sum <- DAT.MEAN.sum %>% filter(loc<=8|loc>=12) %>% arrange(loc)
        
        SUM.PLOT.dec[[paste0(spawn_loc_plot$ocean.region[i],".coast")]] <- 
          ggplot() +
          #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
          geom_line(data=DAT1.sum,aes(y=prop,x=location.name,group=year.group,color=year.group),alpha=ALP,size=LINE.WT) +
          geom_ribbon(data=DAT1.sum,aes(x=location.name,ymin = X.05, ymax = X.95,group=year.group,fill=year.group),alpha=ALP.ribbon) +
          # geom_line( data=DAT.MEAN1.sum, aes(y=prop,x=Site,group=number),color="black",size=1.2,alpha=0.8) +
          # geom_point(data=DAT.MEAN1.sum, aes(y=prop,x=Site),color="black",size=1.5,alpha=0.8) +
          #facet_wrap(~season) +
          scale_color_viridis_d("Year",alpha=ALP,begin=0,end=END.COL) +
          scale_fill_viridis_d("Year",alpha=ALP.ribbon,begin=0,end=END.COL) +
          #scale_x_discrete(breaks=AT,labels=LABS) +
          xlab(XLAB)+
          ylab(YLAB)+
          scale_y_continuous(expand = expand_scale(mult=c(0.001,0.1)))+
          annotation_custom(grob) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) 
        
        ###########################################
        ###########################################  
        ##### FALL PLOTS
        ###########################################
        ###########################################
        grob <- grobTree(textGrob(paste0(spawn_loc_plot$ocean.region[i],", Fall"), x=0.01,  y=0.95, hjust=0))
        
        DAT.fall<- DAT %>% filter(season=="Fal")
        DAT.fall <- left_join(DAT.fall,DAT.ribbon)
        DAT.MEAN.fall <- DAT.MEAN %>% filter(season=="Fal")
        
        FALL.PLOT.dec[[paste0(spawn_loc_plot$ocean.region[i],".all.reg")]] <- 
          ggplot(DAT.fall) +
          #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
          geom_line(aes(y=prop,x=loc,group=year.group,color=year.group),alpha=ALP,size=LINE.WT) +
          geom_ribbon(aes(x=loc,ymin = X.05, ymax = X.95,group=year.group,fill=year.group),alpha=ALP.ribbon) +
          # geom_line(data=DAT.MEAN.fall, aes(y=prop,x=loc),color="black",size=1.2,alpha=0.8) +
          # geom_point(data=DAT.MEAN.fall, aes(y=prop,x=loc),color="black",size=1.5,alpha=0.8) +
          #facet_wrap(~season) +
          scale_color_viridis_d("Year",alpha=ALP,begin=0,end=END.COL) +
          scale_fill_viridis_d("Year",alpha=ALP,begin=0,end=END.COL) +
          scale_x_continuous(breaks=AT,labels=LABS) +
          xlab(XLAB)+
          ylab(YLAB)+
          scale_y_continuous(expand = expand_scale(mult=c(0.001,0.1)))+
          annotation_custom(grob) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
        
        # This takes out the Puget Sound / SOG areas
        DAT1.fall      <- DAT.fall %>% filter(loc<=8|loc>=12) %>% arrange(loc)
        DAT.MEAN1.fall <- DAT.MEAN.fall %>% filter(loc<=8|loc>=12) %>% arrange(loc)
        
        FALL.PLOT.dec[[paste0(spawn_loc_plot$ocean.region[i],".coast")]] <- 
          ggplot() +
          #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
          geom_line(data=DAT1.fall,aes(y=prop,x=location.name,group=year.group,color=year.group),alpha=ALP,size=LINE.WT) +
          geom_ribbon(data=DAT1.fall,aes(x=location.name,ymin = X.05, ymax = X.95,group=year.group,fill=year.group),alpha=ALP.ribbon) +
          # geom_line( data=DAT.MEAN1.fall, aes(y=prop,x=Site,group=number),color="black",size=1.2,alpha=0.8) +
          # geom_point(data=DAT.MEAN1.fall, aes(y=prop,x=Site),color="black",size=1.5,alpha=0.8) +
          #facet_wrap(~season) +
          scale_color_viridis_d("Year",alpha=ALP,begin=0,end=END.COL) +
          scale_fill_viridis_d("Year",alpha=ALP,begin=0,end=END.COL) +
          #scale_x_discrete(breaks=AT,labels=LABS) +
          xlab(XLAB)+
          ylab(YLAB)+
          scale_y_continuous(expand = expand_scale(mult=c(0.001,0.1)))+
          annotation_custom(grob) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
      }
      

      ## WRITE TO FILE.
      pdf(file=paste(base.dir,"/Salmon-Climate/Output plots/Spatial Dist by decade ",NAME,".pdf",sep=""),
          width=7,height=5,onefile = T)
      print(SUM.PLOT.dec)
      print(FALL.PLOT.dec)
      dev.off()
      
      ###################################################################################
      ###################################################################################      
      ###################################################################################
      ###################################################################################      
      ###################################################################################
###################################################################################      
### - Make Multi-panel plot for Manuscript.
###################################################################################
###################################################################################      
      
# Six stocks of interest.
  STOCKS.6 <- c("SFB","NCA","LCOL","MCOL","SNAK","URB")    
  STOCKS.4 <- c("SFB","NCA","LCOL","URB")    

      DAT <- mat_flat %>% filter(quant=="Mean",ocean.region %in% STOCKS.6, season=="Sum",year>=1982)
      DAT.MEAN <- origin_loc_long %>% filter(ocean.region %in% STOCKS.6, season=="Sum")
      
      DAT.ribbon <- mat_flat %>% 
              filter(quant %in% c("0.05","0.95"),ocean.region %in% STOCKS.6, season %in% "Sum",year>=1982) %>%
              pivot_wider(names_from=quant,values_from = prop,id_cols=c("ocean.region","season","origin","loc","location.name","year")) %>%
              rename(X.05 ='0.05',X.95='0.95')
      DAT.sum <- left_join(DAT,DAT.ribbon)
      
      DAT.MEAN.ribbon <- origin_all %>% rename(location.name=Site) %>%
                filter(quant %in% c("0.05","0.95"),ocean.region %in% STOCKS.6, season %in% "Sum") %>%
                pivot_wider(names_from=quant,values_from = prop,id_cols=c("ocean.region","season","origin","loc","location.name")) %>%
                rename(X.05 ='0.05',X.95='0.95')
      
      DAT.MEAN.sum <- left_join(DAT.MEAN,DAT.MEAN.ribbon)      

      
      DAT.MEAN.sum$ocean.region <- factor(DAT.MEAN.sum$ocean.region,levels=STOCKS.6)
      DAT.sum$ocean.region <- factor(DAT.sum$ocean.region,levels=STOCKS.6)
      
      
      
      # COMMON PLOTTING SETTINGS
      ALP = 0.4
      ALP.ribbon = 0.4
      END.COL <- 0.85
      AT = sort(unique(DAT$loc))
      LABS = sort(unique(DAT$location.name))
      LABS <- as.character(LABS)
      LABS[LABS=="PUSO_out"] <- "SJDF"
      XLAB <- ""
      YLAB = "Proportion"
      grob <- grobTree(textGrob(paste0(spawn_loc_plot$ocean.region[i],", Summer"), x=0.01,  y=0.95, hjust=0))
      YEAR.BREAKS = c(min(DAT$year),1990,2000,2010,max(DAT$year))
      #gp=gpar(col="red", fontsize=13, fontface="italic")))
      
    six.stock.plot.all <- 
        ggplot() +
        #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
        geom_line(data=DAT.sum,aes(y=prop,x=loc,group=year),alpha=ALP,color=grey(0.5)) +
        geom_line(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.2,alpha=0.8) +
        geom_point(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.5,alpha=0.8) +
        geom_ribbon(data=DAT.MEAN.sum,aes(x=loc,ymin = X.05, ymax = X.95),alpha=ALP.ribbon) +
      
        facet_rep_wrap(~ ocean.region, scales='free_y', repeat.tick.labels = 'left',ncol=2) +
       # scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL,breaks=YEAR.BREAKS) +
        scale_x_continuous(breaks=AT,labels=LABS) +
        xlab(XLAB)+
        ylab(YLAB)+
        scale_y_continuous(expand = expand_scale(mult=c(0.02,0.1)))+
        #annotation_custom(grob) +
        theme_bw()+
        #theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
        coord_flip()
    
    six.stock.plot.all.wide <- 
      ggplot() +
      #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
      geom_line(data=DAT.sum,aes(y=prop,x=loc,group=year),alpha=ALP,color=grey(0.5)) +
      geom_line(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.2,alpha=0.8) +
      geom_point(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.5,alpha=0.8) +
      geom_ribbon(data=DAT.MEAN.sum,aes(x=loc,ymin = X.05, ymax = X.95),alpha=ALP.ribbon) +
      
      facet_rep_wrap(~ ocean.region, scales='free_y', repeat.tick.labels = 'left',ncol=3) +
      # scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL,breaks=YEAR.BREAKS) +
      scale_x_continuous(breaks=AT,labels=LABS) +
      xlab(XLAB)+
      ylab(YLAB)+
      scale_y_continuous(expand = expand_scale(mult=c(0.02,0.1)))+
      #annotation_custom(grob) +
      theme_bw()+
      #theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
      coord_flip()

    
    four.stock.plot.all <- 
      ggplot() +
      #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
      geom_line(data=DAT.sum %>% filter(ocean.region %in% STOCKS.4),
                  aes(y=prop,x=loc,group=year),alpha=ALP,color=grey(0.5)) +
      geom_line(data=DAT.MEAN.sum %>% filter(ocean.region %in% STOCKS.4),
                aes(y=prop,x=loc),color="black",size=1.2,alpha=0.8) +
      geom_point(data=DAT.MEAN.sum %>% filter(ocean.region %in% STOCKS.4),
                 aes(y=prop,x=loc),color="black",size=1.5,alpha=0.8) +
      geom_ribbon(data=DAT.MEAN.sum %>% filter(ocean.region %in% STOCKS.4),
                  aes(x=loc,ymin = X.05, ymax = X.95),alpha=ALP.ribbon) +
      facet_rep_wrap(~ ocean.region, scales='free_y', repeat.tick.labels = 'left',ncol=2) +
      #scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL,breaks=YEAR.BREAKS) +
      scale_x_continuous(breaks=AT,labels=LABS) +
      xlab(XLAB)+
      ylab(YLAB)+
      scale_y_continuous(expand = expand_scale(mult=c(0.02,0.1)))+
      #annotation_custom(grob) +
      theme_bw()+
#      theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
      coord_flip()
    
    ############# COASTAL ONLY PLOTS  
      DAT.trim      <- DAT.sum %>% filter(loc<=8|loc>=12) %>% arrange(loc)
      DAT.MEAN.trim <- DAT.MEAN.sum %>% filter(loc<=8|loc>=12) %>% arrange(loc) %>% 
                            mutate(loc.mod=case_when(loc<=8~as.numeric(loc),
                                                     loc>=12~loc-3))
      
    six.stock.plot.coast <- 
        ggplot() +
        #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
        geom_line(data=DAT.trim,aes(y=prop,x=location.name,group=year),alpha=ALP,color=grey(0.5)) +
        geom_line( data=DAT.MEAN.trim, aes(y=prop,x=location.name,group=number),color="black",size=1.2,alpha=0.8) +
        geom_point(data=DAT.MEAN.trim, aes(y=prop,x=location.name),color="black",size=1.5,alpha=0.8) +
        geom_ribbon(data=DAT.MEAN.trim %>% filter(ocean.region %in% STOCKS.4),
                    aes(x=loc.mod,ymin = X.05, ymax = X.95),alpha=ALP.ribbon) +
        facet_rep_wrap(~ ocean.region, scales='free_x', repeat.tick.labels = 'left',ncol=2) +
        #scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL,breaks=YEAR.BREAKS) +
        xlab(XLAB)+
        ylab(YLAB)+
        scale_y_continuous(expand = expand_scale(mult=c(0.02,0.1)))+
        #annotation_custom(grob) +
        theme_bw()+
        #theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
        coord_flip()
      
    six.stock.plot.coast.wide <- 
      ggplot() +
      #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
      geom_line(data=DAT.trim,aes(y=prop,x=location.name,group=year),alpha=ALP,color=grey(0.5)) +
      geom_line( data=DAT.MEAN.trim, aes(y=prop,x=location.name,group=number),color="black",size=1.2,alpha=0.8) +
      geom_point(data=DAT.MEAN.trim, aes(y=prop,x=location.name),color="black",size=1.5,alpha=0.8) +
      geom_ribbon(data=DAT.MEAN.trim %>% filter(ocean.region %in% STOCKS.4),
                  aes(x=loc.mod,ymin = X.05, ymax = X.95),alpha=ALP.ribbon) +
      facet_rep_wrap(~ ocean.region, repeat.tick.labels = 'left',ncol=3) +
      #scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL,breaks=YEAR.BREAKS) +
      xlab(XLAB)+
      ylab(YLAB)+
      scale_y_continuous(expand = expand_scale(mult=c(0.02,0.1)))+
      #annotation_custom(grob) +
      theme_bw()+
      #theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
      coord_flip()
    
    
    
    
    four.stock.plot.coast <- 
        ggplot() +
        #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
        geom_line(data=DAT.trim %>% filter(ocean.region %in% STOCKS.4),
                aes(y=prop,x=location.name,group=year,color=year),alpha=ALP,color=grey(0.5)) +
        geom_line( data=DAT.MEAN.trim %>% filter(ocean.region %in% STOCKS.4),
                 aes(y=prop,x=location.name,group=number),color="black",size=1.2,alpha=0.8) +
        
        geom_point(data=DAT.MEAN.trim %>% filter(ocean.region %in% STOCKS.4),
                 aes(y=prop,x=location.name),color="black",size=1.5,alpha=0.8) +
        geom_ribbon(data=DAT.MEAN.trim %>% filter(ocean.region %in% STOCKS.4),
                    aes(x=loc.mod,ymin = X.05, ymax = X.95),alpha=ALP.ribbon) +
        facet_rep_wrap(~ ocean.region, scales='free_x', repeat.tick.labels = 'left',ncol=2) +
        #scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL,breaks=YEAR.BREAKS) +      
        xlab(XLAB)+
        ylab(YLAB)+
        scale_y_continuous(expand = expand_scale(mult=c(0.02,0.1)))+
        #annotation_custom(grob) +
        theme_bw()+
        #theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
        coord_flip()
    
    
    four.stock.plot.coast
    
    
      
    ###### WRITE THESE PLOTS TO FILE
    ## WRITE TO FILE.
    quartz(file=paste0(base.dir,"/Salmon-Climate/Output plots/Spatial Plot FOUR STOCK-COAST, ANNUAL ",NAME,".jpeg",sep=""),
                  height=7,width=6,dpi=600,type="jpeg")
      print(four.stock.plot.coast)
    dev.off()
    
    quartz(file=paste0(base.dir,"/Salmon-Climate/Output plots/Spatial Plot FOUR STOCK-ALL, ANNUAL ",NAME,".jpeg",sep=""),
           height=6,width=8,dpi=600,type="jpeg")
      print(four.stock.plot.all)
    dev.off()
    
    quartz(file=paste0(base.dir,"/Salmon-Climate/Output plots/Spatial Plot SIX STOCK-COAST, ANNUAL ",NAME,".jpeg",sep=""),
           height=8,width=8,dpi=600,type="jpeg")
      print(six.stock.plot.coast)
    dev.off()
    
    quartz(file=paste0(base.dir,"/Salmon-Climate/Output plots/Spatial Plot SIX STOCK-COAST wide, ANNUAL ",NAME,".jpeg",sep=""),
           height=8,width=9,dpi=600,type="jpeg")
        print(six.stock.plot.coast.wide)
    dev.off()
    
    
    quartz(file=paste0(base.dir,"/Salmon-Climate/Output plots/Spatial Plot SIX STOCK-ALL, ANNUAL ",NAME,".jpeg",sep=""),
           height=8,width=8,dpi=600,type="jpeg")
    print(six.stock.plot.all)
    dev.off()
    
    quartz(file=paste0(base.dir,"/Salmon-Climate/Output plots/Spatial Plot SIX STOCK-ALL wide, ANNUAL ",NAME,".jpeg",sep=""),
           height=8,width=9,dpi=600,type="jpeg")
    print(six.stock.plot.all.wide)
    dev.off()
    