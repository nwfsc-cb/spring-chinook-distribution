### process the distribution estimates
library(tidyr)
library(fields)
library(RColorBrewer)
library(viridis)
library(lemon)
library(ggplot2)
library(grid)
library(gridExtra)


Probs =  c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)

### THIS IS FOR THE AVERAGE DISTRIBUTION
MEAN  <- origin_loc_mean
SD    <- origin_loc_sd
MEDIAN  <- origin_loc_median
QUANT <- apply(samp$origin_mat,c(2,3,4),quantile,probs=Probs)
rownames(QUANT) <- Probs

### THIS IS FOR THE YEAR-SEASON DISTRIBUTION
# MEAN.mat  <- apply(samp$origin_mat,c(2,3,4),mean)
# QUANT.mat <- apply(samp$origin_mat,c(2,3,4),quantile,probs=Probs)
# rownames(QUANT.mat) <- Probs
# 
# mat_flat <- melt(QUANT.mat,varnames=c("quant","origin","year.season","loc"),value.name="prop") 
# mat_flat$quant <- as.character(mat_flat$quant)
# mat_flat_mean <- melt(MEAN.mat,varnames=c("origin","year.season","loc"),value.name="prop") %>% mutate(quant="Mean") %>% 
#                       dplyr::select(quant,origin,year.season,loc,prop)
# 
# mat_flat <- bind_rows(mat_flat_mean,mat_flat)
# 
# 
# lab <- data.frame(year.season=1:nrow(ocean_temp_dev),
#                   year = as.numeric(substr(rownames(ocean_temp_dev),1,4)),
#                   season =  substr(rownames(ocean_temp_dev),6,8))
# lab <- lab %>% mutate(year=case_when(season=="Win"~year-1,season!="Win"~year))
# 
# mat_flat <- left_join(mat_flat,lab) %>% 
#             left_join(.,spawn_loc_plot %>% rename(origin=loc.spawn.idx) %>% dplyr::select(ocean.region,origin)) %>% 
#             left_join(.,LOCATIONS %>% rename(loc=location.number))
# 
# mat_flat$ocean.region <- factor(mat_flat$ocean.region,
#                                   levels = c(as.character(spawn_loc$ocean.region)))
# mat_flat$season <- factor(mat_flat$season,
#                             levels = c("Spr", "Sum","Fal","Win"))
# mat_flat$location.name <- factor(mat_flat$location.name,
#                             levels = LOCATIONS$location.name)
# 
########################################################################
#########################################################################

# ocean_temp_agg <- ocean_temp %>% melt(id.vars =c("year","season")) %>%
#                       mutate("Site"=variable) %>% dplyr::select(-variable) %>%
#                       group_by(season,Site) %>% summarise(MEAN=mean(value))

SEAS <- c("Spr","Sum","Fal","Win")
origin_loc_long <- NULL
origin_all <- NULL

for(i in 1: N_season){
  A <- melt(origin_loc_mean[,i,]) %>% as.data.frame()
  colnames(A) <- c("origin","loc","prop")
  A$season = SEAS[i]
  origin_loc_long <- bind_rows(origin_loc_long,A)
  
  B <- melt(QUANT[,,i,],varnames=c("quant","origin","loc"),value.name="prop") %>% as.data.frame()
  B$season = SEAS[i]
  origin_all <- bind_rows(origin_all,B)
}

origin_loc_long <- left_join(origin_loc_long, LOCATIONS, by= c("loc" = "location.number"))
origin_all <- left_join(origin_all,LOCATIONS, by= c("loc" = "location.number"))
#origin_loc_long <- merge(origin_loc_long,LOCATIONS, by.x="loc",by.y="location.number")

colnames(origin_loc_long)[grep("location.name",colnames(origin_loc_long))] <- "Site"
colnames(origin_all)[grep("location.name",colnames(origin_all))] <- "Site"

origin_loc_long <- left_join(origin_loc_long,spawn_loc_plot[,c("init.loc","number","ocean.region","loc.spawn.idx")],
                             by=c("origin"="init.loc"))
#origin_loc_long$number <- floor(origin_loc_long$number)

origin_all <- left_join(origin_all,spawn_loc_plot[,c("init.loc","number","ocean.region","loc.spawn.idx")],
                    by=c("origin"="init.loc"))
#origin_all$number <- floor(origin_all$number)

####
origin_all$ocean.region <- factor(origin_all$ocean.region,
                                       levels = c(as.character(spawn_loc$ocean.region)))
origin_all$season <- factor(origin_all$season,
                                 levels = SEAS)

#################

### OK .
## origin_all is the average distribution with uncertainty.
## mat_flat is the estimated distribution for each season-year

ALP = 0.4
STOCK.PLOT <- list()
SPR.PLOT <- list()
SUM.PLOT <- list()
FALL.PLOT <- list()
WINT.PLOT <- list()

for(i in 1:nrow(spawn_loc_plot)){

  # DAT <- mat_flat %>% 
  #           filter(quant=="Mean",ocean.region==spawn_loc_plot$ocean.region[i],year>=1982, 
  #                   season %in% c("Spr","Sum","Fal")) 
  # DAT.sum<- DAT %>% filter(season=="Sum")

  DAT.MEAN <- origin_loc_long %>% filter(ocean.region==spawn_loc_plot$ocean.region[i], 
                                         season %in% c("Spr","Sum","Fal","Win")) 
  
  
  DAT.QUANT <- origin_all %>% filter(ocean.region==spawn_loc_plot$ocean.region[i], 
                                          season %in% c("Spr","Sum","Fal","Win")) 
  
# COMMON PLOTTING SETTINGS
END.COL <- 0.85
AT = sort(unique(DAT.MEAN$loc))
LABS = DAT.MEAN %>% arrange(loc) %>% distinct(Site) %>% pull(Site)
XLAB <- ""
YLAB = "Proportion"

###########################################
grob <- grobTree(textGrob(paste0(spawn_loc_plot$ocean.region[i],", Spring"), x=0.01,  y=0.95, hjust=0))

DAT.MEAN.spring <- DAT.MEAN %>% filter(season=="Spr")
DAT.QUANT.low <- DAT.QUANT %>% filter(season=="Spr") %>% filter(quant==0.050)
DAT.QUANT.high <- DAT.QUANT %>% filter(season=="Spr") %>% filter(quant==0.95)

SPR.PLOT[[paste0(spawn_loc_plot$ocean.region[i],".all.reg")]] <- 
  ggplot(DAT.MEAN.spring) +
  #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
  geom_line(aes(y=prop,x=loc),alpha=1) +
  # geom_line(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.2,alpha=0.8) +
  # geom_point(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.5,alpha=0.8) +
  #facet_wrap(~season) +
  # scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL) +
  geom_line(data=DAT.QUANT.low,aes(x=loc,y=prop),linetype="dashed",alpha=ALP) +
  geom_line(data=DAT.QUANT.high,aes(x=loc,y=prop),linetype="dashed",alpha=ALP) +
  scale_x_continuous(breaks=AT,labels=LABS) +
  xlab(XLAB)+
  ylab(YLAB)+
  scale_y_continuous(expand = expansion(mult=c(0.001,0.1)))+
  annotation_custom(grob) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))


###########################################
grob <- grobTree(textGrob(paste0(spawn_loc_plot$ocean.region[i],", Summer"), x=0.01,  y=0.95, hjust=0))

  DAT.MEAN.sum <- DAT.MEAN %>% filter(season=="Sum")
  DAT.QUANT.sum <- DAT.QUANT %>% filter(season=="Sum")
  DAT.QUANT.low <- DAT.QUANT %>% filter(season=="Sum") %>% filter(quant==0.050)
  DAT.QUANT.high <- DAT.QUANT %>% filter(season=="Sum") %>% filter(quant==0.95)

SUM.PLOT[[paste0(spawn_loc_plot$ocean.region[i],".all.reg")]] <- 
  ggplot(DAT.MEAN.sum) +
  #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
  geom_line(aes(y=prop,x=loc),alpha=1) +
  # geom_line(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.2,alpha=0.8) +
  # geom_point(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.5,alpha=0.8) +
  #facet_wrap(~season) +
  # scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL) +
  geom_line(data=DAT.QUANT.low,aes(x=loc,y=prop),linetype="dashed",alpha=ALP) +
  geom_line(data=DAT.QUANT.high,aes(x=loc,y=prop),linetype="dashed",alpha=ALP) +
  scale_x_continuous(breaks=AT,labels=LABS) +
  xlab(XLAB)+
  ylab(YLAB)+
  scale_y_continuous(expand = expansion(mult=c(0.001,0.1)))+
  annotation_custom(grob) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))

# This takes out the Puget Sound / SOG areas
# DAT1.sum      <- DAT.sum %>% filter(loc<=8|loc>=12) %>% arrange(loc)
# DAT.MEAN1.sum <- DAT.MEAN.sum %>% filter(loc<=8|loc>=12) %>% arrange(loc)
# 
# SUM.PLOT[[paste0(spawn_loc_plot$ocean.region[i],".coast")]] <- 
#   ggplot() +
#   #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
#   geom_line(data=DAT1.sum,aes(y=prop,x=location.name,group=year,color=year),alpha=ALP) +
#   geom_line( data=DAT.MEAN1.sum, aes(y=prop,x=Site,group=number),color="black",size=1.2,alpha=0.8) +
#   geom_point(data=DAT.MEAN1.sum, aes(y=prop,x=Site),color="black",size=1.5,alpha=0.8) +
#   #facet_wrap(~season) +
#   scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL) +
#   #scale_x_discrete(breaks=AT,labels=LABS) +
#   xlab(XLAB)+
#   ylab(YLAB)+
#   scale_y_continuous(expand = expand_scale(mult=c(0.001,0.1)))+
#   annotation_custom(grob) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) 

###########################################
###########################################  
##### FALL PLOTS
###########################################
###########################################
grob <- grobTree(textGrob(paste0(spawn_loc_plot$ocean.region[i],", Fall"), x=0.01,  y=0.95, hjust=0))

  DAT.MEAN.fall <- DAT.MEAN %>% filter(season=="Fal")
  DAT.QUANT.low <- DAT.QUANT %>% filter(season=="Fal") %>% filter(quant==0.050)
  DAT.QUANT.high <- DAT.QUANT %>% filter(season=="Fal") %>% filter(quant==0.95)
  
  FALL.PLOT[[paste0(spawn_loc_plot$ocean.region[i],".all.reg")]] <- 
    ggplot(DAT.MEAN.fall) +
    #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
    geom_line(aes(y=prop,x=loc),alpha=1) +
    # geom_line(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.2,alpha=0.8) +
    # geom_point(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.5,alpha=0.8) +
    #facet_wrap(~season) +
    # scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL) +
    geom_line(data=DAT.QUANT.low,aes(x=loc,y=prop),linetype="dashed",alpha=ALP) +
    geom_line(data=DAT.QUANT.high,aes(x=loc,y=prop),linetype="dashed",alpha=ALP) +
    scale_x_continuous(breaks=AT,labels=LABS) +
    xlab(XLAB)+
    ylab(YLAB)+
    scale_y_continuous(expand = expansion(mult=c(0.001,0.1)))+
    annotation_custom(grob) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
  
  ###########################################
  grob <- grobTree(textGrob(paste0(spawn_loc_plot$ocean.region[i],", Winter"), x=0.01,  y=0.95, hjust=0))
  
  DAT.MEAN.wint <- DAT.MEAN %>% filter(season=="Win")
  DAT.QUANT.low <- DAT.QUANT %>% filter(season=="Win") %>% filter(quant==0.050)
  DAT.QUANT.high <- DAT.QUANT %>% filter(season=="Win") %>% filter(quant==0.95)
  
  WINT.PLOT[[paste0(spawn_loc_plot$ocean.region[i],".all.reg")]] <- 
    ggplot(DAT.MEAN.wint) +
    #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
    geom_line(aes(y=prop,x=loc),alpha=1) +
    # geom_line(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.2,alpha=0.8) +
    # geom_point(data=DAT.MEAN.sum, aes(y=prop,x=loc),color="black",size=1.5,alpha=0.8) +
    #facet_wrap(~season) +
    # scale_color_viridis("Year",alpha=ALP,begin=0,end=END.COL) +
    geom_line(data=DAT.QUANT.low,aes(x=loc,y=prop),linetype="dashed",alpha=ALP) +
    geom_line(data=DAT.QUANT.high,aes(x=loc,y=prop),linetype="dashed",alpha=ALP) +
    scale_x_continuous(breaks=AT,labels=LABS) +
    xlab(XLAB)+
    ylab(YLAB)+
    scale_y_continuous(expand = expansion(mult=c(0.001,0.1)))+
    annotation_custom(grob) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
  
  
  DAT.MEAN$season <- factor(DAT.MEAN$season,levels=SEAS)
  
  #Plot a grid for each stock
  grob <- grobTree(textGrob(paste0(spawn_loc_plot$ocean.region[i]), x=0.01,  y=0.95, hjust=0))
  STOCK.PLOT[[paste0(spawn_loc_plot$ocean.region[i],".all.seas")]] <-
      ggplot(DAT.MEAN) +
      geom_line(aes(y=prop,x=loc),alpha=1) +
        scale_x_continuous(breaks=AT,labels=LABS) +
        xlab(XLAB)+
        ylab(YLAB)+
        scale_y_continuous(expand = expansion(mult=c(0.0001,0.1)))+
        annotation_custom(grob) +
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
        facet_wrap(~season,scales="free_y")

  
  origin_off_summary$ocean.reg <- factor(origin_off_summary$ocean.reg, levels=ORIGIN.GROUPS$ocean.reg)
  
  OFF.PLOT <- 
  ggplot(origin_off_summary) +
    geom_point(aes(y=Mean,x=ocean.reg),alpha=1) +
    geom_errorbar(aes(ymax=q.0.95,ymin=q.0.05,x=ocean.reg),alpha=ALP) +
    #scale_x_continuous(breaks=AT,labels=LABS) +
    scale_y_continuous(expand = expansion(mult=c(0.0001,0.1)))+
    #annotation_custom(grob) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
    facet_wrap(~season,scales="free_y")
  
}

###############################

pdf(file=paste(base.dir,"/spring-chinook-distribution/Output plots/Spatial Dist all years ",NAME,".pdf",sep=""),
       width=7,height=5,onefile = T)
  print(STOCK.PLOT)
  print(OFF.PLOT)
  print(SPR.PLOT)
  print(SUM.PLOT)
  print(FALL.PLOT)
  print(WINT.PLOT)
dev.off()

    