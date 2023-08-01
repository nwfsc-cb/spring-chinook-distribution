### process the distribution estimates from the hurdle model.

library(fields)
library(RColorBrewer)

gini.simp.div <- function(prop){
  return(1 - sum(prop^2))
}

 MEAN  <- origin_loc_mean
 SD    <- origin_loc_sd
 MEDIAN  <- origin_loc_median
 QUANT <- apply(origin_loc,c(2,3,4),quantile,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975))

 LOCATIONS <- LOCATIONS %>% mutate(plot.name=ifelse(location.name == "PUSO_out","SJDF",location.name))
 
 
 # mean.long <- melt(MEAN) 
 #  colnames(mean.long)[1] <- "origin"
 #  colnames(mean.long)[2] <- "ocean.loc"
##################### 
  plot.prop.heatmap <- function(temp,title){
    
    par(mar=c(5,5,1,1))
    z.lim	<- c(0.0,0.60)
    y.lab <- spawn_loc$ocean.region  
    x.lab	<-	LOCATIONS$plot.name
    #col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
    col.br <- colorRampPalette(c(grey(0.9), "red"))
    par(mfrow=c(1,1),oma=c( 0,0,0,4) )
    
    temp[temp > z.lim[2]] <- z.lim[2]-1e-8
    
    image(z=t(temp),y=1:nrow(temp),x=LOCATIONS$location.number,axes=F,ylab="",xlab="",
          col=col.br(32),zlim=z.lim)
    box(bty="o",lwd=2)
    axis(1,las=2,at=LOCATIONS$location.number,labels=x.lab,hadj=0.85)
    axis(2,las=2,at=spawn_loc$init.loc,labels=y.lab,hadj=0.85)
    title(ylab="Origin",line=4)
    title(xlab="Ocean region",line=3.5)
    title(main=title)
    par(mfrow=c(1,1),oma=c( 0,0,0,0) )
    ticks <- seq(min(z.lim),max(z.lim),length.out=6)
    
    image.plot(temp,legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
    
  }
  
#if(SEASON == TRUE){  
    pdf(paste(base.dir,"/spring-chinook-distribution/Output plots/","Distribution heatmap ",NAME,".pdf",sep=""),height = 6, width=8.5)
    
    plot.prop.heatmap( temp=MEAN[,1,],title= "Spring estimated ocean distribution (MEAN)")
    plot.prop.heatmap( temp=MEAN[,2,],title= "Summer estimated ocean distribution(MEAN)")
    plot.prop.heatmap( temp=MEAN[,3,],title= "Fall estimated ocean distribution(MEAN)")
    plot.prop.heatmap( temp=MEAN[,4,],title= "Winter estimated ocean distribution(MEAN)")
    
    # plot.prop.heatmap( temp=MEDIAN[1,,],title= "Winter-Spring estimated ocean distribution(MEDIAN)")
    # plot.prop.heatmap( temp=MEDIAN[2,,],title= "Summer estimated ocean distribution(MEDIAN)")
    # plot.prop.heatmap( temp=MEDIAN[3,,],title= "Fall estimated ocean distribution(MEDIAN)")
    # 
    dev.off()
#}  

###################################################################################################  
####### 4x1 Panel plot for Publication
###################################################################################################
  
quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/Spatial Average Distribution plot ",NAME,".jpeg",sep=""),
           type="jpeg",dpi=600,width=4.5,height=9)

    layout.matrix <- matrix(c(c(1:8),c(9,9,9,9)), nrow = 4, ncol = 3)
    layout(mat=layout.matrix,
                 widths=c(1,0.1,0.1))

TEXT <- c("a) Spring","b) Summer", "c) Fall", "d) Winter")

#COL <- brewer.pal(32,Yl)
for(i in 1:N_season){  
  par(mar=c(3,8,1,1))
  temp <- MEAN[,i,] 
  z.lim	<- c(0.0,1)
  y.lab <- spawn_loc$ocean.region
  x.lab	<-	LOCATIONS$plot.name
  #col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  col.br <- colorRampPalette(c(grey(0.9),"orange","orangered", "red3"))
  #col.br <- colorRampPalette(COL)
  
  temp[temp > z.lim[2]] <- z.lim[2]-1e-8
  #par(oma=c( 0,0,0,4) )
  image(z=t(temp),y=1:nrow(temp),x=LOCATIONS$location.number,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim)
  box(bty="o",lwd=2)
  axis(1,las=2,at=LOCATIONS$location.number,labels=x.lab,hadj=0.7,cex.axis=0.8,tcl=-0.2)
  axis(2,las=2,at=spawn_loc$init.loc,labels=y.lab,hadj=1,tcl=-0.2,cex.axis=0.8)
  #if(i==2){title(ylab="Origin",line=2.7,cex.lab=1)}
  if(i==4){title(xlab="Ocean region",line=2.25,cex.lab=1)}
  #title(main=title)
  #par(mfrow=c(1,1),oma=c( 0,0,0,0) )
  mtext(TEXT[i],side=3,adj=0,cex=0.7)
  # if(i ==2){
  #   ticks <- seq(min(z.lim),max(z.lim),length.out=6)
  #   image.plot(temp,legend.only=T,smallplot=c(0.91,0.94, 0.25,0.9),col=col.br(32),
  #              zlim=z.lim,axis.args=list(at= ticks,cex.axis=0.7,hadj=0.5,tcl=-0.3),add=T,legend.cex=0.75)
  # }
}


# print offshore
for(i in 1:N_season){
  temp <- origin_off_summary %>% filter(season==i) %>% dplyr::select(Mean)
  par(mar=c(3,1,1,1))
  image(z=t(temp$Mean),y=1:nrow(temp),x=1,axes=F,ylab="",xlab="",
      col=col.br(32),zlim=z.lim)
  box(bty="o",lwd=2)
  axis(1,las=2,at=1,labels="Offshore",hadj=0.7,cex.axis=0.8,tcl=-0.2)
}

#Colorbar
   ticks <- seq(min(z.lim),max(z.lim),length.out=6)
   #par(mar=c(3,1,1,1))
   imagePlot(z=t(temp$Mean),x=1,y=1,
              legend.only=TRUE,smallplot=c(0.91,0.94, 0.6,0.9),
              zlim=z.lim,
              axis.args=list(at= ticks,cex.axis=0.7,hadj=0.5,tcl=-0.3),
              col=col.br(32),
              add=FALSE,
              legend.cex=1.2)

  #  
  #  colorBar(breaks=ticks,
  #            smallplot=c(0.91,0.94, 0.6,0.9),
  #            axis.args=list(at= ticks,cex.axis=0.7,hadj=0.5,tcl=-0.3),
  #            col=col.br(32))
  #  
  #  
  # SEASON.lab <- c("spring","summer","fall")  
  # temp <- expand.grid(origin=spawn_loc$ocean.region,season=SEASON.lab)
  # temp$plot.numb[temp$season=="spring"] <- 1
  # temp$plot.numb[temp$season=="summer"] <- 2
  # temp$plot.numb[temp$season=="fall"]   <- 3
  dev.off()
  
  
  ##############################################################
  ##############################################################
  ##############################################################
  ##############################################################
  ##############################################################
  ##############################################################
  ##############################################################
  ##############################################################
  ##############################################################
  ##############################################################
  
  ##############################################################
  #### SEASONAL PUB PLOTS
  quartz(file=paste(base.dir,"/spring-chinook-distribution/Output plots/Spatial Average Distribution plot SPRING ",NAME,".jpeg",sep=""),
         type="jpeg",dpi=600,width=6,height=4)
  
  layout.matrix <- matrix(c(1,2), nrow = 1, ncol = 2)
  layout(mat=layout.matrix,
         widths=c(1,0.15))
  
  TEXT <- c("Spring")
  
  #COL <- brewer.pal(32,Yl)
  par(mar=c(3.25,5,1,1))
  temp <- MEAN[,1,] 
  z.lim	<- c(0.0,0.6)
  y.lab <- spawn_loc$ocean.region
  x.lab	<-	LOCATIONS$plot.name
  #col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  col.br <- colorRampPalette(c(grey(0.9),"orange","orangered", "red3"))
  #col.br <- colorRampPalette(COL)
  
  temp[temp > z.lim[2]] <- z.lim[2]-1e-8
  #par(oma=c( 0,0,0,4) )
  image(z=t(temp),y=1:nrow(temp),x=LOCATIONS$location.number,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim)
  box(bty="o",lwd=2)
  axis(1,las=2,at=LOCATIONS$location.number,labels=x.lab,hadj=0.6,cex.axis=0.7,tcl=-0.2)
  axis(2,las=2,at=spawn_loc$init.loc,labels=y.lab,hadj=0.8,tcl=-0.2,cex.axis=0.5)
  title(ylab="Stock",line=3.7,cex.lab=1)
  title(xlab="Ocean region",line=2.25,cex.lab=1)
  mtext("Spring",side=3,adj=0,cex=0.7)
  
  # Offshore
  temp <- origin_off_summary %>% filter(season==2) %>% dplyr::select(Mean)
  temp <- temp %>% mutate(Mean=ifelse(Mean>z.lim[2],z.lim[2],Mean))
  par(mar=c(3.25,0,1,2.5))
  image(z=t(temp$Mean),y=1:nrow(temp),x=1,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim)
  box(bty="o",lwd=2)
  axis(1,las=2,at=1,labels="Offshore",hadj=0.7,cex.axis=0.8,tcl=-0.2)  
  
  # Color Bar
  ticks <- seq(min(z.lim),max(z.lim),length.out=6)
  ticks.lab <- ticks; ticks.lab[6] <- paste0(">",ticks.lab[6])
  image.plot(temp,legend.only=T,smallplot=c(0.45,0.60, 0.25,0.9),col=col.br(32),
             zlim=z.lim,axis.args=list(at= ticks,labels=ticks.lab,cex.axis=0.5,hadj=0.8,tcl=-0.3),add=T,legend.cex=0.7)
  
  dev.off()
  
  
  
  #### SUMMER ONLY PUB PLOT
  quartz(file=paste(base.dir,"/spring-chinook-distribution/Output plots/Spatial Average Distribution plot SUMMER ",NAME,".jpeg",sep=""),
         type="jpeg",dpi=600,width=6,height=4)
  
  layout.matrix <- matrix(c(1,2), nrow = 1, ncol = 2)
  layout(mat=layout.matrix,
         widths=c(1,0.15))
  
  TEXT <- c("Summer")
  
  #COL <- brewer.pal(32,Yl)
    par(mar=c(3.25,5,1,1))
    temp <- MEAN[,2,] 
    z.lim	<- c(0.0,0.6)
    y.lab <- spawn_loc$ocean.region
    x.lab	<-	LOCATIONS$plot.name
    #col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
    col.br <- colorRampPalette(c(grey(0.9),"orange","orangered", "red3"))
    #col.br <- colorRampPalette(COL)
    
    temp[temp > z.lim[2]] <- z.lim[2]-1e-8
    #par(oma=c( 0,0,0,4) )
    image(z=t(temp),y=1:nrow(temp),x=LOCATIONS$location.number,axes=F,ylab="",xlab="",
          col=col.br(32),zlim=z.lim)
    box(bty="o",lwd=2)
    axis(1,las=2,at=LOCATIONS$location.number,labels=x.lab,hadj=0.6,cex.axis=0.7,tcl=-0.2)
    axis(2,las=2,at=spawn_loc$init.loc,labels=y.lab,hadj=0.8,tcl=-0.2,cex.axis=0.5)
    title(ylab="Stock",line=3.7,cex.lab=1)
    title(xlab="Ocean region",line=2.25,cex.lab=1)
    mtext("Summer",side=3,adj=0,cex=0.7)
    
    # Offshore
      temp <- origin_off_summary %>% filter(season==2) %>% dplyr::select(Mean)
      temp <- temp %>% mutate(Mean=ifelse(Mean>z.lim[2],z.lim[2],Mean))
      par(mar=c(3.25,0,1,2.5))
      image(z=t(temp$Mean),y=1:nrow(temp),x=1,axes=F,ylab="",xlab="",
            col=col.br(32),zlim=z.lim)
      box(bty="o",lwd=2)
      axis(1,las=2,at=1,labels="Offshore",hadj=0.7,cex.axis=0.8,tcl=-0.2)  
    
    # Color Bar
      ticks <- seq(min(z.lim),max(z.lim),length.out=6)
      ticks.lab <- ticks; ticks.lab[6] <- paste0(">",ticks.lab[6])
      image.plot(temp,legend.only=T,smallplot=c(0.45,0.60, 0.25,0.9),col=col.br(32),
                 zlim=z.lim,axis.args=list(at= ticks,labels=ticks.lab,cex.axis=0.5,hadj=0.8,tcl=-0.3),add=T,legend.cex=0.7)
      
  dev.off()
  
  #### FALL ONLY PUB PLOT
  quartz(file=paste(base.dir,"/spring-chinook-distribution/Output plots/Spatial Average Distribution plot FALL ",NAME,".jpeg",sep=""),
         type="jpeg",dpi=600,width=6,height=4)
  
  layout.matrix <- matrix(c(1,2), nrow = 1, ncol = 2)
  layout(mat=layout.matrix,
         widths=c(1,0.15))
  
  TEXT <- c("Fall")
  
  #COL <- brewer.pal(32,Yl)
  par(mar=c(3.25,5,1,1))
  temp <- MEAN[,3,] 
  z.lim	<- c(0.0,0.6)
  y.lab <- spawn_loc$ocean.region
  x.lab	<-	LOCATIONS$plot.name
  #col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  col.br <- colorRampPalette(c(grey(0.9),"orange","orangered", "red3"))
  #col.br <- colorRampPalette(COL)
  
  temp[temp > z.lim[2]] <- z.lim[2]-1e-8
  #par(oma=c( 0,0,0,4) )
  image(z=t(temp),y=1:nrow(temp),x=LOCATIONS$location.number,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim)
  box(bty="o",lwd=2)
  axis(1,las=2,at=LOCATIONS$location.number,labels=x.lab,hadj=0.6,cex.axis=0.7,tcl=-0.2)
  axis(2,las=2,at=spawn_loc$init.loc,labels=y.lab,hadj=0.8,tcl=-0.2,cex.axis=0.5)
  title(ylab="Stock",line=3.7,cex.lab=1)
  title(xlab="Ocean region",line=2.25,cex.lab=1)
  mtext("Fall",side=3,adj=0,cex=0.7)
  
  # Offshore
  temp <- origin_off_summary %>% filter(season==2) %>% dplyr::select(Mean)
  temp <- temp %>% mutate(Mean=ifelse(Mean>z.lim[2],z.lim[2],Mean))
  par(mar=c(3.25,0,1,2.5))
  image(z=t(temp$Mean),y=1:nrow(temp),x=1,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim)
  box(bty="o",lwd=2)
  axis(1,las=2,at=1,labels="Offshore",hadj=0.7,cex.axis=0.8,tcl=-0.2)  
  
  # Color Bar
  ticks <- seq(min(z.lim),max(z.lim),length.out=6)
  ticks.lab <- ticks; ticks.lab[6] <- paste0(">",ticks.lab[6])
  image.plot(temp,legend.only=T,smallplot=c(0.45,0.60, 0.25,0.9),col=col.br(32),
             zlim=z.lim,axis.args=list(at= ticks,labels=ticks.lab,cex.axis=0.5,hadj=0.8,tcl=-0.3),add=T,legend.cex=0.7)
  
  dev.off()
  
  
  #### FALL ONLY PUB PLOT
  quartz(file=paste(base.dir,"/spring-chinook-distribution/Output plots/Spatial Average Distribution plot WINTER ",NAME,".jpeg",sep=""),
         type="jpeg",dpi=600,width=6,height=4)
  
  layout.matrix <- matrix(c(1,2), nrow = 1, ncol = 2)
  layout(mat=layout.matrix,
         widths=c(1,0.15))
  
  TEXT <- c("WINTER")
  
  #COL <- brewer.pal(32,Yl)
  par(mar=c(3.25,5,1,1))
  temp <- MEAN[,3,] 
  z.lim	<- c(0.0,0.6)
  y.lab <- spawn_loc$ocean.region
  x.lab	<-	LOCATIONS$plot.name
  #col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  col.br <- colorRampPalette(c(grey(0.9),"orange","orangered", "red3"))
  #col.br <- colorRampPalette(COL)
  
  temp[temp > z.lim[2]] <- z.lim[2]-1e-8
  #par(oma=c( 0,0,0,4) )
  image(z=t(temp),y=1:nrow(temp),x=LOCATIONS$location.number,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim)
  box(bty="o",lwd=2)
  axis(1,las=2,at=LOCATIONS$location.number,labels=x.lab,hadj=0.6,cex.axis=0.7,tcl=-0.2)
  axis(2,las=2,at=spawn_loc$init.loc,labels=y.lab,hadj=0.8,tcl=-0.2,cex.axis=0.5)
  title(ylab="Stock",line=3.7,cex.lab=1)
  title(xlab="Ocean region",line=2.25,cex.lab=1)
  mtext("Winter",side=3,adj=0,cex=0.7)
  
  # Offshore
  temp <- origin_off_summary %>% filter(season==2) %>% dplyr::select(Mean)
  temp <- temp %>% mutate(Mean=ifelse(Mean>z.lim[2],z.lim[2],Mean))
  par(mar=c(3.25,0,1,2.5))
  image(z=t(temp$Mean),y=1:nrow(temp),x=1,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim)
  box(bty="o",lwd=2)
  axis(1,las=2,at=1,labels="Offshore",hadj=0.7,cex.axis=0.8,tcl=-0.2)  
  
  # Color Bar
  ticks <- seq(min(z.lim),max(z.lim),length.out=6)
  ticks.lab <- ticks; ticks.lab[6] <- paste0(">",ticks.lab[6])
  image.plot(temp,legend.only=T,smallplot=c(0.45,0.60, 0.25,0.9),col=col.br(32),
             zlim=z.lim,axis.args=list(at= ticks,labels=ticks.lab,cex.axis=0.5,hadj=0.8,tcl=-0.3),add=T,legend.cex=0.7)
  
  dev.off()
  
  
  
  
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
  
  # ocean_temp_agg <- ocean_temp %>% melt(id.vars =c("year","season")) %>%
  #                       mutate("Site"=variable) %>% dplyr::select(-variable) %>%
  #                       group_by(season,Site) %>% summarise(MEAN=mean(value))
  
  SEAS <- c("Spr","Sum","Fal")
  origin_loc_long <- NULL
  
  for(i in 1: N_season){
      A <- melt(origin_loc_mean[i,,]) %>% as.data.frame()
      colnames(A) <- c("origin","loc","prop")
      A$season = SEAS[i]
      origin_loc_long <- rbind(origin_loc_long,A)
  }
  
  origin_loc_long <- merge(origin_loc_long,LOCATIONS, by.x="loc",by.y="location.number")
  colnames(origin_loc_long)[grep("location.name",colnames(origin_loc_long))] <- "Site"
  
  origin_loc_long <- merge(origin_loc_long,spawn_loc_plot[,c("number","ocean.region","loc.spawn.idx")],by.x=c("origin"),by.y=c("loc.spawn.idx"),all=F)
  origin_loc_long$number <- floor(origin_loc_long$number)
  
 ## 3 EXCLUDE VALUES FROM SGEO AND PUSO when calculating distance to river
  origin_loc_long$coast.loc <- origin_loc_long$loc
  #origin_loc_long$coast.loc[origin_loc_long$coast.loc >11] <- origin_loc_long$coast.loc[origin_loc_long$coast.loc >11] -2
  
  origin_loc_long$Dist     <- origin_loc_long$coast.loc - origin_loc_long$number
  origin_loc_long$abs.dist <- abs(origin_loc_long$Dist)
  
  origin_loc_comb <- origin_loc_long #merge(origin_loc_long,ocean_temp_agg)
  # origin_loc_comb <- origin_loc_comb %>% #filter(!Site=="PUSO",!Site=="SGEO") %>%
  #                     filter(!ocean.region=="PUSO",!ocean.region=="SGEO",!ocean.region=="SWVI",!ocean.region=="WAC",!ocean.region=="NOR") %>%
  #                     as.data.frame()
  origin_loc_comb$ocean.region <- factor(origin_loc_comb$ocean.region,
                        levels = c(as.character(spawn_loc$ocean.region)))

  origin_loc_comb$season <- factor(origin_loc_comb$season,
                                         levels = c("Spr", "Sum","Fal"))
  
    
  ##### ALL MAIN ORIGINS, by Season
  all_seas <- list()
  XLAB <- "Distance from origin"
  YLAB <- "Proportion of population"
  
  all_seas[[1]] <- ggplot(origin_loc_comb %>% filter(season=="Spr")) +
    geom_point(aes(x=Dist,y=prop)) +
    geom_line(aes(x=Dist,y=prop)) +
    facet_wrap(~ocean.region) +
    geom_vline(xintercept=0,linetype="dashed") +
    xlim(-15,15) +
    ggtitle("Spring")+
    theme_bw()+
    labs(x=XLAB,y=YLAB )
  
  all_seas[[2]] <- ggplot(origin_loc_comb %>% filter(season=="Sum")) +
    geom_point(aes(x=Dist,y=prop)) +
    geom_line(aes(x=Dist,y=prop)) +
    facet_wrap(~ocean.region) +
    geom_vline(xintercept=0,linetype="dashed")+
    xlim(-15,15) +
    ggtitle("Summer")+
    theme_bw()+
    labs(x=XLAB,y=YLAB )
  
  all_seas[[3]] <- ggplot(origin_loc_comb %>% filter(season=="Fal")) +
    geom_point(aes(x=Dist,y=prop)) +
    geom_line(aes(x=Dist,y=prop)) +
    facet_wrap(~ocean.region) +
    geom_vline(xintercept=0,linetype="dashed")+
    xlim(-15,15) +
    ggtitle("Fall")+
    theme_bw()+
    labs(x=XLAB,y=YLAB )
  
  all_seas_oneplot <- list()
  all_seas_oneplot[[1]] <- ggplot(origin_loc_comb) +
    geom_point(aes(x=Dist,y=prop,color=season)) +
    geom_line(aes(x=Dist,y=prop,color=season)) +
    facet_wrap(~ocean.region) +
    geom_vline(xintercept=0,linetype="dashed") +
    xlim(-15,15) +
    ggtitle("All Seasons")+
    theme_bw()+
    labs(x=XLAB,y=YLAB )
  
  
  pdf(file=paste(base.dir,"/Salmon-Climate/Output plots/Distribution by origin, centered ",NAME,".pdf",sep=""),
         onefile=T,width=11,height=8.5)
    print(all_seas_oneplot)
    print(all_seas)
  dev.off()
  
  ########################################################
  ##########################################################################  
  ##########################################################################
  ##########################################################################
  ###   COLLECT Especially cool and warm Years    
  
  # Temperature Deviations calculated from 3 seasons model 
  ocean_temp_dev 

  #Select a few years of data 
    #summer
  
  COLD <- c(1991,2008)
  HOT  <- c(1983,1997)  
  
  COLD.DEV <- list()
  HOT.DEV  <- list()
  
  A <- paste(COLD,"Spr",sep=".")
  B <- paste(HOT,"Spr",sep=".")
  COLD.DEV[[1]] <- ocean_temp_dev %>% dplyr::filter(.,grepl(paste(A,collapse="|"),rownames(.)))
  HOT.DEV[[1]]  <- ocean_temp_dev %>% dplyr::filter(.,grepl(paste(B,collapse="|"),rownames(.)))

  A <- paste(COLD,"Sum",sep=".")
  B <- paste(HOT,"Sum",sep=".")
  COLD.DEV[[2]] <- ocean_temp_dev %>% dplyr::filter(.,grepl(paste(A,collapse="|"),rownames(.)))
  HOT.DEV[[2]]  <- ocean_temp_dev %>% dplyr::filter(.,grepl(paste(B,collapse="|"),rownames(.)))
  
  A <- paste(COLD,"Fal",sep=".")
  B <- paste(HOT,"Fal",sep=".")
  COLD.DEV[[3]] <- ocean_temp_dev %>% dplyr::filter(.,grepl(paste(A,collapse="|"),rownames(.)))
  HOT.DEV[[3]]  <- ocean_temp_dev %>% dplyr::filter(.,grepl(paste(B,collapse="|"),rownames(.)))
  # 
  
  #3 MAKE A MATRIX FOR INDICATOR VALUES
  NROW <- nrow(samp$origin_sea_int[,1,1,])
  NCOL <- ncol(samp$origin_sea_int[,1,1,])
  MAT <- matrix(1,NROW,NCOL)
  ##########################################MAT[,which(origin_vec==0)] <- 0
  
  SEASON <- c("spring","summer","fall")
  
  OUT <- NULL
  for(i in 1:length(SEASON)){
      for(j in 1:N_origin){
        AVG.YEAR <-  exp(samp$origin_sea_int[,i,j,] * MAT )/
                    rowSums(exp(samp$origin_sea_int[,i,j,] * MAT ))
        
        temp <- data.frame(Site = 1:ncol(COLD.DEV[[1]]))
        temp$origin <- spawn_loc$ocean.region[j]
        temp$origin.numb   <- floor(spawn_loc$number[j])
        temp$Year <- "Mean"
        temp$type <- "avg"
        temp$season <- SEASON[i]
        temp$MEAN <- colMeans(AVG.YEAR)
        temp$MEDIAN = apply(AVG.YEAR,2,median)
        temp$SD = apply(AVG.YEAR,2,sd)
        Q       <- apply(AVG.YEAR,2,quantile,probs=c(0.025,0.05,0.25,0.75,0.95,0.975)) %>% t() 
        colnames(Q) <- c("X.025","X.05","X.25","X.75","X.95","X.975")
        temp <- data.frame(cbind(temp,Q))
        
        OUT <- rbind(OUT,temp)
        
        
        for(k in 1:length(COLD)){  ## SELECTED YEARS
          slope.mat <- matrix(0,NROW,NCOL)
          for(q in 1:ncol(origin_sea_slope)){
            slope.mat[,q] <- samp$origin_sea_slope[ ,i,j,q] * COLD.DEV[[i]][k,q]
          }
          COLD.proj <- exp(samp$origin_sea_int[,i,j,] * MAT + slope.mat) /
                      rowSums(exp(samp$origin_sea_int[,i,j,] * MAT + slope.mat))
          for(q in 1:ncol(origin_sea_slope)){
              slope.mat[,q] <- samp$origin_sea_slope[ ,i,j,q] * HOT.DEV[[i]][k,q]
          }
          HOT.proj  <- exp(samp$origin_sea_int[,i,j,] * MAT + slope.mat) /
                      rowSums(exp(samp$origin_sea_int[,i,j,] * MAT + slope.mat))
    
        temp <- data.frame(Site = 1:ncol(COLD.proj))
        temp$origin <- spawn_loc$ocean.region[j]
        temp$origin.numb   <- floor(spawn_loc$number[j])
        temp$Year <- COLD[k]
        temp$type <- "cold"
        temp$season <- SEASON[i]
        temp$MEAN <- colMeans(COLD.proj)
        temp$MEDIAN = apply(COLD.proj,2,median)
        temp$SD = apply(COLD.proj,2,sd)
        Q       <- apply(COLD.proj,2,quantile,probs=c(0.025,0.05,0.25,0.75,0.95,0.975)) %>% t() 
        colnames(Q) <- c("X.025","X.05","X.25","X.75","X.95","X.975")
        temp <- data.frame(cbind(temp,Q))
    
        OUT <- rbind(OUT,temp)
        
        temp <- data.frame(Site = 1:ncol(HOT.proj))
        temp$origin <- spawn_loc$ocean.region[j]
        temp$origin.numb   <- floor(spawn_loc$number[j])
        temp$Year <- HOT[k]
        temp$type <- "hot"
        temp$season <- SEASON[i]
        temp$MEAN <- colMeans(HOT.proj)
        temp$MEDIAN = apply(HOT.proj,2,median)
        temp$SD = apply(HOT.proj,2,sd)
        Q       <- apply(HOT.proj,2,quantile,probs=c(0.025,0.05,0.25,0.75,0.95,0.975)) %>% t() 
        colnames(Q) <- c("X.025","X.05","X.25","X.75","X.95","X.975")
        temp <- data.frame(cbind(temp,Q))
        
        OUT <- rbind(OUT,temp)
        }
    }
  }
  
  ### OK.... now take the dataframe OUT and make some plots of hot, cold, and average years.

  ##### ALL MAIN ORIGINS, by Season
  all_seas <- list()
  XLAB <- "Distance from origin"
  YLAB <- "Proportion of population"
  
  OUT.trim <- OUT #%>% filter(origin %in% c("SFB","NCA","LCOL","MCOL","UCOL","SNAK","URB")) 
  
  OUT.trim$origin <-     factor(OUT.trim$origin,
                         levels = c(as.character(spawn_loc$ocean.region)))
  OUT.trim$Year   <-     factor(OUT.trim$Year,
                                levels = c("Mean",as.character(COLD),as.character(HOT)))
  
  
  COLS <- c("Mean" = "black",
            #"1985" = "#0000FF",
            "1991" = "#0000FF",
            "2008" = "#0000FF",
            "1983"  = "#DC143C",
            "1997"  = "#DC143C")
            #"2015"  = "#DC143C")

  # OUT.trim <-     factor(OUT.trim$origin,
  #                        levels = c("SFB","NCA","LCOL","MCOL","UCOL","SNAK"))
  # 
  
  summer <- list()
  
  XLAB <- "Distance from origin"
  YLAB <- "Proportion of population"
  
  spring <- list()
  summer <- list()
  fall   <- list()
  for(i in 1:length(unique(OUT.trim$origin))){
  spring[[i]] <-
      ggplot(OUT.trim %>% filter(origin==levels(OUT.trim$origin)[i],season=="spring")) +
      geom_point(aes(x=Site,y=MEAN,color=factor(Year))) +
      geom_line(aes(x=Site,y=MEAN,color=factor(Year)),size=1.5) +
      #facet_wrap(~origin) +
      scale_color_manual(values=COLS) +
      geom_vline(xintercept=floor(spawn_loc$number[i]),linetype="dashed") +
      #xlim(-13,13) +
      ggtitle(paste(spawn_loc$ocean.region[i],"spring"))+
      theme_bw() +
      labs(x=XLAB) +
      theme(axis.text.x=element_text(angle=60,hjust=1))+
      scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name)
    
  summer[[i]] <-
    ggplot(OUT.trim %>% filter(origin==levels(OUT.trim$origin)[i],season=="summer")) +
    geom_point(aes(x=Site,y=MEAN,color=factor(Year))) +
    geom_line(aes(x=Site,y=MEAN,color=factor(Year)),size=1.5) +
    #facet_wrap(~origin) +
    scale_color_manual(values=COLS) +
    geom_vline(xintercept=floor(spawn_loc$number[i]),linetype="dashed") +
    #xlim(-13,13) +
    ggtitle(paste(spawn_loc$ocean.region[i],"summer")) +
    theme_bw() +
    labs(x=XLAB) +
    theme(axis.text.x=element_text(angle=60,hjust=1))+
    scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name)
    
  fall[[i]] <-
    ggplot(OUT.trim %>% filter(origin==levels(OUT.trim$origin)[i],season=="fall")) +
    geom_point(aes(x=Site,y=MEAN,color=factor(Year))) +
    geom_line(aes(x=Site,y=MEAN,color=factor(Year)),size=1.5) +
    #facet_wrap(~origin) +
    scale_color_manual(values=COLS) +
    geom_vline(xintercept=floor(spawn_loc$number[i]),linetype="dashed") +
    #xlim(-13,13) +
    ggtitle(paste(spawn_loc$ocean.region[i],"fall"))+
    theme_bw() +
    labs(x=XLAB) +
    theme(axis.text.x=element_text(angle=60,hjust=1))+
    scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name)
  }
  ####
  facet.spring <-   
    ggplot(OUT.trim %>% filter(season=="spring")) +
    geom_point(aes(x=Site,y=MEAN,color=factor(Year))) +
    geom_line(aes(x=Site,y=MEAN,color=factor(Year)),size=1.1) +
    facet_wrap(~origin) +
    scale_color_manual(values=COLS) +
    #geom_vline(xintercept=floor(spawn_loc$number[i]),linetype="dashed") +
    #xlim(-13,13) +
    ggtitle(paste("Spring"))+
    theme_bw() +
    labs(x=XLAB) +
    theme(axis.text.x=element_text(angle=60,hjust=1))+
    scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name)
  
  facet.summer <-   
    ggplot(OUT.trim %>% filter(season=="summer")) +
    geom_point(aes(x=Site,y=MEAN,color=factor(Year))) +
    geom_line(aes(x=Site,y=MEAN,color=factor(Year)),size=1.1) +
    facet_wrap(~origin) +
    scale_color_manual(values=COLS) +
    #geom_vline(xintercept=floor(spawn_loc$number[i]),linetype="dashed") +
    #xlim(-13,13) +
    ggtitle(paste("Summer"))+
    theme_bw() +
    labs(x=XLAB) +
    theme(axis.text.x=element_text(angle=60,hjust=1))+
    scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name)
  
  facet.fall <-   
    ggplot(OUT.trim %>% filter(season=="fall")) +
    geom_point(aes(x=Site,y=MEAN,color=factor(Year))) +
    geom_line(aes(x=Site,y=MEAN,color=factor(Year)),size=1.1) +
    facet_wrap(~origin) +
    scale_color_manual(values=COLS) +
    #geom_vline(xintercept=floor(spawn_loc$number[i]),linetype="dashed") +
    #xlim(-13,13) +
    ggtitle(paste("Fall"))+
    theme_bw() +
    labs(x=XLAB) +
    theme(axis.text.x=element_text(angle=60,hjust=1))+
    scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name)
  
  
    
  pdf(file=paste(base.dir,"/Salmon-Climate/Output plots/Distribution Warm & Cold Years ",NAME,".pdf",sep=""),
      onefile=T,width=11,height=8.5)
      print(facet.spring)
      print(facet.summer)
      print(facet.fall)
      print(spring)
      print(summer)
      print(fall)
  dev.off()
  

##########################################################################  
##########################################################################
##########################################################################
##########################################################################
### Time-series for each origin, overall and by season 
  
  
  library(fields)
  library(RColorBrewer)
  library(viridis)
  
  plot.all.years <- function(temp,Title){
    
    temp <- as.data.frame(temp)
    colnames(temp) <- LOCATIONS$location.name
    temp <- temp %>% dplyr::select(-PUSO,-SGEO) %>% as.matrix()
    
    z.lim	<- c(0.0,max(temp))
    x.lab	=	LOCATIONS$location.name[LOCATIONS$location.name != "PUSO" &LOCATIONS$location.name != "SGEO"]
    col.br <- colorRampPalette(c(grey(0.9),"orange","orangered", "red3"))
    
    par(mfrow=c(1,1),oma=c( 0,0,0,4) )
    image(z=temp,x=1:nrow(temp),y=1:ncol(temp),axes=F,ylab="",xlab="",
          col=col.br(32),zlim=z.lim)
    box(bty="o",lwd=2)
    
    axis(2,las=2,at=1:ncol(temp),labels=x.lab)
    axis(1,las=2,at=seq(2,nrow(temp),by=4),labels=years.recover)
    title(main=Title)
    par(mfrow=c(1,1),oma=c( 0,0,0,0) )
    ticks <- seq(0,max(z.lim),length.out=6)
  
    image.plot(temp,legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list( at= ticks, labels=round(ticks,2)))
  }
  
  pdf(file=paste(base.dir,"/Salmon-Climate/Output plots/Time-series All Sites ",NAME,".pdf",sep=""),onefile = T,
         width=11,height=8.5)
    for(i in 1:N_origin){
      plot.all.years(origin_mat[i,,],Title=spawn_loc_plot$ocean.region[i])
    }
  dev.off()
  
  #### Plot by season
  # Operative indexing is "temperature_season_idx"
  
  plot.by.season <- function(dat,temp_idx,season,Title){
    
    if(season == "Spr"){ IDX <- 1; start.idx = IDX }
    if(season == "Sum"){ IDX <- 2; start.idx = IDX }
    if(season == "Fal"){ IDX <- 3; start.idx = IDX }
    if(season == "Win"){ IDX <- 4; start.idx = IDX }
    
    temp <- as.data.frame(dat[seq(start.idx,nrow(dat),by=N_month),])
    colnames(temp) <- LOCATIONS$location.name
    temp <- temp %>% dplyr::select(-PUSO,-SGEO,-PUSO_out) %>% as.matrix
    
    z.lim	<- c(0.0,max(temp))
    x.lab	=	LOCATIONS$location.name[LOCATIONS$location.name != "PUSO" &LOCATIONS$location.name != "SGEO" &LOCATIONS$location.name != "PUSO_out"]
    col.br <- colorRampPalette(c(grey(0.9),"orange","orangered", "red3"))
    
    par(mfrow=c(1,1),oma=c( 0,0,0,4) )
    image(z=temp,x=1:nrow(temp),y=1:ncol(temp),axes=F,ylab="",xlab="",
          col=col.br(32),zlim=z.lim)
    box(bty="o",lwd=2)
    
    axis(2,las=2,at=1:ncol(temp),labels=x.lab)
    THESE <- seq(2,nrow(temp),by=2)
    axis(1,las=2,at=THESE,labels=years.recover[THESE])
    title(main=paste(Title,season))
    par(mfrow=c(1,1),oma=c( 0,0,0,0) )
    ticks <- seq(0,max(z.lim),length.out=6)
    
    image.plot(temp,legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list( at= ticks, labels=round(ticks,2)))
  }
  
  # dat <- origin_mat[2,,]
  # season="Sum"
  # temp_idx <- temperature_season_idx

  pdf(file=paste(base.dir,"/Salmon-Climate/Output plots/Time-series by season ",NAME,".pdf",sep=""),onefile = T,
      width=11,height=8.5)
  
  season_nom <- c("Spr","Sum","Fal","Win")
  for(j in 1:N_origin){  
    for(i in 1:length(season_nom)){
      plot.by.season(dat = origin_mat[j,,],
                   temp_idx = temperature_season_idx, 
                   season=season_nom[i], 
                   Title= spawn_loc_plot$ocean.region[j])
    }  
  }
  
  dev.off()  
  
  #########################################################################
  #########################################################################
  #########################################################################
  #########################################################################
  #########################################################################
  
  ### Partition the fraction in each box into a smaller number of categories
  
  plot.ts.by.region <- function(dat,temp_idx,Title,samp,ocean_temp_dev,Row){
    
    #temp <- as.data.frame(dat[seq(start.idx,nrow(dat),by=N_month),])
    temp <- as.data.frame(dat)
    colnames(temp) <- LOCATIONS$location.name
    
    
    if(Output$loc_18 =="TWO_OR"){
    dat.1 <- temp %>% transmute(S_SOR = MONT+SFB+MEN+NCA, N_SOR = SOR+NOR+COL+WAC+PUSO+PUSO_out+SGEO+SWVI+NWVI+CBC+NBC+SSEAK+NSEAK,
                              S_FALCON = MONT+SFB+MEN+NCA+SOR+NOR, N_FALCON = COL+WAC+PUSO+PUSO_out+SGEO+SWVI+NWVI+CBC+NBC+SSEAK+NSEAK,
                              S_WAC =MONT+SFB+MEN+NCA+SOR+NOR+COL+PUSO+PUSO_out+WAC, N_WAC = SGEO+SWVI+NWVI+CBC+NBC+SSEAK+NSEAK) %>% as.data.frame()
    }
    if(Output$loc_18 =="NCA_SOR_PUSO"){
    dat.1 <- temp %>% transmute(S_SOR = MONT+SFB+MEN+NCA, N_SOR = COR+NOR+COL+WAC+PUSO+PUSO_out+SGEO+SWVI+NWVI+CBC+NBC+SSEAK+NSEAK,
                                S_FALCON = MONT+SFB+MEN+NCA+COR+NOR, N_FALCON = COL+WAC+PUSO+PUSO_out+SGEO+SWVI+NWVI+CBC+NBC+SSEAK+NSEAK,
                                S_WAC =MONT+SFB+MEN+NCA+COR+NOR+COL+PUSO+PUSO_out+WAC, N_WAC = SGEO+SWVI+NWVI+CBC+NBC+SSEAK+NSEAK) %>% as.data.frame()
    }
    dat.1$year <- as.numeric(substr(rownames(ocean_temp_dev),1,4))
    dat.1$season <- substr(rownames(ocean_temp_dev),6,8)
    
    dat.1$season <-  factor(dat.1$season,
                        levels = c("Spr", "Sum","Fal","Win"))

    ## Call the MCMC samples and calculate a sd for the sum of S_SOR, S_FALCON, S_WAC
    A <- samp$origin_mat[,Row,,1:LOCATIONS$location.number[LOCATIONS$location.name=="NCA"]]
    dat.1$S_SOR_05 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.05))
    dat.1$S_SOR_25 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.25))
    dat.1$S_SOR_50 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.50))
    dat.1$S_SOR_75 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.75))
    dat.1$S_SOR_95 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.95))

    A <- samp$origin_mat[,Row,,1:LOCATIONS$location.number[LOCATIONS$location.name=="NOR"]]
    dat.1$S_FALCON_05 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.05))
    dat.1$S_FALCON_25 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.25))
    dat.1$S_FALCON_50 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.50))
    dat.1$S_FALCON_75 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.75))
    dat.1$S_FALCON_95 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.95))
    
    A <- samp$origin_mat[,Row,,1:LOCATIONS$location.number[LOCATIONS$location.name=="PUSO_out"]]
    dat.1$S_WAC_05 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.05))
    dat.1$S_WAC_25 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.25))
    dat.1$S_WAC_50 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.50))
    dat.1$S_WAC_75 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.75))
    dat.1$S_WAC_95 <- apply(apply(A,c(1,2),sum),2,quantile,probs=c(0.95))
    
    p<- list()
    p[[1]] <- ggplot(dat.1,aes(y=S_SOR_50,x=year)) + 
                geom_ribbon(aes(x=year,ymin=S_SOR_05, ymax=S_SOR_95 ),alpha=0.3) +
                geom_ribbon(aes(x=year,ymin=S_SOR_25, ymax=S_SOR_75 ),alpha=0.3) +
                geom_point()+
                geom_line()+
                facet_grid(~season)+
                theme_bw()+
                ggtitle(Title)+          
                ylab("Prop. south of CA-OR border")
    p[[2]] <- ggplot(dat.1,aes(y=S_FALCON,x=year)) +
                geom_ribbon(aes(x=year,ymin= S_FALCON_05, ymax=S_FALCON_95),alpha=0.3) +
                geom_ribbon(aes(x=year,ymin= S_FALCON_25, ymax=S_FALCON_75),alpha=0.3) +
                geom_point()+
                geom_line()+
                facet_grid(~season)+
                theme_bw()+
                ggtitle(Title)+
                ylab("Prop. south of Cape Falcon")
    p[[3]] <-  ggplot(dat.1,aes(y=S_WAC,x=year)) + geom_point()+
                geom_ribbon(aes(x=year,ymin= S_WAC_05, ymax=S_WAC_95),alpha=0.3) +
                geom_ribbon(aes(x=year,ymin= S_WAC_25, ymax=S_WAC_75),alpha=0.3) +
                geom_line()+
                facet_grid(~season)+
                theme_bw()+
                ggtitle(Title)+          
                ylab("Prop. south of Canada")

    for(i in 1:3){
      print(p[[i]])      
    }
}

  pdf(file=paste(base.dir,"/Salmon-Climate/Output plots/Time-series region & season ",NAME,".pdf",sep=""),onefile = T,
      width=11,height=5)
    for(j in 1:N_origin){  
        plot.ts.by.region(dat = origin_mat[j,,],
                        samp=samp,
                        ocean_temp_dev=ocean_temp_dev,
                        Row=spawn_loc_plot$loc.spawn.idx[j],
                        Title= spawn_loc_plot$ocean.region[j])
    }
  dev.off()  
  
####################################################################  
####################################################################
####################################################################
####################################################################
  dat.mean <- NULL
  dat.sd <- NULL
  OM <- origin_mat
  OM_sd <- origin_mat_sd
  for(i in 1:N_origin){
    temp.mean <-OM[i,,]
    colnames(temp.mean) <- LOCATIONS$location.name
    temp.sd <-OM_sd[i,,]
    colnames(temp.sd) <- LOCATIONS$location.name
    dat.mean <- rbind(dat.mean, data.frame(  year = as.numeric(substr(rownames(ocean_temp_dev),1,4)),
                        season = substr(rownames(ocean_temp_dev),6,8),
                        origin = spawn_loc_plot$ocean.region[spawn_loc_plot$loc.spawn.idx==i],
                        temp.mean))
    dat.sd <- rbind(dat.sd, data.frame(  year = as.numeric(substr(rownames(ocean_temp_dev),1,4)),
                        season = substr(rownames(ocean_temp_dev),6,8),
                        origin = spawn_loc_plot$ocean.region[spawn_loc_plot$loc.spawn.idx==i],
                        temp.sd))
  }
  
  

  #####################################################################
  #####################################################################
  #####################################################################
  #####################################################################
  #####################################################################
  #####################################################################
  ### MAKE VERTICAL HISTOGRAMS FOR DIFFERENT ORIGIN FISH.
  #####################################################################
  #####################################################################
  #####################################################################
  #####################################################################
  #####################################################################
  #####################################################################
  
  ###
  
  ORIG <- unique(OUT.trim$origin)
  
  COLS <- c("SFB"= "#DAA520", #"#ffffcc",
            "NCA"="#B8860B",
            "SOR"="#3CB371",
            "COR"="#228B22",
            "NOR"="#006400",
            "LCOL"  ="#00BFFF",
            "MCOL"  = "#1E90FF",
            "UCOL"  = "#0000FF",
            "SNAK"  = "#A52A2A",
            "URB"   = "#006400",
            "WAC" ="#FF6347",
            "PUSO_S" ="#FF6347",
            "PUSO_N" ="#FF6347",
            "SGEO_S" ="#DC143C",
            "SGEO_N" ="#DC143C",
            "SWVI" = "#A52A2A")
  
  y.lim <-c(0,0.5)
  lab_coast <- LOCATIONS$location.name #LOCATIONS$location.name[LOCATIONS$location.name != "PUSO" & LOCATIONS$location.name != "SGEO" & LOCATIONS$location.name != "PUSO_out"]

  TITLE <- as.character(spawn_loc$ocean.region)
    #c("a) Central Valley","b) Klamath-Trinity","c) Lower Columbia","d) Middle Columbia", "e) Upper Columbia","f) Snake","g) Upriver Brights")
  P <- list()
  for(i in 1:length(ORIG)){
    exclude.sites <- LOCATIONS %>% #filter(location.name %in% c("PUSO","PUSO_out","SGEO")) %>% 
                        dplyr::select(location.number) %>% 
                        unlist() %>% as.numeric(as.character())
    
    temp <- OUT.trim %>% filter(origin==ORIG[i],season == "summer",type=="avg") #,!Site %in% exclude.sites)
    temp$Site[temp$Site> max(exclude.sites)] <- temp$Site[temp$Site>max(exclude.sites)]-length(exclude.sites)
    
    P[[i]] <-  ggplot(temp) + 
      geom_bar(aes(x = Site, y = MEAN), fill=COLS[i],stat = "identity") + 
      geom_errorbar(aes(x=Site,ymin=X.05,ymax=X.95),stat="identity",width=0) +
      coord_flip() +
      scale_y_continuous(expand = c(0, 0),limits=y.lim) +
      labs(x = " ", y = "",title= TITLE[i]) +
      scale_x_continuous(breaks = 1:length(lab_coast), labels=lab_coast,expand = c(0, 0)) +
      #scale_y_continuous() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.05, 0.05, 0.38), "lines")) 
  
  }  
  

  # quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/", "Mean Dist" ,NAME,".pdf",sep=""),dpi=600,height=6,width=7,type="pdf")   
  # Layout= matrix(c(1,2,3,4,5,6),nrow=3,ncol=3,byrow=T)
  # #QQ <- list(p1,p2,q1,q2)
  # multiplot(plotlist=P ,layout= Layout)
  # dev.off()
  # 
  
  pdf(file=paste(base.dir,"/Salmon-Climate/Output plots/", "Mean Dist by origin " ,NAME,".pdf",sep=""),height=4.5,width=4)   
    for(i in 1:length(ORIG)){
      print(P[[i]])
    }
  dev.off()
  
  ############################
  ############################
  ### OVERPLOT WARM and COLD YEARS
  ############################
  ############################
  
  ORIG <- unique(OUT.trim$origin)
  
  HOT= "#A52A2A"
  COLD= "#0000FF"
  
  
  y.lim <-c(0,0.5)
  lab_coast <- LOCATIONS$location.name[LOCATIONS$location.name != "PUSO" & LOCATIONS$location.name != "SGEO" & LOCATIONS$location.name != "PUSO_out"]
  
  TITLE <- TITLE # inherited from above
    
    #c("a) Central Valley","b) Klamath-Trinity","c) Lower Columbia","d) Middle Columbia", "e) Upper Columbia","f) Snake","g) Upriver Brights")
  QQ <- list()
  QQ_hot <- list()
  QQ_cold <- list()
  for(i in 1:length(ORIG)){

    exclude.sites <- LOCATIONS %>% filter(location.name %in% c("PUSO","PUSO_out","SGEO")) %>% dplyr::select(location.number) %>% 
      unlist() %>% as.numeric(as.character())
    
    temp <- OUT.trim %>% filter(origin==ORIG[i],season == "summer",Year %in% c(1997,2008))#,!Site %in% exclude.sites)
    temp$Site[temp$Site> max(exclude.sites)] <- temp$Site[temp$Site>max(exclude.sites)]-length(exclude.sites)
    
    QQ[[i]] <- ggplot() + 
      geom_bar(data=temp %>% filter(Year == 1997),aes(x = Site, y = MEAN), fill=HOT, alpha=0.5,stat = "identity") + 
      geom_bar(data=temp %>% filter(Year == 2008),aes(x = Site, y = MEAN), fill=COLD, alpha=0.5,stat = "identity") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0),limits=y.lim) +
      labs(x = " ", y = "",title= TITLE[i]) +
      scale_x_continuous(breaks = 1:length(lab_coast), labels=lab_coast,expand = c(0, 0)) +
      #scale_y_continuous() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.05, 0.05, 0.38), "lines")) 

    QQ_hot[[i]] <- ggplot() + 
      geom_bar(data=temp %>% filter(Year == 1997),aes(x = Site, y = MEAN), fill=HOT, alpha=0.5,stat = "identity") + 
      #geom_bar(data=temp %>% filter(Year == 2008),aes(x = Site, y = MEAN), fill=COLD, alpha=0.5,stat = "identity") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0),limits=y.lim) +
      labs(x = " ", y = "",title= TITLE[i]) +
      scale_x_continuous(breaks = 1:length(lab_coast), labels=lab_coast,expand = c(0, 0)) +
      #scale_y_continuous() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.05, 0.05, 0.38), "lines")) 
    
    QQ_cold[[i]] <- ggplot() + 
      #geom_bar(data=temp %>% filter(Year == 1997),aes(x = Site, y = MEAN), fill=HOT, alpha=0.5,stat = "identity") + 
      geom_bar(data=temp %>% filter(Year == 2008),aes(x = Site, y = MEAN), fill=COLD, alpha=0.5,stat = "identity") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0),limits=y.lim) +
      labs(x = " ", y = "",title= TITLE[i]) +
      scale_x_continuous(breaks = 1:length(lab_coast), labels=lab_coast,expand = c(0, 0)) +
      #scale_y_continuous() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.05, 0.05, 0.38), "lines")) 
  }  
  
  # quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/", "Warm-Cold compare" ,NAME,".pdf",sep=""),dpi=600,height=6,width=7,type="pdf")   
  # Layout= matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,ncol=3,byrow=T)
  # #QQ <- list(p1,p2,q1,q2)
  # multiplot(plotlist=QQ ,layout= Layout)
  # dev.off()
  
  
  pdf(file=paste(base.dir,"/Salmon-Climate/Output plots/", "Warm-Cold compare by origin " ,NAME,".pdf",sep=""),height=4.5,width=4)   
  for(i in 1:length(ORIG)){
    print(QQ[[i]])
    print(QQ_hot[[i]])
    print(QQ_cold[[i]])
  }
  dev.off()
  
  ##########################
  ### Make a difference plot from the average.
  ##########################
  
  HOT= "#A52A2A"
  COLD= "#0000FF"
  
  y.lim <-c(-0.10,0.10)
  lab_coast <- LOCATIONS$location.name[LOCATIONS$location.name != "PUSO" & LOCATIONS$location.name != "SGEO"]
  
  #TITLE <- c("a) Central Valley","b) Klamath-Trinity","c) Lower Columbia","d) Middle Columbia", "e) Upper Columbia","f) Snake")
  QQ <- list()
  QQ.hot <- list()
  QQ.cold <- list()
  
  for(i in 1:length(ORIG)){
    
    temp.avg  <- OUT.trim %>% filter(origin==ORIG[i],season == "summer",Year %in% c("Mean"),Site!=10,Site!=11)
    temp.hot <- OUT.trim %>% filter(origin==ORIG[i],season == "summer",Year %in% c(1997),Site!=10,Site!=11)
    temp.cold  <- OUT.trim %>% filter(origin==ORIG[i],season == "summer",Year %in% c(2008),Site!=10,Site!=11)
    
    temp.diff <- temp.avg %>% dplyr::select(Site,origin,season)
    temp.diff$cold <- temp.cold$MEAN - temp.avg$MEAN 
    temp.diff$hot <- temp.hot$MEAN - temp.avg$MEAN 
    
    temp.diff$Site[temp.diff$Site>10] <- temp.diff$Site[temp.diff$Site>10]-2
    
    QQ[[i]] <- ggplot(temp.diff) + 
      geom_bar(aes(x = Site, y = hot), fill=HOT, alpha=1,stat = "identity") + 
      geom_bar(aes(x = Site, y = cold), fill=COLD, alpha=1,stat = "identity") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0),limits=y.lim) +
      labs(x = " ", y = "",title= TITLE[i]) +
      scale_x_continuous(breaks = 1:length(lab_coast), labels=lab_coast,expand = c(0, 0)) +
      #scale_y_continuous() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.1, 0.05, 0.38), "lines")) 
    
    QQ.hot[[i]] <- ggplot(temp.diff) + 
      geom_bar(aes(x = Site, y = hot), fill=HOT, alpha=1,stat = "identity") + 
      #geom_bar(aes(x = Site, y = cold), fill=COLD, alpha=0.5,stat = "identity") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0),limits=y.lim) +
      labs(x = " ", y = "",title= TITLE[i]) +
      scale_x_continuous(breaks = 1:length(lab_coast), labels=lab_coast,expand = c(0, 0)) +
      #scale_y_continuous() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.1, 0.05, 0.38), "lines")) 
    
    QQ.cold[[i]] <- ggplot(temp.diff) + 
      #geom_bar(aes(x = Site, y = hot), fill=HOT, alpha=0.5,stat = "identity") + 
      geom_bar(aes(x = Site, y = cold), fill=COLD, alpha=1,stat = "identity") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0),limits=y.lim) +
      labs(x = " ", y = "",title= TITLE[i]) +
      scale_x_continuous(breaks = 1:length(lab_coast), labels=lab_coast,expand = c(0, 0)) +
      #scale_y_continuous() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.1, 0.05, 0.38), "lines")) 
    
    
  }  
  
  # quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/", "Warm-Cold difference" ,NAME,".pdf",sep=""),dpi=600,height=6,width=7,type="pdf")   
  #   Layout= matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=T)
  #   #QQ <- list(p1,p2,q1,q2)
  #   multiplot(plotlist=QQ ,layout= Layout)
  # dev.off()
  # 
  
  pdf(file=paste(base.dir,"/Salmon-Climate/Output plots/", "Warm-Cold difference compare by origin " ,NAME,".pdf",sep=""),height=6,width=4)   
  for(i in 1:length(ORIG)){
    print(QQ[[i]])
    print(QQ.hot[[i]])
    print(QQ.cold[[i]])
  }
  dev.off()
  
  
  ##########################
  ##########################
  ##########################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # 
  # A <- dat.mean %>% melt(,id.vars=c("year","season","origin")) %>% mutate(mean.prop=value) %>% dplyr::select(-value)
  # B <- dat.sd %>% melt(,id.vars=c("year","season","origin")) %>% mutate(sd.prop=value) %>% dplyr::select(-value)
  # C <-  ocean_temp %>% melt(,id.vars=c("year","season")) %>% mutate(ocean.temp=value) %>% dplyr::select(-value)
  # 
  # ocean_temp_dev$year   <- ocean_temp$year
  # ocean_temp_dev$season <- ocean_temp$season
  # D <-  ocean_temp_dev %>% melt(,id.vars=c("year","season")) %>% mutate(ocean.temp.dev=value) %>% dplyr::select(-value)
  # 
  # Out <- merge(A,B)
  # Out <- merge(Out,C)
  # Out <- merge(Out,D)
  # 
  # Out$season <-  factor(Out$season, levels = c("Spr", "Sum","Fal","Win"))
  # Out$origin <-  factor(Out$origin, levels = as.character(spawn_loc_plot$ocean.region))
  # 
  # Out <- Out %>% filter(variable != "PUSO") %>% filter(variable != "SGEO")
  # 
  # p[[1]]  <-   ggplot(data=Out[Out$season=="Spr",]) +
  #       geom_point(aes(y=mean.prop,x=ocean.temp)) +
  #       facet_wrap(~origin) +
  #       ylab("Mean proportion") +
  #       xlab("Ocean temperature (C)") +
  #       ggtitle("Spr")
  # 
  # p[[2]]  <-   ggplot(data=Out[Out$season=="Sum",]) +
  #       geom_point(aes(y=mean.prop,x=ocean.temp)) +
  #       facet_wrap(~origin) +
  #       ylab("Mean proportion") +
  #       xlab("Ocean temperature (C)") +
  #       ggtitle("Sum")
  # 
  # p[[3]]  <-   ggplot(data=Out[Out$season=="Fal",]) +
  #       geom_point(aes(y=mean.prop,x=ocean.temp)) +
  #       facet_wrap(~origin) +
  #       ylab("Mean proportion") +
  #       xlab("Ocean temperature (C)") +
  #       ggtitle("Fal")
  # 
  # p[[4]]  <-   ggplot(data=Out[Out$season=="Win",]) +
  #       geom_point(aes(y=mean.prop,x=ocean.temp)) +
  #       facet_wrap(~origin) +
  #       ylab("Mean proportion") +
  #       xlab("Ocean temperature (C)") +
  #       ggtitle("Win")
  # 
  # 
  # 
  # 
  # q[[1]]  <-   ggplot(data=Out[Out$season=="Sum",]) +
  #       geom_point(aes(y=mean.prop,x=ocean.temp.dev)) +
  #       facet_wrap(~variable) +
  #       ylab("Mean proportion") +
  #       xlab("Ocean temp dev (C)") +
  #       ggtitle("Sum")
  # 
  # 
  
  
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  ## Plot mean occurrence against mean proportion.
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################

#   
#     
#   origin_loc_mean
#   origin_loc_sd
#   
#   origin_loc_mean[1,,]
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   i=3  
#   A  <- origin_mat[i,temperature_season_idx == 2,]
#   OT <- ocean_temp[temperature_season_idx == 2,]
#   
#   plot(A[,2]~OT$SFB)
#   
#   plot(A[,4]~OT$NCA)
#   
# plot(c(A)~c(unlist(OT[,3:19])))
# 
# 
#   
#   
#   
#   
#   
#   
#   
  