### process the distribution estimates from the hurdle model.

library(fields)
library(RColorBrewer)

gini.simp.div <- function(prop){
  return(1 - sum(prop^2))
}


if(SEASON == FALSE){
 MEAN  <- apply(samp$origin_loc,c(2,3),mean)
 SD    <- apply(samp$origin_loc,c(2,3),sd)
 MEDIAN  <- apply(samp$origin_loc,c(2,3),median)
 QUANT <- apply(samp$origin_loc,c(2,3),quantile,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975))
}
if(SEASON == TRUE){
  MEAN  <- apply(samp$origin_loc,c(2,3,4),mean)
  SD    <- apply(samp$origin_loc,c(2,3,4),sd)
  MEDIAN  <- apply(samp$origin_loc,c(2,3,4),median)
  QUANT <- apply(samp$origin_loc,c(2,3,4),quantile,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975))
}

 # mean.long <- melt(MEAN) 
 #  colnames(mean.long)[1] <- "origin"
 #  colnames(mean.long)[2] <- "ocean.loc"
 
##################### 
  
  plot.prop.heatmap <- function(temp,title){
    
    par(mar=c(5,5,1,1))
    z.lim	<- c(0.0,0.60)
    y.lab <- spawn_loc$ocean.region
    x.lab	<-	LOCATIONS$location.name
    #col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
    col.br <- colorRampPalette(c(grey(0.9), "red"))
    par(mfrow=c(1,1),oma=c( 0,0,0,4) )
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
  
if(SEASON == FALSE){  
  pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/"," Distribution heatmap ",NAME,".pdf",sep=""),height = 6, width=8.5)
  
    plot.prop.heatmap( temp=MEAN,title= "Mean estimated ocean distribution")
    plot.prop.heatmap( temp=MEDIAN,title= "Median estimated ocean distribution")
    
  dev.off()
}
if(SEASON == TRUE){  
    pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/"," Distribution heatmap ",NAME,".pdf",sep=""),height = 6, width=8.5)
    
    plot.prop.heatmap( temp=MEAN[1,,],title= "Winter-Spring estimated ocean distribution (MEAN)")
    plot.prop.heatmap( temp=MEAN[2,,],title= "Summer estimated ocean distribution(MEAN)")
    plot.prop.heatmap( temp=MEAN[3,,],title= "Fall estimated ocean distribution(MEAN)")
    
    plot.prop.heatmap( temp=MEDIAN[1,,],title= "Winter-Spring estimated ocean distribution(MEDIAN)")
    plot.prop.heatmap( temp=MEDIAN[2,,],title= "Summer estimated ocean distribution(MEDIAN)")
    plot.prop.heatmap( temp=MEDIAN[3,,],title= "Fall estimated ocean distribution(MEDIAN)")
    
    dev.off()
}  

###################################################################################################  
####### 2x2 Panel plot for Publication
###################################################################################################
  
quartz(file=paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/Spatial Distribution Pub plot.jpeg",sep=""),type="jpeg",dpi=600,width=7,height=6)
  
par(mfrow=c(2,2))
TEXT <- c("a) Spring","b) Summer", "c) Fall")

for(i in 1:3){  
  if(i==1| i==3){ par(mar=c(3.5,3.5,1,1)) }
  if(i==2) {par(mar=c(3.5,2.75,1,2.75))}
  temp <- MEDIAN[i,,] 
  z.lim	<- c(0.0,0.60)
  y.lab <- spawn_loc$ocean.region
  x.lab	<-	LOCATIONS$location.name
  #col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  col.br <- colorRampPalette(c(grey(0.9), "red"))
  #par(oma=c( 0,0,0,4) )
  image(z=t(temp),y=1:nrow(temp),x=LOCATIONS$location.number,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim)
  box(bty="o",lwd=2)
  axis(1,las=2,at=LOCATIONS$location.number,labels=x.lab,hadj=0.7,cex.axis=0.8,tcl=-0.2)
  axis(2,las=2,at=spawn_loc$init.loc,labels=y.lab,hadj=0.65,tcl=-0.2,cex.axis=0.8)
  if(i==1| i==3){title(ylab="Origin",line=2.5,cex.lab=1)}
  title(xlab="Ocean region",line=2.5,cex.lab=1)
  #title(main=title)
  #par(mfrow=c(1,1),oma=c( 0,0,0,0) )
  mtext(TEXT[i],side=3,adj=0,cex=0.8)
  if(i ==2){
    ticks <- seq(min(z.lim),max(z.lim),length.out=6)
    image.plot(temp,legend.only=T,smallplot=c(0.89,0.92, 0.25,0.9),col=col.br(32),zlim=z.lim,axis.args=list(at= ticks,cex.axis=0.7,hadj=0.5,tcl=-0.3),add=T,legend.cex=0.75)
  }
}
  
  SEASON.lab <- c("spring","summer","fall")  
  temp <- expand.grid(origin=spawn_loc$ocean.region,season=SEASON.lab)
  temp$plot.numb[temp$season=="spring"] <- 1
  temp$plot.numb[temp$season=="summer"] <- 2
  temp$plot.numb[temp$season=="fall"]   <- 3
  
  # For Fourth panel calculate the fraction within +/- 1 area of the origin region.
  ### PLOT OCEAN DIVERSITY BY ORIGIN.
  for(i in 1:max(spawn_loc$init.loc)){
    for(j in 1:dim(MEAN)[1]){
      temp$prop.near.origin[temp$season==SEASON.lab[j] & temp$origin == spawn_loc$ocean.region[i]] <- sum(MEAN[j,spawn_loc$init.loc[i],(spawn_loc$number[i]-1):(spawn_loc$number[i]+1)])
      temp$gini.simp[temp$season==SEASON.lab[j] & temp$origin == spawn_loc$ocean.region[i]] <- gini.simp.div(MEAN[j,spawn_loc$init.loc[i],])
    }
  }  
  
  M <- aggregate(temp$gini.simp,by=list(season=temp$season),median)
  M$MEDIAN <- M$x 
  S <- aggregate(temp$gini.simp,by=list(season=temp$season),sd)
  S$SE <- S$x/sqrt(max(spawn_loc$init.loc))
  S$season.numb <- 1:nrow(S)
  plot.gini <-    merge(M[,c("season","MEDIAN")],S[,c("season","SE","season.numb")])
  
  plot.gini <- plot.gini[order(plot.gini$season.numb),]
  
  TEXT <- c("Spring","Summer", "Fall")
  par(mar=c(3.5,2.75,1,2.75))
  y.lim <- c(0.75,0.9)  
  x.lim <- c(0.8,3.2)
  plot(MEDIAN~season.numb,data=plot.gini,ylim=y.lim,xlim=x.lim,axes=F,xlab="",ylab="")
  arrows(x0=plot.gini$season.numb,x1=plot.gini$season.numb,
         y0=plot.gini$MEDIAN + plot.gini$SE,y1=plot.gini$MEDIAN - plot.gini$SE ,length=0,lwd=1.5)
  par(new=T)
  plot(MEDIAN~season.numb,data=plot.gini,ylim=y.lim,xlim=x.lim,axes=F,xlab="",ylab="",bg="white",pch=21,cex=1.2,lwd=1.5,type="b")
  axis(1,at = 1:nrow(plot.gini),labels = TEXT,padj=-0.8,tcl=-0.2)
  axis(2,las=2,hadj=0.5,tcl=-0.3,cex.axis=0.8,tcl=-0.2)
  box(bty="o",lwd=2)
  title(ylab="Gini-Simpson spatial diversity",line=1.75,cex.lab=1)
  mtext("d)",side=3,adj=0,cex=0.8)
  
dev.off()  
  
  
  
  
  
  
  
  
  ######### Extra junk
  # 
  # aggregate(temp$prop.near.origin,by=list(season=temp$season),mean)
  # aggregate(temp$prop.near.origin,by=list(season=temp$season),sd)
  # 
  # y.lim=c(0.1,0.8)  
  # par(new=F)
  # for(i in 1:max(spawn_loc$init.loc)){
  #   plot(prop.near.origin~plot.numb,data=temp[temp$origin==spawn_loc$ocean.region[i],],type="b",ylim=y.lim)
  #   par(new=T)
  # }
  # boxplot(prop.near.origin~plot.numb,data=temp)
  # 
  # 
  # 
  # y.lim=c(0.6,1)  
  # par(new=F)
  # for(i in 1:max(spawn_loc$init.loc)){
  #   plot(gini.simp~plot.numb,data=temp[temp$origin==spawn_loc$ocean.region[i],],type="b",ylim=y.lim)
  #   par(new=T)
  # }
  # 
  #  
  # 
  # 