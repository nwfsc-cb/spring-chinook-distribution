### PLOT Effort heatmaps
# you need to run Prep raw data for CPUE analysis.R before running this file.


library(fields)
library(RColorBrewer)
library(viridis)
library(cowplot)
library(pdftools)

plot.heatmap <- function(temp,Title,all.seas=TRUE){

temp <- temp[,1:(ncol(temp))]
  
z.lim	<- c(-0.01,max(temp))
if(max(temp) < -0.01){z.lim	<- c(-0.01,0)}
lab.new <- LOCATIONS[order(LOCATIONS$location.number),]
x.lab	=	lab.new$location.name
col.br<- colorRampPalette(c("blue", "cyan", "yellow", "red"))
par(mfrow=c(1,1),oma=c( 0,0,0,4) )
image(z=t(temp),x=1:nrow(t(temp)),y=lab.new$location.number,axes=F,ylab="",xlab="",
      col=1,zlim=c(-9999,0))
image(z=t(temp),x=1:nrow(t(temp)),y=lab.new$location.number,axes=F,ylab="",xlab="",
      col=col.br(32),zlim=z.lim,add=T)
box(bty="o",lwd=2)
axis(2,las=2,at=1:nrow(temp),labels=x.lab)
if(all.seas==TRUE){axis(1,las=2,at=seq(2,ncol(temp),by=4),labels=YEARS.RECOVER[1:(length(YEARS.RECOVER)-1)])}
if(all.seas==FALSE){axis(1,las=2,at=seq(1,length(YEARS.RECOVER),by=2),labels=YEARS.RECOVER[seq(1,length(YEARS.RECOVER),by=2)])}
title(main=Title)
par(mfrow=c(1,1),oma=c( 0,0,0,0) )
ticks <- seq(min(z.lim),max(z.lim),length.out=6)

image.plot(temp,legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list( at= ticks    , labels=round(10^(ticks),0)))

}
#############

######################

plot.heatmap.nolog <- function(temp.all,id){
  Title = paste(REL$ID[id],REL$ocean.region[id],REL$brood_year[id],"y=",REL$n.year[id],";",REL$N.released[id])
  temp <- t(as.matrix(temp.all[id,,]))
  z.lim	<- c(0.01,min(max(max(temp),1),99999))
  lab.new <- LOCATIONS[order(LOCATIONS$location.number),]
  x.lab	=	lab.new$location.name
  col.br<- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  par(mfrow=c(1,1),oma=c( 0,0,0,4) )
  image(z=t(temp),x=1:nrow(t(temp)),y=lab.new$location.number,axes=F,ylab="",xlab="",
        col=grey(0.5),zlim=c(-9999,z.lim[1]))
  image(z=t(temp),x=1:nrow(t(temp)),y=lab.new$location.number,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim,add=T)
  box(bty="o",lwd=2)
  axis(2,las=2,at=1:nrow(temp),labels=x.lab)
  axis(1,las=2,at=seq(2,ncol(temp),by=4),labels=REL$brood_year[id]+2:6)
  title(main=Title)
  par(mfrow=c(1,1),oma=c( 0,0,0,0) )
  ticks <- round(seq(min(z.lim),max(z.lim),length.out=6),1)
  
  image.plot(temp,legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list( at= ticks    , labels=ticks))
}

##############################################################################################
##############################################################################################
##############################################################################################


plot.heatmap.samp.frac <- function(temp,Title){
  
  temp <- temp[,1:(ncol(temp))]
  
  z.lim	<- c(1e-6,1)
  lab.new <- LOCATIONS[order(LOCATIONS$location.number),]
  x.lab	=	lab.new$location.name
  col.br<- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  par(mfrow=c(1,1),oma=c( 0,0,0,4) )
  image(z=t(temp),x=1:nrow(t(temp)),y=lab.new$location.number,axes=F,ylab="",xlab="",
        col=1,zlim=c(-9999,0))
  image(z=t(temp),x=1:nrow(t(temp)),y=lab.new$location.number,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim,add=T)
  box(bty="o",lwd=2)
  axis(2,las=2,at=1:nrow(temp),labels=x.lab)
  axis(1,las=2,at=seq(2,ncol(temp),by=4),labels=YEARS.RECOVER[1:(length(YEARS.RECOVER)-1)])
  title(main=Title)
  par(mfrow=c(1,1),oma=c( 0,0,0,0) )
  ticks <- seq(min(z.lim),max(z.lim),length.out=6)
  
  image.plot(temp,legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list( at= ticks, labels=round(ticks,2),0))
  
}
#############

##############################################################################################
##############################################################################################
### Calculate CPUE for all 
##############################################################################################
##############################################################################################





### THIS IS WHERE WE REVISIT THE TIMING OF EVERYTHING.












plot.CPUE.heatmap <- function(temp.all,effort.all,id,MONTH.STRUCTURE){
  Title = paste(REL$ID[id],REL$ocean.region[id],REL$brood_year[id],REL$N.released[id])
  temp <- t(as.matrix(temp.all[id,,]))
    
  if(REL$brood_year[id]+2 >= 1979){
    if(MONTH.STRUCTURE=="FOUR"|MONTH.STRUCTURE =="SPRING"){
        e.start <- grep(paste(REL$brood_year[i]+2,".month.spring",sep=""),rownames(effort.all))
    }
    if(MONTH.STRUCTURE=="FRAM"){e.start <- grep(paste(REL$brood_year[i]+2,".month.summer",sep=""),rownames(effort.all))}
    e.stop  <- e.start + N.mod.month -1
    effort <- t(as.matrix(effort.all[e.start:e.stop,]))
  }else{  
    e.start <- 1
    n.missing <- - N_month * (REL$brood_year[i]+2 - YEARS.RECOVER[1])
    e.stop    <- e.start + N.mod.month - n.missing -1
    effort <- t(as.matrix(rbind(matrix(0,n.missing,N.LOC),effort.all[e.start:e.stop,])))
  }    

  cpue  <- temp / effort
  cpue[is.nan(cpue)]      <- -99
  cpue[is.infinite(cpue)] <-  99

  z.lim	<- c(1e-6,max(max(cpue[cpue<99])))
  if(z.lim[2]==0 | z.lim[2]== -99){ z.lim[2]=2e-6}
  
  lab.new <- LOCATIONS[order(LOCATIONS$location.number),]
  x.lab	=	lab.new$location.name
  col.br<- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  par(mfrow=c(1,1),oma=c( 0,0,0,4) )
  image(z=t(cpue),x=1:nrow(t(temp)),y=LOCATIONS$location.number,axes=F,ylab="",xlab="",
        col=grey(0.5),zlim=c(-100,-98))
  image(z=t(cpue),x=1:nrow(t(temp)),y=LOCATIONS$location.number,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim,add=T)
  image(z=t(cpue),x=1:nrow(t(temp)),y=LOCATIONS$location.number,axes=F,ylab="",xlab="",
        col="black",zlim=c(-1e-10,1e-10),add=T)
  box(bty="o",lwd=2)
  axis(2,las=2,at=1:nrow(temp),labels=x.lab)
  axis(1,las=2,at=seq(2,ncol(temp),by=4),labels=REL$brood_year[id]+2:6)
  title(main=Title)
  par(mfrow=c(1,1),oma=c( 0,0,0,0) )
  ticks <- round(seq(min(z.lim),max(z.lim),length.out=6),5)
  
  image.plot(temp,legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list( at= ticks    , labels=ticks))
}




#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
### Pub Plot
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

library(fields)
library(RColorBrewer)

plot.CPUE.heatmap2 <- function(temp.all,effort.all,id){
  Title = paste(REL$ID[id],REL$ocean.region[id],REL$brood_year[id],REL$N.released[id])
  temp <- t(as.matrix(temp.all[id,,]))
  e.start <- grep(paste(REL$release_year[id]+1,".month.spring",sep=""),rownames(effort.all))
  e.stop  <- e.start + N.mod.month -1

  if(REL$brood_year[id]== min(YEARS.BROOD)){
    e.start <- min(grep(REL$brood_year[id]+2,rownames(K_troll_flat)))
    e.stop  <- e.start + N.mod.month -1
  }
  effort <- t(as.matrix(effort.all[e.start:e.stop,]))

  cpue  <- temp / effort
  cpue[is.nan(cpue)]      <- -99
  cpue[is.infinite(cpue)] <-  99

  z.lim	<- c(1e-5,max(max(cpue[cpue<99])))
  if(z.lim[2]==0){ z.lim[2]=2e-6}
  lab.new <- LOCATIONS.plot[order(LOCATIONS.plot$location.number),]
  x.lab	=	lab.new$location.name
  
  col.br<- colorRampPalette(c("blue", "cyan", "yellow", "red"))


 # col.br<- viridis(32, alpha = 1, begin = 0, end = 1, option = "D")

  par(mfrow=c(1,1),oma=c( 0,0,0,3),mar=c(4,4,0.5,2) )
  image(z=t(cpue),x=1:nrow(t(temp)),y=lab.new$location.number,axes=F,ylab="",xlab="",
        col=grey(0.5),zlim=c(-100,-98))
  image(z=t(cpue),x=1:nrow(t(temp)),y=lab.new$location.number,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim,add=T)
  image(z=t(cpue),x=1:nrow(t(temp)),y=lab.new$location.number,axes=F,ylab="",xlab="",
        col="black",zlim=c(-1e-10,1e-10),add=T)
  box(bty="o",lwd=2)
  axis(1,las=2,at= round(seq(2,ncol(temp),by=4),3),labels=REL$brood_year[id]+2:6,tcl=0,hadj=1.5,cex.axis=0.9)
  LAB <- rep(c("Spr","Sum","Fall","Wint"),N_years_recover) [1:ncol(temp)]
  axis(1,las=2,at=1:ncol(temp),labels= LAB ,tcl=0,cex.axis=0.7,hadj=0.4)
  axis(2,las=2,at=1:nrow(temp),labels=x.lab,cex.axis=0.9)
  #title(main=Title)
  par(mfrow=c(1,1), oma=c( 0,1,0,0))
  op <- par(cex = 0.75)
  ticks <- c(min(z.lim),round(seq(min(z.lim),max(z.lim),length.out=6),4)[2:6])
  image.plot(temp,legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at=ticks,labels=ticks),
             #legend.args=list(text="",cex=0.8),
             add=T,legend.shrink=0.8)
}
#############
