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

pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/Effort heatmap.pdf",sep=""),height = 8.5, width=11)

  plot.heatmap( temp=log(t(as.matrix(K_troll_flat))+1e-15,10),"Troll Effort (Boat Days or Landings (AK))")
  plot.heatmap( log(t(as.matrix(K_treaty_flat))+1e-15,10),"Treaty Troll Effort (Deliveries)")
  plot.heatmap( log(t(as.matrix(K_rec_flat + K_rec_PUSO_flat))+1e-15,10),"Rec Effort, US (Angler-Days)")
  plot.heatmap( log(t(as.matrix(K_rec_can_flat))+1e-15,10),"Rec Effort, Canada (Boat Trips)")
  plot.heatmap( log(t(as.matrix(K_rec_can_irec_flat))+1e-15,10),"Rec Effort, iRec, Canada (license holder trips)")
  
  
  if(TRAWL.US == "TRUE"){
    plot.heatmap( log(t(as.matrix(K_hake_ashop_flat))+1e-15,10),"ASHOP Trawl effort (boat days proxy)")
    plot.heatmap( log(t(as.matrix(K_hake_shoreside_flat))+1e-15,10),"Shoreside Trawl effort (boat days proxy)")
  }
  if(TRAWL.AK == "TRUE"){
    plot.heatmap( log(t(as.matrix(K_pollock_shoreside_flat))+1e-15,10),"Pollock Shoreside Trawl effort (boat days proxy)")
    plot.heatmap( log(t(as.matrix(K_rockfish_AK_shoreside_flat))+1e-15,10),"AK Rockfish Trawl effort (boat days proxy)")
  }

dev.off()

# For each Season d

SEAS <- c("summer","fall","winter","spring")

for(i in 1:length(SEAS)){
  THESE <- grep(SEAS[i],rownames(K_troll_flat))
  
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/Effort heatmap ",SEAS[i],".pdf",sep=""),height = 8.5, width=11)
  plot.heatmap( temp=log(t(as.matrix(K_troll_flat[THESE,]))+1e-15,10),paste0(SEAS[i]," Troll Effort (Boat Days)"),all.seas=FALSE)
  plot.heatmap( log(t(as.matrix(K_treaty_flat[THESE,]))+1e-15,10),paste0(SEAS[i]," Treaty Troll Effort (Deliveries)"),all.seas=FALSE)
  plot.heatmap( log(t(as.matrix(K_rec_flat[THESE,] + K_rec_PUSO_flat[THESE,]))+1e-15,10),paste0(SEAS[i]," Rec Effort, US (Angler-Days)"),all.seas=FALSE)
  plot.heatmap( log(t(as.matrix(K_rec_can_flat[THESE,]))+1e-15,10),paste0(SEAS[i]," Rec Effort, Canada (Boat Trips)"),all.seas=FALSE)
  plot.heatmap( log(t(as.matrix(K_rec_can_irec_flat[THESE,]))+1e-15,10),paste0(SEAS[i]," Rec Effort, iRec, Canada (license holder trips)"),all.seas=FALSE)

  if(TRAWL.US == "TRUE"){
    plot.heatmap( log(t(as.matrix(K_hake_ashop_flat[THESE,]))+1e-15,10),paste0(SEAS[i]," ASHOP Trawl effort (boat days proxy)"),all.seas=FALSE)
    plot.heatmap( log(t(as.matrix(K_hake_shoreside_flat[THESE,]))+1e-15,10),paste0(SEAS[i]," Shoreside Trawl effort (boat days proxy)"),all.seas=FALSE)
  }
  if(TRAWL.AK == "TRUE"){
    plot.heatmap( log(t(as.matrix(K_pollock_shoreside_flat[THESE,]))+1e-15,10),paste0(SEAS[i],"Pollock Shoreside Trawl effort (boat days proxy)"),all.seas=FALSE)
    plot.heatmap( log(t(as.matrix(K_rockfish_AK_shoreside_flat[THESE,]))+1e-15,10),paste0(SEAS[i],"AK Rockfish Trawl effort (boat days proxy)"),all.seas=FALSE)
  }
dev.off()
}


######################

plot.heatmap.nolog <- function(temp.all,id){
  Title = paste(REL$ID[id],REL$ocean.region[id],REL$brood_year[id],REL$N.released[id])
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

#####
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/All Troll + Treaty observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
  for(i in 1:N.REL){
    plot.heatmap.nolog( temp.all=C_troll_true +C_treaty_true,id=i)
  }
dev.off()

pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/All Rec observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.heatmap.nolog( temp.all=C_rec_true,id=i)
}
dev.off()

pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/All Trawl ASHOP observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.heatmap.nolog( temp.all=C_hake_ashop_true,id=i)
}
dev.off()

pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/All Trawl Shoreside observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.heatmap.nolog( temp.all=C_hake_shoreside_true,id=i)
}
dev.off()

pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/All Pollock Trawl Shoreside observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.heatmap.nolog( temp.all=C_pollock_GOA_true,id=i)
}
dev.off()

pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/All AK rockfish Trawl Shoreside observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.heatmap.nolog( temp.all=C_rockfish_AK_true,id=i)
}
dev.off()

# Sum across all fishery fleets
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/All FLEETS observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.heatmap.nolog( temp.all= C_troll_true +
                                C_treaty_true +        
                                C_rec_true +
                                C_hake_ashop_true +
                                C_hake_shoreside_true +
                                C_pollock_GOA_true +
                                C_rockfish_AK_true,id=i)
}
dev.off()


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

## Make a few plots of the sampling effort used for each area and time...
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/Sample Fraction heatmap.pdf",sep=""),height = 8.5, width=11)

  A <- Lambda_troll_flat;  A[is.na(A)] <- -99
  B <- Lambda_troll_flat_int;  B[is.na(B)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(A)),"Troll sampling fraction ")
  plot.heatmap.samp.frac( temp=t(as.matrix(B)),"Troll sampling fraction + interpolated ")

  A <- Lambda_rec_flat;  A[is.na(A)] <- -99
  B <- Lambda_rec_flat_int;  B[is.na(B)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(A)),"Rec sampling fraction ")
  plot.heatmap.samp.frac( temp=t(as.matrix(B)),"Rec sampling fraction + interpolated ")
  
  A <- Lambda_treaty_flat;  A[is.na(A)] <- -99
  B <- Lambda_treaty_flat_int;  B[is.na(B)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(A)),"Treaty sampling fraction ")
  plot.heatmap.samp.frac( temp=t(as.matrix(B)),"Treaty sampling fraction + interpolated ")

  
  A <- Lambda_hake_ashop_flat_int;  A[is.na(A)] <- -99
  B <- Lambda_hake_ashop_flat;  B[is.na(B)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(B)),"ASHOP sampling fraction")
  plot.heatmap.samp.frac( temp=t(as.matrix(A)),"ASHOP sampling fraction + interpolated")
  
  A <- Lambda_hake_shoreside_flat_int;  A[is.na(A)] <- -99
  B <- Lambda_hake_shoreside_flat;  B[is.na(B)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(B)),"Shoreside sampling fraction")
  plot.heatmap.samp.frac( temp=t(as.matrix(A)),"Shoreside sampling fraction + interpolated ")
 
  #A <- Lambda_pollock_GOA_flat_ing;  A[is.na(A)] <- -99
  B <- Lambda_pollock_GOA_flat;  B[is.na(B)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(B)),"Pollock GOA Shoreside sampling fraction")
  #plot.heatmap.samp.frac( temp=t(as.matrix(A)),"Pollock GOA Shoreside sampling fraction + interpolated ")
  
  #A <- Lambda_pollock_GOA_flat_ing;  A[is.na(A)] <- -99
  B <- Lambda_rockfish_AK_flat;  B[is.na(B)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(B)),"Rockfish AK Shoreside sampling fraction")
  #plot.heatmap.samp.frac( temp=t(as.matrix(A)),"Pollock GOA Shoreside sampling fraction + interpolated ")
   
  A <- Lambda_net_flat;  A[is.na(A)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(A)),"Net sampling fraction ")

dev.off()

##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
### Calculate CPUE for all 
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
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

#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
  for(i in 1:N.REL){
    plot.CPUE.heatmap( temp.all=C_troll_true,effort.all= K_troll_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
  }
dev.off()

#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Rec observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_rec_true,effort.all= K_rec_flat+K_rec_PUSO_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()

#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Rec Can observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_rec_true,effort.all= K_rec_can_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()

#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Rec Can iREC observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_rec_true,effort.all= K_rec_can_irec_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()

#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Treaty observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_treaty_true,effort.all= K_treaty_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()
#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Trawl ASHOP observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_hake_ashop_true,effort.all= K_hake_ashop_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()
#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Trawl Shoreside observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_hake_shoreside_true,effort.all= K_hake_shoreside_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()
#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Pollock Trawl GOA observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_pollock_GOA_true,effort.all= K_pollock_shoreside_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()
#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Pollock Trawl GOA observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_rockfish_AK_true,effort.all= K_rockfish_AK_shoreside_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()
#############################################################################
#############################################################################
#############################################################################




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

# Plot CPUE for troll fisheries for 3 hatcheries over 2 release years (3 row x 2 column grid)

# id = 235 and 254 is Coleman 1980 and 2005
# id = 109 is Colemen small 1980


# Coleman plots
setwd("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/")
quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Coleman 1980.pdf"),height=4,width=5,dpi=600,type="pdf")
      plot.CPUE.heatmap2( temp.all=C_troll_true,effort.all= K_troll_flat,id=235)
dev.off()

quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Coleman 1999.pdf"),height=4,width=5,dpi=600,type="pdf")
  plot.CPUE.heatmap2( temp.all=C_troll_true,effort.all= K_troll_flat,id=254)
dev.off()

# read in files combine into side by side figures
p1 <- ggdraw() + draw_image(magick::image_read_pdf(paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Coleman 1980.pdf"), 
                                                   density = 600))
p2 <- ggdraw() + draw_image(magick::image_read_pdf(paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Coleman 1999.pdf"), 
                                                   density = 600))

quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Coleman Combined.pdf"),height=4,width=10,dpi=600,type="pdf")
  print(plot_grid(p1,p2))
dev.off()

# LCOL "Big Creek_small" 1980 and 1999 are ID = 34,48
quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Big Creek 1980.pdf"),height=4,width=5,dpi=600,type="pdf")
  plot.CPUE.heatmap2( temp.all=C_troll_true,effort.all= K_troll_flat,id=34)
dev.off()

quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Big Creek 1999.pdf"),height=4,width=5,dpi=600,type="pdf")
  plot.CPUE.heatmap2( temp.all=C_troll_true,effort.all= K_troll_flat,id=48)
dev.off()

# read in files combine into side by side figures
p1 <- ggdraw() + draw_image(magick::image_read_pdf(paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Big Creek 1980.pdf"), 
                                                   density = 600))
p2 <- ggdraw() + draw_image(magick::image_read_pdf(paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Big Creek 1999.pdf"), 
                                                   density = 600))

quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Big Creek Combined.pdf"),height=4,width=10,dpi=600,type="pdf")
  print(plot_grid(p1,p2))
dev.off()

# URB "Priest" 1980 and 1999 are ID = 34,48
quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Priest 1980.pdf"),height=4,width=5,dpi=600,type="pdf")
  plot.CPUE.heatmap2( temp.all=C_troll_true,effort.all= K_troll_flat,id=901)
dev.off()

quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Priest 1999.pdf"),height=4,width=5,dpi=600,type="pdf")
  plot.CPUE.heatmap2( temp.all=C_troll_true,effort.all= K_troll_flat,id=920)
dev.off()

# read in files combine into side by side figures
p1 <- ggdraw() + draw_image(magick::image_read_pdf(paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Priest 1980.pdf"), 
                                                   density = 600))
p2 <- ggdraw() + draw_image(magick::image_read_pdf(paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Priest 1999.pdf"), 
                                                   density = 600))

quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Priest Combined.pdf"),height=4,width=10,dpi=600,type="pdf")
print(plot_grid(p1,p2))
dev.off()
