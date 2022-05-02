### PLOT Effort heatmaps
# you need to run Prep raw data for CPUE analysis.R before running this file.


library(fields)
library(RColorBrewer)
library(viridis)



# K_troll_flat  is an 139 rows by 18 columns
# LOCATIONS$location_number is 1:18
# YEARS.RECOVER = 1978:2012
# LOCATIONS$location.name is a character vector of length 18

plot.heatmap <- function(temp,Title){

temp <- temp[,1:(ncol(temp))]
  
z.lim	<- c(-0.01,max(temp))
x.lab	=	LOCATIONS$location.name
col.br<- colorRampPalette(c("blue", "cyan", "yellow", "red"))
par(mfrow=c(1,1),oma=c( 0,0,0,4) )
image(z=t(temp),x=1:nrow(t(temp)),y=LOCATIONS$location.number,axes=F,ylab="",xlab="",
      col=1,zlim=c(-9999,0))
image(z=t(temp),x=1:nrow(t(temp)),y=LOCATIONS$location.number,axes=F,ylab="",xlab="",
      col=col.br(32),zlim=z.lim,add=T)
box(bty="o",lwd=2)
axis(2,las=2,at=1:nrow(temp),labels=x.lab)
axis(1,las=2,at=seq(2,ncol(temp),by=4),labels=YEARS.RECOVER)
title(main=Title)
par(mfrow=c(1,1),oma=c( 0,0,0,0) )
ticks <- seq(min(z.lim),max(z.lim),length.out=6)

image.plot(temp,legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list( at= ticks    , labels=round(10^(ticks),0)))

}
#############

pdf(paste(base.dir,"/Salmon-Climate/Heatmaps/Effort heatmap.pdf",sep=""),height = 8.5, width=11)

  plot.heatmap( temp=log(t(as.matrix(K_troll_flat))+1e-15,10),"Troll Effort (Boat Days)")
  plot.heatmap( log(t(as.matrix(K_treaty_flat))+1e-15,10),"Treaty Troll Effort (Deliveries)")
  plot.heatmap( log(t(as.matrix(K_rec_flat + K_rec_PUSO_flat))+1e-15,10),"Rec Effort, US (Angler-Days)")
  plot.heatmap( log(t(as.matrix(K_rec_can_flat))+1e-15,10),"Rec Effort, Canada (Boat Trips)")

dev.off()
######################

