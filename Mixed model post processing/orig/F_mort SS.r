### THIS IS FOR EXAMINING ESTIMATED FISHING MORTALITY ALONG THE WHOLE COAST

### PLOT Effort heatmaps
plot.heatmap <- function(temp,Title){
  
  z.lim	<- c(1e-5,max(temp))
  x.lab	=	LOCATIONS$location.name
  col.br<- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  par(mfrow=c(1,1),oma=c( 0,0,0,4) )
  image(z=t(temp),x=1:nrow(t(temp)),y=LOCATIONS$location.number,axes=F,ylab="",xlab="",
        col=1,zlim=c(-9999,z.lim[1]))
  image(z=t(temp),x=1:nrow(t(temp)),y=LOCATIONS$location.number,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim,add=T)
  box(bty="o",lwd=2)
  axis(2,las=2,at=1:nrow(temp),labels=x.lab)
  axis(1,las=2,at=seq(2,ncol(temp),by=4),labels=years.recover,tcl=0,hadj=1.5)
  LAB <- rep(c("Spr","Sum","Fall","Wint"),N_years_recover) [1:ncol(temp)]
  axis(1,las=2,at=1:ncol(temp),labels= LAB ,tcl=0,cex.axis=0.5,hadj=0.4)
  title(main=Title)
  par(mfrow=c(1,1),oma=c( 0,0,0,0) )
  ticks <- seq(min(z.lim),max(z.lim),length.out=6)
  
  image.plot(temp,legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list( at= ticks,labels=c("1e-5",round(ticks[2:6],2))))
  
}
#############
quartz(file=paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/F_mortality heatmap ",NAME,".pdf",sep=""),height = 8.5, width=8.5,type="pdf",dpi=300)
  plot.heatmap( temp=t(as.matrix(F_troll_plus_vuln)),"Estimated Troll Mortality")
  plot.heatmap( temp=t(as.matrix(F_rec_plus_vuln)),"Estimated Recreational Mortality")
  plot.heatmap( temp=t(as.matrix(F_rec_plus_vuln+F_troll_plus_vuln)),"Estimated Total Mortality")
dev.off()


