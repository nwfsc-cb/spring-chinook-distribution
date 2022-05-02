### Plotting file for marine and freshwater recoveries.

# this called by "CWT recoveries analysis.r"

unique(marine.by.hatchery$re)


pdf("Plot recoveries by location and month.pdf",onefile=T)
for ( i in 1:nrow(LOC)){
	
	i=1
	temp<-marine.by.hatchery[marine.by.hatchery$rec.month ==i,]
	
	aggregate(temp
	
	
	
	
	
	
	
	ticks	<-	c(1,10,100,1000,4000)

	temp	<-	DAT[DAT$location == LOC$areas[i],4:ncol(DAT)]
	temp[is.na(temp) == T]	<- 0
	y.lab	=	c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	z.lim	<-	c(-0.01,log(max(ticks),10))
	col.br<- colorRampPalette(c("blue", "cyan", "yellow", "red"))

	par(mfrow=c(1,1),oma=c( 0,0,0,4) )

	image(z=as.matrix(temp),y=1:12,x=sort(YEARS$year),xlab="",ylab="",axes=F,
			col=grey(0.8),zlim=c(0,0))
	image(z=log(as.matrix(temp),10),y=1:12,x=sort(YEARS$year),xlab="",ylab="",axes=F,
			col=col.br(32),zlim=z.lim,add=T)
	axis(1,las=2,at=seq(1974,2012,by=2))
	axis(2,las=2,at=1:12,labels=y.lab)
	box(bty="o",lwd=2)
	par(mfrow=c(1,1),oma=c( 0,0,0,0) )
	image.plot(log(as.matrix(temp),10),legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list( at=log(ticks,10), labels=ticks))
	title(paste(LOC$areas[i],"sample sizes"))
}
dev.off()




pdf("Plot recoveries by location and month.pdf",onefile=T)
for ( i in 1:nrow(LOC)){
	ticks	<-	c(1,10,100,1000,4000)

	temp	<-	DAT[DAT$location == LOC$areas[i],4:ncol(DAT)]
	temp[is.na(temp) == T]	<- 0
	y.lab	=	c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	z.lim	<-	c(-0.01,log(max(ticks),10))
	col.br<- colorRampPalette(c("blue", "cyan", "yellow", "red"))

	par(mfrow=c(1,1),oma=c( 0,0,0,4) )

	image(z=as.matrix(temp),y=1:12,x=sort(YEARS$year),xlab="",ylab="",axes=F,
			col=grey(0.8),zlim=c(0,0))
	image(z=log(as.matrix(temp),10),y=1:12,x=sort(YEARS$year),xlab="",ylab="",axes=F,
			col=col.br(32),zlim=z.lim,add=T)
	axis(1,las=2,at=seq(1974,2012,by=2))
	axis(2,las=2,at=1:12,labels=y.lab)
	box(bty="o",lwd=2)
	par(mfrow=c(1,1),oma=c( 0,0,0,0) )
	image.plot(log(as.matrix(temp),10),legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list( at=log(ticks,10), labels=ticks))
	title(paste(LOC$areas[i],"sample sizes"))
}
dev.off()








