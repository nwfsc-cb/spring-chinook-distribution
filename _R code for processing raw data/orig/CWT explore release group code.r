rm(list=ls())

#### SALMON CWT RECOVERY CODE - examined by release group/ hatchery.
setwd("/Users/ole.shelton/Documents/Science/Active projects/Killer Whale/OceanHarvestData")
## Read in recovery data sorted and modified by the file CWT explore code.r
dat 		<- read.csv("Ocean_Recover_Chinook_1973-2013_trim-(11-1-2014).csv",header=T)

setwd("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Releases")
## Read in release data
dat.release	<-  read.csv("release data (chin and coho all).csv",header=T)
# trim to include only chinook
dat.release	<-	dat.release[dat.release$species == 1,]

dat.release$cwt_2nd_mark[is.na(dat.release$cwt_2nd_mark)==T]	<- 0
dat.release$tag_loss_rate[is.na(dat.release$tag_loss_rate)==T]	<- 0
dat.release$stock_location_name	<- as.character(dat.release$stock_location_name)
dat.release$stock_location_name[is.na(dat.release$stock_location_name)==T] <- ""

##########################################################################################
## OK, let's look at the recovery data
##########################################################################################

A <- aggregate(dat.release$stock_location_code, by = list(stock.loc=dat.release$stock_location_code,
					stock.name = dat.release$stock_location_name,state=dat.release$release_location_state),length)
write.csv(A,file="all chinook release stock names.csv")
	# Codes for "run" - indicating run type
	# 	1  	Spring
	# 	2  	Summer
	# 	3  	Fall
	# 	4  	Winter
	# 	5  	Hybrid
	# 	6  	Landlocked
	# 	7  	Late fall

B <- aggregate(dat.release[,c('cwt_1st_mark_count','cwt_2nd_mark_count')],
		by = list(stock.loc	   			= dat.release$stock_location_code,
					stock.name 			= dat.release$stock_location_name,
					release.loc.name 	= dat.release$release_location_name,
					state	   			= dat.release$release_location_state,
					brood.year 			= dat.release$brood_year, run=dat.release$run, 
					date.first.release  = dat.release$first_release_date,
					date.last.release   = dat.release$last_release_date,
					mark.code.1 		= dat.release$cwt_1st_mark,
					mark.code.2 		= dat.release$cwt_2nd_mark,
					tag.loss 			= dat.release$tag_loss_rate),sum,na.rm=T)

C <- aggregate(dat.release[,c('non_cwt_1st_mark_count','non_cwt_2nd_mark_count')],
		by = list(stock.loc=dat.release$stock_location_code,
					stock.name = dat.release$stock_location_name,
					release.loc.name = dat.release$release_location_name,
					state=dat.release$release_location_state,
					brood.year = dat.release$brood_year, run=dat.release$run, 
					date.first.release = dat.release$first_release_date,
					date.last.release = dat.release$last_release_date,
					unmark.code.1 = dat.release$non_cwt_1st_mark,
					unmark.code.2 = dat.release$non_cwt_2nd_mark,
					tag.loss = dat.release$tag_loss_rate),sum,na.rm=T)

D <- merge(B,C,all=T)


B$tot.cwt.tag[substr(B$mark.code.1,1,1)==5]	<-	B$cwt_1st_mark_count[substr(B$mark.code.1,1,1)==5]
B$tot.cwt.tag[substr(B$mark.code.1,1,1)==5 & substr(B$mark.code.2,1,1)==5]	<-	B$cwt_1st_mark_count[substr(B$mark.code.1,1,1)==5 & substr(B$mark.code.2,1,1)==5] +
																				 B$cwt_2nd_mark_count[substr(B$mark.code.1,1,1)==5 & substr(B$mark.code.2,1,1)==5]
B$cwt.tag.fin	<- round(B$tot.cwt.tag*(1-B$tag.loss),0)

##########################################################################################
# Aggregate the various release dates by year, run, and brood year
##########################################################################################

B.out	<-	aggregate(B[,'cwt.tag.fin'],by=list(
			stock.loc 		= B$stock.loc,
			stock.name 		= B$stock.name,
			release.loc.name= B$release.loc.name, 
			state 			= B$state ,
			brood.year  	= B$brood.year, 
			run				= B$run),sum) 

colnames(B.out)[7]	<- "cwt.tag.fin"

B.out	<-	B.out[order(B.out$state,B.out$stock.name,B.out$release.loc.name,B.out$brood.year),]

dat.rel.fin	<-	B.out
##########################################################################################

##########################################################################################

##########################################################################################

E 		<- data.frame(aggregate(dat$stock_location_name,
				by = list(stock.loc	= dat$stock_location_code,
					stock.name 		= dat$stock_location_name,
					rec.area.code	= dat$rec.area.code,
					brood.year 		= dat$brood_year,
					run				= dat$run,
					rec.year = dat$rec.year), length))
colnames(E)[7]	<-	n.recover

F	<-	merge(dat.rel.fin,E,by=c("stock.loc","stock.name","brood.year","run"),all=T)






























##########################################################################################
### OK.  Let's start making some summary tables (make csv turn ocnvert into excel files externally)(
##########################################################################################

setwd("/Users/ole.shelton/Documents/Science/Active projects/Killer Whale/summarize ocean recovery data")
# First observations by catch type and year (across all release rivers)
A 	<-	aggregate(dat.trim$fishery,by=list(year = dat.trim$rec.year,fish.type = dat.trim$fish.type),length)
A	<-	 data.frame(A)

setwd("/Users/ole.shelton/Documents/Science/Active projects/Killer Whale/summarize ocean recovery data")
write.csv(A,file="recoveries by year and gear type.csv",row.names=F)

# Next, observations by location (across all release rivers)
A 	<-	aggregate(dat.trim$fishery,by=list(location = dat.trim$rec.area.code),length)
A	<-	 data.frame(A)
write.csv(A,file="recoveries by location.csv",row.names=F)


# Next, observations by year, and location (across all release rivers)
A 	<-	aggregate(dat.trim$fishery,by=list(year = dat.trim$rec.year,location = dat.trim$rec.area.code),length)
A	<-	 data.frame(A)

B 	<- merge(A, data.frame(expand.grid(year=1973:2012,location=unique(dat.trim$rec.area.code))),all=T)
B	<- B[order(B$location,B$year),]
write.csv(B,file="recoveries by year and location.csv",row.names=F)

#########################################################################################

# OK... looking at the data I think we can safely get rid of most of the categories without losing too much data
 # This gets rid of the multiple spatial category recoveries
 # Exceptions for:
 	# MEN SFB MOB
 	# QCI NBC -  5025
 	# NCA MEN - 22325
 	# NSE SSE - 10109
 	# SFB MOB - 28610
 	# SOR NCA -  7436

AREAS	<- c("CBC","CISS","COL","COR","MEN","MEN SFB MOB","MOB","NBC","NCA","NCA MEN","NOR","NSE","NSE SSE","NWVI",
			"PUSO","PWS","QCI","QCI NBC ","SFB","SFB MOB","SGEO","SOR","SOR NCA","SSE","SWVI","WAC","YAK")            

N.to.S	<- c(8,1,14,16,19,19.21,21,7,18,18.19,15,4,4.5,9,
			12,2,6,6.7,20,20.21,11,17,17.18,5,10,13,3)            

LOC	<-  data.frame(N.to.S=N.to.S,areas = AREAS)
LOC	<-	LOC[order(LOC$N.to.S),]
LOC$plot.numb	<-	1:nrow(LOC)
LOC$plot.name	<- LOC$plot.numb
LOC$plot.name[1:9]	<- paste("0",LOC$plot.numb[1:9],sep="")

dat.t2					<-	dat.trim[which(is.na(match(dat.trim$rec.area.code,LOC$areas))==F),]
dat.t2$loc.name			<-	LOC$plot.name[match(dat.t2$rec.area.code,LOC$areas)]

dat.t2$rec.area.code	<-	as.character(dat.t2$rec.area.code)
dat.t2$rec.area.code	<-	as.factor(dat.t2$rec.area.code)
dat.t2$rec.area.lab		<- paste(dat.t2$loc.name,".",dat.t2$rec.area.code,sep="")


# That gets rid of about 6000 observations 
# > dim(dat.trim)
# [1] 829752     28
# > dim(dat.t2)
# [1] 823726     28
#########################################################################################

# Next, observations by year, and location (across all release rivers)
A <- data.frame(tapply(dat.t2$rec.area.code,dat.t2[,c('rec.year','rec.area.lab')],length))
write.csv(A,file="recoveries by location and year.csv",row.names=F)

# Next, observations by month and location (across all release rivers)
A <- data.frame(tapply(dat.t2$rec.area.code,dat.t2[,c('rec.area.lab','rec.month')],length))
write.csv(A,file="recoveries by location and month.csv",row.names=F)

YEARS	<- data.frame(year=as.numeric(as.character(unique(dat.chin$rec.year))))
MONTH	<-	c("01","02"
ALL		<- NULL
for(i in 1:nrow(LOC)){
	temp = dat.t2[dat.t2$rec.area.code == LOC$areas[i],]
	A 	<- data.frame(tapply(temp$rec.area.code,temp[,c('rec.year','rec.month')],length))
	B	<- data.frame(year= as.numeric(as.character(rownames(A))),A)
	C	<-	data.frame(merge(B,YEARS,all=T))
	C	<-	C[order(C$year),]
# 	D	<-
# 	ALL	<-	rbind(ALL,data.frame(Area = LOC$areas[i],C))

	colnames(C)[2:ncol(C)]	<-	paste(LOC$areas[i],colnames(C)[2:ncol(C)],sep=".")
	write.csv(C,file=paste(LOC$plot.name[i],LOC$areas[i],"recoveries by location and month.csv",sep="."),row.names=F)
}

#### ALL AREAS IN ONE 

A 	<-	aggregate(dat.t2$rec.area.code,by=list(year = dat.t2$rec.year,location = dat.t2$rec.area.code,month=dat.t2$rec.month),length)
A$month	<-	as.numeric(as.character(A$month))
B	<-	 data.frame(expand.grid(location = as.character(LOC$areas),year = as.numeric(as.character(unique(dat.chin$rec.year))),month=1:12))

C	<- merge(A,B,all=T)

ALL	<- NULL
for( i in 1:nrow(LOC)){
	temp.1	<-	C[C$location == LOC$areas[i],]
	
	NEW <- NULL
	for(j in 1:12){
		temp.2	<-	temp.1[temp.1$month == j,]
		NEW		<- cbind(NEW,temp.2$x)
	}
	colnames(NEW)	<- paste("X",sort(unique(dat.t2$rec.month)),sep="")
	location.lab	<- rep(paste(LOC$plot.name[i],".",LOC$areas[i],sep=""),nrow(temp.2))
	NEW				<-	data.frame(temp.2[,1:2],location.lab=location.lab,NEW)

	ALL	<- rbind(ALL,NEW)
	
}

 write.csv(ALL,file=paste("All recoveries by location and month.csv",sep="."),row.names=F)


##### MAKE a grid for each location-month combination
DAT	<-	read.csv("All recoveries by location and month.csv",header=T)
library(fields)

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

























