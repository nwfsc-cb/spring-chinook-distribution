#### SALMON CWT RECOVERY CODE


setwd("/Users/ole.shelton/Documents/Science/Active projects/Killer Whale/data/OceanHarvestData")

dat.chin 	<- read.csv("Ocean_Recover_Chinook_1973-2013.csv",header=T)
# dat.loc	 	<- read.csv("locations_all.csv",header=T)
dat.loc.key	<- read.csv("recovery codes-wietkamp+shelton.csv",header=T)


# Make columns for recovery year, month, day
dat.chin$rec.year	<-	substr(dat.chin$recovery_date,1,4)
dat.chin$rec.month	<-	substr(dat.chin$recovery_date,5,6)
dat.chin$rec.day	<-	substr(dat.chin$recovery_date,7,8)

# cull recoveries in which the month has not been recorded (lose 524 observations)
dat.trim 	<- dat.chin

# Remove freshwater recoveries
dat.trim 	<- dat.trim[substr(dat.trim$recovery_location_code,2,2)!="F",]
dat.trim	<- dat.trim[dat.trim$rec.month!="",]

### Subset the data such that we only look at fisheries 
dim(dat.trim[dat.trim$fishery >=70,])

A	<-	aggregate(dat.trim$fishery,by=list(fishery = dat.trim$fishery),length)

#    fishery Number of obs
# 1       10 	560151	# THESE (#10-18) are Commerical troll
# 2       11 	 28963
# 3       12 	  2673
# 4       15 	 25813
# 5       16 	  1560
# 6       17 	  1228
# 7       18 	   772
# 8       40 	173770	# THESE (#40-42) are Sport 
# 9       41 	 18539
# 10      42 	 13921
# 11      70 	  1023  # THESE (#70+) are Juvenile Sampling 
# 12      72 	  1431
# 13      74 	    97

# I choose to exclude juvenile chinook sampling.  They are rare and much smaller fish
# I also exclude all fish < 15 inches (381 mm) because these are not reproductive fish.

# dat.trim	<- dat.trim[dat.trim$fishery < 70 & dat.trim$length > 381,]
dim(dat.chin)
# [1] 830465     23
dim(dat.trim)
# [1] 825094     26

# So that loses about 5400 fish (< 1% of observations)

#### Assign recoveries into regions delineated by Weitkamp 2002 and 2010
dat.trim$rec.area.code <- NA
dat.trim$rec.area.code <- dat.loc.key$Rec.area[match(dat.trim$recovery_location_code, dat.loc.key$location_code)]

####################
# Lets aggregate by year and commerical type (coastwide)
dat.trim$fish.type	<- ""
dat.trim$fish.type[dat.trim$fishery < 20]	<- "comm"
dat.trim$fish.type[dat.trim$fishery > 20]	<- "sport"


#write this new data set to file to be able to call whenever I want

write.csv(dat.trim,file="Ocean_Recover_Chinook_1973-2013_trim(8-2015).csv",row.names=F)


# A 	<-	aggregate(dat.trim$fishery,by=list(year = dat.trim$rec.year,fish.type = dat.trim$fish.type),length)
# B	<-	data.frame(A)
# B$x	<- as.numeric(as.character(B$x))
# 
# par(mfrow=c(2,1),mar=c(4,4,0.5,0.5))
# barplot(B$x[B$fish.type=="comm"],names.arg=B$year[B$fish.type=="comm"],las=2)
# barplot(B$x[B$fish.type=="sport"],names.arg=B$year[B$fish.type=="sport"],las=2)

####################
### OK.  Let's start making some summary tables (make csv turn ocnvert into excel files externally)(

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
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
dat.t3	<-	dat.t2[dat.t2$fish.type == "comm",]

#### ALL AREAS IN ONE 
A 	<-	aggregate(dat.t3$rec.area.code,by=list(year = dat.t3$rec.year,location = dat.t3$rec.area.code,month=dat.t3$rec.month),length)
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
	colnames(NEW)	<- paste("X",sort(unique(dat.t3$rec.month)),sep="")
	location.lab	<- rep(paste(LOC$plot.name[i],".",LOC$areas[i],sep=""),nrow(temp.2))
	NEW				<-	data.frame(temp.2[,1:2],location.lab=location.lab,NEW)

	ALL	<- rbind(ALL,NEW)
	
}

 write.csv(ALL,file=paste("All COMM recoveries by location and month.csv",sep="."),row.names=F)



##### MAKE a grid for each location-month combination (COMMERCIAL FISH ONLY)
DAT	<-	read.csv("All COMM recoveries by location and month.csv",header=T)
library(fields)

pdf("Plot Comm recoveries by location and month.pdf",onefile=T)
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

