### Collate DFO effort data

#base.dir <- getwd()
data.dir <- "/Users/ole.shelton/GitHub/Orca_Salmon_DATA"
output.dir <- paste(base.dir,"/_Simulation and Analysis",sep="")

effort.dat	<-	read.csv(paste(data.dir,"/Effort info/DFO/DFO Effort Chinook 1982-1995.csv",sep=""))
stat.week.dat	<- read.csv(paste(data.dir,"/Effort info/DFO/DFO stat weeks 1970-1995.csv",sep=""))

# Merge in month for each effort week
effort.dat2	<-	merge(effort.dat,stat.week.dat[,c("Year","stat.week","month")])

Summary<-aggregate(effort.dat2$DAYS_FISHING,by=list(Year=effort.dat2$Year,Month=effort.dat2$month,Area=effort.dat2$AREA_DESCRIPTION),sum)
areas	<- sort(as.character(unique(Summary$Area)))
years	<-	sort(unique(Summary$Year))
month	<-	1:12
all.comb<-expand.grid(Year=years,Area=areas,Month=month)

effort.dat3	<- merge(Summary,all.comb,all=T)
effort.dat3	<-	effort.dat3[order(effort.dat3$Month),]

all.effort	<-	NULL

for(i in 1:length(years)){
	for(j in 1:length(areas)){
		temp	<-	effort.dat3[effort.dat3$Year == years[i] & effort.dat3$Area == areas[j],]
		temp.2	<-	c(years[i],areas[j], temp$x)
		all.effort	<-	rbind(all.effort,temp.2)
	}
}

all.effort[is.na(all.effort)==T]	<-	0
all.effort	<-	data.frame(all.effort)
colnames(all.effort)	<- c("Year","Area","january","february","march","april","may","june","july","august","september","october","november","december")
all.effort$effort.type <- "boat.day"

all.effort	<-	all.effort[order(all.effort$Area,all.effort$Year),]
write.csv(all.effort,file=paste(output.dir,"effort.data.bc.csv",sep=""),row.names=F)