### Collate DFO effort data

library(reshape2)
library(dplyr)
#base.dir <- getwd()
data.dir <- "/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Effort info/BC"
output.dir <- paste("/Users/ole.shelton/GitHub/spring-chinook-distribution/Processed Data/Effort Data",sep="")

effort.dat	<-	read.csv(paste(data.dir,"/DFO Effort Chinook 1982-1995.csv",sep=""))
stat.week.dat	<- read.csv(paste(data.dir,"/DFO stat weeks 1970-1995.csv",sep=""))

# Merge in month for each effort week
effort.dat2	<-	merge(effort.dat,stat.week.dat[,c("Year","stat.week","month")],by.x=c("YEAR","PERIOD_CODE"),by.y=c("Year","stat.week"))

Summary<-aggregate(effort.dat2$DAYS_FISHING,by=list(Year=effort.dat2$YEAR,Month=effort.dat2$month,Area=effort.dat2$AREA_DESCRIPTION),sum)
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


## This is the file with effort for 1982-1995
  #write.csv(all.effort,file=paste(output.dir,"effort.data.bc.csv",sep=""),row.names=F)
## Then we added manually the data from 1979-1981 from published DFO reports
  early.effort <- read.csv(paste(data.dir,"/effort.data.bc+78-81.csv",sep=""))
  
  ############### 1996-2004
  
#  This data is the troll information for 1996-2004

#### Note that separation for effort that did not include CHINOOK RETENTION is designated by the Ole added column "Chinook.Closed"
  
  mid.effort <- read.csv(paste(data.dir,"/1996 to 2004 post season salmon effort.csv",sep=""))
  
  # Only include period that fall during periods open for chinook retention
  mid.effort <- mid.effort %>% filter(Chinook.Closed != "Closed")
  
  mid.effort$PERIOD.START <- as.Date(mid.effort$PERIOD.START.DATE,"%m/%d/%Y")
  mid.effort$PERIOD.END <- as.Date(mid.effort$PERIOD.END.DATE,"%m/%d/%Y")
  mid.effort$year.start <- as.numeric(as.character(substr(mid.effort$PERIOD.START,1,4)))
  mid.effort$month.start <- as.numeric(as.character(substr(mid.effort$PERIOD.START,6,7)))
  mid.effort$day.start <- as.numeric(as.character(substr(mid.effort$PERIOD.START,9,10)))
  mid.effort$year.stop <- as.numeric(as.character(substr(mid.effort$PERIOD.END,1,4)))
  mid.effort$month.stop <- as.numeric(as.character(substr(mid.effort$PERIOD.END,6,7)))
  mid.effort$day.stop <- as.numeric(as.character(substr(mid.effort$PERIOD.END,9,10)))
  
  mid.effort$days.diff <- as.numeric(difftime(mid.effort$PERIOD.END,mid.effort$PERIOD.START,units="days")) + 1
  mid.effort$days.since.month <- mid.effort$day.stop 
  
  mid.effort$w.start <- 0.5
  mid.effort$w.stop  <- 0.5
  
  mid.effort.1 <- mid.effort %>% filter(month.start == month.stop) 
  mid.effort.2 <- mid.effort %>% filter(month.start != month.stop) %>% 
                        mutate(w.stop = days.since.month / days.diff,w.start= 1-w.stop ) 
  
  mid.effort <- rbind(mid.effort.1,mid.effort.2) 
  mid.effort <- mid.effort %>% mutate(eff.start= w.start*EFFORT,eff.stop=w.stop*EFFORT)
  
  A <- mid.effort %>% group_by(AREA.NAME,year.start,month.start) %>% 
          summarise(EFF=sum(eff.start,na.rm=T)) %>% as.data.frame()
  colnames(A)[2:3] <- c("year","month")
  B <- mid.effort %>% group_by(AREA.NAME,year.stop,month.stop) %>% 
          summarise(EFF=sum(eff.stop,na.rm=T)) %>% as.data.frame()
  colnames(B)[2:3] <- c("year","month")
  
  mid.effort <- rbind(A,B) %>% group_by(AREA.NAME,year,month) %>% summarise(effort = sum(EFF,na.rm=T)) %>% as.data.frame()
  
  write.csv(mid.effort,paste(data.dir,"/effort.data.bc.1996-2004.csv",sep=""))

  ## This is the expanded file.  
  mid.effort <- merge(mid.effort,expand.grid(year=sort(unique(mid.effort$year)),month=1:12,AREA.NAME=unique(mid.effort$AREA.NAME)),all=T)
  mid.effort$effort[is.na(mid.effort$effort)==T] <- 0
  
  mid.effort <- dcast(mid.effort, year + AREA.NAME ~ month)
  mid.effort$effort.type <- "boat.day"
###########  
  ### YEARS 2005-2015
###########  
  recent.effort <- NULL
  YR <- 2005:2015
  for(i in 1:length(YR)){
    temp <- read.csv(paste(data.dir,"/Troll-Effort-BC-",YR[i],".csv",sep=""))
    recent.effort <- rbind(temp,recent.effort)
  }
  recent.effort$effort <- as.numeric(as.character(recent.effort$EFFORT..BOAT.DAYS.))
  
  recent.effort$FISHING.DATE <- as.Date(recent.effort$FISHING.DATE,"%m/%d/%Y")
  recent.effort$year <- as.numeric(substr(recent.effort$FISHING.DATE,1,4)) + 2000
  recent.effort <-  recent.effort %>% filter(is.na(recent.effort$year)==F)
  recent.effort$month <-as.numeric(substr(recent.effort$FISHING.DATE,6,7))
  
  recent.effort  <- recent.effort %>% dplyr::select(.,CHINOOK.RETAINABLE,FISHERY.MANAGEMENT.AREA,effort,year,month) %>% 
          filter(CHINOOK.RETAINABLE=="YES") %>% dplyr::select(Area = FISHERY.MANAGEMENT.AREA,year,month,effort)
  recent.effort$Area <- paste("AREA",recent.effort$Area)
  
  recent.effort <- recent.effort %>% group_by(Area,year,month) %>% summarize(tot.effort=sum(effort))
  recent.effort <- dcast(recent.effort, year + Area ~ month,value.var = "tot.effort")
  recent.effort$effort.type <- "boat.day"
  
### Read in 2016-2018 data, modify,   
  
  y16.18.effort <- read.csv(paste0(data.dir,"/Commercial Chinook Salmon Troll Fishery In-season Catch Estimates by Month from 2016-2018.csv"))
  
  y16.18.effort <- y16.18.effort %>% dplyr::select(-FISHERY) %>% 
                  mutate(month.numb = case_when(
                    MONTH == "JANUARY" ~ 1,
                    MONTH == "FEBRUARY" ~ 2,
                    MONTH == "MARCH" ~ 3,
                    MONTH == "APRIL" ~ 4,
                    MONTH == "MAY" ~ 5,
                    MONTH == "JUNE" ~ 6,
                    MONTH == "JULY" ~ 7,
                    MONTH == "AUGUST" ~ 8,
                    MONTH == "SEPTEMBER" ~ 9,
                    MONTH == "OCTOBER" ~ 10,
                    MONTH == "NOVEMBER" ~ 11,
                    MONTH == "DECEMBER" ~ 12)) %>%
                  mutate(Area = paste("AREA",MGMT_AREA))
  
  y16.18.effort <- y16.18.effort %>% mutate(month.numb = as.character(month.numb))
  y16.18.effort$month.numb <- factor(y16.18.effort$month.numb,
                                     levels = as.character(1:12))
  
  temp <- y16.18.effort %>% group_by(YEAR,Area,MONTH,month.numb) %>% 
            summarise(boat.days=sum(BOAT_DAYS)) %>%
            arrange(month.numb)
  
  
  y16.18.effort.wide <- pivot_wider(temp ,id_cols = c("YEAR","Area"),
                            names_from = "month.numb",
                            values_from = "boat.days",
                            values_fill =0) %>%
                        # add a month '12' with zeros
                        mutate('12' = 0, effort.type="boat.day" )
  
######################################################################################  
######################################################################################
######################################################################################
######################################################################################
######################################################################################
## Combine early, mid, late troll data.
  
  colnames(early.effort)  <- colnames(mid.effort)
  colnames(recent.effort) <- colnames(mid.effort)
  colnames(y16.18.effort.wide)<-  colnames(mid.effort)
  
  bc.effort <- rbind(early.effort,mid.effort)
  bc.effort <- rbind(bc.effort,recent.effort)
  bc.effort <- rbind(bc.effort,y16.18.effort.wide)
  
  bc.effort <- bc.effort %>% arrange(year,AREA.NAME)
  bc.effort[is.na(bc.effort)==T] <- 0
  
  write.csv(bc.effort,paste(output.dir,"/effort.data.bc.1978-2018.csv",sep=""),row.names = F)  
  
  