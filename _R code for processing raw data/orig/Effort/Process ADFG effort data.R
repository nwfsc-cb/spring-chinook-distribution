### Process Alaska Fisheries troll effort data from ADFG.
library(dplyr)

#base.dir <- getwd()
data.dir <- "/Users/ole.shelton/GitHub/Orca_Salmon_DATA"

# Read in the raw data for creating 
dat.open<- read.csv(paste(data.dir,"/Effort info/Alaska/Carlile October 2015/SEAK summer chinook openings.csv",sep=""))
dat.stat<- read.csv(paste(data.dir,"/Effort info/Alaska/Carlile October 2015/stat.week.1979_2015_season_breaks.csv",sep=""))
dat.effort.season<- read.csv(paste(data.dir,"/Effort info/Alaska/Carlile October 2015/SEAK Effort 1979-2015 AOS.csv",sep=""))

dat.catch   <- read.csv(paste(data.dir,"/Effort info/Alaska/Carlile October 2015/SEAK Catch 1979-2015 AOS.csv",sep=""))

# adjust hand catch to account for lower cpue for hand vs. power troll.
hand.catch <- dat.catch[dat.catch$GEAR.TYPE == "Hand Troll",]
hand.catch <- hand.catch[order(hand.catch$Season,hand.catch$YEAR),]
power.catch <- dat.catch[dat.catch$GEAR.TYPE == "Power Troll",]  
power.catch <- power.catch[order(power.catch$Season,power.catch$YEAR),]

hand.effort <- dat.effort.season[dat.effort.season$GEAR.TYPE == "Hand Troll",]
hand.effort <- hand.effort[order(hand.effort$Season,hand.effort$YEAR),]
power.effort <- dat.effort.season[dat.effort.season$GEAR.TYPE == "Power Troll",]  
power.effort <- power.effort[order(power.effort$Season,power.effort$YEAR),]

hand.cpue <- data.frame(hand.effort[,1:3],(hand.catch[,4:7] / hand.effort[,4:7]))
power.cpue <- data.frame(power.effort[,1:3],(power.catch[,4:7] / power.effort[,4:7]))

cpue.ratio <- data.frame(power.effort[,1:3],(hand.cpue[,4:7] / power.cpue[,4:7]))
cpue.ratio$GEAR.TYPE <- "ratio.hand.to.power"


dat.hand   <- dat.catch[dat.catch$GEAR.TYPE =="Hand Troll",]
dat.hand   <- dat.hand[order(dat.hand$Season,dat.hand$YEAR),]
dat.hand.mod <- data.frame(dat.hand)
cpue.ratio <- cpue.ratio[order(cpue.ratio$Season,cpue.ratio$YEAR),]

# Get rid of NANs.  Visual inspection shows that all instances are of hand troll being =0 
dat.hand.mod[,c("NE","NW","SE","SW")] <- dat.hand[,c("NE","NW","SE","SW")] * cpue.ratio[,c("NW","NE","SE","SW")]
dat.hand.mod$NE[is.nan(dat.hand.mod$NE)] <- 0
dat.hand.mod$NW[is.nan(dat.hand.mod$NW)] <- 0
dat.hand.mod$SE[is.nan(dat.hand.mod$SE)] <- 0
dat.hand.mod$SW[is.nan(dat.hand.mod$SW)] <- 0

dat.catch <- dat.catch[order(dat.catch$Season,dat.catch$YEAR),]
dat.catch[dat.catch$GEAR.TYPE == "Hand Troll",c("NE","NW","SE","SW")]   <- dat.hand.mod[,c("NE","NW","SE","SW")]
# End hand troll adjustment.

dat.effort.season$N <- apply(dat.effort.season[,c("NE","NW")],1,sum)
dat.effort.season$S <- apply(dat.effort.season[,c("SE","SW")],1,sum)

dat.open$start.time <- strptime(dat.open$date.start,format="%m/%d/%Y")
dat.open$stop.time  <- strptime(dat.open$date.stop,format="%m/%d/%Y")
dat.open$year       <- as.numeric(substr(dat.open$start.time,1,4))

dat.open$start.month <- as.numeric(substr(dat.open$start.time,6,7))
dat.open$stop.month <- as.numeric(substr(dat.open$stop.time,6,7))

dat.stat$start.time <- strptime(dat.stat$date.start,format="%m/%d")
dat.stat$stop.time  <- strptime(dat.stat$date.stop,format="%m/%d")
dat.stat$days <- as.numeric(round(difftime(dat.stat$stop.time,dat.stat$start.time))) +1

#### Calculate fraction of effort that should be apportioned to each month
year    <- 1979:2014
season  <- c("winter.1","winter.2","spring","summer")

dat.prop.effort <- expand.grid(year=year,season=season)
dat.prop.effort<- data.frame(dat.prop.effort,matrix(0,nrow(dat.prop.effort),12))
colnames(dat.prop.effort)[3:ncol(dat.prop.effort)] <- paste("month",c("01","02","03","04","05","06","07","08","09","10","11","12"),sep=".")

for(i in 1:length(year)){
  for(j in 1:length(season)){ #loop over season
    
    if(season[j] == "winter.1"){
        dat.prop.effort[dat.prop.effort$year==year[i] & dat.prop.effort$season == season[j],][,c("month.11","month.12")] <- 0.25 
        dat.prop.effort[dat.prop.effort$year==year[i] & dat.prop.effort$season == season[j],][,c("month.10")] <- 0.50 
    }
    
    if(season[j] == "winter.2"){
       april.days <- as.numeric(substr(dat.stat$stop.time[dat.stat$year == year[i] & dat.stat$season == season[j]],9,10)) 
       frac.april <-april.days /  dat.stat$days[dat.stat$year == year[i] & dat.stat$season == season[j]]           
       frac.jan.mar <- (1 - frac.april) / 3
       dat.prop.effort[dat.prop.effort$year==year[i] & dat.prop.effort$season == season[j],][,c("month.01","month.02","month.03")] <- frac.jan.mar 
       dat.prop.effort[dat.prop.effort$year==year[i] & dat.prop.effort$season == season[j],][,c("month.04")] <- frac.april 
    }
    
    if(season[j] == "spring"){ 
      #statistical days open corresponding to each months
      april.end <- strptime("4/30/2016",format="%m/%d/%Y")
      april.days <- as.numeric(difftime(april.end,dat.stat$start.time[dat.stat$year == year[i] & dat.stat$season == season[j]]) ) +1
      may.days <- 31
      june.days <- as.numeric(substr(dat.stat$stop.time[dat.stat$year == year[i] & dat.stat$season == season[j]],9,10))
      july.days <- 0
      if(dat.stat$date.stop[dat.stat$year == year[i] & dat.stat$season == season[j]] =="7/1"){june.days = 30; july.days =1}

      #how many days in each month was the fishery open?
      temp <- dat.open[dat.open$year == year[i],]
      all.m.d <- NULL
        for(k in 1:nrow(temp)){
            if(temp$start.month[k] == temp$stop.month[k]){
                month.days <-c(temp$start.month[k],as.numeric(difftime(temp$stop.time[k],temp$start.time[k]))+1)
                all.m.d <- rbind(all.m.d, month.days)
            # print("here")
            }
            if(temp$start.month[k] != temp$stop.month[k]){
                n.month <-   temp$stop.month[k] - temp$start.month[k]
                for(l in 1:(n.month+1)){
                  if(l < (n.month+1)){
                    month.days <-c(temp$stop.month[k]-l+1,as.numeric(substr(temp$stop.time[k],9,10)))
                    all.m.d <- rbind(all.m.d, month.days)
                  }
                  if(l ==(n.month+1)){
                   n.day <- 31
                   if(temp$start.month[k] == 6 |temp$start.month[k] == 9 ){n.day == 30}
                   month.days <- c(temp$stop.month[k]-l+1,n.day - as.numeric(substr(temp$start.time[k],9,10)) +1)
                   all.m.d <- rbind(all.m.d, month.days) 
                  }
                }
            }
          }   
      all.m.d <- data.frame(all.m.d)
      colnames(all.m.d) <- c("month","days") 
      
      m.d <- aggregate(all.m.d$days,by=list(month = all.m.d$month),sum)
      m.d <- merge(m.d,data.frame(month=4:9),all=T); m.d[is.na(m.d)==T] <- 0
      poss.days <- data.frame(month = 4:7,n.days=c(april.days,may.days,june.days,july.days))
      m.d <- merge(m.d,poss.days)
      m.d$min <- apply(m.d[,2:3],1,min)
      m.d$frac <- m.d$min/sum(m.d$min)
      if(sum(m.d$min ==0)){m.d$frac <- m.d$n.days/ sum(m.d$n.days)}
      
      
      dat.prop.effort[dat.prop.effort$year==year[i] & dat.prop.effort$season == season[j],][,c("month.04")] <- m.d$frac[m.d$month==4] 
      dat.prop.effort[dat.prop.effort$year==year[i] & dat.prop.effort$season == season[j],][,c("month.05")] <- m.d$frac[m.d$month==5] 
      dat.prop.effort[dat.prop.effort$year==year[i] & dat.prop.effort$season == season[j],][,c("month.06")] <- m.d$frac[m.d$month==6]
      dat.prop.effort[dat.prop.effort$year==year[i] & dat.prop.effort$season == season[j],][,c("month.07")] <- m.d$frac[m.d$month==7] 
    }
   
    if(season[j] == "summer"){ 
    #statistical days open corresponding to each months
    june.end <- strptime("6/30/2016",format="%m/%d/%Y")
    june.days <- as.numeric(difftime(june.end,dat.stat$start.time[dat.stat$year == year[i] & dat.stat$season == season[j]]) ) +1
    july.days <- 31
    if(dat.stat$date.start[dat.stat$year == year[i] & dat.stat$season == season[j]] == "7/1"){june.days = 0;july.days = 31}
    if(dat.stat$date.start[dat.stat$year == year[i] & dat.stat$season == season[j]] == "7/2"){june.days = 0;july.days = 30}
    august.days <- 31
    sept.days <- as.numeric(substr(dat.stat$stop.time[dat.stat$year == year[i] & dat.stat$season == season[j]],9,10))

    #how many days in each month was the fishery open?
    temp <- dat.open[dat.open$year == year[i],]
    all.m.d <- NULL
    for(k in 1:nrow(temp)){
      if(temp$start.month[k] == temp$stop.month[k]){
        month.days <-c(temp$start.month[k],as.numeric(difftime(temp$stop.time[k],temp$start.time[k]))+1)
        all.m.d <- rbind(all.m.d, month.days)
        # print("here")
      }
      if(temp$start.month[k] != temp$stop.month[k]){
        n.month <-   temp$stop.month[k] - temp$start.month[k]
        for(l in 1:(n.month+1)){
          if(l < (n.month+1)){
            month.days <-c(temp$stop.month[k]-l+1,as.numeric(substr(temp$stop.time[k],9,10)))
            all.m.d <- rbind(all.m.d, month.days)
          }
          if(l ==(n.month+1)){
            n.day <- 31
            if(temp$start.month[k] == 6 |temp$start.month[k] == 9 ){n.day == 30}
            month.days <- c(temp$stop.month[k]-l+1,n.day - as.numeric(substr(temp$start.time[k],9,10)) +1)
            all.m.d <- rbind(all.m.d, month.days) 
          }
        }
      }
    }   
    all.m.d <- data.frame(all.m.d)
    colnames(all.m.d) <- c("month","days") 
    
    m.d <- aggregate(all.m.d$days,by=list(month = all.m.d$month),sum)
    m.d <- merge(m.d,data.frame(month=4:9),all=T); m.d[is.na(m.d)==T] <- 0
    
    poss.days <- data.frame(month = 6:9,n.days=c(june.days,july.days,august.days,sept.days))
    m.d <- merge(m.d,poss.days)
    m.d$min <- apply(m.d[,2:3],1,min)
    m.d$frac <- m.d$min/sum(m.d$min)
    
    dat.prop.effort[dat.prop.effort$year==year[i] & dat.prop.effort$season == season[j],][,c("month.06")] <- m.d$frac[m.d$month==6] 
    dat.prop.effort[dat.prop.effort$year==year[i] & dat.prop.effort$season == season[j],][,c("month.07")] <- m.d$frac[m.d$month==7] 
    dat.prop.effort[dat.prop.effort$year==year[i] & dat.prop.effort$season == season[j],][,c("month.08")] <- m.d$frac[m.d$month==8]
    dat.prop.effort[dat.prop.effort$year==year[i] & dat.prop.effort$season == season[j],][,c("month.09")] <- m.d$frac[m.d$month==9] 
  }
  } # end season loop
} # end year loop

    
##### OK.  So "dat.prop.effort" is a way of mapping the observed effort to particular months.  This section makes it happen

SEASON <- c("Winter 1","Winter 2","Spring","Summer")
season <- c("winter.1","winter.2","spring","summer")

hand  <- dat.effort.season[dat.effort.season$GEAR.TYPE == "Hand Troll",]
power <- dat.effort.season[dat.effort.season$GEAR.TYPE == "Power Troll",]

hand.all <- NULL
for(i in year){
  temp.n <- NULL
  temp.s <- NULL
    for(j in 1:length(SEASON)){
      temp.n <- rbind(temp.n, hand$N[hand$YEAR == i & hand$Season == SEASON[j]] * dat.prop.effort[dat.prop.effort$year == i & dat.prop.effort$season == season[j],3:ncol(dat.prop.effort)])
      temp.s <- rbind(temp.s, hand$S[hand$YEAR == i & hand$Season == SEASON[j]] * dat.prop.effort[dat.prop.effort$year == i & dat.prop.effort$season == season[j],3:ncol(dat.prop.effort)])
    }
  hand.all <- rbind(hand.all,c(i,"NSEAK",round(colSums(temp.n))))
  hand.all <- rbind(hand.all,c(i,"SSEAK",round(colSums(temp.s))))
}
  hand.all <- data.frame(hand.all)
  colnames(hand.all)[1:2] <- c("year","SEAK.region")
  hand.all[,3:ncol(hand.all)] <- round(hand.all[,3:ncol(hand.all)])
  #hand.all$gear <- "hand"
  
  
power.all <- NULL
for(i in year){
    temp.n <- NULL
    temp.s <- NULL
    for(j in 1:length(SEASON)){
      temp.n <- rbind(temp.n, power$N[power$YEAR == i & power$Season == SEASON[j]] * dat.prop.effort[dat.prop.effort$year == i & dat.prop.effort$season == season[j],3:ncol(dat.prop.effort)])
      temp.s <- rbind(temp.s, power$S[power$YEAR == i & power$Season == SEASON[j]] * dat.prop.effort[dat.prop.effort$year == i & dat.prop.effort$season == season[j],3:ncol(dat.prop.effort)])
    }
    power.all <- rbind(power.all,c(i,"NSEAK",round(colSums(temp.n))))
    power.all <- rbind(power.all,c(i,"SSEAK",round(colSums(temp.s))))
  }
  power.all <- data.frame(power.all)
  colnames(power.all)[1:2] <- c("year","SEAK.region")
  power.all[,3:ncol(power.all)] <- round(power.all[,3:ncol(power.all)])
  
  #power.all$gear <- "power"

  ### combine and write to file
  power.all <- power.all[order(power.all$year,power.all$SEAK.region),]
  hand.all <- hand.all[order(hand.all$year,hand.all$SEAK.region),]
  for(i in 3:ncol(power.all)){
    power.all[,i] <- as.numeric(as.character(power.all[,i]))
    hand.all[,i] <- as.numeric(as.character(hand.all[,i]))
  }
  both <- data.frame(power.all[,1:2], power.all[,3:ncol(power.all)] + hand.all[,3:ncol(hand.all)])

  write.csv(both, file="effort.data.ak.csv",row.names=F)
  
