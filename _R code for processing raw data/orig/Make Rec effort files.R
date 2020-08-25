### SCRIPT FOR CREATING FILES THAT CAN BE READ IN BY STAN

# Rec Effort data first
#base.dir<-getwd()

ak.eff <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/Effort Data/effort.data.ak.csv",sep=""))
ca.or.wa.eff <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/Effort Data/effort.data.REC.ca.or.wa.csv",sep=""))
puso.eff <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/Effort Data/effort.data.REC.puso.csv",sep=""))
sgeo.eff <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/Effort Data/effort.data.REC.sgeo.csv",sep=""))

colnames(ca.or.wa.eff)[3:14] <- colnames(ak.eff)[3:14]

locations <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/Locations-map to coastal areas 8-25-15.csv",sep=""))

##### These are the month groupings to start:

# Need to make separate matrices for power and hand trolling
ALL.MONTH   <- c("month.01","month.02","month.03","month.04","month.05","month.06","month.07","month.08","month.09","month.10","month.11","month.12")

### At present we lack rec effort for 1978-1995 for both BC (except SGEO) and Alaska.  
### Therefore, we will use three files for rec effort:  
  ### One for the south in which we will use the recreational effort data. 
  ### One for SGEO where we have effort data (post 1982) but it units of boat trips, not angler trips.
  ### One for the north where we will not.

if(MONTH.STRUCTURE == "EIGHT"){
  # April to October by month. November to March as a block
  ##############################################################
  # US Coast first

  # need to map areas in BC to larger geographic provinces.
  ca.or.wa.eff$area.code <- locations$area.code[match(ca.or.wa.eff$port,locations$stat.area.port)]

  ca.or.wa.eff.by.area <- aggregate(ca.or.wa.eff[,ALL.MONTH],by=list(year=ca.or.wa.eff$year,area.code=ca.or.wa.eff$area.code),sum)
  ca.or.wa.eff.by.area$month.wint <- rowSums(ca.or.wa.eff.by.area[,WINT.MONTH])

  ca.or.wa.eff.rec <- ca.or.wa.eff.by.area[,c("year","area.code",MONTH)]

  # Puget Sound second

  puso.eff$total.effort <- rowSums(puso.eff[,4:14])
  temp <- aggregate(puso.eff$total.effort,by=list(year=puso.eff$Year,month=puso.eff$Month),sum)

  temp <- temp[order(temp$year,temp$month),]

  puso.eff.wide <- data.frame(port="All Puget Sound",year=YEARS.RECOVER)
  puso.eff.wide <- cbind(puso.eff.wide,matrix(0,length(YEARS.RECOVER),12))
  for(i in YEARS.RECOVER){
      puso.eff.wide[puso.eff.wide$year == i,3:14] <- temp$x[temp$year == i]   
  }
  colnames(puso.eff.wide)[3:14] <- ALL.MONTH

  puso.eff.wide$effort.type <- "angler.trip"
  puso.eff.wide$Notes <- NA
  puso.eff.wide$area.code <- "PUSO"

  puso.eff.by.area <- aggregate(puso.eff.wide[,ALL.MONTH],by=list(year=puso.eff.wide$year,area.code=puso.eff.wide$area.code),sum)
  puso.eff.by.area$month.wint <- rowSums(puso.eff.by.area[,WINT.MONTH])

  puso.eff.rec <- puso.eff.by.area[,c("year","area.code",MONTH)]
  
  ### SGEO 3rd
  
  sgeo.trim <- sgeo.eff[,grep("X",colnames(sgeo.eff))]
  
  sgeo.eff$total.effort <- rowSums(sgeo.trim,na.rm=T)
  
  sgeo.agg <-expand.grid(month.numb=1:12,year=YEARS.RECOVER)
  sgeo.eff <- merge(sgeo.agg, sgeo.eff, all=T)
  sgeo.agg <- merge(sgeo.eff[,c("year","month.numb","total.effort")],sgeo.agg,all=T)
  
  sgeo.eff.wide <- data.frame(port="SGEO All",year=YEARS.RECOVER)
  sgeo.eff.wide <- cbind(sgeo.eff.wide,matrix(0,length(YEARS.RECOVER),12))
  for(i in YEARS.RECOVER){
    for(j in 1:12){
      sgeo.eff.wide[sgeo.eff.wide$year == i,j+2 ] <- sgeo.agg$total.effort[sgeo.agg$year== i & sgeo.agg$month.numb== j]
    }
  }
  
  colnames(sgeo.eff.wide)[3:ncol(sgeo.eff.wide)] <- ALL.MONTH
  
  sgeo.eff.wide$month.winter <- rowSums(sgeo.eff.wide[,WINTER.MONTH])
  sgeo.eff.wide$month.spring <- rowSums(sgeo.eff.wide[,SPRING.MONTH])
  sgeo.eff.wide$month.summer <- rowSums(sgeo.eff.wide[,SUMMER.MONTH])
  sgeo.eff.wide$month.fall   <- rowSums(sgeo.eff.wide[,FALL.MONTH])
  sgeo.eff.wide$area.code <- "SGEO"
  sgeo.eff.rec <- sgeo.eff.wide[,c("year","area.code",MONTH)]
  
  
  ### OK.  FROM VISUAL INSPECTION OF EFFORT DATA, I MADE THESE EXECUTIVE DECISIONS ON WHICH YEARS TO INCLUDE EFFORT DATA FROM:
  # 1) Exclude effort data from pre-1982. (possible to inclue 7/81, 8/81)
  sgeo.eff.rec[1:3,3:ncol(sgeo.eff.rec)] <- 0
  # 2) Exclude effort data from spring (month 4 in 1982, month 5 in 1983) 1982, 1983
  sgeo.eff.rec$month.04[sgeo.eff.rec$year == 1982] <- 0
  sgeo.eff.rec$month.05[sgeo.eff.rec$year == 1983] <- 0
  # 3) Exclude effrot data from winter 90-91, 91-92,winter 93-94,94-95
  sgeo.eff.rec$month.winter[sgeo.eff.rec$year == 1990] <- 0
  sgeo.eff.rec$month.winter[sgeo.eff.rec$year == 1991] <- 0
  sgeo.eff.rec$month.winter[sgeo.eff.rec$year == 1993] <- 0
  sgeo.eff.rec$month.winter[sgeo.eff.rec$year == 1994] <- 0
  sgeo.eff.rec$month.winter[sgeo.eff.rec$year == 1995] <- 0
  

  ###################################################################################
  ###################################################################################
  # Combine the files and trim to match the Years span specified by the Master File
  effort <- rbind(ca.or.wa.eff.rec,puso.eff.rec)

  temp<- expand.grid(year=YEARS.RECOVER,area.code=LOCATIONS$location.name)

  effort <- merge(effort,data.frame(year=YEARS.RECOVER))
  effort <- merge(effort,temp,all=T)

  effort$area.numb <- LOCATIONS$location.number[match(effort$area.code,LOCATIONS$location.name)]
  effort <- effort[order(effort$area.numb,effort$year),]
  effort[is.na(effort == T)]<- 0 
}
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
if(MONTH.STRUCTURE == "FOUR"){
  # Seasonal Blocks
  ##############################################################
  # AK Effort
  # ak.eff$area.code <- ak.eff$SEAK.region
  # ak.eff.by.area   <- aggregate(ak.eff[,ALL.MONTH],by=list(year=ak.eff$year,area.code=ak.eff$area.code),sum)
  # 
  # ak.eff.by.area$month.winter <- rowSums(ak.eff.by.area[,WINTER.MONTH])
  # ak.eff.by.area$month.spring <- rowSums(ak.eff.by.area[,SPRING.MONTH])
  # ak.eff.by.area$month.summer <- rowSums(ak.eff.by.area[,SUMMER.MONTH])
  # ak.eff.by.area$month.fall   <- rowSums(ak.eff.by.area[,FALL.MONTH])
  # 
  # ak.eff.rec <- ak.eff.by.area[,c("year","area.code",MONTH)]
  
  # US Coast second
  
  # need to map areas in BC to larger geographic provinces.
  ca.or.wa.eff$area.code <- locations$area.code[match(ca.or.wa.eff$port,locations$stat.area.port)]
  ### DIVIDE WESTPORT EFFORT EQUALLY BETWEEN WAC and COL 
    ca.or.wa.eff[ca.or.wa.eff$port=="Westport",][,grep("month",colnames(ca.or.wa.eff))] <- ca.or.wa.eff[ca.or.wa.eff$port=="Westport",][,grep("month",colnames(ca.or.wa.eff))] /2
    temp <- ca.or.wa.eff[ca.or.wa.eff$port=="Westport",]
    temp$area.code <- "COL"
    ca.or.wa.eff <- rbind(ca.or.wa.eff,temp)
  ###
  
  
  ca.or.wa.eff.by.area   <- aggregate(ca.or.wa.eff[,ALL.MONTH],by=list(year=ca.or.wa.eff$year,area.code=ca.or.wa.eff$area.code),sum)
  
  ca.or.wa.eff.by.area$month.winter <- rowSums(ca.or.wa.eff.by.area[,WINTER.MONTH])
  ca.or.wa.eff.by.area$month.spring <- rowSums(ca.or.wa.eff.by.area[,SPRING.MONTH])
  ca.or.wa.eff.by.area$month.summer <- rowSums(ca.or.wa.eff.by.area[,SUMMER.MONTH])
  ca.or.wa.eff.by.area$month.fall   <- rowSums(ca.or.wa.eff.by.area[,FALL.MONTH])
  
  ca.or.wa.eff.rec <- ca.or.wa.eff.by.area[,c("year","area.code",MONTH)]
  
  # Puget Sound Third
  puso.eff[,4:14] <- puso.eff[,4:14] * puso.eff$adjust
  puso.eff$total.effort <- rowSums(puso.eff[,4:14])
  
  temp <- aggregate(puso.eff$total.effort,by=list(year=puso.eff$Year,month=puso.eff$Month),sum)
  temp <- temp[order(temp$year,temp$month),]
  
  puso.eff.wide <- data.frame(port="All Puget Sound",year=YEARS.RECOVER)
  puso.eff.wide <- cbind(puso.eff.wide,matrix(0,length(YEARS.RECOVER),12))
  for(i in YEARS.RECOVER){
    puso.eff.wide[puso.eff.wide$year == i,3:14] <- temp$x[temp$year == i]   
  }
  colnames(puso.eff.wide)[3:14] <- ALL.MONTH
  
  puso.eff.wide$effort.type <- "angler.trip"
  puso.eff.wide$Notes <- NA
  puso.eff.wide$area.code <- "PUSO"
  
  puso.eff.by.area <- aggregate(puso.eff.wide[,ALL.MONTH],by=list(year=puso.eff.wide$year,area.code=puso.eff.wide$area.code),sum)
  
  puso.eff.by.area$month.winter <- rowSums(puso.eff.by.area[,WINTER.MONTH])
  puso.eff.by.area$month.spring <- rowSums(puso.eff.by.area[,SPRING.MONTH])
  puso.eff.by.area$month.summer <- rowSums(puso.eff.by.area[,SUMMER.MONTH])
  puso.eff.by.area$month.fall   <- rowSums(puso.eff.by.area[,FALL.MONTH])
  
  puso.eff.rec <- puso.eff.by.area[,c("year","area.code",MONTH)]
  
  ### SGEO 3rd
  
  sgeo.trim <- sgeo.eff[,grep("X",colnames(sgeo.eff))]
  
  sgeo.eff$total.effort <- rowSums(sgeo.trim,na.rm=T)
  
  sgeo.agg <-expand.grid(month.numb=1:12,year=YEARS.RECOVER)
  sgeo.eff <- merge(sgeo.agg, sgeo.eff, all=T)
  sgeo.agg <- merge(sgeo.eff[,c("year","month.numb","total.effort")],sgeo.agg,all=T)

  sgeo.eff.wide <- data.frame(port="SGEO All",year=YEARS.RECOVER)
  sgeo.eff.wide <- cbind(sgeo.eff.wide,matrix(0,length(YEARS.RECOVER),12))
  for(i in YEARS.RECOVER){
    for(j in 1:12){
      sgeo.eff.wide[sgeo.eff.wide$year == i,j+2 ] <- sgeo.agg$total.effort[sgeo.agg$year== i & sgeo.agg$month.numb== j]
    }
  }
  
  colnames(sgeo.eff.wide)[3:ncol(sgeo.eff.wide)] <- ALL.MONTH
  
  sgeo.eff.wide$month.winter <- rowSums(sgeo.eff.wide[,WINTER.MONTH])
  sgeo.eff.wide$month.spring <- rowSums(sgeo.eff.wide[,SPRING.MONTH])
  sgeo.eff.wide$month.summer <- rowSums(sgeo.eff.wide[,SUMMER.MONTH])
  sgeo.eff.wide$month.fall   <- rowSums(sgeo.eff.wide[,FALL.MONTH])
  sgeo.eff.wide$area.code <- "SGEO"
  sgeo.eff.rec <- sgeo.eff.wide[,c("year","area.code",MONTH)]
  
  
  ### OK.  FROM VISUAL INSPECTION OF EFFORT DATA, I MADE THESE EXECUTIVE DECISIONS ON WHICH YEARS TO INCLUDE EFFORT DATA FROM:
    # 1) Exclude effort data from pre-1982. (possible to include 7/81, 8/81)
        sgeo.eff.rec[1:3,3:6] <- 0
    # 2) Exclude effort data from spring (month 4 in 1982, month 5 in 1983) 1982, 1983
        sgeo.eff.rec$month.spring[sgeo.eff.rec$year == 1982] <- 0
        sgeo.eff.rec$month.spring[sgeo.eff.rec$year == 1983] <- 0
    # 3) Exclude effot data from winter 90-91, 91-92,winter 93-94,94-95
        sgeo.eff.rec$month.winter[sgeo.eff.rec$year == 1990] <- 0
        sgeo.eff.rec$month.winter[sgeo.eff.rec$year == 1991] <- 0
        sgeo.eff.rec$month.winter[sgeo.eff.rec$year == 1993] <- 0
        sgeo.eff.rec$month.winter[sgeo.eff.rec$year == 1994] <- 0
        sgeo.eff.rec$month.winter[sgeo.eff.rec$year == 1995] <- 0
        
  
  ###################################################################################
  ###################################################################################
  # Combine the files and trim to match the Years span specified by the Master File

  effort <- rbind(ca.or.wa.eff.rec,puso.eff.rec)
  effort <- rbind(ca.or.wa.eff.rec,puso.eff.rec,sgeo.eff.rec)
  #effort <- rbind(ca.or.wa.eff.rec,puso.eff.rec,sgeo.eff.rec,ak.eff.rec)
  
  temp<- expand.grid(year=YEARS.RECOVER,area.code=LOCATIONS$location.name)
  
  effort <- merge(effort,data.frame(year=YEARS.RECOVER))
  effort <- merge(effort,temp,all=T)
  
  effort$area.numb <- LOCATIONS$location.number[match(effort$area.code,LOCATIONS$location.name)]
  effort <- effort[order(effort$area.numb,effort$year),]
  effort[is.na(effort == T)]<- 0 
}

effort.rec <- effort