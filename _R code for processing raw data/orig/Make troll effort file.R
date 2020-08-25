### SCRIPT FOR CREATING FILES THAT CAN BE READ IN BY STAN

# Troll Effort data first
#base.dir<-getwd()

ak.eff <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/Effort Data/effort.data.ak.csv",sep=""))
bc.eff <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/Effort Data/effort.data.bc+79-81.csv",sep=""))
colnames(bc.eff)[3:14] <- colnames(ak.eff)[3:14]
ca.or.wa.eff <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/Effort Data/effort.data.ca.or.wa.csv",sep=""))
colnames(ca.or.wa.eff)[3:14] <- colnames(ak.eff)[3:14]

locations <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/Locations-map to coastal areas 8-25-15.csv",sep=""))

ALL.MONTH   <- c("month.01","month.02","month.03","month.04","month.05","month.06","month.07","month.08","month.09","month.10","month.11","month.12")

### DIFFERENT TIME CONFIGURATIONS.
if(MONTH.STRUCTURE == "EIGHT"){
    ##### These are the month groupings to start:
  # April to October by month. November to March as a block

  # Need to make separate matrices for power and hand trolling

  ##############################################################
  # Alaska first (not done - need to fix hand to power troll efficiency)
  
  ak.eff$month.wint <- rowSums(ak.eff[,WINT.MONTH])
  #ak.eff.power <- ak.eff[ak.eff$gear == "power",c("year","SEAK.region",MONTH)]
  #ak.eff.hand <- ak.eff[ak.eff$gear == "hand",c("year","SEAK.region",MONTH)]

  ak.eff <- ak.eff[,c("year","SEAK.region",MONTH)]
  colnames(ak.eff)[2] <- "area.code"

  ##############################################################
  # BC second

  # need to map areas in BC to larger geographic provinces.
  bc.eff$area.code <- locations$area.code[match(bc.eff$Area,locations$stat.area.port)]
  
  bc.eff.by.area <- aggregate(bc.eff[,ALL.MONTH],by=list(Year=bc.eff$Year,area.code=bc.eff$area.code),sum)
  bc.eff.by.area$month.wint <- rowSums(bc.eff.by.area[,WINT.MONTH])

  bc.eff.power <- bc.eff.by.area[,c("Year","area.code",MONTH)]
  colnames(bc.eff.power)[1] <- "year"
  ##############################################################
  # US Coast last second

  # need to map areas in BC to larger geographic provinces.
  ca.or.wa.eff$area.code <- locations$area.code[match(ca.or.wa.eff$port,locations$stat.area.port)]

  ca.or.wa.eff.by.area <- aggregate(ca.or.wa.eff[,ALL.MONTH],by=list(year=ca.or.wa.eff$year,area.code=ca.or.wa.eff$area.code),sum)
  ca.or.wa.eff.by.area$month.wint <- rowSums(ca.or.wa.eff.by.area[,WINT.MONTH])

  ca.or.wa.eff.power <- ca.or.wa.eff.by.area[,c("year","area.code",MONTH)]

  ###################################################################################
  ###################################################################################
  # Combine the files and trim to match the Years span specified by the Master File

  effort <- rbind(ak.eff,bc.eff.power)
  effort <- rbind(effort,ca.or.wa.eff.power)

  temp<- expand.grid(year=YEARS.RECOVER,area.code=LOCATIONS$location.name)

  effort <- merge(effort,data.frame(year=YEARS.RECOVER))
  effort <- merge(effort,temp,all=T)

  effort$area.numb <- LOCATIONS$location.number[match(effort$area.code,LOCATIONS$location.name)]
  effort <- effort[order(effort$area.numb,effort$year),]
  effort[is.na(effort == T)]<- 0 
}
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
if(MONTH.STRUCTURE == "FOUR"){
  ##### These are the month groupings to start:
  #Seasonal Blocks
  ##############################################################
  # Alaska first (not done - need to fix hand to power troll efficiency)
  
  ak.eff$month.winter <- rowSums(ak.eff[,WINTER.MONTH])
  ak.eff$month.spring <- rowSums(ak.eff[,SPRING.MONTH])
  ak.eff$month.summer <- rowSums(ak.eff[,SUMMER.MONTH])
  ak.eff$month.fall   <- rowSums(ak.eff[,FALL.MONTH])
  #ak.eff.power <- ak.eff[ak.eff$gear == "power",c("year","SEAK.region",MONTH)]
  #ak.eff.hand <- ak.eff[ak.eff$gear == "hand",c("year","SEAK.region",MONTH)]
  
  ak.eff <- ak.eff[,c("year","SEAK.region",MONTH)]
  colnames(ak.eff)[2] <- "area.code"
  
  ##############################################################
  # BC second
  
  # need to map areas in BC to larger geographic provinces.
  bc.eff$area.code <- locations$area.code[match(bc.eff$Area,locations$stat.area.port)]
  
  bc.eff.by.area <- aggregate(bc.eff[,ALL.MONTH],by=list(Year=bc.eff$Year,area.code=bc.eff$area.code),sum)

  bc.eff.by.area$month.winter <- rowSums(bc.eff.by.area[,WINTER.MONTH])
  bc.eff.by.area$month.spring <- rowSums(bc.eff.by.area[,SPRING.MONTH])
  bc.eff.by.area$month.summer <- rowSums(bc.eff.by.area[,SUMMER.MONTH])
  bc.eff.by.area$month.fall   <- rowSums(bc.eff.by.area[,FALL.MONTH])
  
  bc.eff.power <- bc.eff.by.area[,c("Year","area.code",MONTH)]
  colnames(bc.eff.power)[1] <- "year"
  ##############################################################
  # US Coast last second
  
  # need to map areas in BC to larger geographic provinces.
  ca.or.wa.eff$area.code <- locations$area.code[match(ca.or.wa.eff$port,locations$stat.area.port)]
    ### DIVIDE WESTPORT EFFORT EQUALLY BETWEEN WAC and COL 
    ca.or.wa.eff[ca.or.wa.eff$port=="Westport",][,grep("month",colnames(ca.or.wa.eff))] <- ca.or.wa.eff[ca.or.wa.eff$port=="Westport",][,grep("month",colnames(ca.or.wa.eff))] /2
    temp <- ca.or.wa.eff[ca.or.wa.eff$port=="Westport",]
    temp$area.code <- "COL"
    ca.or.wa.eff <- rbind(ca.or.wa.eff,temp)
    ###
    
  ca.or.wa.eff.by.area <- aggregate(ca.or.wa.eff[,ALL.MONTH],by=list(year=ca.or.wa.eff$year,area.code=ca.or.wa.eff$area.code),sum)
  ca.or.wa.eff.by.area$month.winter <- rowSums(ca.or.wa.eff.by.area[,WINTER.MONTH])
  ca.or.wa.eff.by.area$month.spring <- rowSums(ca.or.wa.eff.by.area[,SPRING.MONTH])
  ca.or.wa.eff.by.area$month.summer <- rowSums(ca.or.wa.eff.by.area[,SUMMER.MONTH])
  ca.or.wa.eff.by.area$month.fall   <- rowSums(ca.or.wa.eff.by.area[,FALL.MONTH])

  ca.or.wa.eff.power <- ca.or.wa.eff.by.area[,c("year","area.code",MONTH)]
  ###################################################################################
  ###################################################################################
  # Combine the files and trim to match the Years span specified by the Master File
  
  effort <- rbind(ak.eff,bc.eff.power)
  effort <- rbind(effort,ca.or.wa.eff.power)
  
  temp<- expand.grid(year=YEARS.RECOVER,area.code=LOCATIONS$location.name)
  
  effort <- merge(effort,data.frame(year=YEARS.RECOVER))
  effort <- merge(effort,temp,all=T)
  
  effort$area.numb <- LOCATIONS$location.number[match(effort$area.code,LOCATIONS$location.name)]
  effort <- effort[order(effort$area.numb,effort$year),]
  effort[is.na(effort == T)]<- 0 
}



