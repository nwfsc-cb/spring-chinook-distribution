### SCRIPT FOR CREATING FILES THAT CAN BE READ IN BY STAN

# Troll Effort data first
#base.dir<-getwd()

ak.eff <- read.csv(paste(base.dir,"/spring-chinook-distribution/Processed Data/Effort Data/effort.data.ak.2022-09.csv",sep=""))
bc.eff <- read.csv(paste(base.dir,"/spring-chinook-distribution/Processed Data/Effort Data/effort.data.bc.1978-2018.csv",sep=""))
colnames(bc.eff)[3:14] <- colnames(ak.eff)[3:14]
ca.or.wa.eff <- read.csv(paste(base.dir,"/spring-chinook-distribution/Processed Data/Effort Data/effort.data.ca.or.wa-2022-07.csv",sep=""))
colnames(ca.or.wa.eff)[3:14] <- colnames(ak.eff)[3:14]

locations <- read.csv(paste(base.dir,"/spring-chinook-distribution/Processed Data/Locations-map to coastal areas 1-2019.csv",sep=""))

ALL.MONTH   <- c("month.01","month.02","month.03","month.04","month.05","month.06","month.07","month.08","month.09","month.10","month.11","month.12")

### DIFFERENT TIME CONFIGURATIONS.
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
#if(MONTH.STRUCTURE == "FOUR"){
  ##### These are the month groupings to start:
  #Seasonal Blocks
  ##############################################################
  # Alaska first 
  

ak.eff <- ak.eff %>% arrange(year)


  if(MONTH.STRUCTURE=="FOUR"){
    ak.eff$month.winter.2 <- rowSums(ak.eff[,WINTER.MONTH[1:3]])
    ak.eff$month.winter.1 <- rowSums(ak.eff[,WINTER.MONTH[4:5]])
    ak.eff$month.spring <- rowSums(ak.eff[,SPRING.MONTH])
    ak.eff$month.summer <- rowSums(ak.eff[,SUMMER.MONTH])
    ak.eff$month.fall   <- rowSums(ak.eff[,FALL.MONTH])
  }
  if(MONTH.STRUCTURE=="FRAM"){
    ak.eff$month.winter.2 <- ak.eff[,WINTER.MONTH[1]]
    ak.eff$month.winter.1 <- rowSums(ak.eff[,WINTER.MONTH[2:4]])
    ak.eff$month.spring <- rowSums(ak.eff[,SPRING.MONTH])
    ak.eff$month.summer <- rowSums(ak.eff[,SUMMER.MONTH])
    ak.eff$month.fall   <- rowSums(ak.eff[,FALL.MONTH])
  }
  if(MONTH.STRUCTURE=="SPRING"){
    ak.eff$month.winter.2 <- rowSums(ak.eff[,WINTER.MONTH[1:2]])
    ak.eff$month.winter.1 <- rowSums(ak.eff[,WINTER.MONTH[3:4]])
    ak.eff$month.spring <- rowSums(ak.eff[,SPRING.MONTH])
    ak.eff$month.summer <- rowSums(ak.eff[,SUMMER.MONTH])
    ak.eff$month.fall   <- rowSums(ak.eff[,FALL.MONTH])
  }

  #ak.eff.power <- ak.eff[ak.eff$gear == "power",c("year","SEAK.region",MONTH)]
  #ak.eff.hand <- ak.eff[ak.eff$gear == "hand",c("year","SEAK.region",MONTH)]

  ak.eff$year.wint.2  <- ak.eff$year-1
  
  temp <- ak.eff[,c("year.wint.2", "SEAK.region","month.winter.2") ]
  ak.eff <- ak.eff %>% dplyr::select(-year.wint.2,-month.winter.2)
  ak.eff <- merge(ak.eff,temp,by.x=c("year","SEAK.region"),by.y=c("year.wint.2" ,"SEAK.region"),all=T)
  ak.eff$month.winter <- ak.eff$month.winter.2 + ak.eff$month.winter.1
  
  ak.eff <- ak.eff[,c("year","SEAK.region",MONTH)]
  colnames(ak.eff)[2] <- "area.code"
  
  # Replace "YAK" with "NEGOA"
  ak.eff <- ak.eff %>% mutate(area.code=ifelse(area.code == "YAK","NEGOA",area.code))
  # Drop 1984 and earlier data because data starts in 1985
  #ak.eff <- ak.eff %>% filter(year >= 1985)
  
  
  ##############################################################
  # BC second
  
  # need to map areas in BC to larger geographic provinces.
  bc.eff$area.code <- locations$area.code[match(bc.eff$AREA.NAME,locations$stat.area.port)]
  
  bc.eff.by.area <-     aggregate(bc.eff[,ALL.MONTH],by=list(Year=bc.eff$year,area.code=bc.eff$area.code),sum)

  
  if(MONTH.STRUCTURE=="FOUR"){
    bc.eff.by.area$month.winter.2 <- rowSums(bc.eff.by.area[,WINTER.MONTH[1:3]])
    bc.eff.by.area$month.winter.1 <- rowSums(bc.eff.by.area[,WINTER.MONTH[4:5]])
    bc.eff.by.area$month.spring <- rowSums(bc.eff.by.area[,SPRING.MONTH])
    bc.eff.by.area$month.summer <- rowSums(bc.eff.by.area[,SUMMER.MONTH])
    bc.eff.by.area$month.fall   <- rowSums(bc.eff.by.area[,FALL.MONTH])
  }
  if(MONTH.STRUCTURE=="FRAM"){
    bc.eff.by.area$month.winter.2 <- rowSums(bc.eff.by.area[,WINTER.MONTH[1]])
    bc.eff.by.area$month.winter.1 <- rowSums(bc.eff.by.area[,WINTER.MONTH[2:4]])
    bc.eff.by.area$month.spring <- rowSums(bc.eff.by.area[,SPRING.MONTH])
    bc.eff.by.area$month.summer <- rowSums(bc.eff.by.area[,SUMMER.MONTH])
    bc.eff.by.area$month.fall   <- rowSums(bc.eff.by.area[,FALL.MONTH])
  }
  if(MONTH.STRUCTURE=="SPRING"){
    bc.eff.by.area$month.winter.2 <- rowSums(bc.eff.by.area[,WINTER.MONTH[1:2]])
    bc.eff.by.area$month.winter.1 <- rowSums(bc.eff.by.area[,WINTER.MONTH[3:4]])
    bc.eff.by.area$month.spring <- rowSums(bc.eff.by.area[,SPRING.MONTH])
    bc.eff.by.area$month.summer <- rowSums(bc.eff.by.area[,SUMMER.MONTH])
    bc.eff.by.area$month.fall   <- rowSums(bc.eff.by.area[,FALL.MONTH])
  }
  
  
  bc.eff.by.area$year.wint.2  <- bc.eff.by.area$Year-1
  
  temp <- bc.eff.by.area[,c("year.wint.2", "area.code","month.winter.2") ]
  bc.eff.by.area <-  bc.eff.by.area %>% dplyr::select(-year.wint.2,-month.winter.2)
  bc.eff.by.area <- merge(bc.eff.by.area,temp,by.x=c("Year","area.code"),by.y=c("year.wint.2" ,"area.code"),all=T)
  bc.eff.by.area$month.winter <- bc.eff.by.area$month.winter.2 + bc.eff.by.area$month.winter.1
  
  bc.eff.power <- bc.eff.by.area[,c("Year","area.code",MONTH)]
  colnames(bc.eff.power)[1] <- "year"
  
  bc.eff.power[is.na(bc.eff.power)==T] <- 0
  
  
  ##############################################################
  # US Coast last
  
  if(loc_18 %in% c("TWO_OR","_two_OR_PUSO_AK")){
    ca.or.wa.eff$area.code <- locations$area.code.two.OR[match(ca.or.wa.eff$port,locations$stat.area.port)]
  }else if(loc_18=="NCA_SOR_PUSO"){
      ca.or.wa.eff$area.code <- locations$area.code.NCA_SOR_PUSO[match(ca.or.wa.eff$port,locations$stat.area.port)]
  }else{
    ca.or.wa.eff$area.code <- locations$area.code[match(ca.or.wa.eff$port,locations$stat.area.port)]
  }
  # get rid of unmatched locations.
  ca.or.wa.eff <- ca.or.wa.eff %>% filter(!is.na(area.code))
      ### DIVIDE WESTPORT EFFORT EQUALLY BETWEEN WAC and COL 
    ca.or.wa.eff[ca.or.wa.eff$port=="Westport",][,grep("month",colnames(ca.or.wa.eff))] <- ca.or.wa.eff[ca.or.wa.eff$port=="Westport",][,grep("month",colnames(ca.or.wa.eff))] /2
    temp <- ca.or.wa.eff[ca.or.wa.eff$port=="Westport",]
    temp$area.code <- "COL"
    ca.or.wa.eff <- rbind(ca.or.wa.eff,temp)
    ###
    
  ca.or.wa.eff.by.area <- aggregate(ca.or.wa.eff[,ALL.MONTH],by=list(year=ca.or.wa.eff$year,area.code=ca.or.wa.eff$area.code),sum)
  
  if(MONTH.STRUCTURE=="FOUR"){
    ca.or.wa.eff.by.area$month.winter.2 <- rowSums(ca.or.wa.eff.by.area[,WINTER.MONTH[1:3]])
    ca.or.wa.eff.by.area$month.winter.1 <- rowSums(ca.or.wa.eff.by.area[,WINTER.MONTH[4:5]])
    ca.or.wa.eff.by.area$month.spring <- rowSums(ca.or.wa.eff.by.area[,SPRING.MONTH])
    ca.or.wa.eff.by.area$month.summer <- rowSums(ca.or.wa.eff.by.area[,SUMMER.MONTH])
    ca.or.wa.eff.by.area$month.fall   <- rowSums(ca.or.wa.eff.by.area[,FALL.MONTH])
  }
  if(MONTH.STRUCTURE=="FRAM"){
    ca.or.wa.eff.by.area$month.winter.2 <- ca.or.wa.eff.by.area[,WINTER.MONTH[1]]
    ca.or.wa.eff.by.area$month.winter.1 <- rowSums(ca.or.wa.eff.by.area[,WINTER.MONTH[2:4]])
    ca.or.wa.eff.by.area$month.spring <- rowSums(ca.or.wa.eff.by.area[,SPRING.MONTH])
    ca.or.wa.eff.by.area$month.summer <- rowSums(ca.or.wa.eff.by.area[,SUMMER.MONTH])
    ca.or.wa.eff.by.area$month.fall   <- rowSums(ca.or.wa.eff.by.area[,FALL.MONTH])
  }
  if(MONTH.STRUCTURE=="SPRING"){
    ca.or.wa.eff.by.area$month.winter.2 <- rowSums(ca.or.wa.eff.by.area[,WINTER.MONTH[1:2]])
    ca.or.wa.eff.by.area$month.winter.1 <- rowSums(ca.or.wa.eff.by.area[,WINTER.MONTH[3:4]])
    ca.or.wa.eff.by.area$month.spring <- rowSums(ca.or.wa.eff.by.area[,SPRING.MONTH])
    ca.or.wa.eff.by.area$month.summer <- rowSums(ca.or.wa.eff.by.area[,SUMMER.MONTH])
    ca.or.wa.eff.by.area$month.fall   <- rowSums(ca.or.wa.eff.by.area[,FALL.MONTH])
  }
  
  ca.or.wa.eff.by.area$year.wint.2  <- ca.or.wa.eff.by.area$year-1
  
  temp <- ca.or.wa.eff.by.area[,c("year.wint.2", "area.code","month.winter.2") ]
  ca.or.wa.eff.by.area <-  ca.or.wa.eff.by.area %>% dplyr::select(-year.wint.2,-month.winter.2)
  ca.or.wa.eff.by.area <- merge(ca.or.wa.eff.by.area,temp,by.x=c("year","area.code"),by.y=c("year.wint.2" ,"area.code"),all=T)
  ca.or.wa.eff.by.area$month.winter <- ca.or.wa.eff.by.area$month.winter.2 + ca.or.wa.eff.by.area$month.winter.1
  
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
