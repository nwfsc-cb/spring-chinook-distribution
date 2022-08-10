### SCRIPT FOR CREATING FILES THAT CAN BE READ IN BY STAN

# Rec Effort data first
#print(base.dir)

ak.eff <- read.csv("./Processed Data/Effort Data/effort.data.ak.csv") # THIS IS ONLY COMMERCIAL TROLL (OMIT)
ca.or.wa.eff <- read.csv("./Processed Data/Effort Data/effort.data.REC.ca.or.wa.csv")
puso.eff       <- read.csv("./Processed Data/Effort Data/effort.data.REC.puso-to-2020.csv")
puso.retention <- read.csv("./Processed Data/Effort Data/WA PUSO Chinook retention.csv")
sgeo.eff <- read.csv("./Processed Data/Effort Data/effort.data.REC.sgeo.csv")
johnstone.eff <- read.csv("./Processed Data/Effort Data/effort.data.REC.johnstone.csv")
wcvi.eff <- read.csv("./Processed Data/Effort Data/effort.data.REC.wcvi.csv")

bc.eff <- read.csv("./Processed Data/Effort Data/effort.data.REC.bc.2019.csv")

can.irec.eff <- read.csv("./Processed Data/Effort Data/iREC chinook effort 05-2019.csv")
can.irec.mapping <- read.csv("./Processed Data/Effort Data/can.irec.areas.mapping.csv")
colnames(ca.or.wa.eff)[3:14] <- colnames(ak.eff)[3:14]

locations <- read.csv("./Processed Data/Locations-map to coastal areas 1-2019.csv")

##### These are the month groupings to start:

# Need to make separate matrices for power and hand trolling
ALL.MONTH   <- c("month.01","month.02","month.03","month.04","month.05","month.06","month.07","month.08","month.09","month.10","month.11","month.12")

### At present we lack rec effort for 1978-1995 for both BC (except SGEO) and Alaska.  
### Therefore, we will use three files for rec effort:  
  ### One for the south in which we will use the recreational effort data. 
  ### One for SGEO where we have effort data (post 1982) but it units of boat trips, not angler trips.
  ### One for the north where we will not.

##############################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#if(MONTH.STRUCTURE == "FOUR"){
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

# US Coast 

if(loc_18=="TWO_OR" | loc_18=="_two_OR_PUSO_AK"){
  ca.or.wa.eff$area.code <- locations$area.code.two.OR[match(ca.or.wa.eff$port,locations$stat.area.port)]
}else if(loc_18=="NCA_SOR_PUSO"){
  ca.or.wa.eff$area.code <- locations$area.code.NCA_SOR_PUSO[match(ca.or.wa.eff$port,locations$stat.area.port)]
}else{
  ca.or.wa.eff$area.code <- locations$area.code[match(ca.or.wa.eff$port,locations$stat.area.port)]
}
### DIVIDE WESTPORT EFFORT EQUALLY BETWEEN WAC and COL 
  ca.or.wa.eff[ca.or.wa.eff$port=="Westport",][,grep("month",colnames(ca.or.wa.eff))] <- ca.or.wa.eff[ca.or.wa.eff$port=="Westport",][,grep("month",colnames(ca.or.wa.eff))] /2
  temp <- ca.or.wa.eff[ca.or.wa.eff$port=="Westport",]
  temp$area.code <- "COL"
  ca.or.wa.eff <- rbind(ca.or.wa.eff,temp)
###
ca.or.wa.eff.by.area   <- aggregate(ca.or.wa.eff[,ALL.MONTH],by=list(year=ca.or.wa.eff$year,area.code=ca.or.wa.eff$area.code),sum)

if(MONTH.STRUCTURE=="FOUR"){
  ca.or.wa.eff.by.area$month.winter.2 <- rowSums(ca.or.wa.eff.by.area[,WINTER.MONTH[1:3]])
  ca.or.wa.eff.by.area$month.winter.1 <- rowSums(ca.or.wa.eff.by.area[,WINTER.MONTH[4:5]])
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
if(MONTH.STRUCTURE=="FRAM"){
  ca.or.wa.eff.by.area$month.winter.2 <- (ca.or.wa.eff.by.area[,WINTER.MONTH[1]])
  ca.or.wa.eff.by.area$month.winter.1 <- rowSums(ca.or.wa.eff.by.area[,WINTER.MONTH[2:4]])
  ca.or.wa.eff.by.area$month.spring <- rowSums(ca.or.wa.eff.by.area[,SPRING.MONTH])
  ca.or.wa.eff.by.area$month.summer <- rowSums(ca.or.wa.eff.by.area[,SUMMER.MONTH])
  ca.or.wa.eff.by.area$month.fall   <- rowSums(ca.or.wa.eff.by.area[,FALL.MONTH])
}

ca.or.wa.eff.by.area$year.wint.2  <- ca.or.wa.eff.by.area$year-1

temp <- ca.or.wa.eff.by.area[,c("year.wint.2", "area.code","month.winter.2") ]
ca.or.wa.eff.by.area <-  ca.or.wa.eff.by.area %>% dplyr::select(-year.wint.2,-month.winter.2)
ca.or.wa.eff.by.area <- merge(ca.or.wa.eff.by.area,temp,by.x=c("year","area.code"),by.y=c("year.wint.2" ,"area.code")) # added all=T to avoid cutting off a year of data and maintaining consistency with other scripts
ca.or.wa.eff.by.area$month.winter <- ca.or.wa.eff.by.area$month.winter.2 + ca.or.wa.eff.by.area$month.winter.1

ca.or.wa.eff.rec <- ca.or.wa.eff.by.area[,c("year","area.code",MONTH)]

#################################################################################
# Puget Sound 
#################################################################################
  #Adjust later years for chinook non-retention.
    #### make mapping between month number and number of days.
        M <- 1:12
        D <- c(31,28,31,30,31,30,31,31,30,31,30,31)
        REF <- data.frame(M,D)

  #Come up with a mapping between month and fraction of month that Chinook retention was possible for PUSO
        puso.ret.wide <- expand.grid(month=1:12,year=unique(puso.retention$Year))
        A <-matrix(0,nrow(puso.ret.wide),length(grep("Area",colnames(puso.eff))))
        colnames(A) <- colnames(puso.eff)[grep("Area",colnames(puso.eff))]
        puso.ret.wide <- data.frame(cbind(puso.ret.wide,A))

        ALL <- NULL          
        for(i in 1:nrow(puso.retention)){
          area <- puso.retention$Area[i]
          COL  <- grep(area,colnames(puso.ret.wide))
          M    <- puso.retention$month.open[i] : puso.retention$month.close[i]
          M.frac <- NULL
          if(length(M) == 1 ){
                M.frac[1]  <-   (puso.retention$day.close[i] - puso.retention$day.open[i] + 1) / REF$D[REF$M==M[1]]
          }
          if(length(M) > 1 ){
                M.frac[1]  <-   (REF$D[REF$M==M[1]] - puso.retention$day.open[i] + 1) / REF$D[REF$M==M[1]]
                if(length(M)>2){M.frac[2:(length(M)-1)] <- 1}
                M.frac[length(M)]  <-   (puso.retention$day.close[i]) / REF$D[REF$M==M[length(M)]]
          }
          M.frac[M.frac > 1] <- 1
          
          B <- puso.ret.wide %>% filter(year == puso.retention$Year[i],month >= M[1], month <= M[length(M)])
          B[,COL] <- M.frac

          ALL <- rbind(ALL,B)
        }
        
      ALL <- as.data.frame(ALL)
      puso.ret.wide <- ALL %>% group_by(month,year) %>% summarise(Area.5=sum(Area.5),Area.6=sum(Area.6),Area.7=sum(Area.7),Area.8=sum(Area.8),
                        Area.8.1=sum(Area.8.1),Area.8.2=sum(Area.8.2),Area.9=sum(Area.9),
                        Area.10=sum(Area.10),Area.11=sum(Area.11),Area.12=sum(Area.12),Area.13=sum(Area.13)) %>%
                        as.data.frame()
      puso.ret.wide[ ,3:ncol(puso.ret.wide)][puso.ret.wide[ ,3:ncol(puso.ret.wide)] >1 ] <- 1

  #################    
  ## Deal with the effort data.
  #################        
  puso.eff[,4:14] <- puso.eff[,4:14] * puso.eff$adjust
      
      #3 Combine times of chinook retention
  puso.eff <- puso.eff %>% group_by(Year,Month) %>% 
              summarise(Area.5=sum(Area.5),Area.6=sum(Area.6),Area.7=sum(Area.7),Area.8=sum(Area.8),
                        Area.8.1=sum(Area.8.1),Area.8.2=sum(Area.8.2),Area.9=sum(Area.9),
                        Area.10=sum(Area.10),Area.11=sum(Area.11),Area.12=sum(Area.12),Area.13=sum(Area.13)) %>%
              as.data.frame()  
      
  puso.ret.wide <- merge(puso.eff[,c("Year","Month")],puso.ret.wide,by.x=c("Year","Month"),by.y=c("year","month"),all=T)   
  puso.ret.wide[is.na(puso.ret.wide)==T]  <- 1
  puso.eff      <- merge(puso.ret.wide[,c("Year","Month")],puso.eff,by=c("Year","Month"),all=T)   

  puso.eff[,3:ncol(puso.eff)] <- puso.eff[,3:ncol(puso.eff)] * puso.ret.wide[,3:ncol(puso.eff)]  
    #### DROP AREA 5 from the EFFORT STATISTICS IF APPROPRIATE
    # puso.eff.area5 <- puso.eff %>% dplyr::select(Year,Month,Area.5)
    # puso.eff <- puso.eff %>% dplyr::select(-Area.5)
    # 
    # puso.eff.area5$total.effort <- puso.eff.area5$Area.5
  
  puso.eff<-  puso.eff %>% mutate(total.effort.out = Area.5+Area.6+Area.7,
                                  total.effort.in=Area.8+Area.8.1+Area.8.2+Area.9+Area.10+Area.11+Area.12+Area.13,
                                  total.effort = total.effort.in + total.effort.out)

  # temp <- aggregate(puso.eff$total.effort,by=list(year=puso.eff$Year,month=puso.eff$Month),sum)
  # temp <- temp[order(temp$year,temp$month),]
  
  puso.eff.wide <- data.frame(port="All Puget Sound",year=YEARS.RECOVER)
  puso.eff.wide <- cbind(puso.eff.wide,matrix(0,length(YEARS.RECOVER),12))
  
  # This section is for the original 17 area model
  puso.eff.wide <- dcast(puso.eff[,c("Year","Month","total.effort")],Year~Month)
  puso.eff.wide <- data.frame(port="All Puget Sound",puso.eff.wide)
  colnames(puso.eff.wide)[3:14] <- ALL.MONTH
  
  puso.eff.wide$effort.type <- "angler.trip"
  puso.eff.wide$Notes <- NA
  puso.eff.wide$area.code <- "PUSO"
  
    if(loc_18 == "TRUE" | loc_18 == "TWO_OR" | loc_18 =="NCA_SOR_PUSO"){ # This section is for the 18 area model
      puso.eff.in.wide <- dcast(puso.eff[,c("Year","Month","total.effort.in")],Year~Month)
      puso.eff.in.wide <- data.frame(port="All Puget Sound",puso.eff.in.wide)
      colnames(puso.eff.in.wide)[3:14] <- ALL.MONTH
      puso.eff.in.wide$area.code <- "PUSO"
    
      puso.eff.out.wide <- dcast(puso.eff[,c("Year","Month","total.effort.out")],Year~Month)
      puso.eff.out.wide <- data.frame(port="All Puget Sound",puso.eff.out.wide)
      colnames(puso.eff.out.wide)[3:14] <- ALL.MONTH
      puso.eff.out.wide$area.code <- "PUSO_out"
    
      puso.eff.wide <- rbind(puso.eff.in.wide, puso.eff.out.wide)
    }
  
  # This section is shared for all models

  if(MONTH.STRUCTURE=="FOUR"){
    puso.eff.wide$month.winter.2  <- rowSums(puso.eff.wide[,WINTER.MONTH[1:3]])
    puso.eff.wide$month.winter.1  <- rowSums(puso.eff.wide[,WINTER.MONTH[4:5]])
    puso.eff.wide$month.spring    <- rowSums(puso.eff.wide[,SPRING.MONTH])
    puso.eff.wide$month.summer    <- rowSums(puso.eff.wide[,SUMMER.MONTH])
    puso.eff.wide$month.fall      <- rowSums(puso.eff.wide[,FALL.MONTH])
  }
  if(MONTH.STRUCTURE=="SPRING"){
    puso.eff.wide$month.winter.2  <- rowSums(puso.eff.wide[,WINTER.MONTH[1:2]])
    puso.eff.wide$month.winter.1  <- rowSums(puso.eff.wide[,WINTER.MONTH[3:4]])
    puso.eff.wide$month.spring    <- rowSums(puso.eff.wide[,SPRING.MONTH])
    puso.eff.wide$month.summer    <- rowSums(puso.eff.wide[,SUMMER.MONTH])
    puso.eff.wide$month.fall      <- rowSums(puso.eff.wide[,FALL.MONTH])
  }
  if(MONTH.STRUCTURE=="FRAM"){
    puso.eff.wide$month.winter.2  <- (puso.eff.wide[,WINTER.MONTH[1]])
    puso.eff.wide$month.winter.1  <- rowSums(puso.eff.wide[,WINTER.MONTH[2:4]])
    puso.eff.wide$month.spring    <- rowSums(puso.eff.wide[,SPRING.MONTH])
    puso.eff.wide$month.summer    <- rowSums(puso.eff.wide[,SUMMER.MONTH])
    puso.eff.wide$month.fall      <- rowSums(puso.eff.wide[,FALL.MONTH])
  }
  
  puso.eff.wide$year.wint.2  <- puso.eff.wide$Year-1
  
  temp <- puso.eff.wide[,c("year.wint.2", "area.code","month.winter.2") ]
  puso.eff.wide <-  puso.eff.wide %>% dplyr::select(-year.wint.2,-month.winter.2)
  puso.eff.wide <- merge(puso.eff.wide,temp,by.x=c("Year","area.code"),by.y=c("year.wint.2" ,"area.code"),all=T)
  puso.eff.wide$month.winter <- puso.eff.wide$month.winter.2 + puso.eff.wide$month.winter.1
  
  puso.eff.rec <- puso.eff.wide[,c("Year","area.code",MONTH)]
  puso.eff.rec <- puso.eff.rec %>% dplyr::rename(year=Year)

    ##3 puso.eff.area5.wide
# 
#   puso.eff.area5.wide <- data.frame(port="WAC",year=YEARS.RECOVER)
#   puso.eff.area5.wide <- cbind(puso.eff.area5.wide,matrix(0,length(YEARS.RECOVER),12))
#   
#   puso.eff.area5.wide <- dcast(puso.eff.area5[,c("Year","Month","total.effort")],Year~Month)
#   puso.eff.area5.wide <- data.frame(port="All Puget Sound",puso.eff.area5.wide)
#   colnames(puso.eff.area5.wide)[3:14] <- ALL.MONTH
#   
#   puso.eff.area5.wide$effort.type <- "angler.trip"
#   puso.eff.area5.wide$Notes <- NA
#   puso.eff.area5.wide$area.code <- "WAC"
#   
#   puso.eff.area5.wide$month.winter.2  <- rowSums(puso.eff.area5.wide[,WINTER.MONTH[4:5]])
#   puso.eff.area5.wide$month.winter.1  <- rowSums(puso.eff.area5.wide[,WINTER.MONTH[1:3]])
#   puso.eff.area5.wide$month.spring    <- rowSums(puso.eff.area5.wide[,SPRING.MONTH])
#   puso.eff.area5.wide$month.summer    <- rowSums(puso.eff.area5.wide[,SUMMER.MONTH])
#   puso.eff.area5.wide$month.fall      <- rowSums(puso.eff.area5.wide[,FALL.MONTH])
#   
#   puso.eff.area5.wide$year.wint.2  <- puso.eff.area5.wide$Year+1
#   
#   temp <- puso.eff.area5.wide[,c("year.wint.2", "area.code","month.winter.2") ]
#   puso.eff.area5.wide <-  puso.eff.area5.wide %>% dplyr::select(-year.wint.2,-month.winter.2)
#   puso.eff.area5.wide <- merge(puso.eff.area5.wide,temp,by.x=c("Year","area.code"),by.y=c("year.wint.2" ,"area.code"),all=T)
#   puso.eff.area5.wide$month.winter <- puso.eff.area5.wide$month.winter.2 + puso.eff.area5.wide$month.winter.1
#   
#   puso.eff.area5.rec <- puso.eff.area5.wide[,c("Year","area.code",MONTH)]
#   puso.eff.area5.rec <- puso.eff.area5.rec %>% dplyr::rename(year=Year)
#   
    # puso.eff.by.area <- aggregate(puso.eff.wide[,ALL.MONTH],
  #                               by=list(year=puso.eff.wide$year,area.code=puso.eff.wide$area.code),sum)
  
  #update WAC effort with puso.eff.area5 effort
  # temp.WAC <- ca.or.wa.eff.rec %>% filter(area.code == "WAC")%>% filter(year %in% YEARS.RECOVER) %>% arrange(year) 
  # temp.puso.eff.area5 <- puso.eff.area5.rec %>% filter(year %in% YEARS.RECOVER) %>% arrange(year)
  # 
  # if(nrow(temp.WAC)==nrow(temp.puso.eff.area5)){
  #   temp.WAC.rec <- cbind(temp.WAC[,1:2],temp.WAC[,3:ncol(temp.WAC)] + temp.puso.eff.area5[,3:ncol(temp.puso.eff.area5)]) %>% as.data.frame()
  # }else{
  #   print(rep("STOP",3))
  # }
  # 
  # ca.or.wa.eff.rec <- ca.or.wa.eff.rec %>% filter(area.code != "WAC")
  # ca.or.wa.eff.rec <- rbind(ca.or.wa.eff.rec,temp.WAC.rec) %>% arrange(year) %>% as.data.frame() 
  
  #################################################################################
  ### SGEO - NOTE THE JOHNSTONE DATA IS EXCLUDED FROM THIS BECAUSE WE DON"T HAVE EARLY DATA FROM JOHNSTONE
  #################################################################################

  bc.trim <- bc.eff %>% filter(DISPOSITION == "Effort")
  
  john.areas <- c("PFMA 11","PFMA 111","PFMA 12")
  sgeo.areas <- c("PFMA 13","PFMA 14","PFMA 15","PFMA 16","PFMA 17",
                  "PFMA 18","PFMA 19","PFMA 20","PFMA 28","PFMA 29")
  swvi.areas <- c("PFMA 21","PFMA 22","PFMA 23","PFMA 24","PFMA 121","PFMA 123","PFMA 124")
  nwvi.areas <- c("PFMA 25","PFMA 26","PFMA 27","PFMA 125","PFMA 126","PFMA 127")
  
  bc.trim <- bc.trim %>% mutate(area.code = "", area.code=ifelse(PFMA %in% john.areas,"CBC",area.code)) %>%
                      mutate(area.code=ifelse(PFMA %in% sgeo.areas,"SGEO",area.code)) %>%
                      mutate(area.code=ifelse(PFMA %in% swvi.areas,"SWVI",area.code)) %>%
                      mutate(area.code=ifelse(PFMA %in% nwvi.areas,"NWVI",area.code))
  
  bc.trim <- bc.trim %>% group_by(year=YEAR,month.numb,area.code) %>% dplyr::summarise(total.effort=sum(Estimate))
  
  
  if(MONTH.STRUCTURE=="FOUR"){
    bc.trim <- bc.trim %>% mutate(season="", season = ifelse(month.numb<=3, "month.winter.2",""),
                                           season = ifelse(month.numb>=11 & month.numb<=12, "month.winter.1",season),
                                           season = ifelse(month.numb>=4 & month.numb<=5, "month.spring",season),
                                           season = ifelse(month.numb>=6 & month.numb<=7, "month.summer",season),
                                           season = ifelse(month.numb>=8 & month.numb<=10, "month.fall",season))
  }
  if(MONTH.STRUCTURE=="SPRING"){
    bc.trim <- bc.trim %>% mutate(season="", season = ifelse(month.numb<=2, "month.winter.2",""),
                                  season = ifelse(month.numb>=11 & month.numb<=12, "month.winter.1",season),
                                  season = ifelse(month.numb>=3 & month.numb<=5, "month.spring",season),
                                  season = ifelse(month.numb>=6 & month.numb<=7, "month.summer",season),
                                  season = ifelse(month.numb>=8 & month.numb<=10, "month.fall",season))
  }
  if(MONTH.STRUCTURE=="FRAM"){
    bc.trim <- bc.trim %>% mutate(season="",season = ifelse(month.numb<=1, "month.winter.2",""),
                                           season = ifelse(month.numb>=10 & month.numb<=12, "month.winter.1",season),
                                           season = ifelse(month.numb>=4 & month.numb<=5, "month.spring",season),
                                           season = ifelse(month.numb>=6 & month.numb<=7, "month.summer",season),
                                           season = ifelse(month.numb>=8 & month.numb<=10, "month.fall",season))
  }
  
  bc.trim <- bc.trim %>% mutate(year.mod =year,year.mod=ifelse(season=="month.winter.2",year.mod-1,year.mod),
                                          season=ifelse(season=="month.winter.1","month.winter",season),
                                          season=ifelse(season=="month.winter.2","month.winter",season))
  
  bc.mod <- bc.trim %>% group_by(year,season,area.code) %>% dplyr::summarise(total.eff = sum(total.effort))
  
  
  bc.eff.rec <- full_join(expand.grid(area.code=c("CBC","SGEO","NWVI","SWVI"),year=YEARS.RECOVER),
                          dcast(bc.mod,year+area.code~season,value.var=c("total.eff")))
  bc.eff.rec <- bc.eff.rec %>% dplyr::select(year,area.code,month.winter,month.spring,month.summer,month.fall) 
  bc.eff.rec[is.na(bc.eff.rec)==T] <- 0
  
  
  ### iREC DATA
  
        ### Process the iREC data from Canada. This is a different data type and form than the other recreational data.
        ### it needs additional processing.
        
        # extract only effort information.
        can.irec.mod <- can.irec.eff %>% filter(ITEM_GROUP=="EFFORT") %>% dplyr::select(YEAR,MONTH,AREA,ITEM,ESTIMATE)
        # Combine adult and juvenile effort
        can.irec.mod <- can.irec.mod %>% group_by(YEAR,MONTH,AREA) %>% summarize(effort = sum(ESTIMATE))
        can.irec.mod <- left_join(can.irec.mod,can.irec.mapping) %>% filter(!REGION=="RIVER") %>% 
                            group_by(YEAR,MONTH,REGION) %>% summarize(tot.effort = sum(effort))

        if(MONTH.STRUCTURE=="FOUR"){
        can.irec.mod<- can.irec.mod %>% mutate(season = ifelse(MONTH<=3, "month.winter.2",""),
                                               season = ifelse(MONTH>=11 & MONTH<=12, "month.winter.1",season),
                                               season = ifelse(MONTH>=4 & MONTH<=5, "month.spring",season),
                                               season = ifelse(MONTH>=6 & MONTH<=7, "month.summer",season),
                                               season = ifelse(MONTH>=8 & MONTH<=10, "month.fall",season))
        }
        if(MONTH.STRUCTURE=="SPRING"){
          can.irec.mod<- can.irec.mod %>% mutate(season = ifelse(MONTH<=2, "month.winter.2",""),
                                                 season = ifelse(MONTH>=11 & MONTH<=12, "month.winter.1",season),
                                                 season = ifelse(MONTH>=3 & MONTH<=5, "month.spring",season),
                                                 season = ifelse(MONTH>=6 & MONTH<=7, "month.summer",season),
                                                 season = ifelse(MONTH>=8 & MONTH<=10, "month.fall",season))
        }
        if(MONTH.STRUCTURE=="FRAM"){
        can.irec.mod<- can.irec.mod %>% mutate(season = ifelse(MONTH<=1, "month.winter.2",""),
                                                 season = ifelse(MONTH>=10 & MONTH<=12, "month.winter.1",season),
                                                 season = ifelse(MONTH>=4 & MONTH<=5, "month.spring",season),
                                                 season = ifelse(MONTH>=6 & MONTH<=7, "month.summer",season),
                                                 season = ifelse(MONTH>=8 & MONTH<=10, "month.fall",season))
        }
          
        can.irec.mod <- can.irec.mod %>% mutate(year.mod =YEAR,year.mod=ifelse(season=="month.winter.2",year.mod-1,year.mod),
                                                season=ifelse(season=="month.winter.1","month.winter",season),
                                                season=ifelse(season=="month.winter.2","month.winter",season))
        can.irec.mod <- can.irec.mod %>% rename(area.code=REGION)
        can.irec.mod <- can.irec.mod %>% group_by(year.mod,area.code,season) %>% summarize(effort=sum(tot.effort))
        
        can.irec.mod <- dcast(can.irec.mod,year.mod+area.code~season,value.var="effort",sum)
        can.irec.eff.fin <- left_join(data.frame(expand.grid(year.mod=YEARS.RECOVER,area.code=LOCATIONS$location.name)),can.irec.mod)
        can.irec.eff.fin <- can.irec.eff.fin %>% rename(year=year.mod)
        can.irec.eff.fin <- can.irec.eff.fin[,c("year","area.code",MONTH)]
        can.irec.eff.fin[is.na(can.irec.eff.fin)==T] <- 0
        
        effort.can.irec <- can.irec.eff.fin
   ###################################################################################
  ###################################################################################
  # Combine the files and trim to match the Years span specified by the Master File

  #effort <- rbind(ca.or.wa.eff.rec,puso.eff.rec)
  effort <- rbind(ca.or.wa.eff.rec,puso.eff.rec,bc.eff.rec)
  #effort <- rbind(ca.or.wa.eff.rec,puso.eff.rec,sgeo.eff.rec,ak.eff.rec)
  
  temp<- expand.grid(year=YEARS.RECOVER,area.code=LOCATIONS$location.name)
  
  effort <- merge(effort,data.frame(year=YEARS.RECOVER))
  effort <- merge(effort,temp,all=T)
  
  effort$area.numb <- LOCATIONS$location.number[match(effort$area.code,LOCATIONS$location.name)]
  effort <- effort[order(effort$area.numb,effort$year),]
  effort[is.na(effort == T)]<- 0 

  effort.can.irec$area.numb <- LOCATIONS$location.number[match(effort.can.irec$area.code,LOCATIONS$location.name)]
  
effort.rec <- effort


