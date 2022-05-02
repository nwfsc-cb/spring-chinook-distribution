### SCRIPT FOR CREATING FILES THAT CAN BE READ IN BY STAN

# Troll Effort data first
#base.dir<-getwd()

ak.eff <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/Effort Data/effort.data.ak.csv",sep=""))
treaty.eff <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/Effort Data/effort.indian.wa.csv",sep=""))
treaty.eff$area.code[treaty.eff$Area =="4B"] <- "WAC"
treaty.eff$area.code[treaty.eff$Area !="4B"] <- "WAC"

treaty.eff.by.area = aggregate(treaty.eff[,3:10],by=list(Year=treaty.eff$Year,area.code=treaty.eff$area.code),sum)
treaty.eff.by.area$jan <- 0.25 * treaty.eff.by.area$jan.apr
treaty.eff.by.area$feb <- 0.25 * treaty.eff.by.area$jan.apr
treaty.eff.by.area$mar <- 0.25 * treaty.eff.by.area$jan.apr
treaty.eff.by.area$apr <- 0.25 * treaty.eff.by.area$jan.apr
treaty.eff.by.area$nov <- 0.25 * treaty.eff.by.area$nov.dec
treaty.eff.by.area$dec <- 0.25 * treaty.eff.by.area$nov.dec

treaty.eff.trim <- treaty.eff.by.area[,c("Year","area.code","jan","feb","mar","apr","May","June","July","Aug.","Sept.","Oct.","nov","dec")]
colnames(treaty.eff.trim)[3:14] <- colnames(ak.eff)[3:14]
colnames(treaty.eff.trim)[1] <- "year"

if(MONTH.STRUCTURE == "EIGHT"){
  treaty.eff.trim$month.wint <- rowSums(treaty.eff.trim[,WINT.MONTH])
  treaty.eff.fin <- treaty.eff.trim[,c("year","area.code",MONTH)]

  ###################################################################################
  # Combine the files and trim to match the Years span specified by the Master File
  effort.treaty <- treaty.eff.fin
  temp<- expand.grid(year=YEARS.RECOVER,area.code=LOCATIONS$location.name)

  effort.treaty <- merge(effort.treaty,data.frame(year=YEARS.RECOVER))
  effort.treaty <- merge(effort.treaty,temp,all=T)
  
  effort.treaty$area.numb <- LOCATIONS$location.number[match(effort.treaty$area.code,LOCATIONS$location.name)]
  effort.treaty <- effort.treaty[order(effort.treaty$area.numb,effort.treaty$year),]
  effort.treaty[is.na(effort.treaty == T)]<- 0 
}
if(MONTH.STRUCTURE == "FOUR"){
  
  treaty.eff.trim$month.winter.2 <- rowSums(treaty.eff.trim[,WINTER.MONTH[4:5]])
  treaty.eff.trim$month.winter.1 <- rowSums(treaty.eff.trim[,WINTER.MONTH[1:3]])
  treaty.eff.trim$month.spring   <- rowSums(treaty.eff.trim[,SPRING.MONTH])
  treaty.eff.trim$month.summer   <- rowSums(treaty.eff.trim[,SUMMER.MONTH])
  treaty.eff.trim$month.fall     <- rowSums(treaty.eff.trim[,FALL.MONTH])
  
  treaty.eff.trim$year.wint.2  <- treaty.eff.trim$year+1
  
  temp <- treaty.eff.trim[,c("year.wint.2", "area.code","month.winter.2") ]
  treaty.eff.trim <-    treaty.eff.trim %>% dplyr::select(-year.wint.2,-month.winter.2)
  treaty.eff.trim <- merge( treaty.eff.trim,temp,by.x=c("year","area.code"),by.y=c("year.wint.2" ,"area.code"),all=T)
  treaty.eff.trim$month.winter <-   treaty.eff.trim$month.winter.2 +   treaty.eff.trim$month.winter.1
  
  treaty.eff.fin <- treaty.eff.trim[,c("year","area.code",MONTH)]
  
  ###################################################################################
  # Combine the files and trim to match the Years span specified by the Master File
  effort.treaty <- treaty.eff.fin
  temp<- expand.grid(year=YEARS.RECOVER,area.code=LOCATIONS$location.name)
  
  effort.treaty <- merge(effort.treaty,data.frame(year=YEARS.RECOVER))
  effort.treaty <- merge(effort.treaty,temp,all=T)
  
  effort.treaty$area.numb <- LOCATIONS$location.number[match(effort.treaty$area.code,LOCATIONS$location.name)]
  effort.treaty <- effort.treaty[order(effort.treaty$area.numb,effort.treaty$year),]
  effort.treaty[is.na(effort.treaty == T)]<- 0 
}






