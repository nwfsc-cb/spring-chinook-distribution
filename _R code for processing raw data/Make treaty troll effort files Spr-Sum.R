### SCRIPT FOR CREATING FILES THAT CAN BE READ IN BY STAN

# 
#base.dir<-getwd()

ak.eff <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/Effort Data/effort.data.ak.csv",sep=""))
treaty.eff.old <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/Effort Data/effort.indian.wa.csv",sep=""))
treaty.eff <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/Effort Data/effort.treaty.us.csv",sep=""))

if(loc_18 == "NCA_SOR_PUSO" | loc_18 =="TWO_OR"){
  treaty.eff <- treaty.eff %>% mutate(area.code=region.puso.out)
  treaty.eff$area.code <- as.character(treaty.eff$area.code)
  treaty.eff <- treaty.eff %>% mutate(area.code=replace(area.code,area.code=="PUSO.out","PUSO_out"))
}

treaty.eff <- left_join(treaty.eff,data.frame(month.name=names(ak.eff)[grep("month",names(ak.eff))],month=1:12))
treaty.eff.by.area <- full_join(treaty.eff,expand.grid(year=YEARS.RECOVER,month.name=sort(unique(treaty.eff$month.name))))
treaty.eff.by.area <- treaty.eff %>% group_by(year,month.name,area.code) %>% summarise(count = sum(unique.tickets))


treaty.eff.trim <- dcast(treaty.eff.by.area, year+area.code~month.name,value.var = "count")
treaty.eff.trim[is.na(treaty.eff.trim)==T] <- 0

 
# Old way of doing it... (pre-August 2019) 
# 
treaty.eff.old$area.code[treaty.eff.old$Area =="4B"] <- "PUSO_out"
treaty.eff.old$area.code[treaty.eff.old$Area !="4B"] <- "WAC"

treaty.eff.by.area.old = aggregate(treaty.eff.old[,3:10],by=list(Year=treaty.eff.old$Year,area.code=treaty.eff.old$area.code),sum)
treaty.eff.by.area.old$jan <- 0.25 * treaty.eff.by.area.old$jan.apr
treaty.eff.by.area.old$feb <- 0.25 * treaty.eff.by.area.old$jan.apr
treaty.eff.by.area.old$mar <- 0.25 * treaty.eff.by.area.old$jan.apr
treaty.eff.by.area.old$apr <- 0.25 * treaty.eff.by.area.old$jan.apr
treaty.eff.by.area.old$nov <- 0.25 * treaty.eff.by.area.old$nov.dec
treaty.eff.by.area.old$dec <- 0.25 * treaty.eff.by.area.old$nov.dec

treaty.eff.trim.old <- treaty.eff.by.area.old[,c("Year","area.code","jan","feb","mar","apr","May","June","July","Aug.","Sept.","Oct.","nov","dec")]
colnames(treaty.eff.trim.old)[3:14] <- colnames(ak.eff)[3:14]
colnames(treaty.eff.trim.old)[1] <- "year"

# use the old way only for the 1970s data
treaty.eff.trim.old <- treaty.eff.trim.old %>% filter(year<1980)

treaty.eff.trim <- rbind(treaty.eff.trim.old, treaty.eff.trim)

# if(MONTH.STRUCTURE == "EIGHT"){

#   treaty.eff.trim$month.wint <- rowSums(treaty.eff.trim[,WINT.MONTH])
#   treaty.eff.fin <- treaty.eff.trim[,c("year","area.code",MONTH)]
# 
#   ###################################################################################
#   # Combine the files and trim to match the Years span specified by the Master File
#   effort.treaty <- treaty.eff.fin
#   temp<- expand.grid(year=YEARS.RECOVER,area.code=LOCATIONS$location.name)
# 
#   effort.treaty <- merge(effort.treaty,data.frame(year=YEARS.RECOVER))
#   effort.treaty <- merge(effort.treaty,temp,all=T)
#   
#   effort.treaty$area.numb <- LOCATIONS$location.number[match(effort.treaty$area.code,LOCATIONS$location.name)]
#   effort.treaty <- effort.treaty[order(effort.treaty$area.numb,effort.treaty$year),]
#   effort.treaty[is.na(effort.treaty == T)]<- 0 
# }
if(MONTH.STRUCTURE == "FOUR"){
  treaty.eff.trim$month.winter.2 <- rowSums(treaty.eff.trim[,WINTER.MONTH[1:3]],na.rm = T)
  treaty.eff.trim$month.winter.1 <- rowSums(treaty.eff.trim[,WINTER.MONTH[4:5]],na.rm = T)
  treaty.eff.trim$month.spring   <- rowSums(treaty.eff.trim[,SPRING.MONTH],na.rm = T)
  treaty.eff.trim$month.summer   <- rowSums(treaty.eff.trim[,SUMMER.MONTH],na.rm = T)
  treaty.eff.trim$month.fall     <- rowSums(treaty.eff.trim[,FALL.MONTH],na.rm = T)
}
if(MONTH.STRUCTURE == "FRAM"){
  treaty.eff.trim$month.winter.2 <- treaty.eff.trim[,WINTER.MONTH[1]]
  treaty.eff.trim$month.winter.1 <- rowSums(treaty.eff.trim[,WINTER.MONTH[2:4]],na.rm = T)
  treaty.eff.trim$month.spring   <- rowSums(treaty.eff.trim[,SPRING.MONTH],na.rm = T)
  treaty.eff.trim$month.summer   <- rowSums(treaty.eff.trim[,SUMMER.MONTH],na.rm = T)
  treaty.eff.trim$month.fall     <- rowSums(treaty.eff.trim[,FALL.MONTH],na.rm = T)
}

  treaty.eff.trim$year.wint.2  <- treaty.eff.trim$year-1
  
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






