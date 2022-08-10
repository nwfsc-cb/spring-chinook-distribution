### SCRIPT FOR CREATING FILES THAT CAN BE READ IN BY STAN
library(tidyverse)
# ASHOP Effort data first
      #ashop.eff was created in "GitHub/Chinook_Bycatch/scripts/ashop" original .csv is stored in "GitHub/Chinook_Bycatch/ASHOP/final_datasets/ashop.total.effort"

#base.dir<-getwd()

ashop.eff <- read.csv(paste(base.dir,"/spring-chinook-distribution/Processed Data/Effort Data/ashop_total_effort.csv",sep=""))
ashop.sample.frac <- read.csv(paste(base.dir,"/spring-chinook-distribution/Processed Data/Effort Data/ashop_sample_fraction.csv",sep=""))

#make my own locations file with options to combine SOR and COR, and SOR and NCA  
locations_gs <- read.csv(paste(base.dir,"/spring-chinook-distribution/Processed Data/locations_gs.csv",sep=""))

if(loc_18 == "NCA_SOR_PUSO"){
  ashop.eff$region <- locations_gs$area.code.NCA_COR_NOR[match(ashop.eff$region,locations_gs$area.code)]### INSERT CATEGORY ###)]
}

if(loc_18 == "TWO_OR" | loc_18 =="_two_OR_PUSO_AK" ){
  ashop.eff$region <- locations_gs$area.code.SOR.NOR[match(ashop.eff$region,locations_gs$area.code)]### INSERT CATEGORY ###)]
}

# change month from number to month.01 etc
ashop.eff <- ashop.eff %>%
  mutate(month.use = recode(Month, '1' = "month.01",
                            '2' = "month.02",
                            '3' = "month.03",
                            '4' = "month.04",
                            '5' = "month.05",
                            '6'= "month.06",
                            '7' = "month.07",
                            '8' = "month.08",
                            '9' = "month.09",
                            '10' = "month.10",
                            '11' = "month.11",
                            '12' = "month.12")) %>%
  mutate(effort.boat.days = unobserved_boat_days + observed_boat_days) %>%
  rename(year=Year) %>%
  group_by(year,month.use,region) %>% summarise(effort.boat.days=sum(effort.boat.days)) %>%
  dplyr::select(c("year", "region", "month.use", "effort.boat.days")) %>%
  spread(month.use, effort.boat.days) %>%
  replace(., is.na(.), 0) %>%
  mutate(month.02 = 0) %>%# Add in because there are no effort observations for february.
  as.data.frame()

#spread creates a lot of NAs change them to zero
#ashop.eff$month.02=0# no fishing in febraury so had to add column. 
#can do this if we want to use a different grouping than how it is set: 

#may not need this bc I changed it? 
#ALL.MONTH <- c("month.01","month.02","month.03","month.04","month.05","month.06","month.07","month.08","month.09","month.10","month.11","month.12")

#arrange month structure
if(MONTH.STRUCTURE=="FOUR"){
 ashop.eff$month.winter.2 <- rowSums(ashop.eff[,WINTER.MONTH[1:3]])
 ashop.eff$month.winter.1 <- rowSums(ashop.eff[,WINTER.MONTH[4:5]])
 ashop.eff$month.spring <- rowSums(ashop.eff[,SPRING.MONTH])
 ashop.eff$month.summer <- rowSums(ashop.eff[,SUMMER.MONTH])
 ashop.eff$month.fall   <- rowSums(ashop.eff[,FALL.MONTH])
}

if(MONTH.STRUCTURE=="FRAM"){
  ashop.eff$month.winter.2 <- ashop.eff[,WINTER.MONTH[1]]
  ashop.eff$month.winter.1 <- rowSums(ashop.eff[,WINTER.MONTH[2:4]]) 
  ashop.eff$month.spring <- rowSums(ashop.eff[,SPRING.MONTH])
  ashop.eff$month.summer <- rowSums(ashop.eff[,SUMMER.MONTH])
  ashop.eff$month.fall   <- rowSums(ashop.eff[,FALL.MONTH])
}
if(MONTH.STRUCTURE=="SPRING"){
  ashop.eff$month.winter.2 <- rowSums(ashop.eff[,WINTER.MONTH[1:2]],na.rm=T)
  ashop.eff$month.winter.1 <- rowSums(ashop.eff[,WINTER.MONTH[3:4]]) 
  ashop.eff$month.spring <- rowSums(ashop.eff[,SPRING.MONTH])
  ashop.eff$month.summer <- rowSums(ashop.eff[,SUMMER.MONTH])
  ashop.eff$month.fall   <- rowSums(ashop.eff[,FALL.MONTH])
}

ashop.eff$year.wint.2  <- ashop.eff$year-1

temp <- ashop.eff[,c("year.wint.2", "region","month.winter.2") ]
ashop.eff <- ashop.eff %>% dplyr::select(-year.wint.2,-month.winter.2)
ashop.eff <- merge(ashop.eff,temp,by.x=c("year","region"),by.y=c("year.wint.2" ,"region"),all=T)
ashop.eff$month.winter <- ashop.eff$month.winter.2 + ashop.eff$month.winter.1
ashop.eff<- ashop.eff %>% replace(., is.na(.), 0) 

ashop.eff <- ashop.eff[,c("year","region", MONTH)]
colnames(ashop.eff)[2] <- "area.code"

###################################################################################
###################################################################################
# trim files to match the Years span specified by the Master File
temp<- expand.grid(year=YEARS.RECOVER,area.code=LOCATIONS$location.name)

ashop.eff1 <- merge(ashop.eff,data.frame(year=YEARS.RECOVER))
ashop.eff2 <- merge(ashop.eff1,temp,all=T)

ashop.eff2$area.numb <- LOCATIONS$location.number[match(ashop.eff2$area.code,LOCATIONS$location.name)]
#ashop.eff2 <- ashop.eff2[order(ashop.eff2$area.numb, ashop.eff2$year),]

ashop.eff2[is.na(ashop.eff2 == T)]<- 0 

# get rid of effort for ashop for TRIM.ASHOP and earlier due to unreliable sampling 
ashop.eff2 <- ashop.eff2 %>% mutate(month.winter = ifelse(year<=TRIM.ASHOP,0,month.winter ),
                                    month.spring = ifelse(year<=TRIM.ASHOP,0,month.spring ),
                                    month.summer = ifelse(year<=TRIM.ASHOP,0,month.summer ),
                                    month.fall   = ifelse(year<=TRIM.ASHOP,0,month.fall ))
########### final ashop effort file ###############
effort.ashop <- ashop.eff2 %>% arrange(year,area.numb)

############################################################################################################################################
################                                     Sample Fraction file for ASHOP                                       ##################
############################################################################################################################################

#ashop.sample.frac was created in "GitHub/Chinook_Bycatch/scripts/ashop" original .csv is stored in "GitHub/Chinook_Bycatch/ASHOP/final_datasets/ashop.total.effort"

if(loc_18 == "NCA_SOR_PUSO"){
  ashop.sample.frac$region2 <- locations_gs$area.code.NCA_COR_NOR[match(ashop.sample.frac$region,locations_gs$area.code)]### INSERT CATEGORY ###)]
}
if(loc_18 == "TWO_OR" | loc_18 =="_two_OR_PUSO_AK" ){
  ashop.sample.frac$region2 <- locations_gs$area.code.SOR.NOR[match(ashop.sample.frac$region,locations_gs$area.code)]### INSERT CATEGORY ###)]
}


#change month from number to month.01 etc
ashop.sample.frac <- ashop.sample.frac %>%
  mutate(month.use = recode(Month, '1' = "month.01",
                            '2' = "month.02",
                            '3' = "month.03",
                            '4' = "month.04",
                            '5' = "month.05",
                            '6'= "month.06",
                            '7' = "month.07",
                            '8' = "month.08",
                            '9' = "month.09",
                            '10' = "month.10",
                            '11' = "month.11",
                            '12' = "month.12")) %>%
  group_by(region2,year,month.use) %>% summarize(weighted.fraction=sum(fraction * N/sum(N))) %>%
  dplyr::select(year,region=region2,month.use,fraction=weighted.fraction) %>%
  spread(month.use, fraction) %>%
  replace(., is.na(.), 0) %>%
  mutate(month.01 = 0) %>%
  mutate(month.02 = 0) %>%
  as.data.frame()
#spread creates a lot of NAs change them to zero
#ashop.eff$month.02=0# no fishing in febraury so had to add column. 

ashop.sample.frac[ashop.sample.frac==0]<- NA

#arrange month structure
if(MONTH.STRUCTURE=="FOUR"){
  ashop.sample.frac$month.winter.2 <- apply(ashop.sample.frac[,WINTER.MONTH[1:3]],1,median,na.rm=T) 
  ashop.sample.frac$month.winter.1 <-   apply(ashop.sample.frac[,WINTER.MONTH[4:5]],1,median,na.rm=T) 
  ashop.sample.frac$month.spring <- apply(ashop.sample.frac[,SPRING.MONTH],1,median,na.rm=T)
  ashop.sample.frac$month.summer <- apply(ashop.sample.frac[,SUMMER.MONTH],1,median,na.rm=T)
  ashop.sample.frac$month.fall   <- apply(ashop.sample.frac[,FALL.MONTH],1,median,na.rm=T)
}

if(MONTH.STRUCTURE=="FRAM"){
  ashop.sample.frac$month.winter.2 <- ashop.sample.frac[,WINTER.MONTH[1]]
  ashop.sample.frac$month.winter.1 <- apply(ashop.sample.frac[,WINTER.MONTH[2:4]],1,median,na.rm=T) 
  ashop.sample.frac$month.spring   <- apply(ashop.sample.frac[,SPRING.MONTH],1,median,na.rm=T)
  ashop.sample.frac$month.summer   <- apply(ashop.sample.frac[,SUMMER.MONTH],1,median,na.rm=T)
  ashop.sample.frac$month.fall     <- apply(ashop.sample.frac[,FALL.MONTH],1,median,na.rm=T)
}
if(MONTH.STRUCTURE=="SPRING"){
  ashop.sample.frac$month.winter.2 <- apply(ashop.sample.frac[,WINTER.MONTH[1:2]],1,median,na.rm=T) 
  ashop.sample.frac$month.winter.1 <- apply(ashop.sample.frac[,WINTER.MONTH[3:4]],1,median,na.rm=T) 
  ashop.sample.frac$month.spring   <- apply(ashop.sample.frac[,SPRING.MONTH],1,median,na.rm=T)
  ashop.sample.frac$month.summer   <- apply(ashop.sample.frac[,SUMMER.MONTH],1,median,na.rm=T)
  ashop.sample.frac$month.fall     <- apply(ashop.sample.frac[,FALL.MONTH],1,median,na.rm=T)
}


ashop.sample.frac$year.wint.2  <- ashop.sample.frac$year-1

temp <- ashop.sample.frac[,c("year.wint.2", "region","month.winter.2") ]
ashop.sample.frac <- ashop.sample.frac %>% dplyr::select(-year.wint.2,-month.winter.2)
ashop.sample.frac <- merge(ashop.sample.frac,temp,by.x=c("year","region"),by.y=c("year.wint.2" ,"region"),all=T)
ashop.sample.frac$month.winter <- apply(ashop.sample.frac[,c("month.winter.1","month.winter.2")],1,median,na.rm=T)
ashop.sample.frac<- ashop.sample.frac %>% replace(., is.na(.), 0) 

ashop.sample.frac <- ashop.sample.frac[,c("year","region", MONTH)]
colnames(ashop.sample.frac)[2] <- "area.code"

###################################################################################
###################################################################################
# trim files to match the Years span specified by the Master File
temp<- expand.grid(year=YEARS.RECOVER,area.code=LOCATIONS$location.name)

ashop.sample.frac1 <- merge(ashop.sample.frac,data.frame(year=YEARS.RECOVER))
ashop.sample.frac2 <- merge(ashop.sample.frac1,temp,all=T)

ashop.sample.frac2$area.numb <- LOCATIONS$location.number[match(ashop.sample.frac2$area.code,LOCATIONS$location.name)]
#ashop.eff2 <- ashop.eff2[order(ashop.eff2$area.numb, ashop.eff2$year),]

ashop.sample.frac2[is.na(ashop.sample.frac2 == T)]<- 0 

ashop.sample.frac2 <- ashop.sample.frac2 %>% mutate(month.winter = ifelse(year<=TRIM.ASHOP,0,month.winter ),
                                    month.spring = ifelse(year<=TRIM.ASHOP,0,month.spring ),
                                    month.summer = ifelse(year<=TRIM.ASHOP,0,month.summer ),
                                    month.fall   = ifelse(year<=TRIM.ASHOP,0,month.fall ))


########### final ashop sample fraction file ###############
ashop.sample.fraction<- ashop.sample.frac2
