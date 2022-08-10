### SCRIPT FOR CREATING FILES THAT CAN BE READ IN BY STAN
library(tidyverse)
# ASHOP Effort data first
#ashop.eff was created in "GitHub/Chinook_Bycatch/scripts/ashop" original .csv is stored in "GitHub/Chinook_Bycatch/ASHOP/final_datasets/ashop.total.effort"

#base.dir<-getwd()

#dont have data on this fishery until 1980 --> but when I match in dates it starts it at 1978 with 0's - is this ok? 

shoreside.eff <- read.csv(paste(base.dir,"/spring-chinook-distribution/Processed Data/Effort Data/shoreside_hake_effort.csv",sep=""))
# change month from number to month.01 etc

#make my own locations file with options to combine SOR and COR, and SOR and NCA  

locations_gs <- read.csv(paste(base.dir,"/spring-chinook-distribution/Processed Data/locations_gs.csv",sep=""))

if(loc_18 == "NCA_SOR_PUSO"){
  shoreside.eff$fishing_region <- locations_gs$area.code.NCA_COR_NOR[match(shoreside.eff$fishing_region,locations_gs$area.code)]### INSERT CATEGORY ###)]
}
if(loc_18 == "TWO_OR" | loc_18 == "_two_OR_PUSO_AK"){
  shoreside.eff$fishing_region <- locations_gs$area.code.SOR.NOR[match(shoreside.eff$fishing_region,locations_gs$area.code)]### INSERT CATEGORY ###)]
}

shoreside.eff <- shoreside.eff %>%
  mutate(month.use = recode(LANDING_MONTH, '1' = "month.01",
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
  mutate(effort.boat.days = sum_boat_days) %>%
  mutate(year = LANDING_YEAR) %>%
  mutate(region = fishing_region) %>%
  group_by(year,month.use,region) %>% summarise(effort.boat.days=sum(effort.boat.days)) %>%
  dplyr::select(c("year", "region", "month.use", "effort.boat.days")) %>%
  spread(month.use, effort.boat.days) %>%
  replace(., is.na(.), 0) %>%
  mutate(month.01 = 0) %>%
  as.data.frame()

#can do this if we want to use a different grouping than how it is set: ashop.eff$area.code <- locations_gs$area.code[match(ashop.eff$area.code,locations_gs$ ### INSERT CATEGORY ###)]
#may not need this bc I changed it? 
#ALL.MONTH <- c("month.01","month.02","month.03","month.04","month.05","month.06","month.07","month.08","month.09","month.10","month.11","month.12")

#arrange month structure
if(MONTH.STRUCTURE=="FOUR"){
 shoreside.eff$month.winter.2 <- rowSums(shoreside.eff[,WINTER.MONTH[1:3]])
 shoreside.eff$month.winter.1 <- rowSums(shoreside.eff[,WINTER.MONTH[4:5]])
 shoreside.eff$month.spring <- rowSums(shoreside.eff[,SPRING.MONTH])
 shoreside.eff$month.summer <- rowSums(shoreside.eff[,SUMMER.MONTH])
 shoreside.eff$month.fall   <- rowSums(shoreside.eff[,FALL.MONTH])
}

if(MONTH.STRUCTURE=="FRAM"){
  shoreside.eff$month.winter.2 <- shoreside.eff[,WINTER.MONTH[1]]
  shoreside.eff$month.winter.1 <- rowSums(shoreside.eff[,WINTER.MONTH[2:4]]) 
  shoreside.eff$month.spring <- rowSums(shoreside.eff[,SPRING.MONTH])
  shoreside.eff$month.summer <- rowSums(shoreside.eff[,SUMMER.MONTH])
  shoreside.eff$month.fall   <- rowSums(shoreside.eff[,FALL.MONTH])
}

if(MONTH.STRUCTURE=="SPRING"){
  shoreside.eff$month.winter.2 <- rowSums(shoreside.eff[,WINTER.MONTH[1:2]])
  shoreside.eff$month.winter.1 <- rowSums(shoreside.eff[,WINTER.MONTH[3:4]]) 
  shoreside.eff$month.spring <- rowSums(shoreside.eff[,SPRING.MONTH])
  shoreside.eff$month.summer <- rowSums(shoreside.eff[,SUMMER.MONTH])
  shoreside.eff$month.fall   <- rowSums(shoreside.eff[,FALL.MONTH])
}

shoreside.eff$year.wint.2  <- shoreside.eff$year-1

temp <- shoreside.eff[,c("year.wint.2", "region","month.winter.2") ]
shoreside.eff <- shoreside.eff %>% dplyr::select(-year.wint.2,-month.winter.2)
shoreside.eff <- merge(shoreside.eff,temp,by.x=c("year","region"),by.y=c("year.wint.2" ,"region"),all=T)
shoreside.eff$month.winter <- shoreside.eff$month.winter.2 + shoreside.eff$month.winter.1
shoreside.eff<- shoreside.eff %>% replace(., is.na(.), 0) 

shoreside.eff <- shoreside.eff[,c("year","region", MONTH)]
colnames(shoreside.eff)[2] <- "area.code"

###################################################################################
###################################################################################
# trim files to match the Years span specified by the Master File
temp<- expand.grid(year=YEARS.RECOVER,area.code=LOCATIONS$location.name)

shoreside.eff1 <- merge(shoreside.eff,data.frame(year=YEARS.RECOVER))
shoreside.eff2 <- merge(shoreside.eff1,temp,all=T)

shoreside.eff2$area.numb <- LOCATIONS$location.number[match(shoreside.eff2$area.code,LOCATIONS$location.name)]
#ashop.eff2 <- ashop.eff2[order(ashop.eff2$area.numb, ashop.eff2$year),]

shoreside.eff2[is.na(shoreside.eff2 == T)]<- 0 

########### final shoreside effort file ###############
effort.shoreside <- shoreside.eff2

##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
### Make an equivalent dataset for sampling fraction
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################

# This is relatively simple because there is little or no information about sampling prior to 2011.
# 2011 and later have 100% sampling coverage (see Shoreside notes) so we can port the information from effort file and turn
# it into a sampling fraction file in a couple of lines.

shoreside.sample.fraction <- effort.shoreside

THIS <- LOCATIONS$location.number[LOCATIONS$location.name=="PUSO_out"]

shoreside.sample.fraction <- shoreside.sample.fraction %>%
                                mutate(month.winter = ifelse(year >= 2011 & area.numb <= THIS,1,0),
                                      month.spring =ifelse(year >= 2011 & area.numb <= THIS,1,0),
                                      month.summer = ifelse(year >= 2011 & area.numb <= THIS,1,0), 
                                      month.fall = ifelse(year >= 2011 & area.numb <= THIS,1,0))
                                
