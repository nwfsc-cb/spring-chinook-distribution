### SCRIPT FOR CREATING FILES THAT CAN BE READ IN BY STAN
library(tidyverse)
# BC Hake Effort data first
#ashop.eff was created in "GitHub/Chinook_Bycatch/scripts/ashop" original .csv is stored in "GitHub/Chinook_Bycatch/ASHOP/final_datasets/ashop.total.effort"
#S. ANDERSON already allotted months and wrapped them, so that part has been deleted from this script. 
#base.dir<-getwd()

#pull in data for associated month group
#FOUR == sean's month_group1

if(MONTH.STRUCTURE=="FOUR"){
  bc.hake.eff <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/Effort Data/bc.hake.eff1.csv",sep=""))
}
if(MONTH.STRUCTURE=="FRAM"){
  bc.hake.eff <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/Effort Data/bc.hake.eff2.csv",sep=""))
}

# change month from number to month.01 etc
bc.hake.eff <- bc.hake.eff %>%
  mutate(month.use = recode(month_group1, '1' = "month.spring",
                            '2' = "month.summer",
                            '3' = "month.fall",
                            '4' = "month.winter")) %>%
  dplyr::select(c("year.mod", "region", "month.use", "n_fished_days")) %>%
  spread(month.use, n_fished_days) %>%
  replace(., is.na(.), 0) 

colnames(bc.hake.eff)[2] <- "area.code"
colnames(bc.hake.eff)[1] <- "Year"

###################################################################################
###################################################################################

# trim files to match the Years span specified by the Master File
temp<- expand.grid(Year=YEARS.RECOVER,area.code=LOCATIONS$location.name)

bc.hake.eff1 <- merge(bc.hake.eff,data.frame(Year=YEARS.RECOVER))
bc.hake.eff2 <- merge(bc.hake.eff1,temp,all=T)

bc.hake.eff2$area.numb <- LOCATIONS$location.number[match(bc.hake.eff2$area.code,LOCATIONS$location.name)]
#bc.hake.eff2 <- bc.hake.eff2[order(bc.hake.eff2$area.numb, bc.hake.eff2$year),]

bc.hake.eff2[is.na(bc.hake.eff2 == T)]<- 0 

########### final effort file ###############
bc.hake.effort <- bc.hake.eff2 %>% rename(year=Year)
########### merge in with effort.shoreside and make sure there is only one entry for each year and area.  BC and shoreside should not overlap.
effort.shoreside <- bind_rows(effort.shoreside,bc.hake.effort) %>% group_by(year,area.code,area.numb) %>% 
                      dplyr::summarise(month.winter=sum(month.winter),
                                      month.spring=sum(month.spring),
                                      month.summer=sum(month.summer),
                                      month.fall = sum(month.fall)) %>%
                      as.data.frame()
