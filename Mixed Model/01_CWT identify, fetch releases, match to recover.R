library(readxl)
library(dplyr)
library(lubridate)
#### Consolideted CWT releases file for Climate analysis
#base.dir <- "/Users/ole.shelton/GitHub"
##rmis.base.dir <- "C:/Users/jense/Documents/GitHub/rmis-data" # MODIFIED TO REFLECT MY LOCAL DIRECTORY

rmis.base.dir <- "/Users/ole.shelton/GitHub/rmis-data"
base.dir <- "/Users/ole.shelton/GitHub"

RUN.TYPE = "spring-summer" ### options: fall, spring-summer, south, case_study, case_study_south

GROUP <- "FRAM_2022_12"  ### Options "CA", "CA+" "COL" "CA+COL" "CA+COL_AWG" "CA+COL+PUSO" , FRAM_v1, FRAM_v2 # THIS IS WHAT YOU READ IN WITH
                     ###  "FRAM_EXP" is an experimental one for looking at                 
GROUP.2 <- "FRAM_2022_12"  # THIS IS WHAT GETS WRITTEN TO FILE.  Unless == "CA+COL_AWG" this should be the same as GROUP
                          #A CLUGE SO CAN READ IN THE SAME DATA BUT END UP WITH A SLIGHTLY DIFFERENT DATA FRAME
loc_18 <- "_two_OR_PUSO_AK" ## THIS IS THE MAPPING TO RECOVERIES AREA CODING.  Options I have tried:
                              ## options for this are a null string ("", should be default) 
                              ## If you are doing spring run fish, including fish from Alaska you should use:
                                        ### "two_OR_PUSO_AK"
if(RUN.TYPE=="spring-summer"){loc_18 <- "_two_OR_PUSO_AK"}
                              ## or "_18loc" for using 2 areas for PUSO, 
                              ## or "_two_OR_PUSO" for two OR areas (SOR and NOR) and two PUSOs. SOR and COR are combined. 
                                  ### "_two_OR_PUSO" is the spatial grouping used in the 2021 publication.
                              ## or "_NCA_SOR_PUSO" for two OR areas (COR and NOR) and two PUSOs.  NCA and SOR are combined
                              ## or "AK_NCA_SOR_PUSO" for two OR areas (COR and NOR) and two PUSOs.  NCA and SOR are combined + Includes Alaksa areas (YAK, PWS, KOD, CHIR, AKPEN, ALEUT,BER)
                              ## or "two_OR_PUSO_AK" for two OR areas (SOR and NOR), two PUSOs. Includes Alaska areas (YAK, PWS, KOD, CHIR, AKPEN, ALEUT,BER)

### CWT releases
# go to rmis git repo to get release data.
## Identify rivers, releases of interest.
all.release.dat	<-	read.csv(paste0(rmis.base.dir,"/data/chinook/all_releases.csv"))
# This is the older file I used pre-12/2019:
#all.release.dat	<-	read.csv("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Releases/all chinook releases.csv")
all.release.dat$hatchery_location_code	<- as.character(all.release.dat$hatchery_location_code)

### NEED NOTE ON HOW I IDENTIFIED THESE RELEASES IN PARTICULAR
# if(RUN.TYPE=="fall"){
#   NOM <- paste(base.dir,"/Orca_Salmon_DATA/Releases/SPR-SUM/river releases fall chinook ",GROUP," SPR-SUM.csv",sep="")
# }
if(RUN.TYPE=="spring-summer"){
  NOM <- paste(base.dir,"/Orca_Salmon_DATA/Releases/SPR-SUM/river releases spring-summer chinook ",GROUP," SPR-SUM.csv",sep="")
}

chosen.release	<-	read.csv(NOM)
chosen.release$hatchery_location_code	<- as.character(chosen.release$hatchery_location_code)
chosen.release$stock_location_code	<- as.character(chosen.release$stock_location_code)
chosen.release$release_location_name	<-	as.character(chosen.release$release_location_name)

BROOD.YEAR.RANGE	<- c(1975,2016)

focal.releases		<-	NULL
for(i in 1:nrow(chosen.release)){
  
  all.release.dat$release.year			    <-	as.numeric(substr(all.release.dat$first_release_date,1,4))
  all.release.dat$first.release.month		<-	as.numeric(substr(all.release.dat$first_release_date,5,6))
  all.release.dat$last.release.month		<-	as.numeric(substr(all.release.dat$last_release_date,5,6))
  
  if(chosen.release$release_location_name[i] == "" | is.na(chosen.release$release_location_name[i])==T ){
    temp	<- all.release.dat[all.release.dat$hatchery_location_code == chosen.release$hatchery_location_code[i] &
                              all.release.dat$stock_location_code == chosen.release$stock_location_code[i] &
                              all.release.dat$study_integrity !="W"  &
                              all.release.dat$study_integrity !="D" &
                              #all.release.dat$run == chosen.release$Run[i] &
                              all.release.dat$brood_year >= BROOD.YEAR.RANGE[1] &
                              all.release.dat$brood_year <= BROOD.YEAR.RANGE[2] ,] #&
                              #all.release.dat$first.release.month >= chosen.release$month.start[i] &
                              #all.release.dat$last.release.month <= chosen.release$month.stop[i] ,]
  }else if(chosen.release$Hatchery[i] =="NONE" ){
    temp	<- all.release.dat[all.release.dat$stock_location_code == chosen.release$stock_location_code[i] &
                              all.release.dat$study_integrity !="W" &
                              all.release.dat$study_integrity !="D" &
                              #all.release.dat$release_location_name == chosen.release$release_location_name[i] &
                              #all.release.dat$run == chosen.release$Run[i] &
                              all.release.dat$brood_year >= BROOD.YEAR.RANGE[1] &
                              all.release.dat$brood_year <= BROOD.YEAR.RANGE[2] ,]#&

  }else{
    temp	<- all.release.dat[all.release.dat$hatchery_location_code == chosen.release$hatchery_location_code[i] &
                              all.release.dat$stock_location_code == chosen.release$stock_location_code[i] &
                              all.release.dat$study_integrity !="W" &
                              all.release.dat$study_integrity !="D" &
                              all.release.dat$release_location_name == chosen.release$release_location_name[i] &
                              #all.release.dat$run == chosen.release$Run[i] &
                              all.release.dat$brood_year >= BROOD.YEAR.RANGE[1] &
                              all.release.dat$brood_year <= BROOD.YEAR.RANGE[2] ,]#&
                              #all.release.dat$first.release.month >= chosen.release$month.start[i] &
                              #all.release.dat$last.release.month <= chosen.release$month.stop[i] ,]
  }
  
  # Deal with rare NA run data in release data.  Common in Alaska.
  if(chosen.release$State[i] == "AK"){
      temp$run <- 1 #  Almost all of the runs missing run type are from Alaska.  all alaska fish are spring.
  }else{
    if(nrow(temp %>% filter(is.na(temp$run)))>0){
      print(paste("RUN IS NA", chosen.release$stock_location_code[i]));
      #print(temp %>% filter(is.na(run)))
      }
    temp <- temp %>% filter(run == chosen.release$Run[i])
  }
  
#  if(nrow(temp %>% filter(is.na(temp$run)))>0){print(paste("RUN IS NA", chosen.release$stock_location_code[i]))}
  

  if(nrow(temp) > 0 ){
    temp	<-	temp[is.na(temp$tag_code_or_release_id)==FALSE,]
    temp$ID	<-	chosen.release$ID_name[i]
    
    ### THIS SECTION MEANS THAT ONLY CWT and AD-CLIPPED FISH ARE INCLUDED IN THE MARKED COUNT.
    temp <- temp %>% mutate(cwt.released.1 = ifelse(substr(cwt_1st_mark,1,1)==5,cwt_1st_mark_count,0)) %>%
                    mutate(cwt.released.2 = ifelse(substr(cwt_2nd_mark,1,1)==5,cwt_2nd_mark_count,0)) %>%
                    mutate(cwt.released.1 = ifelse(is.na(cwt.released.1)==T,0,cwt.released.1)) %>%              
                    mutate(cwt.released.2 = ifelse(is.na(cwt.released.2)==T,0,cwt.released.2)) %>%
                    mutate(cwt.released = cwt.released.1 + cwt.released.2)
    ## Keep numbers of CWT with no-adclip too.
    temp <- temp %>% mutate(cwt.all.released.1 = ifelse(is.na(cwt_1st_mark_count)==T,0,cwt_1st_mark_count)) %>%              
                     mutate(cwt.all.released.2 = ifelse(is.na(cwt_2nd_mark_count)==T,0,cwt_2nd_mark_count)) %>%
                      mutate(cwt.all.released = cwt.all.released.1 + cwt.all.released.2)
    
    A<-temp %>% filter(!substr(cwt_1st_mark,1,1)==5)
    B<-temp %>% filter(!substr(cwt_2nd_mark,1,1)==5)
    print(paste(i,"nrow=",nrow(temp)))
    
    #print(paste("1st not = 5",nrow(A),"; tot =",sum(A$cwt_1st_mark_count,na.rm=T)))
    #print(paste("2nd not = 5",nrow(B),"; tot =",sum(B$cwt_2nd_mark_count,na.rm=T)))
    
    #temp$cwt.released	<-	rowSums(temp[,c("cwt_1st_mark_count","cwt_2nd_mark_count")],na.rm=T)
    temp$ocean.region 	<-	chosen.release$Ocean.region[i]
    temp$river 			<-	chosen.release$River[i]
    
    trim	<-	temp[,c("ID","river","ocean.region","run","stock_location_code","hatchery_location_code","tag_code_or_release_id",
                    "cwt.released","cwt.all.released","brood_year","release.year","first.release.month","last.release.month",
                    "release_stage","study_type","tag_reused","comments")]
    
    focal.releases	<-	rbind(focal.releases,trim)
  }  
}

colnames(focal.releases)[which(colnames(focal.releases) == "tag_code_or_release_id")]	<- "tag_code"

# Make sure each tag code is only present in the data frame once:
trim.temp      <- data.frame(tag_code=unique(focal.releases$tag_code))
# get rid of codes that are not at least 6 characters long.
trim.temp <- trim.temp %>% mutate(n_char =  nchar(tag_code)) %>% filter(n_char>=6) 

focal.releases <- focal.releases %>% filter(tag_code %in% trim.temp$tag_code) %>%
                        distinct(
                        ID, river,ocean.region,          
                        run,stock_location_code,hatchery_location_code,
                        tag_code,cwt.released,cwt.all.released,      
                        brood_year,release.year,first.release.month, 
                        last.release.month,release_stage,study_type,            
                        tag_reused,comments)
                            
# Remove releases that have exactly 0 cwt and adipose clip releases.
focal.releases <- focal.releases %>% filter(cwt.released>0)

### MODIFY THIS SECTION TO INCLUDE AWG tag groups separately.
# awg.cwt <- read.csv("/Users/ole.shelton/GitHub/GSI_CWT_Chinook/AWG chinook CTC data/WireTagCode-OLE.csv")
# f1  <- focal.releases %>% filter(tag_code %in% awg.cwt$TagCode) %>% mutate(awg.tag.code =1)
# f1$ID <- paste(f1$ID,"_AWG",sep="")
# f2  <- focal.releases %>% filter(!tag_code %in% awg.cwt$TagCode) %>% mutate(awg.tag.code =0)
# 
# focal.releases <- data.frame(rbind(f1,f2))
# # Make indicator variable in the focal.releases data frame that indicates if this code group is used by the CTC model.
# focal.releases <- focal.releases %>% arrange(ID,brood_year)

write.csv(focal.releases,
          file = paste0(base.dir,"/Orca_Salmon_DATA/Releases/SPR-SUM/Tag codes ",RUN.TYPE," chinook ",GROUP," Spr-Sum",loc_18,".csv"),row.names=FALSE)
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################

# Read in AWG CTC information and match it with the extracted tag data.
  # awg.rec <- read.csv("/Users/ole.shelton/GitHub/GSI_CWT_Chinook/AWG chinook CTC data/CWDBRecovery.csv")
  # awg.rec$TagCode <-as.character(awg.rec$TagCode)
  # awg.rec$TagCode[which(nchar(awg.rec$TagCode)<=5)]  <- paste("0",awg.rec$TagCode[which(nchar(awg.rec$TagCode)<=5)],sep="")
  # 
  # focal.awg.rec <- awg.rec %>% filter(TagCode %in% focal.rel.awg$tag_code)

###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
### Pull out recoveries for the tag groups of interest.
################### Go get the tag code file
# this data comes from above script
tag.dat	<- read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Releases/SPR-SUM/Tag codes ",RUN.TYPE," chinook ",GROUP," Spr-Sum",loc_18,".csv"))

# Go get the AWG data. 
dat.awg <- readRDS(paste0(base.dir,"/Orca_Salmon_DATA/AWG chinook CTC data/2022-09/CWDBRecovery.rds"))
dat.awg <- dat.awg %>% mutate(RecDate = as.Date(RecoveryDate,"%m/%d/%Y"),
                   month=month(RecDate),year=year(RecDate), day=day(RecDate))

# Use this to check if the appropriate code tags are present in the database.
dat.wire.code <- read.csv(paste0(base.dir,"/Orca_Salmon_DATA/AWG chinook CTC data/2022-09/WireTagCode.csv")) 

#label the tag codes that are in the AWG database so they can be kept separate during aggregation
awg.codes <- dat.awg %>% distinct(TagCode) %>% rename(tag_code=TagCode)

tag.dat <- tag.dat %>% mutate(ID = ifelse(tag_code %in% awg.codes$tag_code, paste0(ID,"_awg"),ID ))

A	<-	aggregate(tag.dat$cwt.released,by=list(ID=tag.dat$ID,ocean.region=tag.dat$ocean.region,brood_year=tag.dat$brood_year,
                                            release_year=tag.dat$release.year,release_month=tag.dat$first.release.month),sum)
B	<-	data.frame(expand.grid(brood_year=BROOD.YEAR.RANGE[1]:BROOD.YEAR.RANGE[2],ID=sort(unique(A$ID))))
C	<-	merge(B,A,all=TRUE) 
D	<-	aggregate(tag.dat$ocean.region,by=list(ocean.region=tag.dat$ocean.region,ID=tag.dat$ID),length)
C$ocean.region	<-	D$ocean.region[match(C$ID,D$ID)]
C	<-	C[order(C$ocean.region,C$ID,C$brood_year),]
C$x[is.na(C$x)==T]	<-	0

length(C$x[C$x>0])
sum(C$x)
sum.release.by.region	<-	aggregate(C$x,by=list(brood_year=C$brood_year,ocean.region =C$ocean.region),sum)

#### Summarize releases
colnames(A)[which(colnames(A)=="x")] <- "N.released"
A <- A[order(A$ID,A$release_year,A$release_month),]
B <- aggregate(A$release_month,by=list(ID=A$ID,ocean.region=A$ocean.region,brood_year=A$brood_year, release_year=A$release_year),median)
colnames(B)[which(colnames(B)=="x")] <- "Median.month.release"
C <- aggregate(A$N.released,by=list(ID=A$ID,ocean.region=A$ocean.region,brood_year=A$brood_year,release_year=A$release_year),sum)
colnames(C)[which(colnames(C)=="x")] <- "N.released"
D <- merge(B,C)

releases <- list(releases=D)
### Save Results to File
save(releases,file=paste0(base.dir,"/spring-chinook-distribution/Processed Data/Releases ",RUN.TYPE," ",GROUP,".RData"))	    

###########################################################################################################################################		
#### Modify the tag code file here, if desired.
code.all	<- unique(tag.dat$tag_code)

# Go get the recovery codes from weitkamp 
dat.loc.key	<- read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Recoveries/recovery codes-wietkamp+shelton 10-2022 two PUSO.csv"),header=T)

################### First read in all of the recovery data files
#setwd("/Users/ole.shelton/GitHub/rmis-data/data/chinook")
load(paste0(rmis.base.dir,"/all_chinook.Rdata"))

# only keep tag_codes that are in the code.all file
all.code.recoveries <- all_chinook[[1]] %>% filter(tag_code %in% code.all) %>% as.data.frame()

##################################################################################################
### CHECK FOR DUPLICATIONS IN THE CWTs codes queried.
# ##################################################################################################
# C <- data.frame(cbind(A,B))
# colnames(C) = c("tag_code","id")
# D <- C %>% group_by(tag_code) %>% dplyr::summarize(N =length(tag_code)) %>% as.data.frame()
# E <- C %>% filter(tag_code %in% D$tag_code[D$N>=2]) %>% arrange(tag_code)
# 
##################################################################################################
all.code.recoveries$rec.year	<-	as.numeric(substr(all.code.recoveries$recovery_date,1,4))
all.code.recoveries$rec.month	<-	as.numeric(substr(all.code.recoveries$recovery_date,5,6))
#all.code.recoveries$rec.day		<-	as.numeric(substr(all.code.recoveries$recovery_date,5,6))

class(all.code.recoveries$tag_code)

##########################################################################################
#   ## CONSOLIDATE RECOVERIES 
##########################################################################################
### Break into Marine and Freshwater recoveries
recover.marine	<-	all.code.recoveries[substr(all.code.recoveries$recovery_location_code,2,2)=="M",]
recover.fresh	  <-	all.code.recoveries[substr(all.code.recoveries$recovery_location_code,2,2)=="F",]

# use this section to add in Auxiliary recoveries from the AWG data.
# FOR FRESHWATER ONLY.
dat.awg.F <- dat.awg %>% filter(Auxiliary == 1, 
                                TagCode %in% code.all,
                                substr(RecoverySite,2,2)=="F") %>%
                      #replace messed up recovery years.
                      mutate(year=ifelse( abs(year-RunYear) >=1,RunYear,year))  

# Take a look at the marine recoveries.
dat.awg.M <- dat.awg %>% filter(Auxiliary == 1, 
                                TagCode %in% code.all,
                                substr(RecoverySite,2,2)=="M")

# Identify some of these as actually freshwater recoveries in disguise, 
# move them to the recover.fresh database.
FISH.CODE <- c(0,20,22,26,46)
dat.awg.F2 <- dat.awg.M %>% filter(CWDBFishery %in% FISH.CODE )
                                
# Move all gillnet (fishery==20) to fw recoveries.
# Move all coastal gillnet (fishery==22) to fw recoveries.
# Move freshwater sport (fishery 46) to fw recoveries.
# Move in-river subsistence (fishery 0) to fw recoveries.
# They occur in the various terminal areas of SEAK and the fraser.

# Merge the two dat.awg.F data streams and Change the names of dat.awg.F
dat.awg.F <- dat.awg.F %>% bind_rows(.,dat.awg.F2) %>%
  mutate(gear=NA,
         recovery_location_name =NA,
         sampling_site=NA,
         detection_method = NA,
         length_type=NA) %>%
  dplyr::select(species=Species,
                tag_code=TagCode,
                recovery_id=RecoveryID,
                #recovery_date=RecoveryDate,
                fishery=CWDBFishery,
                gear=gear,
                sex=Sex,
                length=Length,
                length_type=length_type,
                length_code=LengthCode,
                recovery_location_code = RecoverySite,
                recovery_location_name =recovery_location_name,
                sampling_site=sampling_site,
                sample_type=SampleType,
                estimation_level=EstimationLevel,
                estimated_number= EstimatedNumber,
                detection_method = detection_method,
                rec.year=year,rec.month=month,
                auxiliary = Auxiliary)

recover.fresh <- bind_rows(recover.fresh %>% mutate(auxiliary = 0) %>% 
                             dplyr::select(-recovery_date),
                             dat.awg.F)

# Get rid of the marine recoveries that we moved to freshwater. 
dat.awg.M <- dat.awg.M %>% filter(!CWDBFishery %in% FISH.CODE) %>%
                mutate(RecDate_short = as.integer(paste0(substr(RecDate,1,4),
                                              substr(RecDate,6,7),
                                              substr(RecDate,9,10))))

#######---  Marine ---####################################################################
#Merge recovery code names with areas used.
#### Assign recoveries into regions delineated by Weitkamp 2002 and 2010 
#### and expanded and modified by Shelton et al 2019, 2021. 
recover.marine$rec.area.code <- NA

#Add in extra recoveries from the Auxiliary data (AWG data)
recover.marine <- dat.awg.M %>% 
  mutate(gear=NA,
         recovery_location_name =NA,
         sampling_site=NA,
         detection_method = NA,
         length_type=NA) %>%
  dplyr::select(species=Species,
                tag_code=TagCode,
                recovery_id=RecoveryID,
                recovery_date=RecDate_short,
                fishery=CWDBFishery,
                gear=gear,
                sex=Sex,
                length=Length,
                length_type=length_type,
                length_code=LengthCode,
                recovery_location_code = RecoverySite,
                recovery_location_name =recovery_location_name,
                sampling_site=sampling_site,
                sample_type=SampleType,
                estimation_level=EstimationLevel,
                estimated_number= EstimatedNumber,
                detection_method = detection_method,
                rec.year=year,rec.month=month) %>%
    bind_rows(recover.marine, .)


# #### USE THIS FOR FIRST RUN 
# recover.marine$rec.area.code <- dat.loc.key$Rec.area.Shelton[match(recover.marine$recovery_location_code, dat.loc.key$location_code)]
# 
# #### USE THIS TO CULL just area 5 off MARINE AREA OF PUSO
# recover.marine$rec.area.code <- dat.loc.key$Rec.area.Shelton3[match(recover.marine$recovery_location_code, dat.loc.key$location_code)]
# 
# #### USE THIS TO DIFFERENTIATE area 5,6,7 FROM INTERIOR PUSO
# recover.marine$rec.area.code <- dat.loc.key$Rec.area.Shelton3[match(recover.marine$recovery_location_code, dat.loc.key$location_code)]
# 
# #### USE THIS TO DIFFERENTIATE area 5,6,7 FROM INTERIOR PUSO with modifications to High Seas Recoveries
# recover.marine$rec.area.code <- dat.loc.key$Rec.area.Sullaway.Shelton[match(recover.marine$recovery_location_code, dat.loc.key$location_code)]
# 

if(loc_18 == "_two_OR_PUSO"){
#### USE THIS TO DIFFERENTIATE area 5,6,7 FROM INTERIOR PUSO and only TWO OREGON REGIONS with modifications to High Seas Recoveries
  recover.marine$rec.area.code <- dat.loc.key$Rec.area.Sullaway.Shelton.two.Oregon[match(recover.marine$recovery_location_code, dat.loc.key$location_code)]
}

if(loc_18 == "_two_OR_PUSO_AK"){
  #### USE THIS TO DIFFERENTIATE area 5,6,7 FROM INTERIOR PUSO and only TWO OREGON REGIONS with modifications to High Seas Recoveries
  recover.marine$rec.area.code <- dat.loc.key$Rec.area.Sullaway.Shelton.TWO_OR_PUSO_AK[match(recover.marine$recovery_location_code, dat.loc.key$location_code)]
}

if(loc_18 == "_NCA_SOR_PUSO" | loc_18 == "_AK_NCA_SOR_PUSO" ){
  #### USE THIS TO DIFFERENTIATE area 5,6,7 FROM INTERIOR PUSO and only TWO OREGON REGIONS with modifications to High Seas Recoveries
  recover.marine$rec.area.code <- dat.loc.key$Rec.area.Sullaway.Shelton.SOR_NCA[match(recover.marine$recovery_location_code, dat.loc.key$location_code)]
}


# THIS IS A CATCH TO LOOK FOR RECOVERY LOCATIONS THAT ARE NOT IN THE DATABASE.
A1 <-recover.marine[is.na(recover.marine$rec.area.code)==T,]
A2 <-recover.marine[recover.marine$rec.area.code=="",]
B <-recover.marine[is.na(recover.marine$rec.area.code)==F,]

ERROR <- NULL
if(nrow(A1)>0|nrow(A2)>0){
  ERROR="%%%%%%%%%%%%%%%%%%%%%%%%%%%  RECOVERIES LOCATIONS MISSING!!!! %%%%%%%%%%%%%%%%%%%%%%%%%%%"
  C1 <- A1 %>% group_by(recovery_location_code,recovery_location_name) %>% summarize(n=length(rec.year),Sum=sum(estimated_number))
  C2 <- A2 %>% group_by(recovery_location_code,recovery_location_name) %>% summarize(n=length(rec.year),Sum=sum(estimated_number))
  write.csv(A1,file=paste0(base.dir,"/spring-chinook-distribution/temp recover.codes1.csv"))
  write.csv(A2,file=paste0(base.dir,"/spring-chinook-distribution/temp recover.codes2.csv"))
  stop("RECOVERIES LOCATIONS MISSING!!!!")
}
#unique(A$recovery_location_code[A$fishery ==10])
  

#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################

### THIS SHOULD BE REWRITTEN IN DPLYR but I don't feel like it.
### On 5-2-2022 Ole rewrote this is dplyr.  
### He found different results for aggregation methods in base R vs. dplyr
### After inspection, all discrepancies were driven by pre-1978 recoveries
### and for post-1978 observations determinied by the inclusion or non-inclusion of estimation.level
### factors.  Essentially this is a NA handling difference between
### the methods.
# Area, year, month, release group, fishery
# marine.1	<- aggregate(recover.marine$estimated_number,by=list(
#   tag_code	= recover.marine$tag_code,
#   fishery		= recover.marine$fishery,	
#   rec.year  = recover.marine$rec.year,
#   rec.month = recover.marine$rec.month,
#   rec.area.code = recover.marine$rec.area.code,
#   estimation.level = recover.marine$estimation_level,
#   sampling_site = recover.marine$sampling_site,
#   recovery_location_name = recover.marine$recovery_location_name,
#   # 							period.type = recover.marine$period_type,
#   # 							period = recover.marine$period,
#   sample.type = recover.marine$sample_type),
#   length)

# aggregate non-high seas recoveries.
marine.1 <- recover.marine %>% 
              group_by(tag_code, 
                       fishery, 
                       rec.year,
                       rec.month,
                       rec.area.code,
                       estimation.level=estimation_level,
                       sampling_site,
                       recovery_location_name,
                       sample.type=sample_type) %>%
              summarise(x=length(estimated_number),
                        count=sum(estimated_number,na.rm=T)) %>%
              # filter out two sample types that cause troubles.
              filter(!grepl("HIGH SEAS",recovery_location_name)) %>% # this is about 3000 observations.
              filter(!sample.type %in% c(4,5)) %>% # This gets rid of ~ 17000 observations.
              as.data.frame()

#This is the high seas for the Gulf of Alaska 
# This does not actually get used in the model, it is primarily calculated for comparing with 
# the data pulled from AFSC observer data.
marine.GOA.HS <- recover.marine %>% 
                    filter(grepl("HIGH SEAS 3",recovery_location_name),
                           fishery %in% c(81,82,812)) %>%
                    group_by(tag_code, 
                          fishery, 
                          rec.year,
                          rec.month,
                          rec.area.code,
                          estimation.level=estimation_level,
                          sampling_site,
                          recovery_location_name,
                          sample.type=sample_type) %>%
                  summarise(count=length(estimated_number)) %>%
                  as.data.frame() 

## Merge with release group
marine.all	<- left_join(tag.dat,marine.1,by="tag_code")
marine.all <- marine.all %>% arrange(tag_code,fishery,rec.year,rec.month) %>%
                  rename(est.numb = count, count = x) %>%
                  filter(est.numb > 0) %>%
                  mutate(frac.samp = count / est.numb,
                         frac.samp = ifelse(frac.samp > 1,1,frac.samp))

marine.GOA.HS <- left_join(marine.GOA.HS,tag.dat,by="tag_code")

#Check to make sure the sums are equivalent after merge.
# A<-marine.merge %>% group_by(rec.year) %>%summarise(a2=sum(count))
# B<-marine.1 %>% group_by(rec.year) %>%summarise(a1 =sum(count))
# C <- left_join(A,B) %>% as.data.frame()
# D <- marine.merge %>% filter(rec.year==1981)
# E <- marine.1 %>% filter(rec.year==1981)

# OLD CODE.  
# marine.merge	<- marine.merge[order(marine.merge$tag_code,marine.merge$fishery,marine.merge$rec.year,marine.merge$rec.month),]
# colnames(marine.merge)[ncol(marine.merge)]	<-	"est.numb"
# 
# marine.all		<-	merge(marine.merge.count,marine.merge)
# marine.all		<-	marine.all[marine.all$sample.type !=5 & marine.all$sample.type !=4,]
# marine.all		<-	marine.all[marine.all$est.numb > 0,]
# ##
# marine.all$frac.samp<-marine.all$count/marine.all$est.numb
# marine.all$frac.samp[marine.all$frac.samp > 1] <- 1
####### Checking equivalency
# marine.1$test <- paste(marine.1$tag_code,
#                        marine.1$fishery,	
#                        marine.1$rec.year,
#                        marine.1$rec.month,
#                        marine.1$rec.area.code,
#                        marine.1$estimation.level,
#                        marine.1$sampling_site,
#                        marine.1$recovery_location_name,
#                        marine.1$sample.type,sep=".")
# 
# marine.1 <- marine.1 %>% arrange(test)
# 
# marine.1b$test <- paste(marine.1b$tag_code,
#                        marine.1b$fishery,	
#                        marine.1b$rec.year,
#                        marine.1b$rec.month,
#                        marine.1b$rec.area.code,
#                        marine.1b$estimation.level,
#                        marine.1b$sampling_site,
#                        marine.1b$recovery_location_name,
#                        marine.1b$sample.type,sep=".")
# 
# marine.1b <- marine.1b %>% arrange(test)
#                        
# A<- marine.1 %>% distinct(test) %>% mutate(dat1="A")
# B<- marine.1b %>% distinct(test) %>% mutate(dat2="B")
# 
# C <- left_join(B %>% ungroup() %>% dplyr::select(test,dat2),
#                A %>% ungroup() %>% dplyr::select(test,dat1))
# 
# D <- C %>% filter(is.na(dat2)|is.na(dat1))
# E <- marine.1b %>% filter(test %in% D$test)

# Area, year, month, release group, fishery

###############
# adjustments for treaty troll fishey in winter - assign all to PUSO.OUT
marine.all$rec.area.code <- as.character(marine.all$rec.area.code)
marine.all <- marine.all %>% 
                  mutate(rec.area.code=
                           ifelse(rec.area.code == "WAC PUSO.OUT" &
                                         fishery==15 & 
                                         rec.month %in% c(1,2,3,4,10,11,12),
                                         "PUSO.OUT",rec.area.code))

#################################################################################
##### Combine Areas
#####################################################################################
source(paste0(base.dir,"/spring-chinook-distribution/_R code for processing raw data/Combine recover areas Spr-Sum",loc_18,".r"))
#####################################################################################

# 		marine.by.hatchery	<-	marine.by.hatchery[order(marine.by.hatchery$ID,
# 															marine.by.hatchery$fishery,
# 															marine.by.hatchery$rel.year,
# 															marine.by.hatchery$rec.year,
# 															marine.by.hatchery$rec.month),]
# 		# Eliminate zero observations
# 		marine.by.hatchery	<-	marine.by.hatchery[marine.by.hatchery$x > 0 ,]
# 		
# 		
# 		marine.by.hatchery[marine.by.hatchery$rec.area.code == "OR",] # fair number of observations throughout the 80s... unclear about what to do with these (all troll)m
# 		marine.by.hatchery[marine.by.hatchery$rec.area.code == "PWS",] # few... all in gill net fisheries.
# 		marine.by.hatchery[marine.by.hatchery$rec.area.code == "CA",] # None after 1976
# 		marine.by.hatchery[marine.by.hatchery$rec.area.code == "BC",] # none after 1977
# 

######################################################################################
marine.by.hatchery <- marine.all %>% group_by(ID,
                                               brood.year=brood_year,
                                               rel.year=release.year,
                                               rel.month=first.release.month,
                                               ocean.region,
                                               fishery,
                                               rec.year,
                                               rec.month,
                                               rec.area.code,
                                               estimation.level) %>%
                                summarise(est.number =sum(est.numb),
                                          Count = sum(count),
                                          median.frac.samp=median(frac.samp)) %>%
                                rename(count=Count,est.numb=est.number) %>% 
                                filter(!is.na(rec.month)) # this gets rid of one entry from the Bering (5/3/2022)

# Systematically exclude recoveries are not of interest and those that will be added in later
marine.all <- marine.all %>% filter(!fishery %in% 
                                      # THESE WILL BE ADDED BACK IN:  
                                      c(80, # Hake Trawl Fishery, At-Sea component (CA/OR/WA)
                                        800, #Hake Trawl Fishery, Shoreside component (OR/WA)
                                        85, # Ocean Trawl By-Catch
                                        # THESE WILL BE PERMANENTLY EXCLUDED  
                                        802, #Limited-Entry Rockfish Trawl (CA/OR/WA)
                                        803, #Limited-Entry Non-Hake Groundfish Trawl (CA/OR/WA)
                                        804, #Limited-Entry Sablefish Fixed Gear (CA/OR/WA)
                                        805, #State-Permitted Nearshore Grndfish Fishery (CA/OR)
                                        83, # Foreign Research Vessels
                                        84, # Foreign Mothership Vessels
                                        86, # Land Based Salmon
                                        87, # Squid Gillnet By-Catch
                                        91))  # PNP Cost Recovery.

### Remove Alaska Trawl Pollock and Rockfish so they can be modified and added separately.
marine.by.hatchery.AK.trawl <- marine.by.hatchery %>% 
  filter(fishery %in% c(81, #         81      Groundfish Observer (Gulf of Alaska)
                        812,#         812     Rockfish Fishery (Gulf of Alaska)
                        82))  #       82      Groundfish Observer (Bering Sea/Aleutians)
marine.by.hatchery <- marine.by.hatchery %>% 
  filter(!fishery %in% c(81, #        81      Groundfish Observer (Gulf of Alaska)
                         812,#        812     Rockfish Fishery (Gulf of Alaska)
                         82))  #      82      Groundfish Observer (Bering Sea/Aleutians)

# PREVIOUS VERSION:
# 
# marine.by.hatchery.numb	<-	aggregate(marine.all$est.numb,by=list(
#   ID		=	marine.all$ID,
#   brood.year = marine.all$brood_year,
#   rel.year = marine.all$release.year,
#   rel.month = marine.all$first.release.month,
#   ocean.region = marine.all$ocean.region,
#   fishery		= marine.all$fishery,	
#   rec.year  = marine.all$rec.year,
#   rec.month = marine.all$rec.month,
#   rec.area.code = marine.all$rec.area.code,
#   estimation.level = marine.all$estimation.level),
#   sum,na.rm=T)
# colnames(marine.by.hatchery.numb)[ncol(marine.by.hatchery.numb)]	<-	"est.numb"
# 
# marine.by.hatchery.count	<-	aggregate(marine.all$count,by=list(
#   ID		=	marine.all$ID,
#   brood.year = marine.all$brood_year,
#   rel.year = marine.all$release.year,
#   rel.month = marine.all$first.release.month,
#   ocean.region = marine.all$ocean.region,
#   fishery		= marine.all$fishery,	
#   rec.year  = marine.all$rec.year,
#   rec.month = marine.all$rec.month,
#   rec.area.code = marine.all$rec.area.code,
#   estimation.level = marine.all$estimation.level),
#   sum,na.rm=T)
# colnames(marine.by.hatchery.count)[ncol(marine.by.hatchery.count)]	<-	"count"
# 
# marine.by.hatchery.frac	<-	aggregate(marine.all$frac.samp,by=list(
#   ID		 	=	marine.all$ID,
#   brood.year = marine.all$brood_year,
#   rel.year 	= marine.all$release.year,
#   rel.month = marine.all$first.release.month,
#   ocean.region = marine.all$ocean.region,
#   fishery		= marine.all$fishery,	
#   rec.year 	= marine.all$rec.year,
#   rec.month 	= marine.all$rec.month,
#   rec.area.code 	= marine.all$rec.area.code,
#   estimation.level = marine.all$estimation.level),
#   median,na.rm=T)
# colnames(marine.by.hatchery.frac)[ncol(marine.by.hatchery.frac)]	<-	"median.frac.samp"
# 
# marine.by.hatchery	<-	merge(marine.by.hatchery.numb,marine.by.hatchery.count)
# marine.by.hatchery	<-	merge(marine.by.hatchery,marine.by.hatchery.frac)

####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
### --- MAKE R DATA FOR USE IN ESTIMATION
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
catch.by.region<- list()
ocean.region <- sort(unique(marine.by.hatchery$ocean.region))
for(i in 1:length(ocean.region)){
  temp <- marine.by.hatchery[marine.by.hatchery$ocean.region == ocean.region[i],]
  reg.ID <- sort(unique(temp$ID))
  ID<- list()
  for(j in 1:length(reg.ID)){
    ID[[j]] <- list(temp[temp$ID == reg.ID[j],])
  }
  names(ID) <- reg.ID  
  catch.by.region[[i]] <- ID
}
names(catch.by.region) <- ocean.region

ocean.recover = list(ocean.recover=catch.by.region,dat=marine.by.hatchery)
save(ocean.recover,file=paste0(base.dir,"/spring-chinook-distribution/Processed Data/Ocean Recoveries ",RUN.TYPE," ",GROUP,loc_18,".RData"))	    


####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################

#### HERE IS WHERE WE BRING IN THE HAKE TRAWL RECOVERIES AND MAKE A PARALLEL DATA FILE 
#### to the non-trawl recoveries (developed above)  The script to creat the "cwt_recoveries" file is: 

source(paste0(base.dir,"/spring-chinook-distribution/_R code for processing raw data/CWT make hake recoveries.R"))
dat.trawl <- recoveries

#  match with tag codes of interest identified above
dat.trawl <- dat.trawl %>% mutate(tag_code=as.character(tag.code))
dat.trawl.target <- left_join(dat.trawl,tag.dat) %>% filter(tag_code %in% code.all)

# Compile the tag codes into groups based on their ids
    ## This includes tag_code as grouping variable
trawl.all.test <- dat.trawl.target %>% 
      group_by(ID,tag_code,fishery,rec.year,rec.month,rec.area.code,ocean.region,brood_year,est.numb) %>% 
      dplyr::summarise(N=length(count),tot.count = sum(count),median.frac.samp = median(median.frac.samp)) %>%
      mutate(est.numb2=tot.count / median.frac.samp)

### Modify the areas to reflect different binning options for areas.
if(loc_18 == "_NCA_SOR_PUSO"){  # Options: "TRUE", "TWO_OR", "NCA_SOR_PUSO"
  dat.trawl.target <- dat.trawl.target %>% 
                mutate(rec.area.code = replace(rec.area.code,rec.area.code=="SOR","NCA")) 
}
if(loc_18 == "_two_OR_PUSO" | loc_18 == "_two_OR_PUSO_AK"){  #
  dat.trawl.target <- dat.trawl.target %>% 
    mutate(rec.area.code = replace(rec.area.code,rec.area.code=="COR","SOR")) 
}

## This includes sums across tag_codes as grouping variable
trawl.all <- dat.trawl.target %>% 
      group_by(ID,brood_year,release.year,first.release.month,ocean.region,fishery,rec.year,rec.month,rec.area.code) %>% 
      dplyr::summarise(N.samp=length(count),tot.count = sum(count),median.frac.samp = median(median.frac.samp)) %>%
      mutate(est.numb = tot.count / median.frac.samp) %>% 
      rename(brood.year=brood_year,rel.year=release.year,rel.month=first.release.month,count=tot.count) %>%
      dplyr::select(ID,brood.year,rel.year,rel.month,ocean.region,fishery,rec.year,rec.month,
              rec.area.code,N.samp,est.numb,count,median.frac.samp)

ocean.recover.trawl = list(dat=trawl.all)
save(ocean.recover.trawl,file=paste0(base.dir,"/spring-chinook-distribution/Processed Data/Ocean Recoveries Hake Trawl ",RUN.TYPE," ",GROUP,loc_18,".RData"))	    


### OK DRAW IN THE POLLOCK DATA FROM ALASKA.

#######################
## POLLOCK SHORESIDE IN ALASKA
#######################
# This takes recoveries of GOA pollock fleet from the RMIS database, and re-calculates 
# the sampling fraction to arrive at a concensus estimate of the estimated number of Chinook.
# It also makes some figures to summarize effort and sampling fraction

# This script is for determining effort and sampling fraction.
source(paste0(base.dir,"/spring-chinook-distribution/_R code for processing raw data/Process ALASKA pollock effort.R"),local=TRUE)
# Important files stemming from this script are (both in spring-chinook-distribution/Processed Data/Effort Data)
sampling.fraction <- readRDS(paste0(write.dir,"Sample_Fraction_Pollock_GOA_Summarized.RDS"))
# "Sample_Fraction_Pollock_GOA_Summarized.RDS" 

# This is the data from CWT recoveries in the Alaska Pollock fleet.
# This reads in the dates of pollock fisheries (important for identifying when pollock bycatch could have occurred)
source(paste0(base.dir,"/spring-chinook-distribution/_R code for processing raw data/Process_Alaska_Trawl_tidy_pollock_fishing_dates.R"))

# This assigns CWT recoveries to regions. d 
# This pulls from the AFSC observer database, NOT RMIS.
source(paste0(base.dir,"/spring-chinook-distribution/_R code for processing raw data/Process_Alaska_Trawl_Assign_RMIS_TripTarget_CWT.R"))

#Filter the AFSC database to only include tag_codes of interest
#This is the data that I will use for central and westen Alaska recoveries.
dat.trawl.pollock <- pollock_cwt %>% filter(tag_code %in% tag.dat$tag_code) %>%
                      left_join(.,tag.dat)

# This compiles into something very similar to the other recovery data frames.
trawl.pollock <- dat.trawl.pollock %>% 
  group_by(ID,brood_year,release.year,first.release.month,ocean.region,year,month,rec.area.code) %>% 
  dplyr::summarise(tot.count=length(year)) %>%
  mutate(fishery="pollock") %>%
  left_join(.,sampling.fraction,by=c("rec.area.code","year","month"="month.numb")) %>%
  rename(median.frac.samp = final.samp.fraction) %>%
  mutate(est.numb = tot.count / median.frac.samp) %>% 
  rename(brood.year=brood_year,rel.year=release.year,rel.month=first.release.month,count=tot.count) %>%
  dplyr::select(ID,brood.year,rel.year,rel.month,ocean.region,fishery,rec.year=year,rec.month=month,
                rec.area.code,est.numb,count,median.frac.samp)

# drop all data pre-1997 because there is no effort for pre-1997.
trawl.pollock <- trawl.pollock %>% filter(rec.year>1997)

# Find a couple of instances with missing sampling fraction
samp.frac.missing <- trawl.pollock %>% filter(is.na(median.frac.samp)) %>%
                        dplyr::select(-median.frac.samp)

extra.samp.frac <-    sampling.fraction %>% filter(year < 2003) %>% 
                        group_by(month.numb,rec.area.code) %>% 
                        summarise(median.frac.samp=median(final.samp.fraction,na.rm=T))

samp.frac.missing <- left_join(samp.frac.missing, extra.samp.frac,
                               by=c("rec.month"="month.numb","rec.area.code"="rec.area.code")) %>%
                               mutate(est.numb = count / median.frac.samp)

trawl.pollock <- trawl.pollock %>% filter(is.na(median.frac.samp)==F) %>%
                        bind_rows(.,samp.frac.missing)

## Write to File
ocean.recover.pollock.trawl = list(dat=trawl.pollock)
save(ocean.recover.pollock.trawl,file=paste0(base.dir,"/spring-chinook-distribution/Processed Data/Ocean Recoveries Pollock Trawl ",RUN.TYPE," ",GROUP,loc_18,".RData"))	    

####################################################
####################################################
# ALASKA ROCKFISH TRAWL
####################################################
####################################################

# This is the effort data from Alaska Rockfish fleet.
# This reads in the dates of rockfish fisheries effort from fish tickets 
source(paste0(base.dir,"/spring-chinook-distribution/_R code for processing raw data/Process ALASKA rockfish effort.R"))

trawl.rockfish.AK <- read.csv(paste0(data.cwt.dir,"/Rockfish CWT.csv")) %>%
  dplyr::mutate(Long=-1*Long, tag_code= str_pad(tag_code, pad = "0", width = 6, side = c("left"))) %>%
  tidyr::separate(recovery_date, into =c("rec.year", "rec_monthrec_day"), sep = 4) %>%
  tidyr::separate(rec_monthrec_day, into =c("rec.month", "rec.day"), sep = 2) %>%
  #dplyr::filter(!rec_year %in% c("2018","2019")) %>%
  dplyr::mutate(source="rockfish") %>%
  dplyr::select(-c(fishery, detection_method)) %>%
  ## assign recoveries to our spatial boxes. Some latitudes dont have associated longitudes, but they do have an associated stat area 
  dplyr::mutate(rec.area.code = case_when(Area == 620 ~ "EAPEN",   
                                          Area == 630 ~ "NWGOA",
                                          Long <= -147 & Long > -154 ~ "NWGOA", #"X630",
                                          Long <= -154 & Long > -159  ~ "WAPEN", #"X620",
                                          TRUE~ "FIX" )) %>%  # 5 fish from 2013 from ship Cape Kiwanda that do not have any location info. 
  filter(#!Missing_Fins %in% c("None", "none"), 
         !rec.area.code=="FIX", 
         !rec.year<2013) %>% #filter out recoveries where the adipose fin wasn't clipped, also filter out recoveries with no lcoatino info, there are only 4.   
  filter(tag_code %in% tag.dat$tag_code)

dat.trawl.rockfish.AK <- trawl.rockfish.AK %>% 
  left_join(.,tag.dat) %>%
  group_by(ID,brood_year,release.year,first.release.month,ocean.region,rec.year,rec.month,rec.area.code) %>% 
  dplyr::summarise(tot.count=length(rec.year)) %>%
  mutate(median.frac.samp = 1) %>% # FULL CENSUS MEANS COMPLETE DETECTION.
  mutate(est.numb = tot.count / median.frac.samp,fishery="rockfish.AK") %>% 
  rename(brood.year=brood_year,rel.year=release.year,rel.month=first.release.month,count=tot.count) %>%
  dplyr::select(ID,brood.year,rel.year,rel.month,ocean.region,fishery,rec.year,rec.month,
                rec.area.code,est.numb,count,median.frac.samp) %>%
  mutate(rec.month = as.numeric(rec.month), rec.year = as.numeric(rec.year))
  

## Write to File
ocean.recover.rockfish.AK.shoreside = list(dat=dat.trawl.rockfish.AK)

save(ocean.recover.rockfish.AK.shoreside,file=paste0(base.dir,"/spring-chinook-distribution/Processed Data/Ocean Recoveries Rockfish AK Shoreside ",RUN.TYPE," ",GROUP,loc_18,".RData"))	    

####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
#######---  FRESHWATER ---#################################################################
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################

setwd(paste0(base.dir,"/spring-chinook-distribution"))

#Merge recovery code names with areas used.
#### Assign recoveries into regions delineated by Weitkamp 2002 and 2010
recover.fresh$rec.area.code <- NA
# get rid of sample type =5 to avoid double counting
recover.fresh <- recover.fresh[recover.fresh$sample_type !=5,]

# Area, year, month, release group, fishery
fresh.all	<- recover.fresh %>% 
              group_by(tag_code=tag_code,
                      fishery,
                      rec.year,
                      rec.area.code = recovery_location_code,
                      sample.type=sample_type,
                      auxiliary) %>%
              summarise(count= length(tag_code), est.numb = sum(estimated_number,na.rm=T)) %>%
              arrange(tag_code,rec.year,fishery) %>%
              full_join(tag.dat,.)
  
################
##### ADD IN AUXILIARY DATA FROM TRINITY AND KLAMATH RIVERS
################

# nca.escape <- read.csv("./Processed Data/Klamath river recoveries/KTFALL.KOHMNLM.01Sept.csv")
# nca.escape <- nca.escape[nca.escape$HATCH_NAME=="TRH" | nca.escape$HATCH_NAME=="IGH",]
# 
# nca.escape$AGE_OLE <- nca.escape$AGE + 1
# #	nca.escape$AGE_OLE[nca.escape$HATCH_NAME=="TRH" | nca.escape$HATCH_NAME=="IGH"] <-  nca.escape$AGE_OLE[nca.escape$HATCH_NAME=="TRH" | nca.escape$HATCH_NAME=="IGH"] + 1 # Why is trinity hatchery different?
# nca.escape <- nca.escape[,c("TAG_CODE","BROOD_YR","RELE_YEAR","AGE_OLE","CWT_RELE","RIVER_ESC","HATCHLENS")]
# nca.escape$est.numb <- nca.escape$RIVER_ESC
# nca.escape$est.numb[which(nca.escape$RIVER_ESC < nca.escape$HATCHLENS)] <- nca.escape$HATCHLENS[which(nca.escape$RIVER_ESC < nca.escape$HATCHLENS)]
# 
# nca.escape$rec.year <- nca.escape$BROOD_YR + nca.escape$AGE_OLE -1
# nca.escape.trim <- nca.escape[,c("TAG_CODE","est.numb","rec.year")]
# colnames(nca.escape.trim)[1] <- "tag_code"
# 
# fresh.nca <- merge(tag.dat,nca.escape.trim)
# fresh.nca$rec.area.code <- NA
# fresh.nca$sample.type <- NA
# fresh.nca$count <- fresh.nca$est.numb / 3
# fresh.nca$fishery <- 9999
# 
# ## Merge count with release group
# fresh.merge.count	<- merge(tag.dat,fresh.1,by="tag_code")
# fresh.merge.count	<- fresh.merge.count[order(fresh.merge.count$tag_code,
#                                              fresh.merge.count$fishery,
#                                              fresh.merge.count$rec.year
# ),] #  fresh.merge.count$rec.month
# colnames(fresh.merge.count)[ncol(fresh.merge.count)]	<-	"count"
# 
# ## Merge with release group
# fresh.merge	<- merge(tag.dat,fresh.2,by="tag_code")
# fresh.merge	<- fresh.merge[order(fresh.merge$tag_code,fresh.merge$fishery,fresh.merge$rec.year),] #
# colnames(fresh.merge)[ncol(fresh.merge)]	<-	"est.numb"
# 
# fresh.all		<-	merge(fresh.merge.count,fresh.merge)
# 
# # Remove NCA recoveries from the RMIS database and replace with recoveries from Will S. Klamath dataset.
# fresh.nca <- fresh.nca[,colnames(fresh.all)]
# fresh.all <- fresh.all %>% filter(!ID %in% c("Irongate", "Irongate_large", "Trinity", "Trinity_large"))
# #fresh.all <- fresh.all[fresh.all$ocean.region!= "NCA",]
# fresh.all <- rbind(fresh.all,fresh.nca)

######################################################################################
# Make one file for determining time of river entry
# Area, year, month, release group, fishery
fresh.river	<- recover.fresh %>% 
                  group_by(tag_code,
                           fishery,
                           rec.year,
                           rec.month,
                           rec.area.code = recovery_location_code,
                           sample.type=sample_type,
                           auxiliary) %>%
                  summarise(est.numb=sum(estimated_number,na.rm=T)) %>%
                  left_join(tag.dat,.)



fresh.by.hatchery.river	<- fresh.river %>% ungroup() %>%
                                group_by(ID,
                                         brood.year=brood_year,
                                         rel.year=release.year,
                                         ocean.region = ocean.region,
                                         fishery,
                                         rec.year,
                                         rec.month,
                                         auxiliary) %>%
                                summarise(est.numb=sum(est.numb,na.rm=T))
  
######################################################################################
fresh.all		<-	fresh.all[fresh.all$sample.type !=5 &fresh.all$sample.type !=4,]
#fresh.all		<-	fresh.all[fresh.all$est.numb > 0,]

# Break the fresh recoveries into two groups.  One with estimated numbers, one without.
fresh.zero <- fresh.all[fresh.all$est.numb==0,]
fresh.pos <- fresh.all %>% filter(est.numb!=0)

fresh.pos$frac.samp<-fresh.pos$count/fresh.pos$est.numb
fresh.pos$frac.samp[fresh.pos$frac.samp > 1] <- 1

######################################################################################
# Deal with the positives first	
fresh.by.hatchery	<-	fresh.pos %>% 
                              group_by(
                                ID,
                                brood.year = brood_year,
                                rel.year = release.year,
                                #rel.month = first.release.month,
                                ocean.region = ocean.region,
                                fishery		= fishery,	
                                rec.year  = rec.year,
                                auxiliary) %>% 
                            summarise(est.numb=sum(est.numb,na.rm=T),
                                      count=sum(count,na.rm=T),
                                      median.frac.samp = median(frac.samp))
# Next the zeros.
fresh.by.hatchery.count.zero	<-	fresh.zero %>% 
    group_by(
      ID,
      brood.year = brood_year,
      rel.year = release.year,
      #rel.month = first.release.month,
      ocean.region = ocean.region,
      fishery		= fishery,	
      rec.year  = rec.year,
      auxiliary) %>% 
    summarise(count.zero=sum(count,na.rm=T))

fresh.w.fishery <- full_join(fresh.by.hatchery, fresh.by.hatchery.count.zero)    

# Eliminated a NA row
fresh.w.fishery <- fresh.w.fishery %>% filter(!is.na(ocean.region))

######################################################################################
#### COMBINE THE VARIOUS FISHERIES
######################################################################################

# Deal with the positives first	
fresh.consolidated	<-	
  fresh.w.fishery %>% group_by(ID,
                         brood.year,
                         rel.year,
                         #rel.month,
                         ocean.region,
                         rec.year,auxiliary) %>%
            summarise(est.numb=sum(est.numb,na.rm=T),
                      count = sum(count,na.rm=T),
                      median.frac.samp = median(median.frac.samp,na.rm=T),
                      count.zero = sum(count.zero,na.rm=T))

####################################################
####################################################
####################################################
####################################################
####################################################
### --- MAKE R DATA FOR USE IN ESTIMATION
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################

fresh.recover = list(#fresh.recover.simple=fresh.recover.simple,
  #fresh.recover.complex=fresh.recover.complex,
  dat.fish=fresh.w.fishery, # this is a data frame with the fisheries still included.
  dat.consolidated =fresh.consolidated, # This is summed across FW fisheries (including spawning)
  river.entry = fresh.by.hatchery.river) #

save(fresh.recover,file=paste0("./Processed Data/Fresh Recoveries ",RUN.TYPE," ",GROUP,loc_18,".RData"))	  

####################################################
#############################################

# Read in auxiliary data for Columbia River survival

UCOL.pit <- read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Columbia SAR data/UP_COL_SAR_11-29-22.csv"))
SNAK.pit <- read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Columbia SAR data/SNAKE_SAR_12-05-22.csv"))
MCOL.pit <- read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Columbia SAR data/MID_COL_SAR_11-29-22.csv"))

# Upper Columbia first
# Filter out only the recoveries that occur at Bonneville
UCOL.pit <- UCOL.pit %>% filter(grepl("to Bonneville",SAR_Reach))
# For each Group Description keep only the furthest upstream release.
# For some this will be release site.  For some this will be Rocky reach.  
# For some this will be McNary Dam
UCOL.pit <- UCOL.pit %>% 
    filter(grepl("Rocky|Release",SAR_Reach) | grepl("Wenatchee",GroupDescription) ) %>%
    mutate(area= "UCOL")
    

UCOL.pit %>% distinct(GroupDescription, MigrYr)

# Middle Columbia next
# Filter out only the recoveries that occur at Bonneville
MCOL.pit <- MCOL.pit %>% filter(grepl("to Bonneville",SAR_Reach))
# For each Group Description keep only the furthest upstream release.
# For some this will be release site.  For some this will be Rocky reach.  
# For some this will be McNary Dam
MCOL.pit <- MCOL.pit %>% 
  filter(grepl("McNary|Release",SAR_Reach) | grepl("John",GroupDescription) ) %>%
  mutate(area= "MCOL")

# Get rid of duplicate Hanford data.
MCOL.pit <- MCOL.pit %>% 
  anti_join(.,data.frame(SAR_Reach="McNary Dam to Bonneville Dam",
                         GroupDescription ="Hanford Reach Wild Fall Chinook"))
  
MCOL.pit %>% distinct(GroupDescription, SAR_Reach)
MCOL.pit %>% distinct(GroupDescription)
  
# Snake last
# Filter out only the recoveries that occur at Bonneville
SNAK.pit <- SNAK.pit %>% filter(grepl("to Bonneville",SAR_Reach))
# For each Group Description keep only the furthest upstream release.
# For some this will be release site.  For most this will be Lower Granite 
# For one (Tucannon, this will be Lower Monumental)

SNAK.pit <- SNAK.pit %>% mutate(area="SNAK")

# Combine all
dat.pit <- bind_rows(SNAK.pit,MCOL.pit,UCOL.pit)


# summarize pit data
pit.summary <- dat.pit %>% group_by(area,GroupDescription,SAR_Reach) %>%
                    summarise(min.year  = min(MigrYr),
                              max.year=max(MigrYr),n.year = length(MigrYr))

pit.sar.data <- list(dat.pit = dat.pit,
                     pit.summary = pit.summary)

save(pit.sar.data,file=paste0("./Processed Data/PIT SAR ",RUN.TYPE," ",GROUP,loc_18,".RData"))	  


# 	Sample Type
# Must match one of the following:
# sample_type
# ’1’ =In-sample recoveries from a sampled fishery with known catch; estimated_number must be absent or greater than ‘0’
# ’2’=Voluntary recoveries from a sampled fishery with known catch; Awareness estimates are available;
# 		estimated_number must be absent or greater than ‘0’ (e.g., Puget Sound Sport)
# ’3’=Voluntary recoveries from an unsampled fishery. Awareness approximations may be possible yielding non-zero
# 		estimated_number; otherwise estimated_number should be absent. (e.g., Hoh River freshwater sport fishery)
# ’4’=In-sample or voluntary recoveries from a sampled fishery with unknown catch;
# 		estimated_number must be absent. (e.g., Stream Survey)
# ’5’=Voluntary or select recoveries from a sampled fishery with known catch and no awareness estimates available; Use of these
# 		recoveries leads to double counting; see also Note #3 to follow
# 		estimated_number must be equal to ‘0’. (e.g., commercial voluntary recoveries);
# ’6’=Mark Incidence – Indirect Sample: Voluntary recoveries from indirectly sampled sport fishery; estimated_number are calculated
# 		from sport_mark_inc_sampl_obs_ads in sport_mark_incidence_sampl_size from the corresponding Catch Sample record
# ’7’ =Pass-Through Sample: Recoveries that are selectively removed from certain in-river sampling programs; The migrant fish are
# 		subject to subsequent destination sampling number_caught must equal number_sampled. see also Note #3 to follow
# 
# Notes for sample_type: (see also notes for Catch/Sample sample_type field #18)
# 1) Four keys are used to distinguish the type of sample:
# a) Sample: In-sample or Voluntary
# b) Fishery: Sampled or Unsampled
# c) Catch: Known or Unknown
# d) Awareness: Available or Unavailable
# 2) Awareness estimates (Sample Type Code 2) are based on current year’s data, while awareness approximations (Sample Type Code 3) are based on extrapolations of data from other periods or locations.
# 3) “Pass-through” Sampling (Sample Type Code 7) In certain sampling programs, some fish are released while selected fish are killed and snouts removed. The non-sampled fish are subject to subsequent destination sampling and the lack of reporting would result in underestimation of the tag codes. In this sampling situation, the number of fish pulled out of the pass-through equals the number sampled and generally gives an estimated number of 1.
# 4) Any associated Catch/Sample and Recovery records must have the same value of sample type.


# Estimation Level

# Level of resolution at which expansion is made; If present, must match one of the following:
# estimation_level
# ’2’=Level 2 (Sector)
# ’3’=Level 3 (Region)
# ’4’=Level 4 (Area)
# ’5’=Level 5 (Location)
# ’6’=Level 6 (Sub-Location)
# Must match the value in corresponding Catch/Sample data file estimation_level
# Required if estimated_number is greater than ’0’	


# Period Type

# Code to Indicate the type of time periods in which sampling occurred in the fishery / stratum for this tag recovery;
# period_type
# If present, must match one of the following:
# ’1’=Escapement period (across years possible)
# ’2’=Bi-weekly (statistical 2 week)
# ’3’=Semi-monthly (calendar)
# ’4’=Statistical month
# ’5’=Calendar month
# ’6’=Statistical week (beginning Monday)
# ’7’=Week (beginning Sunday)
# ’8’=Seasonal (Use for spring, summer, fall, or winter run periods)
# ’10’=Weekend (Saturday, Sunday & observed holiday(s))
# ’11’=Weekday (Monday – Friday excluding observed holiday(s))
# Required if sample_type is ’1’, ’2’, ’4’, or ’6’
# Required if period present;period_type and period must match that used in Catch/Sample data file for the given stratum

# Period

# Indicates the complete range of time in which sampling occurred in the fishery / stratum for this tag recovery; Possible Ranges:
# period
# n=’01’=Escapement period (across years possible)
# n=’01-26’=Bi-weekly (statistical 2 week)
# n=’01-24’=Semi-monthly (calendar)
# n=’01-12’=Statistical month
# n=’01-12’=Calendar month
# n=’01-54’=Statistical week (beginning Monday)
# n=’01-54’=Week (beginning Sunday)
# n=’01-04’=Seasonal periods ( 01=Spring, 02=Summer, 03=Fall, 04=Winter)
# n=’01-54’=Weekend beginning Saturday (or Friday if on observed holiday)
# n=’01-54’=Weekday beginning Monday (or first working day following observed holiday)
# Required to map across to sampling period range in the Catch/Sample data file
# Required if period_type present period_type and period must match that used in Catch/Sample data file
# for the given stratum	

#	write.csv(all.code.recoveries, "Fall Chinook consolidated recoveries.csv",row.names=F)

# 	RMIS “fishery” codes
# 
#         10      Ocean Troll (non-treaty)
#         11      Ocean Troll - Day Boat
#         12      Ocean Troll - Trip Boat
#         13      Ocean Troll - Freezer Boat
#         14      Ocean Troll - Ice Boat
#         15      Treaty Troll
#         16      Terminal Troll
#         17      Ocean Troll (treaty & non-treaty)
#         18      Aboriginal Troll
#         19      Other
#         20      Ocean Gillnet (non-treaty)
#         21      Columbia River Gillnet
#         22      Coastal Gillnet
#         23      Mixed Net and Seine
#         24      Freshwater Net
#         25      Commercial Seine
#         26      Terminal Seine
#         27      Freshwater Seine
#         28      Other Net
#         29      Other Seine
#         30      Aboriginal Seine
#         31      Aboriginal Gillnet
#         32      Aboriginal Mixed Net
#         33      Aboriginal Subsistence Net
#         34      Aboriginal Angler
#         39      Other Aboriginal
#         40      Ocean Sport
#         41      Sport (charter)
#         42      Sport (private)
#         43      Sport (jetty)
#         44      Columbia River Sport
#         45      Estuary Sport
#         46      Freshwater Sport
#         47      Freshwater Sport Snag
#         48      Terminal Sport
#         49      Other
#         50      Hatchery
#         51      Fish Screens
#         52      Fish Trap (freshwater)
#         53      Wild Broodstock Collection
#         54      Spawning Ground
#         55      Treaty Ceremonial
#         56      Treaty Subsistence
#         57      Mixed Wild Broodstock and Hatchery Returns
#         59      Other
#         60      Test Fishery Troll
#         61      Test Fishery Net
#         62      Test Fishery Seine
#         63      Test Fishery Trap
#         64      Test Fishery Unknown Multiple Gear
#         65      Dead Fish Survey
#         69      Other
#         70      Juvenile Sampling - Troll (marine)
#         71      Juvenile Sampling - Gillnet (marine)
#         72      Juvenile Sampling - Seine (marine)
#         73      Juvenile Sampling - Seine (freshwater)
#         74      Juvenile Sampling - trawl (marine)
#         79      Other
#         80      Hake Trawl Fishery, At-Sea component (CA/OR/WA)
#         800     Hake Trawl Fishery, Shoreside component (OR/WA)
#         802     Limited-Entry Rockfish Trawl (CA/OR/WA)
#         803     Limited-Entry Non-Hake Groundfish Trawl (CA/OR/WA)
#         804     Limited-Entry Sablefish Fixed Gear (CA/OR/WA)
#         805     State-Permitted Nearshore Grndfish Fishery (CA/OR)
#         81      Groundfish Observer (Gulf of Alaska)
#         812     Rockfish Fishery (Gulf of Alaska)
#         82      Groundfish Observer (Bering Sea/Aleutians)
#         83      Foreign Research Vessels
#         84      Foreign Mothership Vessels
#         85      Ocean Trawl By-Catch
#         86      Land Based Salmon
#         87      Squid Gillnet By-Catch
#         88      Juvenile Sampling - trawl
#         89      Other
#         90      Multiple Gear
#         91      PNP Cost Recovery
#         92      Columbia River Shad
#         93      Set-Line (Sturgeon)
#         94      Fish Trap (marine)
#         95      Confiscated
#         99      Other


#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
############################################
### KLAMATH, TRINITY SPECIFIC DATA from Will
############################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################

# 		BASIN           'KLAM' OR 'TRIN'
# 		TAG_CODE        (CWT CODE)
# 		RACE            'FALL' OR 'SPRING'
# 		HATCH_NAME      HATCHERY NAME
# 		RELE_NAME       RELEASE LOCATION NAME
# 		BROOD_YR        BROOD YEAR
# 		AGE             2, 3, 4, OR 5*
# 		  * I believe the aging convention used here is that in months 1-8, age = current year - brood year.  In months 9-12, age= current year - brood year + 1
# 		RELE_YEAR       RELEASE YEAR
# 		RELE_MONTH      RELEASE MONTH
# 		RELE_STAGE      'F', 'Y', OR 'Y+'
# 		TOT_RELE        TOTAL RELEASE
# 		CWT_RELE        # RELEASED WITH CWT AND CLIP
# 		AVGRELEGM       RELEASE AVERAGE GRAMS WEIGHT
# 		
# 		OCNALLIMP       ALL OCEAN IMPACTS
# 		
# 		PREOCALIMP      OCEAN IMPACTS BEFORE AVERAGE DATE OF RIVER ENTRY
# 		(SPORT + COMMERCIAL;  CWT IMPACTS + NON-LANDED MORTALITIES)
# 		
# 		POSOCALIMP      OCEAN IMPACTS BEGINNING AVERAGE DATE OF RIVER ENTRY
# 		
# 		PREC_OTAGS      OCEAN COMMERCIAL CWT IMPACTS BEFORE AVERAGE DATE OF RIVER ENTRY
# 		PREC_ONLM       OCEAN COMMERCIAL NON-LANDED MORTALITIES BEFORE AVERAGE DATE OF RIVER ENTRY
# 		
# 		PRES_OTAGS      OCEAN SPORT CWT IMPACTS BEFORE AVERAGE DATE OF RIVER ENTRY
# 		PRES_ONLM       OCEAN SPORT NON-LANDED MORTALITIES BEFORE AVERAGE DATE OF RIVER ENTRY
# 		
# 		POSC_OTAGS      OCEAN COMMERCIAL CWT IMPACTS FROM AVERAGE DATE OF RIVER ENTRY ON
# 		POSC_ONLM       OCEAN COMMERCIAL NON-LANDED MORTALITIES FROM AVERAGE DATE OF RIVER ENTRY ON
# 		
# 		POSS_OTAGS      OCEAN SPORT CWT IMPACTS FROM AVERAGE DATE OF RIVER ENTRY ON
# 		POSS_ONLM       OCEAN SPORT NON-LANDED MORTALITIES FROM AVERAGE DATE OF RIVER ENTRY ON
# 		
# 		RIVER_ESC       RIVERNET+RIVERSPORT+RIVNATURAL+HATCHERY+FISHKILL
# 		
# 		RIVERNET        TRIBAL HARVEST + ESTIMATED NON-LANDED MORTALITIES
# 		RIVERSPORT      SPORT HARVEST + ESTIMATED NON-LANDED MORTALITIES
# 		RIVNATURAL      ESTIMATED NUMBER SPAWNING IN RIVER
# 		HATCHERY        TOTAL RETURN TO HATCHERY
# 		FISHKILL        MORTALITIES IN LOWER KLAMATH YEAR 2002
# 		
# 		RIVEROTH        RETURNS TO RIVER SYSTEMS OUTSIDE KLAMATH-TRINITY BASIN
# 		
# 		HATCHLENS       NUMBER OF MEASUREMENTS OF FISH RETURNING TO HATCHERY
# 		AVGLENMM        AVERAGE LENGTH OF FISH RETURNING TO HATCHERY
# 		VARLENMM        VARIANCE OF LENGTHS OF FISH RETURNING TO HATCHERY
# 		STDEVLENMM      STANDARD DEVIATION OF LENGTHS OF FISH RETURNING TO HATCHERY
# 		
# 		COHORTFLAG      'C'  = COMPLETED COHORT
# 		'ID' = INCOMPLETE COHORT, YEAR WITH    POTENTIAL DATA
# 		'IN' = INCOMPLETE COHORT, YEAR WITHOUT POTENTIAL DATA


### ADCLIP SELECTIVE FISHERY
    
# 'S' = Yes /adclip selective fishery
# 'M' = Yes /mixed selective fishery (ad-clipped plus unclipped fish); see note below
# 'N' = Not selective

if(is.null(ERROR) == F){ print(ERROR)}
  
