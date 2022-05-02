library(RColorBrewer)
library(tidyverse)
library(gtools)
library(rstan)
library(reshape2)
library(MASS)
library(extrafont)
library(gtable)
set.seed(211221)
#test
rm(list=ls())
gc()
NAME <- "DDD GONE(1400) FINAL DIRI=100 phi=3, adt=9 d=0.5 TWO_OR Mfix CLIMATE, Mproc error-FOUR LOGIT-vuln Troll_Rec_Treaty_Trawl_SS+PROC_SMOOTH_effort-SLOPE-Q_07-01-2020-24mon-EXTRA F(0.1)"
MOD.NAME  <- 'climate model GAMMA - quadratic V LOCVAR2.stan'

SAMP.FILE <- paste0("/Users/ole.shelton/GitHub/Salmon-Climate/Output files/",NAME,".csv")

### This is the definitions file for running the spatial statistical model for salmon.
RUN.TYPE  <- "spring-summer" # options: "fall" or "spring-summer"
GROUP     <- "FRAM_v2" ## Define the data file to access options include "CA+COL" "CA+COL+PUSO", others.
SHORT     <- "NO" # This is an indicator variable which is now mostly irrelevant.
loc_18    <- "TWO_OR"  # Options: "TRUE", "TWO_OR", "NCA_SOR_PUSO"

SPAWN = "SMOOTH" # Options are "SMOOTH" or "BLOCK" (Block is original formulation)

TRAWL.US  <- "TRUE" # This is a switch to include(if == TRUE) trawl fisheries from the US west Coast (hake fleets at present)
TRAWL.AK  <- "TRUE" # This is a switch to include(if == TRUE) trawl fisheries from Alaska (pollock fleets)
TRAWL.BC  <- "TRUE"

TRAWL_VULN_QUADRATIC <- "TRUE"

CLOGLOG   <- "FALSE" # This is a new option for making the vulnerability function a complementary log-log function as opposed to a logit link
  
# GROUPINGS FOR MONTHS
MONTH.STRUCTURE <- "FOUR" # Options: "FOUR"(original structure) or "FRAM" (follows the FRAM model schedule)
base.dir  <- "/Users/ole.shelton/GitHub"
print(base.dir)
# These are are the determining factors for defining the years and locations
YEARS.RELEASE     <- 1978:2010
YEARS.RECOVER     <- 1979:2015
YEARS.BROOD       <- 1977:2009

N_years_recover <- length(YEARS.RECOVER)
N_years_release <- length(YEARS.RELEASE)

if(loc_18 !="TRUE"){LOCATIONS <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations.csv",sep=""))}
if(loc_18 =="TRUE"){LOCATIONS <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations_plus.csv",sep=""))}
if(loc_18 =="TWO_OR"){LOCATIONS <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations_plus_two_OR.csv",sep=""))}
if(loc_18 =="NCA_SOR_PUSO"){LOCATIONS <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations_plus_NCA_SOR_PUSO.csv",sep=""))}

# print(paste(base.dir,"/Salmon-Climate/Processed Data/locations_plus_NCA_SOR_PUSO.csv",sep=""))

if(MONTH.STRUCTURE == "FOUR"){
  MONTH       <- c("month.winter","month.spring","month.summer","month.fall")
  SPRING.MONTH  <- c("month.04","month.05")
  SUMMER.MONTH  <- c("month.06","month.07")
  FALL.MONTH    <- c("month.08","month.09","month.10")
  WINTER.MONTH  <- c("month.01","month.02","month.03","month.11","month.12")
  MONTH.START <- 4 # for use with FOUR
  MONTH.SEASON.START <- "spring" # for use with FOUR
}
if(MONTH.STRUCTURE == "FRAM"){
  MONTH       <- c("month.winter","month.spring","month.summer","month.fall")
  SPRING.MONTH  <- c("month.02","month.03","month.04")
  SUMMER.MONTH  <- c("month.05","month.06")
  FALL.MONTH    <- c("month.07","month.08","month.09")
  WINTER.MONTH  <- c("month.01","month.10","month.11","month.12")
  MONTH.START <- 5 # for use with FRAM
  MONTH.SEASON.START <- "summer" # for use with FRAM
}

N_month <- length(MONTH)

# Define Release Groups and Ocean regions of interest.
OCEAN.REGION.RELEASE <-  c("ALL") # #c("SFB","COL","NCA","WAC")  # options: ALL or any of the names of the ocean regions (see LOCATIONS)

## NOT FUNCITONING YET OCEAN.RIVER.RELEASE  <- c("SFB")

#labeling crap
nom <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17")
if(loc_18 == "TRUE"){
  nom <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18")
}

# Define Gear Groups (NON-TRAWL fleets)
# These are drawn from the RMIS database.
GEAR <- c("Troll","Treaty Troll", "Sport","Gillnet & Seine & Other")
N.GEAR <- length(GEAR)
gear.func <- function(X,Y){
  if(X == "Troll"){FISHERY <- c(10,11,12,16,18)}
  if(X == "Treaty Troll"){FISHERY <- c(15)}
  if(X == "Gillnet & Seine & Other"){ FISHERY <- c(20, 22, 23, 25, 26, 28, 30, 31, 62,64,72,80,81,82,85,91,800,802,803,812) }
  if(X == "Sport"){FISHERY <- c(40, 41, 42, 43, 45)}
  THESE <- match(Y,FISHERY)
  return(THESE)
}

### Import Release Infomation from File
load(paste(base.dir,"/Salmon-Climate/Processed Data/Releases ",RUN.TYPE," ",GROUP,".RData",sep=""))
# important is: "releases" # This is made by "CWT identify, fetch releases..."

### Import the CWT recovery data from the ocean and freshwater
if(loc_18 != "TWO_OR" & loc_18 != "NCA_SOR_PUSO" &loc_18 != "TRUE"){
  load(paste(base.dir,"/Salmon-Climate/Processed Data/Ocean Recoveries ",RUN.TYPE," ",GROUP,".RData",sep=""))
  load(paste(base.dir,"/Salmon-Climate/Processed Data/Fresh Recoveries ",RUN.TYPE," ",GROUP,".RData",sep=""))
}
if(loc_18 == "TRUE"){
  load(paste(base.dir,"/Salmon-Climate/Processed Data/Ocean Recoveries ",GROUP,"_18loc.RData",sep=""))
  load(paste(base.dir,"/Salmon-Climate/Processed Data/Fresh Recoveries ",GROUP,"_18loc.RData",sep=""))
  }
if(loc_18 == "TWO_OR"){
  load(paste(base.dir,"/Salmon-Climate/Processed Data/Ocean Recoveries ",RUN.TYPE," ",GROUP,"_two_OR_PUSO.RData",sep=""))
  load(paste(base.dir,"/Salmon-Climate/Processed Data/Ocean Recoveries Trawl ",RUN.TYPE," ",GROUP,"_two_OR_PUSO.RData",sep=""))
  load(paste(base.dir,"/Salmon-Climate/Processed Data/Fresh Recoveries ",RUN.TYPE," ",GROUP,"_two_OR_PUSO.RData",sep=""))
}
if(loc_18 == "NCA_SOR_PUSO"){
  load(paste(base.dir,"/Salmon-Climate/Processed Data/Ocean Recoveries ",RUN.TYPE," ",GROUP,"_NCA_SOR_PUSO.RData",sep=""))
  load(paste(base.dir,"/Salmon-Climate/Processed Data/Ocean Recoveries Trawl ",RUN.TYPE," ",GROUP,"_NCA_SOR_PUSO.RData",sep=""))
  load(paste(base.dir,"/Salmon-Climate/Processed Data/Fresh Recoveries ",RUN.TYPE," ",GROUP,"_NCA_SOR_PUSO.RData",sep=""))
}

#important is: "fresh.recover"

### Make the rec effort file from the US coast data and Canada
source(paste(base.dir,"/Salmon-Climate/_R code for processing raw data/Make Rec effort files CLIMATE.r",sep=""),local=T)
### important file frame is "effort.rec" (used below)

### Make the troll effort file from the Alaska, BC, and US coast data
source(paste(base.dir,"/Salmon-Climate/_R code for processing raw data/Make troll effort files CLIMATE.r",sep=""),local=T)
### important file frame is "effort" (used below)

### Make the treaty troll effort file from the Alaska, BC, and US coast data
source(paste(base.dir,"/Salmon-Climate/_R code for processing raw data/Make treaty troll effort files CLIMATE.r",sep=""),local=T)
### important file frame is "effort.treaty" (used below)

### Make the ashop effort file and SAMPLE FRACTION from the effort data that has been gathered from ASHOP.  
TRIM.ASHOP <- 1990 # Year (inclusive) before which all effort and sampling data is set to 0
source(paste(base.dir,"/Salmon-Climate/_R code for processing raw data/Make ashop effort files CLIMATE.r",sep=""),local=T)

### important file frame is "ashop.effort" and for sample fraction it is "ashop.sample.fraction" (used below)           

### Make the hake shoreside effort file from the effort data that I have already parced
source(paste(base.dir,"/Salmon-Climate/_R code for processing raw data/Make shoreside effort files CLIMATE.r",sep=""),local=T)
### Read in the hake effort from S. Anderson, incorporate into the Shoreside data from the US.
#source(paste(base.dir,"/Salmon-Climate/_R code for processing raw data/Make bc_hake effort files CLIMATE.r",sep=""),local=T)
### important file frame is "effort.shoreside" (used below)          

###################################################################################################
### Create Release Groups Matrix for all releases in a given year set and define first month modeled.
###################################################################################################

# Trim to only include data from a subset of release groups.
release.all <- releases$releases
release.all <- release.all[release.all$brood_year >= min(YEARS.BROOD) & release.all$brood_year <= max(YEARS.BROOD) ,]

release.all$ID <- as.character(release.all$ID)
release.all$ocean.region <- as.character(release.all$ocean.region)

REL.ALL <- release.all
REL.ALL$n.year <- REL.ALL$release_year - REL.ALL$brood_year
# exclude releases with brood_year==release_year
REL.ALL <- REL.ALL %>% filter(n.year>0)

if(MONTH.STRUCTURE=="FOUR"){
  REL.ALL$n.month <- 0
  REL.ALL$n.month <- 12 - REL.ALL$Median.month.release + 4  ## Start keeping track of fish in April (spring) of brood year+2 
  REL.ALL$n.month[REL.ALL$n.year == 2] <- 4 - REL.ALL$Median.month.release[REL.ALL$n.year == 2] ## Start keeping track of fish in April (spring) of year following release.
#  REL.ALL$n.month[REL.ALL$n.year == 0] <- 3 + 12 - REL.ALL$Median.month.release[REL.ALL$n.year == 0] ## Start keeping track of fish in April (spring) of year following release.
  REL.ALL$n.month[REL.ALL$n.month <= 1 ]  <- 1
}
if(MONTH.STRUCTURE=="FRAM"){
  REL.ALL$n.month <- 0
  REL.ALL$n.month <- 12 - REL.ALL$Median.month.release + 5  ## Start keeping track of fish in May (summer) of brood year+2 
  REL.ALL$n.month[REL.ALL$n.year == 2] <- 5 - REL.ALL$Median.month.release[REL.ALL$n.year == 2] ## Start keeping track of fish in May (summer) of year following release.
 # REL.ALL$n.month[REL.ALL$n.year == 0] <-  + 12 - REL.ALL$Median.month.release[REL.ALL$n.year == 0]  
  REL.ALL$n.month[REL.ALL$n.month <= 1 ]  <- 1
}

N.REL.all <- nrow(REL.ALL) # THIS IS THE TOTAL NUMBER OF RELEASES WE ARE EXAMINING.
N.LOC <- max(LOCATIONS$location.number)

# naming vector
#nom <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17")

###################################################################################################
### Create Ocean Troll Effort and Ocean Rec effort (S. US ONLY AND SGEO) (does not link to releases)
###################################################################################################
### These are the Effort Arrays
MONTH.reorder <- c("month.spring", "month.summer", "month.fall", "month.winter") 

K.rec     <- array(0,dim=c(length(YEARS.RECOVER),length(MONTH),N.LOC),dimnames = list(YEARS.RECOVER,MONTH.reorder,paste("loc",nom,sep=".")))
K         <- array(0,dim=c(length(YEARS.RECOVER),length(MONTH),N.LOC),dimnames = list(YEARS.RECOVER,MONTH.reorder,paste("loc",nom,sep=".")))
K.treaty  <- array(0,dim=c(length(YEARS.RECOVER),length(MONTH),N.LOC),dimnames = list(YEARS.RECOVER,MONTH.reorder,paste("loc",nom,sep=".")))
K.rec.can <- array(0,dim=c(length(YEARS.RECOVER),length(MONTH),N.LOC),dimnames = list(YEARS.RECOVER,MONTH.reorder,paste("loc",nom,sep=".")))
K.rec.can.irec <- array(0,dim=c(length(YEARS.RECOVER),length(MONTH),N.LOC),dimnames = list(YEARS.RECOVER,MONTH.reorder,paste("loc",nom,sep=".")))
K.hake.ashop  <- array(0,dim=c(length(YEARS.RECOVER),length(MONTH),N.LOC),dimnames = list(YEARS.RECOVER,MONTH.reorder,paste("loc",nom,sep=".")))
K.hake.shoreside  <- array(0,dim=c(length(YEARS.RECOVER),length(MONTH),N.LOC),dimnames = list(YEARS.RECOVER,MONTH.reorder,paste("loc",nom,sep=".")))

for(i in 1:length(YEARS.RECOVER)){
  temp <- effort[effort$year==YEARS.RECOVER[i],]
  temp <- temp[order(temp$area.numb),]
  K[i,,] <- t(as.matrix(temp[,c(MONTH.reorder)]))

  temp2 <- effort.rec[effort.rec$year==YEARS.RECOVER[i],]
  temp2 <- temp2[order(temp2$area.numb),]
  K.rec[i,,] <- t(as.matrix(temp2[,c(MONTH.reorder)]))

  temp3 <- effort.treaty[effort.treaty$year==YEARS.RECOVER[i],]
  temp3 <- temp3[order(temp3$area.numb),]
  K.treaty[i,,] <- t(as.matrix(temp3[,c(MONTH.reorder)]))

  temp4 <- effort.can.irec[effort.can.irec$year==YEARS.RECOVER[i],]
  temp4 <- temp4[order(temp4$area.numb),]
  K.rec.can.irec[i,,] <- t(as.matrix(temp4[,c(MONTH.reorder)]))

#  if(TRAWL.US == "TRUE"){
    temp6 <- effort.ashop[effort.ashop$year==YEARS.RECOVER[i],] %>% arrange(area.numb)
    temp6 <- temp6[order(temp6$area.numb),]
    K.hake.ashop[i,,] <- t(as.matrix(temp6[,c(MONTH.reorder)]))
    
    temp7 <- effort.shoreside[effort.shoreside$year==YEARS.RECOVER[i],] %>% arrange(area.numb)
    temp7 <- temp7[order(temp7$area.numb),]
    K.hake.shoreside[i,,] <- t(as.matrix(temp7[,c(MONTH.reorder)]))
#  }
}

if(loc_18=="TRUE"){
  K.rec.can[,,12:16] <- K.rec[,,12:16]
  K.rec[,,12:16]     <- 0
}else{
  K.rec.can[,,11:15] <- K.rec[,,11:15]
  K.rec[,,11:15]     <- 0
}

K_troll   <- K # for ease of use in the STAN program
K_rec     <- K.rec# for ease of use in the STAN program
K_rec_can <- K.rec.can# for ease of use in the STAN program
K_rec_can_irec <- K.rec.can.irec# for ease of use in the STAN program
K_treaty  <- K.treaty# for ease of use in the STAN program
K_hake_ashop <- K.hake.ashop
K_hake_shoreside <- K.hake.shoreside

###################################################################################################
# Make intermediate recovery files.
###################################################################################################
# Make helpers file for later use
# This file add a model age to all of the CWT recoveries and, for the trawl fleets, matches up the sampling fractions for each observation.
source(paste(base.dir,"/Salmon-Climate/_R code for processing raw data/Make intermediate recovery files CLIMATE.r",sep=""),local=T)

###################################################################################################
### Create lookup table for fishing morality parameters. [ define year-month-locations for which there were no catches in a particular gear.type ]
###################################################################################################
# Add in trawl fleets to gear list here.
source(paste0(base.dir,"/Salmon-Climate/_R code for processing raw data/Make flat files for effort including missing effort.R"),local=T)

###################################################################################################
### Create lookup table for fishing morality parameters. [ define year-month-locations for which there were no catches in a particular gear.type ]
###################################################################################################
#log.A <- array(0,dim=c(length(YEARS.RECOVER),length(MONTH),N.LOC,N.GEAR),dimnames = list(YEARS.RECOVER,MONTH,paste("loc",nom,sep="."),GEAR))

QQ <- ocean.recover$dat %>% filter(rec.year <= max(YEARS.RECOVER), brood.year <= max(YEARS.BROOD)) %>%
        group_by(fishery.type, rec.year, rec.month,rec.area.code,model.age) %>% summarise(NUMB=sum(est.numb))
QQ <- merge(XX[,c("lab","model.age")],QQ)

if(MONTH.STRUCTURE=="FOUR"){
  QQ$rec.year[QQ$lab=="month.winter" & QQ$rec.month <= 3] <- QQ$rec.year[QQ$lab=="month.winter" & QQ$rec.month <=3] -1
}
if(MONTH.STRUCTURE=="FRAM"){
  QQ$rec.year[QQ$lab=="month.winter" & QQ$rec.month <= 1] <- QQ$rec.year[QQ$lab=="month.winter" & QQ$rec.month <=1] -1
}


RR <- QQ %>% group_by(fishery.type, rec.year, lab,rec.area.code) %>% summarise(N=sum(NUMB)) %>% as.data.frame()
RR$indicator <- 1

ZZ <- expand.grid(rec.year=YEARS.RECOVER,lab=MONTH,rec.area.code=LOCATIONS$location.name)
ZZ$month.lab[ZZ$lab == "month.spring"] <- 1
ZZ$month.lab[ZZ$lab == "month.summer"] <- 2
ZZ$month.lab[ZZ$lab == "month.fall"]   <- 3
ZZ$month.lab[ZZ$lab == "month.winter"] <- 4

ZZ$loc.numb <- LOCATIONS$location.number[match(ZZ$rec.area.code,LOCATIONS$location.name)]

ZZ.nets   <- merge(ZZ,RR[RR$fishery.type == "Gillnet & Seine & Other",],all=T)
ZZ.troll  <- merge(ZZ,RR[RR$fishery.type == "Troll",],all=T)
ZZ.treaty <- merge(ZZ,RR[RR$fishery.type == "Treaty Troll",],all=T)
ZZ.rec    <- merge(ZZ,RR[RR$fishery.type == "Sport",],all=T)
ZZ.ashop  <- merge(ZZ,RR[RR$fishery.type == "ashop",],all=T) 
ZZ.shoreside  <- merge(ZZ,RR[RR$fishery.type == "shoreside",],all=T) 

ZZ.nets   <- ZZ.nets[is.na(match(ZZ.nets$rec.year,YEARS.RECOVER))==F,]
ZZ.troll  <- ZZ.troll[is.na(match(ZZ.troll$rec.year,YEARS.RECOVER))==F,]
ZZ.treaty <- ZZ.treaty[is.na(match(ZZ.treaty$rec.year,YEARS.RECOVER))==F,]
ZZ.rec    <- ZZ.rec[is.na(match(ZZ.rec$rec.year,YEARS.RECOVER))==F,]
ZZ.ashop     <- ZZ.ashop[is.na(match(ZZ.ashop$rec.year,YEARS.RECOVER))==F,]
ZZ.shoreside <- ZZ.shoreside[is.na(match(ZZ.shoreside$rec.year,YEARS.RECOVER))==F,]

ZZ.troll  <- ZZ.troll[order(ZZ.troll$rec.year,ZZ.troll$month.lab,ZZ.troll$loc.numb),]
ZZ.treaty <- ZZ.treaty[order(ZZ.treaty$rec.year,ZZ.treaty$month.lab,ZZ.treaty$loc.numb),]
ZZ.rec    <- ZZ.rec[order(ZZ.rec$rec.year,ZZ.rec$month.lab,ZZ.rec$loc.numb),]
ZZ.nets   <- ZZ.nets[order(ZZ.nets$rec.year,ZZ.nets$month.lab,ZZ.nets$loc.numb),]
ZZ.ashop  <- ZZ.ashop[order(ZZ.ashop$rec.year,ZZ.ashop$month.lab,ZZ.ashop$loc.numb),]
ZZ.shoreside   <- ZZ.shoreside[order(ZZ.shoreside$rec.year,ZZ.shoreside$month.lab,ZZ.shoreside$loc.numb),]

ZZ.troll$indicator[is.na(ZZ.troll$indicator)==T] <- 0
ZZ.treaty$indicator[is.na(ZZ.treaty$indicator)==T] <- 0
ZZ.rec$indicator[is.na(ZZ.rec$indicator)==T] <- 0
ZZ.nets$indicator[is.na(ZZ.nets$indicator)==T] <- 0
ZZ.ashop$indicator[is.na(ZZ.ashop$indicator)==T] <- 0
ZZ.shoreside$indicator[is.na(ZZ.shoreside$indicator)==T] <- 0

#make flat files.  These simply indicate whether there were CWT fish recovered in this area-time combination or not. (1 or 0)
A_troll_flat  <-  dcast(ZZ.troll,rec.year+lab+month.lab~loc.numb,value.var = "indicator") %>% arrange(rec.year,month.lab)
A_treaty_flat <-  dcast(ZZ.treaty,rec.year+lab+month.lab~loc.numb,value.var = "indicator") %>% arrange(rec.year,month.lab)
A_rec_flat    <-  dcast(ZZ.rec,rec.year+lab+month.lab~loc.numb,value.var = "indicator") %>% arrange(rec.year,month.lab)
A_hake_ashop_flat        <-  dcast(ZZ.ashop,rec.year+lab+month.lab~loc.numb,value.var = "indicator") %>% arrange(rec.year,month.lab)
A_hake_shoreside_flat    <-  dcast(ZZ.shoreside,rec.year+lab+month.lab~loc.numb,value.var = "indicator") %>% arrange(rec.year,month.lab)

# A_net_flat    <-  dcast(ZZ.nets[ZZ.nets$rec.area.code !="CookInW" & ZZ.nets$rec.area.code !="PWS", ], 
#                        rec.year+lab+month.lab~loc.numb,value.var = "indicator") %>% arrange(rec.year,month.lab)

# Start model in spring so drop first winter row

first.year = min(YEARS.RECOVER)
last.year=max(YEARS.RECOVER)
last.season="month.fall"
last.row = which(A_troll_flat$rec.year == last.year & A_troll_flat$lab==last.season) 

if(MONTH.STRUCTURE=="FOUR"){
  first.season = "month.spring"
  first.row = which(A_troll_flat$rec.year == first.year & A_troll_flat$lab==first.season) 
}
if(MONTH.STRUCTURE=="FRAM"){
  first.season = "month.summer"
  first.row = which(A_troll_flat$rec.year == first.year & A_troll_flat$lab==first.season) 
}
  A_troll_flat   <- A_troll_flat[first.row:last.row,]
  A_rec_flat     <- A_rec_flat[first.row:last.row,]
  A_treaty_flat  <- A_treaty_flat[first.row:last.row,]
  #A_net_flat     <- A_net_flat[first.row:last.row,]
  A_hake_ashop_flat     <- A_hake_ashop_flat[first.row:last.row,]
  A_hake_shoreside_flat <- A_hake_shoreside_flat[first.row:last.row,]
  
rownames(A_troll_flat)  <- paste(A_troll_flat$rec.year,A_troll_flat$lab,sep=".")
rownames(A_rec_flat)    <- paste(A_rec_flat$rec.year,A_rec_flat$lab,sep=".")
rownames(A_treaty_flat) <- paste(A_treaty_flat$rec.year,A_treaty_flat$lab,sep=".")
#rownames(A_net_flat)    <- paste(A_net_flat$rec.year,A_net_flat$lab,sep=".")
rownames(A_hake_ashop_flat)     <- paste(A_hake_ashop_flat$rec.year,A_hake_ashop_flat$lab,sep=".")
rownames(A_hake_shoreside_flat) <- paste(A_hake_shoreside_flat$rec.year,A_hake_shoreside_flat$lab,sep=".")

col.lab <- paste("loc.",nom,sep="")

# colnames(A_troll_flat)  <- paste(A_troll_flat$rec.year,A_troll_flat$lab,sep=".")
# colnames(A_rec_flat)    <- paste(A_rec_flat$rec.year,A_rec_flat$lab,sep=".")
# colnames(A_treaty_flat) <- paste(A_treaty_flat$rec.year,A_treaty_flat$lab,sep=".")
# colnames(A_net_flat)    <- paste(A_net_flat$rec.year,A_net_flat$lab,sep=".")
# 
# A_troll_flat  <- A_troll_flat %>% dplyr::select(-rec.year,-lab,-month.lab)
# A_rec_flat    <- A_rec_flat %>% dplyr::select(-rec.year,-lab,-month.lab)
# A_treaty_flat <- A_treaty_flat %>% dplyr::select(-rec.year,-lab,-month.lab)
# A_net_flat    <- A_net_flat %>% dplyr::select(-rec.year,-lab,-month.lab)


### Compile the effort files and create a flat file for each gear type
# Add a 2D file type for all of the effort and fishing mortality types
base_mat <- matrix(0,N_years_recover * N_month,N.LOC)
K_troll_flat    <- base_mat
K_rec_flat      <- base_mat
K_rec_can_flat  <- base_mat
K_rec_can_irec_flat  <- base_mat
K_treaty_flat   <- base_mat
K_hake_ashop_flat    <- base_mat
K_hake_shoreside_flat<- base_mat

row.lab <- paste(sort(rep(YEARS.RECOVER,N_month)),rep(MONTH.reorder,N_years_recover),sep=".")
#row.lab <- row.lab[c(2:length(row.lab))]

for(i in 1:N.LOC){
  K_troll_flat[,i]    <- c(t(K_troll[,,i]))
  K_rec_flat[,i]      <- c(t(K_rec[,,i]))
  K_rec_can_flat[,i]  <- c(t(K_rec_can[,,i]))
  K_rec_can_irec_flat[,i] <- c(t(K_rec_can_irec[,,i]))
  K_treaty_flat[,i]    <- c(t(K_treaty[,,i]))
  K_hake_ashop_flat[,i]     <- c(t(K_hake_ashop[,,i]))
  K_hake_shoreside_flat[,i] <- c(t(K_hake_shoreside[,,i]))
}

K_rec_PUSO_flat <- base_mat 
K_rec_PUSO_flat[,c(grep("PUSO",LOCATIONS$location.name))] <- K_rec_flat[,c(grep("PUSO",LOCATIONS$location.name))]
K_rec_flat[,c(grep("PUSO",LOCATIONS$location.name))] <-0

###########################################################
###########################################################
##### MODIFY K_REC to try and eliminate CBC no effort
###########################################################
###########################################################
###########################################################
#K_rec_can_flat[,c(grep("CBC",LOCATIONS$location.name))] <- K_rec_can_flat[,c(grep("NWVI",LOCATIONS$location.name))]
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################

# K_troll_flat   <- K_troll_flat[c(2:nrow(K_troll_flat)),]
# K_rec_flat     <- K_rec_flat[c(2:nrow(K_rec_flat)),] 
# K_rec_can_flat <- K_rec_can_flat[c(2:nrow(K_rec_can_flat)),]
# K_rec_can_irec_flat <- K_rec_can_irec_flat[c(2:nrow(K_rec_can_irec_flat)),]
# K_rec_PUSO_flat <-K_rec_PUSO_flat[c(2:nrow(K_rec_PUSO_flat)),]
# K_treaty_flat  <- K_treaty_flat[c(2:nrow(K_treaty_flat)),]

rownames(K_troll_flat)  <- row.lab
rownames(K_rec_flat)    <- row.lab
rownames(K_rec_can_flat)<- row.lab
rownames(K_rec_can_irec_flat)<- row.lab
rownames(K_rec_PUSO_flat)  <- row.lab
rownames(K_treaty_flat) <- row.lab
rownames(K_hake_ashop_flat) <- row.lab
rownames(K_hake_shoreside_flat) <- row.lab

colnames(K_troll_flat)  <- col.lab
colnames(K_rec_flat)    <- col.lab
colnames(K_rec_can_flat)<- col.lab
colnames(K_rec_can_irec_flat)<- col.lab
colnames(K_rec_PUSO_flat)<- col.lab
colnames(K_treaty_flat) <- col.lab
colnames(K_hake_ashop_flat) <- col.lab
colnames(K_hake_shoreside_flat) <- col.lab

# trim all flat effort files to be the same size as the recovery files and match the A_ files
first.row<- which(rownames(K_troll_flat) == paste(first.year,first.season,sep="."))
last.row<- which(rownames(K_troll_flat) == paste(last.year,last.season,sep="."))

K_troll_flat      <- K_troll_flat[first.row:last.row,]
K_rec_flat        <- K_rec_flat[first.row:last.row,]
K_rec_can_flat    <- K_rec_can_flat[first.row:last.row,]
K_rec_can_irec_flat <- K_rec_can_irec_flat[first.row:last.row,]
K_rec_PUSO_flat   <- K_rec_PUSO_flat[first.row:last.row,]
K_treaty_flat     <- K_treaty_flat[first.row:last.row,]
K_hake_ashop_flat     <- K_hake_ashop_flat[first.row:last.row,]
K_hake_shoreside_flat <- K_hake_shoreside_flat[first.row:last.row,]

##########
N_season_total <- nrow(K_troll_flat)

# This is a way to break the ashop catchability into two parts - one with the foreign fleets, one with domestic.
ashop_year_break <- min(grep("1991",rownames(K_hake_ashop_flat)))
##########

###########################
# Find entries in troll file without effort data but observed catches. Compare the AA matrices with the appropriate K matrices.
K_troll_design <- K_troll_flat   
K_troll_design[K_troll_design > 0] <-1
temp <- (A_troll_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_troll_design
temp[temp<0] <-0
A_list_troll_trim <-NULL
for(i in 1:ncol(temp)){
  dat.temp <- which(temp[,i] == 1)
  A_list_troll_trim  <- rbind(A_list_troll_trim,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(A_list_troll_trim) <- c("year.month","loc.numb")
f_troll_param_idx <- A_list_troll_trim
N_f_troll_idx_param  = nrow(f_troll_param_idx)

### Enumerate which entries in the K_troll_flat > 0
f_troll_effort_idx  <- NULL
  for(i in 1:ncol(K_troll_flat)){
    dat.temp <- which(K_troll_flat[,i] > 0)
    f_troll_effort_idx  <- rbind(f_troll_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
  }
N_f_troll_effort_idx_param <- nrow(f_troll_effort_idx)

### Enumerate which entries in the K_treaty_flat > 0
K_treaty_design <- K_treaty_flat
K_treaty_design[K_treaty_design > 0] <-1
temp <- (A_treaty_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_treaty_design
temp[temp<0] <-0
A_list_treaty_trim <-NULL
for(i in 1:ncol(temp)){
  dat.temp <- which(temp[,i] == 1)
  A_list_treaty_trim  <- rbind(A_list_treaty_trim,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(A_list_treaty_trim) <- c("year.month","loc.numb")
f_treaty_param_idx <- A_list_treaty_trim
N_f_treaty_idx_param  = nrow(f_treaty_param_idx)

f_treaty_effort_idx  <- NULL
for(i in 1:ncol(K_treaty_flat)){
  dat.temp <- which(K_treaty_flat[,i] > 0)
  f_treaty_effort_idx  <- rbind(f_treaty_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
}
N_f_treaty_effort_idx_param <- nrow(f_treaty_effort_idx)

# Repeat for rec files 
# this is a file for all rec data  (includes effort in different units)
K_rec_all_flat <- K_rec_flat +K_rec_can_flat + K_rec_PUSO_flat+ K_rec_can_irec_flat

K_rec_design <- K_rec_flat + K_rec_PUSO_flat + K_rec_can_flat + K_rec_can_irec_flat
K_rec_design[K_rec_design > 0] <-1
temp <- (A_rec_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_rec_design
temp[temp<0] <-0
A_list_rec_trim <-NULL
for(i in 1:ncol(temp)){ # Remember that you deal with PUSO in a separate file effort matrix entirely.
  dat.temp <- which(temp[,i] == 1)
  A_list_rec_trim  <- rbind(A_list_rec_trim,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(A_list_rec_trim) <- c("year.month","loc.numb")

f_rec_param_idx <- A_list_rec_trim
N_f_rec_idx_param  = nrow(f_rec_param_idx)

### Enumerate which entries in the K_rec_can_flat > 0 and where K_rec_can_irec_flat >0
f_rec_overlap_effort_idx  <- NULL
K_temp1 <- K_rec_can_flat ; K_temp1[K_temp1>0] <- 1
K_temp2 <- K_rec_can_irec_flat; K_temp2[K_temp2>0] <- 1 
K_temp <- K_temp1 + K_temp2

for(i in 1:ncol(K_temp)){
  dat.temp <- which(K_temp[,i] ==2)
  f_rec_overlap_effort_idx  <- rbind(f_rec_overlap_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(f_rec_overlap_effort_idx) <- c("year.month","loc.numb")
N_f_rec_overlap_effort_idx_param <- nrow(f_rec_overlap_effort_idx)

### Enumerate which entries in the K_rec_flat > 0 and where K_rec_can_flat >0
f_rec_effort_idx  <- NULL
K_temp <- K_rec_flat +K_rec_PUSO_flat
for(i in 1:ncol(K_temp)){
  dat.temp <- which(K_temp[,i] > 0)
  f_rec_effort_idx  <- rbind(f_rec_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
}
N_f_rec_effort_idx_param <- nrow(f_rec_effort_idx)

f_rec_can_effort_idx  <- NULL
for(i in 1:ncol(K_rec_flat)){
  dat.temp <- which(K_rec_can_flat[,i] > 0)
  f_rec_can_effort_idx  <- rbind(f_rec_can_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
}
N_f_rec_can_effort_idx_param <- nrow(f_rec_can_effort_idx)

### Enumerate which entries in the K_hake_ashop_flat > 0
K_hake_ashop_design <- K_hake_ashop_flat
K_hake_ashop_design[K_hake_ashop_design > 0] <-1
temp <- (A_hake_ashop_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_hake_ashop_design
temp[temp<0] <-0
A_list_hake_ashop_trim <-NULL
for(i in 1:ncol(temp)){
  dat.temp <- which(temp[,i] == 1)
  A_list_hake_ashop_trim  <- rbind(A_list_hake_ashop_trim,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(A_list_hake_ashop_trim) <- c("year.month","loc.numb")
f_hake_ashop_param_idx <- A_list_hake_ashop_trim
N_f_hake_ashop_idx_param  = nrow(f_hake_ashop_param_idx)

# These are helper files for a feature that I no longer use
# f_hake_ashop_effort_idx  <- NULL
# for(i in 1:ncol(K_hake_ashop_flat)){
#   dat.temp <- which(K_hake_ashop_flat[,i] > 0)
#   f_hake_ashop_effort_idx  <- rbind(f_hake_ashop_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
# }
# N_f_hake_ashop_effort_idx_param <- nrow(f_hake_ashop_effort_idx)

### Enumerate which entries in the K_hake_shoreside_flat > 0
K_hake_shoreside_design <- K_hake_shoreside_flat
K_hake_shoreside_design[K_hake_shoreside_design > 0] <-1
temp <- (A_hake_shoreside_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_hake_shoreside_design
temp[temp<0] <-0
A_list_hake_shoreside_trim <-NULL
for(i in 1:ncol(temp)){
  dat.temp <- which(temp[,i] == 1)
  A_list_hake_shoreside_trim  <- rbind(A_list_hake_shoreside_trim,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(A_list_hake_shoreside_trim) <- c("year.month","loc.numb")
f_hake_shoreside_param_idx <- A_list_hake_shoreside_trim
N_f_hake_shoreside_idx_param  = nrow(f_hake_shoreside_param_idx)

# AT PRESENT THERE ARE NO LOCATIONS & TIMES IN WHICH SHORESIDE HAS OBSERVATIONS BUT NO EFFORT.  DON"T INCLUDE IT IN THE MODEL.

###################################################################################################
#### HERES IS WHERE WE DEFINE THE POPULATIONS OR  REGIONS OF INTEREST
###################################################################################################

#### TRIM RELEASE GROUPS, RECOVERIES TO REPRESENT A SUBSET OF AREAS SPECIFIED ABOVE
release.trim <- REL.ALL
if(OCEAN.REGION.RELEASE[1] != "ALL"){
  release.trim <- release.trim[is.na(match(release.trim$ocean.region,OCEAN.REGION.RELEASE))==F,]
  release.trim <- release.trim[release.trim$release_year >= min(YEARS.RELEASE) & release.trim$release_year <= max(YEARS.RELEASE) ,]
}
if(OCEAN.REGION.RELEASE[1] == "ALL"){
  release.trim <- release.trim[release.trim$release_year >= min(YEARS.RELEASE) & release.trim$release_year <= max(YEARS.RELEASE) ,]
}

### ALSO CREATE FOR SPECIFIC RIVERS as desired
REL <- release.trim
REL$ID_numb <- 1:nrow(REL) # This is the identifier for the unique release number.  This will be carried through the remainder of the analysis
N.REL <- max(REL$ID_numb) # THIS IS THE TOTAL NUMBER OF RELEASES WE ARE EXAMINING.

### MAKE SURE CONUMA is treated as a SWVI location, not NWVI.
REL$ocean.region[REL$ocean.region == "NWVI"] <- "SWVI"  

# This is a kluge to add more release locations that enter the ocean in the Columbia ocean region.  
# Could do something similar for other regions,if desired
REL$loc.numb  <- LOCATIONS$location.number[match(REL$ocean.region,LOCATIONS$location.name)]

if(loc_18 == "NCA_SOR_PUSO" ){
  val <- LOCATIONS$location.number[LOCATIONS$location.name=="NCA"]
  REL$loc.numb[REL$ocean.region=="SOR"]  <- val + 0.5
}
if(loc_18 == "TWO_OR" ){
  val <- LOCATIONS$location.number[LOCATIONS$location.name=="SOR"]
  REL$loc.numb[REL$ocean.region=="COR"]  <- val + 0.5
}

val <- LOCATIONS$location.number[LOCATIONS$location.name=="COL"]
REL$loc.numb[REL$ocean.region=="SAB"]   <- val + 0.05
REL$loc.numb[REL$ocean.region=="LCOL"]  <- val + 0.1
REL$loc.numb[REL$ocean.region=="MCOL"]  <- val + 0.25
#REL$loc.numb[REL$ocean.region=="UCOL"]  <- val + 0.5
REL$loc.numb[REL$ocean.region=="SNAK"]  <- val + 0.75
REL$loc.numb[REL$ocean.region=="URB"]   <- val + 0.90

val <- LOCATIONS$location.number[LOCATIONS$location.name=="PUSO"]
REL$loc.numb[REL$ocean.region=="PUSO_S"]  <- val + 0.25
REL$loc.numb[REL$ocean.region=="PUSO_N"]  <- val + 0.75

val <- LOCATIONS$location.number[LOCATIONS$location.name=="SGEO"]
REL$loc.numb[REL$ocean.region=="SGEO_S"]  <- val + 0.25
REL$loc.numb[REL$ocean.region=="SGEO_N"]  <- val + 0.75

REL$start_year <- match(REL$brood_year,YEARS.BROOD)


#### This script trims the included releases to only those that have reasonable numbers of ocean recoveries.
source(paste(base.dir,"/Salmon-Climate/_R code for processing raw data/Trim releases based on recoveries.r",sep=""),local=T)
#########

  REL$ID_numb <- 1:nrow(REL) # This is the identifier for the unique release number.  This will be carried through the remainder of the analysis
  N.REL <- max(REL$ID_numb) # THIS IS THE TOTAL NUMBER OF RELEASES WE ARE EXAMINING.

###################################################################################################
### Create Ocean Recovery Arrays and Escapement Arrays (there is some missing data in the Escapement array )
###################################################################################################
source(paste(base.dir,"/Salmon-Climate/_R code for processing raw data/Make catch and escapement files CLIMATE.r",sep=""),local=T)
# This is where C, Z_catch, Lambda2, and E arrays are created.
# They all have the form C[model.month,location,release.id,gear.type]
# This also creates the E matrix (fish that make it into the river (both to hatchery and caught in fisheries))
# tot.escape is the total number of fish observed in each release group.
C_rand_numb <- sort(sample(1:N.REL,3)) # 3 Catches to monitor for convergence.
# E_true is the observed number of fish for each release in each year.
# E_var and E_sd is the derived variance used in the likelihood by stan.
# recall that all of the escapements for NCA are assumed to be missing because they have crappy data.

C_troll_true  <- C[,,,"Troll"]
C_treaty_true <- C[,,,"Treaty Troll"]
C_rec_true    <- C[,,,"Sport"]
C_net_true    <- C[,,,"Gillnet & Seine & Other"]
C_hake_ashop_true <- C[,,,"ashop"]
C_hake_shoreside_true <- C[,,,"shoreside"]

# Lambda_troll_true  <- Lambda[,,,"Troll"]
# Lambda_treaty_true <- Lambda[,,,"Treaty Troll"]
# Lambda_rec_true    <- Lambda[,,,"Sport"]
# Lambda_net_true    <- Lambda[,,,"Gillnet & Seine & Other"]

# Ignore singleton observation from south of the columbia for net fisheries.
#C_net_true[,,1:8] <-0
# 

#### THIS LOOKS FOR RELEASES THAT HAVE NO OCEAN RECOVERIES AND PROVIDES A WAY TO REMOVE THEM FROM THE DATA 
  # ZZZ <-data.frame(cbind(apply(C_troll_true,c(1),sum),apply(C_rec_true,c(1),sum),apply(C_treaty_true,c(1),sum)))
  # ZZZ$tot <- rowSums(ZZZ)
  # ZZZ$N0 <- REL$N.released
  # ZZZ <- ZZZ[order(ZZZ$tot),]
  # ZZZ[1:50,]
  # 
  #   length(c(Z,FEW,TEN))
  #   length(ZZZ$tot[ZZZ$tot<10])

# C_troll_var <- Z_catch[,,,"Troll"] * Lambda2[,,,"Troll"] + Z_catch[,,,"Treaty Troll"] * Lambda2[,,,"Treaty Troll"] + 1
# C_rec_var   <- Z_catch[,,,"Sport"] * Lambda2[,,,"Sport"] + 1
# C_net_var   <- Z_catch[,,,"Gillnet & Seine & Other"] * Lambda2[,,,"Gillnet & Seine & Other"] + 1
# # Ignore singleton observation from south of the columbia
# C_net_var[,,1:8] <- 1
# 
# C_troll_sd <- sqrt(C_troll_var)
# C_rec_sd <- sqrt(C_rec_var)
# C_net_sd <- sqrt(C_net_var)

C_troll_pos <- C_troll_true
C_troll_pos[C_troll_pos > 0] <- 1
C_troll_zero <- abs(C_troll_pos - 1)

C_rec_pos <- C_rec_true
C_rec_pos[C_rec_pos > 0] <- 1
C_rec_zero <- abs(C_rec_pos - 1)

C_hake_ashop_pos <- C_hake_ashop_true
C_hake_ashop_pos[C_hake_ashop_pos > 0] <- 1
C_hake_ashop_zero <- abs(C_hake_ashop_pos - 1)

C_hake_shoreside_pos <- C_hake_shoreside_true
C_hake_shoreside_pos[C_hake_shoreside_pos > 0] <- 1
C_hake_shoreside_zero <- abs(C_hake_shoreside_pos - 1)

# C_net_pos <- C_net_true
# C_net_pos[C_net_pos > 0] <- 1
# C_net_zero <- abs(C_net_pos - 1)
# 
### OBSERVED CATCH by catch type:

####3 INITIAL RELEASES
N0 <- REL$N.released
####################################################################################################
# Create Movement matrices
max_age <- max(XX$model.year)

# Define Release Groups and Ocean regions of interest.
OCEAN.REGION.TEMP <- unique(REL$ocean.region)

move_id <- matrix(0,length(OCEAN.REGION.TEMP),2)
for(i in 1:length(OCEAN.REGION.TEMP)){
  if(OCEAN.REGION.TEMP[i]=="SFB"){temp <- 1}
  if(OCEAN.REGION.TEMP[i]=="NCA"){temp <- 2}
  if(OCEAN.REGION.TEMP[i]=="SOR"){temp <- 3}
  if(OCEAN.REGION.TEMP[i]=="COR"){temp <- 4}
  if(OCEAN.REGION.TEMP[i]=="NOR"){temp <- 5}
  if(OCEAN.REGION.TEMP[i]=="SAB"){temp <- 6}
  if(OCEAN.REGION.TEMP[i]=="LCOL"){temp <- 7}
  if(OCEAN.REGION.TEMP[i]=="MCOL"){temp <- 8}
  #if(OCEAN.REGION.TEMP[i]=="UCOL"){temp <- }
  if(OCEAN.REGION.TEMP[i]=="SNAK"){temp <- 9}
  if(OCEAN.REGION.TEMP[i]=="URB"){temp <- 10}
  if(OCEAN.REGION.TEMP[i]=="WAC"){temp <- 11}
  if(OCEAN.REGION.TEMP[i]=="PUSO_S"){temp <- 12}
  if(OCEAN.REGION.TEMP[i]=="PUSO_N"){temp <- 13}
  if(OCEAN.REGION.TEMP[i]=="SGEO_S"){temp <- 14}
  if(OCEAN.REGION.TEMP[i]=="SGEO_N"){temp <- 15}
  if(OCEAN.REGION.TEMP[i]=="SWVI"){temp <- 16}
  if(OCEAN.REGION.TEMP[i]=="NWVI"){temp <- 17}
  #if(OCEAN.REGION.TEMP[i]=="NWVI"){temp <- 14} # This makes NWVI identical to SWVI.
  move_id[i,] <- c(temp,as.character(OCEAN.REGION.TEMP[i]))
}

# Define the number of movement matrix stacks we need
move_id <- data.frame(move_id)
move_id$X1 <- as.numeric(as.character(move_id$X1))

move_id <- data.frame(move_id[order(move_id[,1]),])
colnames(move_id) <- c("move_id","ocean_region")
N.move.group  <- length(unique(move_id$move_id))
N.move.spawn  <- length(unique(REL$ocean.region))
move_id_idx   <- data.frame(move_id=as.character(unique(move_id$move_id)),move_id_idx=1:N.move.group)
move_id       <- merge(move_id,move_id_idx)
for(i in unique(REL$ocean.region)){
  # print(i)
  # print(move_id$loc.spawn[move_id$ocean_region == i])
  # print(unique(REL$loc.numb[REL$ocean.region==i]))
  move_id$loc.spawn[move_id$ocean_region == i] <- unique(REL$loc.numb[REL$ocean.region==i])
}
#move_id$loc.spawn <- sort(unique(REL$loc.numb))
move_id_name  <- as.character(unique(move_id$move_id))
move_id_spawn <- as.character((move_id$loc.spawn))
# redefine move_id_idx as a vector
move_id_idx <- move_id_idx$move_id_idx

# Map the correct origin location to the initial dispersal matrix
temp <- cbind(sort(unique(REL$loc.numb)),1:length(sort(unique(REL$loc.numb))))

REL$col.numb.init <- match(REL$loc.numb,temp[,1])
col.numb.init     <- REL$col.numb.init

############################################
# MATURITY and ESCAPEMENT
### Import the Dirichlet derived escapement data for each region.
# see CWT Maturity Proportion for this and Make catch and Escapement CLIMATE.R

source(paste(base.dir,"/Salmon-Climate/_R code for processing raw data/CWT Maturity Proportions CLIMATE.R",sep=""),local=T)
escape_diri <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/Escapement/Escape_Dirichlet_region ",RUN.TYPE," ",GROUP,".csv",sep=""))
escape_diri <- escape_diri[order(escape_diri$number),]
N.diri      <- nrow(escape_diri)
escape_diri$init.loc <- 1:nrow(escape_diri)
diri_constant <- 100

# Map the correct movement group their movement matrices
REL$move_idx <- move_id$move_id_idx[match(REL$ocean.region,move_id$ocean_region)]
move_idx    <- REL$move_idx

#source(paste(base.dir,"/Orca_Salmon_Code/_R code for processing raw data/Make files for movement matrices.r",sep=""))

# Length of time in each month grouping.
Trans_month <- aggregate(XX$model.age,by=list(XX$lab,XX$model.age),length)$x
ocean_age   <-      cumsum(Trans_month) - Trans_month/2

######## Create and index to determine run timing - when during the spawning season each release group jumps into the river to spawn.

if(MONTH.STRUCTURE == "FOUR"){
  REL$spawn_time_fraction <- 0.33 # Equivalent to September 1 Migration
}
if(MONTH.STRUCTURE == "FRAM"){
  REL$spawn_time_fraction <- 0.667   # equivalent to Spetember 1 migration
}

#################################################################################################
# READ IN ENVIRONMENTAL DATA FOR USE IN DISTIRBUTION
#################################################################################################
source(paste(base.dir,"/Salmon-Climate/Climate R Scripts/Ocean Temperatures.R",sep=""),local=T)
# Raw temperature data:
TEMP.DAT <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/Temperature/Temperature Deep.csv",sep=""))

# Temperature Deviations calculated from 3 seasons model (winter is treated as deviation from spring mean for identifiability reasons)
TEMP.DEV.DAT <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/Temperature/Temperature Deviations 4seas Deep.csv",sep=""))

  ocean.temp     <-   TEMP.DAT %>% filter(year <= max(YEARS.RECOVER))  
  #ocean.temp     <-  ocean.temp[2:nrow(ocean.temp),]
  ocean.temp.dev <-  TEMP.DEV.DAT %>% filter(year <= max(YEARS.RECOVER))  
  #ocean.temp.dev <-  ocean.temp.dev[2:nrow(ocean.temp.dev),]
  rownames(ocean.temp.dev) <- paste(ocean.temp.dev$year,ocean.temp.dev$season,sep=".")
  ocean.temp$season <- factor(ocean.temp$season,levels=c("Spr","Sum","Fal","Win"))
  ocean.temp.dev$season <- factor(ocean.temp.dev$season,levels=c("Spr","Sum","Fal","Win"))
  
  ocean.temp <- ocean.temp %>% arrange(year,season)
  ocean.temp.dev <- ocean.temp.dev %>% arrange(year,season) %>% as.data.frame()
  rownames(ocean.temp.dev) <- paste(ocean.temp.dev$year,ocean.temp.dev$season,sep=".")
  
  # This makes winter have a deviation to zero and all years before 1981 have a deviation of zero
  ocean.temp.dev[ocean.temp.dev$year <= 1981,3:ncol(ocean.temp.dev) ] <- 0
  ocean.temp.dev[ocean.temp.dev$season =="Win",3:ncol(ocean.temp.dev) ] <- 0 
  ocean.temp.dev <- ocean.temp.dev %>% dplyr::select(-year, -season)
  #ocean.temp.dev <- ocean.temp.dev[1:N_season_total,]
  
  ocean.temp.dev <- ocean.temp.dev*0.1 #### THIS IS REALLY IMPORTANT. REMEMBER TO RESCALE FUTURE PREDICTIONS by 0.1
  
  # Eliminate deviations from PUSO and SGEO
    #ocean.temp.dev$PUSO <- 0
    #ocean.temp.dev$SGEO <- 0
  
  if(loc_18 == "TRUE" | loc_18 =="TWO_OR" | loc_18=="NCA_SOR_PUSO"){
    #ocean.temp.dev$PUSO_out <- 0
    CHAR <- c(as.character(LOCATIONS$location.name))
    ocean.temp.dev <- ocean.temp.dev %>% dplyr::select(CHAR)
    ocean.temp <- ocean.temp %>% dplyr::select(year,season,CHAR)
  }
  
  #### MAKE A new index for working with temperature deviation data.
  REL$origin_start_year_idx <- 1+(REL$start_year - 1)*N_month  
  origin_year_idx <- matrix(seq(0,N.mod.month-1),N.REL,N.mod.month,byrow=T) + matrix(REL$origin_start_year_idx,N.REL,N.mod.month)

  if(MONTH.STRUCTURE =="FOUR"){
    first <- which(rownames(ocean.temp.dev)==paste(min(YEARS.RECOVER),"Spr",sep="."))
    last <- which(rownames(ocean.temp.dev)==paste(max(YEARS.RECOVER),"Fal",sep="."))
    ocean.temp.dev <- ocean.temp.dev[first:last,]
  }
  
  if(MONTH.STRUCTURE =="FRAM"){
    first <- which(rownames(ocean.temp.dev)==paste(min(YEARS.RECOVER),"Sum",sep="."))
    last <- which(rownames(ocean.temp.dev)==paste(max(YEARS.RECOVER),"Fal",sep="."))
    ocean.temp.dev <- ocean.temp.dev[first:last,]
  }
    
  temperature_season_idx    <- rep(0,nrow(ocean.temp.dev))
  temperature_season_idx[grep("Win",rownames(ocean.temp.dev))] <- 1
  temperature_season_idx[grep("Spr",rownames(ocean.temp.dev))] <- 1
  temperature_season_idx[grep("Sum",rownames(ocean.temp.dev))] <- 2
  temperature_season_idx[grep("Fal",rownames(ocean.temp.dev))] <- 3

#################################################################################################
### Create Priors for maturity, vulnerability, fishing mortality parameters
###################################################################################################

source(paste(base.dir,"/Salmon-Climate/_R code for processing raw data/Priors maturity, mortality, vuln, fishing CLIMATE.R",sep=""),local=T)
# important values are:
# MU_gamma, Sigma_gamma : multivariate normal values (3 param, gamma_0, gamma_age, gamma_lat) (for use with ocean ages 1:5 (recast so that the intercept is for age 5... aka ages = -4:0), latitude on 100s of km with 0 at northern limit)
# MU_m, Sigma_m : multivariate normal (2 param: m0 and m1) (for use with ocean months 1:50+, converted into annual mortality of ~ 0.5, 0.70, 0.85, 0.90
# MU_psi, Sigma_psi : multivariate normal values (3 param, psi_0, psi_1, psi_rec) (for use with ocean ages 1:5 (recast so that the intercept is for age 5... aka ages = -4:0), latitude on 100s of km with 0 at northern limit)

################################################################
## Define fixed mortality schedule.
################################################################
# Simulate time-varying and location varying natural mortality.
# M2  <- exp(MU_m[1] + MU_m[2]* mort_age)
# #M2      <- 0.3 / 12
# SD.M    <- 0.01*M2
# M2.t    <- rep(M2,N.mod.month)
# M2.loc  <- rgamma(N.LOC,1000,1000)
# M2.loc <- rep(1,N.LOC)
# 
# #M2.loc  <- rnorm(N.LOC,-0.5*SD.M^2,SD.M)
# M2.t.loc <- matrix(0,N.mod.month,N.LOC)
# for(i in 1:N.mod.month){
#   for(j in 1:N.LOC){
#     M2.t.loc[i,j] <- M2.t[i] * M2.loc[j] * Trans_month[i]
#   }
# }
spawn_loc <- escape_diri[,c("ocean.region","number","init.loc")]

#################################################
### Calculate the matrix that informs where fish can enter the river from the ocean.
source(paste0(base.dir,"/Salmon-Climate/_R code for processing raw data/Make river entry matrices.R"),local=T)

#### MAKE A DATA FILE FOR log_n_fin_ratio_data to ensure very few fish are left in the ocean at the end of the simulation
REL$log_N_ratio_mean <- -7
REL$log_N_ratio_sd   <- 1.5

#### READ IN SIZE LIMITS THAT DETERMINE VULNERABILITY

if(CLOGLOG == "TRUE"){
  vuln_fixed = 1.52718 ### this is the intercept for the vulnerability curve (on complementary log-log scale asymptotic vulnerability).
}
if(CLOGLOG == "FALSE"){
  vuln_fixed = 4.6 ### this is the intercept for the vulnerability curve (on logit scale, asymptotic vulnerability).
}

if(MONTH.STRUCTURE == "FOUR"){
  vuln.troll.mat  <-   read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/Vulnerability/vuln.troll.1978-2015.csv",sep=""))
  vuln.treaty.mat <-   read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/Vulnerability/vuln.treaty.1978-2015.csv",sep=""))
  vuln.rec.mat    <-   read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/Vulnerability/vuln.rec.1978-2015.csv",sep=""))
}
if(MONTH.STRUCTURE == "FRAM"){
  vuln.troll.mat  <-   read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/Vulnerability/vuln.troll.1978-2015_FRAM.csv",sep=""))
  vuln.treaty.mat <-   read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/Vulnerability/vuln.treaty.1978-2015_FRAM.csv",sep=""))
  vuln.rec.mat    <-   read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/Vulnerability/vuln.rec.1978-2015_FRAM.csv",sep=""))
}

if(loc_18 =="TWO_OR"){
  vuln.troll.mat  <-   vuln.troll.mat %>% mutate(SOR = (SOR + COR )/2) %>% dplyr::select(-COR)
  vuln.treaty.mat  <-   vuln.treaty.mat %>% mutate(SOR = (SOR + COR )/2) %>% dplyr::select(-COR)
  vuln.rec.mat  <-   vuln.rec.mat %>% mutate(SOR = (SOR + COR )/2) %>% dplyr::select(-COR)
}
if(loc_18 =="NCA_SOR_PUSO"){
  vuln.troll.mat  <-   vuln.troll.mat %>% mutate(NCA = (NCA + SOR )/2) %>% dplyr::select(-SOR) #%%>% rename(SOR=COR)
  vuln.treaty.mat  <-   vuln.treaty.mat %>% mutate(NCA = (NCA + SOR )/2) %>% dplyr::select(-SOR)#%>% rename(SOR=COR)
  vuln.rec.mat  <-   vuln.rec.mat %>% mutate(NCA = (NCA + SOR )/2) %>% dplyr::select(-SOR)#%>% rename(SOR=COR)
}
if(loc_18 != "TRUE" & loc_18 != "TWO_OR" & loc_18 != "NCA_SOR_PUSO"){
  vuln.rec.mat    <- vuln.rec.mat %>% dplyr::select(-PUSO_out)
  vuln.treaty.mat <- vuln.treaty.mat %>% dplyr::select(-PUSO_out)
  vuln.troll.mat  <- vuln.troll.mat %>% dplyr::select(-PUSO_out)
}

vuln.troll.mat  <- vuln.troll.mat %>% filter(Year %in% YEARS.RECOVER)
vuln.treaty.mat <- vuln.treaty.mat %>% filter(Year %in% YEARS.RECOVER)
vuln.rec.mat    <- vuln.rec.mat %>% filter(Year %in% YEARS.RECOVER)

vuln.troll.mat <- vuln.troll.mat[2:nrow(vuln.troll.mat),]
vuln.treaty.mat <- vuln.treaty.mat[2:nrow(vuln.treaty.mat),]
vuln.rec.mat <- vuln.rec.mat[2:nrow(vuln.rec.mat),]

# clip off the year and season columns
vuln_troll_mat  <- vuln.troll.mat %>% dplyr::select(-Year,-Season) * 0.01
vuln_treaty_mat <- vuln.treaty.mat %>% dplyr::select(-Year,-Season) * 0.01
vuln_rec_mat    <- vuln.rec.mat %>% dplyr::select(-Year,-Season) * 0.01

############################################################################################################
### Call script that creates matrices needed to make spatial distributions smooth.
############################################################################################################
source(paste(base.dir,"/Salmon-Climate/_R code for processing raw data/Make smooth ocean distribution matrices.R",sep=""),local=T)

######## PLOT EFFORT AND CPUE FILES
source(paste(base.dir,"/Salmon-Climate/_R code for processing raw data/Plot effort, CPUE heatmaps CLIMATE.R",sep=""),local=T)

############# MAKE A PDF of the various hatchery and release attributes.
setwd(paste(base.dir,"/Salmon-Climate/Output plots/  __Markdown",sep=""))
rmarkdown::render(paste(base.dir,"/Salmon-Climate/Output plots/  __Markdown/release-summaries.rmd",sep=""),
                  output_format = "pdf_document")
                  #output_file = paste(base.dir,"/Salmon-Climate/Output plots/"))

#### CALL THE ACTUAL STAN CODE:

source(paste0(base.dir,"/Salmon-Climate/Mixed Model/Mixed bin+pos SS + PROC logit prob_age CLIMATE ts-q SMOOTH.R"),local=T)


