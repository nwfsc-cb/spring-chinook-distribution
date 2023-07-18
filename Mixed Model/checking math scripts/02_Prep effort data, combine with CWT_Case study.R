library(RColorBrewer)
library(tidyverse)
library(gtools)
library(rstan)
library(cmdstanr)
  check_cmdstan_toolchain()
  #install_cmdstan()
library(reshape2)
library(MASS)
library(extrafont)
library(gtable)
library(here)
#set.seed(10)
#test
rm(list=ls())
#gc()

#base.dir  <- "/Users/ole.shelton/GitHub"
#print(base.dir)

NAME <- "Case study south split"
#MOD.NAME  <- 'climate model GAMMA - quadratic V LOCVAR2 NO SST_case study south_TEST.stan'
MOD.NAME  <- 'climate model GAMMA - quadratic V LOCVAR2 NO SST_case study south_GSI+CWT_phi_TEST.stan'

SAMP.FILE <- paste0("./Output files/",NAME,".csv")

### This is the definitions file for running the spatial statistical model for salmon.
RUN.TYPE  <- "case_study_south_split" # options: "fall", "spring-summer", "south", "case_study", "case_study_south", "case_study_south_split"
GROUP     <- "FRAM_v2" ## Define the data file to access options include "CA+COL" "CA+COL+PUSO", others.
SHORT     <- "NO" # This is an indicator variable which is now mostly irrelevant.
loc_18    <- "TWO_OR"  # Options: "TRUE", "TWO_OR", "NCA_SOR_PUSO"

SPAWN = "SMOOTH" # Options are "SMOOTH" or "BLOCK" (Block is original formulation)

TRAWL.US  <- "TRUE" # This is a switch to include(if == TRUE) trawl fisheries from the US west Coast (hake fleets at present)
TRAWL.AK  <- "FALSE" # This is a switch to include(if == TRUE) trawl fisheries from Alaska (pollock fleets)
TRAWL.BC  <- "FALSE"

TRAWL_VULN_QUADRATIC <- "TRUE"

CLOGLOG   <- "FALSE" # This is a new option for making the vulnerability function a complementary log-log function as opposed to a logit link
  
# GROUPINGS FOR MONTHS
MONTH.STRUCTURE <- "SPRING" # Options: "FOUR"(original structure) or 
                              # "FRAM" (follows the FRAM model schedule) or
                              # "SPRING" (follows FOUR but includes March in spring)

# These are are the determining factors for defining the years and locations
YEARS.RELEASE     <- 1978:2013#2010
YEARS.RECOVER     <- 1979:2018#2015
YEARS.BROOD       <- 1977:2012#2009

N_years_recover <- length(YEARS.RECOVER)
N_years_release <- length(YEARS.RELEASE)

if(loc_18 !="TRUE"){LOCATIONS <- read.csv("./Processed Data/locations.csv")}
if(loc_18 =="TRUE"){LOCATIONS <- read.csv("./Processed Data/locations_plus.csv")}
if(loc_18 =="TWO_OR"){LOCATIONS <- read.csv("./Processed Data/locations_plus_two_OR.csv")}
if(loc_18 =="NCA_SOR_PUSO"){LOCATIONS <- read.csv("./Processed Data/locations_plus_NCA_SOR_PUSO.csv")}
if(loc_18 =="SOUTH"){LOCATIONS <- read.csv("./Processed Data/locations_south.csv")}

if(RUN.TYPE=="case_study_south_split"){
  ORIGIN.LAB <- read.csv("./Processed Data/origin_labels_all_runs_split.csv") # new ocean.region names
} else {
  ORIGIN.LAB <- read.csv("./Processed Data/origin_labels_all_runs.csv")
}  
# print(paste(base.dir,"/GSI_CWT_Chinook/Processed Data/locations_plus_NCA_SOR_PUSO.csv",sep=""))

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
if(MONTH.STRUCTURE == "SPRING"){
  MONTH       <- c("month.winter","month.spring","month.summer","month.fall")
  SPRING.MONTH  <- c("month.03","month.04","month.05")
  SUMMER.MONTH  <- c("month.06","month.07")
  FALL.MONTH    <- c("month.08","month.09","month.10")
  WINTER.MONTH  <- c("month.01","month.02","month.11","month.12")
  MONTH.START <- 3 # for use with 
  MONTH.SEASON.START <- "spring" # for use with FOUR
}

N_month <- length(MONTH)

# Define Release Groups and Ocean regions of interest.
OCEAN.REGION.RELEASE <-  c("ALL") # #c("SFB","COL","NCA","WAC")  # options: ALL or any of the names of the ocean regions (see LOCATIONS)

## NOT FUNCTIONING YET OCEAN.RIVER.RELEASE  <- c("SFB")

#labeling crap
nom <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17")
#nom <- c("01","02","03","04","05","06","07","08","09","10")
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

### Import Release Information from File
load(paste0("./Processed Data/Releases ",RUN.TYPE," ",GROUP,".RData"))
# important is: "releases" # This is made by "CWT identify, fetch releases..."

### Import the CWT recovery data from the ocean and freshwater
if(loc_18 != "TWO_OR" & loc_18 != "NCA_SOR_PUSO" &loc_18 != "TRUE"){
  load(paste0("./Processed Data/Ocean Recoveries ",RUN.TYPE," ",GROUP,".RData"))
  load(paste0("./Processed Data/Fresh Recoveries ",RUN.TYPE," ",GROUP,".RData"))
}
if(loc_18 == "TRUE"){
  load(paste0("./Processed Data/Ocean Recoveries ",GROUP,"_18loc.RData"))
  load(paste0("./Processed Data/Fresh Recoveries ",GROUP,"_18loc.RData"))
  }
if(loc_18 == "TWO_OR"){
  load(paste0("./Processed Data/Ocean Recoveries ",RUN.TYPE," ",GROUP,"_two_OR_PUSO.RData"))
  load(paste0("./Processed Data/Ocean Recoveries Trawl ",RUN.TYPE," ",GROUP,"_two_OR_PUSO.RData"))
  load(paste0("./Processed Data/Fresh Recoveries ",RUN.TYPE," ",GROUP,"_two_OR_PUSO.RData"))
}
if(loc_18 == "NCA_SOR_PUSO"){
  load(paste0("./Processed Data/Ocean Recoveries ",RUN.TYPE," ",GROUP,"_NCA_SOR_PUSO.RData"))
  load(paste0("./Processed Data/Ocean Recoveries Trawl ",RUN.TYPE," ",GROUP,"_NCA_SOR_PUSO.RData"))
  load(paste0("./Processed Data/Fresh Recoveries ",RUN.TYPE," ",GROUP,"_NCA_SOR_PUSO.RData"))
}

#important is: "fresh.recover"

### Make the rec effort file from the US coast data and Canada
source("./Base_Code/_R code for processing raw data/Make Rec effort files CLIMATE.r",local=T)
### important file frame is "effort.rec" (used below)

### Make the troll effort file from the Alaska, BC, and US coast data
source("./Base_Code/_R code for processing raw data/Make troll effort files CLIMATE.r",local=T)
### important file frame is "effort" (used below)

### Make the treaty troll effort file from the Alaska, BC, and US coast data
source("./Base_Code/_R code for processing raw data/Make treaty troll effort files CLIMATE.r",local=T)
### important file frame is "effort.treaty" (used below)

### Make the ashop effort file and SAMPLE FRACTION from the effort data that has been gathered from ASHOP.  
TRIM.ASHOP <- 1990 # Year (inclusive) before which all effort and sampling data is set to 0
source("./Base_Code/_R code for processing raw data/Make ashop effort files CLIMATE.r",local=T)

### important file frame is "ashop.effort" and for sample fraction it is "ashop.sample.fraction" (used below)           

### Make the hake shoreside effort file from the effort data that I have already parced
source("./Base_Code/_R code for processing raw data/Make shoreside effort files CLIMATE.r",local=T)
### Read in the hake effort from S. Anderson, incorporate into the Shoreside data from the US.
#source(paste(base.dir,"/GSI_CWT_Chinook/_R code for processing raw data/Make bc_hake effort files CLIMATE.r",sep=""),local=T)
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
  # I WANT TO GET RID OF THIS FILTER (BELOW) = REMOVES LATEFALL FISH INCORRECTLY
REL.ALL <- REL.ALL %>% filter(n.year>0) # losing 28 releases, and mostly lfall and wild_spr

if(MONTH.STRUCTURE=="FOUR"){
  REL.ALL$n.month <- 0
  REL.ALL$n.month <- 12 - REL.ALL$Median.month.release + 4  ## Start keeping track of fish in April (spring) of brood year+2 
  REL.ALL$n.month[REL.ALL$n.year == 2] <- 4 - REL.ALL$Median.month.release[REL.ALL$n.year == 2] ## Start keeping track of fish in April (spring) of year following release.
#  REL.ALL$n.month[REL.ALL$n.year == 0] <- 3 + 12 - REL.ALL$Median.month.release[REL.ALL$n.year == 0] ## Start keeping track of fish in April (spring) of year following release.
  REL.ALL$n.month[REL.ALL$n.month <= 1 ]  <- 1
}
if(MONTH.STRUCTURE=="SPRING"){
  REL.ALL$n.month <- 0
  REL.ALL$n.month <- 12 - REL.ALL$Median.month.release + 3  ## Start keeping track of fish in March (spring) of brood year+2 
  REL.ALL$n.month[REL.ALL$n.year == 2] <- 3 - REL.ALL$Median.month.release[REL.ALL$n.year == 2] ## Start keeping track of fish in April (spring) of year following release.
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
source("./Base_Code/_R code for processing raw data/Make intermediate recovery files CLIMATE.r",local=T)

###################################################################################################
### Create lookup table for fishing morality parameters. [ define year-month-locations for which there were no catches in a particular gear.type ]
###################################################################################################
# Add in trawl fleets to gear list here.
source("./Base_Code/_R code for processing raw data/Make flat files for effort including missing effort_Case study.R",local=T)

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
#REL$loc.numb  <- LOCATIONS$location.number[match(REL$ocean.region,LOCATIONS$location.name)]

REL <- left_join(REL, ORIGIN.LAB %>% dplyr::select(origin.code,loc.numb), by=c("ocean.region"="origin.code")) 

#if(loc_18 == "NCA_SOR_PUSO" ){
#  val <- LOCATIONS$location.number[LOCATIONS$location.name=="NCA"]
#  REL$loc.numb[REL$ocean.region=="SOR"]  <- val + 0.5
#}
#if(loc_18 == "TWO_OR" ){
#  val <- LOCATIONS$location.number[LOCATIONS$location.name=="SOR"]
#  REL$loc.numb[REL$ocean.region=="COR"]  <- val + 0.5
#}

#val <- LOCATIONS$location.number[LOCATIONS$location.name=="COL"]
#REL$loc.numb[REL$ocean.region=="SAB"]   <- val + 0.05
#REL$loc.numb[REL$ocean.region=="LCOL"]  <- val + 0.1
#REL$loc.numb[REL$ocean.region=="MCOL"]  <- val + 0.25
#REL$loc.numb[REL$ocean.region=="UCOL"]  <- val + 0.5
#REL$loc.numb[REL$ocean.region=="SNAK"]  <- val + 0.75
#REL$loc.numb[REL$ocean.region=="URB"]   <- val + 0.90

#val <- LOCATIONS$location.number[LOCATIONS$location.name=="PUSO"]
#REL$loc.numb[REL$ocean.region=="PUSO_S"]  <- val + 0.25
#REL$loc.numb[REL$ocean.region=="PUSO_N"]  <- val + 0.75

#val <- LOCATIONS$location.number[LOCATIONS$location.name=="SGEO"]
#REL$loc.numb[REL$ocean.region=="SGEO_S"]  <- val + 0.25
#REL$loc.numb[REL$ocean.region=="SGEO_N"]  <- val + 0.75

REL$start_year <- match(REL$brood_year,YEARS.BROOD)


#### This script trims the included releases to only those that have reasonable numbers of ocean recoveries.
source("./Base_Code/_R code for processing raw data/Trim releases based on recoveries_South.r",local=T)
#########

  REL$ID_numb <- 1:nrow(REL) # This is the identifier for the unique release number.  This will be carried through the remainder of the analysis
  N.REL <- max(REL$ID_numb) # THIS IS THE TOTAL NUMBER OF RELEASES WE ARE EXAMINING.

###################################################################################################
### Create Ocean Recovery Arrays and Escapement Arrays (there is some missing data in the Escapement array )
###################################################################################################
source("./Base_Code/_R code for processing raw data/Make catch and escapement files CLIMATE_Case study.r",local=T)
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
if(RUN.TYPE=="case_study_south_split"){
  for(i in 1:length(OCEAN.REGION.TEMP)){
    if(OCEAN.REGION.TEMP[i]=="SFB"){temp <- 1}
    if(OCEAN.REGION.TEMP[i]=="CAC"){temp <- 2}
    if(OCEAN.REGION.TEMP[i]=="KLT"){temp <- 3}
    if(OCEAN.REGION.TEMP[i]=="NCASOR"){temp <- 4}
    move_id[i,] <- c(temp,as.character(OCEAN.REGION.TEMP[i]))
  }
} else {
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

source("./Base_Code/_R code for processing raw data/CWT Maturity Proportions CLIMATE_South.R",local=T)
escape_diri <- read.csv(paste0("./Processed Data/Escapement/Escape_Dirichlet_region ",RUN.TYPE," ",GROUP,".csv"))
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
if(MONTH.STRUCTURE == "SPRING"){
  # For fall. time_fraction = 0.33 is September 1 migration
  # For spring run, time_fraction = 0 is March 1, time_fraction = 0.33 is April 1 
  REL <- REL %>% left_join(., ORIGIN.LAB %>% dplyr::select(ocean.region=origin.code,spawn_time_fraction) )
}
if(MONTH.STRUCTURE == "FRAM"){
  REL$spawn_time_fraction <- 0.667   # equivalent to Spetember 1 migration
}

#################################################################################################
# READ IN ENVIRONMENTAL DATA FOR USE IN DISTIRBUTION
#################################################################################################
#source("./Base_Code/Climate R Scripts/Ocean Temperatures.R",local=T)
# Raw temperature data:
TEMP.DAT <- read.csv("./Processed Data/Temperature/Temperature Deep_2018.csv")

# Temperature Deviations calculated from 3 seasons model (winter is treated as deviation from spring mean for identifiability reasons)
TEMP.DEV.DAT <- read.csv("./Processed Data/Temperature/Temperature Deviations 4seas Deep_2018.csv")

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
    CHAR <- c(as.character(LOCATIONS_south$location.name))
    ocean.temp.dev <- ocean.temp.dev %>% dplyr::select(all_of(CHAR))
    ocean.temp <- ocean.temp %>% dplyr::select(year,season,CHAR)
  }
  
  #### MAKE A new index for working with temperature deviation data.
  REL$origin_start_year_idx <- 1+(REL$start_year - 1)*N_month  
  origin_year_idx <- matrix(seq(0,N.mod.month-1),N.REL,N.mod.month,byrow=T) + matrix(REL$origin_start_year_idx,N.REL,N.mod.month)

  if(MONTH.STRUCTURE =="FOUR"|MONTH.STRUCTURE=="SPRING"){
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

source("./Base_Code/_R code for processing raw data/Priors maturity, mortality, vuln, fishing CLIMATE.R",local=T)
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
  # THIS NEEDS TO BE FIXED FOR case_study_south_split - CAC has not escapement info

#################################################
### Calculate the matrix that informs where fish can enter the river from the ocean.
source("./Base_Code/_R code for processing raw data/Make river entry matrices_Case study.R",local=T)

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

if(MONTH.STRUCTURE == "FOUR" | MONTH.STRUCTURE == "SPRING"){
  vuln.troll.mat  <-   read.csv("./Processed Data/Vulnerability/vuln.troll.1978-2018.csv")[,1:11]
  vuln.treaty.mat <-   read.csv("./Processed Data/Vulnerability/vuln.treaty.1978-2018.csv")[,1:11]
  vuln.rec.mat    <-   read.csv("./Processed Data/Vulnerability/vuln.rec.1978-2018.csv")[,1:11]
}
if(MONTH.STRUCTURE == "FRAM"){
  vuln.troll.mat  <-   read.csv("./Processed Data/Vulnerability/vuln.troll.1978-2015_FRAM.csv")[,1:11]
  vuln.treaty.mat <-   read.csv("./Processed Data/Vulnerability/vuln.treaty.1978-2015_FRAM.csv")[,1:11]
  vuln.rec.mat    <-   read.csv("./Processed Data/Vulnerability/vuln.rec.1978-2015_FRAM.csv")[,1:11]
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
source("./Base_Code/_R code for processing raw data/Make smooth ocean distribution matrices_Case study.R",local=T)

######## PLOT EFFORT AND CPUE FILES
#source("./Base_Code/_R code for processing raw data/Plot effort, CPUE heatmaps CLIMATE.R",local=T)

############# MAKE A PDF of the various hatchery and release attributes.
#setwd(paste(base.dir,"/GSI_CWT_Chinook/Output plots/  __Markdown",sep=""))
#rmarkdown::render("./Output plots/  __Markdown/release-summaries.rmd",
#                  output_format = "pdf_document")
                  #output_file = "./Output plots/")

#### CALL THE ACTUAL STAN CODE:
#source(paste0(base.dir,"/GSI_CWT_Chinook/Base_Code/Mixed bin+pos SS + PROC logit prob_age CLIMATE ts-q SMOOTH.R"),local=T)


