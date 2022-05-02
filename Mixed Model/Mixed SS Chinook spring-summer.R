# DEFINE STATISTICAL MODEL OF INTEREST
print(SAMP.FILE)

MODEL    = "Joint"

Warm        = 400
Iter        = 2800
N_CHAIN     = 1
Treedepth   = 9
Adapt_delta = 0.5

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

# Mixed effect model for CPUE data
COV <- REL[,c("ID","ocean.region","brood_year","release_year","N.released","n.month","start_year","ID_numb")]
COV$year.reg <- paste(COV$ocean.region,COV$release_year,sep=".")

# Combine Oregon and combine Van Is.x  
COV$year.reg[substr(COV$year.reg,2,3)=="OR "] <- substr(COV$year.reg[substr(COV$year.reg,2,3)=="OR"],2,8)
COV$year.reg[substr(COV$year.reg,3,4)=="VI"] <- substr(COV$year.reg[substr(COV$year.reg,3,4)=="VI"],3,9)

dat.troll <- NULL
dat.rec <- NULL
dat.treaty <- NULL
dat.hake.ashop <- NULL
dat.hake.shoreside <- NULL

for(i in 1:N.REL){
  troll <- t(as.matrix(C_troll_true[i,,]))
  if(MONTH.STRUCTURE == "FOUR"){
    e.start <- grep(paste(REL$brood_year[i]+2,".month.spring",sep=""),rownames(K_troll_flat))
    if(REL$brood_year[i] == REL$release_year[i]){
    e.start <- grep(paste(REL$brood_year[i]+1,".month.spring",sep=""),rownames(K_troll_flat))
    }
  }
  if(MONTH.STRUCTURE == "FRAM"){
    e.start <- grep(paste(REL$brood_year[i]+2,".month.summer",sep=""),rownames(K_troll_flat))
    if(REL$brood_year[i] == REL$release_year[i]){
      e.start <- grep(paste(REL$brood_year[i]+1,".month.summer",sep=""),rownames(K_troll_flat))
    }
  }

  e.stop  <- e.start + N.mod.month -1
  effort <- t(as.matrix(K_troll_flat[e.start:e.stop,]))
  
  frac_samp <- t(as.matrix(Lambda_troll_flat_int[e.start:e.stop,]))
  time_id <- data.frame(year.season=colnames(frac_samp),time=1:ncol(frac_samp))
  
  dat.temp <- data.frame(melt(troll))
  colnames(dat.temp) <- c("loc","time","catch")
  dat.temp$rel <- COV$ID_numb[i]
  
  dat.effort            <- data.frame(melt(effort))
  colnames(dat.effort) <- c("loc","year.season","effort")
  
  dat.frac_samp        <- data.frame(melt(frac_samp))
  colnames(dat.frac_samp) <- c("loc","year.season","frac_samp")
  dat.frac_samp$time   <- time_id$time[match(dat.frac_samp$year.season,time_id$year.season)]

  A <- merge(dat.frac_samp,dat.effort)
  dat.temp <- merge(dat.temp,A)
  
  dat.temp$effort.type  <- "troll"
  dat.temp$brood_year <- COV$brood_year[i]
  dat.temp$release_year <- COV$release_year[i]
  dat.temp$N.release    <- COV$N.released[i]
  dat.temp$N.month      <- COV$n.month[i]
  dat.temp$year.reg     <- COV$year.reg[i]
  dat.temp$ocean.reg    <- COV$ocean.region[i]
  
  dat.troll <- bind_rows(dat.troll,dat.temp)
}

#### GET RID OF OBSERVATIONS WITHOUT OBSERVED CATCHES or EFFORT (times when no one caught anything or no one was fishing)
#### GET RID OF OBSERVATIONS WITH FISHING OBSERVATIONS:
A_troll_flat$mod.time <- 1:nrow(A_troll_flat)
A_troll_melt <-melt(A_troll_flat,id.vars=c("rec.year","lab","month.lab","mod.time"),variable.name = "loc")
A_troll_melt <- A_troll_melt %>% mutate(year.season = paste(rec.year,lab,sep="."))
A_troll_melt$loc <- as.numeric(as.character(A_troll_melt$loc))
A_troll_melt <- A_troll_melt %>% dplyr::rename(indicator = value)

dat.troll$loc.2  <- as.numeric(as.character(substr(dat.troll$loc,5,6)))

dat.troll <- left_join(dat.troll, A_troll_melt[,c("year.season","indicator","loc")], 
                     by = c("year.season" = "year.season", "loc.2" = "loc")) %>% dplyr::select(-loc.2)

dat.troll$indicator[dat.troll$effort>0] <- 1
dat.troll <- dat.troll %>% filter(dat.troll$indicator ==1) %>% dplyr::select(-indicator)

##### REPEAT FOR RECREATIONAL FISHERIES 
for(i in 1:N.REL){
  rec <- t(as.matrix(C_rec_true[i,,]))
  if(MONTH.STRUCTURE == "FOUR"){
    e.start <- grep(paste(REL$brood_year[i]+2,".month.spring",sep=""),rownames(K_rec_flat))
    if(REL$brood_year[i] == REL$release_year[i]){
      e.start <- grep(paste(REL$brood_year[i]+1,".month.spring",sep=""),rownames(K_rec_flat))
    }
  }
  if(MONTH.STRUCTURE == "FRAM"){
    e.start <- grep(paste(REL$brood_year[i]+2,".month.summer",sep=""),rownames(K_rec_flat))
    if(REL$brood_year[i] == REL$release_year[i]){
      e.start <- grep(paste(REL$brood_year[i]+1,".month.summer",sep=""),rownames(K_rec_flat))
    }
  }
  e.stop  <- e.start + N.mod.month -1
  effort <- t(as.matrix(K_rec_all_flat[e.start:e.stop,]))
  
  frac_samp <- t(as.matrix(Lambda_rec_flat_int[e.start:e.stop,]))
  time_id <- data.frame(year.season=colnames(frac_samp),time=1:ncol(frac_samp))
  
  dat.temp <- data.frame(melt(rec))
  colnames(dat.temp) <- c("loc","time","catch")
  dat.temp$rel <- COV$ID_numb[i]
  
  dat.effort            <- data.frame(melt(effort))
  colnames(dat.effort) <- c("loc","year.season","effort")
  
  dat.frac_samp        <- data.frame(melt(frac_samp))
  colnames(dat.frac_samp) <- c("loc","year.season","frac_samp")
  dat.frac_samp$time   <- time_id$time[match(dat.frac_samp$year.season,time_id$year.season)]
  
  A <- merge(dat.frac_samp,dat.effort)
  dat.temp <- merge(dat.temp,A)
  
  dat.temp$effort.type  <- "rec"
  dat.temp$brood_year   <- COV$brood_year[i]
  dat.temp$release_year <- COV$release_year[i]
  dat.temp$N.release    <- COV$N.released[i]
  dat.temp$N.month      <- COV$n.month[i]
  dat.temp$year.reg     <- COV$year.reg[i]
  dat.temp$ocean.reg    <- COV$ocean.region[i]
  
  dat.rec <- bind_rows(dat.rec,dat.temp)
}

#### GET RID OF OBSERVATIONS WITH FISHING OBSERVATIONS:
A_rec_flat$mod.time <- 1:nrow(A_rec_flat)
A_rec_melt <-melt(A_rec_flat,id.vars=c("rec.year","lab","month.lab","mod.time"),variable.name = "loc")
A_rec_melt <- A_rec_melt %>% mutate(year.season = paste(rec.year,lab,sep="."))
A_rec_melt$loc <- as.numeric(as.character(A_rec_melt$loc))
A_rec_melt <- A_rec_melt %>% dplyr::rename(indicator = value)

dat.rec$loc.2  <- as.numeric(as.character(substr(dat.rec$loc,5,6)))

dat.rec <- left_join(dat.rec, A_rec_melt[,c("year.season","indicator","loc")], 
                        by = c("year.season" = "year.season", "loc.2" = "loc")) %>% dplyr::select(-loc.2)

dat.rec$indicator[dat.rec$effort>0] <- 1
dat.rec <- dat.rec %>% filter(dat.rec$indicator ==1) %>% dplyr::select(-indicator)

###########################  Do the same for Treaty Troll 
for(i in 1:N.REL){
  treaty <- t(as.matrix(C_treaty_true[i,,]))
  if(MONTH.STRUCTURE == "FOUR"){
    e.start <- grep(paste(REL$brood_year[i]+2,".month.spring",sep=""),rownames(K_treaty_flat))
    if(REL$brood_year[i] == REL$release_year[i]){
      e.start <- grep(paste(REL$brood_year[i]+1,".month.spring",sep=""),rownames(K_treaty_flat))
    }
  }
  if(MONTH.STRUCTURE == "FRAM"){
    e.start <- grep(paste(REL$brood_year[i]+2,".month.summer",sep=""),rownames(K_treaty_flat))
    if(REL$brood_year[i] == REL$release_year[i]){
      e.start <- grep(paste(REL$brood_year[i]+1,".month.summer",sep=""),rownames(K_treaty_flat))
    }
  }
  
  e.stop  <- e.start + N.mod.month -1
  effort <- t(as.matrix(K_treaty_flat[e.start:e.stop,]))
  
  frac_samp <- t(as.matrix(Lambda_treaty_flat_int[e.start:e.stop,]))
  time_id <- data.frame(year.season=colnames(frac_samp),time=1:ncol(frac_samp))
  
  dat.temp <- data.frame(melt(treaty))
  colnames(dat.temp) <- c("loc","time","catch")
  dat.temp$rel <- COV$ID_numb[i]
  
  dat.effort            <- data.frame(melt(effort))
  colnames(dat.effort) <- c("loc","year.season","effort")
  
  dat.frac_samp        <- data.frame(melt(frac_samp))
  colnames(dat.frac_samp) <- c("loc","year.season","frac_samp")
  dat.frac_samp$time   <- time_id$time[match(dat.frac_samp$year.season,time_id$year.season)]
  
  A <- merge(dat.frac_samp,dat.effort)
  dat.temp <- merge(dat.temp,A)
  
  dat.temp$effort.type  <- "treaty"
  dat.temp$brood_year <- COV$brood_year[i]
  dat.temp$release_year <- COV$release_year[i]
  dat.temp$N.release    <- COV$N.released[i]
  dat.temp$N.month      <- COV$n.month[i]
  dat.temp$year.reg     <- COV$year.reg[i]
  dat.temp$ocean.reg    <- COV$ocean.region[i]
  
  dat.treaty <- bind_rows(dat.treaty,dat.temp)
}

A_treaty_flat$mod.time <- 1:nrow(A_treaty_flat)
A_treaty_melt <-melt(A_treaty_flat,id.vars=c("rec.year","lab","month.lab","mod.time"),variable.name = "loc")
A_treaty_melt <- A_treaty_melt %>% mutate(year.season = paste(rec.year,lab,sep="."))
A_treaty_melt$loc <- as.numeric(as.character(A_treaty_melt$loc))
A_treaty_melt <- A_treaty_melt %>% dplyr::rename(indicator = value)

dat.treaty$loc.2  <- as.numeric(as.character(substr(dat.treaty$loc,5,6)))

dat.treaty <- left_join(dat.treaty, A_treaty_melt[,c("year.season","indicator","loc")], 
                     by = c("year.season" = "year.season", "loc.2" = "loc")) %>% dplyr::select(-loc.2)

dat.treaty$indicator[dat.treaty$effort>0] <- 1
dat.treaty <- dat.treaty %>% filter(dat.treaty$indicator ==1)
dat.treaty <- dat.treaty %>% dplyr::select(-indicator)


############################################################
### Add Trawl Fisheries data to the mix.  First ASHOP
############################################################


if(TRAWL.US==T){

for(i in 1:N.REL){
  hake_ashop <- t(as.matrix(C_hake_ashop_true[i,,]))
  if(MONTH.STRUCTURE == "FOUR"){
    e.start <- grep(paste(REL$brood_year[i]+2,".month.spring",sep=""),rownames(K_hake_ashop_flat))
    if(REL$brood_year[i] == REL$release_year[i]){
      e.start <- grep(paste(REL$brood_year[i]+1,".month.spring",sep=""),rownames(K_hake_ashop_flat))
    }
  }
  if(MONTH.STRUCTURE == "FRAM"){
    e.start <- grep(paste(REL$brood_year[i]+2,".month.summer",sep=""),rownames(K_hake_ashop_flat))
    if(REL$brood_year[i] == REL$release_year[i]){
      e.start <- grep(paste(REL$brood_year[i]+1,".month.summer",sep=""),rownames(K_hake_ashop_flat))
    }
  }
  
  e.stop  <- e.start + N.mod.month -1
  effort <- t(as.matrix(K_hake_ashop_flat[e.start:e.stop,]))
  
  frac_samp <- t(as.matrix(Lambda_hake_ashop_flat_int[e.start:e.stop,]))
  time_id <- data.frame(year.season=colnames(frac_samp),time=1:ncol(frac_samp))
  
  dat.temp <- data.frame(melt(hake_ashop))
  colnames(dat.temp) <- c("loc","time","catch")
  dat.temp$rel <- COV$ID_numb[i]
  
  dat.effort            <- data.frame(melt(effort))
  colnames(dat.effort) <- c("loc","year.season","effort")
  
  dat.frac_samp        <- data.frame(melt(frac_samp))
  colnames(dat.frac_samp) <- c("loc","year.season","frac_samp")
  dat.frac_samp$time   <- time_id$time[match(dat.frac_samp$year.season,time_id$year.season)]
  
  A <- merge(dat.frac_samp,dat.effort)
  dat.temp <- merge(dat.temp,A)
  
  dat.temp$effort.type  <- "ashop"
  dat.temp$brood_year   <- COV$brood_year[i]
  dat.temp$release_year <- COV$release_year[i]
  dat.temp$N.release    <- COV$N.released[i]
  dat.temp$N.month      <- COV$n.month[i]
  dat.temp$year.reg     <- COV$year.reg[i]
  dat.temp$ocean.reg    <- COV$ocean.region[i]
  
  dat.hake.ashop <- bind_rows(dat.hake.ashop,dat.temp)
}

#### GET RID OF OBSERVATIONS WITHOUT OBSERVED CATCHES or EFFORT or SAMPLING (times when no one caught anything or no one was fishing)
#### GET RID OF OBSERVATIONS WITH FISHING OBSERVATIONS:
A_hake_ashop_flat$mod.time <- 1:nrow(A_hake_ashop_flat)
A_hake_ashop_melt <-melt(A_hake_ashop_flat,id.vars=c("rec.year","lab","month.lab","mod.time"),variable.name = "loc")
A_hake_ashop_melt <- A_hake_ashop_melt %>% mutate(year.season = paste(rec.year,lab,sep="."))
A_hake_ashop_melt$loc <- as.numeric(as.character(A_hake_ashop_melt$loc))
A_hake_ashop_melt <- A_hake_ashop_melt %>% dplyr::rename(indicator = value)

dat.hake.ashop$loc.2  <- as.numeric(as.character(substr(dat.hake.ashop$loc,5,6)))

dat.hake.ashop <- left_join(dat.hake.ashop, A_hake_ashop_melt[,c("year.season","indicator","loc")], 
                       by = c("year.season" = "year.season", "loc.2" = "loc")) %>% dplyr::select(-loc.2)

dat.hake.ashop$indicator[dat.hake.ashop$effort>0] <- 1
dat.hake.ashop$indicator[dat.hake.ashop$effort>0 & dat.hake.ashop$frac_samp==0 ] <- 0
dat.hake.ashop <- dat.hake.ashop %>% filter(indicator == 1) %>% dplyr::select(-indicator)
# Get rid of all observations on or before the year TRIM.ASHOP
dat.hake.ashop <- dat.hake.ashop %>% mutate(YYY= substr(dat.hake.ashop$year.season,1,4) %>% as.numeric())
dat.hake.ashop <- dat.hake.ashop %>% filter(YYY>TRIM.ASHOP) %>% dplyr::select(-YYY) 


##### Shoreside
for(i in 1:N.REL){
  hake_shoreside <- t(as.matrix(C_hake_shoreside_true[i,,]))
  if(MONTH.STRUCTURE == "FOUR"){
    e.start <- grep(paste(REL$brood_year[i]+2,".month.spring",sep=""),rownames(K_hake_shoreside_flat))
    if(REL$brood_year[i] == REL$release_year[i]){
      e.start <- grep(paste(REL$brood_year[i]+1,".month.spring",sep=""),rownames(K_hake_shoreside_flat))
    }
  }
  if(MONTH.STRUCTURE == "FRAM"){
    e.start <- grep(paste(REL$brood_year[i]+2,".month.summer",sep=""),rownames(K_hake_shoreside_flat))
    if(REL$brood_year[i] == REL$release_year[i]){
      e.start <- grep(paste(REL$brood_year[i]+1,".month.summer",sep=""),rownames(K_hake_shoreside_flat))
    }
  }
  
  e.stop  <- e.start + N.mod.month -1
  effort <- t(as.matrix(K_hake_shoreside_flat[e.start:e.stop,]))
  
  frac_samp <- t(as.matrix(Lambda_hake_shoreside_flat_int[e.start:e.stop,]))
  time_id <- data.frame(year.season=colnames(frac_samp),time=1:ncol(frac_samp))
  
  dat.temp <- data.frame(melt(hake_shoreside))
  colnames(dat.temp) <- c("loc","time","catch")
  dat.temp$rel <- COV$ID_numb[i]
  
  dat.effort            <- data.frame(melt(effort))
  colnames(dat.effort) <- c("loc","year.season","effort")
  
  dat.frac_samp        <- data.frame(melt(frac_samp))
  colnames(dat.frac_samp) <- c("loc","year.season","frac_samp")
  dat.frac_samp$time   <- time_id$time[match(dat.frac_samp$year.season,time_id$year.season)]
  
  A <- merge(dat.frac_samp,dat.effort)
  dat.temp <- merge(dat.temp,A)
  
  dat.temp$effort.type  <- "shoreside"
  dat.temp$brood_year   <- COV$brood_year[i]
  dat.temp$release_year <- COV$release_year[i]
  dat.temp$N.release    <- COV$N.released[i]
  dat.temp$N.month      <- COV$n.month[i]
  dat.temp$year.reg     <- COV$year.reg[i]
  dat.temp$ocean.reg    <- COV$ocean.region[i]
  
  dat.hake.shoreside <- bind_rows(dat.hake.shoreside,dat.temp)
}

#### GET RID OF OBSERVATIONS WITHOUT OBSERVED CATCHES or EFFORT (times when no one caught anything or no one was fishing)
#### GET RID OF OBSERVATIONS WITH FISHING OBSERVATIONS:
A_hake_shoreside_flat$mod.time <- 1:nrow(A_hake_shoreside_flat)
A_hake_shoreside_melt <-melt(A_hake_shoreside_flat,id.vars=c("rec.year","lab","month.lab","mod.time"),variable.name = "loc")
A_hake_shoreside_melt <- A_hake_shoreside_melt %>% mutate(year.season = paste(rec.year,lab,sep="."))
A_hake_shoreside_melt$loc <- as.numeric(as.character(A_hake_shoreside_melt$loc))
A_hake_shoreside_melt <- A_hake_shoreside_melt %>% dplyr::rename(indicator = value)

dat.hake.shoreside$loc.2  <- as.numeric(as.character(substr(dat.hake.shoreside$loc,5,6)))

dat.hake.shoreside <- left_join(dat.hake.shoreside, A_hake_shoreside_melt[,c("year.season","indicator","loc")], 
                            by = c("year.season" = "year.season", "loc.2" = "loc")) %>% dplyr::select(-loc.2)

dat.hake.shoreside$indicator[dat.hake.shoreside$effort>0] <- 1
dat.hake.shoreside <- dat.hake.shoreside %>% filter(dat.hake.shoreside$indicator ==1) %>% dplyr::select(-indicator)

########################################################################
TRIM.SHORESIDE <- 2011 # Get rid of data from before this year because there are no reliable recoveries from earlier years.

YYY <- as.numeric(substr(dat.hake.shoreside$year.season,1,4)) 
dat.hake.shoreside <- dat.hake.shoreside %>% filter(YYY >= TRIM.SHORESIDE) 
#####################################################################
} # end TRAWL.US if statement

#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################

N_time_mod   <- max(dat.troll$time)
N_log_N_all  <- N_time_mod - 1 # This is the number of latent states for abundance that we need to track. 

# ONLY KEEP RECOVERIES WITH EFFORT (or identified missing fishing effort) all gear types.
# KEEP ALL OF DATA FOR REC FISH because of missing data for effort in rec fisheries.
dat.all <- NULL
dat.all   <- rbind(dat.troll,dat.rec)
dat.all   <- rbind(dat.all,dat.treaty)
dat.all   <- rbind(dat.all,dat.hake.ashop)
dat.all   <- rbind(dat.all,dat.hake.shoreside)

dat.all.raw <- dat.all
############################################ 

if(MODEL == "Joint"){ 
  dat.bin <- dat.all.raw
  dat.bin$catch_bin <- 0
  dat.bin$catch_bin[dat.bin$catch>0] <- 1
  dat.bin$indicator <- 0
  dat.bin$indicator[dat.bin$catch >0] <- 1
}
dat.all <- dat.bin

## This section adds indexes to the data frame.
dat.all$location <- as.numeric( substr(dat.all$loc,5,6))
# "time" is in terms of calendar years
if(MONTH.STRUCTURE=="FOUR"){
  dat.all$year[dat.all$time <=3] <- 1
  dat.all$year[dat.all$time >3  & dat.all$time <= 7]   <- 2
  dat.all$year[dat.all$time >7  & dat.all$time <= 11]  <- 3
  dat.all$year[dat.all$time >11 & dat.all$time <= 15] <- 4
  dat.all$year[dat.all$time >15 & dat.all$time <= 19] <- 5
  dat.all$year <- as.numeric(dat.all$year)
}
if(MONTH.STRUCTURE=="FRAM"){  
    dat.all$year[dat.all$time <=3] <- 1
    dat.all$year[dat.all$time >3  & dat.all$time <= 7]   <- 2
    dat.all$year[dat.all$time >7  & dat.all$time <= 11]  <- 3
    dat.all$year[dat.all$time >11 & dat.all$time <= 15] <- 4
    dat.all$year[dat.all$time >15 & dat.all$time <= 19] <- 5
    dat.all$year <- as.numeric(dat.all$year)
}
  
  
AGE <- data.frame(time=1:N.mod.month,age=cumsum(Trans_month))
dat.all$age.month <- AGE$age[match(dat.all$time,AGE$time)]

AGE$month_leng[1] <- AGE$age[1]-0
for( i in 2: (nrow(AGE))){
  AGE$month_leng[i] <- AGE$age[i] - AGE$age[i-1]
}

AGE$year
AGE$year[AGE$time <=3] <- 1
AGE$year[AGE$time >3  & AGE$time <= 7]   <- 2
AGE$year[AGE$time >7  & AGE$time <= 11]  <- 3
AGE$year[AGE$time >11 & AGE$time <= 15] <- 4
AGE$year[AGE$time >15 & AGE$time <= 19] <- 5
AGE$year <- as.numeric(AGE$year)

# Make a indicator variable for identifying what model time the fish enter the river to spawn
if(MONTH.STRUCTURE=="FOUR"){
  AGE$spawn_time <- 0
  AGE$spawn_time[AGE$time==3]  <- 1
  AGE$spawn_time[AGE$time==7]  <- 2
  AGE$spawn_time[AGE$time==11] <- 3
  AGE$spawn_time[AGE$time==15] <- 4
  AGE$spawn_time[AGE$time==19] <- 5
}
if(MONTH.STRUCTURE=="FRAM"){
  AGE$spawn_time <- 0
  AGE$spawn_time[AGE$time==2]  <- 1
  AGE$spawn_time[AGE$time==6]  <- 2
  AGE$spawn_time[AGE$time==10] <- 3
  AGE$spawn_time[AGE$time==14] <- 4
  AGE$spawn_time[AGE$time==18] <- 5
}
# Create cumulative adult mortality values to be read into the program
# M2 is in terms of monthly mortality
# cum_M2 <- rep(0,length(AGE$month_leng))
# for(i in 1:length(AGE$month_leng)){
#   cum_M2[i] <- cumsum(M.det)[cumsum(AGE$month_leng)[i]]
# }

#THIS calculates stuff for estimating the M2 curve.
age_month_cal <- 1:max(dat.all$age.month)
age_month_cal <- age_month_cal - 24
age_month_cal[age_month_cal > 0] <- 0
#####  

YEAR.REG <- sort(unique(dat.all$year.reg))
dat.all$year.reg.idx <- match(dat.all$year.reg,YEAR.REG)

##### THIS IS WHERE YOU CHANGE THE JUV MORTALITY GROUPINGS
COV$year.reg.idx     <- match(COV$year.reg,YEAR.REG)
#COV$year.reg.idx     <- COV$ID_numb

dat.all$log.N0 <- log(dat.all$N.release)
dat.all$effort.idx[dat.all$effort.type =="troll"] <- 1
dat.all$effort.idx[dat.all$effort.type =="rec"]   <- 2
dat.all$effort.idx[dat.all$effort.type =="treaty"]      <- 3
dat.all$effort.idx[dat.all$effort.type =="ashop"]       <- 4
dat.all$effort.idx[dat.all$effort.type =="shoreside"]   <- 5

dat.all$effort.idx.rec <- 0
dat.all$effort.idx.rec[dat.all$effort.type =="rec"] <- 1
dat.all$effort.idx.treaty <- 0
dat.all$effort.idx.treaty[dat.all$effort.type =="treaty"] <- 1

dat.all$gear.idx <- 0
dat.all$gear.idx[dat.all$effort.type =="troll"]  <- 1
dat.all$gear.idx[dat.all$effort.type =="rec"]    <- 2
dat.all$gear.idx[dat.all$effort.type =="treaty"] <- 3
dat.all$gear.idx[dat.all$effort.type =="ashop"]  <- 4
dat.all$gear.idx[dat.all$effort.type =="shoreside"] <- 5

N_gear <- length(unique(dat.all$gear.idx))

#### DEFINE VULNERABILIY SCHEDULE.
# For directed fisheries, vulnerability increases with age
MONTH.vuln <- 36
dat.all$age.vuln <- dat.all$age.month - MONTH.vuln # Make it so that all fish are vulnerable by spring (April if "FOUR", May if "FRAM") of their MONTH.vuln time in the model ()
#dat.all$age.vuln[dat.all$age.vuln>0] <- 0
temp <- data.frame(age.month=sort(as.numeric(unique(dat.all$age.month))),age.vuln.idx = 1:length(unique(dat.all$age.month)))
temp$vuln.age <- temp$age.month - MONTH.vuln
# temp$vuln.age[temp$vuln.age > 0] <- 0
vuln_age <- temp$vuln.age 
dat.all$age.vuln.idx <- match(dat.all$age.month,temp$age.month)


# For hake fisheries, allow for different vulnerability schedule
MONTH.vuln.trawl <- 24
dat.all$age.vuln.hake <- dat.all$age.month - MONTH.vuln.trawl  # Make it so that all fish are invulnerable by (April if "FOUR", May if "FRAM") of their MONTH.vuln time in the model ()
#dat.all$age.vuln[dat.all$age.vuln>0] <- 0
temp <- data.frame(age.month=sort(as.numeric(unique(dat.all$age.month))),age.vuln.trawl.idx = 1:length(unique(dat.all$age.month)))
temp$vuln.age.trawl <- temp$age.month - MONTH.vuln.trawl

vuln_age_trawl <- temp$vuln.age.trawl / 10  ## Change the scale of the age in months to improve mixing and such of the associated parameter 

# dat.all$vuln.idx[dat.all$location <= 7 & dat.all$effort.type == "troll"] <- 1
# dat.all$vuln.idx[dat.all$location >  7 & dat.all$effort.type == "troll"] <- 2
# dat.all$vuln.idx[dat.all$effort.type == "treaty"] <- 2
# dat.all$vuln.idx[dat.all$location <= 7 & dat.all$effort.type == "rec"]   <- 3
# dat.all$vuln.idx[dat.all$location >  7 & dat.all$effort.type == "rec"]   <- 4
# dat.all$vuln.int.idx[dat.all$vuln.idx <= 2 ] <- 1
# dat.all$vuln.int.idx[dat.all$vuln.idx > 2 ]  <- 2
# vuln_int_idx      <- c(rep(1,7),rep(2,10))
# vuln_int_rec_idx  <- c(rep(3,7),rep(4,10))

# Ocean distribution for each spawning vulnarea
dat.all$origin.loc     <- paste(dat.all$ocean.reg,dat.all$loc,sep=".")
ORIGIN.LOC             <- sort(unique(dat.all$origin.loc))
temp                   <- data.frame(location.name=unique(dat.all$ocean.reg))
temp.loc               <- LOCATIONS
temp.loc$location.name <- as.character( temp.loc$location.name)

temp.loc <-  REL %>% group_by(ocean.region,loc.numb) %>% summarize(n=length(loc.numb)) %>% 
              dplyr::select(-n) %>% as.data.frame() %>%
              rename(location.name=ocean.region,location.number=loc.numb)

temp                   <- merge(temp,temp.loc)
temp$location.number   <- as.numeric(as.character(temp$location.number))
temp                   <- temp[order(temp$location.number),]
temp$origin.idx        <- 1:nrow(temp)
dat.all$origin.idx     <- match(dat.all$ocean.reg,temp$location.name)
COV$origin.idx         <- match(COV$ocean.reg,temp$location.name)

# This section combines maturity curves for Oregon and Van Is.
dat.all$loc.spawn     <- move_id$loc.spawn[match(dat.all$ocean.reg,move_id$ocean_region)]
temp                  <- data.frame(cbind(sort(unique(dat.all$loc.spawn)),1:length(sort(unique(dat.all$loc.spawn)))))
#temp[temp[,1]>=5 & temp[,1]<=7,2] <- 3 # THIS LINE MAKES IT SO OREGON REGIONS HAVE IDENTICAL SPAWNING
#temp[temp[,1]>=12 ,2] <- 12             # THIS LINE MAKES IT SO VAN IS. REGIONS HAVE IDENTICAL SPAWNING

temp.lab <- data.frame(a=unique(temp$X2),b=1:length(unique(temp$X2)))
temp$loc.spawn.idx <- temp.lab$b[match(temp$X2,temp.lab$a)]
spawn_loc_2 <- merge(spawn_loc,temp,by.x="number",by.y="X1")
spawn_loc_2$indicator_move <- 0
#spawn_loc_2$indicator_move[spawn_loc_2$number<9] <- 1

dat.all$loc.spawn.idx <- spawn_loc_2$loc.spawn.idx[match(dat.all$loc.spawn,spawn_loc_2$number)]
COV$loc.spawn.idx     <- spawn_loc_2$loc.spawn.idx[match(COV$ocean.region,spawn_loc_2$ocean.region)]

#dat.all$log_effort    <- log(dat.all$effort)

# MAke an index for seasons.
dat.all$season.idx    <- 0
if(MONTH.STRUCTURE=="FOUR"){
  wint.spring <- sort(c(seq(1,N_time_mod,by=4),seq(4,N_time_mod,by=4)))
  summer      <- seq(2,N_time_mod,by=4)
  fall        <- seq(3,N_time_mod,by=4)
  N_season = 3
}
if(MONTH.STRUCTURE=="FRAM"){
  wint.spring <- sort(c(seq(3,N_time_mod,by=4),seq(4,N_time_mod,by=4)))
  summer      <- seq(1,N_time_mod,by=4)
  fall        <- seq(2,N_time_mod,by=4)
  N_season = 3
}

for(i in 1:length(wint.spring)){
  dat.all$season.idx[dat.all$time == wint.spring[i]] <- 1
}
for(i in 1:length(summer)){
  dat.all$season.idx[dat.all$time == summer[i]] <- 2
}
for(i in 1:length(fall)){
  dat.all$season.idx[dat.all$time == fall[i]]   <- 3
}

season_idx <- rep(0,N_time_mod)
for(i in 1:length(wint.spring)){
  season_idx[wint.spring[i]] <- 1
  season_idx[summer[i]] <- 2
  season_idx[fall[i]] <- 3
}
AGE$season_idx <- season_idx
#######################

dat.all$start_year <- REL$start_year[match(dat.all$rel,REL$ID_numb)]
dat.all$temp_dat_season_idx <- (dat.all$start_year -1 )* N_month + dat.all$time

############################## THIS SECTION DEFINE THE DIRICHLET ESCAPEMENT PARAMETERS
## Make E_alpha with 
E_temp<- merge(temp,escape_diri,by.y="number",by.x="X1")
E_alpha <- E_temp[match(unique(E_temp$loc.spawn.idx),E_temp$loc.spawn.idx),grep("age",colnames(E_temp))]
#E_alpha[,6] <- E_alpha[,1]
# Adjust E_alpha so that it has reasonable levels of certainty on the distribution of maturing adults for all locations

E_alpha <- E_alpha[,2:6]
E_prop <- as.matrix(E_alpha / rowSums(E_alpha))
# diri_constant is defined in the Prep raw data script.
E_alpha <- E_prop * diri_constant
E_alpha <- as.matrix(E_alpha)

#E_alpha <- data.frame(E_alpha[,1]*0,E_alpha)
#E_alpha[6,] <- E_alpha[6,]*2

# Modify the sections that are unique to the positive model
############# specifically, zero out effort.idx 

#### This is the final data file that provides data for the model fitting routines.
dat.all <- dat.all  %>% arrange(desc(catch))
dat.all$frac_samp_mod <- dat.all$frac_samp

N_obs_bin <- nrow(dat.all)
N_obs_neg <- length(dat.all$catch[dat.all$catch==0])
N_obs_pos <- N_obs_bin-N_obs_neg

dat.all[(N_obs_neg-1):(N_obs_neg+3),]

dat.bin.fin <- dat.all[dat.all$indicator == 0,]
dat.pos.fin <- dat.all[dat.all$indicator == 1,]


############## CALCULATE NUMBER OF OBSERVATIONS FOR EACH FISHING TYPE

# Helper vector defining the year for time-varying q.

if(MONTH.STRUCTURE=="FOUR" ){
  q_year_vec <- sort(c(rep(YEARS.RECOVER[1],4),
                     rep(YEARS.RECOVER[2:(length(YEARS.RECOVER)-1)],4),
                     rep(YEARS.RECOVER[length(YEARS.RECOVER)],3)))
}
if(MONTH.STRUCTURE=="FRAM" ){
  q_year_vec <- sort(c(rep(YEARS.RECOVER[1],3),
                     rep(YEARS.RECOVER[2:(length(YEARS.RECOVER)-1)],4),
                     rep(YEARS.RECOVER[length(YEARS.RECOVER)],3)))
}
  q_year_vec <- q_year_vec - min(q_year_vec)
  log_q_year_vec <- log(q_year_vec + 1)

  q_year_vec <- q_year_vec / 10

  
# make an index for allowing catchability to vary for troll fisheries and us rec fisheries.
  # Make 4 levels - CA, OR & WA, BC, AK
  if(loc_18 == "TWO_OR" | loc_18 == "NCA_SOR_PUSO") { 
    troll_idx <- c(rep(1,4),rep(2,6),rep(3,5),rep(4,2))
    N_troll_idx <- length(unique(troll_idx)) #cbind(LOCATIONS,troll_idx)
    
    #rec_us_idx <- c(rep(1,4),rep(2,13))
    rec_us_idx <- c(rep(1,4),rep(2,13))
    N_rec_us_idx <- length(unique(rec_us_idx)) #cbind(LOCATIONS,troll_idx)
    
    # Make index for observation CV
    sigma_cv_idx <- c(rep(1,4),rep(2,6),rep(3,5),rep(4,2));
    N_sigma_cv_idx <- length(unique(sigma_cv_idx));
  }
  
  
  
#############################################################################################
## Define Shaker mortality (fraction of mortality suffered by fish present in a fishery area and contacted but released)
#############################################################################################
shaker_mort <- 0.1

#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
## Define Fixed cumulative mortatliy

M2_vec <- exp(MU_m[1] + MU_m[2] * age_month_cal) ;
cum_M2_fixed <- cumsum(M2_vec)

N_effort     <- length(unique(dat.all$effort.idx))
N_vuln_month <- length(unique(dat.all$age.vuln.idx))
N_yr_reg     <- length(unique(dat.all$year.reg.idx))
N_loc_spawn  <- length(unique(dat.all$loc.spawn.idx)) 
#THESE THREE ARE OBSERVED ABOVE.
  # N_obs_bin    <- nrow(dat.all)
  # N_obs_pos    <- nrow(dat.pos.fin)
  # N_obs_neg is defined above
N_year       <- max(AGE$year)
N_origin_loc <- length(unique(dat.all$origin.loc)) 
N_origin     <- length(unique(dat.all$origin.idx))
N_vuln       <- length(unique(dat.all$vuln.idx))
N_vuln_int   <- length(unique(dat.all$vuln.int.idx))
N_month_mod  <- max(dat.all$age.month)

CONSTANT     <- 1e-10

#Priors
log_rel_year_mu_prior    = c(log(3) , 0.5) # mean distribution for the mean of hierachical juv mort
log_rel_year_sigma_prior = c(1.5,6) # gamma distributed alpha, beta for hierarchical juvenile mortality 
MU_M2_prior             = MU_m    # Mean for Adult mortality intercept and slope - for multivariate normal
Sigma_M2_prior          = Sigma_m  # covariance matrix for Adult mortality intercept and slope
#vuln_int_prior          = c(3,1)   # normal mean and sd for vulnerability intercept
beta_vuln_prior         = c(3,3) # gamma values for vulnerability slope with age
beta_vuln_hake_prior    = c(3,3) # gamma values for vulnerability slope with age for hake trawl.
# beta_vuln_int_prior     = c(0,0.5) #lognorma mean and sd for vulnerability INT with age
# sigma_prior             = c(1,2) # observation variance for positive model prior (gamma params)
sigma_cv_prior         = c(50,50) # observation CV  true catch (gamma params)
#sigma_slope_prior     = c(-0.1,0.2) # observation variance for positive model prior (normal params)
log_F_prior             = c(log(0.01),1) #  normal mean of log F
F_rec_sigma_prior       = c(2,2)  # gamma params for F_rec
gamma_int_prior         = c(-5,1) # normal prior for intercept of maturity curve
gamma_slope_prior       = c(0,0.25)  # lognormal prior for slope of maturity curve
# logit_offset_int_prior  = c(-1.5,0.2)  # normal prior for logit offset intercept.
# logit_offset_slope_prior= c(1,2)  # normal prior for logit offset slope
# tau_process_prior       = c(1,10)  # gamma prior on process error sd
# tau_process_prod_prior  = c(2,2)   # gamma prior on  M * Tau process error.
# tau_q_dev_prior         = c(10,40) # gamma prior for deviations on log-q standard deviations.
log_q_troll_prior       = c(-11,1) # prior for troll catchability.
log_q_treaty_prior      = c(-8,1) # prior for troll catchability.
log_q_rec_prior         = c(-13,2) # prior for rec catchability.
log_q_hake_prior        = c(-15,2) # prior for rec catchability.
log_q_slope_prior       = c(12,8) # gamma dist for slope parameter (restricted to be positive)
q_int_prior             = c(-5,2) # prior on intercept for log_q_slope
phi_space_prior         = c(3,1) # gamma prior on spatial sd for smoothing
theta_space_prior       = c(3,1) # gamma prior on spatial decay parameter (gaussian correlation)
spawn_smooth_prior      = c(12,2) # gamma prior on spawn smooth


##### W_STAR PARAMETERIZATION
### ALL BASED ON knots at present.
VAL <- 0
MIN <- -3
MIN.2 <- -3
w_star_prior_mean_sf <- matrix(VAL,N_origin,N_knot_sf)
w_star_prior_mean_ws <- matrix(VAL,N_origin,N_knot_ws)

VAL <- 3
MIN <- 0.5
MIN.2 <- 1
w_star_prior_sd_sf <- matrix(VAL,N_origin,N_knot_sf)
w_star_prior_sd_ws <- matrix(VAL,N_origin,N_knot_ws)

# These are priors for how the distribution-environment parameters vary with temperature (or other environmental covariates)
VAL <- 0
origin_sea_slope_prior_mean <- matrix(VAL,N_origin,N.LOC)

VAL = 2
origin_sea_slope_prior_sd <- matrix(VAL,N_origin,N.LOC) 


# Create two indicator vectors for modifying the movement and distribution (connected to calculation of origin_loc)
origin_vec <- c(rep(1,9),0,rep(1,7))

# MAKE A LIST OF PRIOR DISTRIBUTIONS THAT CAN BE SAVED LATER.
PRIORS <- list(E_alpha=E_alpha,
               MU_M2_prior=MU_M2_prior,
               Sigma_M2_prior=Sigma_M2_prior,
               # vuln_int_prior =vuln_int_prior,
               beta_vuln_prior=beta_vuln_prior,
               beta_vuln_hake_prior=beta_vuln_hake_prior,
               # beta_vuln_int_prior=beta_vuln_int_prior,
               log_rel_year_mu_prior=log_rel_year_mu_prior,
               log_rel_year_sigma_prior= log_rel_year_sigma_prior,
               #sigma_pos_prior = sigma_pos_prior,
               # logit_offset_slope_prior = logit_offset_slope_prior,
                sigma_cv_prior = sigma_cv_prior,
 #               sigma_slope_prior = sigma_slope_prior,
               log_F_prior     = log_F_prior,
               F_rec_sigma_prior = F_rec_sigma_prior,
               w_star_prior_mean_sf = w_star_prior_mean_sf,
               w_star_prior_sd_sf = w_star_prior_sd_sf,
               w_star_prior_mean_ws = w_star_prior_mean_ws,
               w_star_prior_sd_ws = w_star_prior_sd_ws,
               # origin_sea_int_prior_mean = origin_sea_int_prior_mean,
               # origin_sea_int_prior_sd = origin_sea_int_prior_sd,
               origin_sea_slope_prior_mean = origin_sea_slope_prior_mean,
               origin_sea_slope_prior_sd = origin_sea_slope_prior_sd,
               gamma_int_prior = gamma_int_prior,
               gamma_slope_prior = gamma_slope_prior,
               # tau_process_prior = tau_process_prior,
               # tau_process_prod_prior = tau_process_prod_prior,
               # tau_q_dev_prior  = tau_q_dev_prior,
               log_q_troll_prior = log_q_troll_prior,
               log_q_rec_prior = log_q_rec_prior,
               log_q_slope_prior = log_q_slope_prior,
               phi_space_prior = phi_space_prior,
               theta_space_prior = theta_space_prior,
               spawn_smooth_prior = spawn_smooth_prior
)

phi_space_fix = 3

#####################################################################################################
#####################################################################################################
### DATA
#####################################################################################################
#####################################################################################################

stan_data = list(
    "bin_catch"       = dat.all$catch_bin,
    "pos_catch"       = dat.pos.fin$catch,
    "inv_frac_samp"       = dat.all$frac_samp_mod^(-1),
    "log_frac_samp"       = log(dat.all$frac_samp_mod),
    "log_inv_frac_samp_pos"   = log(dat.pos.fin$frac_samp_mod^(-1)),
    #"inv_frac_samp_pos"       = dat.pos.fin$frac_samp_mod^(-1),
    #"pos_idx" = dat.all$pos_idx, # indexes the positive observations.
    # "log_frac_samp_comp"       = log(1-dat.bin.fin$frac_samp_mod),
    # "log_frac_samp"   = log(dat.bin.fin$frac_samp_mod),
    # "log_frac_samp_pos"   = dat.pos.fin$frac_samp_log,
    # "logit_offset_int" = dat.bin.fin$logit_offset_int,
    
    "spawn_time_fraction" = REL$spawn_time_fraction,
    "N_rel"           = N.REL,
    "N_time_mod"      = N_time_mod,
    "N_log_N_all"     = N_log_N_all,
    "N_loc"           = N.LOC,
    "N_loc_spawn"     = N_loc_spawn,
    "N_obs_bin"       = N_obs_bin,
    "N_obs_neg"       = N_obs_neg,
    "N_obs_pos"       = N_obs_pos,
    "N_effort"        = N_effort ,
    "N_vuln"          = N_vuln ,
    "N_vuln_int"      = N_vuln_int ,
    "N_vuln_month"    = N_vuln_month,
    "N_yr_reg"        = N_yr_reg , 
    "N_gear"          = N_gear , 
    "N_year"          = N_year ,
    "N_origin_loc"    = N_origin_loc,
    "N_origin"        = N_origin,
    "N_season"        = 3,
    "N_month_mod"     = N_month_mod,
    "month_mod"       = 1:N_month_mod,
    "age_month_cal"   = age_month_cal,
    "vuln_age"        = vuln_age,
    "vuln_age_trawl"  = vuln_age_trawl,
    "N_years_recover" = N_years_recover,
    "N_years_release" = N_years_release,
    "N_month"         = N_month,
    "age_year"        = 1:N_year,
    "N_season_total"  = N_season_total,
    
    # Effort data in matrix form
    "K_troll"        = K_troll_flat, 
    "K_treaty"       = K_treaty_flat, 
    "K_rec"          = K_rec_flat, 
    "K_rec_can"      = K_rec_can_flat,
    "K_rec_can_irec" = K_rec_can_irec_flat,
    "K_rec_PUSO"     = K_rec_PUSO_flat,
    "K_hake_ashop"   = K_hake_ashop_flat,
    "K_hake_shoreside" = K_hake_shoreside_flat,
    
    "ashop_year_break" = ashop_year_break, # season (in terms of model seasons) in which the q changes for the ashop fleet. C
                                            # Could elaborate to make multiple breaks as desired.
    
    # Indexes troll areas
    "N_troll_idx" = N_troll_idx,
    "troll_idx" = troll_idx,
    "N_rec_us_idx" = N_rec_us_idx,
    "rec_us_idx" = rec_us_idx,
    "N_sigma_cv_idx" = N_sigma_cv_idx,
    "sigma_cv_idx" = sigma_cv_idx,
    
    # Size limits for three fishery types for use with Vulnerability schedules
    "vuln_troll_mat" = vuln_troll_mat,
    "vuln_treaty_mat"= vuln_treaty_mat,
    "vuln_rec_mat"   = vuln_rec_mat,
    
    # helper vector for catchability.
    "q_year_vec"     = q_year_vec,
    "log_q_year_vec"  = log_q_year_vec,
    
    # Matrices for ensuring smooth ocean distributions.
    "N_pred_loc"   = N_pred_loc,
    "N_pred_loc_salish" = N_pred_loc_salish,
    "N_knot_sf"     = N_knot_sf,
    "d_knot_knot_sf2" = d_knot_knot_sf2,
    "d_pred_knot_sf2" = d_pred_knot_sf2,
    
    "N_knot_ws"     = N_knot_ws,
    "d_knot_knot_ws2" = d_knot_knot_ws2,
    "d_pred_knot_ws2" = d_pred_knot_ws2,
    
    "knot_idex"    = k.pred.index[,c("location.number")],
    "knot_idex_salish"    = k.pred.index.salish[,c("location.number")],
    
    # Ocean Temperature observations    
    "ocean_temp_dev" = ocean.temp.dev,
    # indexes associated with temperatures
    "temperature_season_idx" = temperature_season_idx,
    # helper files for origin_...
    #"origin_vec" = origin_vec,
    
    # Spawner Data... mean proportion for each release
    "E_prop"         = E_prop,
    "diri_constant"  = diri_constant, # Assumed precision for the dirichlet distribution
    
    # Helper files for mapping fishing effort     
    "constant_origin"= 1e-4,
    "constant"       = CONSTANT, # added value to ensure no 0 values in the fishing mortality vectors

    "f_rec_param_idx"    = f_rec_param_idx, ## THIS IS THE 2D file for use with random effect for each location.
    "N_f_rec_idx_param"  = N_f_rec_idx_param,

    "f_treaty_param_idx"    = f_treaty_param_idx, ## THIS IS THE 2D file for use with random effect for each location.
    "N_f_treaty_idx_param"  = N_f_treaty_idx_param,
    
    "f_troll_param_idx"    = f_troll_param_idx, ## THIS IS THE 2D file for use with random effect for each location.
    "N_f_troll_idx_param"  = N_f_troll_idx_param,

    "f_hake_ashop_param_idx"    = f_hake_ashop_param_idx, ## THIS IS THE 2D file for use with random effect for each location.
    "N_f_hake_ashop_idx_param"  = N_f_hake_ashop_idx_param,
    
    "f_rec_overlap_effort_idx" = f_rec_overlap_effort_idx,
    "N_f_rec_overlap_effort_idx_param" = N_f_rec_overlap_effort_idx_param,
    
    ### Helper files for making fishing stochastic.
    # "f_troll_effort_idx"  <- f_troll_effort_idx,
    # "N_f_troll_effort_idx_param" <- N_f_troll_effort_idx_param, 
    # 
    # "f_treaty_effort_idx"  <- f_treaty_effort_idx  ,
    # "N_f_treaty_effort_idx_param" <- N_f_treaty_effort_idx_param ,
    # 
    # "f_rec_effort_idx"  <- f_rec_effort_idx  ,
    # "N_f_rec_effort_idx_param" <- N_f_rec_effort_idx_param ,
    # 
    # "f_rec_can_effort_idx"  <- f_rec_can_effort_idx  ,
    # "N_f_rec_can_effort_idx_param" <- N_f_rec_can_effort_idx_param ,
    
    # River entry indicator matrix
    "river_entry"     = river_entry,
    
    # Covariates STATE SPACE
    "log_N0"          = log(REL$N.released),
    "month_rec"       = COV$n.month,
    
    # end of model constraints
    "log_N_ratio_mean" = REL$log_N_ratio_mean,
    "log_N_ratio_sd"   = REL$log_N_ratio_sd,  
    
    # Fixed Mortality
    "cum_M2_fixed"     = cum_M2_fixed,
    "shaker_mort"      = shaker_mort,
    
    #Fixed vulnerability intercept
    "vuln_fixed"      = vuln_fixed,
    
    # Indexes States Space
       #"rel_cov_idx"       = COV$ID,
     "loc_spawn_idx"     = COV$loc.spawn.idx,
     "age_year_idx"      = AGE$year,
     "year_region_idx"   = COV$year.reg.idx,
     "age_month_idx"     = AGE$age,
     "spawn_time"        = AGE$spawn_time,
     "spawn_time_idx"    = AGE$spawn_time,
     # "vuln_int_idx"      = vuln_int_idx,
     # "vuln_int_rec_idx"  = vuln_int_rec_idx,
     "origin_idx"        = COV$origin.idx,
     "season_idx"        = AGE$season_idx ,
     "start_year"        = COV$start_year,
     "indicator_move_idx"= spawn_loc_2$indicator_move,
     "origin_year_idx"   =  origin_year_idx,
  
    # Indexes associated with observations
    "mod_time_idx"        = dat.all$time,
    "mod_time_N_all_idx"  = dat.all$time - 1,
    "rel_idx"             = dat.all$rel,
    "origin_bin_idx"      = dat.all$origin.idx,
    "season_bin_idx"      = dat.all$season.idx,
    "gear_bin_idx"        = dat.all$gear.idx,
    "loc_idx"             = dat.all$location,
    "loc_spawn_bin_idx"   = dat.all$loc.spawn.idx,
    "temp_dat_season_bin_idx" = dat.all$temp_dat_season_idx,

    # "mod_time_pos_idx"      = dat.pos.fin$time,
    # "rel_pos_idx"           = dat.pos.fin$rel,
    # "origin_pos_idx"        = dat.pos.fin$origin.idx,
    # "season_pos_idx"        = dat.pos.fin$season.idx,
    # "gear_pos_idx"          = dat.pos.fin$gear.idx,
    # "loc_pos_idx"           = dat.pos.fin$location,    
    # "loc_spawn_pos_idx"    = dat.pos.fin$loc.spawn.idx,
    # "loc_pos"              = dat.pos.fin$location,
    # "temp_dat_season_pos_idx" = dat.pos.fin$temp_dat_season_idx,

    #Priors
    "E_alpha"              = E_alpha,
    "log_rel_year_mu_prior"= log_rel_year_mu_prior, 
    "log_rel_year_sigma_prior" = log_rel_year_sigma_prior,
    "MU_M2"                = MU_M2_prior,
    "Sigma_M2"             = Sigma_M2_prior,
    #"vuln_int_prior"       = vuln_int_prior,
    "beta_vuln_prior"  = beta_vuln_prior,
    "beta_vuln_hake_prior"  = beta_vuln_hake_prior,
    #"beta_vuln_int_prior"  = beta_vuln_int_prior,
    #"sigma_pos_prior"      = sigma_pos_prior,
    "sigma_cv_prior"      = sigma_cv_prior,
    #"sigma_slope_prior"    = sigma_slope_prior,
    "log_F_prior"          = log_F_prior,
    "F_rec_sigma_prior"    = F_rec_sigma_prior,
    # "origin_sea_int_prior_mean"  = origin_sea_int_prior_mean,
    # "origin_sea_int_prior_sd"    = origin_sea_int_prior_sd,

    "w_star_prior_mean_ws"  = w_star_prior_mean_ws,
    "w_star_prior_sd_ws"    = w_star_prior_sd_ws,
    "w_star_prior_mean_sf"  = w_star_prior_mean_sf,
    "w_star_prior_sd_sf"    = w_star_prior_sd_sf,
    
    "origin_sea_slope_prior_mean"= origin_sea_slope_prior_mean,
    "origin_sea_slope_prior_sd"  = origin_sea_slope_prior_sd,
    "gamma_int_prior"      = gamma_int_prior,
    "gamma_slope_prior"    = gamma_slope_prior,
    #"logit_offset_int_prior"   = logit_offset_int_prior,
    # "tau_process_prior"    = tau_process_prior,
    # "tau_process_prod_prior"    = tau_process_prod_prior,
    # "tau_q_dev_prior"      = tau_q_dev_prior,
    "log_q_troll_prior"    = log_q_troll_prior,
    "log_q_troll_prior"    = log_q_troll_prior,
    "log_q_treaty_prior"    = log_q_treaty_prior,
    "log_q_rec_prior"      = log_q_rec_prior,
    "log_q_hake_prior"      = log_q_hake_prior,
    "log_q_slope_prior"      = log_q_slope_prior,
    "q_int_prior"          = q_int_prior,
    "phi_space_prior"      = phi_space_prior,
    "phi_space_fix"        = phi_space_fix,
    "theta_space_prior"    = theta_space_prior,
    "spawn_smooth_prior"   = spawn_smooth_prior
)

# parameters to monitor
stan_pars = c(
  "log_q_troll_pos","log_q_troll_start","log_q_troll_slope",
  "log_q_treaty_pos","log_q_treaty_start","log_q_treaty_slope",
  "log_q_rec_pos","log_q_rec_start","log_q_rec_slope",
  "log_q_rec_can_pos","log_q_rec_can_start","log_q_rec_can_slope",
  "log_q_rec_can_irec_pos",  "log_q_rec_can_irec_start",
  "log_q_hake_ashop_start", "log_q_hake_ashop_pos",
  "log_q_hake_shoreside_start","log_q_hake_shoreside_pos",
  #"log_q_troll_area","troll_area_sd",
  "q_int",
  # "observe_frac",
    #"log_q_rec_PUSO_pos",
  #"tau_process",
  #"tau_process_prod",
  # "epsilon",
  #"logit_offset_int",
  #"logit_offset_slope",
  #"log_M2",
  #"cum_M2",
  #"sigma_pos",
  "sigma_cv",
  "sigma_cv_hake",
  "spawn_smooth",
  # "log_spawn_smooth_mean",
  # "log_spawn_smooth_sigma",
  #"sigma_slope",
  #"sigma_slope_samp",
  "prob_age_year",
  #"vuln_int",
  #"beta_vuln_int",
  "beta_vuln",
  "beta_vuln_hake",
  #"alpha_pay_mean",
  #"alpha_pay_sd",
  #"log_beta_pay_mean",
  #"log_beta_pay_sd",
  "alpha_pay",
  "beta_pay",
  #"vuln_mat",
  "rel_year_all",
  "log_rel_year_mu", 
  "log_rel_year_sigma",
  "origin_sea_int",
  "origin_sea_slope",
  "origin_mat",
  "prop_D",
  "D",
  "log_N_all",
  #"log_N_ratio",
  "F_rec",
  "F_troll",
  "F_hake_ashop",
  "log_F_rec_mean",
  #"F_sigma",
  "log_F_troll_mean",
  #"F_sigma", 
   "F_rec_sigma",
   "F_troll_sigma",
  "F_troll_array",
  "F_treaty_array",
  "F_rec_array",
  "F_hake_ashop_array",
  "F_hake_shoreside_array",
  "cum_M2_temp",
  # "tau_q_troll",
  # "tau_q_rec",
  "theta_space",
  #"phi_space",
  "w_star_sf",
  "w_star_ws",
  "w_star_salish"
  
 )
#"Z_init","Z_move","Z_spawn")#,"M2_mean","M2_sd")
 

# Use this section if you want to start the distribution values in close to the right spot to speed up convergence.
  # load(paste(base.dir,"/Salmon-Climate/Mixed Model/Start Values/W_star_sf_start_vals.RData",sep=""))
  # load(paste(base.dir,"/Salmon-Climate/Mixed Model/Start Values/W_star_ws_start_vals.RData",sep=""))
  # load(paste(base.dir,"/Salmon-Climate/Mixed Model/Start Values/W_star_salish_start_vals.RData",sep=""))

### Initial Values
  stan_init_f1 <- function(n.chain,N_loc_spawn,MU_m,Sigma_m,N_rel,N_loc,N_f_rec_idx_param,N_knot_sf,N_knot_ws,
                           W_star_sf,W_star_ws,W_star_salish,N_troll_idx, N_rec_us_idx, N_sigma_cv_idx){ 
        A <- list()
  for(i in 1:n.chain){
    A[[i]] <- list(
      log_q_troll_start  = rnorm(N_troll_idx,-10,0.5),
      log_q_troll_slope  = rnorm(1,1,0.2),
      log_q_treaty_start  = rnorm(1,-10,0.5),
      log_q_treaty_slope  = rnorm(1,1,0.1),
      log_q_rec_start    = rnorm(N_rec_us_idx,-13,0.5),
      log_q_rec_slope    = rnorm(1,1,0.1),
      log_q_rec_can_start    = rnorm(1,-14,0.5),
      log_q_rec_can_slope    = rnorm(1,1,0.1),
      log_q_rec_can_irec_start    = rnorm(1,-14,0.5),
      log_q_hake_ashop_start    = rnorm(1,-14,0.5),
      log_q_hake_shoreside_start    = rnorm(1,-14,0.5),
      q_int = rnorm(1,-3,0.5),
      #log_q_rec_half    = rnorm(1,-20,1),
      #log_q_treaty_pos = rnorm(1,-6,0.5),
      #log_q_rec_can_half    = rnorm(1,-20,1),
      #log_q_rec_PUSO_pos    = rnorm(1,-12,0.5),
      #sigma_pos = rgamma(1,3,2),
      sigma_cv = runif(2,0.8,1.2),
      sigma_cv_hake = runif(1,0.8,1.2),
      #sigma_slope = runif(1,-0.01,0),
      #sigma_slope_samp = runif(1,-0.01,0),
      spawn_smooth = runif(1,3,5),
      # theta_space = c(2.05, 	3.10),
      # phi_space = runif(1,2,3),
      # # sigma_pos = runif(2,0.5,2),
      # sigma_pos_vec = runif(N_obs_pos,0.5,2),
      #vuln_int = rnorm(1,3,0.3),
      #beta_vuln_int = runif(2,0.1,0.5),
      beta_vuln = runif(2,0.5,1.0),
      beta_vuln_hake = c(runif(1,0.1,0.2),runif(1,-0.05,0)),
      #log_M2 = mvrnorm(1,MU_m,Sigma_m),
      #beta_age_month = rnorm(1,-0.15,0.01),
      #beta_age_year = rnorm(N_loc_spawn,-0.3,0.1),
      log_rel_year_mu = rnorm(1,0.5,0.2),
      log_rel_year_sigma = runif(1,0.2,0.5),
     # rel_year_all = runif(N_rel,1,2),
      log_F_rec_mean = rnorm(1,-4,0.5),
      log_F_troll_mean = rnorm(1,-4,0.5),
      #logit_offset_slope = rnorm(1,1,0.1),
      F_troll = runif(N_f_troll_idx_param,0.001,0.04),
      F_rec = runif(N_f_rec_idx_param,0.001,0.04),
      #alpha_pay = rnorm(N_loc_spawn,-5,0.5),
      log_beta_pay = rnorm(N_loc_spawn,0,0.5),
      alpha_pay_mean = rnorm(1,-5,0.5),
      alpha_pay_sd = runif(1,0.1,2),
      # beta_pay_mean = rnorm(1,2,0.5),
      # beta_pay_sd = runif(1,0.1,2),
      # alpha_pay = rnorm(N_loc_spawn,-5,0.5),
      # beta_pay = rnorm(N_loc_spawn,2,0.5),
      #phi_space = rgamma(2,300,100),
      w_star_sf = array(runif(N_loc_spawn*2*N_knot_sf,-0.1,0.1),dim=c(2,N_loc_spawn,N_knot_sf)),
      w_star_ws = array(runif(N_loc_spawn*N_knot_ws,-0.1,0.1),dim=c(N_loc_spawn,N_knot_ws)),
      w_star_salish = array(runif(N_loc_spawn*2*3,-0.1,0.1),dim=c(3,N_loc_spawn,3))
      # w_star_sf = W_star_sf,
      # w_star_ws = W_star_ws,
      # w_star_salish = W_star_salish
      )
  }  
  return(A)
}
                # matrix<lower=0,upper=1>[N_LOC,N_LOC] Z_move[N_LOC_INIT,N_month];
                # matrix<lower=0,upper=1>[N_LOC,N_LOC] Z_spawn[N_LOC_INIT,model_year_int_max];
# Some options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
##############
# stanvb = vb(stan_model(model_code=model.stan))
# 
#               verbose = FALSE, chains = N_CHAIN, thin = 1, 
#               warmup = Warm, iter = Warm + Iter, 
#               control = list(max_treedepth=Treedepth,adapt_delta=Adapt_delta,metric="diag_e"),
#               
# 
#               init = stan_init_f1(n.chain=N_CHAIN,N_loc_spawn=N_loc_spawn,
#                          MU_m =MU_m,Sigma_m=Sigma_m,
#                          N_rel=N.REL,N_loc=N.LOC,
#                          N_f_rec_idx_param=N_f_rec_idx_param,
#                          origin_loc_init = origin_loc_init)
# 


# write everything but the stanfit object to file
TREATY <- TRUE
SEASON <- TRUE
# NAME <- "CLIMATE Troll_Rec_Treaty_SS+PROC_E100_M2FIX_effort-ts-q_vulnfix_12-22-2017-FIT_ONLY two sigma-24mon-1978-zeroPUSO-EXTRA F(0.2)- noREtau EE"
# 
# Output <- list(#stanMod=stanMod,stanMod_summary=stanMod_summary,
#                #converge=samp_params, pars=pars,
#                raw.dat.bin=dat.bin.fin,raw.dat.pos=dat.pos.fin,N_CHAIN=N_CHAIN,
#                PRIORS = PRIORS,cum_M2_fixed = cum_M2_fixed, origin_vec=origin_vec,
#                stan_pars = stan_pars,
#                AGE = cbind(M2,AGE), 
#                COV = COV, age_month_cal=age_month_cal,
#                ocean_temp_dev = ocean.temp.dev, ocean_temp = ocean.temp,
#                river_entry=river_entry, #vuln_int_idx = vuln_int_idx, vuln_int_rec_idx = vuln_int_rec_idx,
#                temperature_season_idx = temperature_season_idx,
#                N_years_recover = N_years_recover,  N_years_release = N_years_release, N_month = N_month, N_time_mod=N_time_mod,
#                spawn_loc=spawn_loc_2,TREATY= TREATY,SEASON=SEASON, NAME = NAME, REL=REL,
#                YEARS.RELEASE=YEARS.RELEASE,YEARS.RECOVER=YEARS.RECOVER,YEARS.BROOD=YEARS.BROOD)
# 
# setwd("/Users/ole.shelton/GitHub/Salmon-Climate/Output files/_Fit objects")
# # indexes associated with temperatures
# save(Output,file=paste(GROUP," ",NAME,".RData",sep=""))


###############################################################################
###############################################################################
###############################################################################
###############################################################################
setwd("/Users/ole.shelton/GitHub/Salmon-Climate/Mixed Model")
stanMod = stan(file = MOD.NAME,
               data = stan_data, 
               verbose = FALSE, chains = N_CHAIN, thin = 1, 
               warmup = Warm, iter = Warm + Iter, 
               control = list(max_treedepth=Treedepth,
                              adapt_delta=Adapt_delta,
                              #adapt_init_buffer=75,
                              stepsize = 0.01,
                              metric="diag_e"),
               pars = stan_pars,
               boost_lib = NULL,
               sample_file = SAMP.FILE,
               init = stan_init_f1(n.chain=N_CHAIN,N_loc_spawn=N_loc_spawn,
                                   MU_m =MU_m,Sigma_m=Sigma_m,
                                   N_rel=N.REL,N_loc=N.LOC,
                                   N_f_rec_idx_param=N_f_rec_idx_param,
                                   N_knot_sf=N_knot_sf, N_knot_ws=N_knot_ws,
                                   W_star_sf = W_star_sf, W_star_ws = W_star_ws, W_star_salish =W_star_salish,
                                   N_troll_idx=N_troll_idx,N_rec_us_idx=N_rec_us_idx, N_sigma_cv_idx=N_sigma_cv_idx),
               init_r = 1
                                   ) 

pars <- rstan::extract(stanMod, permuted = T)
# get_adaptation_info(stanMod)
samp_params <- get_sampler_params(stanMod)
#
base_params <- c(#"tau_process",
                 #"tau_process_prod",
                  "log_q_rec_start","log_q_troll_start","log_q_rec_can_start","log_q_treaty_start",
                 "log_q_rec_slope","log_q_troll_slope","log_q_rec_can_slope","log_q_treaty_slope",
                 "log_q_rec_can_irec_start","log_q_hake_ashop_start","log_q_hake_shoreside_start",
                 "q_int",
                 #"log_q_troll_pos", "log_q_rec_pos","log_q_treaty_pos","log_q_rec_can_pos",#"log_q_rec_PUSO_pos",
                 #"logit_offset_slope",
                 #"sigma_pos",
                 "sigma_cv","sigma_cv_hake",
                 #"sigma_slope",
                 #"sigma_slope_samp",
                 # "alpha_pay_mean",
                 # "alpha_pay_sd",
                 # "beta_pay_mean",
                 # "beta_pay_sd",
                 "log_rel_year_mu", "log_rel_year_sigma",
                 "beta_vuln","beta_vuln_hake",
                 #"log_M2",
                 "log_F_rec_mean","log_F_troll_mean","F_rec_sigma","F_troll_sigma"
                 #"tau_q_troll","tau_q_rec"
                 # "kappa_troll", "kappa_treaty",
                 # "kappa_rec", "kappa_rec_can"
                 )#"prob_age_year") #"beta_age_month""vuln_int"

#print(traceplot(stanMod,pars=c("lp__"),inc_warmup=F))
# summary(stanMod,pars=base_params)$summary
# 
stanMod_summary <- summary(stanMod)$summary
summary(stanMod,pars=base_params)$summary
summary(stanMod,pars=c("alpha_pay","beta_pay"))

# origin_summary <-summary(stanMod,pars=c("origin_sea_int","origin_sea_slope"))
# origin_summary <- summary(stanMod,pars=c("origin_sea_int","origin_sea_slope"))$summary
# origin_mat_summary <- as.data.frame(summary(stanMod,pars=c("origin_mat"))$summary)


# If you want to save the posterior as start values for later.
 #  W_star_sf <- pars$w_star_sf[1,,,]
 #  W_star_ws <- pars$w_star_ws[1,,,]
 #  W_star_salish <- pars$w_star_salish[1,,,]
 # 
 # save(W_star_salish,file="W_star_salish_start_vals.RData")
 # save(W_star_ws,file="W_star_ws_start_vals.RData")
 # save(W_star_sf,file="W_star_sf_start_vals.RData")

# origin_loc   <- apply(pars$origin_loc,c(2,3,4),mean)
# dim(origin_loc)
#rowSums(origin_loc[,9,])
# 
# print(traceplot(stanMod,pars=c("lp__",base_params),inc_warmup=F))
# 
Output <- list(stanMod=stanMod,stanMod_summary=stanMod_summary,
               converge=samp_params, pars=pars,
               raw.dat.all=dat.all,raw.dat.pos=dat.pos.fin,N_CHAIN=N_CHAIN,
               PRIORS = PRIORS,cum_M2_fixed = cum_M2_fixed, origin_vec=origin_vec,
               diri_constant=diri_constant,
               ashop_year_break=ashop_year_break,
               stan_pars = stan_pars,
               AGE = AGE,
               COV = COV, age_month_cal=age_month_cal,
               ocean_temp_dev = ocean.temp.dev, ocean_temp = ocean.temp, ocean_temp_avg =time.avg.deep.trim,
               river_entry=river_entry,
               shaker_mort = shaker_mort,
               knot.loc.sum.fall = knot.loc.sum.fall, knot.loc.wint.spr = knot.loc.wint.spr,
               #logit_offset_int = logit_offset_int,
               #vuln_int_idx = vuln_int_idx, vuln_int_rec_idx = vuln_int_rec_idx,
               q_year_vec=q_year_vec,
               phi_space_fix = phi_space_fix,
               vuln_age=vuln_age,vuln_age_trawl=vuln_age_trawl, vuln_fixed=vuln_fixed,MONTH.vuln =MONTH.vuln, MONTH.vuln.trawl =MONTH.vuln.trawl,
               vuln_troll_mat = vuln_troll_mat, vuln_treaty_mat =vuln_treaty_mat,  vuln_rec_mat =vuln_rec_mat,
               spawn_time_fraction=REL$spawn_time_fraction,
               temperature_season_idx = temperature_season_idx,
               N_years_recover = N_years_recover,  N_years_release = N_years_release, N_month = N_month, N_time_mod=N_time_mod,
               spawn_loc=spawn_loc_2,TREATY= TREATY,SEASON=SEASON, NAME = NAME, REL=REL,
               loc_18=loc_18,MONTH.STRUCTURE=MONTH.STRUCTURE, CLOGLOG=CLOGLOG, SPAWN=SPAWN,
               TRAWL.US=TRAWL.US, TRAWL.BC=TRAWL.BC, TRAWL.AK=TRAWL.AK,
               first.season = first.season, last.season=last.season,
               YEARS.RELEASE=YEARS.RELEASE,YEARS.RECOVER=YEARS.RECOVER,YEARS.BROOD=YEARS.BROOD)

setwd("/Users/ole.shelton/GitHub/Salmon-Climate/Output files/_Fit objects")
save(Output,file=paste(GROUP," ",NAME,".RData",sep=""))


# code.dir    <- "/Users/ole.shelton/GitHub/Salmon-Climate/Mixed Model post processing"
# setwd(code.dir)
# source("Diagnostics and descriptive plots SS CLIMATE.R")

