#### RESIDUAL ANALYSIS.

# rm(list=ls())
library(ggplot2)
library(reshape2)
library(dplyr)
library(rstan)
library(gtools)
library(MASS)

### Make diagnostic and exploratory plots for model objects

base.dir    <- "/Users/ole.shelton/GitHub"
results.dir <- "/Users/ole.shelton/GitHub/Orca_Salmon/Output files/_Mixed Results"
code.dir    <- "/Users/ole.shelton/GitHub/Orca_Salmon_Code/Mixed model post processing"

setwd(results.dir)
load("Binomial+Positive_output-1978-90 Troll_Rec_Treaty_6year_STATE_SPACE_E25_M2EST_originP0_vuln1_90FIN.RData")
LOCATIONS <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/locations.csv",sep=""))
#source(paste(base.dir,"/Orca_Salmon_Code/_Stan code/Mixed Model/Prep raw data for CPUE analysis.R",sep=""))

# Extract the necessary information from the fitted model object.
setwd(code.dir)
source("Basic Data Extraction.R")

## MAKE PREDICTIONS FOR EACH OBSERVATION.  THIS IS DIRECTLY STOLEN FROM THE "OBS-PRED SS.R" script.
make.pred <- "TRUE"
if(make.pred == "TRUE"){

# Calculate the point predictions for each observations
# binomial

# Mixing First
base_params <- c(#"log_q_troll","log_q_rec","log_q_treaty",
  "log_q_troll_pos", "log_q_rec_pos","log_q_treaty_pos","log_q_rec_can_pos",
  "logit_offset",
  "sigma_pos",
  "log_rel_year_mu", "rel_year_sigma",
  "beta_vuln","vuln_int",
  #"log_M2",
  "log_F_rec_mean","F_rec_sigma"
)#"prob_age_year") #"beta_age_month""vuln_int"

#######################################  FIX ME!!! 
Baranov <- function(M2,  F_focal,  F_tot,  log_N,  log_origin){
  return  (log(F_focal) - log(M2 + F_tot) + log_N + log_origin + log(1 - exp(-(M2 + F_tot) )))  
}


# Extract all of the indexes and name them the same as they are in STAN
# Indexes States Space
loc_spawn_idx     = COV$loc.spawn.idx
age_year_idx      = AGE$year
year_region_idx   = COV$year.reg.idx
age_month_idx     = AGE$age
spawn_time        = AGE$spawn_time
spawn_time_idx    = AGE$spawn_time
vuln_int_idx      = vuln_int_idx
vuln_int_rec_idx  = vuln_int_rec_idx
origin_idx        = COV$origin.idx
season_idx        = AGE$season_idx
start_year        = COV$start_year


mod_time_idx        = dat.bin.fin$time
rel_idx             = dat.bin.fin$rel
origin_bin_idx      = dat.bin.fin$origin.idx
season_bin_idx      = dat.bin.fin$season.idx
gear_bin_idx        = dat.bin.fin$gear.idx
loc_idx             = dat.bin.fin$location
loc_spawn_bin_idx   = dat.bin.fin$loc.spawn.idx

mod_time_pos_idx      = dat.pos.fin$time
rel_pos_idx           = dat.pos.fin$rel
origin_pos_idx        = dat.pos.fin$origin.idx
season_pos_idx        = dat.pos.fin$season.idx
gear_pos_idx          = dat.pos.fin$gear.idx
loc_pos_idx           = dat.pos.fin$location    
loc_spawn_pos_idx    = dat.pos.fin$loc.spawn.idx
loc_pos              = dat.pos.fin$location

vuln_int_idx         = Output$vuln_int_idx 
vuln_int_rec_idx     = Output$vuln_int_rec_idx

# First Calculate all of the necessary fishing mortality params, just like in STAN
#F_troll_array      
#F_rec_array        

# make arrays on the scale of the release groups turn: (year,month,location) into (release id,model month, location)
N_loc <- length(unique(dat.bin$location))
N_rel <- nrow(REL)
N_time_mod <- max(dat.bin$time)
N_years_release
troll_mat <- matrix(0,N_time_mod,N_loc)
rec_mat   <- troll_mat


for(i in 1:N_loc){
  troll_mat[,i] <- vuln_mat[vuln_int_idx[i],]   
  rec_mat[,i]   <- vuln_mat[vuln_int_rec_idx[i],]
}     

F_troll_fin <- array(0,dim= c(N_time_mod,N_loc,N_years_release))
F_rec_fin   <- F_troll_fin
F_tot_fin   <- F_troll_fin

for(i in 1:N_years_release){ # Make an array of the same dimensions as the state vector
  START <- (1+(i-1)*N_month)
  STOP  <- START + N_time_mod -1 
  F_troll_fin[,,i]      <- F_troll_array[START:STOP,] * troll_mat  ;
  F_rec_fin[,,i]        <- F_rec_array[START:STOP,] * rec_mat  ;
  F_tot_fin[,,i]        <- F_troll_fin[,,i] + F_rec_fin[,,i] ; 
  # Process error
  #zeta_fin[i]         <- block(zeta_mat,(1+(i-1)*N_month),1,N_time_mod,N_loc) ;
}

############################################################################################
############################################################################################
###################################################################################################################
##################################################################################################################
############ LATENT STATES 


#cum_M2_temp <- cum_M2_fixed
cum_M2_temp <- cum_M2
for(i in 1:N_time_mod){
  if(i==1){cum_M2_temp[i]  <- cum_M2[age_month_idx[i]] ;} 
  if(i > 1){cum_M2_temp[i] <- cum_M2[age_month_idx[i]] - cum_M2[age_month_idx[i-1]] ;} 
}

prob<- NULL
### CALCULATING THE OBSERVATIONS 
### CALCULATING THE PREDICTIONS FOR OBSERVATIONS 
for(i in 1:N_obs_bin){
  if( gear_bin_idx[i]  == 1 ){ F_focal <- F_troll_fin[mod_time_idx[i],loc_idx[i],start_year[rel_idx[i]]] ; } 
  if( gear_bin_idx[i]  == 2 ){ F_focal <- F_rec_fin[mod_time_idx[i],loc_idx[i],start_year[rel_idx[i]]] ; }      
  F_tot <- F_tot_fin[mod_time_idx[i],loc_idx[i],start_year[rel_idx[i]]]
  if(spawn_time[mod_time_idx[i]] == 0){
    lambda_temp <- logit_offset[1]  +
      Baranov(  cum_M2_temp[mod_time_idx[i]],
                F_focal,
                F_tot,
                log_N_all[rel_idx[i],mod_time_idx[i]],
                log(origin_loc[season_idx[mod_time_idx[i]],origin_bin_idx[i],loc_idx[i]] ) );
  }
  if(spawn_time[mod_time_idx[i]] > 0){
    lambda_temp <- 
      Baranov(  cum_M2_temp[mod_time_idx[i]] * 0.5,
                F_focal * 0.5,
                F_tot * 0.5,
                log_N_all[rel_idx[i],mod_time_idx[i]],
                log(origin_loc[season_idx[mod_time_idx[i]],origin_bin_idx[i],loc_idx[i]] ) );
    log_N_temp_1 <- log_N_all[rel_idx[i],mod_time_idx[i]] - 
      F_tot * 0.5 -
      cum_M2_temp[mod_time_idx[i]] * 0.5 +
      log(origin_loc[season_idx[mod_time_idx[i]],origin_bin_idx[i],loc_idx[i]] ) +
      log(1 - prob_age_year[loc_spawn_bin_idx[i],spawn_time_idx[mod_time_idx[i]]] * river_entry[rel_idx[i],loc_idx[i]] ); 
    lambda_temp <- logit_offset[1] +
      log(exp( lambda_temp) +
            exp(Baranov(  cum_M2_temp[mod_time_idx[i]] * 0.5 ,
                          F_focal * 0.5,
                          F_tot * 0.5,
                          log_N_temp_1,
                          0)));
  }
  prob[i] <- exp(lambda_temp) / (exp(lambda_temp)+1) 
}

mu_pos <- NULL
for(i in 1:N_obs_pos){
  if(gear_pos_idx[i] ==1 ){ F_focal <- F_troll_fin[mod_time_pos_idx[i],loc_pos_idx[i],start_year[rel_pos_idx[i]]] ;}
  if(gear_pos_idx[i] ==2 ){ F_focal <- F_rec_fin[mod_time_pos_idx[i],loc_pos_idx[i],start_year[rel_pos_idx[i]]] ;}
  F_tot <- F_tot_fin[mod_time_pos_idx[i],loc_pos_idx[i],start_year[rel_pos_idx[i]]] 
  if(spawn_time[mod_time_pos_idx[i]] == 0){
    mu_pos[i] <- Baranov(  cum_M2_temp[mod_time_pos_idx[i]],
                           F_focal ,
                           F_tot ,
                           log_N_all[rel_pos_idx[i],mod_time_pos_idx[i]],
                           log(origin_loc[season_idx[mod_time_pos_idx[i]],origin_pos_idx[i],loc_pos_idx[i]] ) );
  }
  if(spawn_time[mod_time_pos_idx[i]] > 0){
    mu_pos[i] <- Baranov(  cum_M2_temp[mod_time_pos_idx[i]] * 0.5,
                           F_focal * 0.5,
                           F_tot * 0.5,
                           log_N_all[rel_pos_idx[i],mod_time_pos_idx[i]],
                           log(origin_loc[season_idx[mod_time_pos_idx[i]],origin_pos_idx[i],loc_pos_idx[i]] ) );
    log_N_temp_1 <- log_N_all[rel_pos_idx[i],mod_time_pos_idx[i]] - 
      F_tot * 0.5 -
      cum_M2_temp[mod_time_pos_idx[i]] * 0.5 +
      log(origin_loc[season_idx[mod_time_pos_idx[i]],origin_pos_idx[i],loc_pos_idx[i]] ) +
      log(1 - prob_age_year[loc_spawn_pos_idx[i],spawn_time_idx[mod_time_pos_idx[i]]] * river_entry[rel_pos_idx[i],loc_pos_idx[i]] ); 
    mu_pos[i] <- log(exp(mu_pos[i]) +
                       exp(Baranov(  cum_M2_temp[mod_time_pos_idx[i]] * 0.5 ,
                                     F_focal * 0.5,
                                     F_tot * 0.5,
                                     log_N_temp_1,
                                     0)));
  }
}
#######################
############################################################################################
####
}
### MAKE SOME PLOTS OF RESIDUALS

#3 LOOP over:
    # Origin
    # Ages
    # Seasons
    # Ocean Location


dat.bin.fin$prob <- prob

temp <- dat.bin.fin[dat.bin.fin$ocean.reg=="COL" & dat.bin.fin$year==3, ]


temp.agg <- aggregate(temp$prob, by=list(release_year = temp$release_year, location = temp$location,age.month=temp$age.month),mean)
colnames(temp.agg)[4] <- "MEAN"
temp.agg.2 <- aggregate(temp$prob, by=list(release_year = temp$release_year, location = temp$location,age.month=temp$age.month),sd)
colnames(temp.agg.2)[4] <- "SD"
temp.agg$SD <- temp.agg.2$SD
temp.agg.2 <- aggregate(temp$prob, by=list(release_year = temp$release_year, location = temp$location,age.month=temp$age.month),length)
colnames(temp.agg.2)[4] <- "Length"
temp.agg$Length <- temp.agg.2$Length
temp.agg$SE <- temp.agg$SD / sqrt(temp.agg$Length)


LOC  <- 6
AM   <- 26
y.lim=c(0,1)
plot(MEAN~release_year,dat=temp.agg[temp.agg$location==LOC & temp.agg$age.month == AM,],ylim=y.lim)
arrows(x0=temp.agg$release_year[temp.agg$location==LOC & temp.agg$age.month == AM],
       x1=temp.agg$release_year[temp.agg$location==LOC & temp.agg$age.month == AM],
       y0=temp.agg$MEAN[temp.agg$location==LOC & temp.agg$age.month == AM] - temp.agg$SE[temp.agg$location==LOC & temp.agg$age.month == AM],
       y1=temp.agg$MEAN[temp.agg$location==LOC & temp.agg$age.month == AM] + temp.agg$SE[temp.agg$location==LOC & temp.agg$age.month == AM],
       length=0
       )

par(new=T)

plot(catch~release_year,data=temp[temp$location==LOC & temp$age.month==AM,],col=2,ylim=y.lim)
















length(prob)