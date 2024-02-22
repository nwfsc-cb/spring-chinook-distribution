### DATA EXTRACTIONS FROM THE FITTED MODEL OBJECT

mod  <- Output$stanMod
samp <- Output$pars
dat.all <- Output$raw.dat.all
dat.pos <- Output$raw.dat.pos
converge <- Output$converge

ORIGIN.GROUPS <-Output$ORIGIN.GROUPS
##########################################'''\
## DATA AND INDICES
##########################################'''\
MONTH.STRUCTURE=Output$MONTH.STRUCTURE
TRAWL.US = Output$TRAWL.US
TRAWL.BC = Output$TRAWL.BC
TRAWL.AK = Output$TRAWL.AK

dat.all.fin <- dat.all
dat.pos.fin <- dat.pos

PIT.dat.fin <- Output$PIT.dat.fin
AWG_dat <- Output$AWG_dat

log_N0          = log(Output$REL$N.released)
#logit_offset_int <- dat.bin.fin$logit_offset_int
#cum_M2  <- Output$cum_M2
spawn_loc     <- Output$spawn_loc
TREATY        <- Output$TREATY
SEASON        <- Output$SEASON
NAME          <- Output$NAME
REL           <- Output$REL
AGE           <- Output$AGE
age_month_cal <- Output$age_month_cal
PRIORS        <- Output$PRIORS
COV           <- Output$COV
river_entry   <- Output$river_entry
spawn_loc     <- Output$spawn_loc
# vuln_int_idx  <- Output$vuln_int_idx
# vuln_int_rec_idx       <- Output$vuln_int_rec_idx
MONTH.vuln    <- Output$MONTH.vuln
vuln_fixed    <- Output$vuln_fixed
vuln_age      <- Output$vuln_age
vuln_troll_mat <- Output$vuln_troll_mat
vuln_rec_mat   <- Output$vuln_rec_mat
vuln_treaty_mat <- Output$vuln_treaty_mat
vuln_age_hake <- Output$vuln_age_hake
vuln_age_pollock <- Output$vuln_age_pollock
constant       <- 1e-10

q_year_vec <- Output$q_year_vec

diri_constant = Output$diri_constant

# Spatial variantion in 
phi_space_fix <- Output$phi_space_fix

ashop_year_break <- Output$ashop_year_break

# Sampling fraction information.
frac_samp <- dat.all.fin$frac_samp_mod
inv_frac_samp <- dat.all$frac_samp_mod^(-1)
log_frac_samp <- log(dat.all$frac_samp_mod)
inv_frac_samp_pos <- dat.pos.fin$frac_samp_mod^(-1)
log_inv_frac_samp_pos <- log(dat.pos.fin$frac_samp_mod^(-1))
age.month  <- sort(unique(dat.all.fin$age.month))


temperature_season_idx <- Output$temperature_season_idx
temp_dat_season_bin_idx = dat.all.fin$temp_dat_season_idx
#temp_dat_season_pos_idx = dat.pos.fin$temp_dat_season_idx

N_years_release <- Output$N_years_release
N_years_recover <- Output$N_years_recover
years.recover   <- Output$YEARS.RECOVER
N_month    <- Output$N_month
N_obs_all  <- nrow(dat.all)
N_obs_pos  <- nrow(dat.pos)
N_time_mod <- Output$N_time_mod
N_loc      <- nrow(LOCATIONS)
N_year     <- max(AGE$year)
N_origin   <- length(unique(REL$loc.numb))
N_season   <- 4
spawn_time_fraction <- Output$spawn_time_fraction

shaker_mort = Output$shaker_mort
origin_vec  <- Output$origin_vec

#  Make spawning proportions helper files
E_alpha         <- PRIORS$E_alpha
E_prop_1 <- Output$E_prop_1 # this is the proportion spawning for fingerings (column are ages, rows are release stocks)
E_prop_2 <- Output$E_prop_2 # this is the proportion spawning for yearlings (column are ages, rows are release stocks)


# ocean_temp_avg = Output$ocean_temp_avg
# ocean_temp_dev = Output$ocean_temp_dev
# ocean_temp     = Output$ocean_temp

  vuln_troll_array  <- array(0,dim=c(N_years_release,N_time_mod,N_loc))
  vuln_rec_array    <- vuln_troll_array
  vuln_treaty_array <- vuln_troll_array
  #if(TRAWL.US=="TRUE"){
    vuln_hake_ashop_array <- vuln_troll_array
    vuln_hake_shoreside_array <- vuln_troll_array
    vuln_hake_pollock_GOA_array <- vuln_troll_array
    vuln_hake_rockfish_AK_array <- vuln_troll_array
  #}
    
  for(i in 1:N_years_release){ ## Make an array of the same dimensions as the state vector
    START <- (1+(i-1)*N_month)
    STOP  <- START + N_time_mod -1
    
    vuln_troll_array[i,1:N_time_mod,1:N_loc]    = as.matrix(vuln_troll_mat[START:STOP,1:N_loc] )
    vuln_treaty_array[i,1:N_time_mod,1:N_loc]   = as.matrix(vuln_treaty_mat[START:STOP,]) 
    vuln_rec_array[i,1:N_time_mod,1:N_loc]      = as.matrix(vuln_rec_mat[START:STOP,] )
    ##vuln_hake_ashop_array[i,1:N_time_mod,1:N_loc]      = as.matrix(vuln_hake_ashop_mat[START:STOP,] )
    ##vuln_hake_shoreside_array[i,1:N_time_mod,1:N_loc]  = as.matrix(vuln_hake_shoreside_mat[START:STOP,] )
  }

########################################################################
# Calculate summaries of various parameters
########################################################################
    #temp <-apply(samp$prob_age_year,c(2,3),median)

# Derive expected distribution under average ocean conditions.
# Call this quantity "origin_loc" (with mean and sd identifiers appended)

#origin_sea_int_exp <- exp(samp$origin_sea_int)
# THIS <- which(origin_vec ==0)
# origin_sea_int_exp[,,,THIS] <- 1

# SUMS <- apply(origin_sea_int_exp,c(2,3),rowSums)
# origin_loc <- array(0,dim=c(dim(origin_sea_int_exp)))
# for(i in 1:N_season){
#   for(j in 1:N_origin){
#     origin_loc[,i,j,] <- origin_sea_int_exp[,i,j,] / SUMS[,i,j]
#   }
# }

origin_loc <- samp$origin_mat

origin_loc_mean   <- apply(origin_loc,c(2,3,4),mean)
origin_loc_sd     <- apply(origin_loc,c(2,3,4),sd)
origin_loc_median <- apply(origin_loc,c(2,3,4),median)

origin_loc_dat <- melt(origin_loc) %>% as.data.frame()
colnames(origin_loc_dat) <- c("iterations","origin","season","loc","prop")

# fraction offshore.
origin_off_dat <- samp$origin_off
origin_off_dat <- melt(origin_off_dat) %>% as.data.frame()
colnames(origin_off_dat) <- c("iterations","origin","prop_off")

origin_off_summary <- origin_off_dat %>% ungroup() %>% group_by(origin) %>%
                        summarise(Mean=mean(prop_off),SD=sd(prop_off),
                                  q.0.05 = quantile(prop_off,probs=0.05),
                                  q.0.95 = quantile(prop_off,probs=0.95)) %>% 
                       left_join(.,ORIGIN.GROUPS,by=c("origin"="origin.idx"))

origin_off_mean = origin_off_summary %>% dplyr::select(Mean) %>% as.matrix()


# origin_off <- origin_loc_dat %>% group_by(origin,season) %>% summarise(OFF = 1-sum(prop)) 
# 
# origin_off_summary <- origin_off %>% ungroup() %>% group_by(origin,season) %>%
#                          summarise(Mean=mean(prop_off),SD=sd(prop_off),
#                          q.0.05 = quantile(prop_off,probs=0.05),
#                          q.0.95 = quantile(prop_off,probs=0.95)) %>%
#                       left_join(.,ORIGIN.GROUPS,by=c("origin"="origin.idx"))


# origin_mat       <- apply(samp$origin_mat,c(2,3,4),mean)
# origin_mat_sd    <- apply(samp$origin_mat,c(2,3,4),sd)
#origin_sea_slope <- apply(samp$origin_sea_slope,c(2,3,4),mean)

#theta_space <- apply(samp$theta_space,2,mean)
theta_space <- mean(samp$theta_space)
#phi_space   <- mean(samp$phi_space)
phi_space <- phi_space_fix

rel_year_all    <- apply(samp$rel_year_all,c(2),mean)
rel_year_sd    <- apply(samp$rel_year_all,c(2),sd)
 log_rel_year_mu <- mean(samp$log_rel_year_mu)
 log_rel_year_sigma  <- mean(samp$log_rel_year_sigma)

#beta_vuln_int   <- apply(samp$beta_vuln_int,2,mean)
beta_vuln       <- apply(samp$beta_vuln,2,mean)
beta_vuln_hake  <- apply(samp$beta_vuln_hake,2,mean) #mean(samp$beta_vuln_hake)
beta_vuln_pollock  <- apply(samp$beta_vuln_pollock,2,mean) #mean(samp$beta_vuln_hake)

#vuln_mat        <- apply(samp$vuln_mat,c(2,3),mean)
cum_M2          <- apply(samp$cum_M2,2,mean)
cum_M2_temp     <- apply(samp$cum_M2_temp,2,mean)
cum_M2_fixed    <- Output$cum_M2_fixed

prob_age_year   <- apply(samp$prob_age_year,c(2,3,4),mean)
spawn_smooth    <- mean(samp$spawn_smooth)

q_int <- mean(samp$q_int)



if(length(dim(samp$log_q_troll_start))>1){
  log_q_troll_start <- apply(samp$log_q_troll_start,2,mean)  
}else{
  log_q_troll_start <- mean(samp$log_q_troll_start)  
}
  
  
log_q_treaty_start  <- mean(samp$log_q_treaty_start)
if(length(dim(samp$log_q_rec_start))>1){
  log_q_rec_start     <- apply(samp$log_q_rec_start,2,mean)
}else{
  log_q_rec_start     <- mean(samp$log_q_rec_start)
}
log_q_rec_can_start <- mean(samp$log_q_rec_can_start)
log_q_rec_can_irec_start <- mean(samp$log_q_rec_can_irec_start)

 if(length(dim(samp$log_q_troll_slope))>1){
   log_q_troll_slope <- apply(samp$log_q_troll_slope,2,mean)  
 }else{
   log_q_troll_slope   <- mean(samp$log_q_troll_slope)  
 }
 #log_q_treaty_slope  <- mean(samp$log_q_treaty_slope)
 log_q_rec_slope     <- mean(samp$log_q_rec_slope)
 log_q_rec_can_slope <- mean(samp$log_q_rec_can_slope)

  log_q_troll_pos   = apply(samp$log_q_troll_pos,2,mean)
  log_q_treaty_pos  = apply(samp$log_q_treaty_pos,2,mean)
  log_q_rec_pos     = apply(samp$log_q_rec_pos,2,mean)
  log_q_rec_can_pos = apply(samp$log_q_rec_can_pos,2,mean)

  if(TRAWL.US=="TRUE"){
    log_q_hake_ashop_start     <- mean(samp$log_q_hake_ashop_start)
    log_q_hake_shoreside_start <- mean(samp$log_q_hake_shoreside_start) 
    log_q_hake_ashop_pos       <- apply(samp$log_q_hake_ashop_pos,2,mean)
    log_q_hake_shoreside_pos   <- apply(samp$log_q_hake_shoreside_pos,2,mean)
  }
  if(TRAWL.BC=="TRUE"){
    log_q_hake_bc <- apply(samp$log_q_hake_bc,2,mean)
      }
  if(TRAWL.AK=="TRUE"){
    log_q_pollock_GOA_start <- mean(samp$log_q_pollock_GOA_start)
    log_q_rockfish_AK_start <- mean(samp$log_q_rockfish_AK_start)
    log_q_pollock_GOA_pos   <- apply(samp$log_q_pollock_GOA_pos,2,mean)
    log_q_rockfish_AK_pos   <- apply(samp$log_q_rockfish_AK_pos,2,mean)
    #log_q_pollock_shoreside <- apply(samp$log_q_pollock_shoreside,2,mean) 
  }

    # Observation variability parameters. (CV = exp(sigma_int))
  fix_cv <- 0.1
  
  # if(is.na(dim(samp$sigma_cv)[2])==T){
  #   sigma_cv <- mean(samp$sigma_cv)
  # }else{
  #   sigma_cv <- apply(samp$sigma_cv,2,mean)
  # }
  # 
  #   sigma_cv_hake <- mean(samp$sigma_cv_hake)
  #   sigma_cv_pollock <- mean(samp$sigma_cv_pollock)
  
  #sigma_pos_slope <- mean(samp$sigma_pos_slope)
  #sigma_pos_slope_samp <- samp$sigma_pos_slope_samp


  #sigma_cv_hake <- mean(samp$sigma_cv_hake)
  
  #sigma_pos_slope <- mean(samp$sigma_pos_slope)
  #sigma_pos_slope_samp <- samp$sigma_pos_slope_samp
  
  #logit_offset_slope      <- mean(samp$logit_offset_slope)
  #observe_frac            <- mean(samp$observe_frac)
  
  #logit_offset_int        <- log(frac_samp * observe_frac) - log(1-(frac_samp * observe_frac)) ;
  #logit_offset_int        <- log(frac_samp) - log(1-(frac_samp)) ;

# Process error
#tau_process_prod <- mean(samp$tau_process_prod)
epsilon     <- apply(samp$epsilon,c(2,3),mean)

D         <- apply(samp$D,c(2,3),mean)
D_sd      <- apply(samp$D,c(2,3),sd)
prop_D    <- apply(samp$prop_D,c(2,3),mean)

colnames(D) <- colnames(AWG_dat)[grep("mod",colnames(AWG_dat))]
colnames(D_sd) <- colnames(AWG_dat)[grep("mod",colnames(AWG_dat))]

D <- as.data.frame(D)
D$ID_numb <- 1:nrow(D)
D_sd <- as.data.frame(D_sd)
D_sd$ID_numb <- 1:nrow(D_sd)

AWG_dat_long <- pivot_longer(AWG_dat,cols=grep("mod",colnames(AWG_dat)),
                             names_to = "mod.year",values_to="obs_count")
D_long  <- pivot_longer(D,cols=grep("mod",colnames(D)),
                        names_to = "mod.year",values_to="pred_mean_count")                             
D_long_sd  <- pivot_longer(D_sd,cols=grep("mod",colnames(D_sd)),
                        names_to = "mod.year",values_to="pred_sd_count")                             

AWG_dat_long <- left_join(AWG_dat_long,D_long) %>% left_join(.,D_long_sd) %>%
  left_join(.,REL)

#########################
F_rec     <- apply(samp$F_rec,c(2),mean)
# log_F_rec_mean <- mean(samp$log_F_rec_mean)
# log_F_troll_mean <- mean(samp$log_F_troll_mean)
#F_sigma   <- mean(samp$F_sigma)
# F_rec_sigma   <- mean(samp$F_rec_sigma)
# F_troll_sigma   <- mean(samp$F_troll_sigma)

# States and beginning / end ratio
log_N_all <- apply(samp$log_N_all,c(2,3),mean)
log_N_off <- apply(samp$log_N_off,c(2,3),mean)
# log_N_ratio <- apply(samp$log_N_ratio,c(2),mean)

F_troll_array <- apply(samp$F_troll_array,c(2,3),mean)
F_treaty_array <- apply(samp$F_treaty_array,c(2,3),mean)
F_rec_array <- apply(samp$F_rec_array,c(2,3),mean)
if(TRAWL.US=="TRUE"){
  F_hake_ashop_array <- apply(samp$F_hake_ashop_array,c(2,3),mean)
  F_hake_shoreside_array <- apply(samp$F_hake_shoreside_array,c(2,3),mean)
}

if(TRAWL.AK=="TRUE"){
  F_pollock_GOA_array <- apply(samp$F_pollock_GOA_array,c(2,3),mean)
  F_rockfish_AK_array <- apply(samp$F_rockfish_AK_array,c(2,3),mean)
}



AGE$M2_est <- cum_M2_temp

#OBJECTS THAT ARE IMPORTANT FROM THE STAN OBJECT

# Extract all of the indexes and name them the same as they are in STAN
# Indexes States Space
loc_spawn_idx     = COV$loc.spawn.idx
age_year_idx      = AGE$year
year_region_idx   = COV$year.reg.idx
age_month_idx     = AGE$age
spawn_time_array  = Output$spawn_time_array 

spawn_time        = AGE$spawn_time
spawn_time_idx    = AGE$spawn_time
origin_idx        = COV$origin.idx
season_idx        = AGE$season_idx
start_year        = COV$start_year
start_month_idx   = COV$start_month_idx


mod_time_idx        = dat.all$time
mod_time_N_all_idx  = dat.all$time - 1
rel_idx             = dat.all$rel
origin_bin_idx      = dat.all$origin.idx
season_bin_idx      = dat.all$season.idx
gear_bin_idx        = dat.all$gear.idx
loc_idx             = dat.all$location
loc_spawn_bin_idx   = dat.all$loc.spawn.idx
juv_bin_idx         = dat.all$juv.bin.idx


## Spawn helper file
spawn_helper <- COV %>% dplyr::select(ocean.region, origin.idx, juv.idx) %>%
  bind_cols(.,max.mod.year=apply(spawn_time_array,1,max)) %>%
  distinct(ocean.region,origin.idx,juv.idx,max.mod.year) %>% 
  arrange(origin.idx,juv.idx)

# mod_time_pos_idx      = dat.pos.fin$time
# rel_pos_idx           = dat.pos.fin$rel
# origin_pos_idx        = dat.pos.fin$origin.idx
# season_pos_idx        = dat.pos.fin$season.idx
# gear_pos_idx          = dat.pos.fin$gear.idx
# loc_pos_idx           = dat.pos.fin$location    
# loc_spawn_pos_idx    = dat.pos.fin$loc.spawn.idx
# loc_pos              = dat.pos.fin$location

### FISHING ASSOCIATED VALUES

 #F_troll_plus_vuln <- F_troll_array * vuln_mat[1,19]
 #F_rec_plus_vuln <- F_rec_array * vuln_mat[3,19]

### Calculate several scenarios of FISHING MORTALITY
F_TOT   <- data.frame(F_troll_array + 
                        F_rec_array + 
                        F_treaty_array +
                        F_hake_ashop_array +
                        F_hake_shoreside_array +
                        F_pollock_GOA_array +
                        F_rockfish_AK_array)
F_TROLL <- data.frame(F_troll_array)
F_TREATY<- data.frame(F_treaty_array)
F_REC   <- data.frame(F_rec_array)

if(TRAWL.US=="TRUE"){
  F_HAKE_ASHOP <- data.frame(F_hake_ashop_array)
  F_HAKE_SHORESIDE <- data.frame(F_hake_shoreside_array)
}
if(TRAWL.AK=="TRUE"){
  F_POLLOCK_GOA <- data.frame(F_pollock_GOA_array)
  F_ROCKFISH_AK <- data.frame(F_rockfish_AK_array)
}


if(MONTH.STRUCTURE=="FOUR"){ 
  Years   <- sort(c(rep(years.recover[1],4),rep(years.recover[2:(N_years_recover-1)],4),rep(years.recover[N_years_recover],3)))
  Seasons <- rep(c("Spr","Sum","Fal","Win"),N_years_recover) [1:length(Years)]
}
if(MONTH.STRUCTURE=="FRAM"){ 
  Years   <- sort(c(rep(years.recover[1],3),rep(years.recover[2:(N_years_recover-1)],4),rep(years.recover[N_years_recover],3)))
  Seasons <- rep(c("Sum","Fal","Win","Spr"),N_years_recover) [1:length(Years)]
}
if(MONTH.STRUCTURE=="SPRING"){ 
  Years   <- sort(c(rep(years.recover[1],4),rep(years.recover[2:(N_years_recover-1)],4),rep(years.recover[N_years_recover],1)))
  Seasons <- rep(c("Spr","Sum","Fal","Win"),N_years_recover) [1:length(Years)]
}



#   F_TROLL$years  <- Years
#   F_TROLL$season <- Seasons
#   F_REC$years  <- Years
#   F_REC$season <- Seasons
# 
# F_troll_flat   <- melt(F_TROLL,id=c("years","season"))
# F_troll_median <- aggregate(F_troll_flat$value,by=list(season=F_troll_flat$season,location = F_troll_flat$variable),median)
# F_troll_max    <- aggregate(F_troll_flat$value,by=list(season=F_troll_flat$season,location = F_troll_flat$variable),max)
# 
# F_rec_flat   <- melt(F_REC,id=c("years","season"))
# F_rec_median <- aggregate(F_troll_flat$value,by=list(season=F_rec_flat$season,location = F_rec_flat$variable),median)
# F_rec_max    <- aggregate(F_rec_flat$value,by=list(season=F_rec_flat$season,location = F_rec_flat$variable),max)
# 
# F_troll_zero_mat   <- matrix(0,4,N_loc)
# F_troll_median_mat <- F_troll_zero_mat
# F_troll_max_mat    <- F_troll_zero_mat
# F_rec_zero_mat   <- F_troll_zero_mat
# F_rec_median_mat <- F_troll_zero_mat
# F_rec_max_mat    <- F_troll_zero_mat
# 
# # MAKE F_Troll files
# F_troll_median_mat <- matrix(0,4,N_loc)
# F_troll_median_mat[1,] <- F_troll_median$x[order(F_troll_median$location)][F_troll_median$season=="Spr"]
# F_troll_median_mat[2,] <- F_troll_median$x[order(F_troll_median$location)][F_troll_median$season=="Sum"]
# F_troll_median_mat[3,] <- F_troll_median$x[order(F_troll_median$location)][F_troll_median$season=="Fal"]
# F_troll_median_mat[4,] <- F_troll_median$x[order(F_troll_median$location)][F_troll_median$season=="Win"]
# 
# F_troll_max_mat[1,] <- F_troll_max$x[order(F_troll_max$location)][F_troll_max$season=="Spr"]
# F_troll_max_mat[2,] <- F_troll_max$x[order(F_troll_max$location)][F_troll_max$season=="Sum"]
# F_troll_max_mat[3,] <- F_troll_max$x[order(F_troll_max$location)][F_troll_max$season=="Fal"]
# F_troll_max_mat[4,] <- F_troll_max$x[order(F_troll_max$location)][F_troll_max$season=="Win"]
# 
# rownames(F_troll_zero_mat) <- c("Spr","Sum","Fal","Win")
# rownames(F_troll_median_mat) <- c("Spr","Sum","Fal","Win")
# rownames(F_troll_max_mat) <- c("Spr","Sum","Fal","Win")
# 
# # MAKE F_rec files
# F_rec_median_mat <- matrix(0,4,N_loc)
# F_rec_median_mat[1,] <- F_rec_median$x[order(F_rec_median$location)][F_rec_median$season=="Spr"]
# F_rec_median_mat[2,] <- F_rec_median$x[order(F_rec_median$location)][F_rec_median$season=="Sum"]
# F_rec_median_mat[3,] <- F_rec_median$x[order(F_rec_median$location)][F_rec_median$season=="Fal"]
# F_rec_median_mat[4,] <- F_rec_median$x[order(F_rec_median$location)][F_rec_median$season=="Win"]
# 
# F_rec_max_mat[1,] <- F_rec_max$x[order(F_rec_max$location)][F_rec_max$season=="Spr"]
# F_rec_max_mat[2,] <- F_rec_max$x[order(F_rec_max$location)][F_rec_max$season=="Sum"]
# F_rec_max_mat[3,] <- F_rec_max$x[order(F_rec_max$location)][F_rec_max$season=="Fal"]
# F_rec_max_mat[4,] <- F_rec_max$x[order(F_rec_max$location)][F_rec_max$season=="Win"]
# 
# rownames(F_rec_zero_mat) <- c("Spr","Sum","Fal","Win")
# rownames(F_rec_median_mat) <- c("Spr","Sum","Fal","Win")
# rownames(F_rec_max_mat) <- c("Spr","Sum","Fal","Win")

#### Make a helper file for plotting. 
spawn_loc_plot <- spawn_loc
spawn_loc_plot$nom <- as.character(spawn_loc$ocean.region)
spawn_loc_plot$nom[substr(spawn_loc_plot$nom,2,3)=="OR"] <- "OR"
spawn_loc_plot$nom[substr(spawn_loc_plot$nom,3,4)=="VI"] <- "VI"
