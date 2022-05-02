# read in shared attributes
base.dir    <- "/Users/ole.shelton/GitHub"
LOCATIONS <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/locations.csv",sep=""))

### DATA EXTRACTIONS FROM THE FITTED MODEL OBJECT
mod  <- Output$stanMod
samp <- Output$pars
dat.bin <- Output$raw.dat.bin
dat.pos <- Output$raw.dat.pos
converge <- Output$converge

dat.bin.fin <- dat.bin
dat.pos.fin <- dat.pos
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
vuln_int_idx  <- Output$vuln_int_idx
vuln_int_rec_idx  <- Output$vuln_int_rec_idx
E_alpha       <- PRIORS$E_alpha
N_years_release <- Output$N_years_release
N_years_recover <- Output$N_years_recover
years.recover   <- 1979:(1979+N_years_recover-1)
N_month <- Output$N_month
N_obs_bin <- nrow(dat.bin)
N_obs_pos <- nrow(dat.pos)
N_time_mod <- Output$N_time_mod
N_loc <- nrow(LOCATIONS)
N_year <- max(AGE$year)

# Calculate summaries of various parameters
temp <-apply(samp$prob_age_year,c(2,3),median)

if(SEASON ==FALSE){  origin_loc   <- apply(samp$origin_loc,c(2,3),mean)}
if(SEASON ==TRUE) {  origin_loc   <- apply(samp$origin_loc,c(2,3,4),mean)}
rel_year_all    <- apply(samp$rel_year_all,c(2),mean)
 #log_rel_year_mu <- mean(samp$log_rel_year_mu)
 #rel_year_sigma  <- mean(samp$rel_year_sigma)

beta_vuln       <- apply(samp$beta_vuln,2,mean)
vuln_mat        <- apply(samp$vuln_mat,c(2,3),mean)
cum_M2          <- apply(samp$cum_M2,2,mean)
prob_age_year   <- apply(samp$prob_age_year,c(2,3),mean)

log_q_troll_pos   <- mean(samp$log_q_troll_pos)  
log_q_treaty_pos  <- mean(samp$log_q_treaty_pos)
log_q_rec_pos     <- mean(samp$log_q_rec_pos)
log_q_rec_can_pos <- mean(samp$log_q_rec_can_pos)
logit_offset      <- apply(samp$logit_offset,2,mean)

# Process error
  #tau_process <- apply(samp$tau_process,c(2,3),mean)

D         <- apply(samp$D,c(2,3),mean)
prop_D    <- apply(samp$prop_D,c(2,3),mean)
F_rec     <- apply(samp$F_rec,c(2),mean)
log_F_rec_mean <- mean(samp$log_F_rec_mean)
F_rec_sigma   <- mean(samp$F_rec_sigma)
log_N_all <- apply(samp$log_N_all,c(2,3),mean)

log_N_all <- apply(samp$log_N_all,c(2,3),mean)
log_N_ratio <- apply(samp$log_N_ratio,c(2),mean)

F_troll_array <- apply(samp$F_troll_array,c(2,3),mean)
F_rec_array <- apply(samp$F_rec_array,c(2,3),mean)

AGE$M2_est <- colMeans(samp$cum_M2_temp)

#OBJECTS THAT ARE IMPORTANT FROM THE STAN OBJECT

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


### FISHING ASSOCIATED VALUES

F_troll_plus_vuln <- F_troll_array * vuln_mat[1,19]
F_rec_plus_vuln <- F_rec_array * vuln_mat[3,19]

### Calculate several scenarios of FISHING MORTALITY
F_TOT   <- data.frame(F_troll_array + F_rec_array)
F_TROLL <- data.frame(F_troll_array)
F_REC   <- data.frame(F_rec_array)

years   <- c(rep(years.recover,4))
seasons <- rep(c("Spr","Sum","Fall","Wint"),N_years_recover) [1:length(years)]

F_TROLL$years  <- years
F_TROLL$season <- seasons
F_REC$years  <- years
F_REC$season <- seasons

F_troll_flat   <- melt(F_TROLL,id=c("years","season"))
F_troll_median <- aggregate(F_troll_flat$value,by=list(season=F_troll_flat$season,location = F_troll_flat$variable),median)
F_troll_max    <- aggregate(F_troll_flat$value,by=list(season=F_troll_flat$season,location = F_troll_flat$variable),max)

F_rec_flat   <- melt(F_REC,id=c("years","season"))
F_rec_median <- aggregate(F_troll_flat$value,by=list(season=F_rec_flat$season,location = F_rec_flat$variable),median)
F_rec_max    <- aggregate(F_rec_flat$value,by=list(season=F_rec_flat$season,location = F_rec_flat$variable),max)

F_troll_zero_mat   <- matrix(0,4,N_loc)
F_troll_median_mat <- F_troll_zero_mat
F_troll_max_mat    <- F_troll_zero_mat
F_rec_zero_mat   <- F_troll_zero_mat
F_rec_median_mat <- F_troll_zero_mat
F_rec_max_mat    <- F_troll_zero_mat

# MAKE F_Troll files
F_troll_median_mat <- matrix(0,4,N_loc)
F_troll_median_mat[1,] <- F_troll_median$x[order(F_troll_median$location)][F_troll_median$season=="Spr"]
F_troll_median_mat[2,] <- F_troll_median$x[order(F_troll_median$location)][F_troll_median$season=="Sum"]
F_troll_median_mat[3,] <- F_troll_median$x[order(F_troll_median$location)][F_troll_median$season=="Fall"]
F_troll_median_mat[4,] <- F_troll_median$x[order(F_troll_median$location)][F_troll_median$season=="Wint"]

F_troll_max_mat[1,] <- F_troll_max$x[order(F_troll_max$location)][F_troll_max$season=="Spr"]
F_troll_max_mat[2,] <- F_troll_max$x[order(F_troll_max$location)][F_troll_max$season=="Sum"]
F_troll_max_mat[3,] <- F_troll_max$x[order(F_troll_max$location)][F_troll_max$season=="Fall"]
F_troll_max_mat[4,] <- F_troll_max$x[order(F_troll_max$location)][F_troll_max$season=="Wint"]

rownames(F_troll_zero_mat) <- c("Spr","Sum","Fall","Wint")
rownames(F_troll_median_mat) <- c("Spr","Sum","Fall","Wint")
rownames(F_troll_max_mat) <- c("Spr","Sum","Fall","Wint")

# MAKE F_rec files
F_rec_median_mat <- matrix(0,4,N_loc)
F_rec_median_mat[1,] <- F_rec_median$x[order(F_rec_median$location)][F_rec_median$season=="Spr"]
F_rec_median_mat[2,] <- F_rec_median$x[order(F_rec_median$location)][F_rec_median$season=="Sum"]
F_rec_median_mat[3,] <- F_rec_median$x[order(F_rec_median$location)][F_rec_median$season=="Fall"]
F_rec_median_mat[4,] <- F_rec_median$x[order(F_rec_median$location)][F_rec_median$season=="Wint"]

F_rec_max_mat[1,] <- F_rec_max$x[order(F_rec_max$location)][F_rec_max$season=="Spr"]
F_rec_max_mat[2,] <- F_rec_max$x[order(F_rec_max$location)][F_rec_max$season=="Sum"]
F_rec_max_mat[3,] <- F_rec_max$x[order(F_rec_max$location)][F_rec_max$season=="Fall"]
F_rec_max_mat[4,] <- F_rec_max$x[order(F_rec_max$location)][F_rec_max$season=="Wint"]

rownames(F_rec_zero_mat) <- c("Spr","Sum","Fall","Wint")
rownames(F_rec_median_mat) <- c("Spr","Sum","Fall","Wint")
rownames(F_rec_max_mat) <- c("Spr","Sum","Fall","Wint")

#### Make a helper file for plotting. 
spawn_loc_plot <- spawn_loc
spawn_loc_plot$nom <- as.character(spawn_loc$ocean.region)
spawn_loc_plot$nom[substr(spawn_loc_plot$nom,2,3)=="OR"] <- "OR"
spawn_loc_plot$nom[substr(spawn_loc_plot$nom,3,4)=="VI"] <- "VI"



