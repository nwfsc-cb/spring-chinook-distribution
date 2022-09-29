  
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

C_pollock_GOA_pos <- C_pollock_GOA_true
C_pollock_GOA_pos[C_pollock_GOA_pos > 0] <- 1
C_pollock_GOA_zero <- abs(C_pollock_GOA_pos - 1)

C_rockfish_AK_pos <- C_rockfish_AK_true
C_rockfish_AK_pos[C_rockfish_AK_pos > 0] <- 1
C_rockfish_AK_zero <- abs(C_rockfish_AK_pos - 1)

# C_net_pos <- C_net_true
# C_net_pos[C_net_pos > 0] <- 1
# C_net_zero <- abs(C_net_pos - 1)
 
### OBSERVED CATCH by catch type:

####3 INITIAL RELEASES
N0 <- REL$N.released
####################################################################################################
# Create Movement matrices
max_age <- max(XX$model.year)

# Define Release Groups and Ocean regions of interest.
move_id <- REL %>% distinct(loc.numb,ocean.region) %>% arrange(loc.numb,ocean.region) %>% 
              mutate( move_id=1:nrow(.),move_id_idx=move_id,loc.spawn=loc.numb) %>% 
              dplyr::select(move_id,ocean_region=ocean.region,move_id_idx,loc.spawn)

N.move.group  <- nrow(move_id)
move_id_name  <- as.character(unique(move_id$move_id))
move_id_spawn <- as.character((move_id$loc.spawn))

# redefine move_id_idx as a vector
move_id_idx <- move_id$move_id_idx

############################################
# MATURITY and ESCAPEMENT
############################################
### Import the Dirichlet derived escapement data for each region.
### see CWT Maturity Proportion for this and Make catch and Escapement CLIMATE.R

source("./_R code for processing raw data/CWT Maturity Proportions Spr-Sum.R",local=T)
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
  # For spring run, time_fraction = 0 is March 1, time_fraction = 0.33 is April 1, 0.667 is May 1 
  REL <- REL %>% ungroup() %>% left_join(., ORIGIN.LAB %>% dplyr::select(ocean.region=origin.code,spawn_time_fraction) )
}

if(MONTH.STRUCTURE == "FRAM"){
  REL$spawn_time_fraction <- 0.667   # equivalent to Spetember 1 migration
}

#################################################################################################
# READ IN ENVIRONMENTAL DATA FOR USE IN DISTIRBUTION
#################################################################################################
#source("./Base_Code/Climate R Scripts/Ocean Temperatures.R",local=T)
# Raw temperature data:
# TEMP.DAT <- read.csv("./Processed Data/Temperature/Temperature Deep_2018.csv")
# 
# # Temperature Deviations calculated from 3 seasons model (winter is treated as deviation from spring mean for identifiability reasons)
# TEMP.DEV.DAT <- read.csv("./Processed Data/Temperature/Temperature Deviations 4seas Deep_2018.csv")
# 
#   ocean.temp     <-   TEMP.DAT %>% filter(year <= max(YEARS.RECOVER))  
#   #ocean.temp     <-  ocean.temp[2:nrow(ocean.temp),]
#   ocean.temp.dev <-  TEMP.DEV.DAT %>% filter(year <= max(YEARS.RECOVER))  
#   #ocean.temp.dev <-  ocean.temp.dev[2:nrow(ocean.temp.dev),]
#   rownames(ocean.temp.dev) <- paste(ocean.temp.dev$year,ocean.temp.dev$season,sep=".")
#   ocean.temp$season <- factor(ocean.temp$season,levels=c("Spr","Sum","Fal","Win"))
#   ocean.temp.dev$season <- factor(ocean.temp.dev$season,levels=c("Spr","Sum","Fal","Win"))
#   
#   ocean.temp <- ocean.temp %>% arrange(year,season)
#   ocean.temp.dev <- ocean.temp.dev %>% arrange(year,season) %>% as.data.frame()
#   rownames(ocean.temp.dev) <- paste(ocean.temp.dev$year,ocean.temp.dev$season,sep=".")
#   
#   # This makes winter have a deviation to zero and all years before 1981 have a deviation of zero
#   ocean.temp.dev[ocean.temp.dev$year <= 1981,3:ncol(ocean.temp.dev) ] <- 0
#   ocean.temp.dev[ocean.temp.dev$season =="Win",3:ncol(ocean.temp.dev) ] <- 0 
#   ocean.temp.dev <- ocean.temp.dev %>% dplyr::select(-year, -season)
#   #ocean.temp.dev <- ocean.temp.dev[1:N_season_total,]
#   
#   ocean.temp.dev <- ocean.temp.dev*0.1 #### THIS IS REALLY IMPORTANT. REMEMBER TO RESCALE FUTURE PREDICTIONS by 0.1
#   
#   # Eliminate deviations from PUSO and SGEO
#     #ocean.temp.dev$PUSO <- 0
#     #ocean.temp.dev$SGEO <- 0
#   
#   if(loc_18 == "TRUE" | loc_18 =="TWO_OR" | loc_18=="NCA_SOR_PUSO"){
#     #ocean.temp.dev$PUSO_out <- 0
#     CHAR <- c(as.character(LOCATIONS$location.name))
#     ocean.temp.dev <- ocean.temp.dev %>% dplyr::select(all_of(CHAR))
#     ocean.temp <- ocean.temp %>% dplyr::select(year,season,CHAR)
#   }
#   
#   #### MAKE A new index for working with temperature deviation data.
#   REL$origin_start_year_idx <- 1+(REL$start_year - 1)*N_month  
#   origin_year_idx <- matrix(seq(0,N.mod.month-1),N.REL,N.mod.month,byrow=T) + matrix(REL$origin_start_year_idx,N.REL,N.mod.month)
# 
#   if(MONTH.STRUCTURE =="FOUR"|MONTH.STRUCTURE=="SPRING"){
#     first <- which(rownames(ocean.temp.dev)==paste(min(YEARS.RECOVER),"Spr",sep="."))
#     last <- which(rownames(ocean.temp.dev)==paste(max(YEARS.RECOVER),"Fal",sep="."))
#     ocean.temp.dev <- ocean.temp.dev[first:last,]
#   }
#   
#   if(MONTH.STRUCTURE =="FRAM"){
#     first <- which(rownames(ocean.temp.dev)==paste(min(YEARS.RECOVER),"Sum",sep="."))
#     last <- which(rownames(ocean.temp.dev)==paste(max(YEARS.RECOVER),"Fal",sep="."))
#     ocean.temp.dev <- ocean.temp.dev[first:last,]
#   }
#     
#   temperature_season_idx    <- rep(0,nrow(ocean.temp.dev))
#   temperature_season_idx[grep("Win",rownames(ocean.temp.dev))] <- 1
#   temperature_season_idx[grep("Spr",rownames(ocean.temp.dev))] <- 1
#   temperature_season_idx[grep("Sum",rownames(ocean.temp.dev))] <- 2
#   temperature_season_idx[grep("Fal",rownames(ocean.temp.dev))] <- 3

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

#################################################
### Calculate the matrix that informs where fish can enter the river from the ocean.
source("./_R code for processing raw data/Make river entry matrices.R",local=T)

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
  vuln.troll.mat  <-   read.csv("./Processed Data/Vulnerability/vuln.troll.1978-2018.csv")
  vuln.treaty.mat <-   read.csv("./Processed Data/Vulnerability/vuln.treaty.1978-2018.csv")
  vuln.rec.mat    <-   read.csv("./Processed Data/Vulnerability/vuln.rec.1978-2018.csv")
}
if(MONTH.STRUCTURE == "FRAM"){
  vuln.troll.mat  <-   read.csv("./Processed Data/Vulnerability/vuln.troll.1978-2018_FRAM.csv")
  vuln.treaty.mat <-   read.csv("./Processed Data/Vulnerability/vuln.treaty.1978-2018_FRAM.csv")
  vuln.rec.mat    <-   read.csv("./Processed Data/Vulnerability/vuln.rec.1978-2018_FRAM.csv")
}

if(loc_18 =="TWO_OR" | loc_18 == "_two_OR_PUSO_AK"){
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
source("./_R code for processing raw data/Make smooth ocean distribution matrices Spr-Sum.R",local=T)

######## PLOT EFFORT AND CPUE FILES
source("./_R code for processing raw data/Plot effort, CPUE heatmaps CLIMATE.R",local=T)

############# MAKE A PDF of the various hatchery and release attributes.
#setwd(paste(base.dir,"/GSI_CWT_Chinook/Output plots/  __Markdown",sep=""))
#rmarkdown::render("./Output plots/  __Markdown/release-summaries.rmd",
#                  output_format = "pdf_document")
                  #output_file = "./Output plots/")

#### CALL THE ACTUAL STAN CODE:
#source(paste0(base.dir,"/GSI_CWT_Chinook/Base_Code/Mixed bin+pos SS + PROC logit prob_age CLIMATE ts-q SMOOTH.R"),local=T)


