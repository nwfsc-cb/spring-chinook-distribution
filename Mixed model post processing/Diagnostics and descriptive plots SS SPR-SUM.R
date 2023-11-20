rm(list=ls()); gc()
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(rstan)
library(gtools)
library(MASS)
library(fields)
library(RColorBrewer)
library(viridis)
library(extrafont)
library(abind)
### Make diagnostic and exploratory plots for model objects

base.dir    <- "/Users/ole.shelton/GitHub"
results.dir <- "/Users/ole.shelton/GitHub/spring-chinook-distribution/Output files/_Fit objects"
code.dir    <- "/Users/ole.shelton/GitHub/spring-chinook-distribution/Mixed model post processing"
markdown.dir <- "/Users/ole.shelton/GitHub/spring-chinook-distribution/Writing/Markdown"
  
setwd(results.dir)

load("FRAM_2023_08 F1-NB=400;FIX-OFF-TROLL_4_param,hake!=pollock,WAPEN=EAPEN(all),cv=hake,pollock,vuln=hake,pollock;4-season(single offshore,wint=avg),year+fing,ZEROS.RData")

# load("CA+COL CLIMATE Troll_Rec_Treaty_SS+PROC_E100_M2EST_vulnfix_07-26-2017.RData")

LOCATIONS <- read.csv(paste(base.dir,"/spring-chinook-distribution/Processed Data/locations",Output$loc_18,".csv",sep=""))

# source(paste(base.dir,"/Orca_Salmon_Code/_Stan code/Mixed Model/Prep raw data for CPUE analysis.R",sep=""))

COMBINE = "FALSE" # This is a switch for smashing multiple chains together

######################################################################
###### COMBINE OUTPUTS 
######################################################################

if(COMBINE=="TRUE"){
setwd(results.dir)

  # Read in each of the fitted objects.  
  load("FRAM_2023_07 DA1-pois;TROLL_2_param,hake!=pollock,WAPEN=EAPEN(all),cv=hake,pollock,vuln=hake,pollock;4-season(single offshore,wint=avg),year+fing,ZEROS.RData")
  
## EXTRACT PERMUTED VERSION because lists are easier to deal with than arrays.
f1 <- rstan::extract(Output$stanMod,permuted = TRUE)
e1 <- rstan::extract(Output$stanMod,pars=Output$stan_pars, permuted = FALSE)
out.array <- array(dim=c(dim(e1)[1],3,dim(e1)[3]))
out.array[,1,] <- e1
rm(e1); gc()

load("FRAM_2023_07 DA2-pois;TROLL_2_param,hake!=pollock,WAPEN=EAPEN(all),cv=hake,pollock,vuln=hake,pollock;4-season(single offshore,wint=avg),year+fing,ZEROS.RData")
f2 <- rstan::extract(Output$stanMod,permuted = TRUE)
e2 <- rstan::extract(Output$stanMod,pars=Output$stan_pars, permuted = FALSE)
out.array[,2,] <- e2
rm(e2); gc()

load("FRAM_2023_07 DA3-pois;TROLL_2_param,hake!=pollock,WAPEN=EAPEN(all),cv=hake,pollock,vuln=hake,pollock;4-season(single offshore,wint=avg),year+fing,ZEROS.RData")
f3 <- rstan::extract(Output$stanMod,permuted = TRUE)
e3 <- rstan::extract(Output$stanMod,pars=Output$stan_pars, permuted = FALSE)
out.array[,3,] <- e3
rm(e3); gc()

# load("FRAM_v2 DDD DIRI=50 FINAL TWO_OR Salish-temp Mfix PHI-FIX CLIMATE, Mproc error-FOUR LOGIT-vuln Troll_Rec_Treaty_Trawl_SS+PROC_SMOOTH_effort-SLOPE-Q_05-28-2020-24mon-EXTRA F(0.1).RData")
# f4 <- rstan::extract(Output$stanMod,permuted = TRUE)
# e4 <- rstan::extract(Output$stanMod,pars=Output$stan_pars, permuted = FALSE)
# out.array[,4,] <- e4
# rm(e4); gc()
# 
# load("FRAM_v2 EEE DIRI=50 FINAL TWO_OR Salish-temp Mfix PHI-FIX CLIMATE, Mproc error-FOUR LOGIT-vuln Troll_Rec_Treaty_Trawl_SS+PROC_SMOOTH_effort-SLOPE-Q_05-28-2020-24mon-EXTRA F(0.1).RData")
# f5 <- rstan::extract(Output$stanMod,permuted = TRUE)
# e5 <- rstan::extract(Output$stanMod,pars=Output$stan_pars, permuted = FALSE)
# out.array[,5,] <- e5
#rm(e5); gc()
# 
# load("FRAM_v2 FFF FINAL TWO_OR Salish-temp Mfix PHI-FIX CLIMATE, Mproc error-FOUR LOGIT-vuln Troll_Rec_Treaty_Trawl_SS+PROC_SMOOTH_effort-SLOPE-Q_05-15-2020-24mon-EXTRA F(0.1).RData")
# f6 <- rstan::extract(Output$stanMod,permuted = TRUE)
# # e5 <- rstan::extract(Output$stanMod,pars=Output$stan_pars, permuted = FALSE)
# # out.array[,5,] <- e5
# # rm(e5); gc()

stan_pars <- names(f1)
pars <- list()

for(i in 1:length(stan_pars)){
  A <- f1[[stan_pars[i]]]
  B <- f2[[stan_pars[i]]]
  C <- f3[[stan_pars[i]]]
  # D <- f4[[stan_pars[i]]]
  # E <- f5[[stan_pars[i]]]

  temp <- abind(A,B,along=1)
  temp <- abind(temp,C,along=1)
  # temp <- abind(temp,D,along=1)
  # temp <- abind(temp,E,along=1)
  # temp <- abind(temp,F1,along=1)
  
  pars[[stan_pars[i]]] <- temp
}

# rm(f1,f2,f3,f4); gc()
# rm(f1,f2,f3,f4,f5,f6); gc()
# rm(A,B,C,D,E,F1)
# rm(temp); gc()

### Save reordered extracted objects.
#rm(pars); gc()

## Convergence diagnostics
mon <- rstan::monitor(out.array, print = F, warmup = 0)
rownames(mon) <- dimnames(e3)[3]$parameters
mon <- as.data.frame(mon)

Output$pars <- pars
Output$stanMod_summary=mon
NAME <- "08-14-2023 SPR-SUM MODEL"
Output$NAME <- NAME
save(Output,file=paste("__OUTPUT ",NAME,".RData",sep=""))
}

###################################################################################
###################################################################################
###################################################################################
###################################################################################
setwd(results.dir)

#load("FRAM_v2 III DIRI=100 SST locvar2 phi=3, adapt=10 TEST TWO_OR Salish-temp Mfix PHI-FIX CLIMATE, Mproc error-FOUR LOGIT-vuln Troll_Rec_Treaty_Trawl_SS+PROC_SMOOTH_effort-SLOPE-Q_05-28-2020-24mon-EXTRA F(0.1).RData")

if(Output$loc_18=="TRUE"){LOCATIONS <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations_plus.csv",sep=""))}
if(Output$loc_18=="TWO_OR"){LOCATIONS <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations_plus_two_OR.csv",sep=""))}
if(Output$loc_18 =="NCA_SOR_PUSO"){LOCATIONS <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations_plus_NCA_SOR_PUSO.csv",sep=""))}
if(Output$loc_18 =="_two_OR_PUSO_AK"){LOCATIONS <- read.csv(paste(base.dir,"/spring-chinook-distribution/Processed Data/locations",Output$loc_18,".csv",sep=""))}

# Extract the necessary information from the fitted model object.
setwd(code.dir)
source("Basic Data Extraction + PROC SPR-SUM.R")

WRITE_REL <- "TRUE"
if(WRITE_REL=="TRUE"){
  Releases <- list(Output$REL)
  save(Releases,file=paste0(markdown.dir,"/",NAME,"_Releases.RData"))
}

###########################################################################
# smaller plotting scripts
###########################################################################

setwd(code.dir)
# source("multiplot.r")
# source("theme_acs.r")
# source("Pairs SS SPR-SUM.r")
#source("Obs-Pred SS + PROC.r")          # Script for making observed vs. predicted plots
source("Obs-Pred SS SPR-SUM.r")          # Script for making observed vs. predicted plots
#source("Log-likelihood tables.R")        # Script for calculating likelihood components
source("Spatial location SS SPR-SUM.r")  # Script for making proportions in each area.
source("Spatial location plots SS SPR-SUM.r")
source("In-River Recoveries.r")

source("Mat, Vuln plot SS SPR-SUM.r")  
source("Juv_mort SS SPR-SUM.r")          # Script for looking at initial mortality rates among sites.
source("F_mort SS SPR-SUM.r")
source("Proc Error SPR-SUM.R")

#source("Juv_mort to AUG1 by+2 CLIMATE.R")
#source("Adult_mort SS CLIMATE.r")
# source("Spatial location Future Projections SS Climate.r")
# source("Temperature vs Distribution.R")

# source("calculate DIC.R")

# Write early mortality to file
#A<- cbind(REL,log.M.mean=rel_year_all,log.M.sd=rel_year_sd)
#write.csv(A,file="Estimated log_M.csv")


############################################################################
## Generating projections of chinook distributions for a couple of simple cases.
############################################################################
# setwd(code.dir)
#source("Cumulative Chinook Distribution SS.R")
# 
# base_params <- c("tau_process",
#                  #"log_q_rec","log_q_treaty",
#                  "log_q_troll_pos", "log_q_rec_pos","log_q_treaty_pos","log_q_rec_can_pos",#"log_q_rec_PUSO_pos",
#                  "logit_offset",
#                  "sigma_pos",
#                  #"log_rel_year_mu", "rel_year_sigma",
#                  "beta_vuln",#"vuln_int",
#                  #"log_M2",
#                  "log_F_rec_mean","F_rec_sigma") #"prob_age_year") #"beta_age_month""vuln_int"
# 
# summary(mod,pars=base_params)$summary
# 
# c( "lp__","log_q_troll_pos", "log_q_treaty_pos", "log_q_rec_pos", "log_q_rec_can_pos",
#    "logit_offset", "log_F_rec_mean", "F_rec_sigma","log_rel_year_mu","rel_year_sigma",
#     "vuln_int", "beta_vuln", "sigma_pos","log_M2","alpha_pay","beta_pay")

