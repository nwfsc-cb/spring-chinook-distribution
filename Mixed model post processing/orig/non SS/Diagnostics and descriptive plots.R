#rm(list=ls())
library(rstan)
### Make diagnostic and exploratory plots for model objects

base.dir    <- "/Users/ole.shelton/GitHub"
results.dir <- "/Users/ole.shelton/GitHub/Orca_Salmon/Output files/_Mixed Results"
code.dir    <- "/Users/ole.shelton/GitHub/Orca_Salmon_Code/Mixed model post processing"

setwd(results.dir)
#load("Binomial+Positive_output-1978-90 Troll_Rec_Treaty_E200_6year_vuln_int_seasonal_no1_WAC.RData")
LOCATIONS <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/locations.csv",sep=""))
#source(paste(base.dir,"/Orca_Salmon_Code/_Stan code/Mixed Model/Prep raw data for CPUE analysis.R",sep=""))

#NAME <- "Troll_Rec_Treaty_vuln_int"
# Interest in (Output)

mod  <- Output$stanMod
samp <- Output$pars
dat.bin <- Output$raw.dat.bin
dat.pos <- Output$raw.dat.pos
#cum_M2  <- Output$cum_M2
spawn_loc <- Output$spawn_loc
TREATY <- Output$TREATY
SEASON        <- Output$SEASON
NAME          <- Output$NAME
REL           <- Output$REL
age_month_cal <- Output$age_month_cal
PRIORS        <- PRIORS

# Calculate summaries of various parameters
temp <-apply(samp$prob_age_year,c(2,3),median)
sum_prob <- apply(samp$sum_prob,c(2,3),median)

if(SEASON ==FALSE){  origin_loc   <- apply(samp$origin_loc,c(2,3),median)}
if(SEASON ==TRUE) {  origin_loc   <- apply(samp$origin_loc,c(2,3,4),median)}
rel_year_all    <- apply(samp$rel_year_all,c(2),mean)
beta_vuln       <- apply(samp$beta_vuln,2,mean)
vuln_mat        <- apply(samp$vuln_mat,c(2,3),mean)
cum_M2          <- apply(samp$cum_M2,2,mean)
prob_age_year   <- data.frame(rep(0,dim(samp$prob_age_year)[2]),apply(samp$prob_age_year,c(2,3),mean))

###########################################################################
# smaller plotting scripts
###########################################################################

setwd(code.dir)
source("multiplot.r")
source("theme_acs.r")
source("Obs-Pred.r")          # Script for making observed vs. predicted plots
source("Spatial location.r")  # Script for making proportions in each area.
source("Juv_mort.r")          # Script for looking at initial mortality rates among sites.
source("Mat, Vuln plot.r") 
#source("Adult_mort.r")
#source("Residual plot.r")

############################################################################
## Generating projections of chinook distributions.
############################################################################
setwd(code.dir)
source("Cumulative Chinook Distribution.R")