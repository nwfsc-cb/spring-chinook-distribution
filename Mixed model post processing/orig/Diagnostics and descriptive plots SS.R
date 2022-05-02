#rm(list=ls())
library(ggplot2)
library(reshape2)
library(dplyr)
library(rstan)
library(gtools)
library(MASS)
library(fields)
library(RColorBrewer)
library(viridis)
library(extrafont)
### Make diagnostic and exploratory plots for model objects

base.dir    <- "/Users/ole.shelton/GitHub"
results.dir <- "/Users/ole.shelton/GitHub/Orca_Salmon/Output files/_Mixed Results"
code.dir    <- "/Users/ole.shelton/GitHub/Orca_Salmon_Code/Mixed model post processing"

setwd(results.dir)
load("Binomial+Positive_output-1978-90 Troll_Rec_Treaty_6year_STATE_SPACE+PROC_E100_M2FIXED_originP0_vulnfix_05-08-2017.RData")
LOCATIONS <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/locations.csv",sep=""))
# source(paste(base.dir,"/Orca_Salmon_Code/_Stan code/Mixed Model/Prep raw data for CPUE analysis.R",sep=""))

# Extract the necessary information from the fitted model object.
setwd(code.dir)
source("Basic Data Extraction +PROC.R")
###########################################################################
# smaller plotting scripts
###########################################################################

setwd(code.dir)
source("multiplot.r")
source("theme_acs.r")
source("Pairs SS.r")
#source("Obs-Pred SS + PROC.r")          # Script for making observed vs. predicted plots
source("Obs-Pred SS.r")          # Script for making observed vs. predicted plots
source("Spatial location SS.r")  # Script for making proportions in each area.
source("Mat, Vuln plot SS.r") 
source("Juv_mort SS.r")          # Script for looking at initial mortality rates among sites.
source("Juv_mort to AUG1 by+2.R")
source("Adult_mort SS.r")
source("F_mort SS.r")
source("Proc Error.R")
#source("Residual plot.r")

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
