# Parse Survivorship and Mortality for Kiva and Dan

rm(list=ls())
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
library(Hmisc)

### Make diagnostic and exploratory plots for model objects




base.dir    <- "/Users/ole.shelton/GitHub"
results.dir <- "/Users/ole.shelton/GitHub/Salmon-Climate/Output files/_Fit objects"
code.dir    <- "/Users/ole.shelton/GitHub/Salmon-Climate/Mixed model post processing"

setwd(results.dir)
load("CA+COL CLIMATE Troll_Rec_Treaty_SS+PROC_E50_M2FIX_SMOOTH_effort-SLOPE-Q_vulnfix_03-06-2019-OBS-FRAC two sigma-24mon-zeroPUSO-EXTRA F(0.2) FIT-AND-DATA.RData")
#load("CA+COL CLIMATE Troll_Rec_Treaty_SS+PROC_E100_M2EST_vulnfix_07-26-2017.RData")
LOCATIONS <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations.csv",sep=""))
# source(paste(base.dir,"/Orca_Salmon_Code/_Stan code/Mixed Model/Prep raw data for CPUE analysis.R",sep=""))
if(Output$loc_18=="TRUE"){LOCATIONS <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations_plus.csv",sep=""))}
if(Output$loc_18=="TWO_OR"){LOCATIONS <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations_plus_two_OR.csv",sep=""))}

# Extract the necessary information from the fitted model object.
setwd(code.dir)
source("Basic Data Extraction + PROC CLIMATE.R")

# Map early survival to 
RYA <- samp$rel_year_all %>% t(.) %>% cbind(Output$REL %>% dplyr::select(ID,ocean.region,brood_year,release_year),.)
RYA <- melt(RYA,id.vars=c("ID","ocean.region","brood_year","release_year"),variable.name="iter")

RYA.summary.M <- RYA %>% group_by(ocean.region,release_year,iter) %>% dplyr::summarise(Mean= mean(value),SD=sd(value),N=length(value)) %>%
                  group_by(ocean.region,release_year,N) %>% 
                  dplyr::summarise(simple.mean = mean(Mean), simple.se = sd(Mean), 
                            w.mean = Hmisc::wtd.mean(Mean, w= SD^(-2)),
                            #SE.w.mean2= sd((Mean*SD^(-2))/ (SD^(-2))),
                            SE.w.mean = sqrt(Hmisc::wtd.var(Mean, w= SD^(-2))))
                 
RYA.summary.surv <- RYA %>% mutate(value = exp(-value)) %>% group_by(ocean.region,release_year,iter) %>% 
                  dplyr::summarize(Mean= mean(value),SD=sd(value),N=length(value)) %>%
                  group_by(ocean.region,release_year) %>% 
                  dplyr::summarise(simple.mean = mean(Mean), simple.se = sd(Mean), 
                            w.mean = Hmisc::wtd.mean(Mean, SD^(-2)),
                            #SE.w.mean2= sd((Mean*SD^(-2))/ (SD^(-2))),
                            SE.w.mean = sqrt(Hmisc::wtd.var(Mean, SD^(-2))))

RYA.summary.M <-  RYA.summary.M %>% filter(ocean.region %in% c("SFB","NCA","LCOL","MCOL","UCOL","SNAK"))
RYA.summary.surv <-  RYA.summary.surv%>% filter(ocean.region %in% c("SFB","NCA","LCOL","MCOL","UCOL","SNAK"))

setwd("/Users/ole.shelton/GitHub/Salmon-Climate/Output files")
dat <- list(Chinook_M_series= RYA.summary.M,
            Chinook_surv_series = RYA.summary.surv)
save(dat,file="Fall_Chinook_M_and_Surv_Estimates.RData")




ggplot(RYA.summary.surv) +
    geom_point(aes(y=w.mean,x=release_year),alpha=0.3) +
    geom_point(aes(y=simple.mean,x=release_year),color="red",alpha=0.3) +
    geom_line(aes(y=w.mean,x=release_year),alpha=0.3) +
    geom_line(aes(y=simple.mean,x=release_year),color="red",alpha=0.3) +
    facet_wrap(~ocean.region)
  
ggplot(RYA.summary.M) +
  geom_point(aes(y=w.mean,x=release_year),alpha=0.3) +
  geom_point(aes(y=simple.mean,x=release_year),color="red",alpha=0.3) +
  geom_line(aes(y=w.mean,x=release_year),alpha=0.3) +
  geom_line(aes(y=simple.mean,x=release_year),color="red",alpha=0.3) +
  facet_wrap(~ocean.region)

  
  