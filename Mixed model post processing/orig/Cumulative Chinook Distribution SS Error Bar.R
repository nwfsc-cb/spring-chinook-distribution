rm(list=ls())
#### # MAKING SALMON DISTRIBUTION SURFACES
library(ggplot2)
library(reshape2)
library(dplyr)

# READ IN POSTERIOR FILE FROM MODEL FIT OF INTEREST:
base.dir    <- "/Users/ole.shelton/GitHub"
results.dir <- "/Users/ole.shelton/GitHub/Orca_Salmon/Output files/_Mixed Results"
code.dir    <- "/Users/ole.shelton/GitHub/Orca_Salmon_Code/Mixed model post processing"

setwd(results.dir)
load("Binomial+Positive_output-1978-90 Troll_Rec_Treaty_6year_STATE_SPACE+PROC_E100_M2FIXED_originP0_vulnfix_05-08-2017.RData")
LOCATIONS <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/locations.csv",sep=""))

setwd(code.dir)
source("multiplot.R")
source("Basic Data Extraction +PROC.R")
source("Juv_mort SS.R")
source("Proc Error.R")
  # important objects from Juv_mort SS.r are : juv.dat.sum 
  # important object from Proc Error.R is epsilon.dat

  # Calculate average deviation by origin region
  epsilon.region <- data.frame(summarise(group_by(epsilon.dat,ocean.region,model.time),
                                       MEAN=mean(value),MEDIAN=median(value),SD=sd(value),n=length(value)))

## Simpson diversity function
inv.simp.div <- function(prop){
  return(1 / sum(prop^2))
}

gini.simp.div <- function(prop){
  return(1 - sum(prop^2))
}
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
# Calculate number of outmigrant smolt from various regions....
smolt.dat <- read.csv("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Total Smolt and Run Info/smolt_data_01-2017.csv")

# smolt.mod <- smolt.dat[smolt.dat$type == "independent",]
# smolt.mod$total.wild.hatch <- smolt.mod$total.releases.median * (1+smolt.mod$frac.wild.missing) 
# smolt.mod$finger <- smolt.mod$total.wild.hatch * (1 - smolt.mod$frac.yearling )
# smolt.mod$yearling <- smolt.mod$total.wild.hatch * smolt.mod$frac.yearling
# smolt.prod <- aggregate(smolt.mod[,c("finger","yearling")],
#                         by=list(number=smolt.mod$location.number,region=smolt.mod$rel.region,
#                                 n.mon.fing = smolt.mod$n.month.finger ,n.mon.year = smolt.mod$n.month.yearling 
#                         ),sum)

smolt.dat$total <- smolt.dat$hatchery + smolt.dat$wild
smolt.prod <- summarize(group_by(smolt.dat,region,number),Sum=sum(total))
smolt.prod <- smolt.prod[order(smolt.prod$number),]

#colnames(smolt.prod)[which(names(smolt.prod)=="location.number")] ="number"
#############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
#############################################################################################
### PROJECT SPATIAL DISTRIBUTION FOR EACH ORIGIN

#### DATA FITS
# mod  <- Output$stanMod
# samp <- Output$pars
# dat.bin <- Output$raw.dat.bin
# dat.pos <- Output$raw.dat.pos
# cum_M2  <- Output$cum_M2
# spawn_loc <- Output$spawn_loc

###### Calculate Initial Monthly mortality rate
# rel_yr <- data.frame(rel_id = 1:length(unique(dat.bin$release_year)),rel_year = sort(unique(dat.bin$release_year)))
# 
# nom.all <- sort(unique(dat.bin$year.reg))
# THESE   <- match(nom.all,dat.bin$year.reg)
# nom     <- data.frame( dat.bin[THESE,c("year.reg.idx","ocean.reg","release_year","loc.spawn.idx")],nom = nom.all)
# 
# #nom$ocean.reg[nom$ocean.reg == "NWVI" | nom$ocean.reg == "SWVI"] <- "VI"
# #nom$ocean.reg[nom$ocean.reg == "SOR" | nom$ocean.reg == "COR" |nom$ocean.reg == "NOR"] <- "OR"
# nom <- nom[order(nom$loc.spawn.idx),]
# 
# dat.start <- matrix(-99,length(unique(spawn_loc$init.loc)),nrow(rel_yr))
# colnames(dat.start) <- rel_yr$rel_year
# rownames(dat.start) <- unique(nom$ocean.reg)
# dat.mort.mean <- dat.start
# dat.mort.sd   <- dat.start
# 
# for(i in 1:max(dat.bin$year.reg.idx)){
#   X <- which(rownames(dat.start)==nom$ocean.reg[i]) 
#   Y <- which(colnames(dat.start)==nom$release_year[i]) 
#   these.col <- which( REL$ocean.region == nom$ocean.reg[i] & REL$release_year == nom$release_year[i])
#   if(length(these.col)>1){
#       dat.mort.mean[X,Y] <- mean(apply(samp$rel_year_all[,c(these.col)],1,median))
#       dat.mort.sd[X,Y]   <- sd(rowMeans(samp$rel_year_all[,c(these.col)]))
#   }
#   if(length(these.col)==1){
#       dat.mort.mean[X,Y] <- mean(samp$rel_year_all[,c(these.col)])
#       dat.mort.sd[X,Y]   <- sd(samp$rel_year_all[,c(these.col)])
#   }
# }
# 
# juv.mort <- NULL
# for(i in 1:nrow(dat.mort.mean)){
#   temp    <- dat.mort.mean[i,]
#   juv.mort <- rbind(juv.mort,c(rownames(dat.mort.mean)[i],mean(temp[temp>0])))
# }
# juv.mort <- data.frame(juv.mort)
# colnames(juv.mort) <- c("region","juv.mort")
# juv.mort$juv.mort <- as.numeric(as.character(unlist(juv.mort$juv.mort)))
# juv.mort$loc.spawn.idx <- 1:nrow(juv.mort)

#use raw median
juv.mort.shared <- median(rel_year_all)
#use weighted mean from Juv mort SS.r
juv.mort.shared <-  mean(exp(juv.dat.sum$w.mean))
#############################################################################################

sim.region <- aggregate(dat.bin$ocean.reg,
        by=list(region = dat.bin$ocean.reg,
            origin.idx = dat.bin$origin.idx,
            loc.spawn.idx = dat.bin$loc.spawn.idx
            #season.idx = dat.bin$season.idx
            ),length)[,1:3]

#sim.region <- merge(sim.region,juv.mort[,c("loc.spawn.idx","juv.mort")],all=T,by="loc.spawn.idx")

sim.region <- merge(sim.region,smolt.prod)
sim.region <- sim.region[order(sim.region$number),]
#sim.region$juv.mort.shared <- mean(exp(samp$log_rel_year_mu))

N.mod.int     <- max(dat.bin$time)
LOC           <- sort(unique(dat.bin$location))
AGE_MONTH_IDX <- 1:N.mod.int 
SEASON_NAME   <- c(c("spring","summer","fall"),rep(c("winter","spring","summer","fall"),4))
SEASON_IDX    <- c(c(1,2,3),rep(c(1,1,2,3),4))

sim.dat       <-  expand.grid(age.month=AGE_MONTH_IDX,loc=LOC)
sim.dat$season.idx <- SEASON_IDX
sim.dat$season.name <-SEASON_NAME
sim.dat$age.year <- 0
sim.dat$age.year[sim.dat$age.month<=3]<-1
sim.dat$age.year[sim.dat$age.month >3 & sim.dat$age.month <= 7 ]  <- 2
sim.dat$age.year[sim.dat$age.month >7 & sim.dat$age.month <= 11 ] <- 3
sim.dat$age.year[sim.dat$age.month >11 & sim.dat$age.month <= 15 ]<- 4
sim.dat$age.year[sim.dat$age.month >15 & sim.dat$age.month <= 19 ]<- 5

OUT <- sim.dat

smolt.prod$origin.idx <- COV$origin.idx[match(smolt.prod$region,COV$ocean.region)]
smolt.prod$loc.spawn.idx <- COV$loc.spawn.idx[match(smolt.prod$region,COV$ocean.region)]

log_N_pred <- matrix(0,N_time_mod,N_loc)
D_pred     <- matrix(0,nrow(smolt.prod),N_year)
river_entry_proj <- river_entry[match(smolt.prod$region,REL$ocean.region),]

F_scen <- c("F_zero","F_median","F_max")
F_scen <- c("F_median")
############## PROJECTIONS ( USING 3 Fishing Mortality )


ALL.LONG <- NULL
# CONTROL HOW MANY SAMPLES TO TAKE FROM THE POSTERIOR
THESE <-seq(1,length(samp$log_q_troll_pos),length.out=1000)

for(k in 1:length(F_scen)){
  if(F_scen[k]=="F_zero"){F_t_temp  <- F_troll_zero_mat; F_r_temp   <- F_rec_zero_mat}
  if(F_scen[k]=="F_median"){F_t_temp  <- F_troll_median_mat; F_r_temp <- F_rec_median_mat}
  if(F_scen[k]=="F_max"){F_t_temp  <- F_troll_max_mat; F_r_temp    <- F_rec_max_mat}
  F_TROLL_MAT <- NULL
  F_REC_MAT <- NULL
    for(kk in 1:5){
        F_TROLL_MAT <- rbind(F_TROLL_MAT,F_t_temp)
        F_REC_MAT <- rbind(F_REC_MAT,F_r_temp)
    }
  F_TROLL_MAT <- F_TROLL_MAT[1:N_time_mod,]  
  F_REC_MAT   <- F_REC_MAT[1:N_time_mod,]

  for(kkk in 1:ncol(F_TROLL_MAT)){
      F_TROLL_MAT[,kkk] <- F_TROLL_MAT[,kkk] * vuln_mat[vuln_int_idx[kkk],]
      F_REC_MAT[,kkk]   <- F_REC_MAT[,kkk]   * vuln_mat[vuln_int_rec_idx[kkk],]
  }

  F_TOT_MAT <- F_TROLL_MAT + F_REC_MAT

  ##################################################################
  for(i in 1:nrow(smolt.prod)){  # Cycle Across release groups.
    print(paste(i,"of",nrow(smolt.prod)))
    ALL.SIM <- NULL
    ALL.SIM.end <- NULL
    for(L in 1:length(THESE)){ # cycle across samples from the posterior.
        O_L <- samp$origin_loc[THESE[L],,,]
        P_A_Y <- samp$prob_age_year[THESE[L],,]
    
      log_N_pred  <- log_N_pred  * 0
      log_N_begin <- log_N_pred
      for(j in 1:N_time_mod){ # Cycle Across model time..
        if(j == 1){
          log_N_pred[j,] <-  log(smolt.prod$Sum[i]*1e6  ) - juv.mort.shared - AGE$M2_est[j] +
                                log(O_L[AGE$season_idx[j],smolt.prod$origin.idx[i],]) 
          log_N_begin[j,] <-  log(smolt.prod$Sum[i]*1e6 ) - juv.mort.shared +
                                  log(O_L[AGE$season_idx[j],smolt.prod$origin.idx[i],]) 
        }
        if(j>1){
          if(spawn_time[j] == 0){
            log_N_pred[j,]   <-       log(sum(exp(log_N_pred[j-1,]))) + 
                                      epsilon.region$MEAN[epsilon.region$ocean.region == smolt.prod$region[i] & 
                                                 epsilon.region$model.time == j-1] +  
                                      log(O_L[AGE$season_idx[j],smolt.prod$origin.idx[i],]) -
                                      AGE$M2_est[j]   -
                                      F_TOT_MAT[j,]
            log_N_begin[j,]   <-       log(sum(exp(log_N_pred[j-1,]))) +  
                                      epsilon.region$MEAN[epsilon.region$ocean.region == smolt.prod$region[i] & 
                                                epsilon.region$model.time == j-1]  +
                                      log(O_L[AGE$season_idx[j],smolt.prod$origin.idx[i],])
          }
          if(AGE$season_idx[j]==3){
            log_N_temp     <-       log(sum(exp(log_N_pred[j-1,]))) +  
                                    epsilon.region$MEAN[epsilon.region$ocean.region == smolt.prod$region[i] & 
                                        epsilon.region$model.time == j-1] +
                                    log(O_L[AGE$season_idx[j],smolt.prod$origin.idx[i],]) -
                                    AGE$M2_est[j] * 0.5   -
                                    F_TOT_MAT[j,] * 0.5 ;
            log_N_begin[j,]   <-    log(sum(exp(log_N_pred[j-1,]))) + 
                                    epsilon.region$MEAN[epsilon.region$ocean.region == smolt.prod$region[i] & 
                                        epsilon.region$model.time == j-1] +
                                    log(O_L[AGE$season_idx[j],smolt.prod$origin.idx[i],])
            D_pred[i,AGE$spawn_time[j]] <- sum(exp((log_N_temp +  log(P_A_Y[smolt.prod$loc.spawn.idx[i], AGE$spawn_time[j]])) * river_entry_proj[i,])) ;
            log_N_temp     <-   log_N_temp +
                                      log(1- P_A_Y[smolt.prod$loc.spawn.idx[i], AGE$spawn_time[j]]* river_entry_proj[i,]) -
                                      AGE$M2_est[j] * 0.5   -
                                      F_TOT_MAT[j,] * 0.5  ;
            log_N_pred[j,] <-  log_N_temp ;
          }
        }
      }

  A_temp <- data.frame(ocean.region= smolt.prod$region[i],   
                          F_scen =F_scen[k],
                          time=AGE$time,
                          age.month=AGE$age,
                          age.year=AGE$year,
                          season=rownames(F_TOT_MAT),
                          juv.mort=juv.mort.shared,
                          exp(log_N_begin))
  ALL.SIM <- rbind(ALL.SIM,A_temp)
  
  B_temp <- data.frame(ocean.region= smolt.prod$region[i],   
                       F_scen =F_scen[k],
                       time=AGE$time,
                       age.month=AGE$age,
                       age.year=AGE$year,
                       season=rownames(F_TOT_MAT),
                       juv.mort=juv.mort.shared,
                       exp(log_N_pred))
  ALL.SIM.end <- rbind(ALL.SIM.end,B_temp)
    } # End posterior sample loop
  
    # Create object which is sum of fish across all origin locations.
      if(i == 1){
      ALL.SIM.sum <- ALL.SIM
      ALL.SIM.end.sum <- ALL.SIM
    }
    if(i > 1){
      these.col <- grep("X",colnames(ALL.SIM.sum))
      ALL.SIM.sum[these.col] <- ALL.SIM.sum[,these.col] + ALL.SIM[,these.col]
      ALL.SIM.sum$ocean.region <- "TOTAL"
      ALL.SIM.end.sum[these.col] <- ALL.SIM.end.sum[,these.col] + ALL.SIM.end[,these.col]
      ALL.SIM.end.sum$ocean.region <- "TOTAL"
    }
  
    # Summarize simulations from given release location  
    THESE.COL <- grep("X",colnames(ALL.SIM))
    colnames(ALL.SIM)[THESE.COL] <- as.character(LOCATIONS$location.name)
    ALL.LONG.temp <- melt(ALL.SIM,id.vars = c("ocean.region","F_scen","time","age.month","age.year","season","juv.mort"))
    colnames(ALL.LONG.temp)[colnames(ALL.LONG.temp)=="variable"] <- "loc"

    TEMP <- summarise(group_by(ALL.LONG.temp,ocean.region,F_scen,time,age.month,age.year,season,loc),MEAN = mean(value),MEDIAN=median(value),SD =sd(value),
              q.025 = quantile(value,probs=0.025),q.05 = quantile(value,probs=0.05), q.25 = quantile(value,probs=0.25),
              q.75 = quantile(value,probs=0.75),q.95 = quantile(value,probs=0.95), q.975 = quantile(value,probs=0.975)
              )
    #### FIX THIS TO PROPERLY PROPAGATE ERROR ACROSS AGES TO GET TO THE TOTAL VARIABILITY IN AGE a+ categories.
    
    
    
    
    
    
    
    
    ALL.LONG <-rbind(ALL.LONG,TEMP)    
    }  # End Release location loop  
} # End Fishing mortality loop

THESE.COL <- grep("X",colnames(ALL.SIM.sum))
colnames(ALL.SIM.sum)[THESE.COL] <- as.character(LOCATIONS$location.name)

ALL.LONG.sum <- melt(ALL.SIM.sum,id.vars = c("ocean.region","F_scen","time","age.month","age.year","season","juv.mort"))
colnames(ALL.LONG.sum)[colnames(ALL.LONG.sum)=="variable"] <- "loc"

ALL.LONG.sum <- summarise(group_by(ALL.LONG.sum,ocean.region,F_scen,time,age.month,age.year,season,loc),MEAN = mean(value),MEDIAN=median(value),SD =sd(value),
                  q.025 = quantile(value,probs=0.025),q.05 = quantile(value,probs=0.05), q.25 = quantile(value,probs=0.25),
                  q.75 = quantile(value,probs=0.75),q.95 = quantile(value,probs=0.95), q.975 = quantile(value,probs=0.975)
        )

# ALL.LONG$value <- ALL.LONG$value
# 
# OUT.TOT    <- ALL.LONG[ALL.LONG$loc=="TOTAL",]
OUT.LONG        <- ALL.LONG
OUT.LONG.sum    <- ALL.LONG.sum

##############################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
##### PLOT FOR PUB ( stacked PROPORTIONAL CONTRIBUTION)
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

region.area <- read.csv(file="/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Region Area estimates/region_area_blake_7_2016.csv")
region.area <- merge(region.area,LOCATIONS)

##################################################################################################################
##################################################################################################################
# 
# summer <- OUT[OUT$season.name =="summer",]
# fall   <- OUT[OUT$season.name =="fall",]
# winter <- OUT[OUT$season.name =="winter",]
# spring <- OUT[OUT$season.name =="spring",]

#### MAKE CUMULATIVE DISTRIBUTION FOR ALL AREAS BASED ON MEAN SURVIVAL FOR EACH REGION, MEAN DISTRIBUTION, MEAN EVERYTHING.
age <- c(2,3,4,5)
for(QQQ in 1:length(age)){
  for(WWW in 1:length(F_scen)){  # CHOOSE AN AGE

  F_scen_name <- F_scen[WWW] # options are" "F_zero"   "F_median" "F_max"
  AGE_temp <- age[QQQ]
#####################
  age.name <- AGE_temp 
  temp <- OUT.LONG.sum[OUT.LONG.sum$age.year >= AGE_temp,]
  OUT.OLD <- summarise(group_by(temp,loc,season,F_scen),chin = sum(MEAN),chin.SD = sqrt(sum(SD^2)))
  OUT.OLD <- data.frame(OUT.OLD)
  OUT.OLD$chin    <- OUT.OLD$chin/1000
  OUT.OLD$chin.SD <- OUT.OLD$chin.SD/1000
  
  temp <- OUT.LONG[OUT.LONG$age.year >= AGE_temp,]
  OUT.OLD.REG <- summarise(group_by(temp,ocean.region,loc,season,F_scen),chin = sum(MEAN),chin.SD = sqrt(sum(SD^2)))
  OUT.OLD.REG <- data.frame(OUT.OLD.REG)
  
  OUT.OLD.DENS<- merge(OUT.OLD.REG,region.area,by.x="loc",by.y="location.name")
  OUT.OLD.DENS$dens <- OUT.OLD.DENS$chin / OUT.OLD.DENS$km2.10.200m

  ### Subset out the relevant Fishing scenario.
  OUT.OLD.REG <- OUT.OLD.REG[OUT.OLD.REG$F_scen == F_scen_name,]
  OUT.OLD.DENS <- OUT.OLD.DENS[OUT.OLD.DENS$F_scen == F_scen_name,]

  ### Calculate Evenness for each region and each season
  # total.by.reg <- summarise(group_by(OUT.OLD.REG,loc,season),TOT=, )
  #   
  #   
  #   
  #   aggregate(OUT.OLD.REG$chin,by=list(loc=OUT.OLD.REG$loc,season=OUT.OLD.REG$season),sum)
  # colnames(total.by.reg)[3] <- "TOT"
  # total.by.reg <- merge(total.by.reg,region.area,by.x="loc",by.y="location.name")
  # total.by.reg$DENS <- total.by.reg$TOT / total.by.reg$km2.10.200m
  # total.by.reg$TOT <- total.by.reg$TOT /1000
  # total.by.reg$loc.name <- total.by.reg$loc
  # total.by.reg$loc         <- LOCATIONS$location.number[match(total.by.reg$loc.name,LOCATIONS$location.name)]

# OUT.EVEN <- merge(OUT.OLD.REG,total.by.reg)
# OUT.EVEN$prop <- OUT.EVEN$chin / OUT.EVEN$TOT
# even.plot <- aggregate(OUT.EVEN$prop,by=list(loc=OUT.EVEN$loc,season=OUT.EVEN$season),gini.simp.div)
# colnames(even.plot)[3] <- "gini.simp"

  OUT.OLD.REG$chin     <- OUT.OLD.REG$chin / 1000
  OUT.OLD.REG$loc.name <- OUT.OLD.REG$loc
  OUT.OLD.REG$loc         <- LOCATIONS$location.number[match(OUT.OLD.REG$loc.name,LOCATIONS$location.name)]
  OUT.OLD.REG$origin.numb <- spawn_loc$number[match(OUT.OLD.REG$ocean.region,spawn_loc$ocean.region)]

  OUT.OLD.DENS$loc.name <- OUT.OLD.DENS$loc
  OUT.OLD.DENS$loc <- LOCATIONS$location.number[match(OUT.OLD.DENS$loc.name,LOCATIONS$location.name)]
  OUT.OLD.DENS$origin.numb <- spawn_loc$number[match(OUT.OLD.DENS$ocean.region,spawn_loc$ocean.region)]

  OUT.OLD.REG <- OUT.OLD.REG[order(OUT.OLD.REG$origin.numb),]
  OUT.OLD.DENS <- OUT.OLD.DENS[order(OUT.OLD.DENS$origin.numb),]

# Proportional contribution
q1 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="Spr",],  aes(x = loc, y = chin, fill = ocean.region)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous() +
  labs(x = "", y = paste("Proportion",sep="")) +
  scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) +
  ggtitle("Spring Distribution") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
  scale_fill_discrete(name="Origin")
q1

# COLS <- c("SFB"="#d7191c",
#           "NCA"="#fdae61",
#           "SOR"="#ffffbf",
#           "COR"="#abd9e9",
#           "NOR"="#2c7bb6",
#           "COL"  ="#d7191c",
#           "UPCOL"="#fdae61",
#           "WAC"  = "#ffffbf",
#           "PUSO" ="#abd9e9",
#           "SGEO" ="#2c7bb6",
#           "SWVI" = "#d7191c")


COLS <- c("SFB"= "#DAA520", #"#ffffcc",
          "NCA"="#B8860B",
          "SOR"="#3CB371",
          "COR"="#228B22",
          "NOR"="#006400",
          "COL"  ="#00BFFF",
          "MCOL"  = "#1E90FF",
          "UPCOL"= "#0000FF",
          "WAC"  = "black",
          "PUSO" ="#FF6347",
          "SGEO" ="#DC143C",
          "SWVI" = "#A52A2A")

OUT.OLD.REG$ocean.region <-  factor(OUT.OLD.REG$ocean.region, 
                              levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                         "NOR","COR","SOR","NCA","SFB"))
OUT.OLD.DENS$ocean.region <-  factor(OUT.OLD.DENS$ocean.region, 
                               levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                          "NOR","COR","SOR","NCA","SFB"))

# p + scale_colour_manual(values = COLS)
# scale_fill_brewer(palette = 12)

# Proportional contribution
q1 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="Spr",],  aes(x = loc, y = chin, fill = ocean.region )) + 
  geom_bar(position = "fill",stat = "identity") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = " ", y = "",title="a) Spring") +
  scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
  scale_fill_manual(values=COLS,name="Origin") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
        plot.margin=unit(c(0.1, 0.05, 0.05, 0.38), "lines")) 
q1
q2 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = ocean.region )) + 
  geom_bar(position = "fill",stat = "identity") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Ocean region", y = "",title="b) Summer") +
  scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
  scale_fill_manual(values=COLS,name="Origin") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
        plot.margin=unit(c(0.1, 0.05, 0.05, 0.2), "lines")) 
q2
q3 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="Fall",],  aes(x = loc, y = chin, fill = ocean.region )) + 
  geom_bar(position = "fill",stat = "identity") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = " ", y = paste("Proportion",sep=""),title="c) Fall") +
  scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
  scale_fill_manual(values=COLS,name="Origin") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
        plot.margin=unit(c(0.1, 0.05, 0.05, 0.38), "lines")) 

q4 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="Wint",],  aes(x = loc, y = chin, fill = region )) + 
  geom_bar(position = "fill",stat = "identity") + 
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = " ", y = paste("Proportion",sep=""),title="c) Winter") +
  scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
  scale_fill_manual(values=COLS,name="Origin") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
        plot.margin=unit(c(0.1, 0.05, 0.05, 0.2), "lines")) 

if(age.name==2){LIM=c(0,7000)}
if(age.name==3){LIM=c(0,2200)}
if(age.name==4){LIM=c(0,1000)}
p1 <- ggplot(data=OUT.OLD[OUT.OLD$season == "Spr",]) + 
  geom_bar(aes(x = loc, y = chin ),fill=grey(0.5),stat = "identity") +
  coord_flip() +
  labs(x = "",y="",title="e)") + 
  xlab(NULL) +
  scale_x_discrete(expand=c(0,0),breaks = 1:nrow(LOCATIONS), labels= rep("",nrow(LOCATIONS))) + 
  scale_y_continuous(expand=c(0,0),limits=LIM) +
  scale_fill_manual(values=COLS,name="Origin") +
  theme_bw() +
  geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),
        plot.title = element_text(hjust = 0,color="white",size=rel(0.9)),legend.position="none",
        panel.border=element_rect(colour="black",size=1.5),
        plot.margin=unit(c(0.1, 4.62, 0.05, 0.3), "lines")) 
p1

p2 <- ggplot() + 
  geom_bar(data=OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = ocean.region ),stat = "identity") +
  geom_bar(data=OUT.OLD[OUT.OLD$season == "Sum",],  aes(x = loc, y = chin ),fill=grey(0.5),stat = "identity") +
  geom_errorbar(data=OUT.OLD[OUT.OLD$season == "Sum",],aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
  coord_flip() +
  labs(x = "",y="",title="e)") + 
  xlab(NULL) +#, y = paste("Chinook, age ",age.name,"+ (thousands)",sep="")) +
  scale_x_discrete(expand=c(0.0,0),breaks = 1:nrow(LOCATIONS), labels= rep("",nrow(LOCATIONS))) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
  scale_y_continuous(expand=c(0,0),limits=LIM) +
  scale_fill_manual(values=COLS,name="Origin") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,color="white",size=rel(0.9)),
        panel.border=element_rect(colour="black",size=1.5),
        plot.margin=unit(c(0.1, 0.0, 0.05, 0.3), "lines"),
        legend.key.size = unit(0.4, "cm")) 
p2  

p3 <- ggplot(OUT.OLD[OUT.OLD$season == "Fall",]) + 
  geom_bar(aes(x = loc, y = chin ),fill=grey(0.5),stat="identity") +
  geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
  coord_flip() +
  labs(x = "", y = paste("Chinook, age ",age.name,"+ (1000s)",sep=""),title="f)") +
  xlab(NULL) +
  scale_x_discrete(expand=c(0.0,0),breaks = 1:nrow(LOCATIONS), labels= rep("",nrow(LOCATIONS))) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
  scale_y_continuous(expand=c(0,0),limits=LIM) +
  scale_fill_manual(values=COLS,name="Origin") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),legend.position="none",
        panel.border=element_rect(colour="black",size=1.5),
        plot.margin=unit(c(0.1, 4.62, 0.05, 0.3), "lines")) 
p3
######


# p4 <- ggplot() + 
#   #geom_area(data=OUT.OLD.REG[OUT.OLD.REG$season=="fall",],  aes(x = loc, y = chin, fill = region )) +
#   geom_bar(data=total.by.reg[total.by.reg$season == "Wint",],  aes(x = loc, y = TOT ),fill=grey(0.5),stat="identity") +
#   #geom_line(data=total.by.reg[total.by.reg$season == "fall",],  aes(x = loc, y = TOT ),color= "black") +
#   coord_flip() +
#   labs(x = "", y = paste("Chinook, age ",age.name,"+ (1000s)",sep=""),title="f)") +
#   xlab(NULL) +
#   scale_x_continuous(expand=c(0.0,0),breaks = 1:nrow(LOCATIONS), labels= rep("",nrow(LOCATIONS))) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
#   scale_y_continuous(expand=c(0,0),limits=LIM) +
#   scale_fill_manual(values=COLS,name="Origin") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),legend.position="none",
#         panel.border=element_rect(colour="black",size=1.5),
#         plot.margin=unit(c(0.1, 5, 0.05, 0.01), "lines")) 
# p4
######

quartz(file=paste("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/Mixed Model/Abundance plot + ERROR",age.name,"+ ; F=",F_scen_name,".pdf",sep=""),dpi=600,height=8,width=7,type="pdf")   
  Layout= matrix(c(4,4,4,4,1,1,1,5,5,5,5,2,2,2,6,6,6,6,3,3,3),nrow=3,ncol=7,byrow=T)
  QQ <- list(p1,p2,p3,q1,q2,q3)
  multiplot(plotlist=QQ ,layout= Layout)
dev.off()
  } # End WWW loop
} # End QQQ loop



#######################
######################
#######################
######################
#######################
######################
# Age-comparison plot
#######################
######################
#######################
######################
#######################
######################

COLS <- c("SFB"= "#DAA520", #"#ffffcc",
          "NCA"="#B8860B",
          "SOR"="#3CB371",
          "COR"="#228B22",
          "NOR"="#006400",
          "COL"  ="#00BFFF",
          "MCOL"  = "#1E90FF",
          "UPCOL"= "#0000FF",
          "WAC"  = "black",
          "PUSO" ="#FF6347",
          "SGEO" ="#DC143C",
          "SWVI" = "#A52A2A")

#### MAKE CUMULATIVE DISTRIBUTION FOR ALL AREAS BASED ON MEAN SURVIVAL FOR EACH REGION, MEAN DISTRIBUTION, MEAN EVERYTHING.
    F_scen_name <- "F_median" # options are" "F_zero"   "F_median" "F_max"

#### FIRST AGE ###########################################################################     
AGE_temp <- 2
    
    #####################
    age.name <- AGE_temp 
    temp <- OUT.LONG.sum[OUT.LONG.sum$age.year >= AGE_temp,]
    OUT.OLD <- summarise(group_by(temp,loc,season,F_scen),chin = sum(MEAN),chin.SD = sqrt(sum(SD^2)))
    OUT.OLD <- data.frame(OUT.OLD)
    OUT.OLD$chin    <- OUT.OLD$chin/1000
    OUT.OLD$chin.SD <- OUT.OLD$chin.SD/1000
    
    temp <- OUT.LONG[OUT.LONG$age.year >= AGE_temp,]
    OUT.OLD.REG <- summarise(group_by(temp,ocean.region,loc,season,F_scen),chin = sum(MEAN),chin.SD = sqrt(sum(SD^2)))
    OUT.OLD.REG <- data.frame(OUT.OLD.REG)
    
    OUT.OLD.DENS<- merge(OUT.OLD.REG,region.area,by.x="loc",by.y="location.name")
    OUT.OLD.DENS$dens <- OUT.OLD.DENS$chin / OUT.OLD.DENS$km2.10.200m
    
    ### Subset out the relevant Fishing scenario.
    OUT.OLD.REG <- OUT.OLD.REG[OUT.OLD.REG$F_scen == F_scen_name,]
    OUT.OLD.DENS <- OUT.OLD.DENS[OUT.OLD.DENS$F_scen == F_scen_name,]
    
    ###################################################################################
    ### Calculate Evenness for each region and each season
    OUT.OLD.REG$chin     <- OUT.OLD.REG$chin / 1000
    OUT.OLD.REG$loc.name <- OUT.OLD.REG$loc
    OUT.OLD.REG$loc         <- LOCATIONS$location.number[match(OUT.OLD.REG$loc.name,LOCATIONS$location.name)]
    OUT.OLD.REG$origin.numb <- spawn_loc$number[match(OUT.OLD.REG$ocean.region,spawn_loc$ocean.region)]
    
    OUT.OLD.DENS$loc.name <- OUT.OLD.DENS$loc
    OUT.OLD.DENS$loc <- LOCATIONS$location.number[match(OUT.OLD.DENS$loc.name,LOCATIONS$location.name)]
    OUT.OLD.DENS$origin.numb <- spawn_loc$number[match(OUT.OLD.DENS$ocean.region,spawn_loc$ocean.region)]
    
    OUT.OLD.REG <- OUT.OLD.REG[order(OUT.OLD.REG$origin.numb),]
    OUT.OLD.DENS <- OUT.OLD.DENS[order(OUT.OLD.DENS$origin.numb),]
    
    OUT.OLD.REG$ocean.region <-  factor(OUT.OLD.REG$ocean.region, 
                                        levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                                   "NOR","COR","SOR","NCA","SFB"))
    OUT.OLD.DENS$ocean.region <-  factor(OUT.OLD.DENS$ocean.region, 
                                         levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                                    "NOR","COR","SOR","NCA","SFB"))
    
    LIM=c(0,4000)
    
    # Proportional contribution
    q1 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = ocean.region )) + 
      geom_bar(position = "fill",stat = "identity") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = " ", y = "",title="a) Summer, Age 2+") +
      scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
      scale_fill_manual(values=COLS,name="Origin") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.05, 0.05, 0.38), "lines")) 
    q1

    p1 <- ggplot(data=OUT.OLD[OUT.OLD$season == "Sum",]) + 
      geom_bar(aes(x = loc, y = chin ),fill=grey(0.5),stat = "identity") +
      coord_flip() +
      labs(x = "",y="",title="e)") + 
      xlab(NULL) +
      scale_x_discrete(expand=c(0,0),breaks = 1:nrow(LOCATIONS), labels= rep("",nrow(LOCATIONS))) + 
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      scale_fill_manual(values=COLS,name="Origin") +
      theme_bw() +
      geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),
            plot.title = element_text(hjust = 0,color="white",size=rel(0.9)),legend.position="none",
            panel.border=element_rect(colour="black",size=1.5),
            plot.margin=unit(c(0.1, 4.62, 0.05, 0.3), "lines")) 
    p1
    
#### SECOND AGE ######################################################################
AGE_temp <- 4
    #####################
    age.name <- AGE_temp 
    temp <- OUT.LONG.sum[OUT.LONG.sum$age.year >= AGE_temp,]
    OUT.OLD <- summarise(group_by(temp,loc,season,F_scen),chin = sum(MEAN),chin.SD = sqrt(sum(SD^2)))
    OUT.OLD <- data.frame(OUT.OLD)
    OUT.OLD$chin    <- OUT.OLD$chin/1000
    OUT.OLD$chin.SD <- OUT.OLD$chin.SD/1000
    
    temp <- OUT.LONG[OUT.LONG$age.year >= AGE_temp,]
    OUT.OLD.REG <- summarise(group_by(temp,ocean.region,loc,season,F_scen),chin = sum(MEAN),chin.SD = sqrt(sum(SD^2)))
    OUT.OLD.REG <- data.frame(OUT.OLD.REG)
    
    OUT.OLD.DENS<- merge(OUT.OLD.REG,region.area,by.x="loc",by.y="location.name")
    OUT.OLD.DENS$dens <- OUT.OLD.DENS$chin / OUT.OLD.DENS$km2.10.200m
    
    ### Subset out the relevant Fishing scenario.
    OUT.OLD.REG <- OUT.OLD.REG[OUT.OLD.REG$F_scen == F_scen_name,]
    OUT.OLD.DENS <- OUT.OLD.DENS[OUT.OLD.DENS$F_scen == F_scen_name,]
    
    ###################################################################################
    ### Calculate Evenness for each region and each season
    OUT.OLD.REG$chin     <- OUT.OLD.REG$chin / 1000
    OUT.OLD.REG$loc.name <- OUT.OLD.REG$loc
    OUT.OLD.REG$loc         <- LOCATIONS$location.number[match(OUT.OLD.REG$loc.name,LOCATIONS$location.name)]
    OUT.OLD.REG$origin.numb <- spawn_loc$number[match(OUT.OLD.REG$ocean.region,spawn_loc$ocean.region)]
    
    OUT.OLD.DENS$loc.name <- OUT.OLD.DENS$loc
    OUT.OLD.DENS$loc <- LOCATIONS$location.number[match(OUT.OLD.DENS$loc.name,LOCATIONS$location.name)]
    OUT.OLD.DENS$origin.numb <- spawn_loc$number[match(OUT.OLD.DENS$ocean.region,spawn_loc$ocean.region)]
    
    OUT.OLD.REG <- OUT.OLD.REG[order(OUT.OLD.REG$origin.numb),]
    OUT.OLD.DENS <- OUT.OLD.DENS[order(OUT.OLD.DENS$origin.numb),]
    
    OUT.OLD.REG$ocean.region <-  factor(OUT.OLD.REG$ocean.region, 
                                        levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                                   "NOR","COR","SOR","NCA","SFB"))
    OUT.OLD.DENS$ocean.region <-  factor(OUT.OLD.DENS$ocean.region, 
                                         levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                                    "NOR","COR","SOR","NCA","SFB"))
    
    LIM=c(0,750)
    
    # Proportional contribution
    q2 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = ocean.region )) + 
      geom_bar(position = "fill",stat = "identity") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "", y = "Proportion",title="b) Summer, Age 4+") +
      scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
      scale_fill_manual(values=COLS,name="Origin") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.05, 0.05, 0.38), "lines")) 
    q2
    
    p2 <- ggplot(data=OUT.OLD[OUT.OLD$season == "Sum",]) + 
      geom_bar(data=OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = ocean.region ),stat = "identity") +
      geom_bar(aes(x = loc, y = chin ),fill=grey(0.5),stat="identity") +
      geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
      coord_flip() +
      labs(x = "", y = "Chinook (1000s)",title="f)") +
      xlab(NULL) +
      scale_x_discrete(expand=c(0.0,0),breaks = 1:nrow(LOCATIONS), labels= rep("",nrow(LOCATIONS))) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      scale_fill_manual(values=COLS,name="Origin") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,color="white",size=rel(0.9)),
            panel.border=element_rect(colour="black",size=1.5),
            plot.margin=unit(c(0.1, 0.0, 0.05, 0.3), "lines"),
            legend.key.size = unit(0.4, "cm")) 
    p2    
    
    
    quartz(file=paste("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/Mixed Model/Abundance plot COMPARE 2 & 4; F=",F_scen_name,".pdf",sep=""),dpi=600,height=6,width=7,type="pdf")   
      Layout= matrix(c(3,3,3,3,1,1,1,4,4,4,4,2,2,2),nrow=2,ncol=7,byrow=T)
      QQ <- list(p1,p2,q1,q2)
      multiplot(plotlist=QQ ,layout= Layout)
    dev.off()
    
    #########################################################################################
    #########################################################################################
    #########################################################################################
    ####################################### ##################################################
    #########################################################################################
    #########################################################################################
    
    # alternate comparison of age-structure
    
    #########################################################################################
    #########################################################################################
    #########################################################################################
    #########################################################################################
    #########################################################################################
    #########################################################################################
    
    AGE_temp <- 2
    ORIG.OLD <- summarise(group_by(OUT.LONG[OUT.LONG$age.year >= AGE_temp,],ocean.region,loc,season,F_scen),chin = sum(MEAN)/1000,chin.SD = sqrt(sum(SD^2))/1000)
    ORIG.OLD$ocean.region <-  factor(ORIG.OLD$ocean.region, 
                                        levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                                   "NOR","COR","SOR","NCA","SFB"))
                
    ORIG.OLD.sum <- summarise(group_by(OUT.LONG.sum[OUT.LONG.sum$age.year >= AGE_temp,],loc,season,F_scen),chin = sum(MEAN)/1000,chin.SD = sqrt(sum(SD^2))/1000)
    
    
    
    # Make 2 row by 5 column histograms of abundance distribution.
    LIM=c(0,1500)
    PLOT.MAR <- c(0.0,0.25,0.0,-0.5)
    PLOT.MAR.first <-c(0.0,0.25,0.0,0.5)
    a2.sfb <- ggplot(data=ORIG.OLD[ORIG.OLD$season == "Sum" & ORIG.OLD$ocean.region == "SFB",]) + 
      # geom_bar(data=OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = ocean.region ),stat = "identity") +
      geom_bar(aes(x = loc, y = chin,fill=ocean.region ),stat="identity") +
      geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
      coord_flip() +
      labs(x = "", y = "Chinook (1000s)",title="a)") +
      scale_fill_manual(values=COLS,name="Origin") +
      #scale_x_discrete(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) + #rp("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.9,vjust=0.5),
            axis.title.x = element_text(color="white"),
            plot.title = element_text(hjust=0,size=rel(0.9),vjust=0),legend.position="none",
            plot.margin=unit(PLOT.MAR.first, "lines")) 

    a2.col <- ggplot(data=ORIG.OLD[ORIG.OLD$season == "Sum" & ORIG.OLD$ocean.region == "COL",]) + 
      # geom_bar(data=OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = ocean.region ),stat = "identity") +
      geom_bar(aes(x = loc, y = chin,fill=ocean.region ),stat="identity") +
      geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
      coord_flip() +
      labs(x = "", y = "Chinook (1000s)",title="b)") +
      scale_fill_manual(values=COLS,name="Origin") +
      scale_x_discrete(expand=c(0.0,0)) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.9, vjust=0.5),
            axis.title.x = element_text(color="white"),
            axis.text.y = element_blank(),
            plot.title = element_text(hjust=0,size=rel(0.9),vjust=0),legend.position="none",
            plot.margin=unit(PLOT.MAR, "lines")) 
    a2.col
    
    a2.wac <- ggplot(data=ORIG.OLD[ORIG.OLD$season == "Sum" & ORIG.OLD$ocean.region == "WAC",]) + 
      # geom_bar(data=OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = ocean.region ),stat = "identity") +
      geom_bar(aes(x = loc, y = chin,fill=ocean.region ),stat="identity") +
      geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
      coord_flip() +
      xlab(NULL) +
      labs(x = "", y = "Chinook (2+,1000s)",title="c)") +
      scale_fill_manual(values=COLS,name="Origin") +
      scale_x_discrete(expand=c(0.0,0)) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.9, vjust=0.5),
            axis.text.y = element_blank(),
            axis.title.x = element_text(size=rel(0.9)),
            plot.title = element_text(hjust=0,size=rel(0.9),vjust=0),legend.position="none",
            plot.margin=unit(PLOT.MAR, "lines")) 
    
    a2.sgeo <- ggplot(data=ORIG.OLD[ORIG.OLD$season == "Sum" & ORIG.OLD$ocean.region == "SGEO",]) + 
      # geom_bar(data=OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = ocean.region ),stat = "identity") +
      geom_bar(aes(x = loc, y = chin,fill=ocean.region ),stat="identity") +
      geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
      coord_flip() +
      xlab(NULL) +
      labs(x = "", y = "Chinook (1000s)",title="d)") +
      scale_fill_manual(values=COLS,name="Origin") +
      scale_x_discrete(expand=c(0.0,0)) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.9, vjust=0.5),
            axis.title.x = element_text(color="white"),
            axis.text.y = element_blank(),
            plot.title = element_text(hjust=0,size=rel(0.9),vjust=0),legend.position="none",
            plot.margin=unit(PLOT.MAR, "lines")) 
   
    
    LIM=c(0,4000)
    a2.all <- ggplot(data=ORIG.OLD.sum[ORIG.OLD.sum$season == "Sum" ,]) + 
      geom_bar(aes(x = loc, y = chin ),fill=grey(0.4), stat="identity") +
      geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
      coord_flip() +
      xlab(NULL) +
      labs(x = "", y = "Chinook (1000s)",title="e)") +
      scale_fill_manual(values=COLS,name="Origin") +
      scale_x_discrete(expand=c(0.0,0)) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.9, vjust=0.5),
            axis.title.x = element_text(color="white",size=rel(0.9)),
            axis.text.y = element_blank(),
            plot.title = element_text(hjust=0,size=rel(0.9),vjust=0),legend.position="none",
            plot.margin=unit(PLOT.MAR, "lines")) 
    
    
    ### AGE 4+
    
    AGE_temp <- 4
    ORIG.OLD <- summarise(group_by(OUT.LONG[OUT.LONG$age.year >= AGE_temp,],ocean.region,loc,season,F_scen),chin = sum(MEAN)/1000,chin.SD = sqrt(sum(SD^2))/1000)
    ORIG.OLD$ocean.region <-  factor(ORIG.OLD$ocean.region, 
                                     levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                                "NOR","COR","SOR","NCA","SFB"))
    ORIG.OLD.sum <- summarise(group_by(OUT.LONG.sum[OUT.LONG.sum$age.year >= AGE_temp,],loc,season,F_scen),chin = sum(MEAN)/1000,chin.SD = sqrt(sum(SD^2))/1000)
    
    LIM = c(0,100)
    
    a4.sfb <- ggplot(data=ORIG.OLD[ORIG.OLD$season == "Sum" & ORIG.OLD$ocean.region == "SFB",]) + 
      # geom_bar(data=OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = ocean.region ),stat = "identity") +
      geom_bar(aes(x = loc, y = chin,fill=ocean.region ),stat="identity") +
      geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
      coord_flip() +
      labs(x = "", y = "Chinook (1000s)",title="f)") +
      scale_fill_manual(values=COLS,name="Origin") +
      #scale_x_discrete(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) + #rp("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.9,vjust=0.5),
            axis.title.x = element_text(color="white"),
            plot.title = element_text(hjust=0,size=rel(0.9),vjust=0),legend.position="none",
            plot.margin=unit(PLOT.MAR.first, "lines")) 
    
    a4.col <- ggplot(data=ORIG.OLD[ORIG.OLD$season == "Sum" & ORIG.OLD$ocean.region == "COL",]) + 
      # geom_bar(data=OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = ocean.region ),stat = "identity") +
      geom_bar(aes(x = loc, y = chin,fill=ocean.region ),stat="identity") +
      geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
      coord_flip() +
      labs(x = "", y = "Chinook (1000s)",title="g)") +
      scale_fill_manual(values=COLS,name="Origin") +
      scale_x_discrete(expand=c(0.0,0)) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.9, vjust=0.5),
            axis.title.x = element_text(color="white"),
            axis.text.y = element_blank(),
            plot.title = element_text(hjust=0,size=rel(0.9),vjust=0),legend.position="none",
            plot.margin=unit(PLOT.MAR, "lines")) 
    
    a4.wac <- ggplot(data=ORIG.OLD[ORIG.OLD$season == "Sum" & ORIG.OLD$ocean.region == "WAC",]) + 
      # geom_bar(data=OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = ocean.region ),stat = "identity") +
      geom_bar(aes(x = loc, y = chin,fill=ocean.region ),stat="identity") +
      geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
      coord_flip() +
      xlab(NULL) +
      labs(x = "", y = "Chinook (4+,1000s)",title="h)") +
      scale_fill_manual(values=COLS,name="Origin") +
      scale_x_discrete(expand=c(0.0,0)) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.9, vjust=0.5),
            axis.title.x = element_text(color="black",size=rel(0.9)),
            axis.text.y = element_blank(),
            plot.title = element_text(hjust=0,size=rel(0.9),vjust=0),legend.position="none",
            plot.margin=unit(PLOT.MAR, "lines")) 
    
    a4.sgeo <- ggplot(data=ORIG.OLD[ORIG.OLD$season == "Sum" & ORIG.OLD$ocean.region == "SGEO",]) + 
      # geom_bar(data=OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = ocean.region ),stat = "identity") +
      geom_bar(aes(x = loc, y = chin,fill=ocean.region ),stat="identity") +
      geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
      coord_flip() +
      xlab(NULL) +
      labs(x = "", y = "Chinook (1000s)",title="i)") +
      scale_fill_manual(values=COLS,name="Origin") +
      scale_x_discrete(expand=c(0.0,0)) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.9, vjust=0.5),
            axis.title.x = element_text(color="white"),
            axis.text.y = element_blank(),
            plot.title = element_text(hjust=0,size=rel(0.9),vjust=0),legend.position="none",
            plot.margin=unit(PLOT.MAR, "lines")) 
    
    LIM <- c(0,400)
    
    a4.all <- ggplot(data=ORIG.OLD.sum[ORIG.OLD.sum$season == "Sum",]) + 
      # geom_bar(data=OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = ocean.region ),stat = "identity") +
      geom_bar(aes(x = loc, y = chin ),fill=grey(0.4),stat="identity") +
      geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
      coord_flip() +
      xlab(NULL) +
      labs(x = "", y = "Chinook (1000s)",title="j)") +
      scale_fill_manual(values=COLS,name="Origin") +
      scale_x_discrete(expand=c(0.0,0)) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.9, vjust=0.5),
            axis.title.x = element_text(color="white"),
            axis.text.y = element_blank(),
            plot.title = element_text(hjust=0,size=rel(0.9),vjust=0),legend.position="none",
            plot.margin=unit(PLOT.MAR, "lines")) 
    
    ##################################
    ##################################        
    ##################################    
    ##################################
    quartz(file=paste("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/Mixed Model/Abundance plot COMPARE 2 & 4 by region; F=",F_scen_name,".pdf",sep=""),dpi=600,height=6,width=7,type="pdf")   
      Layout= matrix(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,7,8,8,9,9,10,10),nrow=2,ncol=11,byrow=T)
      QQ <- list(a2.sfb,a2.col,a2.wac,a2.sgeo, a2.all, a4.sfb,a4.col,a4.wac,a4.sgeo, a4.all)
      multiplot(plotlist=QQ ,layout= Layout)
    dev.off()
    
    
    
    
    
    
    
    

    ################################################################################################################
    ################################################################################################################
    ################################################################################################################
    ################################################################################################################
    ################################################################################################################
    ################################################################################################################
    ################################################################################################################
    ### RE RUN SIMULATIONS WITH HALF PUGET SOUND HATCHERY PRODUCTION
    ################################################################################################################
    ################################################################################################################
    ################################################################################################################
    ################################################################################################################
    ################################################################################################################
    ################################################################################################################
    ################################################################################################################
    ################################################################################################################
    
PUSO.reg     <- ALL.LONG
PUSO.reg.sum <- ALL.LONG.sum 
    # Calculate number of outmigrant smolt from various regions....
    smolt.dat <- read.csv("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Total Smolt and Run Info/smolt_data_01-2017.csv")
    
    # smolt.mod <- smolt.dat[smolt.dat$type == "independent",]
    # smolt.mod$total.wild.hatch <- smolt.mod$total.releases.median * (1+smolt.mod$frac.wild.missing) 
    # smolt.mod$finger <- smolt.mod$total.wild.hatch * (1 - smolt.mod$frac.yearling )
    # smolt.mod$yearling <- smolt.mod$total.wild.hatch * smolt.mod$frac.yearling
    # smolt.prod <- aggregate(smolt.mod[,c("finger","yearling")],
    #                         by=list(number=smolt.mod$location.number,region=smolt.mod$rel.region,
    #                                 n.mon.fing = smolt.mod$n.month.finger ,n.mon.year = smolt.mod$n.month.yearling 
    #                         ),sum)
    smolt.dat$hatchery[smolt.dat$region == "PUSO"] <- smolt.dat$hatchery[smolt.dat$region == "PUSO"] /2
    smolt.dat$total <- smolt.dat$hatchery + smolt.dat$wild
    smolt.prod <- summarize(group_by(smolt.dat,region,number),Sum=sum(total))
    smolt.prod <- smolt.prod[order(smolt.prod$number),]
    #colnames(smolt.prod)[which(names(smolt.prod)=="location.number")] ="number"
    #############################################################################################
    ##############################################################################################
    ##############################################################################################
    ##############################################################################################
    #############################################################################################
    ### PROJECT SPATIAL DISTRIBUTION FOR EACH ORIGIN
    
        #use raw median
    juv.mort.shared <- median(rel_year_all)
    #use weighted mean from Juv mort SS.r
    juv.mort.shared <-  mean(exp(juv.dat.sum$w.mean))
    #############################################################################################
    
    sim.region <- aggregate(dat.bin$ocean.reg,
                            by=list(region = dat.bin$ocean.reg,
                                    origin.idx = dat.bin$origin.idx,
                                    loc.spawn.idx = dat.bin$loc.spawn.idx
                                    #season.idx = dat.bin$season.idx
                            ),length)[,1:3]
    
    #sim.region <- merge(sim.region,juv.mort[,c("loc.spawn.idx","juv.mort")],all=T,by="loc.spawn.idx")
    
    sim.region <- merge(sim.region,smolt.prod)
    sim.region <- sim.region[order(sim.region$number),]
    #sim.region$juv.mort.shared <- mean(exp(samp$log_rel_year_mu))
    
    N.mod.int     <- max(dat.bin$time)
    LOC           <- sort(unique(dat.bin$location))
    AGE_MONTH_IDX <- 1:N.mod.int 
    SEASON_NAME   <- c(c("spring","summer","fall"),rep(c("winter","spring","summer","fall"),4))
    SEASON_IDX    <- c(c(1,2,3),rep(c(1,1,2,3),4))
    
    sim.dat       <-  expand.grid(age.month=AGE_MONTH_IDX,loc=LOC)
    sim.dat$season.idx <- SEASON_IDX
    sim.dat$season.name <-SEASON_NAME
    sim.dat$age.year <- 0
    sim.dat$age.year[sim.dat$age.month<=3]<-1
    sim.dat$age.year[sim.dat$age.month >3 & sim.dat$age.month <= 7 ]  <- 2
    sim.dat$age.year[sim.dat$age.month >7 & sim.dat$age.month <= 11 ] <- 3
    sim.dat$age.year[sim.dat$age.month >11 & sim.dat$age.month <= 15 ]<- 4
    sim.dat$age.year[sim.dat$age.month >15 & sim.dat$age.month <= 19 ]<- 5
    
    OUT <- sim.dat
    
    smolt.prod$origin.idx <- COV$origin.idx[match(smolt.prod$region,COV$ocean.region)]
    smolt.prod$loc.spawn.idx <- COV$loc.spawn.idx[match(smolt.prod$region,COV$ocean.region)]
    
    log_N_pred <- matrix(0,N_time_mod,N_loc)
    D_pred     <- matrix(0,nrow(smolt.prod),N_year)
    river_entry_proj <- river_entry[match(smolt.prod$region,REL$ocean.region),]
    
    F_scen <- c("F_zero","F_median","F_max")
    F_scen <- c("F_median")
    ############## PROJECTIONS ( USING 3 Fishing Mortality defined by F_scen)
    
    
    ALL.LONG <- NULL
    # CONTROL HOW MANY SAMPLES TO TAKE FROM THE POSTERIOR
    THESE <-seq(1,length(samp$log_q_troll_pos),length.out=1000)
    
    for(k in 1:length(F_scen)){
      if(F_scen[k]=="F_zero"){F_t_temp  <- F_troll_zero_mat; F_r_temp   <- F_rec_zero_mat}
      if(F_scen[k]=="F_median"){F_t_temp  <- F_troll_median_mat; F_r_temp <- F_rec_median_mat}
      if(F_scen[k]=="F_max"){F_t_temp  <- F_troll_max_mat; F_r_temp    <- F_rec_max_mat}
      F_TROLL_MAT <- NULL
      F_REC_MAT <- NULL
      for(kk in 1:5){
        F_TROLL_MAT <- rbind(F_TROLL_MAT,F_t_temp)
        F_REC_MAT <- rbind(F_REC_MAT,F_r_temp)
      }
      F_TROLL_MAT <- F_TROLL_MAT[1:N_time_mod,]  
      F_REC_MAT   <- F_REC_MAT[1:N_time_mod,]
      
      for(kkk in 1:ncol(F_TROLL_MAT)){
        F_TROLL_MAT[,kkk] <- F_TROLL_MAT[,kkk] * vuln_mat[vuln_int_idx[kkk],]
        F_REC_MAT[,kkk]   <- F_REC_MAT[,kkk]   * vuln_mat[vuln_int_rec_idx[kkk],]
      }
      
      F_TOT_MAT <- F_TROLL_MAT + F_REC_MAT
      
      ##################################################################
      for(i in 1:nrow(smolt.prod)){  # Cycle Across release groups.
        print(paste(i,"of",nrow(smolt.prod)))
        ALL.SIM <- NULL
        ALL.SIM.end <- NULL
        for(L in 1:length(THESE)){ # cycle across samples from the posterior.
          O_L <- samp$origin_loc[THESE[L],,,]
          P_A_Y <- samp$prob_age_year[THESE[L],,]
          
          log_N_pred  <- log_N_pred  * 0
          log_N_begin <- log_N_pred
          for(j in 1:N_time_mod){ # Cycle Across model time..
            if(j == 1){
              log_N_pred[j,] <-  log(smolt.prod$Sum[i]*1e6  ) - juv.mort.shared - AGE$M2_est[j] +
                log(O_L[AGE$season_idx[j],smolt.prod$origin.idx[i],]) 
              log_N_begin[j,] <-  log(smolt.prod$Sum[i]*1e6 ) - juv.mort.shared +
                log(O_L[AGE$season_idx[j],smolt.prod$origin.idx[i],]) 
            }
            if(j>1){
              if(spawn_time[j] == 0){
                log_N_pred[j,]   <-       log(sum(exp(log_N_pred[j-1,]))) + 
                  epsilon.region$MEAN[epsilon.region$ocean.region == smolt.prod$region[i] & 
                                        epsilon.region$model.time == j-1] +  
                  log(O_L[AGE$season_idx[j],smolt.prod$origin.idx[i],]) -
                  AGE$M2_est[j]   -
                  F_TOT_MAT[j,]
                log_N_begin[j,]   <-       log(sum(exp(log_N_pred[j-1,]))) +  
                  epsilon.region$MEAN[epsilon.region$ocean.region == smolt.prod$region[i] & 
                                        epsilon.region$model.time == j-1]  +
                  log(O_L[AGE$season_idx[j],smolt.prod$origin.idx[i],])
              }
              if(AGE$season_idx[j]==3){
                log_N_temp     <-       log(sum(exp(log_N_pred[j-1,]))) +  
                  epsilon.region$MEAN[epsilon.region$ocean.region == smolt.prod$region[i] & 
                                        epsilon.region$model.time == j-1] +
                  log(O_L[AGE$season_idx[j],smolt.prod$origin.idx[i],]) -
                  AGE$M2_est[j] * 0.5   -
                  F_TOT_MAT[j,] * 0.5 ;
                log_N_begin[j,]   <-    log(sum(exp(log_N_pred[j-1,]))) + 
                  epsilon.region$MEAN[epsilon.region$ocean.region == smolt.prod$region[i] & 
                                        epsilon.region$model.time == j-1] +
                  log(O_L[AGE$season_idx[j],smolt.prod$origin.idx[i],])
                D_pred[i,AGE$spawn_time[j]] <- sum(exp((log_N_temp +  log(P_A_Y[smolt.prod$loc.spawn.idx[i], AGE$spawn_time[j]])) * river_entry_proj[i,])) ;
                log_N_temp     <-   log_N_temp +
                  log(1- P_A_Y[smolt.prod$loc.spawn.idx[i], AGE$spawn_time[j]]* river_entry_proj[i,]) -
                  AGE$M2_est[j] * 0.5   -
                  F_TOT_MAT[j,] * 0.5  ;
                log_N_pred[j,] <-  log_N_temp ;
              }
            }
          }
          
          A_temp <- data.frame(ocean.region= smolt.prod$region[i],   
                               F_scen =F_scen[k],
                               time=AGE$time,
                               age.month=AGE$age,
                               age.year=AGE$year,
                               season=rownames(F_TOT_MAT),
                               juv.mort=juv.mort.shared,
                               exp(log_N_begin))
          ALL.SIM <- rbind(ALL.SIM,A_temp)
          
          B_temp <- data.frame(ocean.region= smolt.prod$region[i],   
                               F_scen =F_scen[k],
                               time=AGE$time,
                               age.month=AGE$age,
                               age.year=AGE$year,
                               season=rownames(F_TOT_MAT),
                               juv.mort=juv.mort.shared,
                               exp(log_N_pred))
          ALL.SIM.end <- rbind(ALL.SIM.end,B_temp)
        } # End posterior sample loop
        
        # Create object which is sum of fish across all origin locations.
        if(i == 1){
          ALL.SIM.sum <- ALL.SIM
          ALL.SIM.end.sum <- ALL.SIM
        }
        if(i > 1){
          these.col <- grep("X",colnames(ALL.SIM.sum))
          ALL.SIM.sum[these.col] <- ALL.SIM.sum[,these.col] + ALL.SIM[,these.col]
          ALL.SIM.sum$ocean.region <- "TOTAL"
          ALL.SIM.end.sum[these.col] <- ALL.SIM.end.sum[,these.col] + ALL.SIM.end[,these.col]
          ALL.SIM.end.sum$ocean.region <- "TOTAL"
        }
        
        # Summarize simulations from given release location  
        THESE.COL <- grep("X",colnames(ALL.SIM))
        colnames(ALL.SIM)[THESE.COL] <- as.character(LOCATIONS$location.name)
        ALL.LONG.temp <- melt(ALL.SIM,id.vars = c("ocean.region","F_scen","time","age.month","age.year","season","juv.mort"))
        colnames(ALL.LONG.temp)[colnames(ALL.LONG.temp)=="variable"] <- "loc"
        
        TEMP <- summarise(group_by(ALL.LONG.temp,ocean.region,F_scen,time,age.month,age.year,season,loc),MEAN = mean(value),MEDIAN=median(value),SD =sd(value),
                          q.025 = quantile(value,probs=0.025),q.05 = quantile(value,probs=0.05), q.25 = quantile(value,probs=0.25),
                          q.75 = quantile(value,probs=0.75),q.95 = quantile(value,probs=0.95), q.975 = quantile(value,probs=0.975)
        )
        ALL.LONG <-rbind(ALL.LONG,TEMP)    
      }  # End Release location loop  
    } # End Fishing mortality loop
    
    THESE.COL <- grep("X",colnames(ALL.SIM.sum))
    colnames(ALL.SIM.sum)[THESE.COL] <- as.character(LOCATIONS$location.name)
    
    ALL.LONG.sum <- melt(ALL.SIM.sum,id.vars = c("ocean.region","F_scen","time","age.month","age.year","season","juv.mort"))
    colnames(ALL.LONG.sum)[colnames(ALL.LONG.sum)=="variable"] <- "loc"
    
    ALL.LONG.sum <- summarise(group_by(ALL.LONG.sum,ocean.region,F_scen,time,age.month,age.year,season,loc),MEAN = mean(value),MEDIAN=median(value),SD =sd(value),
                              q.025 = quantile(value,probs=0.025),q.05 = quantile(value,probs=0.05), q.25 = quantile(value,probs=0.25),
                              q.75 = quantile(value,probs=0.75),q.95 = quantile(value,probs=0.95), q.975 = quantile(value,probs=0.975)
    )
    
    # ALL.LONG$value <- ALL.LONG$value
    # 
    # OUT.TOT    <- ALL.LONG[ALL.LONG$loc=="TOTAL",]
    OUT.LONG        <- ALL.LONG
    OUT.LONG.sum    <- ALL.LONG.sum
    
    ####################################
    PUSO.half     <- ALL.LONG
    PUSO.half.sum <- ALL.LONG.sum 
    ################################################################################################
    ################################################################################################
    ################################################################################################
    ################################################################################################
    ################################################################################################
    ################################################################################################
    ################## MAKE PLOT    
    #### FULL PUSO AGE ###########################################################################     
    AGE_temp <- 3
    SEASON <- "Sum"
    
    #####################
    age.name <- AGE_temp 
    temp <- PUSO.reg.sum[PUSO.reg.sum$age.year >= AGE_temp,]
    OUT.OLD <- summarise(group_by(temp,loc,season,F_scen),chin = sum(MEAN),chin.SD = sqrt(sum(SD^2)))
    OUT.OLD <- data.frame(OUT.OLD)
    OUT.OLD$chin    <- OUT.OLD$chin/1000
    OUT.OLD$chin.SD <- OUT.OLD$chin.SD/1000
    
    temp <- PUSO.reg[PUSO.reg$age.year >= AGE_temp,]
    OUT.OLD.REG <- summarise(group_by(temp,ocean.region,loc,season,F_scen),chin = sum(MEAN),chin.SD = sqrt(sum(SD^2)))
    OUT.OLD.REG <- data.frame(OUT.OLD.REG)
    
    OUT.OLD.DENS<- merge(OUT.OLD.REG,region.area,by.x="loc",by.y="location.name")
    OUT.OLD.DENS$dens <- OUT.OLD.DENS$chin / OUT.OLD.DENS$km2.10.200m
    
    ### Subset out the relevant Fishing scenario.
    OUT.OLD.REG <- OUT.OLD.REG[OUT.OLD.REG$F_scen == F_scen_name,]
    OUT.OLD.DENS <- OUT.OLD.DENS[OUT.OLD.DENS$F_scen == F_scen_name,]
    
    ###################################################################################
    ### Calculate Evenness for each region and each season
    OUT.OLD.REG$chin     <- OUT.OLD.REG$chin / 1000
    OUT.OLD.REG$loc.name <- OUT.OLD.REG$loc
    OUT.OLD.REG$loc         <- LOCATIONS$location.number[match(OUT.OLD.REG$loc.name,LOCATIONS$location.name)]
    OUT.OLD.REG$origin.numb <- spawn_loc$number[match(OUT.OLD.REG$ocean.region,spawn_loc$ocean.region)]
    
    OUT.OLD.DENS$loc.name <- OUT.OLD.DENS$loc
    OUT.OLD.DENS$loc <- LOCATIONS$location.number[match(OUT.OLD.DENS$loc.name,LOCATIONS$location.name)]
    OUT.OLD.DENS$origin.numb <- spawn_loc$number[match(OUT.OLD.DENS$ocean.region,spawn_loc$ocean.region)]
    
    OUT.OLD.REG <- OUT.OLD.REG[order(OUT.OLD.REG$origin.numb),]
    OUT.OLD.DENS <- OUT.OLD.DENS[order(OUT.OLD.DENS$origin.numb),]
    
    OUT.OLD.REG$ocean.region <-  factor(OUT.OLD.REG$ocean.region, 
                                        levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                                   "NOR","COR","SOR","NCA","SFB"))
    OUT.OLD.DENS$ocean.region <-  factor(OUT.OLD.DENS$ocean.region, 
                                         levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                                    "NOR","COR","SOR","NCA","SFB"))
    
    LIM=c(0,1800)
    
    BASE <- OUT.OLD
    # Proportional contribution
    q1 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season==SEASON,],  aes(x = loc, y = chin, fill = ocean.region )) + 
      geom_bar(position = "fill",stat = "identity") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = " ", y = "",title=paste("a) Summer, Age ",AGE_temp,"+"," base",sep="")) +
      scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
      scale_fill_manual(values=COLS,name="Origin") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.05, 0.05, 0.38), "lines")) 
    q1
    
    p1 <- ggplot(data=OUT.OLD[OUT.OLD$season == SEASON,]) + 
      geom_bar(aes(x = loc, y = chin ),fill=grey(0.5),stat = "identity") +
      coord_flip() +
      labs(x = "",y="",title="e)") + 
      xlab(NULL) +
      scale_x_discrete(expand=c(0,0),breaks = 1:nrow(LOCATIONS), labels= rep("",nrow(LOCATIONS))) + 
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      scale_fill_manual(values=COLS,name="Origin") +
      theme_bw() +
      geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),
            plot.title = element_text(hjust = 0,color="white",size=rel(0.9)),legend.position="none",
            panel.border=element_rect(colour="black",size=1.5),
            plot.margin=unit(c(0.1, 4.62, 0.05, 0.3), "lines")) 
    p1
    
    #### HALF PUSO ######################################################################
    #####################
    age.name <- AGE_temp 
    temp <- PUSO.half.sum[PUSO.half.sum$age.year >= AGE_temp,]
    OUT.OLD <- summarise(group_by(temp,loc,season,F_scen),chin = sum(MEAN),chin.SD = sqrt(sum(SD^2)))
    OUT.OLD <- data.frame(OUT.OLD)
    OUT.OLD$chin    <- OUT.OLD$chin/1000
    OUT.OLD$chin.SD <- OUT.OLD$chin.SD/1000
    
    temp <- PUSO.half[PUSO.half$age.year >= AGE_temp,]
    OUT.OLD.REG <- summarise(group_by(temp,ocean.region,loc,season,F_scen),chin = sum(MEAN),chin.SD = sqrt(sum(SD^2)))
    OUT.OLD.REG <- data.frame(OUT.OLD.REG)
    
    OUT.OLD.DENS<- merge(OUT.OLD.REG,region.area,by.x="loc",by.y="location.name")
    OUT.OLD.DENS$dens <- OUT.OLD.DENS$chin / OUT.OLD.DENS$km2.10.200m
    
    ### Subset out the relevant Fishing scenario.
    OUT.OLD.REG <- OUT.OLD.REG[OUT.OLD.REG$F_scen == F_scen_name,]
    OUT.OLD.DENS <- OUT.OLD.DENS[OUT.OLD.DENS$F_scen == F_scen_name,]
    
    ###################################################################################
    ### Calculate Evenness for each region and each season
    OUT.OLD.REG$chin     <- OUT.OLD.REG$chin / 1000
    OUT.OLD.REG$loc.name <- OUT.OLD.REG$loc
    OUT.OLD.REG$loc         <- LOCATIONS$location.number[match(OUT.OLD.REG$loc.name,LOCATIONS$location.name)]
    OUT.OLD.REG$origin.numb <- spawn_loc$number[match(OUT.OLD.REG$ocean.region,spawn_loc$ocean.region)]
    
    OUT.OLD.DENS$loc.name <- OUT.OLD.DENS$loc
    OUT.OLD.DENS$loc <- LOCATIONS$location.number[match(OUT.OLD.DENS$loc.name,LOCATIONS$location.name)]
    OUT.OLD.DENS$origin.numb <- spawn_loc$number[match(OUT.OLD.DENS$ocean.region,spawn_loc$ocean.region)]
    
    OUT.OLD.REG <- OUT.OLD.REG[order(OUT.OLD.REG$origin.numb),]
    OUT.OLD.DENS <- OUT.OLD.DENS[order(OUT.OLD.DENS$origin.numb),]
    
    OUT.OLD.REG$ocean.region <-  factor(OUT.OLD.REG$ocean.region, 
                                        levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                                   "NOR","COR","SOR","NCA","SFB"))
    OUT.OLD.DENS$ocean.region <-  factor(OUT.OLD.DENS$ocean.region, 
                                         levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                                    "NOR","COR","SOR","NCA","SFB"))
   
    REDUCE <- OUT.OLD
    # Proportional contribution
    q2 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season== SEASON,],  aes(x = loc, y = chin, fill = ocean.region )) + 
      geom_bar(position = "fill",stat = "identity") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "", y = "Proportion",title=paste("b) Summer, Age ",AGE_temp,"+"," reduced hatchery",sep="")) +
      scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
      scale_fill_manual(values=COLS,name="Origin") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.05, 0.05, 0.38), "lines")) 
    q2
    
    p2 <- ggplot(data=OUT.OLD[OUT.OLD$season == SEASON,]) + 
      geom_bar(data=OUT.OLD.REG[OUT.OLD.REG$season==SEASON,],  aes(x = loc, y = chin, fill = ocean.region ),stat = "identity") +
      geom_bar(aes(x = loc, y = chin ),fill=grey(0.5),stat="identity") +
      geom_errorbar(aes(x = loc,ymax = chin + 2*chin.SD, ymin=chin - 2*chin.SD), width=0) +
      coord_flip() +
      labs(x = "Proportion", y = "Chinook (1000s)",title="f)") +
      xlab(NULL) +
      scale_x_discrete(expand=c(0.0,0),breaks = 1:nrow(LOCATIONS), labels= rep("",nrow(LOCATIONS))) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      scale_fill_manual(values=COLS,name="Origin") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,color="white",size=rel(0.9)),
            panel.border=element_rect(colour="black",size=1.5),
            plot.margin=unit(c(0.1, 0.0, 0.05, 0.3), "lines"),
            legend.key.size = unit(0.4, "cm")) 
    p2    
    
    
    ###3 MAKE A DIFFERENCE PLOT OF CHANGES IN PUSO ABUNDANCE.
    colnames(REDUCE)[grep("chin",colnames(REDUCE))] <- c("reduce.chin","reduce.chin.SD")
    combo <- merge(BASE,REDUCE)
    
    combo$diff.mean <- combo$reduce.chin - combo$chin  
    combo$diff.sd <- sqrt(combo$chin.SD^2 + combo$reduce.chin.SD^2)
    combo$diff.prop <- combo$diff.mean / combo$chin
    combo$diff.prop.plus <- (combo$diff.mean+combo$diff.sd) / combo$chin
    combo$diff.prop.minus <- (combo$diff.mean-combo$diff.sd) / combo$chin
    
    
   diff.plot <- ggplot(data=combo[combo$season==SEASON,]) + 
      geom_bar(aes(x = loc, y = diff.prop),fill=grey(0.4),stat = "identity") +
      labs(x = "Ocean region", y = "Proportional change\n(Age 3+)",title="c)") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,color="black",size=rel(0.9)),
           panel.border=element_rect(colour="black",size=1.5),
           plot.margin=unit(c(0.1, -0.40, 0.1, 0.45), "lines"),
           legend.key.size = unit(0.4, "cm")) 
   
    diff.plot
    
    
    quartz(file=paste("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/Mixed Model/Abundance plot PUSO COMPARE; F=",F_scen_name,".pdf",sep=""),dpi=600,height=8,width=7,type="pdf")   
      Layout= matrix(c(3,3,3,3,1,1,1,4,4,4,4,2,2,2,5,5,5,5,5,5,6),nrow=3,ncol=7,byrow=T)
      QQ <- list(p1,p2,q1,q2,diff.plot)
      multiplot(plotlist=QQ ,layout= Layout)
    dev.off()

















