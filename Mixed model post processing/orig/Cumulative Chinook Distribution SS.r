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
load("Binomial+Positive_output-1978-90 Troll_Rec_Treaty_6year_STATE_SPACE+PROC_E100_M2FIXED_originP0_vulnfix_02-13-2017.RData")
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

############## PROJECTIONS ( USING 3 Fishing Mortality )
ALL.SIM <- NULL
ALL.SIM.end <- NULL
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
    
    log_N_pred  <- log_N_pred  * 0
    log_N_begin <- log_N_pred
    for(j in 1:N_time_mod){ # Cycle Across model time..
      if(j == 1){
        log_N_pred[j,] <-  log(smolt.prod$Sum[i]*1e6  ) - juv.mort.shared - AGE$M2_est[j] +
                              log(origin_loc[AGE$season_idx[j],smolt.prod$origin.idx[i],]) 
        log_N_begin[j,] <-  log(smolt.prod$Sum[i]*1e6 ) - juv.mort.shared +
                                log(origin_loc[AGE$season_idx[j],smolt.prod$origin.idx[i],]) 
      }
      if(j>1){
        if(spawn_time[j] == 0){
          log_N_pred[j,]   <-       log(sum(exp(log_N_pred[j-1,]))) + 
                                    epsilon.region$MEAN[epsilon.region$ocean.region == smolt.prod$region[i] & 
                                               epsilon.region$model.time == j-1] +  
                                    log(origin_loc[AGE$season_idx[j],smolt.prod$origin.idx[i],]) -
                                    AGE$M2_est[j]   -
                                    F_TOT_MAT[j,]
          log_N_begin[j,]   <-       log(sum(exp(log_N_pred[j-1,]))) +  
                                    epsilon.region$MEAN[epsilon.region$ocean.region == smolt.prod$region[i] & 
                                              epsilon.region$model.time == j-1]  +
                                    log(origin_loc[AGE$season_idx[j],smolt.prod$origin.idx[i],])
        }
        if(AGE$season_idx[j]==3){
          log_N_temp     <-       log(sum(exp(log_N_pred[j-1,]))) +  
                                  epsilon.region$MEAN[epsilon.region$ocean.region == smolt.prod$region[i] & 
                                      epsilon.region$model.time == j-1] +
                                  log(origin_loc[AGE$season_idx[j],smolt.prod$origin.idx[i],]) -
                                  AGE$M2_est[j] * 0.5   -
                                  F_TOT_MAT[j,] * 0.5 ;
          log_N_begin[j,]   <-    log(sum(exp(log_N_pred[j-1,]))) + 
                                  epsilon.region$MEAN[epsilon.region$ocean.region == smolt.prod$region[i] & 
                                      epsilon.region$model.time == j-1] +
                                  log(origin_loc[AGE$season_idx[j],smolt.prod$origin.idx[i],])
          D_pred[i,AGE$spawn_time[j]] <- sum(exp((log_N_temp +  log(prob_age_year[smolt.prod$loc.spawn.idx[i], AGE$spawn_time[j]])) * river_entry_proj[i,])) ;
          log_N_temp     <-   log_N_temp +
                                    log(1- prob_age_year[smolt.prod$loc.spawn.idx[i], AGE$spawn_time[j]]* river_entry_proj[i,]) -
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
  }  # End Release location loop  
} # End Fishing mortality loop


THESE <- grep("X",colnames(ALL.SIM))
colnames(ALL.SIM)[THESE] <- as.character(LOCATIONS$location.name)
ALL.SIM$TOTAL <- rowSums(ALL.SIM[,THESE])

ALL.LONG <- melt(ALL.SIM,id.vars = c("ocean.region","F_scen","time","age.month","age.year","season","juv.mort"))
colnames(ALL.LONG)[colnames(ALL.LONG)=="variable"] <- "loc"

ALL.LONG$value <- ALL.LONG$value

OUT.TOT    <- ALL.LONG[ALL.LONG$loc=="TOTAL",]
OUT.LONG   <- ALL.LONG[ALL.LONG$loc!="TOTAL",]

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
OUT.OLD <- aggregate(OUT.LONG$value[OUT.LONG$age.year>=AGE_temp],
                     by=list(loc=OUT.LONG$loc[OUT.LONG$age.year>=AGE_temp],
                             season=OUT.LONG$season[OUT.LONG$age.year>=AGE_temp],
                             F_scen=OUT.LONG$F_scen[OUT.LONG$age.year>=AGE_temp]),sum) 
OUT.OLD <- data.frame(OUT.OLD)
colnames(OUT.OLD)[4] <- "chin"

OUT.OLD.REG <- aggregate(OUT.LONG$value[OUT.LONG$age.year>=AGE_temp],
                         by=list(loc=OUT.LONG$loc[OUT.LONG$age.year>=AGE_temp],
                                 season=OUT.LONG$season[OUT.LONG$age.year>=AGE_temp],
                                 region=OUT.LONG$ocean.region[OUT.LONG$age.year>=AGE_temp],
                                 F_scen=OUT.LONG$F_scen[OUT.LONG$age.year>=AGE_temp]),sum) 
OUT.OLD.REG <- data.frame(OUT.OLD.REG)
colnames(OUT.OLD.REG)[5] <- "chin"

OUT.OLD.DENS<- merge(OUT.OLD.REG,region.area,by.x="loc",by.y="location.name")
OUT.OLD.DENS$dens <- OUT.OLD.DENS$chin / OUT.OLD.DENS$km2.10.200m

### Subset out the relevant Fishing scenario.
OUT.OLD.REG <- OUT.OLD.REG[OUT.OLD.REG$F_scen == F_scen_name,]
OUT.OLD.DENS <- OUT.OLD.DENS[OUT.OLD.DENS$F_scen == F_scen_name,]

### Calculate Evenness for each region and each season
total.by.reg <- aggregate(OUT.OLD.REG$chin,by=list(loc=OUT.OLD.REG$loc,season=OUT.OLD.REG$season),sum)
colnames(total.by.reg)[3] <- "TOT"
total.by.reg <- merge(total.by.reg,region.area,by.x="loc",by.y="location.name")
total.by.reg$DENS <- total.by.reg$TOT / total.by.reg$km2.10.200m
total.by.reg$TOT <- total.by.reg$TOT /1000
total.by.reg$loc.name <- total.by.reg$loc
total.by.reg$loc         <- LOCATIONS$location.number[match(total.by.reg$loc.name,LOCATIONS$location.name)]

# OUT.EVEN <- merge(OUT.OLD.REG,total.by.reg)
# OUT.EVEN$prop <- OUT.EVEN$chin / OUT.EVEN$TOT
# even.plot <- aggregate(OUT.EVEN$prop,by=list(loc=OUT.EVEN$loc,season=OUT.EVEN$season),gini.simp.div)
# colnames(even.plot)[3] <- "gini.simp"

OUT.OLD.REG$chin     <- OUT.OLD.REG$chin / 1000
OUT.OLD.REG$loc.name <- OUT.OLD.REG$loc
OUT.OLD.REG$loc         <- LOCATIONS$location.number[match(OUT.OLD.REG$loc.name,LOCATIONS$location.name)]
OUT.OLD.REG$origin.numb <- spawn_loc$number[match(OUT.OLD.REG$region,spawn_loc$ocean.region)]

OUT.OLD.DENS$loc.name <- OUT.OLD.DENS$loc
OUT.OLD.DENS$loc <- LOCATIONS$location.number[match(OUT.OLD.DENS$loc.name,LOCATIONS$location.name)]
OUT.OLD.DENS$origin.numb <- spawn_loc$number[match(OUT.OLD.DENS$region,spawn_loc$ocean.region)]

OUT.OLD.REG <- OUT.OLD.REG[order(OUT.OLD.REG$origin.numb),]
OUT.OLD.DENS <- OUT.OLD.DENS[order(OUT.OLD.DENS$origin.numb),]

# Proportional contribution
q1 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="Spr",],  aes(x = loc, y = chin, fill = region)) + 
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

OUT.OLD.REG$region <-  factor(OUT.OLD.REG$region, 
                              levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                         "NOR","COR","SOR","NCA","SFB"))
OUT.OLD.DENS$region <-  factor(OUT.OLD.DENS$region, 
                               levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                          "NOR","COR","SOR","NCA","SFB"))

# p + scale_colour_manual(values = COLS)
# scale_fill_brewer(palette = 12)

# Proportional contribution
q1 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="Spr",],  aes(x = loc, y = chin, fill = region )) + 
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
q2 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = region )) + 
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
q3 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="Fall",],  aes(x = loc, y = chin, fill = region )) + 
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

if(age.name==2){LIM=c(0,6000)}
if(age.name==3){LIM=c(0,3000)}
if(age.name==4){LIM=c(0,1000)}
p1 <- ggplot() + 
  #geom_bar(data=OUT.OLD.REG[OUT.OLD.REG$season=="spring",],  aes(x = loc, y = chin, fill = region )) +
  geom_bar(data=total.by.reg[total.by.reg$season == "Spr",],  aes(x = loc, y = TOT ),fill=grey(0.5),stat = "identity") +
  #geom_line(data=total.by.reg[total.by.reg$season == "spring",],  aes(x = loc, y = TOT ),color= "black") +
  coord_flip() +
  #labs(title = "New plot title")
  labs(x = "",y="",title="e)") + 
  xlab(NULL) +#, y = paste("Chinook, age ",age.name,"+ (thousands)",sep="")) +
  scale_x_continuous(expand=c(0.0,0),breaks = 1:nrow(LOCATIONS), labels= rep("",nrow(LOCATIONS))) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
  scale_y_continuous(expand=c(0,0),limits=LIM) +
  scale_fill_manual(values=COLS,name="Origin") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,color="white",size=rel(0.9)),legend.position="none",
        panel.border=element_rect(colour="black",size=1.5),
        plot.margin=unit(c(0.1, 4.62, 0.05, 0.01), "lines")) 
p1

p2 <- ggplot() + 
  geom_bar(data=OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = region ),stat = "identity") +
  geom_bar(data=total.by.reg[total.by.reg$season == "Sum",],  aes(x = loc, y = TOT ),fill=grey(0.5),stat = "identity") +
  #geom_line(data=total.by.reg[total.by.reg$season == "summer",],  aes(x = loc, y = TOT ),color= "black") +
  coord_flip() +
  #labs(title = "New plot title")
  labs(x = "",y="",title="e)") + 
  xlab(NULL) +#, y = paste("Chinook, age ",age.name,"+ (thousands)",sep="")) +
  scale_x_continuous(expand=c(0.0,0),breaks = 1:nrow(LOCATIONS), labels= rep("",nrow(LOCATIONS))) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
  scale_y_continuous(expand=c(0,0),limits=LIM) +
  #  ggtitle("e)") +
  scale_fill_manual(values=COLS,name="Origin") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,color="white",size=rel(0.9)),
        panel.border=element_rect(colour="black",size=1.5),
        plot.margin=unit(c(0.1, 0.0, 0.05, 0.01), "lines"),
        legend.key.size = unit(0.4, "cm")) 
#p2  

p3 <- ggplot() + 
  #geom_area(data=OUT.OLD.REG[OUT.OLD.REG$season=="fall",],  aes(x = loc, y = chin, fill = region )) +
  geom_bar(data=total.by.reg[total.by.reg$season == "Fall",],  aes(x = loc, y = TOT ),fill=grey(0.5),stat="identity") +
  #geom_line(data=total.by.reg[total.by.reg$season == "fall",],  aes(x = loc, y = TOT ),color= "black") +
  coord_flip() +
  labs(x = "", y = paste("Chinook, age ",age.name,"+ (1000s)",sep=""),title="f)") +
  xlab(NULL) +
  scale_x_continuous(expand=c(0.0,0),breaks = 1:nrow(LOCATIONS), labels= rep("",nrow(LOCATIONS))) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
  scale_y_continuous(expand=c(0,0),limits=LIM) +
  scale_fill_manual(values=COLS,name="Origin") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),legend.position="none",
        panel.border=element_rect(colour="black",size=1.5),
        plot.margin=unit(c(0.1, 4.62, 0.05, 0.01), "lines")) 
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

quartz(file=paste("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/Mixed Model/Abundance plot ",age.name,"+ ; F=",F_scen_name,".pdf",sep=""),dpi=600,height=8,width=7,type="pdf")   
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
# Same plot but with densities instead of total densities.  [ this is close to working but not quite done.... can't figure quite out why.]
#######################
######################
#######################
######################
#######################
######################

# p + scale_colour_manual(values = COLS)
# scale_fill_brewer(palette = 12)

# Proportional contribution
# q1 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="Spr",],  aes(x = loc, y = chin, fill = region )) + 
#   geom_bar(position = "fill",stat = "identity") + 
#   coord_flip() +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(x = "", y = "",title="a) Spring") +
#   scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
#   scale_fill_manual(values=COLS,name="Origin") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
#         plot.margin=unit(c(0.1, 0.05, 0.05, 0.01), "lines")) 
# q1
# 
# q2 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="Sum",],  aes(x = loc, y = chin, fill = region )) + 
#   geom_bar(position = "fill",stat = "identity") + 
#   coord_flip() +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(x = "", y = "",title="b) Summer") +
#   scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
#   scale_fill_manual(values=COLS,name="Origin") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
#         plot.margin=unit(c(0.1, 0.05, 0.05, 0.01), "lines")) 
# 
# q3 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="Fall",],  aes(x = loc, y = chin, fill = region )) + 
#   geom_bar(position = "fill",stat = "identity") + 
#   coord_flip() +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(x = "", y = paste("Proportion",sep=""),title="c) Fall") +
#   scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
#   scale_fill_manual(values=COLS,name="Origin") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
#         plot.margin=unit(c(0.1, 0.05, 0.05, 0.01), "lines")) 
# 
# if(age.name==3){LIM=c(0,25)}
# if(age.name==4){LIM=c(0,15)}
# p1 <- ggplot() + 
#     #geom_bar(data=OUT.OLD.DENS[OUT.OLD.DENS$season=="Spr",],  aes(x = loc, y = dens), fill = grey(0.5),stat="identity" ) +
# 
#   geom_bar(data=total.by.reg[total.by.reg$season == "Spr",],  aes(x = loc, y = DENS ),fill=grey(0.5),stat="identity") +
#   coord_flip() +
#   #labs(title = "New plot title")
#   labs(x = "",y="",title="e)") + #, y = paste("Chinook, age ",age.name,"+ (thousands)",sep="")) +
#   xlab(NULL) +
#   #scale_x_continuous(breaks = 1:nrow(LOCATIONS),expand=c(0,0), labels= rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
#   scale_x_continuous(expand=c(0.0,0),breaks = 1:nrow(LOCATIONS), labels= rep("",nrow(LOCATIONS))) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
#   
#   scale_y_continuous(expand=c(0,0),limits=LIM) +
#   #  ggtitle("e)") +
#   scale_fill_manual(values=COLS,name="Origin") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,color="white",size=rel(0.9)),legend.position="none",
#         panel.border=element_rect(colour="black",size=1.5),
#         plot.margin=unit(c(0.1, 5, 0.05, 0.01), "lines")) 
# p1
# 
# p2 <- ggplot() + 
#   geom_bar(data=OUT.OLD.DENS[OUT.OLD.DENS$season=="Sum",],  aes(x = loc, y = dens, fill = region ),stat="identity") +
#   #geom_line(data=total.by.reg[total.by.reg$season == "summer",],  aes(x = loc, y = DENS ),color= "black") +
#   geom_bar(data=total.by.reg[total.by.reg$season == "Sum",],  aes(x = loc, y = DENS ),fill=grey(0.5),stat="identity") +
#   coord_flip() +
#   #labs(title = "New plot title")
#   labs(x = "",y="",title="e)") + 
#   xlab(NULL) +#, y = paste("Chinook, age ",age.name,"+ (thousands)",sep="")) +
#   scale_x_continuous(expand=c(0.0,0),breaks = 1:nrow(LOCATIONS), labels= rep("",nrow(LOCATIONS))) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
#   scale_y_continuous(expand=c(0,0),limits=LIM) +
#   #  ggtitle("e)") +
#   scale_fill_manual(values=COLS,name="Origin") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,color="white",size=rel(0.9)),
#         panel.border=element_rect(colour="black",size=1.5),
#         plot.margin=unit(c(0.1, 0.0, 0.05, 0.01), "lines"),
#         legend.key.size = unit(0.4, "cm")) 
# p2  
# 
# p3 <- ggplot() + 
#   geom_bar(data=total.by.reg[total.by.reg$season == "Fall",],  aes(x = loc, y = DENS ),fill=grey(0.5),stat="identity") +
#   #geom_line(data=total.by.reg[total.by.reg$season == "fall",],  aes(x = loc, y = DENS ),color= "black") +
#   coord_flip() +
#   labs(x = "", y = paste("Chinook density (#/km2)",sep=""),title="f)") +
#   xlab(NULL) +
#   scale_x_continuous(expand=c(0.0,0),breaks = 1:nrow(LOCATIONS), labels= rep("",nrow(LOCATIONS))) + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
#   scale_y_continuous(expand=c(0,0),limits=LIM) +
#   scale_fill_manual(values=COLS,name="Origin") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),legend.position="none",
#         panel.border=element_rect(colour="black",size=1.5),
#         plot.margin=unit(c(0.1, 5, 0.05, 0.01), "lines")) 
# p3
# ######
# 
# quartz(file=paste("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/Mixed Model/Chinook Density plot ",age.name,"+.pdf",sep=""),dpi=600,height=8,width=7,type="pdf")   
# Layout= matrix(c(4,4,4,4,1,1,1,5,5,5,5,2,2,2,6,6,6,6,3,3,3),nrow=3,ncol=7,byrow=T)
# QQ <- list(p1,p2,p3,q1,q2,q3)
# multiplot(plotlist=QQ ,layout= Layout)
# dev.off()












































# 
# 
# 
# for(i in 1:nrow(sim.region)){
#   finger   <- rep(0,nrow(sim.dat))
#   yearling <- rep(0,nrow(sim.dat))
#   if(sim.region$finger[i] > 0){
#     for(j in 1:nrow(sim.dat)){
#         finger[j] <- 
#           log(sim.region$finger[i] * 1e6) - 
#           cum_M2[sim.dat$age.month[j]] -  # fixed mortality for adults.
#           sim.region$juv.mort[i] * sim.region$n.mon.fing[i] +               # Initial period before fish are observed                   
#             # log_q_troll +                                                 # effort offset
#             # log_q_rec * effort_idx_rec[i] +                               # effort offset
#             # log_q_treaty * effort_idx_treaty[i] +                         # effort offset
#             # log_effort[i] +
#           log(origin_loc[sim.dat$season.idx[j],sim.region$origin.idx[i],sim.dat$loc[j]]) +     # dispersion in the ocean term (sum to 1)                   
#           log(1 - sum_prob[sim.region$loc.spawn.idx[i],sim.dat$age.year[j]]) 
#             # log(vuln_mat[vuln_idx[i],age_vuln_idx[i]]) ; 
#     }
#   }
#   if(sim.region$yearling[i] > 0){
#     for(j in 1:nrow(sim.dat)){
#       yearling[j] <- 
#         log(sim.region$yearling[i] * 1e6) - 
#         cum_M2[sim.dat$age.month[j]] -  # fixed mortality for adults.
#         sim.region$juv.mort[i] * sim.region$n.mon.year[i] +               # Initial period before fish are observed                   
#         # log_q_troll +                                                 # effort offset
#         # log_q_rec * effort_idx_rec[i] +                               # effort offset
#         # log_q_treaty * effort_idx_treaty[i] +                         # effort offset
#         # log_effort[i] +
#         log(origin_loc[sim.dat$season.idx[j],sim.region$origin.idx[i],sim.dat$loc[j]]) +     # dispersion in the ocean term (sum to 1)                   
#         log(1 - sum_prob[sim.region$loc.spawn.idx[i],sim.dat$age.year[j]]) 
#       # log(vuln_mat[vuln_idx[i],age_vuln_idx[i]]) ; 
#     }
#   }
#   OUT[,paste(sim.region$region[i])] <- exp(finger) + exp(yearling)
#   
# }
# 
# THESE <- match(sim.region$region,colnames(OUT))
# OUT[,THESE] <- OUT[,THESE] / 1000
# OUT$TOTAL   <- rowSums(OUT[,THESE])
# 
# ##################################################################################################################
# ##################################################################################################################
# 
# summer <- OUT[OUT$season.name =="summer",]
# fall   <- OUT[OUT$season.name =="fall",]
# winter <- OUT[OUT$season.name =="winter",]
# spring <- OUT[OUT$season.name =="spring",]
# 
# temp <- summer[summer$age.year == 5,]
# plot(TOTAL~loc,data=temp,type="l")
# 
# temp <- fall[fall$age.year == 4,]
# plot(TOTAL~loc,data=temp,type="l")
# 
# temp <- spring[spring$age.year == 4,]
# plot(TOTAL~loc,data=temp,type="l")

OUT.LONG <- melt(OUT,id=c("loc","season.name","age.year","age.month","season.idx"))
OUT.TOT  <- OUT.LONG[OUT.LONG$variable == "TOTAL",]
OUT.LONG <- OUT.LONG[OUT.LONG$variable != "TOTAL",]

#### MAKE CUMULATIVE DISTRIBUTION FOR ALL AREAS BASED ON MEAN SURVIVAL FOR EACH REGION, MEAN DISTRIBUTION, MEAN EVERYTHING.
age <- c(1,2,3,4)
for(i in 1:length(age)){
  AGE <- age[i]
  age.name <- AGE + 1
  OUT.OLD <- aggregate(OUT.TOT$value[OUT.TOT$age.year>=AGE],
                     by=list(loc=OUT.TOT$loc[OUT.TOT$age.year>=AGE],
                             season=OUT.TOT$season.name[OUT.TOT$age.year>=AGE]),sum) 
  OUT.OLD <- data.frame(OUT.OLD)
  colnames(OUT.OLD)[3] <- "chin"

  OUT.OLD.REG <- aggregate(OUT.LONG$value[OUT.LONG$age.year>=AGE],
                         by=list(loc=OUT.LONG$loc[OUT.LONG$age.year>=AGE],
                                 season=OUT.LONG$season.name[OUT.LONG$age.year>=AGE],
                                 region=OUT.LONG$variable[OUT.LONG$age.year>=AGE]),sum) 
  OUT.OLD.REG <- data.frame(OUT.OLD.REG)
  colnames(OUT.OLD.REG)[4] <- "chin"
  
  ### Calculate Evenness for each region and each season
  
  total.by.reg <- aggregate(OUT.OLD.REG$chin,by=list(loc=OUT.OLD.REG$loc,season=OUT.OLD.REG$season),sum)
  colnames(total.by.reg)[3] <- "TOT"
  OUT.EVEN <- merge(OUT.OLD.REG,total.by.reg)
  OUT.EVEN$prop <- OUT.EVEN$chin / OUT.EVEN$TOT
  
  even.plot <- aggregate(OUT.EVEN$prop,by=list(loc=OUT.EVEN$loc,season=OUT.EVEN$season),gini.simp.div)
  colnames(even.plot)[3] <- "gini.simp"

  
  
  
############################
  ### plots start here
############################
  
  # Plots
  p1 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="spring",], aes(x = loc, y = chin, fill = region)) + 
          geom_area() +
          labs(x = "", y = paste("Chinook, age ",age.name,"+ (thousands)",sep="")) +
          scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) +
          ggtitle("Spring Distribution") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
          scale_fill_discrete(name="Origin")


  p2 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="summer",], aes(x = loc, y = chin, fill = region)) + 
          geom_area() +
          labs(x = "", y = paste("Chinook, age ",age.name,"+ (thousands)",sep="")) +
          scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) +
          ggtitle("Summer Distribution") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+
          scale_fill_discrete(name="Origin")


  p3 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="fall",], aes(x = loc, y = chin, fill = region)) + 
          geom_area() +
          labs(x = "", y = paste("Chinook, age ",age.name,"+ (thousands)",sep="")) +
          scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) +
          ggtitle("Fall Distribution") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
          scale_fill_discrete(name="Origin")

  ################################################################
  #### MAKE PROPORTIONAL ORIGIN OF FISH IN EACH AREA
  ################################################################
  q1 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="spring",],  aes(x = loc, y = chin, fill = region)) + 
    geom_bar(position = "fill",stat = "identity") + 
    scale_y_continuous() +
    labs(x = "", y = paste("Proportion",sep="")) +
    scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) +
    ggtitle("Spring Distribution") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
    scale_fill_discrete(name="Origin")
  
  q2 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="summer",],  aes(x = loc, y = chin, fill = region)) + 
    geom_bar(position = "fill",stat = "identity") + 
    scale_y_continuous() +
    labs(x = "", y = paste("Proportion",sep="")) +
    scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) +
    ggtitle("Summer Distribution") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
    scale_fill_discrete(name="Origin")
  
  q3 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="fall",],  aes(x = loc, y = chin, fill = region)) + 
    geom_bar(position = "fill",stat = "identity") + 
    scale_y_continuous() +
    labs(x = "", y = paste("Proportion",sep="")) +
    scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) +
    ggtitle("Fall Distribution") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
    scale_fill_discrete(name="Origin")
  
setwd("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/Mixed Model")
pdf(file= paste("Fall Chinook, age ",age.name,"+ abundance in space.pdf",sep=""),width=8,height=4.5)
  print(p1)
  print(p2)
  print(p3)
  print(q1)
  print(q2)
  print(q3)
  
  par(mar=c(4,4,1,1))
  x.lim <- c(1,17)
  y.lim <- c(0,1)
    plot(gini.simp ~ loc, data=even.plot[even.plot$season == "spring",],
          axes=F,type="b",xlim=x.lim,ylim=y.lim,xlab="",ylab="",yaxs="i",lwd=2,pch=21,bg=1)
    par(new=T)      
    plot(gini.simp ~ loc, data=even.plot[even.plot$season == "summer",],
         axes=F,type="b",xlim=x.lim,ylim=y.lim,xlab="",ylab="",col=2,yaxs="i",lwd=2,pch=24,bg=2)
    par(new=T)  
    plot(gini.simp ~ loc, data=even.plot[even.plot$season == "fall",],
         axes=F,type="b",xlim=x.lim,ylim=y.lim,xlab="",ylab="",col=4,yaxs="i",lwd=2,pch=22,bg=4)
    axis(1,1:nrow(LOCATIONS), labels=LOCATIONS$location.name,las=2)
    axis(2,las=2)
    title(ylab= "Gini-Simpson index")
    box(bty="o",lwd=2)
    title("Run Diversity in each ocean region")
    
    legend(x=14,y=0.4,legend=c("Spring","Summer","Fall"),col=c(1,2,4),pch=c(21,24,22),lwd=2,pt.bg=c(1,2,4),bty="n")
  
dev.off()
} 

################################################################
################################################################
################################################################
################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
 # Repeat above but use single, coast wide average juv mort rate
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

#############################################################################################
##############################################################################################
#############################################################################################
### PROJECT SPATIAL DISTRIBUTION FOR EACH ORIGIN
###### Calculate Initial Monthly mortality rate


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

for(i in 1:nrow(sim.region)){
  finger   <- rep(0,nrow(sim.dat))
  yearling <- rep(0,nrow(sim.dat))
  if(sim.region$finger[i] > 0){
    for(j in 1:nrow(sim.dat)){
      finger[j] <- 
        log(sim.region$finger[i] * 1e6) - 
        cum_M2[sim.dat$age.month[j]] -  # fixed mortality for adults.
        sim.region$juv.mort.shared[i] * sim.region$n.mon.fing[i] +               # Initial period before fish are observed                   
        # log_q_troll +                                                 # effort offset
        # log_q_rec * effort_idx_rec[i] +                               # effort offset
        # log_q_treaty * effort_idx_treaty[i] +                         # effort offset
        # log_effort[i] +
        log(origin_loc[sim.dat$season.idx[j],sim.region$origin.idx[i],sim.dat$loc[j]]) +     # dispersion in the ocean term (sum to 1)                   
        log(1 - sum_prob[sim.region$loc.spawn.idx[i],sim.dat$age.year[j]]) 
      # log(vuln_mat[vuln_idx[i],age_vuln_idx[i]]) ; 
    }
  }
  if(sim.region$yearling[i] > 0){
    for(j in 1:nrow(sim.dat)){
      yearling[j] <- 
        log(sim.region$yearling[i] * 1e6) - 
        cum_M2[sim.dat$age.month[j]] -  # fixed mortality for adults.
        sim.region$juv.mort.shared[i] * sim.region$n.mon.year[i] +               # Initial period before fish are observed                   
        # log_q_troll +                                                 # effort offset
        # log_q_rec * effort_idx_rec[i] +                               # effort offset
        # log_q_treaty * effort_idx_treaty[i] +                         # effort offset
        # log_effort[i] +
        log(origin_loc[sim.dat$season.idx[j],sim.region$origin.idx[i],sim.dat$loc[j]]) +     # dispersion in the ocean term (sum to 1)                   
        log(1 - sum_prob[sim.region$loc.spawn.idx[i],sim.dat$age.year[j]]) 
      # log(vuln_mat[vuln_idx[i],age_vuln_idx[i]]) ; 
    }
  }
  OUT[,paste(sim.region$region[i])] <- exp(finger) + exp(yearling)
  
}


THESE <- match(sim.region$region,colnames(OUT))
OUT[,THESE] <- OUT[,THESE] / 1000
OUT$TOTAL      <- rowSums(OUT[,THESE])

##################################################################################################################
##################################################################################################################

summer <- OUT[OUT$season.name =="summer",]
fall   <- OUT[OUT$season.name =="fall",]
winter <- OUT[OUT$season.name =="winter",]
spring <- OUT[OUT$season.name =="spring",]

temp <- summer[summer$age.year == 5,]
plot(TOTAL~loc,data=temp,type="l")

temp <- fall[fall$age.year == 4,]
plot(TOTAL~loc,data=temp,type="l")

temp <- spring[spring$age.year == 4,]
plot(TOTAL~loc,data=temp,type="l")


OUT.LONG <- melt(OUT,id=c("loc","season.name","age.year","age.month","season.idx"))

OUT.TOT  <- OUT.LONG[OUT.LONG$variable == "TOTAL",]
OUT.LONG <- OUT.LONG[OUT.LONG$variable != "TOTAL",]

#### MAKE CUMULATIVE DISTRIBUTION FOR ALL AREAS BASED ON MEAN SURVIVAL FOR EACH REGION, MEAN DISTRIBUTION, MEAN EVERYTHING.
age <- c(1,2,3,4)
for(i in 1:length(age)){
  AGE <- age[i]
  age.name <- AGE + 1
  OUT.OLD <- aggregate(OUT.TOT$value[OUT.TOT$age.year>=AGE],
                       by=list(loc=OUT.TOT$loc[OUT.TOT$age.year>=AGE],
                               season=OUT.TOT$season.name[OUT.TOT$age.year>=AGE]),sum) 
  OUT.OLD <- data.frame(OUT.OLD)
  colnames(OUT.OLD)[3] <- "chin"
  
  OUT.OLD.REG <- aggregate(OUT.LONG$value[OUT.LONG$age.year>=AGE],
                           by=list(loc=OUT.LONG$loc[OUT.LONG$age.year>=AGE],
                                   season=OUT.LONG$season.name[OUT.LONG$age.year>=AGE],
                                   region=OUT.LONG$variable[OUT.LONG$age.year>=AGE]),sum) 
  OUT.OLD.REG <- data.frame(OUT.OLD.REG)
  colnames(OUT.OLD.REG)[4] <- "chin"
  
  ### Calculate Evenness for each region and each season
  
  total.by.reg <- aggregate(OUT.OLD.REG$chin,by=list(loc=OUT.OLD.REG$loc,season=OUT.OLD.REG$season),sum)
  colnames(total.by.reg)[3] <- "TOT"
  OUT.EVEN <- merge(OUT.OLD.REG,total.by.reg)
  OUT.EVEN$prop <- OUT.EVEN$chin / OUT.EVEN$TOT
  
  even.plot <- aggregate(OUT.EVEN$prop,by=list(loc=OUT.EVEN$loc,season=OUT.EVEN$season),gini.simp.div)
  colnames(even.plot)[3] <- "gini.simp"
  
  # Start plots
  p1 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="spring",], aes(x = loc, y = chin, fill = region)) + 
    geom_area() +
    labs(x = "", y = paste("Chinook, age ",age.name,"+ (thousands)",sep="")) +
    scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) +
    ggtitle("Spring Distribution") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
    scale_fill_discrete(name="Origin")
  
  
  p2 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="summer",], aes(x = loc, y = chin, fill = region)) + 
    geom_area() +
    labs(x = "", y = paste("Chinook, age ",age.name,"+ (thousands)",sep="")) +
    scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) +
    ggtitle("Summer Distribution") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+
    scale_fill_discrete(name="Origin")
  
  
  p3 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="fall",], aes(x = loc, y = chin, fill = region)) + 
    geom_area() +
    labs(x = "", y = paste("Chinook, age ",age.name,"+ (thousands)",sep="")) +
    scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) +
    ggtitle("Fall Distribution") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
    scale_fill_discrete(name="Origin")
  
  # Proportional contribution
  
  q1 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="spring",],  aes(x = loc, y = chin, fill = region)) + 
    geom_bar(position = "fill",stat = "identity") + 
    scale_y_continuous() +
    labs(x = "", y = paste("Proportion",sep="")) +
    scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) +
    ggtitle("Spring Distribution") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
    scale_fill_discrete(name="Origin")
  
  q2 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="summer",],  aes(x = loc, y = chin, fill = region)) + 
    geom_bar(position = "fill",stat = "identity") + 
    scale_y_continuous() +
    labs(x = "", y = paste("Proportion",sep="")) +
    scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) +
    ggtitle("Summer Distribution") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
    scale_fill_discrete(name="Origin")
  
  q3 <- ggplot(OUT.OLD.REG[OUT.OLD.REG$season=="fall",],  aes(x = loc, y = chin, fill = region)) + 
    geom_bar(position = "fill",stat = "identity") + 
    scale_y_continuous() +
    labs(x = "", y = paste("Proportion",sep="")) +
    scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) +
    ggtitle("Fall Distribution") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
    scale_fill_discrete(name="Origin")
  
  setwd("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/Mixed Model")
  pdf(file= paste("Fall Chinook, age ",age.name,"+ abundance in space (shared mean mortality).pdf",sep=""),width=8,height=4.5)
    print(p1)
    print(p2)
    print(p3)
    print(q1)
    print(q2)
    print(q3)
  
    par(mar=c(4,4,1,1))
    x.lim <- c(1,17)
    y.lim <- c(0,1)
    plot(gini.simp ~ loc, data=even.plot[even.plot$season == "spring",],
         axes=F,type="b",xlim=x.lim,ylim=y.lim,xlab="",ylab="",yaxs="i",lwd=2,pch=21,bg=1)
    par(new=T)      
    plot(gini.simp ~ loc, data=even.plot[even.plot$season == "summer",],
         axes=F,type="b",xlim=x.lim,ylim=y.lim,xlab="",ylab="",col=2,yaxs="i",lwd=2,pch=24,bg=2)
    par(new=T)  
    plot(gini.simp ~ loc, data=even.plot[even.plot$season == "fall",],
         axes=F,type="b",xlim=x.lim,ylim=y.lim,xlab="",ylab="",col=4,yaxs="i",lwd=2,pch=22,bg=4)
    axis(1,1:nrow(LOCATIONS), labels=LOCATIONS$location.name,las=2)
    axis(2,las=2)
    title(ylab= "Gini-Simpson index")
    title("Run Diversity in each ocean region")
    box(bty="o",lwd=2)
    legend(x=14,y=0.4,legend=c("Spring","Summer","Fall"),col=c(1,2,4),pch=c(21,24,22),lwd=2,pt.bg=c(1,2,4),bty="n")
  
  dev.off()
} 

#################################################################
#################################################################
#################################################################
#################################################################
# PROPAGATE UNCERTAINTY FOR PARAMETERS TO LOOK AT AMONG YEAR VARIATION FOR EACH ORIGIN AREA
  # USE FIXED NUMBERS OF RELEASES FOR EACH AREA, THOUGH

N.rep    <- 1000
N.MCMC <- dim(samp$log_q_troll)
THIS <- sample(1:N.MCMC,N.rep)

all.site <- list()
all.site.summary <- list()

# Cycle across origin regions:
for(i in 1:nrow(sim.region)){
  SITE     <- sim.region$region[i]
  TEMP.OUT <- matrix(0,nrow(sim.dat),N.rep)
  
  # pick a juvenile mortality
  nom_reg <- nom[nom$ocean.reg == sim.region$region[i],]
  if(sim.region$region[i] == "SOR" | sim.region$region[i] == "COR" | sim.region$region[i] == "NOR"){ nom_reg <- nom[nom$ocean.reg == "OR",]}
  if(sim.region$region[i] == "SWVI" | sim.region$region[i] == "NWVI"){ nom_reg <- nom[nom$ocean.reg == "VI",]}
  
  for(j in 1:N.rep){
    finger   <- rep(0,nrow(sim.dat))
    yearling <- rep(0,nrow(sim.dat))

    # Juvenile mortality  
    juv.idx   <- sample(nom_reg$year.reg.idx,1)
    juv.mort  <- samp$rel_year_all[THIS[j], juv.idx]
    
        if(sim.region$finger[i] > 0){
          for(k in 1:nrow(sim.dat)){
            finger[k] <- 
              log(sim.region$finger[i] * 1e6) - 
              cum_M2[sim.dat$age.month[k]] -  # fixed mortality for adults.
              juv.mort * sim.region$n.mon.fing[i] +               # Initial period before fish are observed                   
              # log_q_troll +                                                 # effort offset
              # log_q_rec * effort_idx_rec[i] +                               # effort offset
              # log_q_treaty * effort_idx_treaty[i] +                         # effort offset
              # log_effort[i] +
              log(samp$origin_loc[THIS[j],sim.dat$season.idx[k],sim.region$origin.idx[i],sim.dat$loc[k]]) +     # dispersion in the ocean term (sum to 1)
              log(1 - samp$sum_prob[THIS[j],sim.region$loc.spawn.idx[i],sim.dat$age.year[k]]) 
            # log(vuln_mat[vuln_idx[i],age_vuln_idx[i]]) ; 
          }
        }
        if(sim.region$yearling[i] > 0){
          for(k in 1:nrow(sim.dat)){
            yearling[k] <- 
              log(sim.region$yearling[i] * 1e6) - 
              cum_M2[sim.dat$age.month[k]] -  # fixed mortality for adults.
              juv.mort * sim.region$n.mon.fing[i] +               # Initial period before fish are observed                   
              # log_q_troll +                                                 # effort offset
              # log_q_rec * effort_idx_rec[i] +                               # effort offset
              # log_q_treaty * effort_idx_treaty[i] +                         # effort offset
              # log_effort[i] +
              log(samp$origin_loc[THIS[j],sim.dat$season.idx[k],sim.region$origin.idx[i],sim.dat$loc[k]]) +     # dispersion in the ocean term (sum to 1)
              log(1 - samp$sum_prob[THIS[j],sim.region$loc.spawn.idx[i],sim.dat$age.year[k]]) 
            # log(vuln_mat[vuln_idx[i],age_vuln_idx[i]]) ; 
          }
        }
        TEMP.OUT[,j] <- exp(finger) + exp(yearling)
      }
  all.site[[SITE]] <- data.frame(cbind(sim.dat,TEMP.OUT))
  temp <- data.frame(MEAN=apply(TEMP.OUT,1,mean),
                     SD=apply(TEMP.OUT,1,sd),
                     q=t(as.matrix(apply(TEMP.OUT,1,quantile,probs=c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975)))))
  colnames(temp)[3:ncol(temp)] <- c("q.025","q.05","q.10","q.25","q.50","q.75","q.90","q.95","q.975")
  all.site.summary[[SITE]] <- data.frame(cbind(sim.dat,temp))
}
      
##################################################################################################      
 # Calculate the total abundance      
THESE <-  grep("X",colnames(all.site[[1]]))

for(i in 1:length(names(all.site))){      
  if(i==1){ all.site.sum <- all.site[[i]][,THESE]}
  all.site.sum <- all.site.sum + all.site[[i]][,THESE]
}
    
  total.chin <- data.frame(MEAN=apply(all.site.sum,1,mean),
                           SD=apply(all.site.sum,1,sd),
                           q=t(as.matrix(apply(all.site.sum,1,quantile,probs=c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975)))))
  colnames(total.chin)[3:ncol(total.chin)] <- c("q.025","q.05","q.10","q.25","q.50","q.75","q.90","q.95","q.975")
  total.chin <-  data.frame(cbind(sim.dat,total.chin))
  total.chin$CV <- total.chin$SD / total.chin$MEAN    
      
### Plot functions Mean and Variability of Total Chinook Abundance
plot.mean <- function(temp,TITLE.REG,LOCATIONS,AGE,SEASON,i,j,k){
    par(mar=c(4,5,2,1))
    x.lim <- c(1,max(temp$loc))
    y.lim <- c(0,max(temp$q.95)/1000)

    a <- temp[,c("loc","q.05")]; colnames(a)[2] <- "y"
    b <- temp[,c("loc","q.95")]; colnames(b)[2] <- "y"
    b <- b[order(b$loc,decreasing=T),]
    a <- rbind(a,b)
    a$y <- a$y / 1000
      
    plot(MEAN/1000 ~loc,data=temp,xlim=x.lim,ylim=y.lim,type="l",axes=F,xlab="",ylab="", yaxs="i")
    par(new=T)
    polygon(x=a$loc,y=a$y,col=grey(0.7),border=F)
    par(new=T)
    plot(MEAN/1000 ~loc,data=temp,xlim=x.lim,ylim=y.lim,type="l",axes=F,xlab="",ylab="",lwd=2, yaxs="i")

    axis(1,las=2,at=1:max(temp$loc),LOCATIONS$location.name)
    axis(2,las=2)
    box(bty="o",lwd=2)

    title(ylab=paste("Chinook (thousands)"))
    title(main=paste(TITLE.REG,";age",AGE[j]+1,SEASON[i],"distribution"))
}
  
plot.cv <- function(temp,TITLE.REG,LOCATIONS,AGE,SEASON,i,j,k){
  temp$CV <- temp$SD / temp$MEAN
  par(mar=c(4,5,2,1))
  x.lim <- c(1,max(temp$loc))
  y.lim <- c(0,round(max(temp$CV)*1.1,1))
  
  a <- temp[,c("loc","q.05")]; colnames(a)[2] <- "y"
  b <- temp[,c("loc","q.95")]; colnames(b)[2] <- "y"
  b <- b[order(b$loc,decreasing=T),]
  a <- rbind(a,b)
  a$y <- a$y / 1000
  
  plot(CV ~loc,data=temp,xlim=x.lim,ylim=y.lim,type="l",axes=F,xlab="",ylab="", yaxs="i")
  #par(new=T)
  # polygon(x=a$loc,y=a$y,col=grey(0.7),border=F)
  # par(new=T)
  #plot(MEAN/1000 ~loc,data=temp,xlim=x.lim,ylim=y.lim,type="l",axes=F,xlab="",ylab="",lwd=2, yaxs="i")
  axis(1,las=2,at=1:max(temp$loc),LOCATIONS$location.name)
  axis(2,las=2)
  box(bty="o",lwd=2)
  
  title(ylab=paste("Chinook CV"))
  title(main=paste(TITLE.REG,";age",AGE[j]+1,SEASON[i],"distribution"))
}

##################################################################################
########### MAKE PLOTS OF ABUNDANCE AND CV
##################################################################################
##### EACH REGION
season.name <- c("spring","summer","fall")
age.name    <- c(3)

pdf("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/Mixed Model/Mean_CV_by_region.pdf",onefile = T,height=3,width=8.5)
par(mfcol=c(1,3))
for(k in 1:nrow(sim.region)){
    for(i in 1:length(season.name)){
      for(j in 1:length(age.name)){
        temp <- all.site.summary[[sim.region$region[k]]]
        temp <- temp[temp$season.name == season.name[i] & temp$age.year == age.name[j],]
        plot.mean(temp,sim.region$region[k],LOCATIONS,age.name,season.name,i,j,k)
      }
    }
    for(i in 1:length(season.name)){
      for(j in 1:length(AGE)){
        temp <- all.site.summary[[sim.region$region[k]]]
        temp <- temp[temp$season.name == season.name[i] & temp$age.year == age.name[j],]
        plot.cv(temp,sim.region$region[k],LOCATIONS,age.name,season.name,i,j,k)
      }
    }
}
dev.off()
      
######## TOTAL ABUNDANCE 
AGE    <- c(2,3,4)

pdf("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/Mixed Model/Mean_CV_Sum.pdf",onefile = T,height=3,width=8.5)
par(mfcol=c(1,3))
  for(i in 1:length(season.name)){
    for(j in 1:length(age.name)){
      temp <- total.chin
      temp <- temp[temp$season.name == season.name[i] & temp$age.year == AGE[j],]
      plot.mean(temp,"Total Chinook",LOCATIONS,age.name,season.name,i,j,k)
    }
  }
  for(i in 1:length(season.name)){
    for(j in 1:length(age.name)){
      temp <- total.chin
      temp <- temp[temp$season.name == season.name[i] & temp$age.year == age.name[j],]
      plot.cv(temp,"Total Chinook",LOCATIONS,age.name,season.name,i,j,k)
    }
  }

dev.off()
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
#####