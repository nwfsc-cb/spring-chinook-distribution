#### # MAKING SALMON DISTRIBUTION SURFACES
library(ggplot2)
library(reshape2)
library(dplyr)

# READ IN POSTERIOR FILE FROM MODEL FIT OF INTEREST:
base.dir    <- "/Users/ole.shelton/GitHub"
results.dir <- "/Users/ole.shelton/GitHub/Orca_Salmon/Output files/_Mixed Results"
code.dir    <- "/Users/ole.shelton/GitHub/Orca_Salmon_Code/Mixed model post processing"


setwd(results.dir)
load("Binomial+Positive_output-1978-90 Troll_Rec_Treaty_6year_STATE_SPACE_E50_M2EST_originP0_vuln1_90BIG_FIN_10-27-2016.RData")
#load("Binomial+Positive_output-1978-90 Troll_Rec_Treaty_6year_STATE_SPACE_E25_M2EST_originP0_vuln1_90FIN.RData")
LOCATIONS <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/locations.csv",sep=""))

setwd(code.dir)
source("Basic Data Extraction.R")
source("multiplot.r")

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
#DEFINE SCENARIO SIMULATION Options are: BASE & HATCHERY
SCEN.SIM <- "BASE"
## Sample N.SAMP of the total number of MCMC draws available
N.SAMP <- 300
# Calculate number of outmigrant smolt from various regions....
smolt.dat <- read.csv("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Total Smolt and Run Info/smolt_data_10-2016.csv")

# smolt.mod <- smolt.dat[smolt.dat$type == "independent",]
# smolt.mod$total.wild.hatch <- smolt.mod$total.releases.median * (1+smolt.mod$frac.wild.missing) 
# smolt.mod$finger <- smolt.mod$total.wild.hatch * (1 - smolt.mod$frac.yearling )
# smolt.mod$yearling <- smolt.mod$total.wild.hatch * smolt.mod$frac.yearling
# smolt.prod <- aggregate(smolt.mod[,c("finger","yearling")],
#                         by=list(number=smolt.mod$location.number,region=smolt.mod$rel.region,
#                                 n.mon.fing = smolt.mod$n.month.finger ,n.mon.year = smolt.mod$n.month.yearling 
#                         ),sum)

smolt.prod <- summarize(group_by(smolt.dat,region,number),Sum=sum(total),Hatch=sum(hatchery,na.rm = T),Wild=sum(wild,na.rm = T))
smolt.prod <- smolt.prod[order(smolt.prod$number),]
smolt.prod$half.hatch <- smolt.prod$Wild + smolt.prod$Hatch/2

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
rel_yr <- data.frame(rel_id = 1:length(unique(dat.bin$release_year)),rel_year = sort(unique(dat.bin$release_year)))

nom.all <- sort(unique(dat.bin$year.reg))
THESE   <- match(nom.all,dat.bin$year.reg)
nom     <- data.frame( dat.bin[THESE,c("year.reg.idx","ocean.reg","release_year","loc.spawn.idx")],nom = nom.all)

#nom$ocean.reg[nom$ocean.reg == "NWVI" | nom$ocean.reg == "SWVI"] <- "VI"
#nom$ocean.reg[nom$ocean.reg == "SOR" | nom$ocean.reg == "COR" |nom$ocean.reg == "NOR"] <- "OR"
nom <- nom[order(nom$loc.spawn.idx),]

dat.start <- matrix(-99,length(unique(spawn_loc$init.loc)),nrow(rel_yr))
colnames(dat.start) <- rel_yr$rel_year
rownames(dat.start) <- unique(nom$ocean.reg)
dat.mort.mean <- dat.start
dat.mort.sd   <- dat.start

for(i in 1:max(dat.bin$year.reg.idx)){
  X <- which(rownames(dat.start)==nom$ocean.reg[i]) 
  Y <- which(colnames(dat.start)==nom$release_year[i]) 
  these.col <- which( REL$ocean.region == nom$ocean.reg[i] & REL$release_year == nom$release_year[i])
  if(length(these.col)>1){
    dat.mort.mean[X,Y] <- mean(apply(samp$rel_year_all[,c(these.col)],1,median))
    dat.mort.sd[X,Y]   <- sd(rowMeans(samp$rel_year_all[,c(these.col)]))
  }
  if(length(these.col)==1){
    dat.mort.mean[X,Y] <- mean(samp$rel_year_all[,c(these.col)])
    dat.mort.sd[X,Y]   <- sd(samp$rel_year_all[,c(these.col)])
  }
}

juv.mort <- NULL
for(i in 1:nrow(dat.mort.mean)){
  temp    <- dat.mort.mean[i,]
  juv.mort <- rbind(juv.mort,c(rownames(dat.mort.mean)[i],mean(temp[temp>0])))
}
juv.mort <- data.frame(juv.mort)
colnames(juv.mort) <- c("region","juv.mort")
juv.mort$juv.mort <- as.numeric(as.character(unlist(juv.mort$juv.mort)))
juv.mort$loc.spawn.idx <- 1:nrow(juv.mort)


juv.mort.shared <- median(rel_year_all)

#############################################################################################

sim.region <- aggregate(dat.bin$ocean.reg,
                        by=list(region = dat.bin$ocean.reg,
                                origin.idx = dat.bin$origin.idx,
                                loc.spawn.idx = dat.bin$loc.spawn.idx
                                #season.idx = dat.bin$season.idx
                        ),length)[,1:3]

sim.region <- merge(sim.region,juv.mort[,c("loc.spawn.idx","juv.mort")],all=T,by="loc.spawn.idx")

sim.region <- merge(sim.region,smolt.prod)
sim.region <- sim.region[order(sim.region$number),]
sim.region$juv.mort.shared <- mean(exp(samp$log_rel_year_mu))

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

F_scen <- c("F_median")

############## PROJECTIONS ( USING Median Fishing Mortality )

  ALL.SIM <- NULL
  ALL.SIM.end <- NULL
  mcmc.ZZZ <- round(seq(1,dim(samp$log_q_troll_pos),length.out=N.SAMP),0)

##################
#### SET UP TO MAKE JUVENILE SURVIVAL RANDOM OR FIXED
  JUV.SURV <- "FIXED"
  juv.mort.mean <- log(median(exp(-rel_year_all)))
  juv.mort.sd   <- rel_year_sigma
  
  A <-rlnorm(1e6,juv.mort.mean-0.5*juv.mort.sd^2,juv.mort.sd)
  hist(A)
  mean(A)

if(JUV.SURV == "FIXED"){
  juv.mort <- matrix(juv.mort.mean,nrow(smolt.prod),N.SAMP)
  juv.mort <- - juv.mort
}
if(JUV.SURV == "RAND"){
  temp <- rlnorm(N.SAMP,juv.mort.mean-0.5*juv.mort.sd^2,juv.mort.sd)
  juv.mort <- matrix(temp,nrow(smolt.prod),N.SAMP,byrow = T)
  juv.mort <- -log(juv.mort)
}

####################################################################################################################
for(k in 1:length(F_scen)){
  #if(F_scen[k]=="F_zero"){F_t_temp  <- F_troll_zero_mat; F_r_temp   <- F_rec_zero_mat}
  if(F_scen[k]=="F_median"){F_t_temp  <- F_troll_median_mat; F_r_temp <- F_rec_median_mat}
  #if(F_scen[k]=="F_max"){F_t_temp  <- F_troll_max_mat; F_r_temp    <- F_rec_max_mat}
  F_TROLL_MAT <- NULL
  F_REC_MAT <- NULL
  for(kk in 1:5){
    F_TROLL_MAT <- rbind(F_TROLL_MAT,F_t_temp)
    F_REC_MAT   <- rbind(F_REC_MAT,F_r_temp)
  }
  F_TROLL_MAT <- F_TROLL_MAT[1:N_time_mod,]  
  F_REC_MAT   <- F_REC_MAT[1:N_time_mod,]
  
  for(kkk in 1:ncol(F_TROLL_MAT)){
    F_TROLL_MAT[,kkk] <- F_TROLL_MAT[,kkk] * vuln_mat[vuln_int_idx[kkk],]
    F_REC_MAT[,kkk]   <- F_REC_MAT[,kkk]   * vuln_mat[vuln_int_rec_idx[kkk],]
  }
  
  F_TOT_MAT <- F_TROLL_MAT + F_REC_MAT

#### LOOP OVER MCMC DRAWS.  
  for(ZZZ in mcmc.ZZZ){
  # select a realization of the MCMC
      M2_est_mcmc <- samp$cum_M2_temp[ZZZ,]
      origin_loc_mcmc <- samp$origin_loc[ZZZ,,,]
      prob_age_year_mcmc <- samp$prob_age_year[ZZZ,,]
      juv.mort.mcmc <- juv.mort[,which(mcmc.ZZZ==ZZZ)]
  
  for(i in 1:nrow(smolt.prod)){  # Cycle Across release groups.
    if(SCEN.SIM == "HATCHERY"){START <- smolt.prod$half.hatch[i]}
    if(SCEN.SIM == "BASE"){START <- smolt.prod$Sum[i] 
       # if(smolt.prod$region[i] == "PUSO"){ START <- smolt.prod$Wild}
    }
    ### SPATIALLY VARYING SURVIVAL
    if(smolt.prod$region[i] == "SFB" | smolt.prod$region[i] == "NCA"  ){ 
     surv <- exp(-juv.mort.mcmc[i])
      juv.mort.mcmc[i] <- - log(surv/4)
    }
    
    
    log_N_pred  <- log_N_pred  * 0
    log_N_begin <- log_N_pred
    for(j in 1:N_time_mod){ # Cycle Across model time..
      
      if(j == 1){
        log_N_pred[j,] <-  log(START*1e6  ) - juv.mort.mcmc[i] - M2_est_mcmc[j] +
          log(origin_loc_mcmc[AGE$season_idx[j],smolt.prod$origin.idx[i],]) 
        log_N_begin[j,] <-  log(START*1e6  ) - juv.mort.mcmc[i] +
          log(origin_loc_mcmc[AGE$season_idx[j],smolt.prod$origin.idx[i],]) 
      }
      if(j>1){
        if(spawn_time[j] == 0){
          log_N_pred[j,]   <-       log(sum(exp(log_N_pred[j-1,]))) +  
            log(origin_loc_mcmc[AGE$season_idx[j],smolt.prod$origin.idx[i],]) -
            M2_est_mcmc[j]   -
            F_TOT_MAT[j,]
          log_N_begin[j,]   <-       log(sum(exp(log_N_pred[j-1,]))) +  
            log(origin_loc_mcmc[AGE$season_idx[j],smolt.prod$origin.idx[i],])
        }
        if(AGE$season_idx[j]==3){
          log_N_temp     <-    log(sum(exp(log_N_pred[j-1,]))) +  
            log(origin_loc_mcmc[AGE$season_idx[j],smolt.prod$origin.idx[i],]) -
            M2_est_mcmc[j] * 0.5   -
            F_TOT_MAT[j,] * 0.5 ;
          log_N_begin[j,]   <-       log(sum(exp(log_N_pred[j-1,]))) +  
            log(origin_loc_mcmc[AGE$season_idx[j],smolt.prod$origin.idx[i],])
          D_pred[i,AGE$spawn_time[j]] <- sum(exp((log_N_temp +  log(prob_age_year_mcmc[smolt.prod$loc.spawn.idx[i], AGE$spawn_time[j]])) * river_entry_proj[i,])) ;
          log_N_temp     <-   log_N_temp +
            log(1- prob_age_year_mcmc[smolt.prod$loc.spawn.idx[i], AGE$spawn_time[j]]* river_entry_proj[i,]) -
            M2_est_mcmc[j] * 0.5   -
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
                         indicator=ZZZ,
                         exp(log_N_begin))
    ALL.SIM <- rbind(ALL.SIM,A_temp) # This is the dataframe for predictions at the beginning of a given season.
    
    B_temp <- data.frame(ocean.region= smolt.prod$region[i],   
                         F_scen =F_scen[k],
                         time=AGE$time,
                         age.month=AGE$age,
                         age.year=AGE$year,
                         season=rownames(F_TOT_MAT),
                         juv.mort=juv.mort.shared,
                         indicator=ZZZ,
                         exp(log_N_pred))
    ALL.SIM.end <- rbind(ALL.SIM.end,B_temp) # This is the dataframe for predictions at the end of a given season.
  }  # End Release location loop  
  print(ZZZ)
  }  # End MCMC loop    
} # End Fishing mortality loop

THESE <- grep("X",colnames(ALL.SIM))
colnames(ALL.SIM)[THESE] <- as.character(LOCATIONS$location.name)
ALL.SIM$TOTAL <- rowSums(ALL.SIM[,THESE])

ALL.LONG <- melt(ALL.SIM,id.vars = c("ocean.region","F_scen","time","age.month","age.year","season","juv.mort","indicator"))
colnames(ALL.LONG)[colnames(ALL.LONG)=="variable"] <- "loc"

###
summary.val <- data.frame(summarise(group_by(ALL.LONG,ocean.region,time,age.month,age.year,season,loc),
                                   Mean = mean(value),
                                   SD   = sd(value),
                                   q.05=quantile(value,0.05),
                                   q.25=quantile(value,0.25),
                                   Median=quantile(value,0.50),
                                   q.75=quantile(value,0.75),
                                   q.95=quantile(value,0.95)))

###
tot.chin <- summarise(group_by(ALL.LONG[ALL.LONG$loc != "TOTAL",],time,age.month,age.year,season,indicator,loc),Sum = sum(value)) 
tot.chin.by.age   <- data.frame(summarise(group_by(tot.chin,time,age.month,age.year,season,loc),
                                 Mean = mean(Sum),
                                 SD   = sd(Sum),
                                 q.05=quantile(Sum,0.05),
                                 q.25=quantile(Sum,0.25),
                                 Median=quantile(Sum,0.50),
                                 q.75=quantile(Sum,0.75),
                                 q.95=quantile(Sum,0.95)))

tot.chin.by.age[,6:ncol(tot.chin.by.age)] <- tot.chin.by.age[,6:ncol(tot.chin.by.age)] / 1000
tot.chin.by.age$location.number <- LOCATIONS$location.number[match(tot.chin.by.age$loc,LOCATIONS$location.name)]

## summarize across age classes [e.g. age 2+]
tot.chin <- data.frame(tot.chin)
  # tot.chin$age.month <- as.numeric(tot.chin$age.month)
age.month <- c(7,19,31)  ### THIS CORRESPONDS TO AGES (3,4,5)

cum.chin.all <- NULL
for(i in 1:length(age.month)){
  temp <- tot.chin[tot.chin$age.month > age.month[i],]
  cum.chin.1  <- summarise(group_by(temp,season,indicator,loc),Sum = sum(Sum))   
  cum.chin    <- data.frame(summarise(group_by(cum.chin.1,season,loc),
                                            Mean = mean(Sum),
                                            SD   = sd(Sum),
                                            q.05=quantile(Sum,0.05),
                                            q.25=quantile(Sum,0.25),
                                            Median=quantile(Sum,0.50),
                                            q.75=quantile(Sum,0.75),
                                            q.95=quantile(Sum,0.95)))
  cum.chin[,3:ncol(cum.chin)] <- cum.chin[,3:ncol(cum.chin)]/1000
  cum.chin$age <- min(temp$age.year)+1
  cum.chin.all <- rbind(cum.chin.all,cum.chin)
}

cum.chin.all$location.number <- LOCATIONS$location.number[match(cum.chin.all$loc,LOCATIONS$location.name)]
###############

# OK.  MAKE PLOTS

###############
SEASON.NAME <- c("Wint","Spr", "Sum","Fall")
for(WWW in 1:length(SEASON.NAME)){

age.name <- 3
season.name <- SEASON.NAME[WWW]
LIM <- c(0,max(cum.chin.all$q.95[cum.chin.all$age == age.name & cum.chin.all$season==season.name])*1.05)
LIM <- c(0,2500)
p3 <- ggplot(data=cum.chin.all[cum.chin.all$age == age.name & cum.chin.all$season==season.name,]) + 
      geom_bar(aes( x=loc,y=Mean),stat="identity",fill=grey(0.4)) +
      geom_errorbar(aes(x=loc, ymax=q.95,ymin=q.05),  width=0) +
      labs(x = "", y = paste("Chinook, age ",age.name,"+ (1000s)",sep=""),title=paste("Age = ",age.name,"+",sep="")) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
  
      xlab(NULL) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,size=rel(0.9)))
      # theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),legend.position="none",
      #   panel.border=element_rect(colour="black",size=1.5),
      #   plot.margin=unit(c(0.1, 5, 0.05, 0.01), "lines")) 
      # 
age.name <- 4
LIM <- c(0,max(cum.chin.all$q.95[cum.chin.all$age == age.name & cum.chin.all$season==season.name])*1.05)
LIM <- c(0,800)
p4 <- ggplot(data=cum.chin.all[cum.chin.all$age == age.name & cum.chin.all$season==season.name,]) + 
  geom_bar(aes( x=loc,y=Mean),stat="identity",fill=grey(0.4)) +
  geom_errorbar(aes(x=loc, ymax=q.95,ymin=q.05),  width=0) +
  labs(x = "", y = paste("Chinook, age ",age.name,"+ (1000s)",sep=""),title=paste("Age = ",age.name,"+",sep="")) +
  scale_y_continuous(expand=c(0,0),limits=LIM) +
  
  xlab(NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,size=rel(0.9)))
#,plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),legend.position="none",
#   panel.border=element_rect(colour="black",size=1.5),
#   plot.margin=unit(c(0.1, 5, 0.05, 0.01), "lines")) 
# 

if(JUV.SURV=="FIXED"){
  quartz(file=paste("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/Mixed Model/Cumulative Distribution Plots/",SCEN.SIM,"Juv Surv Fixed=",round(exp(juv.mort.mean),2),";Age= 3+ and 4+, ",season.name," F=",F_scen,".pdf",sep=""),
          dpi=600,height=4,width=6,type="pdf")   
      print(p3)
      print(p4)  
dev.off()  
}

if(JUV.SURV=="RAND"){
  quartz(file=paste("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/Mixed Model/Cumulative Distribution Plots/",SCEN.SIM,"Juv Surv Mean=",round(exp(juv.mort.mean),2),"; log.SD=",round(juv.mort.sd,2),"; Age= 3+ and 4+, ",season.name," F=",F_scen,".pdf",sep=""),
       dpi=600,height=4,width=6,type="pdf")   
      print(p3)
      print(p4)  
dev.off()  
}
} # End Season Loop
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

#### MAKE CUMULATIVE DISTRIBUTION FOR ALL AREAS BASED ON MEAN SURVIVAL FOR EACH REGION, MEAN DISTRIBUTION, MEAN EVERYTHING.
#age.month <- c(7,19,31)
age.name  <- c(3,4,5)
for(QQQ in 1:length(age.month)){

    temp <- summary.val[summary.val$age.month>age.month[QQQ] & summary.val$loc!="TOTAL",]
    F_scen_name <- F_scen # options are" "F_zero"   "F_median" "F_max"

    old.by.region <- data.frame(summarise(group_by(temp,ocean.region,season,loc),chin=sum(Median)))
    old.by.region$loc.numb <- LOCATIONS$location.number[match(old.by.region$loc,LOCATIONS$location.name)]
    old.by.region$ocean.numb <- smolt.prod$number[match(old.by.region$ocean.region,smolt.prod$region)]
      
    old.by.region <- old.by.region[order(old.by.region$ocean.numb),]
    
    # Proportional contribution
    q1 <- ggplot(old.by.region[old.by.region$season=="Spr",],  aes(x = loc.numb, y = chin, fill = ocean.region)) + 
      geom_bar(position = "fill",stat = "identity") + 
      scale_y_continuous() +
      labs(x = "", y = paste("Proportion",sep="")) +
      scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name) +
      ggtitle("Spring Distribution") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
      scale_fill_discrete(name="Origin")
    q1
    
    COLS <- c("SFB"="#d7191c",
              "NCA"="#fdae61",
              "SOR"="#ffffbf",
              "COR"="#abd9e9",
              "NOR"="#2c7bb6",
              "COL"  ="#d7191c",
              "UPCOL"="#fdae61",
              "WAC"  = "#ffffbf",
              "PUSO" ="#abd9e9",
              "SGEO" ="#2c7bb6",
              "SWVI" = "#d7191c")
    
    
    COLS <- c("SFB"= "#DAA520", #"#ffffcc",
              "NCA"="#B8860B",
              "SOR"="#3CB371",
              "COR"="#228B22",
              "NOR"="#006400",
              "COL"  ="#00BFFF",
              "UPCOL"="#1E90FF",
              "WAC"  = "#0000FF",
              "PUSO" ="#FF6347",
              "SGEO" ="#DC143C",
              "SWVI" = "#A52A2A")
    
    old.by.region$ocean.region <-  factor(old.by.region$ocean.region,
                                        levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","COL",
                                                    "NOR","COR","SOR","NCA","SFB"))
    # OUT.OLD.DENS$region <-  factor(OUT.OLD.DENS$region, 
    #                                levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","COL",
    #                                           "NOR","COR","SOR","NCA","SFB"))
    # 
    # p + scale_colour_manual(values = COLS)
    # scale_fill_brewer(palette = 12)
    
    # Proportional contribution
    q1 <- ggplot(old.by.region[old.by.region$season=="Spr",],  aes(x = loc.numb, y = chin, fill = ocean.region )) + 
      geom_bar(position = "fill",stat = "identity") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "", y = "",title="a) Spring") +
      scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
      scale_fill_manual(values=COLS,name="Origin") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.05, 0.05, 0.01), "lines")) 
    q1
    q2 <- ggplot(old.by.region[old.by.region$season=="Sum",],  aes(x = loc.numb, y = chin, fill = ocean.region )) + 
      geom_bar(position = "fill",stat = "identity") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "", y = "",title="b) Summer") +
      scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
      scale_fill_manual(values=COLS,name="Origin") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.05, 0.05, 0.01), "lines")) 
    q2
    q3 <- ggplot(old.by.region[old.by.region$season=="Fall",],  aes(x = loc.numb, y = chin, fill = ocean.region )) + 
      geom_bar(position = "fill",stat = "identity") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "", y = paste("Proportion",sep=""),title="c) Fall") +
      scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
      scale_fill_manual(values=COLS,name="Origin") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.05, 0.05, 0.01), "lines")) 
    
    q4 <- ggplot(old.by.region[old.by.region$season=="Wint",],  aes(x = loc.numb, y = chin, fill = ocean.region )) + 
      geom_bar(position = "fill",stat = "identity") + 
      coord_flip() +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "", y = paste("Proportion",sep=""),title="c) Winter") +
      scale_x_continuous(breaks = 1:nrow(LOCATIONS), labels=LOCATIONS$location.name,expand = c(0, 0)) +
      scale_fill_manual(values=COLS,name="Origin") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.9,vjust=0.5),plot.title = element_text(hjust=0,size=rel(0.9)),legend.position="none",
            plot.margin=unit(c(0.1, 0.05, 0.05, 0.01), "lines")) 
    
    
    ## Plot the grey histograms
    
    LIM <- c(0,max(cum.chin.all$Mean[cum.chin.all$age == age.name[QQQ] ])*1.05)
    # LIM <- c(0,2500)
    p1  <- ggplot(data=cum.chin.all[cum.chin.all$age == age.name[QQQ] & cum.chin.all$season=="Spr",]) + 
      geom_bar(aes( x=loc,y=Mean),stat="identity",fill=grey(0.4)) +
      #geom_errorbar(aes(x=loc, ymax=q.95,ymin=q.05),  width=0) +
      coord_flip() +
      labs(x = "", y = "",title="e)") +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      scale_fill_manual(values=COLS,name="Origin") +
      xlab(NULL) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,color="white",size=rel(0.9)),legend.position="none",
            panel.border=element_rect(colour="black",size=1.5),
            plot.margin=unit(c(0.1, 5, 0.05, 0.01), "lines")) 
    p1

    p2 <- ggplot() + 
      geom_bar(data=old.by.region[old.by.region$season=="Sum",],  aes(x = loc, y = chin/1e6, fill = ocean.region ),stat = "identity") +
      geom_bar(data=cum.chin.all[cum.chin.all$age == age.name[QQQ] & cum.chin.all$season=="Sum",],  aes(x = loc, y = Mean ),fill=grey(0.4),stat = "identity") +
      #geom_line(data=total.by.reg[total.by.reg$season == "summer",],  aes(x = loc, y = TOT ),color= "black") +
      coord_flip() +
      #labs(title = "New plot title")
      labs(x = "",y="",title="e)") + 
      xlab(NULL) +#, y = paste("Chinook, age ",age.name,"+ (thousands)",sep="")) +
      #scale_x_continuous(expand=c(0.0,0), labels= "") + #rep("",nrow(LOCATIONS))) + #LOCATIONS$location.name) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      #  ggtitle("e)") +
      scale_fill_manual(values=COLS,name="Origin") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,color="white",size=rel(0.9)),
            panel.border=element_rect(colour="black",size=1.5),
            plot.margin=unit(c(0.1, 0.0, 0.05, 0.01), "lines"),
            legend.key.size = unit(0.4, "cm")) 
    p2  
    
    p3 <- ggplot() + 
      geom_bar(data=cum.chin.all[cum.chin.all$age == age.name[QQQ] & cum.chin.all$season=="Fall",],  aes(x = loc, y = Mean ),fill=grey(0.4),stat = "identity") +
      coord_flip() +
      labs(x = "", y = paste("Chinook, age ",age.name[QQQ],"+ (1000s)",sep=""),title="f)") +
      xlab(NULL) +
      scale_y_continuous(expand=c(0,0),limits=LIM) +
      scale_fill_manual(values=COLS,name="Origin") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),legend.position="none",
            panel.border=element_rect(colour="black",size=1.5),
            plot.margin=unit(c(0.1, 5, 0.05, 0.01), "lines")) 
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
    
    if(JUV.SURV=="FIXED"){
      quartz(file=paste("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/Mixed Model/Cumulative Distribution Plots/Cumul Abund plot,",SCEN.SIM," Juv Surv Fixed=",round(exp(juv.mort.mean),3),"; ",age.name[QQQ],"+ ; F=",F_scen_name,".pdf",sep=""),dpi=600,height=8,width=7,type="pdf")   
    }  
    if(JUV.SURV=="RAND"){
      quartz(file=paste("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/Mixed Model/Cumulative Distribution Plots/Cumul Abund plot,",SCEN.SIM," Juv Surv Fixed=",round(exp(juv.mort.mean),3),"; log.SD=",juv.mort.sd,";",age.name[QQQ],"+ ; F=",F_scen_name,".pdf",sep=""),dpi=600,height=8,width=7,type="pdf")   
    }             
    
      Layout= matrix(c(4,4,4,4,1,1,1,5,5,5,5,2,2,2,6,6,6,6,3,3,3),nrow=3,ncol=7,byrow=T)
      QQ <- list(p1,p2,p3,q1,q2,q3)
      multiplot(plotlist=QQ ,layout= Layout)
    dev.off()
} # End QQQ loop