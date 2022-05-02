###

# Calculate the point predictions for each observations
  # binomial

  
# Mixing First
base_params <- c(#"log_q_troll","log_q_rec","log_q_treaty",
  "tau_process",
  "log_q_troll_pos", "log_q_rec_pos","log_q_treaty_pos","log_q_rec_can_pos",
  "logit_offset",
  "sigma_pos",
  #"log_rel_year_mu", "rel_year_sigma",
  "beta_vuln",#"vuln_int",
  #"log_M2",
  "log_F_rec_mean","F_rec_sigma"
)#"prob_age_year") #"beta_age_month""vuln_int"

  pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/",NAME, " traceplots.pdf",sep=""),height = 6, width=8.5)
      print(traceplot(mod,pars=c("lp__",base_params),inc_warmup=F))
  dev.off()
    
  
 # plot the distribution of final vs. initial population sizes in the ocean    
  pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/",NAME, " log_N_ratio.pdf",sep=""),height = 6, width=8.5)
  
    temp <- summarize(group_by(REL,ocean.region),loc.numb=mean(loc.numb))
    temp <- temp[order(temp$loc.numb),]
  
    #x.lim <- c(min(LOCATIONS$location.number),max(LOCATIONS$location.number))
    par(mar=c(4,4,0.5,0.5))
    AT    <- temp$loc.numb
    plot(y=log_N_ratio,x=REL$loc.numb+runif(length(REL$loc.numb),-0.2,0.2),axes=F,xlab="",ylab="")
    axis(1,las=2,at=AT,labels=temp$ocean.region)
    axis(2,las=2)
    title(ylab="log-ratio \n (log[final N / post-juv N])",line=2)
    box(bty="o",lwd=2)
  
  dev.off()
  
  
  #######################################  FIX ME!!! 
Baranov <- function(M2,  F_focal,  F_tot,  log_N,  log_origin){
    return  (log(F_focal) - log(M2 + F_tot) + log_N + log_origin + log(1 - exp(-(M2 + F_tot) )))  
  }


# make arrays on the scale of the release groups. Turn (year,month,location) into (release id,model month, location)
N_loc <- length(unique(dat.bin$location))
N_rel <- nrow(REL)
N_time_mod <- max(dat.bin$time)
N_years_release
troll_mat <- matrix(0,N_time_mod,N_loc)
rec_mat   <- troll_mat


for(i in 1:N_loc){
  troll_mat[,i] <- vuln_mat[vuln_int_idx[i],]   
  rec_mat[,i]   <- vuln_mat[vuln_int_rec_idx[i],]
}     

F_troll_fin <- array(0,dim= c(N_time_mod,N_loc,N_years_release))
F_rec_fin   <- F_troll_fin
F_tot_fin   <- F_troll_fin

for(i in 1:N_years_release){ # Make an array of the same dimensions as the state vector
  START <- (1+(i-1)*N_month)
  STOP  <- START + N_time_mod -1 
  F_troll_fin[,,i]      <- F_troll_array[START:STOP,] * troll_mat  ;
  F_rec_fin[,,i]        <- F_rec_array[START:STOP,] * rec_mat  ;
  F_tot_fin[,,i]        <- F_troll_fin[,,i] + F_rec_fin[,,i] ; 
  # Process error
  #zeta_fin[i]         <- block(zeta_mat,(1+(i-1)*N_month),1,N_time_mod,N_loc) ;
}

############################################################################################
############################################################################################
###################################################################################################################
##################################################################################################################
############ LATENT STATES 


#cum_M2_temp <- cum_M2_fixed
cum_M2_temp <- cum_M2
  for(i in 1:N_time_mod){
    if(i==1){cum_M2_temp[i]  <- cum_M2[age_month_idx[i]] ;} 
    if(i > 1){cum_M2_temp[i] <- cum_M2[age_month_idx[i]] - cum_M2[age_month_idx[i-1]] ;} 
  }

prob<- NULL
### CALCULATING THE OBSERVATIONS 
### CALCULATING THE PREDICTIONS FOR OBSERVATIONS 
for(i in 1:N_obs_bin){
  if( gear_bin_idx[i]  == 1 ){ F_focal <- F_troll_fin[mod_time_idx[i],loc_idx[i],start_year[rel_idx[i]]] ; } 
  if( gear_bin_idx[i]  == 2 ){ F_focal <- F_rec_fin[mod_time_idx[i],loc_idx[i],start_year[rel_idx[i]]] ; }      
  F_tot <- F_tot_fin[mod_time_idx[i],loc_idx[i],start_year[rel_idx[i]]]
  if(spawn_time[mod_time_idx[i]] == 0){
    lambda_temp <- logit_offset[gear_bin_idx[i],1]  + logit_offset[gear_bin_idx[i],2] *
      Baranov(  cum_M2_temp[mod_time_idx[i]],
                F_focal,
                F_tot,
                log_N_all[rel_idx[i],mod_time_idx[i]],
                log(origin_loc[season_idx[mod_time_idx[i]],origin_bin_idx[i],loc_idx[i]] ) );
  }
  if(spawn_time[mod_time_idx[i]] > 0){
    lambda_temp <- 
      Baranov(  cum_M2_temp[mod_time_idx[i]] * 0.5,
                F_focal * 0.5,
                F_tot * 0.5,
                log_N_all[rel_idx[i],mod_time_idx[i]],
                log(origin_loc[season_idx[mod_time_idx[i]],origin_bin_idx[i],loc_idx[i]] ) );
    log_N_temp_1 <- log_N_all[rel_idx[i],mod_time_idx[i]] - 
      F_tot * 0.5 -
      cum_M2_temp[mod_time_idx[i]] * 0.5 +
      log(origin_loc[season_idx[mod_time_idx[i]],origin_bin_idx[i],loc_idx[i]] ) +
      log(1 - prob_age_year[loc_spawn_bin_idx[i],spawn_time_idx[mod_time_idx[i]]] * river_entry[rel_idx[i],loc_idx[i]] ); 
    lambda_temp <- logit_offset[gear_bin_idx[i],1]  + logit_offset[gear_bin_idx[i],2] *
       log(exp( lambda_temp) +
            exp(Baranov(  cum_M2_temp[mod_time_idx[i]] * 0.5 ,
                F_focal * 0.5,
                F_tot * 0.5,
                log_N_temp_1,
                0)));
  }
  prob[i] <- exp(lambda_temp) / (exp(lambda_temp)+1) 
}

mu_pos <- NULL
for(i in 1:N_obs_pos){
  if(gear_pos_idx[i] ==1 ){ F_focal <- F_troll_fin[mod_time_pos_idx[i],loc_pos_idx[i],start_year[rel_pos_idx[i]]] ;}
  if(gear_pos_idx[i] ==2 ){ F_focal <- F_rec_fin[mod_time_pos_idx[i],loc_pos_idx[i],start_year[rel_pos_idx[i]]] ;}
  F_tot <- F_tot_fin[mod_time_pos_idx[i],loc_pos_idx[i],start_year[rel_pos_idx[i]]] 
  if(spawn_time[mod_time_pos_idx[i]] == 0){
    mu_pos[i] <- Baranov(  cum_M2_temp[mod_time_pos_idx[i]],
                           F_focal ,
                           F_tot ,
                           log_N_all[rel_pos_idx[i],mod_time_pos_idx[i]],
                           log(origin_loc[season_idx[mod_time_pos_idx[i]],origin_pos_idx[i],loc_pos_idx[i]] ) );
  }
  if(spawn_time[mod_time_pos_idx[i]] > 0){
    mu_pos[i] <- Baranov(  cum_M2_temp[mod_time_pos_idx[i]] * 0.5,
                           F_focal * 0.5,
                           F_tot * 0.5,
                           log_N_all[rel_pos_idx[i],mod_time_pos_idx[i]],
                           log(origin_loc[season_idx[mod_time_pos_idx[i]],origin_pos_idx[i],loc_pos_idx[i]] ) );
    log_N_temp_1 <- log_N_all[rel_pos_idx[i],mod_time_pos_idx[i]] - 
      F_tot * 0.5 -
      cum_M2_temp[mod_time_pos_idx[i]] * 0.5 +
      log(origin_loc[season_idx[mod_time_pos_idx[i]],origin_pos_idx[i],loc_pos_idx[i]] ) +
      log(1 - prob_age_year[loc_spawn_pos_idx[i],spawn_time_idx[mod_time_pos_idx[i]]] * river_entry[rel_pos_idx[i],loc_pos_idx[i]] ); 
    mu_pos[i] <- log(exp(mu_pos[i]) +
                      exp(Baranov(  cum_M2_temp[mod_time_pos_idx[i]] * 0.5 ,
                          F_focal * 0.5,
                          F_tot * 0.5,
                          log_N_temp_1,
                          0)));
  }
}
#######################
############################################################################################
############################################################################################
###################################################################################################################
  
  pred.obs.plot <- function(data, prob, smoother.int ){
  ### MAKE A BOX CAR SMOOTHER FUNCTION
  INTERVAL	<-	seq(smoother.int/2,1-smoother.int/2,by=0.005)
  OUTPUT	  <- data.frame(interval=INTERVAL)
  INC		    =	 smoother.int

  for(i in 1:length(INTERVAL)){
    temp <-  data$catch[prob > (INTERVAL[i] - INC/2) & prob < (INTERVAL[i] + INC/2) ]
    OUTPUT$pred[i] <-  sum( temp) / length(temp)
  }  

  JIT <- 0.15
  catch.plot <- data$catch + runif(nrow(data),-JIT,JIT)
  y.lim <- c(-JIT,1+JIT)
  x.lim=c(0,1)
  plot(catch.plot~prob,pch='.',ylim=y.lim,xlim=x.lim,axes=F,xlab="",ylab="")
  par(new=T)
  plot(OUTPUT$pred~OUTPUT$interval,type="l",ylim=y.lim,xlim=x.lim,lwd=4,col="white",axes=F,xlab="",ylab="")
  par(new=T)
  plot(OUTPUT$pred~OUTPUT$interval,type="l",ylim=y.lim,xlim=x.lim,lwd=2,axes=F,xlab="",ylab="")
  abline(0,1,lty=2,lwd=2,col=2)
  axis(1,at=seq(0,1,by=0.2))
  axis(2,at=seq(0,1,by=0.2),las=2)
  box(bty="o",lwd=2)
  title(xlab="Predicted")
  title(ylab="Observed")
}
  ############################################################################################################
  ############################################################################################################
  ############################################################################################################
  
# Plot binomial predictions by area
pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/", "Obs-Pred Binomial " ,NAME,".pdf",sep=""),height = 6, width=8.5)
  INT <- 0.1
  pred.obs.plot(data=dat.bin,prob=prob,smoother.int=INT)
  title("ALL")
  nom <- as.character(spawn_loc$ocean.region)
  for(i in 1:length(nom)){
    pred.obs.plot(data=dat.bin[dat.bin$ocean.reg == nom[i],],prob=prob[dat.bin$ocean.reg ==nom[i]],smoother.int=INT)
    title(nom[i])
  }
  pred.obs.plot(data=dat.bin[dat.bin$effort.type == "troll",],prob=prob[dat.bin$effort.type == "troll"],smoother.int=INT)
  title("Troll")
  pred.obs.plot(data=dat.bin[dat.bin$effort.type == "treaty",],prob=prob[dat.bin$effort.type == "treaty"],smoother.int=INT)
  title("Treaty")
  pred.obs.plot(data=dat.bin[dat.bin$effort.type == "rec",],prob=prob[dat.bin$effort.type == "rec"],smoother.int=INT)
  title("Rec")

  age <- as.character(unique(dat.bin$year))
  for(i in 1:length(age)){
    pred.obs.plot(data=dat.bin[dat.bin$year == age[i],],prob=prob[dat.bin$year == age[i]],smoother.int=INT)
    title(paste("Year =",age[i]))
  }
dev.off()

############################################################################################################
############################################################################################################
############################################################################################################
#### POSITIVE.
############################################################################################################
############################################################################################################
############################################################################################################


pred.obs.pos.plot <- function(data, pred.pos,pch){ #, smoother.int ){
 
  par(mfrow=c(1,2))
   x.lim=c(min(pred.pos,log(data$catch)),max(pred.pos,log(data$catch)))
  plot(log(data$catch)~pred.pos,pch=pch,ylim=x.lim,xlim=x.lim,axes=F,xlab="",ylab="")
  par(new=T)
  # plot(OUTPUT$pred~OUTPUT$interval,type=l,ylim=y.lim,xlim=x.lim,lwd=4,col=white,axes=F,xlab=,ylab=)
  # par(new=T)
  # plot(OUTPUT$pred~OUTPUT$interval,type=l,ylim=y.lim,xlim=x.lim,lwd=2,axes=F,xlab=,ylab=)
  abline(0,1,lty=2,lwd=2,col=2)
  axis(1)
  axis(2,las=2)
  box(bty="o",lwd=2)
  title(xlab="Predicted (log scale)")
  title(ylab="Observed (log scale)")

  x.lim=c(min(exp(pred.pos),(data$catch)),max(exp(pred.pos),(data$catch)))
  plot((data$catch)~exp(pred.pos),pch=pch,ylim=x.lim,xlim=x.lim,axes=F,xlab="",ylab="")
  par(new=T)
  # plot(OUTPUT$pred~OUTPUT$interval,type=l,ylim=y.lim,xlim=x.lim,lwd=4,col=white,axes=F,xlab=,ylab=)
  # par(new=T)
  # plot(OUTPUT$pred~OUTPUT$interval,type=l,ylim=y.lim,xlim=x.lim,lwd=2,axes=F,xlab=,ylab=)
  abline(0,1,lty=2,lwd=2,col=2)
  axis(1)
  axis(2,las=2)
  box(bty="o",lwd=2)
  title(xlab="Predicted (log scale)")
  title(ylab="Observed (log scale)")
}

# Plot positive predictions by area
pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/", "Obs-Pred Positive " ,NAME,".pdf",sep=""),height = 6, width=8.5)
  pred.obs.pos.plot(data=dat.pos,pred.pos=mu_pos,pch=".")
  title("ALL")
  nom <- as.character(spawn_loc$ocean.region)

  for(i in 1:length(nom)){
    pred.obs.pos.plot(data=dat.pos[dat.pos$ocean.reg == nom[i],],pred.pos=(mu_pos[dat.pos$ocean.reg ==nom[i]]),pch=21)
    title(nom[i])
  }
pred.obs.pos.plot(data=dat.pos[dat.pos$effort.type == "troll",],pred.pos=(mu_pos[dat.pos$effort.type == "troll"]),pch=21)
title("Troll")
# pred.obs.pos.plot(data=dat.pos[dat.pos$effort.type == treaty,],pred.pos=pred.pos[dat.pos$effort.type == treaty],pch=21)
# title(Treaty)
pred.obs.pos.plot(data=dat.pos[dat.pos$effort.type == "rec",],pred.pos=(mu_pos[dat.pos$effort.type == "rec"]),pch=21)
title("Rec")

#### PLOTS BY AGE
age <- as.character(unique(dat.pos$year))
for(i in 1:length(age)){
  pred.obs.pos.plot(data=dat.pos[dat.pos$year == age[i],],pred.pos=(mu_pos[dat.pos$year == age[i]]),pch=21)
  title(paste("Year =",age[i]))
}
dev.off()


