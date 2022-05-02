###

# Calculate the point predictions for each observations
  # binomial

  
# Mixing First
  if(TREATY == FALSE){
  base_params <- c("log_q_troll","log_q_rec",#"log_q_treaty",
                   "log_q_troll_pos", "log_q_rec_pos",#"log_q_treaty_pos",
                   "sigma_pos",
                   "log_rel_year_mu", "rel_year_sigma",
                   "vuln_int","beta_vuln")#"prob_age_year") #"beta_age_month""vuln_int"
  }
  if(TREATY == TRUE){
    base_params <- c("log_q_troll","log_q_rec","log_q_treaty",
                     "log_q_troll_pos", "log_q_rec_pos","log_q_treaty_pos",
                     "sigma_pos",
                     "log_rel_year_mu", "rel_year_sigma",
                     "vuln_int","beta_vuln")#"prob_age_year") #"beta_age_month""vuln_int"
  }

  
  pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/",NAME," traceplots.pdf",sep=""),height = 6, width=8.5)
      print(traceplot(mod,pars=c("lp__",pars=base_params),inc_warmup=F))
  dev.off()
      
if(TREATY == FALSE & SEASON == FALSE){
    #### MAKE THE mean predictions for the presence - absence.
  lambda <- NULL
  prob   <- NULL
  for(i in 1:nrow(dat.bin)){

    lambda[i] <- dat.bin$log.N0[i] - cum_M2[dat.bin$time[i]] +                        # fixed mortality for adults.
      #beta_age_month * age_month[i] +                       # Age_month continuous covariate
      mean(samp$log_q_troll) +  
      mean(samp$log_q_rec) * dat.bin$effort.idx.rec[i] +           # effort offset
      #mean(samp$log_q_treaty) * dat.bin$effort.idx.treaty[i] +     # effort offset
      log(origin_loc[dat.bin$origin.idx[i],dat.bin$location[i]]) -               # dispersion in the ocean term (sum to 1)                   
      rel_year_all[dat.bin$year.reg.idx[i]] * dat.bin$N.month[i] +            # Initial period before fish are observed                   
      log(dat.bin$effort[i]) +
      log(1 - sum_prob[dat.bin$loc.spawn.idx[i],dat.bin$year[i]]) +
      log(vuln_mat[dat.bin$vuln.idx[i],dat.bin$age.vuln.idx[i]]) ;             # Vulnerability as a function of ocean age in months.
    
      prob[i]   <- exp(lambda[i]) / (exp(lambda[i]) + 1)  ;
  }  
}
if(TREATY == TRUE & SEASON == FALSE){
  #### MAKE THE mean predictions for the presence - absence.
  lambda <- NULL
  prob   <- NULL
  for(i in 1:nrow(dat.bin)){
    
    lambda[i] <- dat.bin$log.N0[i] - cum_M2[dat.bin$time[i]] +                        # fixed mortality for adults.
      mean(samp$log_q_troll) +  
      mean(samp$log_q_rec) * dat.bin$effort.idx.rec[i] +           # effort offset
      mean(samp$log_q_treaty) * dat.bin$effort.idx.treaty[i] +     # effort offset
      log(origin_loc[dat.bin$origin.idx[i],dat.bin$location[i]]) -               # dispersion in the ocean term (sum to 1)                   
      rel_year_all[dat.bin$year.reg.idx[i]] * dat.bin$N.month[i] +            # Initial period before fish are observed                   
      log(dat.bin$effort[i]) +
      log(1 - sum_prob[dat.bin$loc.spawn.idx[i],dat.bin$year[i]]) +
      log(vuln_mat[dat.bin$vuln.idx[i],dat.bin$age.vuln.idx[i]])              # Vulnerability as a function of ocean age in months.
    
    prob[i]   <- exp(lambda[i]) / (exp(lambda[i]) + 1)  ;
  }   
}  

  if(TREATY == TRUE & SEASON == TRUE){
    #### MAKE THE mean predictions for the presence - absence.
    lambda <- NULL
    prob   <- NULL
    for(i in 1:nrow(dat.bin)){
      
      lambda[i] <- dat.bin$log.N0[i] - cum_M2[dat.bin$time[i]] +                        # fixed mortality for adults.
        mean(samp$log_q_troll) +  
        mean(samp$log_q_rec) * dat.bin$effort.idx.rec[i] +           # effort offset
        mean(samp$log_q_treaty) * dat.bin$effort.idx.treaty[i] +     # effort offset
        log(origin_loc[dat.bin$season.idx[i],dat.bin$origin.idx[i],dat.bin$location[i]]) -               # dispersion in the ocean term (sum to 1)                   
        rel_year_all[dat.bin$year.reg.idx[i]] * dat.bin$N.month[i] +            # Initial period before fish are observed                   
        log(dat.bin$effort[i]) +
        log(1 - sum_prob[dat.bin$loc.spawn.idx[i],dat.bin$year[i]]) +
        log(vuln_mat[dat.bin$vuln.idx[i],dat.bin$age.vuln.idx[i]])              # Vulnerability as a function of ocean age in months.
      
      prob[i]   <- exp(lambda[i]) / (exp(lambda[i]) + 1)  ;
    }   
  }  
  
  if(TREATY == FALSE & SEASON == TRUE){
    #### MAKE THE mean predictions for the presence - absence.
    lambda <- NULL
    prob   <- NULL
    for(i in 1:nrow(dat.bin)){
      
      lambda[i] <- dat.bin$log.N0[i] - cum_M2[dat.bin$time[i]] +                        # fixed mortality for adults.
        mean(samp$log_q_troll) +  
        mean(samp$log_q_rec) * dat.bin$effort.idx.rec[i] +           # effort offset
        #mean(samp$log_q_treaty) * dat.bin$effort.idx.treaty[i] +     # effort offset
        log(origin_loc[dat.bin$season.idx[i],dat.bin$origin.idx[i],dat.bin$location[i]]) -               # dispersion in the ocean term (sum to 1)                   
        rel_year_all[dat.bin$year.reg.idx[i]] * dat.bin$N.month[i] +            # Initial period before fish are observed                   
        log(dat.bin$effort[i]) +
        log(1 - sum_prob[dat.bin$loc.spawn.idx[i],dat.bin$year[i]]) +
        log(vuln_mat[dat.bin$vuln.idx[i],dat.bin$age.vuln.idx[i]])              # Vulnerability as a function of ocean age in months.
      
      prob[i]   <- exp(lambda[i]) / (exp(lambda[i]) + 1)  ;
    }   
  }  
  
  
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
  
# Plot binomial predictions by area
pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/"," Obs-Pred Binomial ",NAME,".pdf",sep=""),height = 6, width=8.5)
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

#### MAKE THE mean predictions for the positive
mu <- NULL
if(TREATY == TRUE & SEASON ==FALSE){
for(i in 1:nrow(dat.pos)){
  mu[i] <- dat.pos$log.N0[i] - cum_M2[dat.pos$time[i]] +                        # fixed mortality for adults.
    #beta_age_month * age_month[i] +                       # Age_month continuous covariate
    mean(samp$log_q_troll_pos) +  
    mean(samp$log_q_rec_pos) * dat.pos$effort.idx.rec[i] +           # effort offset
   # mean(samp$log_q_treaty_pos) * dat.pos$effort.idx.treaty[i] +     # effort offset
    log(origin_loc[dat.pos$origin.idx[i],dat.pos$location[i]]) -               # dispersion in the ocean term (sum to 1)                   
    rel_year_all[dat.pos$year.reg.idx[i]] * dat.pos$N.month[i] +            # Initial period before fish are observed                   
    log(dat.pos$effort[i]) +
    log(1 - sum_prob[dat.pos$loc.spawn.idx[i],dat.pos$year[i]]) +
    log(vuln_mat[dat.pos$vuln.idx[i],dat.pos$age.vuln.idx[i]]) ;              # Vulnerability as a function of ocean age in months.
    # beta_vuln[dat.pos$vuln.idx[i]] * dat.pos$age.vuln[i] ;              # Vulnerability as a function of ocean age in months.
}  
}
if(TREATY == FALSE & SEASON ==FALSE){
  for(i in 1:nrow(dat.pos)){
    mu[i] <- dat.pos$log.N0[i] - cum_M2[dat.pos$time[i]] +                        # fixed mortality for adults.
      #beta_age_month * age_month[i] +                       # Age_month continuous covariate
      mean(samp$log_q_troll_pos) +  
      mean(samp$log_q_rec_pos) * dat.pos$effort.idx.rec[i] +           # effort offset
      mean(samp$log_q_treaty_pos) * dat.pos$effort.idx.treaty[i] +     # effort offset
      log(origin_loc[dat.pos$origin.idx[i],dat.pos$location[i]]) -               # dispersion in the ocean term (sum to 1)                   
      rel_year_all[dat.pos$year.reg.idx[i]] * dat.pos$N.month[i] +            # Initial period before fish are observed                   
      log(dat.pos$effort[i]) +
      log(1 - sum_prob[dat.pos$loc.spawn.idx[i],dat.pos$year[i]]) +
      log(vuln_mat[dat.pos$vuln.idx[i],dat.pos$age.vuln.idx[i]]) ;              # Vulnerability as a function of ocean age in months.
    # beta_vuln[dat.pos$vuln.idx[i]] * dat.pos$age.vuln[i] ;              # Vulnerability as a function of ocean age in months.
  }  
}

if(TREATY == TRUE & SEASON == TRUE){
  for(i in 1:nrow(dat.pos)){
    mu[i] <- dat.pos$log.N0[i] - cum_M2[dat.pos$time[i]] +                        # fixed mortality for adults.
      #beta_age_month * age_month[i] +                       # Age_month continuous covariate
      mean(samp$log_q_troll_pos) +  
      mean(samp$log_q_rec_pos) * dat.pos$effort.idx.rec[i] +           # effort offset
      mean(samp$log_q_treaty_pos) * dat.pos$effort.idx.treaty[i] +     # effort offset
      log(origin_loc[dat.pos$season.idx[i],dat.pos$origin.idx[i],dat.pos$location[i]]) -               # dispersion in the ocean term (sum to 1)                   
      rel_year_all[dat.pos$year.reg.idx[i]] * dat.pos$N.month[i] +            # Initial period before fish are observed                   
      log(dat.pos$effort[i]) +
      log(1 - sum_prob[dat.pos$loc.spawn.idx[i],dat.pos$year[i]]) +
      log(vuln_mat[dat.pos$vuln.idx[i],dat.pos$age.vuln.idx[i]]) ;              # Vulnerability as a function of ocean age in months.
    # beta_vuln[dat.pos$vuln.idx[i]] * dat.pos$age.vuln[i] ;              # Vulnerability as a function of ocean age in months.
  }  
}
if(TREATY == FALSE & SEASON == TRUE){
  for(i in 1:nrow(dat.pos)){
    mu[i] <- dat.pos$log.N0[i] - cum_M2[dat.pos$time[i]] +                        # fixed mortality for adults.
      #beta_age_month * age_month[i] +                       # Age_month continuous covariate
      mean(samp$log_q_troll_pos) +  
      mean(samp$log_q_rec_pos) * dat.pos$effort.idx.rec[i] +           # effort offset
      #mean(samp$log_q_treaty_pos) * dat.pos$effort.idx.treaty[i] +     # effort offset
      log(origin_loc[dat.pos$season.idx[i],dat.pos$origin.idx[i],dat.pos$location[i]]) -               # dispersion in the ocean term (sum to 1)                   
      rel_year_all[dat.pos$year.reg.idx[i]] * dat.pos$N.month[i] +            # Initial period before fish are observed                   
      log(dat.pos$effort[i]) +
      log(1 - sum_prob[dat.pos$loc.spawn.idx[i],dat.pos$year[i]]) +
      log(vuln_mat[dat.pos$vuln.idx[i],dat.pos$age.vuln.idx[i]]) ;              # Vulnerability as a function of ocean age in months.
    # beta_vuln[dat.pos$vuln.idx[i]] * dat.pos$age.vuln[i] ;              # Vulnerability as a function of ocean age in months.
  }  
}


pred.pos <- exp(mu + (mean(samp$sigma_pos)^2) / 2)
pred.pos <- exp(mu)

plot(dat.pos$catch~pred.pos)

plot(log(dat.pos$catch)~log(pred.pos))
abline(0,1,col=2)

pred.obs.pos.plot <- function(data, pred.pos,pch){ #, smoother.int ){
 
  par(mfrow=c(1,2))
   x.lim=c(min(log(pred.pos),log(data$catch)),max(log(pred.pos),log(data$catch)))
  plot(log(data$catch)~log(pred.pos),pch=pch,ylim=x.lim,xlim=x.lim,axes=F,xlab="",ylab="")
  par(new=T)
  # plot(OUTPUT$pred~OUTPUT$interval,type="l",ylim=y.lim,xlim=x.lim,lwd=4,col="white",axes=F,xlab="",ylab="")
  # par(new=T)
  # plot(OUTPUT$pred~OUTPUT$interval,type="l",ylim=y.lim,xlim=x.lim,lwd=2,axes=F,xlab="",ylab="")
  abline(0,1,lty=2,lwd=2,col=2)
  axis(1)
  axis(2,las=2)
  box(bty="o",lwd=2)
  title(xlab="Predicted (log scale)")
  title(ylab="Observed (log scale)")

  x.lim=c(min((pred.pos),(data$catch)),max((pred.pos),(data$catch)))
  plot((data$catch)~(pred.pos),pch=pch,ylim=x.lim,xlim=x.lim,axes=F,xlab="",ylab="")
  par(new=T)
  # plot(OUTPUT$pred~OUTPUT$interval,type="l",ylim=y.lim,xlim=x.lim,lwd=4,col="white",axes=F,xlab="",ylab="")
  # par(new=T)
  # plot(OUTPUT$pred~OUTPUT$interval,type="l",ylim=y.lim,xlim=x.lim,lwd=2,axes=F,xlab="",ylab="")
  abline(0,1,lty=2,lwd=2,col=2)
  axis(1)
  axis(2,las=2)
  box(bty="o",lwd=2)
  title(xlab="Predicted (identity scale)")
  title(ylab="Observed (identity scale)")
}

# Plot positive predictions by area
pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/"," Obs-Pred Postive ",NAME,".pdf",sep=""),height = 6, width=8.5)
  pred.obs.pos.plot(data=dat.pos,pred.pos=pred.pos,pch=".")
  title("ALL")
  nom <- as.character(spawn_loc$ocean.region)

  for(i in 1:length(nom)){
    pred.obs.pos.plot(data=dat.pos[dat.pos$ocean.reg == nom[i],],pred.pos=pred.pos[dat.pos$ocean.reg ==nom[i]],pch=21)
    title(nom[i])
  }
pred.obs.pos.plot(data=dat.pos[dat.pos$effort.type == "troll",],pred.pos=pred.pos[dat.pos$effort.type == "troll"],pch=21)
title("Troll")
# pred.obs.pos.plot(data=dat.pos[dat.pos$effort.type == "treaty",],pred.pos=pred.pos[dat.pos$effort.type == "treaty"],pch=21)
# title("Treaty")
pred.obs.pos.plot(data=dat.pos[dat.pos$effort.type == "rec",],pred.pos=pred.pos[dat.pos$effort.type == "rec"],pch=21)
title("Rec")

#### PLOTS BY AGE
age <- as.character(unique(dat.pos$year))
for(i in 1:length(age)){
  pred.obs.pos.plot(data=dat.pos[dat.pos$year == age[i],],pred.pos=pred.pos[dat.pos$year == age[i]],pch=21)
  title(paste("Year =",age[i]))
}

dev.off()

