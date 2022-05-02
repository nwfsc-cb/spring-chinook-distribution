###

# Calculate the point predictions for each observations
  # binomial

# Mixing First
base_params <- c(#"tau_process",
                 #"tau_process_prod",
                 "log_q_rec_start","log_q_troll_start","log_q_rec_can_start","log_q_treaty_start",
                 "log_q_rec_slope","log_q_troll_slope","log_q_rec_can_slope",#"log_q_treaty_slope",
                 "log_q_rec_can_irec_start",
                 "log_q_hake_ashop_start",
                 "log_q_hake_shoreside_start",
                 "q_int",
                 #"log_q_troll_pos", "log_q_rec_pos","log_q_treaty_pos","log_q_rec_can_pos",#"log_q_rec_PUSO_pos",
                 #"logit_offset_slope",
                 #"sigma_pos",
                 "sigma_cv",
                 "sigma_cv_hake",
                 #"sigma_pos_slope",
                 # "alpha_pay_mean",
                 # "alpha_pay_sd",
                 # "beta_pay_mean",
                 # "beta_pay_sd",
                 "log_rel_year_mu", "log_rel_year_sigma",
                 "beta_vuln","beta_vuln_hake",
                 #"log_M2",
                 "log_F_rec_mean","log_F_troll_mean","F_rec_sigma","F_troll_sigma"
)

   pdf(paste(base.dir,"/Salmon-Climate/Output plots/",NAME, " traceplots.pdf",sep=""),height = 6, width=8.5)
       print(rstan::traceplot(mod,pars=c("lp__",base_params),inc_warmup=F))
   dev.off()
    
  
 # plot the distribution of final vs. initial population sizes in the ocean    
  # pdf(paste(base.dir,"/Salmon-Climate/Output plots/",NAME, " log_N_ratio.pdf",sep=""),height = 6, width=8.5)
  # 
  #   temp <- dplyr::summarize(group_by(REL,ocean.region),loc.numb=mean(loc.numb))
  #   temp <- temp[order(temp$loc.numb),]
  # 
  #   #x.lim <- c(min(LOCATIONS$location.number),max(LOCATIONS$location.number))
  #   par(mar=c(4,4,0.5,0.5))
  #   AT    <- temp$loc.numb
  #   plot(y=log_N_ratio,x=REL$loc.numb+runif(length(REL$loc.numb),-0.2,0.2),axes=F,xlab="",ylab="")
  #   axis(1,las=2,at=AT,labels=temp$ocean.region)
  #   axis(2,las=2)
  #   title(ylab="log-ratio \n (log[final N / post-juv N])",line=2)
  #   box(bty="o",lwd=2)
  # 
  # dev.off()
  # 
  
  #######################################
Baranov <- function(M2,  F_focal,  F_tot,  log_N,  log_origin){
    return  (log(F_focal) - log(M2 + F_tot) + log_N + log_origin + log(1 - exp(-(M2 + F_tot) )))  
  }

# make arrays on the scale of the release groups. Turn (year,month,location) into (release id,model month, location)
N_rel <- nrow(REL)
########

troll_mat  <- vuln_troll_array * 0
treaty_mat <- vuln_troll_array * 0
rec_mat    <- vuln_troll_array * 0
hake_ashop_mat     <- vuln_troll_array * 0
hake_shoreside_mat <- vuln_troll_array * 0

if(vuln_fixed>2){ # THis is for the logistic formulation
  for(i in 1:N_years_release){ ## Make an array of the same dimensions as the state vector
    for(j in 1:N_time_mod){
    troll_mat[i,,]  = 1 / (1 + exp(-(vuln_fixed + vuln_troll_array[i,,] * (beta_vuln[1] * vuln_age))))  
    treaty_mat[i,,] = 1 / (1 + exp(-(vuln_fixed + vuln_treaty_array[i,,] * (beta_vuln[1] * vuln_age))))  
    rec_mat[i,,]    = 1 / (1 + exp(-(vuln_fixed + vuln_rec_array[i,,] * (beta_vuln[2] * vuln_age))))  
    if(length(beta_vuln_hake)==1){
      hake_ashop_mat[i,,]     = 1 / (1 + exp(-(vuln_fixed + (beta_vuln_hake  * vuln_age_trawl))))
      hake_shoreside_mat[i,,] = 1 / (1 + exp(-(vuln_fixed + (beta_vuln_hake  * vuln_age_trawl))))
    }
    if(length(beta_vuln_hake)==2){
      hake_ashop_mat[i,,]     = 1 / (1 + exp(-(vuln_fixed + (beta_vuln_hake[1]  * vuln_age_trawl + beta_vuln_hake[2]  * vuln_age_trawl^2))))
      hake_shoreside_mat[i,,] = 1 / (1 + exp(-(vuln_fixed + (beta_vuln_hake[1]  * vuln_age_trawl + beta_vuln_hake[2]  * vuln_age_trawl^2))))
    }
    }
  }
}
if(vuln_fixed<2){ # THis is for the complementary log-log formulation
  for(i in 1:N_years_release){ ## Make an array of the same dimensions as the state vector
    for(j in 1:N_time_mod){
      troll_mat[i,,]  = 1 - exp(-exp(vuln_fixed + vuln_troll_array[i,,] * (beta_vuln[1] * vuln_age)))  
      treaty_mat[i,,] = 1 - exp(-exp(vuln_fixed + vuln_treaty_array[i,,] * (beta_vuln[1] * vuln_age)))
      rec_mat[i,,]    = 1 - exp(-exp(vuln_fixed + vuln_rec_array[i,,] * (beta_vuln[1] * vuln_age)))
      hake_ashop_mat[i,,]     = 1 - exp(-exp(vuln_fixed + (beta_vuln_hake * vuln_age_trawl)))
      hake_shoreside_mat[i,,] = 1 - exp(-exp(vuln_fixed + (beta_vuln_hake * vuln_age_trawl)))
    }
  }
}

F_troll_fin       <- array(0,dim= c(N_time_mod,N_loc,N_years_release))
F_treaty_fin      <- F_troll_fin
F_rec_fin         <- F_troll_fin
F_hake_ashop_fin  <- F_troll_fin
F_hake_shoreside_fin <- F_troll_fin
F_tot_fin   <- F_troll_fin

for(i in 1:N_years_release){ # Make an array of the same dimensions as the state vector
  START <- (1+(i-1)*N_month)
  STOP  <- START + N_time_mod -1 
  F_troll_fin[,,i]      <- F_troll_array[START:STOP,] * troll_mat[i,,] ;
  F_treaty_fin[,,i]     <- F_treaty_array[START:STOP,] * treaty_mat[i,,] ;
  F_rec_fin[,,i]        <- F_rec_array[START:STOP,] * rec_mat[i,,]  ;
  F_hake_ashop_fin[,,i]        <- F_hake_ashop_array[START:STOP,] * hake_ashop_mat[i,,]  ;
  F_hake_shoreside_fin[,,i]    <- F_hake_shoreside_array[START:STOP,] * hake_shoreside_mat[i,,]  ;

  F_tot_fin[,,i]        <- F_troll_fin[,,i] + F_rec_fin[,,i] + F_treaty_fin[,,i] + F_hake_ashop_fin[,,i] + F_hake_shoreside_fin[,,i] +
                                  F_troll_array[START:STOP,] * (1-troll_mat[i,,]) * shaker_mort +
                                  F_treaty_array[START:STOP,] * (1-treaty_mat[i,,]) * shaker_mort +
                                  F_rec_array[START:STOP,] * (1-rec_mat[i,,]) * shaker_mort  ;          
}

############################################################################################
############################################################################################
###################################################################################################################
##################################################################################################################
############ LATENT STATES 

#functions used in calculating pred-observed.
alpha_calc <- function(sigma_cv){
   alp = (sigma_cv)^(-2)
  return(alp) 
}
beta_calc <- function(log_lambda, alpha){
  bet = exp(log(alpha) - log_lambda)
  return(bet) 
}


Prob_0_Calc <- function(alpha, beta, inv_frac_samp) {
  B = beta * inv_frac_samp
  return( exp(alpha * (log(B)-log(B+1))))  
}

# Positive part of the model.
# these equations use the alpha_calc and beta_calc functions from above in them.
 E_trunc_pos <- function(alpha, beta, inv_frac_samp){
   B = beta * inv_frac_samp
  temp = exp(-alpha * log(1+(B^(-1)))) 
  Mean = exp(log(alpha)-log(B)-log(1-temp)) 
  return(Mean) 
 }
 
V_trunc_pos <- function(alpha, beta, E_trunc_pos, Prob_0_Calc, inv_frac_samp){
    B = beta * inv_frac_samp
    Var = exp(log(alpha) - log(B) + log((1 + alpha / B)) - log(1-Prob_0_Calc)) -
          (Prob_0_Calc) * E_trunc_pos^2 
    #if(Var < (1e-10 * E_trunc_pos^2)){Var= (1e-10 * E_trunc_pos^2) ; } 
    if(Var < 1e-4 ){Var= 1e-4 ; } 
    return(Var) ; 
}

# these are for the alpha and beta in the likelihood from the approximate Gamma likelihood.
log_alpha_calc_pos <-function(E_trunc_pos, V_trunc_pos){ 
  return( 2*log(E_trunc_pos) - log(V_trunc_pos) ) ;
}

log_beta_calc_pos <- function(E_trunc_pos,V_trunc_pos,log_inv_samp_frac){         
  return( log(E_trunc_pos) - log_inv_samp_frac - log(V_trunc_pos) ) ;
}

bern_log_like <- function(obs,pred){
  ll <- obs * log(pred) + (1-obs) * log(1-pred) ;
  return(ll)
}

gamma_log_like <- function(obs,alp,bet){
  ll <- alp * log(bet) - lgamma(alp) + (alp-1) * log(obs) - bet * obs ;
}



# V_trunc_pos <- function(alpha, beta, E_trunc_pos, Prob_0_Calc){
#   Var = ((alpha / beta) * (1 + alpha/beta)) / (1-Prob_0_Calc) -
#     (Prob_0_Calc) * Mean^2 
#   if(Var < (1e-10 * E_trunc_pos^2)){Var= (1e-10 * E_trunc_pos^2) ; } 
#   return(Var) ; 
# }


cum_M2 <- cum_M2_fixed
#cum_M2_temp <- cum_M2
  for(i in 1:N_time_mod){
    if(i==1){cum_M2_temp[i]  <- cum_M2[age_month_idx[i]] ;}
    if(i > 1){cum_M2_temp[i] <- cum_M2[age_month_idx[i]] - cum_M2[age_month_idx[i-1]] ;}
  }

prob<- NULL
E_trunc_pred <- NULL
V_trunc_pred <- NULL
E_pred <- NULL
V_pred <- NULL
alpha_pos <- NULL
beta_pos  <- NULL

### CALCULATING THE OBSERVATIONS 
### CALCULATING THE PREDICTIONS FOR OBSERVATIONS 
for(i in 1:N_obs_all){
  if( gear_bin_idx[i]  == 1 ){ F_focal <- F_troll_fin[mod_time_idx[i],loc_idx[i],start_year[rel_idx[i]]] ; } 
  if( gear_bin_idx[i]  == 2 ){ F_focal <- F_rec_fin[mod_time_idx[i],loc_idx[i],start_year[rel_idx[i]]] ; }      
  if( gear_bin_idx[i]  == 3 ){ F_focal <- F_treaty_fin[mod_time_idx[i],loc_idx[i],start_year[rel_idx[i]]] ; }   
  if( gear_bin_idx[i]  == 4 ){ F_focal <- F_hake_ashop_fin[mod_time_idx[i],loc_idx[i],start_year[rel_idx[i]]] ; }   
  if( gear_bin_idx[i]  == 5 ){ F_focal <- F_hake_shoreside_fin[mod_time_idx[i],loc_idx[i],start_year[rel_idx[i]]] ; }   
  
    F_tot <- F_tot_fin[mod_time_idx[i],loc_idx[i],start_year[rel_idx[i]]]
  if(spawn_time[mod_time_idx[i]] == 0){
    if(mod_time_idx[i] == 1){
      lambda_temp <- #logit_offset_int[i]  + logit_offset_slope *
        Baranov(  cum_M2_temp[mod_time_idx[i]],
                  F_focal,
                  F_tot,
                  log_N0[rel_idx[i]] - rel_year_all[rel_idx[i]],
                  log(origin_mat[origin_bin_idx[i],temp_dat_season_bin_idx[i],loc_idx[i]] ) );
    }else{
     lambda_temp <- #logit_offset_int[i]  + logit_offset_slope *
      Baranov(  cum_M2_temp[mod_time_idx[i]],
                F_focal,
                F_tot,
                log_N_all[rel_idx[i],mod_time_N_all_idx[i]],
                log(origin_mat[origin_bin_idx[i],temp_dat_season_bin_idx[i],loc_idx[i]] ) );
    }
  }
  if(spawn_time[mod_time_idx[i]] > 0){
    if(mod_time_idx[i]< N_time_mod){
    lambda_temp <- 
      Baranov(  cum_M2_temp[mod_time_idx[i]] * spawn_time_fraction[rel_idx[i]],
                F_focal * spawn_time_fraction[rel_idx[i]],
                F_tot * spawn_time_fraction[rel_idx[i]],
                log_N_all[rel_idx[i],mod_time_N_all_idx[i]],
                log(origin_mat[origin_bin_idx[i],temp_dat_season_bin_idx[i],loc_idx[i]] ) );
    log_N_temp_1 <- log_N_all[rel_idx[i],mod_time_idx[i]] - 
                  F_tot * spawn_time_fraction[rel_idx[i]] -
                  cum_M2_temp[mod_time_idx[i]] * spawn_time_fraction[rel_idx[i]] +
                  log(origin_mat[origin_bin_idx[i],temp_dat_season_bin_idx[i],loc_idx[i]])  +
                  log(1- exp(log(prob_age_year[loc_spawn_bin_idx[i], spawn_time_idx[mod_time_idx[i]]]) - 
                          river_entry[rel_idx[i],loc_idx[i]] * (spawn_smooth^(-1)))) 
    
    #      log(1 - prob_age_year[loc_spawn_bin_idx[i],spawn_time_idx[mod_time_idx[i]]] * river_entry[rel_idx[i],loc_idx[i]] ); 
    lambda_temp <- #logit_offset_int[i]  + logit_offset_slope *
       log(exp( lambda_temp) +
            exp(Baranov(  cum_M2_temp[mod_time_idx[i]] * (1-spawn_time_fraction[rel_idx[i]]) ,
                F_focal * (1-spawn_time_fraction[rel_idx[i]]),
                F_tot * (1-spawn_time_fraction[rel_idx[i]]),
                log_N_temp_1,
                0)));
    }else{
      lambda_temp <- 
        Baranov(  cum_M2_temp[mod_time_idx[i]] * spawn_time_fraction[rel_idx[i]],
                  F_focal * spawn_time_fraction[rel_idx[i]],
                  F_tot * spawn_time_fraction[rel_idx[i]],
                  log_N_all[rel_idx[i],mod_time_N_all_idx[i]],
                  log(origin_mat[origin_bin_idx[i],temp_dat_season_bin_idx[i],loc_idx[i]] ) );
    }
  }

    if( gear_bin_idx[i]  == 1 ){ alpha_temp = alpha_calc(sigma_cv[1])  ; }
    if( gear_bin_idx[i]  == 2 ){ alpha_temp = alpha_calc(sigma_cv[2])  ; }
    if( gear_bin_idx[i]  == 3 ){ alpha_temp = alpha_calc(sigma_cv[1])  ; }
    if( gear_bin_idx[i]  >=4 ){  alpha_temp = alpha_calc(sigma_cv_hake) ; }
    # 
    
    #alpha_temp <- alpha_calc(sigma_cv)
    beta_temp  = beta_calc(lambda_temp, alpha_temp) ;
    
    prob[i] = 1- Prob_0_Calc(alpha_temp,beta_temp, inv_frac_samp[i]) ;

    if(prob[i]<constant){ prob[i] = 1e-12;}   
    if(prob[i]>(1-constant)){ prob[i] = 1- 1e-12;} 
    
    # THIS SECTION CALCULATES THE POSITIVE COMPONENT OF THE MODEL (if it is observed > 0 )
    if(i <= N_obs_pos){  
      
      prob_0_temp =  1 - prob[i] 
      E_trunc_temp = E_trunc_pos(alpha_temp,beta_temp, inv_frac_samp[i]) 
      V_trunc_temp = V_trunc_pos(alpha_temp,beta_temp,E_trunc_temp, prob_0_temp, inv_frac_samp[i]) 
      
      E_trunc_pred[i] <- E_trunc_temp 
      V_trunc_pred[i] <- V_trunc_temp 
      
      E_pred[i] <- E_trunc_temp / exp(log_frac_samp[i])
      V_pred[i] <- V_trunc_temp / exp(log_frac_samp[i])^2
      
      alpha_pos[i] = exp(log_alpha_calc_pos( E_trunc_temp, V_trunc_temp)) ;
      beta_pos[i]  = exp(log_beta_calc_pos(  E_trunc_temp, V_trunc_temp, log_inv_frac_samp_pos[i])) ;
    }
  # Older versions of Probability calculations.
    
  # prob[i] <- exp(lambda_temp) / (exp(lambda_temp)+1) 
  # prob[i] = exp(lambda_temp) / (exp(lambda_temp)+1) ;  // logistic formulation ()
  # prob[i] = 1 - exp(-exp(lambda_temp + log_frac_samp[i] )) ; ##// poisson sampling assumption
  # prob[i] = 1 - exp( exp(lambda_temp) * log_frac_samp_comp[i] ) ; ##// binomial sampling assumption
}

############################################################################################
# connect the predictions from the marginal posteriors to the observations  
# These are called in the "Log-likelihood tables.R" script.

dat.all$prob   = prob
dat.pos$E_pred = E_pred
dat.pos$V_pred = V_pred
dat.pos$alpha_pos <- alpha_pos
dat.pos$beta_pos  <- beta_pos

dat.all$log.like.bern <- bern_log_like(dat.all$catch_bin,dat.all$prob)
dat.pos$log.like.gamma <- gamma_log_like(dat.pos$catch,dat.pos$alpha_pos,dat.pos$beta_pos)


############################################################################################
###################################################################################################################
  
  pred.obs.plot <- function(data, prob, smoother.int ){
  ### MAKE A BOX CAR SMOOTHER FUNCTION
  INTERVAL	<-	seq(smoother.int/2,1-smoother.int/2,by=0.005)
  OUTPUT	  <- data.frame(interval=INTERVAL)
  INC		    =	 smoother.int

  for(i in 1:length(INTERVAL)){
    temp <-  data$catch_bin[prob > (INTERVAL[i] - INC/2) & prob < (INTERVAL[i] + INC/2) ]
    OUTPUT$pred[i] <-  sum( temp) / length(temp)
  }  

  JIT <- 0.15
  catch.plot <- data$catch_bin + runif(nrow(data),-JIT,JIT)
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
  
# Plot binomial predictions by Gear, age, location
pdf(paste(base.dir,"/Salmon-Climate/Output plots/", "Obs-Pred Binomial " ,NAME,".pdf",sep=""),height = 5, width=9)
  INT <- 0.05
  pred.obs.plot(data=dat.all,prob=dat.all$prob,smoother.int=INT)
  title("ALL")
  nom <- as.character(spawn_loc$ocean.region)
  for(i in 1:length(nom)){
    pred.obs.plot(data=dat.all[dat.all$ocean.reg == nom[i],],prob=dat.all$prob[dat.all$ocean.reg ==nom[i]],smoother.int=INT)
    title(paste("Origin Region =",nom[i]))
  }
  pred.obs.plot(data=dat.all[dat.all$effort.type == "troll",],prob=dat.all$prob[dat.all$effort.type == "troll"],smoother.int=INT)
  title("Troll")
  pred.obs.plot(data=dat.all[dat.all$effort.type == "treaty",],prob=dat.all$prob[dat.all$effort.type == "treaty"],smoother.int=INT)
  title("Treaty")
  pred.obs.plot(data=dat.all[dat.all$effort.type == "rec",],prob=dat.all$prob[dat.all$effort.type == "rec"],smoother.int=INT)
  title("Rec")
  pred.obs.plot(data=dat.all[dat.all$effort.type == "ashop",],prob=dat.all$prob[dat.all$effort.type == "ashop"],smoother.int=INT)
  title("Hake ASHOP")
  pred.obs.plot(data=dat.all[dat.all$effort.type == "shoreside",],prob=dat.all$prob[dat.all$effort.type == "shoreside"],smoother.int=INT)
  title("Hake Shoreside")


  # Year - Gear Type bins
  BY <- c(1970,1980,1990,2000,2010,2020)
  
  for(i in 1:(length(BY)-1)){
    d.temp <- dat.all %>% filter(effort.type =="troll",brood_year >= BY[i],brood_year < BY[i+1])
    pred.obs.plot(data=d.temp,prob=d.temp$prob,smoother.int=INT)
    title(paste("Troll",BY[i],"to",BY[i+1]-1))
    
    d.temp <- dat.all %>% filter(effort.type =="rec",brood_year >= BY[i],brood_year < BY[i+1])
    pred.obs.plot(data=d.temp,prob=d.temp$prob,smoother.int=INT)
    title(paste("Rec",BY[i],"to",BY[i+1]-1))
    
    d.temp <- dat.all %>% filter(effort.type =="treaty",brood_year >= BY[i],brood_year < BY[i+1])
    pred.obs.plot(data=d.temp,prob=d.temp$prob,smoother.int=INT)
    title(paste("Treaty",BY[i],"to",BY[i+1]-1))
    
    d.temp <- dat.all %>% filter(effort.type =="ashop",brood_year >= BY[i],brood_year < BY[i+1])
    pred.obs.plot(data=d.temp,prob=d.temp$prob,smoother.int=INT)
    title(paste("Hake ASHOP",BY[i],"to",BY[i+1]-1))
    
    d.temp <- dat.all %>% filter(effort.type =="shoreside",brood_year >= BY[i],brood_year < BY[i+1])
    pred.obs.plot(data=d.temp,prob=d.temp$prob,smoother.int=INT)
    title(paste("Hake Shore",BY[i],"to",BY[i+1]-1))
  }
  for(i in 1:(length(BY)-1)){
    for(j in 1:length(nom)){
      d.temp <- dat.all %>% filter(ocean.reg==nom[j],brood_year >= BY[i],brood_year < BY[i+1])
      pred.obs.plot(data=d.temp,prob=d.temp$prob,smoother.int=INT)
      title(paste(nom[j],BY[i],"to",BY[i+1]-1))
    }
  }
  
  age <- sort(as.character(unique(dat.all$year)))
  for(i in 1:length(age)){
    pred.obs.plot(data=dat.all[dat.all$year == age[i],],prob=dat.all$prob[dat.all$year == age[i]],smoother.int=INT)
    title(paste("Year =",age[i]))
  }
  
  # By Recovery Year
  A <- dat.all
  A$rec_year <- A$start_year+ A$brood_year + A$year
  for(i in 1:length(years.recover)){
    pred.obs.plot(data=A[A$rec_year == years.recover[i],],prob=dat.all$prob[A$rec_year == years.recover[i]],smoother.int=INT)
    title(paste("Recovery Year =",years.recover[i]))
  }
  # By Recovery Location
  for(i in 1:nrow(LOCATIONS)){
    pred.obs.plot(data=dat.all[dat.all$location == i,],prob=dat.all$prob[dat.all$location == i],smoother.int=INT)
    title(paste("Location =",LOCATIONS$location.name[LOCATIONS$location.number==i]))
  }
  
  # By Season
  for(i in 1:3){
    pred.obs.plot(data=dat.all[dat.all$season.idx == i ,],
                      prob=dat.all$prob[dat.all$season.idx == i ],smoother.int=INT)
    title(paste("Season =",i))
    mtext(paste("1=wint+spr, 2=sum, 3=fall"),padj=-0.5)
  }
dev.off()

############################################################################################################
############################################################################################################
############################################################################################################
#### POSITIVE.
############################################################################################################
############################################################################################################
############################################################################################################


pred.obs.pos.plot <- function(data,pred.pos,pch,col=1){ #, smoother.int ){

  par(mfrow=c(1,2))
  x.lim=c(min(log(pred.pos),log(data$catch)),max(log(pred.pos),log(data$catch)))
  plot(log(data$catch)~log(pred.pos),pch=pch,ylim=x.lim,xlim=x.lim,axes=F,xlab="",ylab="",col=col)
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

  x.lim=c(min(pred.pos,data$catch),max(pred.pos,data$catch))
  plot((data$catch)~pred.pos,pch=pch,ylim=x.lim,xlim=x.lim,axes=F,xlab="",ylab="",col=col)
  par(new=T)
  # plot(OUTPUT$pred~OUTPUT$interval,type=l,ylim=y.lim,xlim=x.lim,lwd=4,col=white,axes=F,xlab=,ylab=)
  # par(new=T)
  # plot(OUTPUT$pred~OUTPUT$interval,type=l,ylim=y.lim,xlim=x.lim,lwd=2,axes=F,xlab=,ylab=)
  abline(0,1,lty=2,lwd=2,col=2)
  axis(1)
  axis(2,las=2)
  box(bty="o",lwd=2)
  title(xlab="Predicted")
  title(ylab="Observed ")
}

# Plot positive predictions by area
pdf(paste(base.dir,"/Salmon-Climate/Output plots/", "Obs-Pred Positive " ,NAME,".pdf",sep=""),height = 5, width=9)
  pred.obs.pos.plot(data=dat.pos,pred.pos=dat.pos$E_pred,pch=".")
  title("ALL")
  nom <- as.character(spawn_loc$ocean.region)

  for(i in 1:length(nom)){
    pred.obs.pos.plot(data=dat.pos[dat.pos$ocean.reg == nom[i],],pred.pos=(dat.pos$E_pred[dat.pos$ocean.reg ==nom[i]]),pch=21)
    title(paste("Origin Region =",nom[i]))
  }
pred.obs.pos.plot(data=dat.pos[dat.pos$effort.type == "troll",],pred.pos=(dat.pos$E_pred[dat.pos$effort.type == "troll"]),pch=21)
title("Troll")
pred.obs.pos.plot(data=dat.pos[dat.pos$effort.type == "treaty",],pred.pos=(dat.pos$E_pred[dat.pos$effort.type == "treaty"]),pch=21)
title("Treaty")
pred.obs.pos.plot(data=dat.pos[dat.pos$effort.type == "rec",],pred.pos=(dat.pos$E_pred[dat.pos$effort.type == "rec"]),pch=21)
title("Rec")
pred.obs.pos.plot(data=dat.pos[dat.pos$effort.type == "ashop",],pred.pos=(dat.pos$E_pred[dat.pos$effort.type == "ashop"]),pch=21)
title("ASHOP (hake)")
pred.obs.pos.plot(data=dat.pos[dat.pos$effort.type == "shoreside",],pred.pos=(dat.pos$E_pred[dat.pos$effort.type == "shoreside"]),pch=21)
title("Shoreside (hake)")

#### PLOTS BY AGE
age <- sort(as.character(unique(dat.pos$year)))
for(i in 1:length(age)){
  pred.obs.pos.plot(data=dat.pos[dat.pos$year == age[i],],pred.pos=(dat.pos$E_pred[dat.pos$year == age[i]]),pch=21)
  title(paste("Year =",age[i]))
}

# By Recovery Year
A <- dat.pos
A$rec_year <- A$start_year+ A$brood_year + A$year
for(i in 1:length(years.recover)){
  pred.obs.pos.plot(data=A[A$rec_year == years.recover[i],],
                    pred.pos=dat.pos$E_pred[A$rec_year == years.recover[i]],pch=21)
  title(paste("Recovery Year =",years.recover[i]))
}
# By Recovery Location
for(i in 1:nrow(LOCATIONS)){
  pred.obs.pos.plot(data=dat.pos[dat.pos$location == i ,],
                    pred.pos=dat.pos$E_pred[dat.pos$location == i ],pch=21,col=1)
  title(paste("Location =",LOCATIONS$location.name[LOCATIONS$location.number==i]))
}
# By Season
for(i in 1:3){
  pred.obs.pos.plot(data=dat.pos[dat.pos$season.idx == i ,],
                    pred.pos=dat.pos$E_pred[dat.pos$season.idx == i ],pch=21,col=1)
  title(paste("Season =",i))
  mtext(paste("1=wint+spr, 2=sum, 3=fall"),padj=-0.5)
}

dev.off()

############################################################################
############################################################################
############################################################################
############################################################################
### --- Make plots for manuscript with ggplot
############################################################################
############################################################################
############################################################################
############################################################################
library(splines)
library(MASS)
library(gtable)

all.bin <- ggplot(dat.all) +
  geom_jitter(aes(x=prob,y=catch_bin),width=0,height=0.1,shape=".",alpha=0.1) +
  geom_smooth(aes(x=prob,y=catch_bin),method="gam", formula=y~s(x,k=4)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="red") +
  scale_y_continuous("Observed",breaks=c(0,0.25,0.5,0.75,1)) +
  scale_x_continuous("Predicted",breaks=c(0,0.25,0.5,0.75,1)) +
  theme_bw()
#all.bin

all.pos <- ggplot(dat.pos) +
  geom_point(aes(x=log(E_pred),y=log(catch)),alpha=0.1) +
  geom_smooth(aes(x=log(E_pred),y=log(catch)),method="gam", formula=y~s(x,k=6)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="red") +
  scale_y_continuous("log(Observed)") +
  scale_x_continuous("log(Predicted)") +
  theme_bw()
#all.pos

dat.all$ocean.reg <- factor(dat.all$ocean.reg,levels=spawn_loc$ocean.region)
dat.pos$ocean.reg <- factor(dat.pos$ocean.reg,levels=spawn_loc$ocean.region)

all.bin.origin.facet <- ggplot(dat.all) +
  geom_jitter(aes(x=prob,y=catch_bin),width=0,height=0.1,shape=".",alpha=0.1) +
  geom_smooth(aes(x=prob,y=catch_bin),method="gam", formula=y~s(x,k=4)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="red") +
  scale_y_continuous("Observed",breaks=c(0,0.25,0.5,0.75,1)) +
  scale_x_continuous("Predicted",breaks=c(0,0.25,0.5,0.75,1)) +
  theme_bw() +
  facet_wrap(~ocean.reg)
#all.bin.origin.facet

all.bin.time.facet <- ggplot(dat.all) +
  geom_jitter(aes(x=prob,y=catch_bin),width=0,height=0.1,alpha=0.1) +
  geom_smooth(aes(x=prob,y=catch_bin),method="gam", formula=y~s(x,k=4)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="red") +
  scale_y_continuous("Observed",breaks=c(0,0.25,0.5,0.75,1)) +
  scale_x_continuous("Predicted",breaks=c(0,0.25,0.5,0.75,1)) +
  theme_bw() +
  facet_wrap(~time,ncol=4)
#all.bin.time.facet

all.bin.time.facet.origin <- ggplot(dat.all) +
  geom_jitter(aes(x=prob,y=catch_bin,color=ocean.reg),width=0,height=0.2,alpha=0.2) +
  geom_smooth(aes(x=prob,y=catch_bin),method="gam", formula=y~s(x,k=4)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="red") +
  scale_y_continuous("Observed",breaks=c(0,0.25,0.5,0.75,1)) +
  scale_x_continuous("Predicted",breaks=c(0,0.25,0.5,0.75,1)) +
  theme_bw() +
  facet_wrap(~time,ncol=4)

all.bin.time.facet.reg <- ggplot(dat.all) +
  geom_jitter(aes(x=prob,y=catch_bin,color=loc),width=0,height=0.2,alpha=0.2) +
  geom_smooth(aes(x=prob,y=catch_bin),method="gam", formula=y~s(x,k=4)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="red") +
  scale_y_continuous("Observed",breaks=c(0,0.25,0.5,0.75,1)) +
  scale_x_continuous("Predicted",breaks=c(0,0.25,0.5,0.75,1)) +
  theme_bw() +
  facet_wrap(~time,ncol=4)

all.bin.time.facet.reg.by.origin <- list()
all.pos.time.facet.reg.by.origin <- list()
all.bin.time.facet.reg.by.origin.bw <- list()
all.pos.time.facet.reg.by.origin.bw <- list()

for(i in 1:nrow(spawn_loc)){
  temp.bin <- dat.all %>% filter(ocean.reg==spawn_loc$ocean.region[i])
  temp.pos <- dat.pos %>% filter(ocean.reg==spawn_loc$ocean.region[i])
  
all.bin.time.facet.reg.by.origin[[as.character(spawn_loc$ocean.region[i])]] <- ggplot(temp.bin) +
  geom_jitter(aes(x=prob,y=catch_bin,color=loc),width=0,height=0.2,alpha=0.3) +
  geom_smooth(aes(x=prob,y=catch_bin),method="gam", formula=y~s(x,k=4)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="red") +
  scale_y_continuous("Observed",breaks=c(0,0.25,0.5,0.75,1)) +
  scale_x_continuous("Predicted",breaks=c(0,0.25,0.5,0.75,1)) +
  theme_bw() +
  ggtitle(spawn_loc$ocean.region[i])+
  facet_wrap(~time,ncol=4)

all.pos.time.facet.reg.by.origin[[as.character(spawn_loc$ocean.region[i])]] <- ggplot(temp.pos) +
  geom_point(aes(x=log(E_pred),y=log(catch),color=loc),alpha=0.3) +
  geom_smooth(aes(x=log(E_pred),y=log(catch)),method="gam", formula=y~s(x,k=5)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="red") +
  scale_y_continuous("log(Observed)") +
  scale_x_continuous("log(Predicted)") +
  theme_bw() +
  ggtitle(spawn_loc$ocean.region[i])+
  facet_wrap(~time,ncol=4)

all.bin.time.facet.reg.by.origin.bw[[as.character(spawn_loc$ocean.region[i])]] <- ggplot(temp.bin) +
  geom_jitter(aes(x=prob,y=catch_bin),width=0,height=0.2,alpha=0.3) +
  geom_smooth(aes(x=prob,y=catch_bin),method="gam", formula=y~s(x,k=4)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="red") +
  scale_y_continuous("Observed",breaks=c(0,0.25,0.5,0.75,1)) +
  scale_x_continuous("Predicted",breaks=c(0,0.25,0.5,0.75,1)) +
  theme_bw() +
  ggtitle(spawn_loc$ocean.region[i])+
  facet_wrap(~time,ncol=4)

all.pos.time.facet.reg.by.origin.bw[[as.character(spawn_loc$ocean.region[i])]] <- ggplot(temp.pos) +
  geom_point(aes(x=log(E_pred),y=log(catch)),alpha=0.3) +
  geom_smooth(aes(x=log(E_pred),y=log(catch)),method="gam", formula=y~s(x,k=5)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="red") +
  scale_y_continuous("log(Observed)") +
  scale_x_continuous("log(Predicted)") +
  theme_bw() +
  ggtitle(spawn_loc$ocean.region[i])+
  facet_wrap(~time,ncol=4)



}


all.pos.origin.facet <- ggplot(dat.pos) +
  geom_point(aes(x=log(E_pred),y=log(catch)),alpha=0.1) +
  geom_smooth(aes(x=log(E_pred),y=log(catch)),method="gam", formula=y~s(x,k=6)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="red") +
  scale_y_continuous("log(Observed)") +
  scale_x_continuous("log(Predicted)") +
  theme_bw() +
  facet_wrap(~ocean.reg)
#all.pos.origin.facet

all.pos.time.facet <- ggplot(dat.pos) +
  geom_point(aes(x=log(E_pred),y=log(catch)),alpha=0.1) +
  geom_smooth(aes(x=log(E_pred),y=log(catch)),method="gam", formula=y~s(x,k=6)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="red") +
  scale_y_continuous("log(Observed)") +
  scale_x_continuous("log(Predicted)") +
  theme_bw() +
  facet_wrap(~time,ncol=4)


all.pos.time.facet.reg <- ggplot(dat.pos) +
  geom_point(aes(x=log(E_pred),y=log(catch),color=loc),alpha=0.1) +
  geom_smooth(aes(x=log(E_pred),y=log(catch)),method="gam", formula=y~s(x,k=6)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="red") +
  scale_y_continuous("log(Observed)") +
  scale_x_continuous("log(Predicted)") +
  theme_bw() +
  facet_wrap(~time,ncol=4)


#############################################

DPI = 300

quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/Obs-Pred Bin-all " ,NAME,".jpeg",sep=""),height=5,width=5,dpi=DPI,type="jpeg")
  print(all.bin)
dev.off()

quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/Obs-Pred Pos-all " ,NAME,".jpeg",sep=""),height=5,width=5,dpi=DPI,type="jpeg")
  print(all.pos)
dev.off()

quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/Obs-Pred Bin-facet-origin " ,NAME,".jpeg",sep=""),height=7,width=7,dpi=DPI,type="jpeg")
  print(all.bin.origin.facet)
dev.off()
 
quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/Obs-Pred Bin-facet-time " ,NAME,".jpeg",sep=""),height=7,width=7,dpi=DPI,type="jpeg")
  print(all.bin.time.facet)
dev.off()

quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/Obs-Pred Bin-facet-time-origin " ,NAME,".jpeg",sep=""),height=7,width=7,dpi=DPI,type="jpeg")
  print(all.bin.time.facet.origin)
dev.off()

quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/Obs-Pred Bin-facet-time-region " ,NAME,".jpeg",sep=""),height=7,width=7,dpi=DPI,type="jpeg")
  print(all.bin.time.facet.reg)
dev.off()

quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/Obs-Pred Pos-facet-origin " ,NAME,".jpeg",sep=""),height=7,width=7,dpi=DPI,type="jpeg")
  print(all.pos.origin.facet)
dev.off()

quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/Obs-Pred Pos-facet-time " ,NAME,".jpeg",sep=""),height=7,width=7,dpi=DPI,type="jpeg")
  print(all.pos.time.facet)
dev.off()

quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/Obs-Pred Pos-facet-time-region " ,NAME,".jpeg",sep=""),height=7,width=7,dpi=DPI,type="jpeg")
  print(all.pos.time.facet.reg)
dev.off()


pdf(file=paste(base.dir,"/Salmon-Climate/Output plots/Obs-Pred Bin-facet-time-region-by-origin " ,NAME,".pdf",sep=""),
    onefile=T)
  print(all.bin.time.facet.reg.by.origin)
dev.off()

pdf(file=paste(base.dir,"/Salmon-Climate/Output plots/Obs-Pred Pos-facet-time-region-by-origin " ,NAME,".pdf",sep=""),
    onefile=T)
  print(all.pos.time.facet.reg.by.origin)
dev.off()

pdf(file=paste(base.dir,"/Salmon-Climate/Output plots/Obs-Pred Bin-facet-time-region-by-origin.BW " ,NAME,".pdf",sep=""),
    onefile=T)
print(all.bin.time.facet.reg.by.origin.bw)
dev.off()

pdf(file=paste(base.dir,"/Salmon-Climate/Output plots/Obs-Pred Pos-facet-time-region-by-origin.BW " ,NAME,".pdf",sep=""),
    onefile=T)
print(all.pos.time.facet.reg.by.origin.bw)
dev.off()



