## TAKE MCMC SAMPLES TO CALCULATE DEVIANCE.

####################################### FUNCTIONS
Baranov <- function(M2,  F_focal,  F_tot,  log_N,  log_origin){
  return  (log(F_focal) - log(M2 + F_tot) + log_N + log_origin + log(1 - exp(-(M2 + F_tot) )))  
}

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

normal_log_like <- function(OBS,MEAN,SD){
  -log(SD) - 0.5 * ((MEAN - OBS)^2) / SD  
}

log.diri <- function(Alpha,E){
  ll <- lgamma(diri_constant) - sum(lgamma(Alpha)) + sum( (Alpha-1) * log(E) ) 
  return(ll)
}

#########################################################################
# make arrays on the scale of the release groups. Turn (year,month,location) into (release id,model month, location)
N_rel <- nrow(REL)
########

troll_mat  <- vuln_troll_array * 0
treaty_mat <- vuln_troll_array * 0
rec_mat    <- vuln_troll_array * 0
hake_ashop_mat     <- vuln_troll_array * 0
hake_shoreside_mat <- vuln_troll_array * 0

#### #########################################################3
#### #########################################################3
Q.tot <- 1000 # Number of MCMC samples to pull
Q.vec <- seq(1,dim(samp$rel_year_all)[1],length.out=Q.tot) %>% round()

LL.output <- matrix(data=0,nrow=Q.tot,ncol=4) %>% as.data.frame() ; colnames(LL.output) <- c("Catch.bern","Catch.pos","N_ratio","Dirichlet")

for(k in 1:Q.tot){
  Q=Q.vec[k]
  print(paste(k,"of",Q.tot))
  
  
  troll_mat  <- vuln_troll_array * 0
  treaty_mat <- vuln_troll_array * 0
  rec_mat    <- vuln_troll_array * 0
  hake_ashop_mat     <- vuln_troll_array * 0
  hake_shoreside_mat <- vuln_troll_array * 0
  
  
if(vuln_fixed>2){ # THis is for the logistic formulation
  for(i in 1:N_years_release){ ## Make an array of the same dimensions as the state vector
    for(j in 1:N_time_mod){
      troll_mat[i,,]  = 1 / (1 + exp(-(vuln_fixed + vuln_troll_array[i,,] * (samp$beta_vuln[Q,1] * vuln_age))))  
      treaty_mat[i,,] = 1 / (1 + exp(-(vuln_fixed + vuln_treaty_array[i,,] * (samp$beta_vuln[Q,1] * vuln_age))))  
      rec_mat[i,,]    = 1 / (1 + exp(-(vuln_fixed + vuln_rec_array[i,,] * (samp$beta_vuln[Q,2] * vuln_age))))  
      # if(length(beta_vuln_hake)==1){
      #   hake_ashop_mat[i,,]     = 1 / (1 + exp(-(vuln_fixed + (beta_vuln_hake  * vuln_age_trawl))))
      #   hake_shoreside_mat[i,,] = 1 / (1 + exp(-(vuln_fixed + (beta_vuln_hake  * vuln_age_trawl))))
      # }
      if(length(beta_vuln_hake)==2){
        hake_ashop_mat[i,,]     = 1 / (1 + exp(-(vuln_fixed + (samp$beta_vuln[Q,1]  * vuln_age_trawl + samp$beta_vuln[Q,2]  * vuln_age_trawl^2))))
        hake_shoreside_mat[i,,] = 1 / (1 + exp(-(vuln_fixed + (samp$beta_vuln[Q,1]  * vuln_age_trawl + samp$beta_vuln[Q,2]  * vuln_age_trawl^2))))
      }
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
  F_troll_fin[,,i]      <- samp$F_troll_array[Q,START:STOP,] * troll_mat[i,,] ;
  F_treaty_fin[,,i]     <- samp$F_treaty_array[Q,START:STOP,] * treaty_mat[i,,] ;
  F_rec_fin[,,i]        <- samp$F_rec_array[Q,START:STOP,] * rec_mat[i,,]  ;
  F_hake_ashop_fin[,,i]        <- samp$F_hake_ashop_array[Q,START:STOP,] * hake_ashop_mat[i,,]  ;
  F_hake_shoreside_fin[,,i]    <- samp$F_hake_shoreside_array[Q,START:STOP,] * hake_shoreside_mat[i,,]  ;
  
  F_tot_fin[,,i]        <- F_troll_fin[,,i] + F_rec_fin[,,i] + F_treaty_fin[,,i] + F_hake_ashop_fin[,,i] + F_hake_shoreside_fin[,,i] +
    samp$F_troll_array[Q,START:STOP,] * (1-troll_mat[i,,]) * shaker_mort +
    samp$F_treaty_array[Q,START:STOP,] * (1-treaty_mat[i,,]) * shaker_mort +
    samp$F_rec_array[Q,START:STOP,] * (1-rec_mat[i,,]) * shaker_mort  ;          
}

############################################################################################
############################################################################################
###################################################################################################################
##################################################################################################################
############ LATENT STATES 



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
                  log_N0[rel_idx[i]] - samp$rel_year_all[Q,rel_idx[i]],
                  log(samp$origin_mat[Q,origin_bin_idx[i],temp_dat_season_bin_idx[i],loc_idx[i]] ) );
    }else{
      lambda_temp <- #logit_offset_int[i]  + logit_offset_slope *
        Baranov(  cum_M2_temp[mod_time_idx[i]],
                  F_focal,
                  F_tot,
                  samp$log_N_all[Q,rel_idx[i],mod_time_N_all_idx[i]],
                  log(samp$origin_mat[Q,origin_bin_idx[i],temp_dat_season_bin_idx[i],loc_idx[i]] ) );
    }
  }
  if(spawn_time[mod_time_idx[i]] > 0){
    if(mod_time_idx[i] < N_time_mod ){
    lambda_temp <- 
      Baranov(  cum_M2_temp[mod_time_idx[i]] * spawn_time_fraction[rel_idx[i]],
                F_focal * spawn_time_fraction[rel_idx[i]],
                F_tot * spawn_time_fraction[rel_idx[i]],
                samp$log_N_all[Q,rel_idx[i],mod_time_N_all_idx[i]],
                log(samp$origin_mat[Q,origin_bin_idx[i],temp_dat_season_bin_idx[i],loc_idx[i]] ) );
    log_N_temp_1 <- samp$log_N_all[Q,rel_idx[i],mod_time_N_all_idx[i]] - 
      F_tot * spawn_time_fraction[rel_idx[i]] -
      cum_M2_temp[mod_time_idx[i]] * spawn_time_fraction[rel_idx[i]] +
      log(samp$origin_mat[Q,origin_bin_idx[i],temp_dat_season_bin_idx[i],loc_idx[i]])  +
      log(1- exp(log(samp$prob_age_year[Q,loc_spawn_bin_idx[i], spawn_time_idx[mod_time_idx[i]]]) - 
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
                  samp$log_N_all[Q,rel_idx[i],mod_time_N_all_idx[i]],
                  log(samp$origin_mat[Q,origin_bin_idx[i],temp_dat_season_bin_idx[i],loc_idx[i]] ) );
    }
  }
  
  if( gear_bin_idx[i]  == 1 ){ alpha_temp = alpha_calc(samp$sigma_cv[Q,1])  ; }
  if( gear_bin_idx[i]  == 2 ){ alpha_temp = alpha_calc(samp$sigma_cv[Q,2])  ; }
  if( gear_bin_idx[i]  == 3 ){ alpha_temp = alpha_calc(samp$sigma_cv[Q,1])  ; }
  if( gear_bin_idx[i]  >= 4 ){  alpha_temp = alpha_calc(samp$sigma_cv_hake[Q]) ; }
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

  LL.output$Catch.bern[k] <- bern_log_like(dat.all$catch_bin,prob) %>% sum()
  LL.output$Catch.pos[k]  <- gamma_log_like(dat.pos$catch,alpha_pos,beta_pos) %>% sum()
  
  # N Ratio Likelihood Component
  #LL.output$N_ratio[k] <-  normal_log_like(samp$log_N_ratio[Q,],REL$log_N_ratio_mean,REL$log_N_ratio_sd) %>% sum()
  
  # Spawning Maturity
  #diri_constant = 50
  ALPHA <- samp$prop_D[Q,,] * diri_constant
  E_prop <- as.matrix(E_alpha / rowSums(E_alpha))

  ll.dirichlet <- rep(0,nrow(REL))
  for(i in 1:nrow(REL)){
    ll.dirichlet[i] <- log.diri(ALPHA[i,],E_prop[COV$loc.spawn.idx[i],])
  }
  LL.output$Dirichlet[k] <- ll.dirichlet %>% sum()
  
} #END LOOP OVER Q - MCMC draws

### CALCULATIONS NECESSARY FOR DIC.
  LL.output <- LL.output %>% mutate(Tot.Like = Catch.bern + Catch.pos + Dirichlet) %>% mutate(Deviance= -2 * Tot.Like)

  Dev.bar   <- mean(LL.output$Deviance)
  var.Dev.half <- 2*var(LL.output$Tot.Like)
  
  #p.WAIC.1 <- LL.output %>%
  
  # Pull in output from "Log-likelihood tables.R" which calculates the log likelihood values for the data at each
  # the joint posterior mean
  
  load(paste0(markdown.dir,"/LL ",NAME,".Rdata"))
  
  catch.ll.point     <- LL$dat.origin.LL %>% filter(origin=="TOTAL")
  #n.ratio.ll.point <- LL$ll.N.ratio.total
  dirichlet.ll.point <- LL$ll.dirichlet.total
  
  dev.point.at.post <- -2*(catch.ll.point$LL.total + dirichlet.ll.point)

  #effective.param = var.Dev.half
  effective.param1 = Dev.bar - dev.point.at.post
  effective.param2 = var.Dev.half 
  
  DIC <- list(NAME=NAME,
              Dev.bar = Dev.bar,
              var.Dev.half = var.Dev.half, 
              catch.ll.point = catch.ll.point,
              #n.ratio.ll.point = n.ratio.ll.point,
              dirichlet.ll.point = dirichlet.ll.point,
              dev.point.at.post = dev.point.at.post,
              effective.param1 = effective.param1,
              effective.param2 = effective.param2,
              DIC.1 = dev.point.at.post + 2*effective.param1,
              DIC.2 = dev.point.at.post + 2*effective.param2)
  
  save(DIC,file=paste0(markdown.dir,"/DIC ",NAME,".Rdata"))
  
  