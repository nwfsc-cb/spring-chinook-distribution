

library(cmdstanr)
setwd("./Github/spring-chinook-distribution")
library(here)
###############################################################################
###############################################################################

load(here("Mixed Model","all_data.Rdata"))
###############################################################################
###############################################################################

MODEL    = "Joint"

Warm        = 200
Iter        = 200
N_CHAIN     = 3
Treedepth   = 10
Adapt_delta = 0.7


file <- file.path(here("Mixed Model", MOD.NAME))
mod <- cmdstan_model(file)

fit <-mod$sample(data = stan_data, 
                 #verbose = TRUE, 
                 chains = N_CHAIN, 
                 thin = 1, 
                 iter_warmup = Warm, 
                 iter_sampling = Iter, 
                 max_treedepth=Treedepth,
                 adapt_delta=Adapt_delta,
                 init_buffer=75,
                 step_size = 0.01,
                 metric="diag_e",
                 #pars = stan_pars,
                 #boost_lib = NULL,
                 #sample_file = SAMP.FILE,
                 init = stan_init_f1(n.chain=N_CHAIN,N_loc_spawn=N_loc_spawn,
                                     MU_m =MU_m,Sigma_m=Sigma_m,
                                     N_rel=N.REL,N_loc=N.LOC,
                                     N_f_rec_idx_param=N_f_rec_idx_param,
                                     N_knot_sf=N_knot_sf, N_knot_ws=N_knot_ws,
                                     #W_star_sf = W_star_sf, W_star_ws = W_star_ws, W_star_salish =W_star_salish,
                                     N_troll_idx=N_troll_idx,N_rec_us_idx=N_rec_us_idx, 
                                     N_origin=N_origin,N_sigma_cv_idx=N_sigma_cv_idx,
                                     N_season=N_season,N_year=N_year,N_juv=N_juv,N_season_total = N_season_total,
                                     N_vuln_grp=N_vuln_grp)
         
) 


setwd(paste0(base.dir,"/spring-chinook-distribution/Mixed Model"))
stanMod = stan(file = MOD.NAME,
               data = stan_data, 
               verbose = FALSE, chains = N_CHAIN, thin = 1, 
               warmup = Warm, iter = Warm + Iter, 
               control = list(max_treedepth=Treedepth,
                              adapt_delta=Adapt_delta,
                              adapt_init_buffer=75,
                              stepsize = 0.01,
                              metric="diag_e"),
               pars = stan_pars,
               boost_lib = NULL,
               sample_file = SAMP.FILE,
               init = stan_init_f1(n.chain=N_CHAIN,N_loc_spawn=N_loc_spawn,
                                   MU_m =MU_m,Sigma_m=Sigma_m,
                                   N_rel=N.REL,N_loc=N.LOC,
                                   N_f_rec_idx_param=N_f_rec_idx_param,
                                   N_knot_sf=N_knot_sf, N_knot_ws=N_knot_ws,
                                   #W_star_sf = W_star_sf, W_star_ws = W_star_ws, W_star_salish =W_star_salish,
                                   N_troll_idx=N_troll_idx,N_rec_us_idx=N_rec_us_idx, 
                                   N_origin=N_origin,N_sigma_cv_idx=N_sigma_cv_idx,
                                   N_season=N_season,N_year=N_year,N_juv=N_juv),
               init_r = 1
) 

pars <- rstan::extract(stanMod, permuted = T)
#get_adaptation_info(stanMod)
samp_params <- get_sampler_params(stanMod)
#
# base_params <- c(#"tau_process",
#                  #"tau_process_prod",
#                   "log_q_rec_start","log_q_troll_start","log_q_rec_can_start","log_q_treaty_start",
#                  "log_q_rec_slope","log_q_troll_slope","log_q_rec_can_slope","log_q_treaty_slope",
#                  "log_q_rec_can_irec_start","log_q_hake_ashop_start","log_q_hake_shoreside_start",
#                  "log_q_pollock_GOA_start","log_q_rockfish_AK_start",
#                  "q_int",
#                  #"log_q_troll_pos", "log_q_rec_pos","log_q_treaty_pos","log_q_rec_can_pos",#"log_q_rec_PUSO_pos",
#                  #"logit_offset_slope",
#                  #"sigma_pos",
#                  "sigma_cv","sigma_cv_hake",
#                  #"sigma_slope",
#                  #"sigma_slope_samp",
#                  # "alpha_pay_mean",
#                  # "alpha_pay_sd",
#                  # "beta_pay_mean",
#                  # "beta_pay_sd",
#                  "log_rel_year_mu", "log_rel_year_sigma",
#                  "beta_vuln","beta_vuln_hake",
#                  #"log_M2",
#                  "log_F_rec_mean","log_F_troll_mean","F_rec_sigma","F_troll_sigma"
#                  #"tau_q_troll","tau_q_rec"
#                  # "kappa_troll", "kappa_treaty",
#                  # "kappa_rec", "kappa_rec_can"
#                  )#"prob_age_year") #"beta_age_month""vuln_int"
#
#print(traceplot(stanMod,pars=c("lp__"),inc_warmup=F))
# summary(stanMod,pars=base_params)$summary
# 
stanMod_summary <- summary(stanMod)$summary
# summary(stanMod,pars=base_params)$summary
#summary(stanMod,pars=c("alpha_pay","beta_pay"))

origin_summary <-summary(stanMod,pars=c("origin_sea_int")) # "origin_sea_slope"
# origin_summary <- summary(stanMod,pars=c("origin_sea_int","origin_sea_slope"))$summary
origin_mat_summary <- as.data.frame(summary(stanMod,pars=c("origin_mat"))$summary)


# If you want to save the posterior as start values for later.
#  W_star_sf <- pars$w_star_sf[1,,,]
#  W_star_ws <- pars$w_star_ws[1,,,]
#  W_star_salish <- pars$w_star_salish[1,,,]
# 
# save(W_star_salish,file="W_star_salish_start_vals.RData")
# save(W_star_ws,file="W_star_ws_start_vals.RData")
# save(W_star_sf,file="W_star_sf_start_vals.RData")

# origin_loc   <- apply(pars$origin_loc,c(2,3,4),mean)
# dim(origin_loc)
#rowSums(origin_loc[,9,])
# 
# print(traceplot(stanMod,pars=c("lp__",base_params),inc_warmup=F))
# 
Output <- list(stanMod=stanMod,stanMod_summary=stanMod_summary,
               converge=samp_params, pars=pars,
               raw.dat.all=dat.all,raw.dat.pos=dat.pos.fin,N_CHAIN=N_CHAIN,
               PIT.dat.fin=PIT.dat.fin,
               AWG_dat = AWG_dat,
               
               PRIORS = PRIORS,cum_M2_fixed = cum_M2_fixed, origin_vec=origin_vec,
               diri_constant=diri_constant,
               ashop_year_break=ashop_year_break,
               stan_pars = stan_pars,
               AGE = AGE,
               COV = COV, age_month_cal=age_month_cal,
               #ocean_temp_dev = ocean.temp.dev, ocean_temp = ocean.temp, ocean_temp_avg =time.avg.deep.trim,
               E_prop_1 = E_prop_1,
               E_prop_2 = E_prop_2,
               river_entry=river_entry,
               shaker_mort = shaker_mort,
               knot.loc.sum.fall = knot.loc.sum.fall, knot.loc.wint.spr = knot.loc.wint.spr,
               #logit_offset_int = logit_offset_int,
               #vuln_int_idx = vuln_int_idx, vuln_int_rec_idx = vuln_int_rec_idx,
               q_year_vec=q_year_vec,
               phi_space_fix = phi_space_fix,
               vuln_age=vuln_age,vuln_age_hake=vuln_age_hake, vuln_age_pollock=vuln_age_pollock,
               vuln_fixed=vuln_fixed,
               MONTH.vuln =MONTH.vuln, MONTH.vuln.hake = MONTH.vuln.hake, MONTH.vuln.pollock =MONTH.vuln.pollock,
               vuln_troll_mat = vuln_troll_mat, vuln_treaty_mat =vuln_treaty_mat,  vuln_rec_mat =vuln_rec_mat,
               spawn_time_fraction=REL$spawn_time_fraction,
               spawn_time_array = spawn_time_array,
               temperature_season_idx = temperature_season_idx,
               N_years_recover = N_years_recover,  N_years_release = N_years_release, N_month = N_month, N_time_mod=N_time_mod,
               spawn_loc=spawn_loc_2,TREATY= TREATY,SEASON=SEASON, NAME = NAME, REL=REL,
               loc_18=loc_18,MONTH.STRUCTURE=MONTH.STRUCTURE, CLOGLOG=CLOGLOG, SPAWN=SPAWN,
               ORIGIN.GROUPS = ORIGIN.GROUPS,
               CALENDAR=CALENDAR,
               TRAWL.US=TRAWL.US, TRAWL.BC=TRAWL.BC, TRAWL.AK=TRAWL.AK,
               first.season = first.season, last.season=last.season,
               YEARS.RELEASE=YEARS.RELEASE,YEARS.RECOVER=YEARS.RECOVER,YEARS.BROOD=YEARS.BROOD,
               FW_coverage = FW_coverage)

setwd("/Users/ole.shelton/GitHub/spring-chinook-distribution/Output files/_Fit objects")
save(Output,file=paste(GROUP," ",NAME,".RData",sep=""))


# code.dir    <- "/Users/ole.shelton/GitHub/Salmon-Climate/Mixed Model post processing"
# setwd(code.dir)
# source("Diagnostics and descriptive plots SS CLIMATE.R")

