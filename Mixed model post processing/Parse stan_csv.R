library(rstan)
library(dplyr)

# Read in stan csv output
setwd("/Users/ole.shelton/GitHub/Salmon-Climate/Output files")
stanMod1 <- read_stan_csv("samp_file_mixed_test_A 100_1.csv")
stanMod2 <- read_stan_csv("samp_file_mixed_test_A 100_2.csv")
stanMod4 <- read_stan_csv("samp_file_mixed_test_A 100_4.csv")
stanMod3 <- read_stan_csv("samp_file_mixed_test_A 100_3.csv")

# Get the remainder of the information for the stanfit (priors, raw data, etc.)
results.dir <- "/Users/ole.shelton/GitHub/Salmon-Climate/Output files/_Fit objects"
setwd(results.dir)
load("CA+COL CLIMATE Troll_Rec_Treaty_SS+PROC_E100_M2FIX_effort-ts-q_vulnfix_10-24-2017-NO_FIT.RData")

### Identify the parameters that were supposed to be kept by the stanfit.
stan_pars <- Output$stan_pars

## Extract only the stan pars that are important 
e1 <- rstan::extract(stanMod1,pars=stan_pars, permuted = FALSE)
e2 <- rstan::extract(stanMod2,pars=stan_pars, permuted = FALSE)
e3 <- rstan::extract(stanMod3,pars=stan_pars, permuted = FALSE)
e4 <- rstan::extract(stanMod4,pars=stan_pars, permuted = FALSE)

out.array <- array(dim=c(dim(e1)[1],4,dim(e1)[3]))
out.array[,1,] <- e1
out.array[,2,] <- e2
out.array[,3,] <- e3
out.array[,4,] <- e4

## Convergence diagnostics
mon <- rstan::monitor(out.array, print = TRUE, warmup = 0)
rownames(mon) <- dimnames(e1)[3]$parameters
mon <- as.data.frame(mon)

## EXTRACT PERMUTED VERSION because lists are easier to deal with than arrays.
f1 <- rstan::extract(stanMod1,pars=stan_pars, permuted = TRUE)
f2 <- rstan::extract(stanMod2,pars=stan_pars, permuted = TRUE)
f3 <- rstan::extract(stanMod3,pars=stan_pars, permuted = TRUE)
f4 <- rstan::extract(stanMod4,pars=stan_pars, permuted = TRUE)

pars <- list()

for(i in 1:length(stan_pars)){
  
  A <- f1[[stan_pars[i]]]
  B <- f2[[stan_pars[i]]]
  C <- f3[[stan_pars[i]]]
  D <- f4[[stan_pars[i]]]
  
  temp <- abind(A,B,along=1)
  temp <- abind(temp,C,along=1)
  temp <- abind(temp,D,along=1)
  
  pars[[stan_pars[i]]] <- temp
}

# get_adaptation_info(stanMod)
  samp_params <-  list(get_sampler_params(stanMod1),
                        get_sampler_params(stanMod2),
                        get_sampler_params(stanMod3),
                        get_sampler_params(stanMod4))
  
  Output$pars <- pars
  Output$converge = samp_params
  Output$stanMod_summary=mon
  
  NAME <- "CA+COL CLIMATE Troll_Rec_Treaty_SS+PROC_E100_M2FIX_effort-ts-q_vulnfix_10-24-2017-FIT_AND_DATA"
  setwd(results.dir)
  save(Output,file=paste(NAME,".RData",sep=""))
  
  
# #A
# base_params <- c("tau_process",
#                  "log_q_rec_start","log_q_troll_start","log_q_rec_can_start","log_q_treaty_start",
#                  "log_q_rec_slope","log_q_troll_slope","log_q_rec_can_slope","log_q_treaty_slope",
#                  #"log_q_troll_pos", "log_q_rec_pos","log_q_treaty_pos","log_q_rec_can_pos",#"log_q_rec_PUSO_pos",
#                  "logit_offset",
#                  #"sigma_pos",
#                  "sigma_pos_int",
#                  "sigma_pos_slope",
#                  "alpha_pay_mean",
#                  "alpha_pay_sd",
#                  "beta_pay_mean",
#                  "beta_pay_sd",
#                  "log_rel_year_mu", "log_rel_year_sigma",
#                  "beta_vuln",#"vuln_int",
#                  #"log_M2",
#                  "log_F_rec_mean","F_rec_sigma"
#                  # "kappa_troll", "kappa_treaty",
#                  # "kappa_rec", "kappa_rec_can"
# )#"prob_age_year") #"beta_age_month""vuln_int"
# 
# stanMod_summary <- summary(stanMod4,pars=stan_pars)$summary
# 
# #print(traceplot(stanMod,pars=c("lp__"),inc_warmup=F))
# # summary(stanMod,pars=base_params)$summary
#  
# stanMod_summary <- summary(stanMod)$summary
# summary(stanMod,pars="tau_process")
# summary(stanMod,pars=c("alpha_pay","beta_pay"))
# 
# origin_summary <-summary(stanMod,pars=c("origin_sea_int","origin_sea_slope"))
# origin_summary <- summary(stanMod,pars=c("origin_sea_int","origin_sea_slope"))$summary
# origin_mat_summary <- as.data.frame(summary(stanMod,pars=c("origin_mat"))$summary)
# 
# #######################################################################################
# #######################################################################################
# #######################################################################################
# #######################################################################################
# 
# # origin_loc   <- apply(pars$origin_loc,c(2,3,4),mean)
# # dim(origin_loc)
# #rowSums(origin_loc[,9,])
# 
# print(traceplot(stanMod,pars=c("lp__",base_params),inc_warmup=FALSE))
# 
# Output$pars <- pars
# 
# <- list(stanMod=stanMod,stanMod_summary=mon,
#                converge=, pars=pars)
# 
# 
# NAME <- "CLIMATE Troll_Rec_Treaty_SS+PROC_E100_M2FIX_effort-ts-q_vulnfix_10-21-2017-FIT_ONLY"
# 
