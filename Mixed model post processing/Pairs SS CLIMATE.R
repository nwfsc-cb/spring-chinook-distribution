### Make Pairs plots to diagnose sampling problems.

# define a bunch of the parameters to examine.
PARS.1 <- c( "lp__","log_q_troll_start", "log_q_treaty_start", "log_q_rec_start", "log_q_rec_can_start","q_int")
PARS.2 <- c( "theta_space")#,"phi_space") #"tau_process_prod",) #"tau_q_troll","tau_q_rec",
PARS.3 <- c( "log_F_rec_mean","log_F_troll_mean", "F_rec_sigma","F_troll_sigma","log_rel_year_mu","log_rel_year_sigma")
PARS.4 <- c( "beta_vuln","beta_vuln_hake","sigma_cv")#,"log_M2" , "beta_vuln_int"
#PARS.4 <- c("log_M2")
PARS.5 <- c( "log_q_troll_slope","log_q_rec_slope")#  "log_q_treaty_slope")#"beta_pay_mean",
            #"alpha_pay_sd")#"beta_pay_sd")
PARS.6 <- c("alpha_pay","beta_pay")

### Process origin_loc to make pairs possible.
  # Make indicator variable to pull out divergent transitions.
# ind.vec.all <- NULL
# for(i in 1:length(lengths(converge))){
#   these <- which(converge[[i]][(Warm+1):nrow(converge[[i]]),"divergent__"]==1)
#   ind.vec <- rep(0,Iter)
#   if(length(these)!=0){
#       ind.vec[these] <- 1  
#   }
#   ind.vec.all <- c(ind.vec.all,ind.vec)
# }

# Pull out origin_loc areas for examination one at a time
# Winter

# all.origin.loc <- NULL
#   for(i in 1:nrow(spawn_loc)){
#             temp <- samp$origin_loc[,1,i,]    
#             colnames(temp) <- paste(LOCATIONS$location.name,sep=".")
#             temp1 <- data.frame("season"="wint.spring","origin"=spawn_loc$ocean.region[i],"divergent"=ind.vec.all,temp)
#           
#             temp <- samp$origin_loc[,2,i,]    
#             colnames(temp) <- paste(LOCATIONS$location.name,sep=".")
#             temp2 <- data.frame("season"="summer","origin"=spawn_loc$ocean.region[i],"divergent"=ind.vec.all,temp)
#             
#             temp <- samp$origin_loc[,3,i,]    
#             colnames(temp) <- paste(LOCATIONS$location.name,sep=".")
#             temp3 <- data.frame("season"="fall","origin"=spawn_loc$ocean.region[i],"divergent"=ind.vec.all,temp)
#             
#         all.origin.loc <- rbind(all.origin.loc,temp1)
#         all.origin.loc <- rbind(all.origin.loc,temp2)
#         all.origin.loc <- rbind(all.origin.loc,temp3)
#   }






# "prob_age_year",
# "vuln_mat",
# "rel_year_all",
# "log_rel_year_mu", 
# "rel_year_sigma",
# "origin_loc",
# "prop_D",
# "D",
# "log_N_all",
# "log_N_ratio",
# "F_rec",
# "F_troll",
# "log_F_rec_mean",
# "F_rec_sigma",
# "F_troll_array",
# "F_rec_array",
# "cum_M2_temp"
# 

### 
pdf(paste(base.dir,"/Salmon-Climate/Output plots/","Pairs ",NAME,".pdf",sep=""),height = 8.5, width=8.5,onefile=T)

  pairs(Output$stanMod, pars = c(PARS.1), log = FALSE, las = 1)
  pairs(Output$stanMod, pars = c(PARS.2), log = FALSE, las = 1)
  pairs(Output$stanMod, pars = c(PARS.3), log = TRUE, las = 1)
  pairs(Output$stanMod, pars = c(PARS.4), log = TRUE, las = 1)
  pairs(Output$stanMod, pars = c(PARS.5), log = FALSE, las = 1)
  pairs(Output$stanMod, pars = c(PARS.6), log = TRUE, las = 1)
  #pairs(Output$stanMod, pars = c("origin_loc"))
  
dev.off()