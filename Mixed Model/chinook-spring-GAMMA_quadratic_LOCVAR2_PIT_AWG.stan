functions {/////////////////////////////////////////////////////////////////
    real Baranov(real M2, real F_focal, real F_tot, real log_N, real log_origin) {
                return  (log(F_focal) - log(M2 + F_tot) + log_N + log_origin + log(1 - exp(-(M2 + F_tot) )))  ;
    }

    /// These are the functions converting means and variances of the true catch distribution into the NB observed catch distributions
    // real alpha_calc(real lambda, real sigma_int,real sigma_slope_1){ 
    //         return(exp(lambda)^2 * pow(exp(sigma_int + sigma_slope_1 * lambda) ,-1)) ;
    // }
    // real beta_calc(real lambda, real sigma_int,real sigma_slope_1, real inv_frac_samp){ 
    //         return(exp(lambda) * pow(exp(sigma_int + sigma_slope_1 * lambda) ,-1) * inv_frac_samp) ;
    // }


    // Alpha and beta are the parameters for the true catch distribution
    real alpha_calc( real sigma_cv){
          real alp ;
          alp = pow(sigma_cv,-2) ;
          return(alp) ;
    }
    real beta_calc(real log_lambda, real alpha){ 
          real bet ;
          bet = exp(log(alpha) - log_lambda ) ;
          return(bet) ;
    }
   // The probability of observing 0 is a function of the true catch and the sampling fraction. (Neg Binom)  
    real Prob_0_Calc(real alpha, real beta, real inv_frac_samp) {
         real B ;
         B = beta * inv_frac_samp ;
         return( exp(alpha * (log(B)-log(B+1))))  ;
    }
    
   // The observations >0 are true catch and the sampling fraction but are not the same a simply alpha and beta. 
    real E_trunc_pos(real alpha,real beta, real inv_frac_samp ){
          real temp ;
          real Mean ;
          real B ;
          B = beta * inv_frac_samp ;
          temp = exp(-alpha * log(1+inv(B))) ;
          Mean = exp(log(alpha)-log(B)-log(1-temp)) ; 
          return(Mean) ;
    }
    real V_trunc_pos(real alpha, real beta,real E_trunc_pos1, real Prob_0_Calc1, real inv_frac_samp){
          real Var ;
          real B ;
          B = beta * inv_frac_samp ;
          Var = exp(log(alpha) - log(B) + log(1 + alpha / B) - log(1-Prob_0_Calc1)) -
                      (Prob_0_Calc1) * pow(E_trunc_pos1,2) ;
          if(Var <  1e-04 ){Var= 1e-04 ;} 
          return(Var) ; 
    }
    
    /// these are for the alpha and beta in the likelihood from the approximate Gamma likelihood.
    real log_alpha_calc_pos(real E_trunc_pos2, real V_trunc_pos1){ 
            return( 2*log(E_trunc_pos2) - log(V_trunc_pos1) ) ;
    }
        
    real log_beta_calc_pos(real E_trunc_pos3, real V_trunc_pos1,real log_inv_samp_frac){         
            return( log(E_trunc_pos3) - log_inv_samp_frac - log(V_trunc_pos1) ) ;
    }
}
data { ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  int N_rel ;   // Number of releases  
  int N_time_mod ;   // Number of model intervals
  int N_log_N_all ; // Number of latent states we need to track.
  int N_obs_bin ;   // Number of observations binomial  
  int N_obs_pos ;   // Number of observations positive
  int N_loc ;       // Number of spatial locations
  //int N_loc_spawn ; // Number of spatial spawning locations 
  int N_gear ;      // Number of harvest gear types
  int N_effort ;    // Number of effort categories
  int N_vuln   ;    // Number of vulnerability categories
  int N_vuln_int   ;    // Number of vulnerability categories for intercept
  int N_vuln_month ;    // Number of vulnerability categories
  int N_yr_reg ;    // Number of year-region categories used for first year recruitment
  int N_year ;      // Number of year-region categories used for first year recruitment
  int N_origin ;    // Number of origin categories for ocean distribution
  int N_season ;    // Number of seasons
  int N_month_mod ; // Number of calender months modelled
  int N_years_recover; // Number of recovery years
  int N_years_release; // Number of release years
  int N_month;     // Number of month groups in a year
  int N_season_total ; // Total number of seasons with effort data and temperature data
  int N_time_mod_rel[N_rel] ; // Length of time to model for each release
  int N_no_offshore ; // number of locations for which there is no offshore box.
  int N_juv ; // Number of juvenile stages (2 fingerling and yearlings)
  // In-river data.
  int N_PIT ; // Total number of releases with PIT survival data
  int N_AWG ; // Total number of releases wtih AWG data
  
  vector[N_year] age_year;

  //vector[N_loc] origin_vec ;

  ////////////// TEMPERATURE DEVIATION DATA and INDEXes needed for making matrices of coeffients
  //matrix[N_season_total, N_loc] ocean_temp_dev ;
  int temperature_season_idx[N_season_total] ;

  real phi_space_fix ;
  
  // fixed mortality
  vector[N_month_mod] cum_M2_fixed ;
  real shaker_mort ;
 
  // fixed vulnerability intercept
  real vuln_fixed ;
  
  matrix<lower=0>[N_season_total, N_loc] vuln_troll_mat ;      // Minimum Size matrix for Vulnerability (troll) [model month, location]
  matrix<lower=0>[N_season_total, N_loc] vuln_treaty_mat ;     // Minimum Size matrix for Vulnerability (treaty)   [model month, location]
  matrix<lower=0>[N_season_total, N_loc] vuln_rec_mat ;        // Minimum Size matrix for Vulnerability (treaty)   [model month, location]

  real constant;  // constant value for modifying fising mortality to avoid log(zero)
  real constant_origin;  // constant value for modifying origin_loc in special cases
  vector[N_month_mod] month_mod ; // vector of 1:N_month_mod for calculating monthly mortality.
  vector[N_month_mod] age_month_cal ; // vector of 1:N_month_mod for defining monthly mortality rate.

  // Observations of catch
  int bin_catch[N_obs_bin];
  //real logit_offset_int[N_obs_bin];
  //vector[N_obs_bin] frac_samp;
  vector[N_obs_bin] inv_frac_samp;
  vector[N_obs_bin] log_frac_samp;
  //vector[N_obs_bin] log_frac_samp_comp;
  //vector[N_obs_bin] logit_offset_int;
  
  real pos_catch[N_obs_pos];
  //vector[N_obs_pos] inv_frac_samp_pos;
  vector[N_obs_pos] log_inv_frac_samp_pos;
  
  // Observations of PIT data in-river
  // N (total number of observations)
  // k (total number of successes (survivors))

  int PIT_N[N_PIT]; // Implied sample size for binomial for in river PIT data
  int PIT_K[N_PIT]; // Implied recoveries for binomial for in-river PIT data
  
  // Observations of CWT in-river recoveries
  // N (total number of observations)
  // k (total number of successes (survivors))

  int AWG_dat[N_AWG,N_year]; 
  // Overdispersion for Neg Biomial (AWG in-river data)
    real<lower=0>  nb_phi_fix ;


  // End of model constraints ; 
  // vector[N_rel] log_N_fin_ratio_data  ;
  // vector[N_rel] log_N_fin_ratio_sd ; 

  // Assumed constraint on the final abundance (<0.001 of initial)
  // row_vector[N_rel] log_N_ratio_mean;
  // row_vector<lower=0>[N_rel] log_N_ratio_sd;

  // Fishing associated things
  int<lower=1> N_f_rec_idx_param;     // Number of rec params for fishing mortality
  int<lower=1> f_rec_param_idx[N_f_rec_idx_param,2];     // Index for mapping fishing params to the right location

  // int<lower=1> N_f_treaty_idx_param;     // Number of rec params for fishing mortality
  // int<lower=1> f_treaty_param_idx[N_f_treaty_idx_param,2];     // Index for mapping fishing params to the right location

  int<lower=1> N_f_troll_idx_param;     // Number of rec params for fishing mortality
  int<lower=1> f_troll_param_idx[N_f_troll_idx_param,2];     // Index for mapping fishing params to the right location

  // int<lower=1> N_f_hake_ashop_idx_param;     // Number of rec params for fishing mortality
  // int<lower=1> f_hake_ashop_param_idx[N_f_hake_ashop_idx_param,2];     // Index for mapping fishing params to the right location

  //int<lower=1> N_f_shoreside_idx_param;     // Number of rec params for fishing mortality
  //int<lower=1> f_hake_shoreside_param_idx[N_f_hake_shoreside_idx_param,2];     // Index for mapping fishing params to the right location

  int<lower=1>  N_f_rec_overlap_effort_idx_param; // locations where there are two rec effort data points from Canada
  int<lower=1>  f_rec_overlap_effort_idx[N_f_rec_overlap_effort_idx_param,2]; /// Index for mapping overlap in canadian effort.

  // Effort data.
    matrix<lower=0>[N_season_total, N_loc] K_troll ;
    matrix<lower=0>[N_season_total, N_loc] K_treaty ;
    matrix<lower=0>[N_season_total, N_loc] K_rec ;
    matrix<lower=0>[N_season_total, N_loc] K_rec_can ;
    matrix<lower=0>[N_season_total, N_loc] K_rec_can_irec ;
    matrix<lower=0>[N_season_total, N_loc] K_rec_PUSO ;
    matrix<lower=0>[N_season_total, N_loc] K_hake_ashop ;
    matrix<lower=0>[N_season_total, N_loc] K_hake_shoreside ;
    matrix<lower=0>[N_season_total, N_loc] K_pollock_GOA ;
    matrix<lower=0>[N_season_total, N_loc] K_rockfish_AK ;

    int ashop_year_break ;
  // Helper files for making fishing stochastic.
    // int<lower=0> N_f_troll_effort_idx_param ; 
    // int<lower=0> f_troll_effort_idx[N_f_troll_effort_idx_param,2] ;
    // 
    // int<lower=0> N_f_treaty_effort_idx_param ;
    // int<lower=0> f_treaty_effort_idx[N_f_treaty_effort_idx_param,2]  ;
    // 
    // int<lower=0> N_f_rec_effort_idx_param ;
    // int<lower=0> f_rec_effort_idx[N_f_rec_effort_idx_param,2]  ;
    // 
    // int<lower=0> N_f_rec_can_effort_idx_param ;
    // int<lower=0> f_rec_can_effort_idx[N_f_rec_can_effort_idx_param,2]  ;

  // Files used for mapping instances of postive catch and assigning parameters to each
    real q_year_vec[N_season_total] ;
    real log_q_year_vec[N_season_total] ;
    
   // indices and matrices to make ocean distribution smooth (predictive process model)
   int<lower=0> N_knot_sf      ;
   int<lower=0> N_knot_ws      ;
   int<lower=0> N_pred_loc  ;
   int<lower=0> N_pred_loc_salish ;
   matrix[N_knot_sf,N_knot_sf] d_knot_knot_sf2  ;
   matrix[N_knot_sf,N_pred_loc] d_pred_knot_sf2  ;
   matrix[N_knot_ws,N_knot_ws] d_knot_knot_ws2  ;
   matrix[N_knot_ws,N_pred_loc] d_pred_knot_ws2  ;
   
  // river entry indicator matrix
    vector[N_loc] river_entry[N_rel] ;

  // Indexes for fishing catchability
    int N_troll_idx ;
    int troll_idx[N_loc] ;
    int N_rec_us_idx ;
    int rec_us_idx[N_loc] ;
    // int N_sigma_cv_idx ;
    // int sigma_cv_idx[N_loc] ;

  // Indexes state space
      int mod_time_idx[N_obs_bin] ; // number of months between release and recruitment 
      int mod_time_N_all_idx[N_obs_bin] ; // number of months between release and recruitment 
      int rel_idx[N_obs_bin]  ;      // release index for all binomial observations,

      //int mod_time_pos_idx[N_obs_pos] ; // number of months between release and recruitment 
      //int rel_pos_idx[N_obs_pos] ;        // release index for all binomial observations,
      //int<lower=0> loc_spawn_idx[N_rel] ;
      int<lower=0> age_year_idx[N_time_mod] ;
      int<lower=0> year_region_idx[N_rel] ;
      int<lower=0> age_month_idx[N_time_mod] ;
      //int<lower=0> spawn_time_idx[N_time_mod] ;
      int<lower=0> spawn_time_array[N_rel,N_time_mod] ;
      int<lower=0> season_idx[N_time_mod] ;
      int<lower=0> origin_idx[N_rel] ;
      int<lower=0> start_year[N_rel] ;
      int<lower=0> origin_year_idx[N_rel,N_time_mod] ;
      int<lower=0> start_month_idx[N_rel];
      int<lower=0> juv_idx[N_rel];
      
  // values for spawn timing within a season
      real spawn_time_fraction[N_rel] ;  // For each release
      
  /// Index for spatial smoothing
      int<lower=0> knot_idex[N_pred_loc] ;
      int<lower=0> knot_idex_salish[N_pred_loc_salish] ;
      int<lower=0> knot_idex_offshore ;
          
  // Indexes, binomial
  int<lower=0> loc_idx[N_obs_bin] ;
  int<lower=0> origin_bin_idx[N_obs_bin] ;
  int<lower=0> season_bin_idx[N_obs_bin] ;
  int<lower=0> gear_bin_idx[N_obs_bin] ;
  //int<lower=0> loc_spawn_bin_idx[N_obs_bin] ;
  int<lower=0> temp_dat_season_bin_idx[N_obs_bin] ;
  int<lower=0> juv_bin_idx[N_obs_bin] ; 

  // Index for PIT data and AWG data
  int<lower=0> PIT_idx[N_PIT]; // This is the release number of the pit data.
  int<lower=0> AWG_idx[N_AWG]; // This is the release number of the pit data.

  // Indexes, positive
  // int<lower=0> loc_pos_idx[N_obs_pos] ;
  // int<lower=0> origin_pos_idx[N_obs_pos] ;
  // int<lower=0> season_pos_idx[N_obs_pos];
  // int<lower=0> gear_pos_idx[N_obs_pos];
  // int<lower=0> loc_spawn_pos_idx[N_obs_pos] ;
  // int<lower=0> temp_dat_season_pos_idx[N_obs_pos] ;
  
  // Continuous covariates, shared
    row_vector[N_vuln_month] vuln_age;
    row_vector[N_vuln_month] vuln_age_hake;
    row_vector[N_vuln_month] vuln_age_pollock;
    
    // Continuous covariates, State space version
      real month_rec[N_rel] ; // number of months between release and recruitment 
      real log_N0[N_rel] ;    // number of initial relases
      real spawn_time[N_rel,N_time_mod] ;

  // Data and assumptions for spawners
    vector[N_year] E_prop_1[N_origin];
    vector[N_year] E_prop_2[N_origin];
    //vector[N_year] E_prop[N_origin];
    real diri_constant;           // Assumed precision for the dirichlet distribution

  //Priors 
  vector[N_year] E_alpha[N_origin];
  real log_rel_year_mu_prior[2] ;
  real log_rel_year_sigma_prior[2] ;
  vector[2] MU_M2 ;
  matrix[2,2] Sigma_M2;
  //real vuln_int_prior[2];
  real beta_vuln_prior[2];
  // real beta_vuln_hake_prior[2];
  // real beta_vuln_pollock_prior[2];
  //real beta_vuln_int_prior[2];
  // real sigma_prior[2];
  real sigma_cv_prior[2];
  //real sigma_slope_prior[2];
  real log_F_prior[2];
  real F_rec_sigma_prior[2];
  vector[N_knot_sf] w_star_prior_mean_sf[N_origin];
  vector[N_knot_sf] w_star_prior_sd_sf[N_origin];
  vector[N_knot_ws] w_star_prior_mean_ws[N_origin];
  vector[N_knot_ws] w_star_prior_sd_ws[N_origin];
  // vector[N_loc] origin_sea_int_prior_mean[N_origin];
  // vector[N_loc] origin_sea_int_prior_sd[N_origin];
  vector[N_loc] origin_sea_slope_prior_mean[N_origin];
  vector[N_loc] origin_sea_slope_prior_sd[N_origin];
  real gamma_int_prior[2] ; 
  real gamma_slope_prior[2] ; 
  //real logit_offset_slope_prior[2] ;
  //real tau_process_prior[2] ;
  //real tau_process_prod_prior[2] ;
  real log_q_troll_prior[2] ;
  real log_q_treaty_prior[2] ;
  real log_q_rec_prior[2] ;
  real log_q_hake_prior[2] ;
  real log_q_pollock_prior[2] ;
  real log_q_rockfish_AK_prior[2] ;
  real log_q_slope_prior[2] ;
  // real tau_q_dev_prior[2] ;
  real phi_space_prior[2] ;
  real theta_space_prior[2] ;
  real q_int_prior[2] ;
  real spawn_smooth_prior[2];
  // real nb_phi_prior[2];
}

transformed data { ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //matrix[2,2] L_M2_Sigma ;
  
  vector[N_year] vec_one ;
  row_vector[N_loc] vec_one_loc ;
  matrix[N_season_total,N_loc] ZERO_mat ; /// Matrix of zeros
  vector[N_knot_sf] zero_vec_pred_loc_sf ;
  vector[N_knot_ws] zero_vec_pred_loc_ws ;
  vector[N_pred_loc_salish] zero_vec_pred_loc_salish ;
  // real<lower=0> tau_q_troll ;
  // real<lower=0> tau_q_rec ;
  
  // vector[N_obs_bin] log_frac_samp ;
  // vector[N_obs_bin] log_frac_samp_comp ; /// log of complement of frac_samp (i.e. 1-frac_samp)
  
  // Vulnerability Array helper files
      matrix[N_time_mod,N_loc] vuln_troll_array[N_years_release];      // Minimum Size array for Vulnerability (troll) [release year, model month, location]
      matrix[N_time_mod,N_loc] vuln_treaty_array[N_years_release];     // Minimum Size array for Vulnerability (treaty)   [release year, model month, location]
      matrix[N_time_mod,N_loc] vuln_rec_array[N_years_release];        // Minimum Size array for Vulnerability (rec)   [release group, model month, location]

  /// spatial smoothing helpers
  matrix[N_pred_loc,N_knot_sf] t_d_pred_knot_sf2  ; // Transpose of squared distance matrix of knot to prediction locations.
  matrix[N_pred_loc,N_knot_ws] t_d_pred_knot_ws2  ; // Transpose of squared distance matrix of knot to prediction locations.
  
  /// Helper file for log_M2 matt trick.
  
  //L_M2_Sigma = cholesky_decompose(Sigma_M2);
  
  
  // log_frac_samp = log(frac_samp);
  // log_frac_samp_comp = log(1-frac_samp);
  
  // tau_q_troll = 0.000001 ;
  // tau_q_rec = 0.000001 ;

  t_d_pred_knot_sf2 = d_pred_knot_sf2' ;
  t_d_pred_knot_ws2 = d_pred_knot_ws2' ;
  zero_vec_pred_loc_sf = rep_vector(0, N_knot_sf);
  zero_vec_pred_loc_ws = rep_vector(0, N_knot_ws);
  zero_vec_pred_loc_salish = rep_vector(0, N_pred_loc_salish);

  // for(i in 1:2){
  //   logit_offset_int[i] = -1.4;
  // }

  for(i in 1:N_season_total){
    ZERO_mat[i] = rep_row_vector(0,N_loc) ;
  }
  for(i in 1:N_year){
    vec_one[i] = 1 ;
  }
  for(i in 1:N_loc){
    vec_one_loc[i] = 1 ;
  }
  
     for(i in 1:N_years_release){ // Make an array of the same dimensions as the state vector
      vuln_troll_array[i]    = block(vuln_troll_mat,(1+(i-1)*N_month),1,N_time_mod,N_loc) ;
      vuln_treaty_array[i]   = block(vuln_treaty_mat,(1+(i-1)*N_month),1,N_time_mod,N_loc) ;
      vuln_rec_array[i]      = block(vuln_rec_mat,(1+(i-1)*N_month),1,N_time_mod,N_loc) ;
     }
}
parameters { ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Catchability params
    vector[N_troll_idx] log_q_troll_start ;
    real<lower=0> log_q_troll_slope ;
      
    real log_q_treaty_start ;      
    real<lower=0> log_q_treaty_slope ;     
      
    vector[N_rec_us_idx] log_q_rec_start ;
    real<lower=0> log_q_rec_slope ;
      
    real log_q_rec_can_start ;
    real<lower=0> log_q_rec_can_slope ;

    real log_q_rec_can_irec_start ;

    real log_q_hake_ashop_start ;
    real log_q_hake_shoreside_start ;
    
    real log_q_pollock_GOA_start ;
    real log_q_rockfish_AK_start ;

    // Observation CV parameters
    real<lower=0> sigma_cv[2] ;
    real<lower=0> sigma_cv_hake ;
    real<lower=0> sigma_cv_pollock ;
    
    real log_rel_year_mu ;
    real<lower=0> log_rel_year_sigma ;

    // real<lower=0> nb_phi ;

   // Vulnerability parameters (logit)
        //real beta_vuln_int[2] ; // These are the intercept and slope parameters for how vulnerability is shaped with respect to model age
        vector[2] beta_vuln ;
        real beta_vuln_hake[2] ;
        real beta_vuln_pollock[2] ;
        //real beta_vuln_rockfish[2] ;
        
  /// Ocean distribution smoothing parameters
      real<lower = 0> theta_space[2]  ; /// spatial correlation parameter (1 is summer-fall, 2 is winter-spring)
      //real<lower = 0> phi_space    ; /// spatial sd parameter (1 is summer-fall, 2 is winter-spring)

    // Spawning Parameters
    //vector[N_juv] alpha_pay[N_origin] ;
    //vector[N_juv] log_beta_pay[N_origin] ;
    
    vector[N_year] gamma_pay[N_origin,N_juv] ; // alternate parameterization for maturity.  No constraint on higher fraction of fish having to leave ocean with age.
    real<lower=0> spawn_smooth ;

    // Catchability offsets

      real q_int ;
      //real<lower=0,upper=1> observe_frac;
      
  // hierarchical fishing mortalities for locations that do not have effort
        real  log_F_rec_mean ;
        real<lower=0> F_rec_sigma ;
        real  log_F_troll_mean ;
        real<lower=0> F_troll_sigma ;

  // M2 estimates
      //vector[2] log_M2_raw ;
      
  // Continuous Variables
      // Hierarchical spawn fraction parameters
      // real alpha_pay_mean ;
      // real<lower=0> alpha_pay_sd ;
      // real log_spawn_smooth_mean ;
      // real log_spawn_smooth_sigma ;

  // More ocean smooth paramters
      vector[N_knot_sf] w_star_sf[2, N_origin] ; /// Values at the knots(by origin and season) Summer-Fall
      vector[N_knot_ws] w_star_ws[2, N_origin] ; /// Values at the knots(by origin and season) Winter-spring
      vector[N_pred_loc_salish] w_star_salish[N_season, N_origin] ;
      real w_logit_offshore[N_origin] ;
    
    // Distribution in the ocean coefficients (Origin-location slopes in response to ocean temperature)
       // vector[N_loc] origin_sea_slope[N_season,N_origin] ;

    // fishing params without effort
      // vector<lower=0>[N_f_rec_idx_param]  F_rec ;
       vector[N_f_rec_idx_param] log_F_rec_raw ;                 // These are for fishing parameters without effort info.
       vector[N_f_troll_idx_param] log_F_troll_raw ;             // These are for fishing parameters without effort info.
       // vector[N_f_treaty_idx_param] log_F_treaty_raw ;        // These are for fishing parameters without effort info.
       // vector[N_f_hake_ashop_idx_param] log_F_hake_ashop_raw; // These are for fishing parameters without effort info.

    
    // Deviations from catchability (Matt trick)
        // row_vector[N_loc]  q_troll_dev_raw[N_season_total] ;
        // row_vector[N_loc]  q_rec_dev_raw[N_season_total] ;
        // row_vector[N_loc]  q_treaty_dev_raw[N_season_total] ;

        // real q_troll_dev_raw[N_season_total,N_loc] ;
        // real q_rec_dev_raw[N_season_total,N_loc] ;
        // real q_treaty_dev_raw[N_season_total] ;

    // Process error realizations
      //vector[N_rel] epsilon_raw[N_time_mod] ;
      vector[N_time_mod] epsilon_raw[N_rel] ;

    // early mortality realizations
        vector[N_rel] log_rel_year_raw ;

        // vector[N_f_troll_effort_idx_param ] q_troll_dev ;
        // vector[N_f_treaty_effort_idx_param ] q_treaty_dev ;
        // vector[N_f_rec_effort_idx_param ] q_rec_dev ;
        // vector[N_f_rec_can_effort_idx_param ] q_rec_can_dev ;
       // row_vector[N_loc] q_rec_dev[N_season_total] ;
       }
transformed parameters { ////////////////////////////////////////////////////////////////////////////////////////////////////

    // Vulnerability parameters
      // vector<lower=0>[2] beta_vuln ;
      // real<lower=0> beta_vuln_hake ;
    
    // Mortality Estimates.
      //vector[2] log_M2 ;
      // M2 associated estimates
    
    //vector<lower=0>[N_month_mod] M2_vec ;
    vector[N_month_mod] cum_M2 ;
    vector[N_time_mod] cum_M2_temp ;

    // Distribution in the ocean coefficients (Origin-location offsets)
       // real origin_ref[N_origin] ;
        vector[N_loc] origin_sea_int[N_season,N_origin] ;
        //vector[N_season] origin_sea_offshore[N_origin] ;
        vector[N_pred_loc] w_temp ;
        
        // local variable for inverse of knot matrix
        matrix<lower=0>[N_knot_sf,N_knot_sf] C_knot_sf ;
        matrix<lower=0>[N_pred_loc,N_knot_sf] c_pred_trans_sf ;
        matrix[N_knot_sf,N_knot_sf] C_knot_inverse_sf ;
        cholesky_factor_cov[N_knot_sf] L_knot_sf[N_origin];

        // local variable for inverse of knot matrix Winter Spring
        matrix<lower=0>[N_knot_ws,N_knot_ws] C_knot_ws ;
        matrix<lower=0>[N_pred_loc,N_knot_ws] c_pred_trans_ws ;
        matrix[N_knot_ws,N_knot_ws] C_knot_inverse_ws ;
        cholesky_factor_cov[N_knot_ws] L_knot_ws[N_origin];

    /////
      row_vector[N_loc] log_q_troll_start_rv ;
      row_vector[N_loc] log_q_troll_slope_rv ;
      row_vector[N_loc] log_q_rec_us_start_rv ;
     // row_vector[N_loc] log_q_rec_us_slope_rv ;
      matrix[N_season_total,N_loc] log_q_troll_pos ;
      matrix[N_season_total,N_loc] log_q_treaty_pos ;
      matrix[N_season_total,N_loc] log_q_rec_pos ;
      matrix[N_season_total,N_loc] log_q_rec_can_pos ;
      matrix[N_season_total,N_loc] log_q_rec_can_irec_pos ;
      matrix[N_season_total,N_loc] log_q_hake_ashop_pos ;
      matrix[N_season_total,N_loc] log_q_hake_shoreside_pos ;
      matrix[N_season_total,N_loc] log_q_pollock_GOA_pos ;
      matrix[N_season_total,N_loc] log_q_rockfish_AK_pos ;

    // DEFINE ORIGIN_LOC AS A MATRIX OF VECTORS
    vector[N_loc] origin_mat[N_origin,N_season] ;        // Array of coefficients for location information.
    vector[N_origin] origin_off ;        // Array of coefficients for location information.
 
  // States for all model ages and releases
     // row_vector[N_rel] log_N_ratio;
      matrix[N_rel,N_log_N_all] log_N_all ;
      matrix[N_rel,N_log_N_all] log_N_off ; // offshore component
        //matrix[N_time_mod,N_rel] log_N_all_t ;
       //real log_N_temp_1;

  // Probability of entering the river.
    //vector[N_juv] beta_pay[N_origin] ;
    vector[N_year] prob_age_year[N_origin,N_juv] ;

  // early mortality
     vector[N_rel] rel_year_all ;
  
  // process error ;
     //vector[N_rel] epsilon[N_time_mod] ;
     vector[N_time_mod] epsilon[N_rel] ;

  // Add fishing mortality
     vector[N_f_rec_idx_param] F_rec ;         // realizations of Fs that don't have observations.
     vector[N_f_troll_idx_param] F_troll ;     // realizations of Fs that don't have observations.
     //vector[N_f_treaty_idx_param] F_treaty;  // realizations of Fs that don't have observations.
     //vector[N_f_hake_ashop_idx_param] F_hake_ashop; // realizations of Fs that don't have observations.

    //real<lower=0> sigma_cv_temp ;
    //vector[N_obs_pos] mu_pos ;
    //vector[N_loc] mu_all[N_rel,N_time_mod] ;

  // Spawning States
      vector<lower=0>[N_year] D[N_rel] ;    //  Number of individuals in river in each age in the fall
      vector[N_year] prop_D[N_rel] ; // Proportion of individuals in river in each age in the fall
 
 // Spawning States
      vector[N_PIT] prop_PIT; // Proportion of individuals in river in each age in the fall
 
  // Vulnerability to fisheries
      //matrix[N_vuln,N_vuln_month] vuln_mat ;
  
  // Fishing helper files
      matrix[N_time_mod,N_loc] F_troll_fin[N_years_release];      // Fishing mortality array (troll) [release year, model month, location]
      matrix[N_time_mod,N_loc] F_treaty_fin[N_years_release];     // Fishing mortality array (rec)   [release year, model month, location]
      matrix[N_time_mod,N_loc] F_rec_fin[N_years_release];        // Fishing mortality array (rec)   [release year, model month, location]
      matrix[N_time_mod,N_loc] F_hake_ashop_fin[N_years_release];        // Fishing mortality array (rec)   [release year, model month, location]
      matrix[N_time_mod,N_loc] F_hake_shoreside_fin[N_years_release];        // Fishing mortality array (rec)   [release year, model month, location]
      matrix[N_time_mod,N_loc] F_pollock_GOA_fin[N_years_release];        // Fishing mortality array (rec)   [release year, model month, location]
      matrix[N_time_mod,N_loc] F_rockfish_AK_fin[N_years_release];        // Fishing mortality array (rec)   [release year, model month, location]
      matrix[N_time_mod,N_loc] F_tot_fin[N_years_release];        // Fishing mortality array (sum of all fishing types)   [release group, model month, location]

      matrix[N_season_total,N_loc] F_troll_array;        // Fishing mortality array (troll) [year,month group, location]
      //matrix<lower=0>[N_loc,N_month*N_years_recover] F_troll_array_temp;  // Fishing mortality array for sites without effort
      matrix[N_season_total,N_loc] F_treaty_array;          // Fishing mortality array (rec)   [year,month group, location]
      matrix[N_season_total,N_loc] F_rec_array;          // Fishing mortality array (rec)   [year,month group, location]
      matrix[N_season_total,N_loc] F_rec_can_array;          // Fishing mortality array (rec)   [year,month group, location]
      matrix[N_season_total,N_loc] F_hake_ashop_array;          // Fishing mortality array (rec)   [year,month group, location]
      matrix[N_season_total,N_loc] F_hake_shoreside_array;          // Fishing mortality array (rec)   [year,month group, location]
      matrix[N_season_total,N_loc] F_pollock_GOA_array;          // Fishing mortality array (rec)   [year,month group, location]
      matrix[N_season_total,N_loc] F_rockfish_AK_array;          // Fishing mortality array (rec)   [year,month group, location]

      
      //matrix<lower=0>[N_loc,N_month*N_years_recover] F_rec_array_temp;    // Fishing mortality array for sites without effort
      matrix[N_time_mod,N_loc] troll_mat[N_years_release];
      matrix[N_time_mod,N_loc] treaty_mat[N_years_release];
      matrix[N_time_mod,N_loc] rec_mat[N_years_release];
      matrix[N_time_mod,N_loc] hake_ashop_mat[N_years_release];
      matrix[N_time_mod,N_loc] hake_shoreside_mat[N_years_release];
      matrix[N_time_mod,N_loc] pollock_GOA_mat[N_years_release];
      matrix[N_time_mod,N_loc] rockfish_AK_mat[N_years_release];


   real<lower = 0> phi_space_origin_sf[N_origin]    ; /// spatial sd parameter
   real<lower = 0> phi_space_origin_ws[N_origin]    ; /// spatial sd parameter
  ////////////////////////////////////////////////////////

    // beta_vuln = exp(log_beta_vuln) ;
    // beta_vuln_hake = exp(log_beta_vuln_hake) ;
    
  // Spatial smoothing part.
      //phi_space_origin[1] = phi_space ;
      for(i in 1:N_origin){
        phi_space_origin_sf[i] = phi_space_fix ;
        phi_space_origin_ws[i] = phi_space_fix ;
      }
    
    for(i in 1:N_origin){ 
          //// Spatial smoothing calculations- Summer-Fall
        C_knot_sf         = exp(-d_knot_knot_sf2 * inv(pow(theta_space[1],2))) *pow(phi_space_origin_sf[i],2) ;
        C_knot_inverse_sf = inverse(C_knot_sf) ;
        c_pred_trans_sf   = exp(-t_d_pred_knot_sf2 * inv(pow(theta_space[1],2))) * pow(phi_space_origin_sf[i],2) ;
        L_knot_sf[i]      = cholesky_decompose(C_knot_sf) ;
        
        //// Spatial smoothing calculations - Winter Spring
        C_knot_ws         = exp(-d_knot_knot_ws2 * inv(pow(theta_space[2],2))) *pow(phi_space_origin_ws[i],2) ;
        //print("C_knot",C_knot);
        C_knot_inverse_ws = inverse(C_knot_ws) ;
        //print("C_knot_inv",C_knot_inverse);
        c_pred_trans_ws   = exp(-t_d_pred_knot_ws2 * inv(pow(theta_space[2],2))) * pow(phi_space_origin_ws[i],2) ;
        // //print("c_pred_trans",c_pred_trans) ;
        L_knot_ws[i]         = cholesky_decompose(C_knot_ws) ;
        //print("w_star",w_star[1,1])
    
        for(j in 1:N_season){
          // This is the smoothing projection
           if(j == 1){
              w_temp  =   c_pred_trans_ws * C_knot_inverse_ws  * w_star_ws[1,i] ; // This means spring is its own thing
           }
           if(j == 2){
              w_temp  =   c_pred_trans_sf * C_knot_inverse_sf  * w_star_sf[1,i] ; // summer and fall are treated the same.
           }
           if(j == 3){
              w_temp  =   c_pred_trans_sf * C_knot_inverse_sf  * w_star_sf[2,i] ;
           }
           if(j == 4){
              w_temp  =   c_pred_trans_ws * C_knot_inverse_ws  * w_star_ws[2,i] ; // This means winter is its own thing
           }
          for(k in 1:N_pred_loc){
              origin_sea_int[j,i,knot_idex[k]] = w_temp[k] ;
          }
          // if(i > 21){  // Kluge to make Monterey stop having stupid abundances in the winter ( only for Canada and north )
          //    if(j == 4){
          //     origin_sea_int[j,i,1] = origin_sea_int[1,i,1] ; // make winter == spring for Monterey
          //     origin_sea_int[j,i,2] = origin_sea_int[1,i,2] ; // make winter == spring for San Fran
          //     origin_sea_int[j,i,3] = origin_sea_int[1,i,3] ; // make winter == spring for MEN
          //     origin_sea_int[j,i,4] = origin_sea_int[1,i,4] ; // make winter == spring for NCA
          //    }
          // }
          if(i <= 3 ){  // Kluge to make Central Alaska stop having stupid abundances in all seasons (only for SOR and south)
              origin_sea_int[j,i,21] = origin_sea_int[j,i,17] ; // make WAPEN==NSEAK, all seasons
              origin_sea_int[j,i,20] = origin_sea_int[j,i,17] ; // make EAPEN==NSEAK, all seasons
              origin_sea_int[j,i,19] = origin_sea_int[j,i,17] ; // make NWGOA==NSEAK, all seasons
              origin_sea_int[j,i,18] = origin_sea_int[j,i,17] ; // make NEGOA== NSEAK, all seasons
          }else if(i ==5){    
              origin_sea_int[j,i,21] = origin_sea_int[j,i,17] ; // make WAPEN==NSEAK, all seasons
              origin_sea_int[j,i,20] = origin_sea_int[j,i,17] ; // make EAPEN==NSEAK, all seasons
              origin_sea_int[j,i,19] = origin_sea_int[j,i,17] ; // make NWGOA==NSEAK, all seasons
              origin_sea_int[j,i,18] = origin_sea_int[j,i,17] ; // make NEGOA== NSEAK, all seasons
          }
          
          
          for(k in 1:N_pred_loc_salish){ // this is the value for the salish sea (SGEO and PUSO and PUSO_out) and not 
              origin_sea_int[j,i,knot_idex_salish[k]] = w_star_salish[j,i,k] ;
          }
          // Ensure that the reference location has value ==0
          //    origin_sea_int[j,i] = origin_sea_int[j,i]  .* origin_vec ;
        }
    }
  
  // ORIGIN_MAT
  //print("VEC",origin_vec)
  //print("TWO",origin_mat[2])
  
  //print("Temp IDX",temperature_season_idx[1])
  
  //print(origin_sea_int[temperature_season_idx[1],1])

  //THIS IS THE CLIMATE DISTRIBUTION SECTION:
  
    // for(i in 1:N_origin){
    //    for(j in 1:N_season_total){
    //         origin_mat[i,j] =   exp(origin_sea_int[temperature_season_idx[j],i] +
    //                                 origin_sea_slope[temperature_season_idx[j],i] .* to_vector(ocean_temp_dev[j])) *
    //                                 pow(sum(exp(origin_sea_int[temperature_season_idx[j],i] +
    //                                 origin_sea_slope[temperature_season_idx[j],i] .* to_vector(ocean_temp_dev[j]))),-1)
    //                            ;
    //    }
    // }
    
    
    // THIS IS THE NEW SPR-SUM ONE
     // for(i in 1:N_origin){
     //   for(j in 1:N_season_total){
     //     
     //        if(i <= 7 ){ // Force all fish from NOR south to not be offshore
     //        origin_mat[i,j] =   exp(origin_sea_int[temperature_season_idx[j],i]) *
     //                                //origin_sea_slope[temperature_season_idx[j],i] .* to_vector(ocean_temp_dev[j])) *
     //                                pow(sum(exp(origin_sea_int[temperature_season_idx[j],i])),-1) ;
     //                                //origin_sea_slope[temperature_season_idx[j],i] .* to_vector(ocean_temp_dev[j]))),-1)
     //        }else{ // allow for all COL and north fish to be offshore
     //        origin_mat[i,j] =   exp(origin_sea_int[temperature_season_idx[j],i]) *
     //                                //origin_sea_slope[temperature_season_idx[j],i] .* to_vector(ocean_temp_dev[j])) *
     //                                pow(sum(exp(origin_sea_int[temperature_season_idx[j],i])) + exp(w_star_offshore[i,temperature_season_idx[j]]),-1) ;
     //                                //origin_sea_slope[temperature_season_idx[j],i] .* to_vector(ocean_temp_dev[j]))),-1)
     //        }                   
     //   }
     
     
     
     for(i in 1:N_origin){
        if(i<= 3){origin_off[i] = 1e-5  ;
        }else if(i==5){origin_off[i] = 1e-5 ; 
        }else{
            origin_off[i] = 1 /  (1 + exp(-(w_logit_offshore[i]))) ;
        }
     }
     
     
     for(i in 1:N_origin){
       for(j in 1:N_season){
         
            // if(i <= 7 ){ // Force all fish from NOR south to not be offshore
            // origin_mat[i,j] =   exp(origin_sea_int[j,i]) *
            //                         //origin_sea_slope[temperature_season_idx[j],i] .* to_vector(ocean_temp_dev[j])) *
            //                         pow(sum(exp(origin_sea_int[j,i])),-1) ;
            //                         //origin_sea_slope[temperature_season_idx[j],i] .* to_vector(ocean_temp_dev[j]))),-1)
            // }else{ // allow for all COL and north fish to be offshore
            
            if(j <=3){ // Make spring, summer and fall distributions 
            origin_mat[i,j] =   exp(origin_sea_int[j,i]) *
                                    pow(sum(exp(origin_sea_int[j,i])),-1) ;
  
            }else if(j == 4){ // Make winter half way between fall and spring.
            origin_mat[i,j] =   0.5 * (origin_mat[i,1] + origin_mat[i,3]) ;
            }

            origin_mat[i,j] = origin_mat[i,j] * (1-origin_off[i]) ; // Adjust for the offshore component
       }
    }
    
  // Prob_age_year logit transform
    // for(i in 1:N_origin){
    //   beta_pay[i] = exp(log_beta_pay[i]) ;
    // }
  
      for(i in 1:N_origin){
        for(j in 1:N_juv){
          // prob_age_year[i,j] =   vec_one ./ (1 + exp(-(alpha_pay[i,j] + age_year * beta_pay[i,j]))) ; //maturity for fingerlings and yearlings
          
          prob_age_year[i,j] =   vec_one ./ (1 + exp(-(gamma_pay[i,j]))) ; //maturity for fingerlings and yearlings, with logit form
        }
          prob_age_year[i,1,N_year] = 1 ; // maturity for fingerlings
          prob_age_year[i,2,N_year] = 1 ; //maturity for yearlings
          if(i == 2){
            prob_age_year[i,1,N_year-1] = 1 ; //maturity for SFB_wint fing
            prob_age_year[i,2,N_year-1] = 1 ; //maturity for SFB_wint yearlings
          }
        
      }

    // M2 vector and cum_M2 calculation
      //log_M2 = MU_M2 + L_M2_Sigma * log_M2_raw ;
       
      //M2_vec = exp(log_M2[1] + log_M2[2] * age_month_cal) ;
      //cum_M2 = cumulative_sum(M2_vec) ;
      cum_M2  = cum_M2_fixed ;

      for(i in 1:N_time_mod){
        if(i==1){cum_M2_temp[i]  = cum_M2[age_month_idx[i]] ;}
        if(i > 1){cum_M2_temp[i] = cum_M2[age_month_idx[i]] - cum_M2[age_month_idx[i-1]] ;}
      }

//////////////////////////////////////////////////////////////////////////////////////////////////
  // log_F_rec Matt trick
      F_rec    = exp(log_F_rec_mean   + log_F_rec_raw * F_rec_sigma) ;
      F_troll  = exp(log_F_troll_mean + log_F_troll_raw * F_troll_sigma) ;
      // F_treaty = exp(log_F_troll_mean + log_F_treaty_raw * F_troll_sigma) ;
      // F_hake_ashop = exp(log_F_rec_mean + log_F_hake_ashop_raw * F_rec_sigma) ;
      // HERE?
  
  // rel_year_all Matt trick
      rel_year_all = exp(log_rel_year_mu + log_rel_year_raw * log_rel_year_sigma) ; 

  // epsilon Matt trick
      for( i in 1:N_rel){
        epsilon[i] =  epsilon_raw[i] .* (cum_M2_temp) ; 
      }


  // Make logistic function for q for the various fleets.
  
    for(i in 1:N_loc){
      log_q_troll_start_rv[i] = log_q_troll_start[troll_idx[i]] ;
      log_q_troll_slope_rv[i] = log_q_troll_slope ;
      log_q_rec_us_start_rv[i] = log_q_rec_start[rec_us_idx[i]]  ;
      //log_q_rec_us_slope_rv[i] = log_q_rec_slope[rec_us_idx[i]] ;
    }
    
    // print("log_q_start", log_q_troll_start_rv);
    // print("log_q_slope", log_q_troll_slope_rv);
  
    for(i in 1:N_season_total ){
       log_q_troll_pos[i]   = log_q_troll_start_rv - 
                                            (log_q_troll_start_rv ./ (1 +exp(- log_q_troll_slope_rv * (q_year_vec[i] - q_int))) - log_q_troll_start_rv ) ; 
       // log_q_troll_pos[i]   = rep_row_vector(log_q_troll_start_rv - 
       //                                      (log_q_troll_start_rv / (1 +exp(- log_q_troll_slope * (q_year_vec[i] - q_int))) - log_q_troll_start_rv ),N_loc); 
       log_q_treaty_pos[i]   = rep_row_vector((log_q_treaty_start ) - 
                                            ((log_q_treaty_start ) / (1 +exp(- log_q_treaty_slope * (q_year_vec[i] - q_int))) - (log_q_treaty_start ) ),N_loc); 
       log_q_rec_pos[i]   = log_q_rec_us_start_rv  - 
                                            (log_q_rec_us_start_rv / (1 +exp(- log_q_rec_slope * (q_year_vec[i] - q_int))) - log_q_rec_us_start_rv ) ; 
        // log_q_rec_pos[i]   = rep_row_vector(log_q_rec_start  - 
        //                                     (log_q_rec_start / (1 +exp(- log_q_rec_slope * (q_year_vec[i] - q_int))) - log_q_rec_start ),N_loc);                     
       log_q_rec_can_pos[i]   = rep_row_vector((log_q_rec_can_start ) - 
                                            ((log_q_rec_can_start )/ (1 +exp(- log_q_rec_can_slope * (q_year_vec[i] - q_int))) - (log_q_rec_can_start ) ),N_loc);
       log_q_rec_can_irec_pos[i] = rep_row_vector(log_q_rec_can_irec_start ,N_loc); 
       
       log_q_hake_ashop_pos[i] = rep_row_vector(log_q_hake_ashop_start ,N_loc); 
       
       log_q_hake_shoreside_pos[i] = rep_row_vector(log_q_hake_shoreside_start ,N_loc); 
       
       log_q_pollock_GOA_pos[i] = rep_row_vector(log_q_pollock_GOA_start,N_loc); // NOTE THAT POLLOCK == ASHOP HAKE HERE!!!
       
       log_q_rockfish_AK_pos[i] = rep_row_vector(log_q_rockfish_AK_start ,N_loc); 
    } 
    
    // This is a section for dealing with the fact that there are two estimates of effort for certain times and locations in Canada.
        F_rec_can_array =   exp(log_q_rec_can_pos) .* K_rec_can +
                            exp(log_q_rec_can_irec_pos) .* K_rec_can_irec;
        for(i in 1:N_f_rec_overlap_effort_idx_param){
          F_rec_can_array[f_rec_overlap_effort_idx[i,1],f_rec_overlap_effort_idx[i,2]] =
                        0.5 * (F_rec_can_array[f_rec_overlap_effort_idx[i,1],f_rec_overlap_effort_idx[i,2]]);
        }    
    
    // Create flat matrices for the various fisheries.
          F_troll_array = exp(log_q_troll_pos) .* K_troll  + constant ;
          F_treaty_array = exp(log_q_treaty_pos) .* K_treaty + constant ;
          F_rec_array   = exp(log_q_rec_pos)  .* K_rec   +
                          F_rec_can_array +
                          exp(log_q_rec_pos) .* K_rec_PUSO +
                            constant;
          F_hake_ashop_array =exp(log_q_hake_ashop_pos) .* K_hake_ashop + constant ;
          F_hake_shoreside_array =exp(log_q_hake_shoreside_pos) .* K_hake_shoreside + constant ;
          F_pollock_GOA_array =exp(log_q_pollock_GOA_pos) .* K_pollock_GOA + constant ;
          F_rockfish_AK_array =exp(log_q_rockfish_AK_pos) .* K_rockfish_AK + constant ;

    ////// Fishing troll with observations but without effort.
    for(i in 1:N_f_troll_idx_param){
        F_troll_array[f_troll_param_idx[i,1],f_troll_param_idx[i,2]] =  F_troll[i]  ;//
    }
    // for(i in 1:N_f_treaty_idx_param){
    //     F_treaty_array[f_treaty_param_idx[i,1],f_treaty_param_idx[i,2]] =  F_treaty[i]  ;//
    // } 
    for(i in 1:N_f_rec_idx_param){
        F_rec_array[f_rec_param_idx[i,1],f_rec_param_idx[i,2]] =   F_rec[i]  ;//
    }
    // for(i in 1:N_f_hake_ashop_idx_param){
    //     F_hake_ashop_array[f_hake_ashop_param_idx[i,1],f_hake_ashop_param_idx[i,2]] =   F_hake_ashop[i]  ;//
    // }

    // make arrays on the scale of the release groups turn: (year,month,location) into (release id,model month, location)
  // print(vuln_age) ;
  // print(vuln_troll_array[1]) ;
  if(vuln_fixed >2){
  //This is the logistic formulation for vulnerability (this has an assymetrical shape to the vulnerability curve)
    for(i in 1:N_years_release){ // Make an array of the same dimensions as the state vector
      for(j in 1:N_time_mod){
      troll_mat[i,j]  = to_row_vector(vec_one_loc ./ (1 + exp(-(vuln_fixed + vuln_troll_array[i,j]  * (beta_vuln[1] * vuln_age[j])))));
      treaty_mat[i,j] = to_row_vector(vec_one_loc ./ (1 + exp(-(vuln_fixed + vuln_treaty_array[i,j] * (beta_vuln[1]  * vuln_age[j])))));
      rec_mat[i,j]    = to_row_vector(vec_one_loc ./ (1 + exp(-(vuln_fixed + vuln_rec_array[i,j]    * (beta_vuln[2]  * vuln_age[j])))));
      
      hake_ashop_mat[i,j] = to_row_vector(vec_one_loc ./ (1 + exp(-(vuln_fixed + (beta_vuln_hake[1] * vuln_age_hake[j] + beta_vuln_hake[2] * pow(vuln_age_hake[j] ,2) )))));
      hake_shoreside_mat[i,j] = to_row_vector(vec_one_loc ./ (1 + exp(-(vuln_fixed + (beta_vuln_hake[1] * vuln_age_hake[j] + beta_vuln_hake[2] * pow(vuln_age_hake[j],2) )))));

      pollock_GOA_mat[i,j] = to_row_vector(vec_one_loc ./ (1 + exp(-(vuln_fixed + (beta_vuln_pollock[1] * vuln_age_pollock[j] + beta_vuln_pollock[2] * pow(vuln_age_pollock[j] ,2) )))));
      rockfish_AK_mat[i,j] = to_row_vector(vec_one_loc ./ (1 + exp(-(vuln_fixed + (beta_vuln_pollock[1] * vuln_age_pollock[j] + beta_vuln_pollock[2] * pow(vuln_age_pollock[j] ,2) )))));


        // hake_ashop_mat[i,j] = to_row_vector(vec_one_loc ./ (1 + exp(-(vuln_fixed + (beta_vuln_hake  * vuln_age_trawl[j] + beta_vuln_hake*pow(vuln_age_trawl[j],2) )))));
        // hake_shoreside_mat[i,j] = to_row_vector(vec_one_loc ./ (1 + exp(-(vuln_fixed + (beta_vuln_hake  * vuln_age_trawl[j] + beta_vuln_hake*pow(vuln_age_trawl[j],2) )))));

      }
    }
  }


    for(i in 1:N_years_release){ // Make an array of the same dimensions as the state vector
      F_troll_fin[i]      = block(F_troll_array,(1+(i-1)*N_month),1,N_time_mod,N_loc) .* troll_mat[i] ;
      F_treaty_fin[i]     = block(F_treaty_array,(1+(i-1)*N_month),1,N_time_mod,N_loc) .* treaty_mat[i] ;
      F_rec_fin[i]        = block(F_rec_array,(1+(i-1)*N_month),1,N_time_mod,N_loc)   .* rec_mat[i]   ;
      F_hake_ashop_fin[i]     = block(F_hake_ashop_array,(1+(i-1)*N_month),1,N_time_mod,N_loc)   .* hake_ashop_mat[i]   ;
      F_hake_shoreside_fin[i] = block(F_hake_shoreside_array,(1+(i-1)*N_month),1,N_time_mod,N_loc)   .* hake_shoreside_mat[i]   ;
      F_pollock_GOA_fin[i] = block(F_pollock_GOA_array,(1+(i-1)*N_month),1,N_time_mod,N_loc)   .* pollock_GOA_mat[i]   ;
      F_rockfish_AK_fin[i] = block(F_rockfish_AK_array,(1+(i-1)*N_month),1,N_time_mod,N_loc)   .* rockfish_AK_mat[i]   ;
      
      F_tot_fin[i]        = F_troll_fin[i]  + F_rec_fin[i] + F_treaty_fin[i] +F_hake_ashop_fin[i] + F_hake_shoreside_fin[i] +
                                F_pollock_GOA_fin[i] + F_rockfish_AK_fin[i] +
                                // THIS IS EXTRA MORTALITY FOR SHAKERS AND SUCH FROM THE FISHERY
                                block(F_troll_array,(1+(i-1)*N_month),1,N_time_mod,N_loc) .* (1-troll_mat[i]) * shaker_mort +
                                block(F_treaty_array,(1+(i-1)*N_month),1,N_time_mod,N_loc) .* (1-treaty_mat[i]) * shaker_mort + 
                                block(F_rec_array,(1+(i-1)*N_month),1,N_time_mod,N_loc)   .* (1-rec_mat[i]) * shaker_mort   ;
    }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////// LATENT STATES
  { // local variables
   // Helper files for calculating Fishing mortalities
   real F_focal ;
   real F_tot ;
   real log_N_temp_1 ;
   vector[N_loc] log_N_temp_vec ;
   real log_N_temp_off;

  for(i in 1:N_rel){
    D[i,1] = 1e-5 ; // ensure there are a trivial number of spawners at the first time point, even if the releases happen after the first time.
    // D[i,2] = 1e-5 ;
    // D[i,3] = 1e-5 ;
    // D[i,4] = 1e-5 ;
    // D[i,5] = 1e-5 ;
    D[i,N_year] = 1e-5 ; // ensure final year is not nan
    for(j in 1:N_time_mod_rel[i]){
        if(j == (start_month_idx[i]+1)){ // This accounts for cases when the fish were released after the model starts.
              log_N_all[i,j] =  log_sum_exp(
                                  log_N0[i] - rel_year_all[i] + //* month_rec[i] -
                                  log(origin_mat[origin_idx[i],season_idx[j]]) -
                                  cum_M2_temp[j]   -
                                  to_vector(F_tot_fin[start_year[i], j])) + epsilon[i,j];
              log_N_off[i,j] =    log_N0[i] - rel_year_all[i] + //* month_rec[i] -
                                  log(origin_off[origin_idx[i]]) -
                                  cum_M2_temp[j] + epsilon[i,j];
                                  
          if(spawn_time_array[i,j] > 0 ){
              // print("stoped here A") ;
              log_N_temp_vec = log_N0[i] - rel_year_all[i] +
                              log(origin_mat[origin_idx[i],season_idx[j]]) -
                              to_vector(F_tot_fin[start_year[i], j]) * spawn_time_fraction[i] -
                              cum_M2_temp[j] * spawn_time_fraction[i] ;
              log_N_temp_off = log_N0[i] - rel_year_all[i] +
                              log(origin_off[origin_idx[i]]) -
                              cum_M2_temp[j] * spawn_time_fraction[i] ;

              D[i,spawn_time_array[i,j]] = sum(exp(log_N_temp_vec +
                                        log(prob_age_year[origin_idx[i],juv_idx[i], spawn_time_array[i,j]]) - river_entry[i] * inv(spawn_smooth))) +
                                        exp(log_N_temp_off + log(prob_age_year[origin_idx[i],juv_idx[i], spawn_time_array[i,j]]) - 2 * inv(spawn_smooth)) ; // make offshore equivalent to 1 space away from spawning ground.
              
              log_N_all[i,j] = log_sum_exp(
                                  log_N_temp_vec +
                                  log(1- exp(log(prob_age_year[origin_idx[i],juv_idx[i], spawn_time_array[i,j]]) - river_entry[i] * inv(spawn_smooth))) -
                                  to_vector(F_tot_fin[start_year[i], j]) * (1-spawn_time_fraction[i]) -
                                  cum_M2_temp[j] * (1-spawn_time_fraction[i])) +
                                  epsilon[i,j] ;
              log_N_off[i,j] =    log_N_temp_off +
                                  log(1- exp(log(prob_age_year[origin_idx[i],juv_idx[i], spawn_time_array[i,j]]) - inv(spawn_smooth))) -
                                  cum_M2_temp[j] * (1-spawn_time_fraction[i]) +
                                  epsilon[i,j];
          }
        }
        if(j> (start_month_idx[i]+1)){
          if(j != N_time_mod_rel[i]){
            if(spawn_time_array[i,j] == 0){
             // print("stoped here B") ;

             
              log_N_all[i,j]   =   log_sum_exp(
                                          log(exp(log_N_all[i,j-1]) + exp(log_N_off[i,j-1])) +
                                          log(origin_mat[origin_idx[i],season_idx[j]]) -
                                           cum_M2_temp[j]   -
                                           to_vector(F_tot_fin[start_year[i], j])) + epsilon[i,j];
              log_N_off[i,j] =    log(exp(log_N_all[i,j-1]) + exp(log_N_off[i,j-1])) +
                                  log(origin_off[origin_idx[i]]) -
                                  cum_M2_temp[j] + epsilon[i,j];
                                           
           // if(i==1135){
           //   if(j == (N_time_mod-1) ){
           //   print("log(mean) =",log_rel_year_mu,"; log(sd) = ",log_rel_year_sigma," rel_year_all = ",rel_year_all[i-10]," , ",rel_year_all[i]," , ",rel_year_all[i+10]);
           //   print("log_N_all = ",log_N_all[i]);
           //   print("Dist = ",(origin_mat[origin_idx[i],origin_year_idx[i,j]]));  
           //   //print("Sum Dist = ",sum(origin_mat[origin_idx[i],origin_year_idx[i,j]]));  
           //   print("M2 =" , cum_M2_temp);
           //   //print("F1 = ",to_vector(F_tot_fin[start_year[i], 1])); 
           //   print("F = ",to_vector(F_tot_fin[start_year[i], j])); 
           //   print("Epsilon = ",epsilon[i]);
           //  
           //   print("troll int ",log_q_troll_start,"; troll slope ",log_q_troll_slope) ;
           //   print("rec int ",log_q_rec_start,"; rec slope ",log_q_rec_slope) ;
           //   print("treaty int ",log_q_treaty_start,"; treaty slope ",log_q_treaty_slope) ;
           //   print("rec can int ",log_q_rec_can_irec_start) ;
           //   print("ahsop int",log_q_hake_ashop_start, "shoreside int ",log_q_hake_shoreside_start) ;
           //   print("beta_vuln ",beta_vuln," hake", beta_vuln_hake) ;
           //   // print("w_star_ws ",w_star_ws[1] );
           //   // print("w_star_sf sum ",w_star_sf[1,1] );
           //   // print("w_star_sf fall ",w_star_sf[1,2] );
           //   // print("origin_mat sum",origin_mat[1,2] );
           //   // print("origin_mat wint",origin_mat[1,1] );
           //  // print("Tau_process_prod = ",tau_process_prod);
           //   //print("SD_proc = ",sqrt(tau_process_prod*cum_M2_temp));
           //   }
           // }
            }
           }
            if(spawn_time_array[i,j] > 0 ){
              if(j < N_time_mod_rel[i]){
               // print("stoped here C") ;
              log_N_temp_vec = log(exp(log_N_all[i,j-1]) + exp(log_N_off[i,j-1])) +
                              log(origin_mat[origin_idx[i],season_idx[j]]) -
                              to_vector(F_tot_fin[start_year[i], j]) * spawn_time_fraction[i] -
                              cum_M2_temp[j] * spawn_time_fraction[i] ;
              log_N_temp_off = log(exp(log_N_all[i,j-1]) + exp(log_N_off[i,j-1])) +
                              log(origin_off[origin_idx[i]]) -
                              cum_M2_temp[j] * spawn_time_fraction[i] ;

              D[i,spawn_time_array[i,j]] = sum(exp(log_N_temp_vec +
                              log(prob_age_year[origin_idx[i],juv_idx[i], spawn_time_array[i,j]]) - river_entry[i] * inv(spawn_smooth))) +
                              exp(log_N_temp_off + log(prob_age_year[origin_idx[i],juv_idx[i], spawn_time_array[i,j]]) - 2 * inv(spawn_smooth)) ; // make offshore equivalent to 1 space away from spawning ground.

              log_N_all[i,j] = log_sum_exp(
                                  log_N_temp_vec +
                                  log(1- exp(log(prob_age_year[origin_idx[i],juv_idx[i], spawn_time_array[i,j]]) - river_entry[i] * inv(spawn_smooth))) -
                                  to_vector(F_tot_fin[start_year[i], j]) * (1-spawn_time_fraction[i]) -
                                  cum_M2_temp[j] * (1-spawn_time_fraction[i])) +
                                  epsilon[i,j] ;
              log_N_off[i,j] =    log_N_temp_off +
                                  log(1- exp(log(prob_age_year[origin_idx[i],juv_idx[i], spawn_time_array[i,j]]) - 2 * inv(spawn_smooth))) -
                                  cum_M2_temp[j] * (1-spawn_time_fraction[i]) +
                                  epsilon[i,j];
              }
              if(j==N_time_mod_rel[i]){
                
                log_N_temp_vec = log(exp(log_N_all[i,j-1] + log_N_off[i,j-1]))  +
                              log(origin_mat[origin_idx[i],season_idx[j]]) -
                              to_vector(F_tot_fin[start_year[i], j]) * spawn_time_fraction[i] -
                              cum_M2_temp[j] * spawn_time_fraction[i] ;
                log_N_temp_off = log(exp(log_N_all[i,j-1] + log_N_off[i,j-1]))  +
                              log(origin_off[origin_idx[i]]) -
                              cum_M2_temp[j] * spawn_time_fraction[i] ;
               
                D[i,spawn_time_array[i,j]] = sum(exp(log_N_temp_vec)) + exp(log_N_temp_off) ;
          
              }// end last model step.
            } // end spawn_time_array[i,j] > 0 

          // if(i==1135){
          //    if(spawn_time_idx[j]==4){
          //    //print("Vec move to river(log) = ",log(prob_age_year[loc_spawn_idx[i], spawn_time_idx[j]]) - river_entry[i] * pow(spawn_smooth,-1));
          //    print("Vec move to river(prob) = ",spawn_time_idx[j]," ; ",exp(log(prob_age_year[loc_spawn_idx[i], spawn_time_idx[j]]) - river_entry[i] * inv(spawn_smooth)));
          //    print("spawn_smooth = ",spawn_smooth);}
          //    }
          // }
             // if(j == N_time_mod){
           //   if(spawn_time_array[i,j] > 0){
           //    log_N_temp_vec = log_N_all[i,j-1] +
           //                    log(origin_mat[origin_idx[i],origin_year_idx[i,j]]) -
           //                    to_vector(F_tot_fin[start_year[i], j]) * spawn_time_fraction[i] -
           //                    cum_M2_temp[j] * spawn_time_fraction[i] ;
           //    
           //    D[i,spawn_time_array[i,j]] = sum(exp(log_N_temp_vec)) ;
           //    }
           // } // end j == N_time_mod
          } // end j>(start_month_idx[i]+1) if statement
          
        // eliminate process error for final step (messes up N_ratio calculation)
          // if(j == N_time_mod){
          //  if(spawn_time[j] == 0){
          //     log_N_all[i,j]   =   log(sum(exp(
          //                                 log_N_all[i,j-1] +
          //                                 log(origin_mat[origin_idx[i],origin_year_idx[i,j]]) -
          //                                 cum_M2_temp[j]   -
          //                                 to_vector(F_tot_fin[start_year[i], j])))) ;
          //  }
          //   if(spawn_time[j] > 0){
          //                  log_N_temp_vec = log_N_all[i,j-1] +
          //                     log(origin_mat[origin_idx[i],origin_year_idx[i,j]]) -
          //                     to_vector(F_tot_fin[start_year[i], j]) * spawn_time_fraction[i] -
          //                     cum_M2_temp[j] * spawn_time_fraction[i] ;
          // 
          //     D[i,spawn_time_idx[j]] = sum(exp(log_N_temp_vec +
          //                               log(prob_age_year[loc_spawn_idx[i], spawn_time_idx[j]]) - river_entry[i] * inv(spawn_smooth))) ;
          //               
          //     log_N_all[i,j] = log(sum(exp(
          //                         log_N_temp_vec +
          //                         log(1- exp(log(prob_age_year[loc_spawn_idx[i], spawn_time_idx[j]]) - river_entry[i] * inv(spawn_smooth))) -
          //                         to_vector(F_tot_fin[start_year[i], j]) * (1-spawn_time_fraction[i]) -
          //                         cum_M2_temp[j] * (1-spawn_time_fraction[i])))) ;
          //  }
          // }
        } // End j loop for N_time_mod
    // if(is_nan(log_N_all[i,j])){print("log_N NAN ",i) ;}
    // if(is_nan(D[i,j])){print("D is NAN ",i) ;}
    } // End for loop over N_rel.
  } // end local variables
//log_N_ratio = (log_N_all')[N_log_N_all] - ((to_vector(log_N0) - rel_year_all)')  ; //Add back in mortality from first period.
//arf ;
// Proportion of fish that show up in the river
// for(i in 1:100){
//   print("D=",i," ",D[i,]) ;
// }

    for(i in 1:N_rel){
      prop_D[i] = D[i] / sum(D[i]) ;
    }
    for(i in 1:N_PIT){
      prop_PIT[i] =  sum(D[PIT_idx[i]]) / exp(log_N0[PIT_idx[i]]) ;
      if(prop_PIT[i] > 1){
        prop_PIT[i] = 1 - 1e-10;
      }
    }
    
} // end transformed parameters section. 
model {////////////////////////////////////////////////////////////////////////////////////////////////////
    // Process Error
      //for(i in 1:N_rel){
      //   //epsilon[i] ~ normal(0 ,tau_process) ;
      //   epsilon[i] ~ normal(0 , tau_process_prod * cum_M2_temp) ;
      //} 
      
      for(i in 1:N_rel){
        //epsilon[i] ~ normal(0 ,tau_process) ;
        epsilon_raw[i] ~ normal(0,1) ;
      }

  // Smoothing parameters priors
    //phi_space_mean 
    //phi_space ~ gamma(phi_space_prior[1],phi_space_prior[2]) ;  
    theta_space ~ gamma(theta_space_prior[1],theta_space_prior[2]) ; 
   
   
  for(i in 1:N_origin){
      // ORIGINAL
      // w_star_ws[i] ~ multi_normal_cholesky(zero_vec_pred_loc_ws,L_knot_ws[i]) ;
      // w_star_salish[1,i] ~ normal(0,phi_space_fix) ;
      // w_star_offshore[i,1] ~ normal(0,phi_space_fix) ;
      for(j in 1:2){
        // if(j>1){
          w_star_salish[j,i] ~ normal(0,phi_space_fix) ;
          w_star_ws[j,i] ~ multi_normal_cholesky(zero_vec_pred_loc_ws,L_knot_ws[i]) ;
          w_star_sf[j,i] ~ multi_normal_cholesky(zero_vec_pred_loc_sf,L_knot_sf[i]) ;
         // w_star_offshore[i,j] ~ normal(0,phi_space_fix) ;
      }
    }
    
    w_logit_offshore ~ normal(-3,1.5); // logit prior for offshore proportions.
    

  ////////////////////////////////////////
    // Prior on temperature-distribution slopes (centered on 0)
    // for(i in 1:N_origin){
    //   for(j in 1:N_season){
    //     // w_star[j,i] ~ normal(w_star_prior_mean[i],w_star_prior_sd[i]) ;
    //     origin_sea_slope[j,i] ~ normal(origin_sea_slope_prior_mean[i],origin_sea_slope_prior_sd[i]) ;
    //   }
    // }

    //alpha_pay_mean ~normal(gamma_int_prior[1],gamma_int_prior[2]) ;
    //beta_pay_mean ~ normal(gamma_slope_prior[1],gamma_slope_prior[2]) ;

    //alpha_pay_sd ~ lognormal(0.1,0.2) ;
    //log_beta_pay ~ normal(0,0.5) ;
    // for(i in 1:N_origin){
    //   alpha_pay[i] ~ normal(gamma_int_prior[1],gamma_int_prior[2]) ;
    //   log_beta_pay[i]  ~ normal(gamma_slope_prior[1],gamma_slope_prior[2]);
    // }
    for(i in 1:N_origin){
      for(j in 1:N_juv){
      gamma_pay[i,j] ~ normal(0,3); 
      }
    }
      spawn_smooth ~ gamma(spawn_smooth_prior[1],spawn_smooth_prior[2]) ;

  // prior on slopes for beta_vuln (must be positive)
      beta_vuln ~  gamma(beta_vuln_prior[1],beta_vuln_prior[2]) ;
      beta_vuln_hake[1] ~  normal(-1,1) ;
      beta_vuln_hake[2] ~  normal(-1,1) ;
      beta_vuln_pollock ~  normal(-1,1) ;
      //beta_vuln_rockfish ~  normal(-1,1) ;

      sigma_cv ~  gamma(sigma_cv_prior[1],sigma_cv_prior[2]) ; // remember the sigma_cv is the CV of the true catch)
      sigma_cv_hake ~ gamma(sigma_cv_prior[1],sigma_cv_prior[2]) ;
      sigma_cv_pollock ~ gamma(sigma_cv_prior[1],sigma_cv_prior[2]) ;
  // NB overdispersion on AWG data
      // nb_phi ~gamma(nb_phi_prior[1],nb_phi_prior[2]) ;

  // rel_year_all on a monthly scale make prior on log-scale.
      log_rel_year_mu ~ normal(log_rel_year_mu_prior[1],log_rel_year_mu_prior[2]) ;
      log_rel_year_sigma  ~ gamma(log_rel_year_sigma_prior[1],log_rel_year_sigma_prior[2]);
      log_rel_year_raw ~ normal(0,1) ;

  // Fishing mortality hierarchical
      // F_rec ~ lognormal(log_F_rec_mean,F_rec_sigma) ;
      // log_F_rec_mean ~ normal(log_F_prior[1],log_F_prior[2]) ;
      // F_rec_sigma  ~ gamma(F_rec_sigma_prior[1],F_rec_sigma_prior[2]) ;

     log_F_rec_mean ~ normal(log_F_prior[1],log_F_prior[2]) ;
     log_F_troll_mean ~ normal(log_F_prior[1],log_F_prior[2]) ;
     
     F_troll_sigma ~   gamma(F_rec_sigma_prior[1],F_rec_sigma_prior[2]) ;
     F_rec_sigma ~   gamma(F_rec_sigma_prior[1],F_rec_sigma_prior[2]) ;
     log_F_rec_raw ~ normal(0,1) ; // MATT TRICK FOR F_rec params without effort.
     log_F_troll_raw ~ normal(0,1) ; // MATT TRICK FOR F_troll params without effort.
     // log_F_hake_ashop_raw ~ normal(0,1) ; // MATT TRICK FOR F_hake_ashop params without effort.

 // Priors for catchability
      log_q_troll_start ~ normal(log_q_troll_prior[1] ,log_q_troll_prior[2]) ;
      log_q_troll_slope ~ gamma(log_q_slope_prior[1],log_q_slope_prior[2] );

      log_q_treaty_start ~ normal(log_q_treaty_prior[1] ,log_q_treaty_prior[2]) ;
      log_q_treaty_slope ~ gamma(log_q_slope_prior[1],log_q_slope_prior[2] );

      log_q_rec_start ~ normal(log_q_rec_prior[1] ,log_q_rec_prior[2]) ;
      log_q_rec_slope ~ gamma(log_q_slope_prior[1],log_q_slope_prior[2] );

      log_q_rec_can_start ~ normal(log_q_rec_prior[1] ,log_q_rec_prior[2]) ;
      log_q_rec_can_slope ~ gamma(log_q_slope_prior[1],log_q_slope_prior[2] );

      log_q_rec_can_irec_start ~ normal(log_q_rec_prior[1] ,log_q_rec_prior[2]) ;

      log_q_hake_ashop_start ~ normal(log_q_hake_prior[1] ,log_q_hake_prior[2]) ;
      log_q_hake_shoreside_start ~ normal(log_q_hake_prior[1] ,log_q_hake_prior[2]) ;
      log_q_pollock_GOA_start ~ normal(log_q_pollock_prior[1] ,log_q_pollock_prior[2]) ;
      log_q_rockfish_AK_start ~ normal(log_q_rockfish_AK_prior[1] ,log_q_rockfish_AK_prior[2]) ;

      q_int ~ normal(q_int_prior[1],q_int_prior[2]) ;
  
  // M2 prior
     //log_M2_raw ~ normal(0,1) ;
     //log_M2 ~ multi_normal(MU_M2,Sigma_M2) ;

{ /////////// Declare Local Variables
   // Latent States
     //vector[N_obs_bin] lambda ;
       // for the pres-abs model
       real lambda_temp ;
       vector[N_obs_bin] prob   ;
    
     // helper local variables
      //real mu_temp ;
      real alpha_temp ;
      real beta_temp ;
      real E_trunc_temp ;
      real V_trunc_temp ;
      real prob_0_temp ;
      
    // 
    // // for the positive model   
       vector[N_obs_pos] alpha_pos ;
       vector[N_obs_pos] beta_pos ;
       //  vector<lower=0>[N_obs_pos] alpha_pos ;
       // vector<lower=0>[N_obs_pos] beta_pos ;

      // Helper objects for the observations
       real F_focal ;
       real F_tot ;
       real log_N_temp_1 ; 
       real log_N_temp_2 ; 

////// CALCULATING THE PREDICTIONS FOR OBSERVATIONS
  for(i in 1:N_obs_bin){
      if( gear_bin_idx[i]  == 1 ){ F_focal = F_troll_fin[start_year[rel_idx[i]],mod_time_idx[i],loc_idx[i]] ; }
      if( gear_bin_idx[i]  == 2 ){ F_focal = F_rec_fin[start_year[rel_idx[i]],mod_time_idx[i],loc_idx[i]] ; }
      if( gear_bin_idx[i]  == 3 ){ F_focal = F_treaty_fin[start_year[rel_idx[i]],mod_time_idx[i],loc_idx[i]] ; }
      if( gear_bin_idx[i]  == 4 ){ F_focal = F_hake_ashop_fin[start_year[rel_idx[i]],mod_time_idx[i],loc_idx[i]] ; }
      if( gear_bin_idx[i]  == 5 ){ F_focal = F_hake_shoreside_fin[start_year[rel_idx[i]],mod_time_idx[i],loc_idx[i]] ; }
      if( gear_bin_idx[i]  == 6 ){ F_focal = F_pollock_GOA_fin[start_year[rel_idx[i]],mod_time_idx[i],loc_idx[i]] ; }
      if( gear_bin_idx[i]  == 7 ){ F_focal = F_rockfish_AK_fin[start_year[rel_idx[i]],mod_time_idx[i],loc_idx[i]] ; }
      
      //if(i==124){ 
      //   print("start_year ",start_year[rel_idx[i]]);
      //   print("mod_time_idx ", mod_time_idx[i]) ;
      //   print("loc_idx ",loc_idx[i]);
      //   //print("F_pollock_GOA_fin start year ",F_pollock_GOA_fin[start_year[rel_idx[i]]]) ;
      // }
      // print("start_month ",start_month_idx[rel_idx[i]]) ;
      // print("mod_time_idx ",mod_time_idx[i]) ;
      F_tot = F_tot_fin[start_year[rel_idx[i]],mod_time_idx[i],loc_idx[i]];
      
      // Calculate abundance.  
        if(mod_time_idx[i] == (start_month_idx[rel_idx[i]]+1)){ 
          log_N_temp_2 =  log_N0[rel_idx[i]] - rel_year_all[rel_idx[i]] ;
        }else{
          log_N_temp_2 = log(exp(log_N_all[rel_idx[i],mod_time_N_all_idx[i]]) + exp(log_N_off[rel_idx[i],mod_time_N_all_idx[i]])) ;
        }

      // if( i == 49374){
      // print("STA = ",spawn_time_array[rel_idx[i] ,mod_time_idx[i]]) ;
      // print("mod_time_idx ", mod_time_idx[i]) ;
      // print("F_focal =", F_focal);
      // print("F_tot =", F_tot);
      // print("log_N_all = ", log_N_all[rel_idx[i],mod_time_N_all_idx[i]]);
      // print("log_N_off = ", log_N_all[rel_idx[i],mod_time_N_all_idx[i]]);
      // }

      // 
      // if( i == 8481){
      // print(i) ;
      // print("STA = ",spawn_time_array[rel_idx[i] ,mod_time_idx[i]]) ;
      // print("mod_time_idx ", mod_time_idx[i]) ;
      // print("F_focal =", F_focal);
      // print("F_tot =", F_tot);
      // print("log_N_all = ", log_N_all[rel_idx[i],mod_time_N_all_idx[i]]);
      // print("log_N_off = ", log_N_all[rel_idx[i],mod_time_N_all_idx[i]]);
      // 
      // }
     
     
     
     
     
      if(spawn_time_array[rel_idx[i] ,mod_time_idx[i]] == 0){
            lambda_temp =
                  Baranov(  cum_M2_temp[mod_time_idx[i]],
                      F_focal,
                      F_tot,
                      log_N_temp_2,
                      ////log(origin_loc[season_idx[mod_time_idx[i]],origin_bin_idx[i],loc_idx[i]] ) );
                      log(origin_mat[origin_bin_idx[i],season_bin_idx[i],loc_idx[i]] ) );
        }
        
        if(spawn_time_array[rel_idx[i] ,mod_time_idx[i]] > 0){
          if(mod_time_idx[i] < N_time_mod_rel[rel_idx[i]]){
            // print("mod_time ",  mod_time_N_all_idx[i]]) ;
            // print("rel_idx",rel_idx[i]);
            // print("log_N_all ",log_N_all[rel_idx[i],mod_time_N_all_idx[i]]) ;
            lambda_temp =
                Baranov(  cum_M2_temp[mod_time_idx[i]] * spawn_time_fraction[rel_idx[i]],
                F_focal * spawn_time_fraction[rel_idx[i]],
                F_tot * spawn_time_fraction[rel_idx[i]],
                log_N_temp_2,
                log(origin_mat[origin_bin_idx[i],season_bin_idx[i],loc_idx[i]] ) );
            
            //  if(i==1){
            //  print("lambda_temp A ",lambda_temp) ;
            //  print("M2 ",cum_M2_temp[mod_time_idx[i]] * spawn_time_fraction[rel_idx[i]]) ;
            //  print("log_N_temp_2 ",log_N_temp_2);
            //  print("log_N_temp_1 ",log_N_temp_1);
            //  print("F_tot ", F_tot) ;
            //  print("F_focal ", F_focal) ;
            //  print("SPT ",spawn_time_fraction[rel_idx[i]]) ;
            // }

            
            log_N_temp_1 = log_N_temp_2 + //log_N_all[rel_idx[i],mod_time_N_all_idx[i]] +
                              log(origin_mat[origin_bin_idx[i],season_bin_idx[i],loc_idx[i]]) -
                              F_tot * spawn_time_fraction[rel_idx[i]] -
                              cum_M2_temp[mod_time_idx[i]] * spawn_time_fraction[rel_idx[i]] +
                              log(1- exp(log(prob_age_year[origin_bin_idx[i],juv_bin_idx[i], spawn_time_array[rel_idx[i],mod_time_idx[i]]]) - river_entry[rel_idx[i],loc_idx[i]] * inv(spawn_smooth))) ;
            lambda_temp =
                  //logit_offset_int[i]  + logit_offset_slope *
                  log(exp(lambda_temp) +
                      exp(Baranov(cum_M2_temp[mod_time_idx[i]] * (1-spawn_time_fraction[rel_idx[i]]) ,
                            F_focal * (1-spawn_time_fraction[rel_idx[i]]),
                            F_tot * (1-spawn_time_fraction[rel_idx[i]]),
                            log_N_temp_1,
                            0)));
                            
            //  if(i==1){
            //  print("lambda_temp B ",lambda_temp) ;
            //  print("M2 ",cum_M2_temp[mod_time_idx[i]] * spawn_time_fraction[rel_idx[i]]) ;
            //  print("log_N_temp_2 ",log_N_temp_2);
            //  print("log_N_temp_1 ",log_N_temp_1);
            //  print("F_tot ", F_tot) ;
            //  print("F_focal ", F_focal) ;
            //  print("SPT ",spawn_time_fraction[rel_idx[i]]) ;
            // }

          }else{
            lambda_temp =
                Baranov(  cum_M2_temp[mod_time_idx[i]] * spawn_time_fraction[rel_idx[i]],
                F_focal * spawn_time_fraction[rel_idx[i]],
                F_tot * spawn_time_fraction[rel_idx[i]],
                log_N_temp_2,
                log(origin_mat[origin_bin_idx[i],season_bin_idx[i],loc_idx[i]] ) );
          }
        }

      if( gear_bin_idx[i]  == 1 ){ alpha_temp = alpha_calc(sigma_cv[1])  ; } //troll
      if( gear_bin_idx[i]  == 2 ){ alpha_temp = alpha_calc(sigma_cv[2]) ; }  //rec
      if( gear_bin_idx[i]  == 3 ){ alpha_temp = alpha_calc(sigma_cv[1]) ; } //treaty
      if( gear_bin_idx[i]  >= 4 ){ alpha_temp = alpha_calc(sigma_cv_hake) ; } //all trawl bycatch fleets (hake (ashop & shore) and rockfish except pollock)
      if( gear_bin_idx[i]  == 6 ){ alpha_temp = alpha_calc(sigma_cv_pollock) ; }
    // if(gear_bin_idx[i]<=3){        alpha_temp = alpha_calc(sigma_cv_temp) ;}
    //  if(gear_bin_idx[i]>=4){        alpha_temp = 1 ;}
    
    beta_temp  = beta_calc(lambda_temp, alpha_temp) ;

    prob[i] = 1- Prob_0_Calc(alpha_temp,beta_temp, inv_frac_samp[i]) ;
    
    // if(i==582){
    //   print("rel_idx ", rel_idx[i] ) ;
    //   print("log_N_temp_1",log_N_temp_1) ;
    //   print("log_N_temp_2",log_N_temp_2) ;
    //   print("lambda_temp ",lambda_temp) ;
    //   print("alpha_temp ",alpha_temp) ;
    //   print("inv_frac_samp",inv_frac_samp[i]) ;
    //   // print("mod_time_idx ",mod_time_idx[i])
    //   print("prob ",i," ",prob[i]);
    // }
    // if(is_nan(prob[i])){print("NA ",i) ;}
    // if(is_inf(prob[i])){print("INF ",i) ;}
    // if( i == 100000){
    //   print("PRES-ABS MOD: ", i) ;
    //   print("Frac_samp ",inv(inv_frac_samp[i])) ;
    //   print("OBS ", bin_catch[i])
    //   print("LAMBDA ",lambda_temp) ;
    //   print("exp.LAMBDA (aka T) ",exp(lambda_temp)) ;
    //   print("alp ",alpha_temp,"; beta ",beta_temp,"; E[T]= ",alpha_temp/beta_temp) ; 
    //   print("NB Mean ",alpha_temp / (beta_temp*inv_frac_samp[i])) ;
    //   print("PROB_1 ",prob[i]) ;
    //   print("CV, gear = ",gear_bin_idx[i]," CV all = ",sigma_cv," ",sigma_cv_hake) ;
    //   print("phi space ",phi_space_origin_sf[1]," ; ",phi_space_origin_ws[1]) ;
    //   print("theta space ",theta_space) ;
    //  }
     if(prob[i]< 1e-12){ prob[i] = 1e-12;}   
     if(prob[i]>(1- 1e-12)){ prob[i] = 1-1e-12;} 
    
    // if(i == N_obs_bin){print("min prob= ",min(prob)) ;}
    
    
    // THIS SECTION CALCULATES THE POSITIVE COMPONENT OF THE MODEL (if it is observed > 0 )
    if(i <= N_obs_pos){  
      
      prob_0_temp =  1 - prob[i] ;
      E_trunc_temp = E_trunc_pos(alpha_temp,beta_temp, inv_frac_samp[i]) ;
      V_trunc_temp = V_trunc_pos(alpha_temp,beta_temp,E_trunc_temp, prob_0_temp, inv_frac_samp[i]) ;

        // if( i == 8481){
          //  print("POS MOD: ", i) ;
          //  print("OBS ", pos_catch[i]) ;
          //  print("Frac_samp ",inv(inv_frac_samp[i])) ;
          //  print("LAMBDA ",lambda_temp) ;
          //  print("exp.LAMBDA ",exp(lambda_temp)) ;
          //  print("E_trunc ",E_trunc_temp) ;
          // //print("PROB_0 ",prob_0_temp) ;
          // print("V_trunc ", V_trunc_temp) ;
          // print("SD_trunc ", pow(V_trunc_temp, 0.5)) ;
          // print("MEAN Pos Value ",exp(log_alpha_calc_pos( E_trunc_temp, V_trunc_temp)) /
          //                       exp(log_beta_calc_pos( E_trunc_temp, V_trunc_temp, log_inv_frac_samp_pos[i]))) ;
          // print("SD Pos Value ",pow(exp(log_alpha_calc_pos( E_trunc_temp, V_trunc_temp)) /
          //                       pow(exp(log_beta_calc_pos( E_trunc_temp, V_trunc_temp, log_inv_frac_samp_pos[i])),2),0.5)) ;
        //   print("ALP_POS_LIKE ",exp(log_alpha_calc_pos( E_trunc_temp, V_trunc_temp, log_inv_frac_samp_pos[i]))) ;
        //   print("BETA_POS_LIKE ",exp(log_beta_calc_pos( E_trunc_temp, V_trunc_temp, log_inv_frac_samp_pos[i]))) ;
        // }
        //if(V_trunc_temp < 1e-9){ print(i,"var error",)}
    
      alpha_pos[i] = exp(log_alpha_calc_pos( E_trunc_temp, V_trunc_temp)) ;
      beta_pos[i]  = exp(log_beta_calc_pos(  E_trunc_temp, V_trunc_temp, log_inv_frac_samp_pos[i])) ;

    //if(is_nan(alpha_pos[i])){print("NA ALP ",i) ;}
    //if(is_inf(alpha_pos[i])){print("INF ALP ",i) ;}

    //if(is_nan(beta_pos[i])){print("NA BETA ",i) ;}
    //if(is_inf(beta_pos[i])){print("INF BETA ",i) ;}

    }
} // end of loop over observations. 

// Likelihood part of Bayesian inference
        bin_catch ~ bernoulli(prob);
        pos_catch ~ gamma(alpha_pos , beta_pos) ;

} // end Local Variable Declaration 

  // Likelihood forcing the fraction of fish at the last time step to be about 0.001 of fish who survive to start the model.
       //log_N_ratio_mean ~ normal(log_N_ratio,log_N_ratio_sd) ;

  // Likelihood fraction disappearing to spawn. // This is for all RELEASES
    for( i in 1:N_rel){
      if(juv_idx[i]==1){  
        // print("i= ",i," origin= ",origin_idx[i]," 1") ;
        // print(prop_D[i]) ;
        // print(D[i]) ;
        E_prop_1[origin_idx[i]] ~  dirichlet(prop_D[i] * diri_constant) ;
      }else if(juv_idx[i]==2){
        E_prop_2[origin_idx[i]] ~  dirichlet(prop_D[i] * diri_constant) ;
      }
    }
   // Likelihood data for AWG data.  USE NB.  but the dispersion term goes to 0... so add 1 for now.
   for(i in 1:N_AWG){
      //AWG_dat[i] ~  neg_binomial_2(D[AWG_idx[i]],nb_phi_fix) ;
      AWG_dat[i] ~  poisson(D[AWG_idx[i]]) ;
   }

   // print("prop_PIT", prop_PIT[1]," and ", prop_PIT[10]);
   // Likelihood for PIT TAG DATA. // THIS IS FOR ALL RELEASES WITH PIT RELEASES (COLUMBIA RIVER)
     PIT_K ~ binomial(PIT_N, prop_PIT);
}


