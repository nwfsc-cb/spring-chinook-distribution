
### MATURATION CURVE (THIS IS FOR THE LOCAL PROBABILITY OF ENTERING THE RIVER... NOT THE GLOBAL PROBABILITY)
# Little code for generating priors for maturity curves and mortality schedules

library(MASS)

N.LOC.INIT <- length(unique(REL$ocean.region))

mat.age.true <- seq(1,max(OCEAN.MODEL.AGE$model.year),by=0.1)
mat.age <- mat.age.true-max(OCEAN.MODEL.AGE$model.year)
#log.mat.age <- log(mat.age)
lat <- seq(0,8, by=1)

gamma_0_fix      <- 2
#gamma_0_sig     <-  0.1
gamma_age_mu    <- 1.0
gamma_age_sig   <- 0.6
# gamma_lat_mu    <- 0.0
# gamma_lat_sig   <- 0.0001

corr_0_age      <- -0.8
corr_0_lat      <- 0
corr_age_lat    <- 0

#Make vectors and matrices

MU_gamma    <- c(rep(gamma_age_mu,N.LOC.INIT))
Sigma_gamma <- diag(gamma_age_sig^2,length(MU_gamma))

# Sigma_gamma[,1] <- c(gamma_0_sig^2 , gamma_0_sig * gamma_age_sig * corr_0_age, gamma_0_sig * gamma_lat_sig * corr_0_lat)
# Sigma_gamma[,2] <- c(gamma_0_sig * gamma_age_sig * corr_0_age, gamma_age_sig^2 , gamma_age_sig * gamma_lat_sig * corr_age_lat)
# Sigma_gamma[,3] <- c( gamma_0_sig * gamma_lat_sig * corr_0_lat, gamma_age_sig * gamma_lat_sig * corr_age_lat, gamma_lat_sig^2)

Sigma_gamma
MU_gamma

param <- mvrnorm(1000,MU_gamma,Sigma_gamma)

pred <- list()

for(i in 1:N.LOC.INIT){
  temp <- -log(1-plogis(gamma_0_fix + param[,i] %*% t(mat.age)))
  quant.temp <- data.frame(t(as.matrix(apply(temp,2,quantile,probs=c(0.05,0.25,0.5,0.75,0.95)))))
  colnames(quant.temp) <- paste("q.",c("05",25,50,75,95),sep="")
  quant.temp$Mean <- colMeans(temp)
  quant.temp <- 1-exp(-quant.temp)
  quant.temp$mat.age <- mat.age.true
  pred[[i]] <- quant.temp
}
names(pred) <- paste("X",move_id$ocean_region,sep=".")
pred
### Make some plots

par(mfrow=c(3,1),mar=c(3,3,0.5,0.5))
these <- sort(sample(1:N.LOC.INIT,min(N.LOC.INIT,3)))


for(i in names(pred)[these]){
  temp <- pred[[i]]
  print(i)
  x.lim <- c(1,max(OCEAN.MODEL.AGE$model.year))
  y.lim <- c(0,1)
  
  #   plot(Mean~mat.age, data=temp, lwd=4,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
  #     par(new=T)
  plot(q.05~mat.age, data=temp, lwd=1,lty=2,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
  par(new=T)
  plot(q.25~mat.age, data=temp, lwd=3,lty=3,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
  par(new=T)
  plot(q.50~mat.age, data=temp, lwd=3,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
  par(new=T)
  plot(q.75~mat.age, data=temp, lwd=3,lty=3,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
  par(new=T)
  plot(q.95~mat.age, data=temp, lwd=1,lty=2,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
  
  axis(1, at = 1:6,labels=1:6)
  axis(2,las=2)
  box(bty="l")
  text(i,x=0,y=0.9)
  
  abline(h=0.5,col=2)
  abline(v=c(2,3),col=2 )
}

######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################

# prior for juvenile mortality (monthly mortality)
  log_M1_mu_prior_mean   <- log(0.22)
  log_M1_mu_prior_sig   <- 0.25

  log_M1_sigma_prior_a  <- 5
  log_M1_sigma_prior_b  <- 100
  
  log_M1_mu_prior <- c(log_M1_mu_prior_mean,log_M1_mu_prior_sig)
  log_M1_sigma_prior <- c(log_M1_sigma_prior_a,log_M1_sigma_prior_b)
#   XXX <- seq(0.01,6,length.out = 1000)
#   plot(XXX,dgamma(XXX,m1_a,m1_b))

  # Mortality that declines with ocean age
intercept <-24 # age in month at which the mortality rate is constant.
month <- 1:max(OCEAN.MODEL.AGE$ocean.age)
month[month>=25] <- 24

month <- month - intercept 

# make files that can be read into STAN
mort_age <- ocean_age
mort_age[mort_age>intercept] <- intercept
mort_age  <- mort_age- intercept

###################################################

#  log.M <- log(0.04) - 0.03 * month
# log.M <- -2.02 - 0.025 * month
# 
# M <- exp(log.M)
#  
#  #plot(month,exp(-M))
#  
#  prod(exp(-M[1:12])); exp(sum(-M[1:12]))
#  prod(exp(-M[13:24]))
#  prod(exp(-M[25:36]))
#  prod(exp(-M[37:48]))
##################################################################

#m0_mu   <- log(-log(0.85)/12) # THIS IS THE REFERENCE
#m10mu   <- 0.080



# m0_mu   <-log(-log(0.85)/12)
m0_sig  <- 0.05
# m1_mu   <- - 0.06
m1_sig  <- 0.001
corr_m0_m1 <- -0.5

# THESE ARE THE VALUES USED IN THE 2020-06 Fits:
m0_mu   <-log(-log(0.90)/12)
m1_mu   <- - 0.087  ## THIS SHOULD BE 0.08 for REFERENCE

#Deterministic 
log.M.det <- m0_mu + m1_mu %*% t(month) 
M.det     <- exp(log.M.det)

# m0_mu   <- -6
# m1_mu   <- - 0.06
 # m0_mu <- -3.75
 # m1_mu <- -0.065

MU_m    <- c(m0_mu,m1_mu)
Sigma_m <- matrix(c(m0_sig^2, m0_sig * m1_sig * corr_m0_m1, m0_sig * m1_sig * corr_m0_m1, m1_sig^2),2,2)

#### FIX THIS TO BE IDENTICAL TO THE STAN PARAMETERIZATION...
param_m <- mvrnorm(10000,MU_m,Sigma_m)

    temp.log.M <-  param_m[,1] + param_m[,2] %*% t(month) 
    temp.M <- exp(temp.log.M)
    temp.S <- exp(-exp(temp.log.M))
    surv.all <- data.frame(surv.1=apply(temp.S[,1:12],1,prod),
                          surv.2=apply(temp.S[,13:24],1,prod), 
                          surv.3=apply(temp.S[,25:36],1,prod),
                          surv.4=apply(temp.S[,37:48],1,prod))
    
    
    ann.surv <- data.frame(t(as.matrix(apply(surv.all,2,quantile,probs=c(0.05,0.25,0.5,0.75,0.95)))))
    colnames(ann.surv) <- paste("q.",c("05",25,50,75,95),sep="")
    ann.surv$Mean <- colMeans(surv.all)
    ann.surv$ocean.year <- 1:4


par(mar=c(3,5,0.5,0.5),mfrow=c(1,1))
x.lim <- c(1,4)
y.lim <- c(0,1)
AT <- c(1:4)

### MAKE PLOT OF STUFF
# 
plot(q.05~ocean.year, data=ann.surv, lwd=1,lty=2,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
par(new=T)
plot(q.25~ocean.year, data=ann.surv, lwd=3,lty=3,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
par(new=T)
plot(q.50~ocean.year, data=ann.surv, lwd=3,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
par(new=T)
plot(q.75~ocean.year, data=ann.surv, lwd=3,lty=3,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
par(new=T)
plot(q.95~ocean.year, data=ann.surv, lwd=1,lty=2,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")

axis(1,at=AT)
axis(2,las=2)
box(bty="l")

ann.surv
# title(ylab="Annual Survival")
# # 
#  abline(h=c(0.5,0.8,0.85,0.9),col=2,lty=3)
# # abline(v=c(2,3),col=2 )
# # abline(h=c(1),col=1 )




### Using cholesky decompose to impose this structure on the model

pred.val <- MU_m + chol(Sigma_m) %*% c(-17.2,6.3)
pred.log.M <- pred.val[1] + pred.val[2] * month
pred.M <- exp(pred.log.M)

pred.M.year <- rep(0,4)
pred.M.year[1] = pred.M[1:12] %>% sum()
pred.M.year[2] = pred.M[13:24] %>% sum()
pred.M.year[3] = pred.M[25:36] %>% sum()
pred.M.year[4] = pred.M[37:48] %>% sum()

pred.S.year <- exp(-pred.M.year)
pred.S.year

plot(pred.S.year,ylim=c(0.5,1))
par(new=T)
plot(exp(pred.log.M),ylim=c(0,0.2),col=2)


#   plot(Mean~ocean.year, data=ann.surv, lwd=4,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
#     par(new=T)


######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
# Priors for vulnerability of fish to different gears.
  ### Add in suceptibility to various gears as a logisitic

  # THIS IS THE MAPPING FOR MAKING VULNERABILITY DIFFERENT FOR THE WA coast and north and the OREGON, CALI coast
  #   vuln_index <- c(rep(1,7),rep(2,10))
  # 
  # library(MASS)
  # 
  # psi_0_fixed       <- 6
  # psi_troll_mu      <- 0.5
  # psi_troll_sig     <- 0.05
  # psi_rec_mu        <- 0.4
  # psi_rec_sig       <- 0.1
  # 
  # corr_troll_rec    <- 0
  # corr_troll_troll  <- 0.2
  # corr_rec_rec      <- 0.2  
  # #Make vectors and matrices
  # 
  # MU_psi    <- c(psi_troll_mu,psi_troll_mu  ,psi_rec_mu,psi_rec_mu )
  # Sigma_psi <- matrix(0,length(MU_psi),length(MU_psi))
  # 
  # Sigma_psi[,1] <- c( psi_troll_sig^2 , psi_troll_sig^2 * corr_troll_troll, psi_rec_sig * psi_troll_sig * corr_troll_rec, psi_rec_sig * psi_troll_sig * corr_troll_rec)
  # Sigma_psi[,2] <- c( psi_troll_sig^2* corr_troll_troll, psi_troll_sig^2,   psi_rec_sig * psi_troll_sig * corr_troll_rec, psi_rec_sig * psi_troll_sig * corr_troll_rec)
  # Sigma_psi[,3] <- c( psi_rec_sig * psi_troll_sig * corr_troll_rec, psi_rec_sig * psi_troll_sig * corr_troll_rec, psi_rec_sig^2, psi_rec_sig^2 * corr_rec_rec)
  # Sigma_psi[,4] <-c( psi_rec_sig * psi_troll_sig * corr_troll_rec, psi_rec_sig * psi_troll_sig * corr_troll_rec, psi_rec_sig^2* corr_rec_rec, psi_rec_sig^2)
  # 
  # Sigma_psi
  # MU_psi
  # 
  # param <- mvrnorm(1000,MU_psi,Sigma_psi)
  # 
  #   temp_troll <-  psi_0_fixed + param[,1] %*% t(mort_age) 
  #   temp_rec   <-  psi_0_fixed + param[,2] %*% t(mort_age)
  #   temp_troll  <- plogis(temp_troll)
  #   temp_rec   <- plogis(temp_rec)
  #   
  #   quant.troll <- data.frame(t(as.matrix(apply(temp_troll,2,quantile,probs=c(0.05,0.25,0.5,0.75,0.95)))))
  #   quant.rec <- data.frame(t(as.matrix(apply(temp_rec,2,quantile,probs=c(0.05,0.25,0.5,0.75,0.95)))))
  #   colnames(quant.troll) <- paste("q.",c("05",25,50,75,95),sep="")
  #   colnames(quant.rec)   <- colnames(quant.troll)
  #   quant.troll$Mean <- colMeans(temp_troll)
  #   quant.troll$ocean_age <- ocean_age
  #   quant.rec$Mean <- colMeans(temp_rec)
  #   quant.rec$ocean_age <- ocean_age
  # 
  #   par(mar=c(3,5,0.5,0.5),mfrow=c(1,1))
  #   x.lim <- c(1,max(ocean_age))
  #   y.lim <- c(0,1)
  # 
  #   plot(q.05~ocean_age, data=quant.troll, lwd=1,lty=2,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
  #   par(new=T)
  #   plot(q.25~ocean_age, data=quant.troll, lwd=3,lty=3,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
  #   par(new=T)
  #   plot(q.50~ocean_age, data=quant.troll, lwd=3,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
  #   par(new=T)
  #   plot(q.75~ocean_age, data=quant.troll, lwd=3,lty=3,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
  #   par(new=T)
  #   plot(q.95~ocean_age, data=quant.troll, lwd=1,lty=2,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l")
  #   
  #   par(new=T)
  #   plot(q.05~ocean_age, data=quant.rec, lwd=1,lty=2,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l",col=2)
  #   par(new=T)
  #   plot(q.25~ocean_age, data=quant.rec, lwd=3,lty=3,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l",col=2)
  #   par(new=T)
  #   plot(q.50~ocean_age, data=quant.rec, lwd=3,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l",col=2)
  #   par(new=T)
  #   plot(q.75~ocean_age, data=quant.rec, lwd=3,lty=3,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l",col=2)
  #   par(new=T)
  #   plot(q.95~ocean_age, data=quant.rec, lwd=1,lty=2,axes=F,xlim=x.lim,ylim=y.lim,xlab="",ylab="",type="l",col=2)
  #   
  #   axis(1)
  #   axis(2,las=2)
  #   box(bty="l")
  #   title(ylab="Vulnerability")
  #   
  #   legend(x=40,y=0.4,col=c(1,2),lwd=2,legend=c("Troll","Rec"))
  #   abline(h=c(0.5,0.8),col=2,lty=3)
  #   abline(v=c(2,3),col=2 )
  #   abline(h=c(1),col=1 )
  #   
    ######################################################################################################
    ######################################################################################################
    ######################################################################################################
    ######################################################################################################
    ######################################################################################################
    ######################################################################################################
    # Priors for FISHING MORTALITIES FOR REC AND NET FISHERIES
    ######################################################################################################
    ######################################################################################################
    ######################################################################################################
    ######################################################################################################
    ######################################################################################################
    ######################################################################################################
  
    # Make fishing mortality hierarchical, give variance parameters a gamma prior
      log_F_rec_sd_a <- 2
      log_F_rec_sd_b <- 10
    
      log_F_net_sd_a <- 5
      log_F_net_sd_b <- 10
      
      log_F_prior <- c(-6,2)
      log_q_prior <- c(-10,4) 
      
      ######################################################################################################
      ######################################################################################################
      ######################################################################################################
      ######################################################################################################
      # Priors for PROCESS ERROR
      ######################################################################################################
      ######################################################################################################
      ######################################################################################################
      # Gamma distribution params (alpha,beta)
      #zeta_sigma_prior <- c(3,60)
      
      
      