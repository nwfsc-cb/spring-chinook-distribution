
# Mortality that declines with ocean age
intercept <-24 # age in month at which the mortality rate is constant.
month <- 1:55
month[month>=25] <- 24

month <- month - intercept 

##################################################################
### her
##################################################################
m0_mu   <-log(-log(0.90)/12)
#m0_mu   <- log(-log(0.85)/12) # THIS IS THE REFERENCE for the earlier model (Shelton et al. 2019)
#m0_sig  <- 0.0002
m0_sig  <- 0.1
m1_mu   <- - 0.087  ## THIS SHOULD BE 0.08 for REFERENCE
m1_sig  <- 0.008
corr_m0_m1 <- -0.5


#Deterministic Monthly mortality and survival.
log.M.det <- m0_mu + m1_mu %*% t(month) 
M.det     <- exp(log.M.det)
S.det     <- exp(-M.det)

# Simulate to make stochastic.
MU_m    <- c(m0_mu,m1_mu)
Sigma_m <- matrix(c(m0_sig^2, m0_sig * m1_sig * corr_m0_m1, m0_sig * m1_sig * corr_m0_m1, m1_sig^2),2,2)

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
# title(ylab="Annual Survival")
# # 
#  abline(h=c(0.5,0.8,0.85,0.9),col=2,lty=3)
# # abline(v=c(2,3),col=2 )
# # abline(h=c(1),col=1 )

ann.surv

