#############################################################################################################################
#############################################################################################################################
######## PLOT for PUB (Adult Mortality)
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################


dat.age <- data.frame(mod.cov = age_month_cal, mod.month = 1:length(age_month_cal))
dat.M2 <- data.frame(cbind(cum_M2,t(as.matrix(apply(samp$cum_M2,2,quantile,probs=c(0.05,0.25,0.75,0.95))))))
dat.M2 <- data.frame(dat.M2,dat.age)
dat.M2$cum_surv <- exp(-dat.M2$cum_M2)
dat.M2$cum_surv_05 <- exp(-dat.M2$X5.)
dat.M2$cum_surv_95 <- exp(-dat.M2$X95.)

# Simulate from prior

param_m <- mvrnorm(10000,PRIORS$MU_M2_prior,PRIORS$Sigma_M2_prior)

temp <-  exp(param_m[,1] + param_m[,2] %*% t(dat.M2$mod.cov))
temp.mort <- t(apply(temp,1,cumsum))
mort.prior <- data.frame(t(as.matrix(apply(temp.mort,2,quantile,probs=c(0.05,0.25,0.5,0.75,0.95)))))
colnames(mort.prior) <- paste("q.",c("05",25,50,75,95),sep="")
mort.prior$Mean <- colMeans(temp.mort)
mort.prior$mod.month <- dat.M2$mod.month

temp.surv <- exp(-temp.mort)
surv.prior <- data.frame(t(as.matrix(apply(temp.surv,2,quantile,probs=c(0.05,0.25,0.5,0.75,0.95)))))
colnames(surv.prior) <- paste("q.",c("05",25,50,75,95),sep="")
surv.prior$Mean <- colMeans(temp.surv)
surv.prior$mod.month <- dat.M2$mod.month







#############################
# Cumulative Mortality
y.lim=c(0,2)
par(mfcol=c(2,2))
plot(cum_M2~mod.month,data=dat.M2,type="l",lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)",ylim=y.lim)
par(new=T)
plot(X95.~mod.month,data=dat.M2,type="l",lty=2,lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)",ylim=y.lim)
par(new=T)
plot(X5.~mod.month,data=dat.M2,type="l",lty=2,lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)",ylim=y.lim)

# Add prior
par(new=T)
plot(Mean~mod.month,data=mort.prior,type="l",lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)",col=2,ylim=y.lim)
par(new=T)
plot(q.05~mod.month,data=mort.prior,type="l",lty=2,lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)",col=2,ylim=y.lim)
par(new=T)
plot(q.95~mod.month,data=mort.prior,type="l",lty=2,lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)",col=2,ylim=y.lim)
abline(v=c(12,24,36,48),lty=2)
abline(v=7,col=2)
title("Cumulative mortality \n (black is estimate,red is prior)")

# Cumulative Survival
y.lim=c(0,1)
plot(cum_surv~mod.month,data=dat.M2,type="l",lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)",ylim=y.lim)
par(new=T)
plot(cum_surv_95~mod.month,data=dat.M2,type="l",lty=2,lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)",ylim=y.lim)
par(new=T)
plot(cum_surv_05~mod.month,data=dat.M2,type="l",lty=2,lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)",ylim=y.lim)
abline(v=c(12,24,36,48),lty=2)
abline(v=7,col=2)
# Add prior
par(new=T)
plot(Mean~mod.month,data=surv.prior,type="l",lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)",col=2,ylim=y.lim)
par(new=T)
plot(q.05~mod.month,data=surv.prior,type="l",lty=2,lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)",col=2,ylim=y.lim)
par(new=T)
plot(q.95~mod.month,data=surv.prior,type="l",lty=2,lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)",col=2,ylim=y.lim)
abline(v=c(12,24,36,48),lty=2)
abline(v=7,col=2)
title("Cumulative mortality \n (black is estimate,red is prior)")

### ADD MORTALITY MONTHLY RATE HERE

### ADD SURVIVAL MONTHLY RATE HERE




year.mort <- data.frame(age=1:4)
year.mort$ann.surv[1] <- exp(-dat.M2$cum_M2[dat$age == 12])
year.mort$ann.surv[2] <- exp(-dat.M2$cum_M2[dat$age == 24] + dat.M2$cum_M2[dat$age == 12])
year.mort$ann.surv[3] <- exp(-dat.M2$cum_M2[dat$age == 36] + dat.M2$cum_M2[dat$age == 24])
year.mort$ann.surv[4] <- exp(-dat.M2$cum_M2[dat$age == 48] + dat.M2$cum_M2[dat$age == 36])

plot(ann.surv~age,data=year.mort,type="b",lwd=2,ylab="Annual Survival",xlab="Model Age (Year)")


