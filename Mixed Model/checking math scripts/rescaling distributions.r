library(dplyr)
library(ggplot2)

# This is for checking the re-parameterization of the salmon likelihoods circa 8-2019

alpha=20;beta=2;p=0.5
lambda <- rgamma(1e6,alpha,beta*(p^(-1)))
A <- rpois(length(lambda),lambda)

mean(A); alpha/(beta*p^(-1))

Var_A <- (alpha/(beta*p^(-1)))*(1+(1/(beta*p^(-1))))
var(A); Var_A

# check p(A=0)

p0 = ((beta*p^(-1)) / ((beta*p^(-1))+1))^(alpha) # non-logarithms
p0_2 =exp(alpha*(log((beta*p^(-1)))-log((beta*p^(-1))+1))) # logarithms

length(A[A==0])/length(A); p0; p0_2

# OK... so this works.
# Check calculations of expectations of zero-truncated negative binomial.

B<-A[A>0]

E_trunc <- alpha / ((beta*p^(-1)) * (1 - (1 + 1/(beta*p^(-1)))^(-alpha)))
mean(B); E_trunc

V_trunc <- (Var_A / (1-p0)) - p0 * E_trunc^2
var(B); V_trunc


C <- p^(-1) * B
E_expand = p^(-1) * E_trunc
mean(C); E_expand

V_expand = V_trunc * p^(-2)
var(C); V_expand

print(c("Base;","Mean = ",alpha/beta,"; Var = ",alpha/beta^2))


####### DEALING WITH INF and NA results in STAN
#  This is for the using the truncated negative binomial in the distribution of C


alpha_calc <- function(MEAN, VAR){ 
  return( MEAN^2 * VAR^(-1)) 
}
beta_calc <- function(MEAN, VAR, frac_samp){ 
  return(MEAN * VAR^(-1) * frac_samp)
}
Prob_0_Calc<- function(alpha, beta) { # This uses the beta from "beta_calc" and so includes sampling fraction
  return( exp(alpha * (log(beta)-log(beta+1))))  ;
}

# Positive part of the model. - re-expansion and approximation with a Gamma distribution.
# these equations use the alpha_calc and beta_calc functions from above in them.

E_trunc_pos <- function(alpha, beta){
  temp = exp(-alpha * log(1+beta^(-1))) ;
  Mean = exp(log(alpha)-log(beta)-log(1-temp)) ; 
  return(Mean) 
}

# remember that "beta" here always includes the sampling fraction value (from beta_calc) so don't need to explicitly include it.
V_trunc_pos <- function(alpha, beta, E_trunc_pos, Prob_0_Calc){
    Var = ((alpha / beta) * (1 + alpha / beta)) / (1-Prob_0_Calc) -
              (Prob_0_Calc) * E_trunc_pos^2 ;
    return(Var) 
}

#####################
# set up a grid of alpha an beta values to test across

MEAN <- sort(exp(c(-15:10,0.01,0.1,0.25,0.5,1,5)))
CV   <- c(0.01,0.10,0.25,0.5,1.0,1.5,2.0,5.0)

FRAC_SAMP <- c(0.01,0.05,0.20,0.50)
ALL <- NULL

for(i in 1: length(MEAN)){
  for(j in 1: length(FRAC_SAMP)){

    VAR <- (CV*MEAN[i])^2
    
    Alpha <- alpha_calc(MEAN[i], VAR)
    Beta  <- beta_calc(MEAN[i], VAR, FRAC_SAMP[j])
    
    Mean_NB <- Alpha / Beta
    Var_NB  <- (Alpha / Beta) * (1 + (Alpha / Beta))
    
    P_0 <- Prob_0_Calc(Alpha, Beta)
    
    E_pos <- E_trunc_pos(Alpha,Beta)
    Var_pos <- V_trunc_pos(Alpha,Beta,E_pos,P_0)
    
    out <- data.frame(mean_raw=MEAN[i],var_raw=VAR,cv_raw=CV,frac_samp=FRAC_SAMP[j],Mean_NB,Var_NB,Alpha,Beta,P_0,E_pos,Var_pos)
    
    ALL <- rbind(ALL,out)
    
  }
}

ALL$CV <- ALL$cv_raw
ALL$CV_NB <- sqrt(ALL$Var_NB)/ALL$Mean_NB
ALL$CV_pos <- sqrt(ALL$Var_pos) / ALL$E_pos

ALL <- ALL %>% mutate(term_1_pos =(Var_NB)/(1-P_0),term_2_pos = P_0*E_pos^2)

ALL <- ALL %>% mutate(E_pos_term = (1+Beta^(-1))^(-Alpha))
#### LOOK AT THESE SIMULATIONS





P1 <- ggplot(ALL %>% filter(frac_samp == 0.05)) +
      geom_point(aes(y=Var_pos,x=E_pos)) +
      scale_y_log10() +
      scale_x_log10()
P1


ggplot(ALL %>% filter(frac_samp == 0.05)) +
  geom_point(aes(y=E_pos,x=mean_raw)) +
#  geom_abline(intercept=0,slope=1) +
  scale_x_log10()  +
  scale_y_log10()


ggplot(ALL %>% filter(frac_samp == 0.05)) +
  geom_point(aes(y=P_0,x=log(mean_raw),color=log10(CV))) +
  scale_x_log10()





ggplot(ALL %>% filter(E_pos<2,CV>0.25,CV<=1.5)) +
  geom_point(aes(y=Var_pos,x=E_pos,color=log(mean_raw))) +
  geom_abline(intercept=0,slope=1) +
  scale_x_log10() +
  scale_y_log10()





ggplot(ALL %>% filter(log(mean_raw)<0)) +
  geom_point(aes(y=CV_NB,x=Mean_NB,color=log(mean_raw),shape=as.factor(frac_samp))) +
  geom_abline(intercept=1,slope=0) +
  scale_x_log10()


ggplot(ALL %>% filter(log(mean_raw)<0)) +
  geom_point(aes(y=P_0,x=(Var_NB),color=log(mean_raw),shape=as.factor(frac_samp))) +
  #geom_abline(intercept=1,slope=0) +
  scale_x_log10()


ggplot(ALL %>% filter(log(mean_raw)<0)) +
  geom_point(aes(y=(Var_NB),x=(Var_pos),color=log(mean_raw),shape=as.factor(frac_samp))) +
  geom_abline(intercept=0,slope=1) +
  scale_x_log10()+
  scale_y_log10()
  
ggplot(ALL %>% filter(log(mean_raw)< -2)) +
  geom_point(aes(x=(Var_NB),y=(Var_pos),color=log10(var_raw),shape=as.factor(frac_samp))) +
  geom_abline(intercept=0,slope=1) +
  scale_x_log10()+
  scale_y_log10() +
  facet_wrap(~mean_raw) +
  theme_bw( )





ggplot(ALL) +# %>% filter(log(mean_raw)< -2)) +
  geom_point(aes(y= term_1_pos ,x= term_2_pos ,color=log(mean_raw),shape=as.factor(frac_samp))) +
  geom_abline(intercept=0,slope=1) +
  scale_x_log10()+
  scale_y_log10() +
  facet_wrap(~var_raw) +
  theme_bw( )

#############
ggplot(ALL %>% filter((Var_pos)<0)) +
  geom_point(aes(y= term_1_pos ,x= term_2_pos ,color=log(mean_raw),shape=as.factor(frac_samp))) +
  geom_abline(intercept=0,slope=1) +
  scale_x_log10()+
  scale_y_log10() +
  facet_wrap(~var_raw) +
  theme_bw( )
#########

ggplot(ALL %>% filter(Alpha>1e-30)) +
  geom_point(aes(y= E_pos_term ,x= Alpha,color=log10(Beta))) +
  scale_x_log10()

##########################################################################
##########################################################################
##########################################################################
##########################################################################
### OK cases where things go haywire:
## Var_pos is negative:
  A <- ALL %>% filter(Var_pos<0)
  summary(A)
  
  ggplot(A) +
    geom_point(aes(y=Mean_NB,x=E_pos,color=log10(Var_NB))) +
    scale_x_log10() +
    scale_y_log10() 
  
  
  
# Var_pos is infinite (doesn't occur)
  B <- ALL %>% filter(is.infinite(Var_pos))
  #Occurs rarely when var_raw is large and mean raw is large and frac_samp is large

# E_pos is Inf  
  C <- ALL %>% filter(is.infinite(E_pos))
  # Always associated with P_0 = 1 and 
  # driven by E_pos_term == 1. which can happen when alpha is very small
  
  ############ THIS GETS RID OF ALL CASES WHERE E_pos is infinite.
  Q <- ALL %>% filter( Mean_NB > 1e-10)
  # When Trimming by Mean_NB, set Var_NB == Mean_NB
  
  ggplot(Q) +
    geom_point(aes(y= Var_pos, x= Var_NB ,color=log(mean_raw),shape=as.factor(frac_samp))) 
    #scale_x_log10() +
    #scale_y_log10() 
  ggplot(Q %>% filter(Var_pos<0)) +
    geom_point(aes(y= Var_NB, x= Mean_NB ,
                   color=log(mean_raw),shape=as.factor(frac_samp))) +
    geom_abline(intercept=0,slope=1) +
    scale_x_log10() +
    scale_y_log10()
  

  D <- ALL %>% filter(Var_pos>0,E_pos<10)
  
  ggplot(D) +
    geom_point(aes(y= CV_pos, x= E_pos ,color=CV,shape=as.factor(frac_samp))) +
    scale_x_log10()
  
  E <- ALL %>% filter(Var_pos<0,E_pos<10)
  
  ggplot(Q %>% filter(Var_pos>0)) +
    geom_point(aes(y= CV_NB, x= CV_pos  ,
                   color=as.factor(CV),shape=as.factor(frac_samp))) +
    #geom_abline(intercept=0,slope=1) +
    scale_x_log10() +
    scale_y_log10() + facet_wrap(~CV)
  
  ggplot(Q %>% filter(Var_pos>0)) +
    geom_point(aes(y= CV_NB, x= mean_raw  ,
                   color=as.factor(CV),shape=as.factor(frac_samp))) +
    #geom_abline(intercept=0,slope=1) +
    scale_x_log10() +
    scale_y_log10() +
    facet_wrap(~CV)
  
  
  ggplot(D %>% filter (Var_pos<0)) +
    geom_point(aes(y= term_1_pos ,x= term_2_pos ,color=log(mean_raw),shape=as.factor(frac_samp))) +
    geom_abline(intercept=0,slope=1) +
    scale_x_log10()+
    scale_y_log10() +
    facet_wrap(~var_raw) +
    theme_bw( )
  #mean_raw << var_raw ... CV is huge
    #
  
  # Alpha_NB is very small (<1e-20) and less than Beta_NB
  
  
  # occurs very commonly when CV of the raw is very large


