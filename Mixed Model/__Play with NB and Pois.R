library(ggplot2)
# desired CV and M and sampling fractions
CV = 0.3
M = 1000
prob <- 0.2


alp <- 1/(CV^2) 
bet <- alp / M


## Define Functions for Modified Neg Binom.
nb_func <- function(X,alpha,beta,prob){
  bp = beta*prob^(-1)
  
  term1 = lgamma(X+alpha) -lgamma(X+1) - lgamma(alpha)
  term2 = alpha * (log(bp) - log(bp + 1))
  term3 = X * -log(bp+1)
  
  return(exp(term1 + term2 +term3))
}

p_zero = function(alpha,beta,prob){
  bp = beta*prob^(-1)
  return((bp / (bp+1))^alpha)
}


X_start <- seq(0,M*4)
Y_samp <- nb_func(X=X_start,alpha=alp,beta=bet,prob=prob)
Y_real <- nb_func(X=X_start,alpha=alp,beta=bet,prob=1)

plot(y=Y_real,x=X_start,col="red",type="l", )
par(new=T)
plot(y=Y_real,x=X_start,col="red",type="l")
par(new=T)
plot(y=Y_real,x=X_start* 1/prob,xlim=c(0,max(X_start)),col="blue")

p_zero(alpha=alp,beta=bet,prob=prob)












M <- alp / bet
V <- alp / bet^2
SD <- sqrt(V)
CV <- SD / M

print(M)
print(CV)

N.val <- 1000000
lamb <- rgamma(N.val,alp,bet)

C <- rpois(N.val,lamb)


B <- rbinom(N.val,C,prob)

C.prime <- 1/prob * B

mean(C)
sd(C)

mean(C.prime)
sd(C.prime)

dat <- rbind(data.frame(id = "C",val=C),data.frame(id="C.prime",val=C.prime))

ggplot(dat) +
    geom_histogram(aes(fill=id,x=val),alpha=0.7) +
    scale_x_continuous(breaks=round(seq(0,M*5,length.out=20),1)) +
    theme_bw() +
    facet_wrap(~id)



hist(C)
hist(C.prime,breaks=1000)
