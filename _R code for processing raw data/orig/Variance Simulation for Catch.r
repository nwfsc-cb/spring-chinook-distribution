# Little simulation to check variance of catch


N			<-	10000
TOT.CATCH	<- 	1000
prop.A		<-	0.001
frac.samp	<-	0.3


Y 		<-	rbinom(N,TOT.CATCH*frac.samp,prop.A)
p.est	<-	Y / (TOT.CATCH*frac.samp)

Y.expand	<-	Y*(1/frac.samp)

mean(Y.expand)
var(Y.expand)

calc.var	<- function(Ya,p.hat,frac){
				return(((1/frac)^2)*(Ya-p.hat*Ya+p.hat^2-p.hat))
				}
calc.var.simp	<- function(Ya,p.hat,frac){
				return(((1/frac)^2)*(Ya))
				}

				
Y.var.est	<-	calc.var(Y,p.est,frac.samp)
Y.var.simp	<-	calc.var.simp(Y,p.est,frac.samp)


mean(sd(Y.expand))
mean(sqrt(Y.var.est))
mean(sqrt(Y.var.simp))
hist(sqrt(Y.var.est))