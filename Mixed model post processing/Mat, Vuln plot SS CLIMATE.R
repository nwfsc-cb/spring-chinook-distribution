library(gtools)

 # Vulnerability and Maturity plotting of Model output

#3 Maturity first

plot.name <- data.frame(loc.spawn.idx=unique(spawn_loc_plot$loc.spawn.idx))
plot.name$nom <- spawn_loc_plot$nom[match(plot.name$loc.spawn.idx,spawn_loc_plot$loc.spawn.idx)]

##
leave.ocean <- as.matrix(prop_D)

pdf(paste(base.dir,"/Salmon-Climate/Output plots/","Leave Ocean ",NAME,".pdf",sep=""),height = 7, width=7)

par(mfrow=c(3,3),mar=c(3,3,1,0))
y.lim <- c(0,0.75)
text.x <- 0.25
text.y <- y.lim[2]*0.95
X.LAB <- 2:6

for(j in 1:nrow(spawn_loc_plot)){
  print(j)
    prop.A <- rdirichlet(1000,t(E_alpha[spawn_loc_plot$loc.spawn.idx[j],]))
    base.prop <- data.frame(age=2:6,t(rbind( colMeans(prop.A), apply(prop.A,2,quantile,probs=c(0.025,0.25,0.75,0.975)))))
    colnames(base.prop) <- c("age","Mean","q.025","q.25","q.75","q.975")

    these <- which(REL$ocean.region==spawn_loc_plot$ocean.region[j]) 
    prop_D_temp <- prop_D[these,]
    par(mfrow=c(4,4),mar=c(3,4,1,0.5))
    for(i in 1:nrow(prop_D_temp)){
      prop.temp <- data.frame(prior=base.prop$Mean,est=prop_D_temp[i,])
      
      A <- barplot(as.matrix(t(prop.temp)),ylim=y.lim,axes=F,names.arg=rep("",nrow(prop.temp)),beside=T,col=c(grey(0.5),1))
      arrows(x0=A[1,],x1=A[1,],y0=base.prop$q.025,y1=base.prop$q.975,length=0)
      axis(1,at=colMeans(A),X.LAB,padj=-1,tcl=-0.2)
      axis(2,las=2,hadj=0.7,tcl=-0.2)
      text(x=text.x,y=text.y,"Grey is prior",pos=4)
      title(xlab="Age",plot.name$name[i],line=1.5) 
      title(ylab="Prop. leave ocean",line=2) 
      title(main=paste(REL$ocean.region[these[i]],REL$ID[these[i]],REL$brood_year[these[i]]),cex.main=0.9)
    }
}
dev.off()


### Make Publication plot for a select number of runs - 1 for each origin region.


quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/","PUB Leave Ocean ",NAME,".pdf",sep=""),height = 8, width=8,type="pdf",dpi=300)

par(mfrow=c(4,4),mar=c(3,3,1,0))
y.lim <- c(0,0.75)
text.x <- 0.25
text.y <- y.lim[2]*0.95
X.LAB <- 2:6

for(j in 1:nrow(spawn_loc_plot)){
  print(j)
  prop.A <- rdirichlet(1000,t(E_alpha[spawn_loc_plot$loc.spawn.idx[j],]))
  base.prop <- data.frame(age=2:6,t(rbind( colMeans(prop.A), apply(prop.A,2,quantile,probs=c(0.025,0.25,0.75,0.975)))))
  colnames(base.prop) <- c("age","Mean","q.025","q.25","q.75","q.975")
  
  these <- which(REL$ocean.region==spawn_loc_plot$ocean.region[j]) 
  prop_D_temp <- prop_D[these,]
  #for(i in 1:nrow(prop_D_temp)){
    
    i = 8

    prop.temp <- data.frame(prior=base.prop$Mean,est=prop_D_temp[i,])
    
    A <- barplot(as.matrix(t(prop.temp)),ylim=y.lim,axes=F,names.arg=rep("",nrow(prop.temp)),beside=T,col=c(grey(0.5),1))
    arrows(x0=A[1,],x1=A[1,],y0=base.prop$q.025,y1=base.prop$q.975,length=0)
    axis(1,at=colMeans(A),X.LAB,padj=-1,tcl=-0.2)
    axis(2,las=2,hadj=0.7,tcl=-0.2)
    #text(x=text.x,y=text.y,"Grey is prior",pos=4)
    title(xlab="Age",plot.name$name[i],line=1.5) 
    title(ylab="Proportion",line=2) 
    title(main=paste(REL$ocean.region[these[i]],REL$ID[these[i]],REL$brood_year[these[i]]),cex.main=0.9,adj=0)
  #}
}
dev.off()

########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################

## Vulnerability
### MAKE two kinds of plots - 
    #### One of vulnerability for a given size.
    #### Two of vulnerability in practice (actual vulnerability for a given area or gear type.)

SIZE <- c(0.16,0.2,0.24,0.26,0.28)
troll_vuln_plot <- NULL
rec_vuln_plot <- NULL

if(vuln_fixed>2){ # This uses the logistic formulation.
  for(i in 1:length(SIZE)){
    tvp <- 1 / (1 + exp(-(vuln_fixed + SIZE[i] * (beta_vuln[1] * vuln_age))))
    troll_vuln_plot <- cbind(troll_vuln_plot,tvp)
    rvp <- 1 / (1 + exp(-(vuln_fixed + SIZE[i] * (beta_vuln[2] * vuln_age))))
    rec_vuln_plot <- cbind(rec_vuln_plot,rvp)
  }
  
  trawl_vuln_plot <- 1 / (1 + exp(- (vuln_fixed + (beta_vuln_hake[1] * vuln_age_trawl + beta_vuln_hake[2] * vuln_age_trawl^2))))
}

if(vuln_fixed<2){ # C-log-log formulation
  for(i in 1:length(SIZE)){ 
    tvp <- 1 - exp(- exp(vuln_fixed + SIZE[i] * (beta_vuln[1] * vuln_age)))
    troll_vuln_plot <- cbind(troll_vuln_plot,tvp)
    rvp <- 1 - exp(- exp(vuln_fixed + SIZE[i] * (beta_vuln[2] * vuln_age)))
    rec_vuln_plot <- cbind(rec_vuln_plot,rvp)
  }
  #if(length(beta_vuln_hake)==1){
  trawl_vuln_plot <- 1 - exp(- exp(vuln_fixed + (beta_vuln_hake[1] * vuln_age_trawl) ))
  #}
}


NOM <- paste("size",SIZE*100,sep=".")
colnames(troll_vuln_plot) <- NOM
colnames(rec_vuln_plot)   <- NOM
troll_vuln_plot <- data.frame(age.month,vuln_age=vuln_age, troll_vuln_plot)
rec_vuln_plot   <- data.frame(age.month,vuln_age=vuln_age, rec_vuln_plot)
trawl_vuln_plot <- data.frame(age.month,vuln_age_trawl=vuln_age_trawl, trawl_vuln_plot)
##   
pdf(paste(base.dir,"/Salmon-Climate/Output plots/"," Vulnerability ",NAME,".pdf",sep=""),height = 3.5, width=7)

par(mfrow=c(1,2),mar=c(3,4,0.5,0.5))
y.lim <- c(0,1)
x.lim <- c(0,48)
COL <- c(1,2,3,4,5)

for(i in 1: length(NOM)){
  XX <- grep(NOM[i],colnames(troll_vuln_plot))
  if(i >1){par(new=T)}
    plot( troll_vuln_plot[,XX] ~troll_vuln_plot$age.month,type="b",
          ylim=y.lim,xlim=x.lim,axes=F,lwd=2,xlab="",ylab="",col=COL[i])
    par(new=T)
    plot( troll_vuln_plot[,XX] ~troll_vuln_plot$age.month,type="b",
          ylim=y.lim,xlim=x.lim,axes=F,lwd=2,xlab="",ylab="",col=COL[i])
}  
    par(new=T)
    plot( trawl_vuln_plot$trawl_vuln_plot ~ troll_vuln_plot$age.month,type="b",
          ylim=y.lim,xlim=x.lim,axes=F,lwd=2,xlab="",ylab="",col=1,pch=17)
  axis(1)
  axis(2,las=2)
  box(bty="o",lwd=2)
  title(xlab="Ocean age (months)",line=1.75)
  title(ylab="Vulnerability (Troll and Trawl)",line=2.5)
  legend(col=c(COL,1),x=30,y=0.5,legend=c(SIZE*100,"Trawl"),pch=c(rep(1,length(NOM)),17),bty="n",lwd=2,cex=0.8,title="Length(in)")

  
  for(i in 1: length(NOM)){
    XX <- grep(NOM[i],colnames(troll_vuln_plot))
    if(i >1){par(new=T)}
    plot( troll_vuln_plot[,XX] ~troll_vuln_plot$age.month,type="b",
          ylim=y.lim,xlim=x.lim,axes=F,lwd=2,xlab="",ylab="",col=COL[i])
    par(new=T)
    plot( troll_vuln_plot[,XX] ~troll_vuln_plot$age.month,type="b",
          ylim=y.lim,xlim=x.lim,axes=F,lwd=2,xlab="",ylab="",col=COL[i])
  }    
  axis(1)
  axis(2,las=2)
  box(bty="o",lwd=2)
  title(xlab="Ocean age (months)",line=1.75)
  title(ylab="Vulnerability (Recreational)",line=2.5)
  legend(col=COL,x=30,y=0.5,legend=SIZE*100,bty="n",lwd=2,cex=0.8,title="Length(in)")
  
dev.off()

