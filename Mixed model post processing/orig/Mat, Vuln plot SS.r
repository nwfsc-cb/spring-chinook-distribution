library(gtools)

 # Vulnerability and Maturity plotting of Model output

#3 Maturity first

plot.name <- data.frame(loc.spawn.idx=unique(spawn_loc_plot$loc.spawn.idx))
plot.name$nom <- spawn_loc_plot$nom[match(plot.name$loc.spawn.idx,spawn_loc_plot$loc.spawn.idx)]

##
leave.ocean <- as.matrix(prop_D)

pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/","Leave Ocean ",NAME,".pdf",sep=""),height = 7, width=7)

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


quartz(file=paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/","PUB Leave Ocean ",NAME,".pdf",sep=""),height = 9, width=7,type="pdf",dpi=300)

par(mfrow=c(4,3),mar=c(3,3,1,0))
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










## Vulnerability
vuln_age <- sort(unique(dat.bin$age.month))

pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/"," Vulnerability ",NAME,".pdf",sep=""),height = 3.5, width=7)

par(mfrow=c(1,2),mar=c(3,4,0.5,0.5))
y.lim <- c(0,1)
x.lim <- c(0,48)

plot( vuln_mat[1,]~vuln_age,type="b",ylim=y.lim,xlim=x.lim,axes=F,lwd=2,xlab="",ylab="")
par(new=T)
plot( vuln_mat[2,]~vuln_age,type="b",pch=22,lty=2,ylim=y.lim,xlim=x.lim,axes=F,lwd=2,xlab="",ylab="")
axis(1)
axis(2,las=2)
box(bty="o",lwd=2)
title(xlab="Ocean age (months)",line=1.75)
title(ylab="Vulnerability",line=2.5)
legend(lty=c(1,2),x=25,y=0.7,legend=c("Troll South","Troll North"),bty="n",lwd=2,cex=0.8)

plot( vuln_mat[3,]~vuln_age,type="b",ylim=y.lim,xlim=x.lim,axes=F,lwd=2,xlab="",ylab="")
par(new=T)
plot( vuln_mat[4,]~vuln_age,type="b",pch=22,lty=2,ylim=y.lim,xlim=x.lim,axes=F,lwd=2,xlab="",ylab="")
axis(1)
axis(2,las=2)
box(bty="o",lwd=2)
title(xlab="Ocean age (months)",line=1.75)
title(ylab="Vulnerability",line=2.5)
legend(lty=c(1,2),x=25,y=0.7,legend=c("Rec South","Rec North"),bty="n",lwd=2,cex=0.8)

dev.off()

