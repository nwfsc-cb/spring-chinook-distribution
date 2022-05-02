###### PLOT INITIAL MORTALITY

rel_yr <- data.frame(rel_id = 1:length(unique(dat.bin$release_year)),rel_year = sort(unique(dat.bin$release_year)))

nom.all <- sort(unique(dat.bin$year.reg))
THESE   <- match(nom.all,dat.bin$year.reg)
nom     <- data.frame( dat.bin[THESE,c("year.reg.idx","ocean.reg","release_year","loc.spawn.idx")],nom = nom.all)

nom$ocean.reg[nom$ocean.reg == "NWVI" | nom$ocean.reg == "SWVI"] <- "VI"
nom$ocean.reg[nom$ocean.reg == "SOR" | nom$ocean.reg == "COR" |nom$ocean.reg == "NOR"] <- "OR"
nom <- nom[order(nom$loc.spawn.idx),]

dat <- matrix(-99,length(unique(spawn_loc$loc.spawn.idx)),nrow(rel_yr))
colnames(dat) <- rel_yr$rel_year
rownames(dat) <- unique(nom$ocean.reg)
dat.mean <- dat
dat.sd   <- dat

for(i in 1:max(dat.bin$year.reg.idx)){
  X <- which(rownames(dat)==nom$ocean.reg[i]) 
  Y <- which(colnames(dat)==nom$release_year[i]) 
  dat.mean[X,Y] <- mean(samp$rel_year_all[,nom$year.reg.idx[i]])
  dat.sd[X,Y]   <- sd(samp$rel_year_all[,nom$year.reg.idx[i]])
}

###############################################################################
##### CONVERT TO PROPORTION SURVIVING FROM RELEASE UP TO WHEN THE MODEL STARTS.
###############################################################################

# Calculate number of outmigrant smolt from various regions....
  smolt.dat <- read.csv("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Total Smolt and Run Info/_smolt_migrants.csv")
  smolt.dat <- smolt.dat[smolt.dat$type=="independent",]
  smolt.dat$ocean.region <- as.character(smolt.dat$rel.region)
  smolt.dat$ocean.region[smolt.dat$ocean.region == "SOR" | smolt.dat$ocean.region == "COR" | smolt.dat$ocean.region == "NOR" ] <- "OR"
  smolt.dat$ocean.region[smolt.dat$ocean.region == "SWVI" | smolt.dat$ocean.region == "NWVI"] <- "VI"
  
  smolt.mod <- smolt.dat[smolt.dat$type == "independent",]
  smolt.mod$total.wild.hatch <- smolt.mod$total.releases.median * (1+smolt.mod$frac.wild.missing) 
  smolt.mod$finger <- smolt.mod$total.wild.hatch * (1 - smolt.mod$frac.yearling )
  smolt.mod$yearling <- smolt.mod$total.wild.hatch * smolt.mod$frac.yearling
  
  smolt.prod <- aggregate(smolt.mod[,c("finger","yearling")],
                          by=list(number=smolt.mod$location.number,region=smolt.mod$rel.region,
                                  n.mon.fing = smolt.mod$n.month.finger ,n.mon.year = smolt.mod$n.month.yearling 
                          ),sum)
  smolt.prod <- smolt.prod[order(smolt.prod$number),]
  
  dat.fing.year <- aggregate(smolt.dat[,c("n.month.finger","n.month.yearling")],by=list(ocean.region=smolt.dat$ocean.region),mean)

  temp <- REL[,c("n.month","ocean.region","release_year")]
  temp$ocean.reg <- temp$ocean.region
  temp$ocean.reg[temp$ocean.reg == "SOR" | temp$ocean.reg == "COR" | temp$ocean.reg == "NOR" ] <- "OR"
  temp$ocean.reg[temp$ocean.reg == "SWVI" | temp$ocean.reg == "NWVI"] <- "VI"
  mean.n.month  <- aggregate(temp$n.month,by=list(ocean.region=temp$ocean.reg,rel_year=temp$release_year),mean)
  colnames(mean.n.month)[3] <- "n.month"
  mean.n.month <- merge( mean.n.month,dat.fing.year)
  mean.n.month <- merge( mean.n.month,rel_yr)
  colnames(mean.n.month)[1:2] <- c("release_year","ocean.reg")
  mean.n.month <- merge(mean.n.month,nom)

for(i in  1: dim(samp$rel_year_all)[2]){
  vals <- sample(samp$rel_year_all[,mean.n.month$year.reg.idx[i]],1000)
  A    <-   exp(-mean.n.month$n.month.finger[i] * vals)
  mean.n.month$MEAN.fing[i] <- mean(A)
  mean.n.month$SD.fing[i]   <- sd(A)
  B    <-   exp(-mean.n.month$n.month.yearling[i] * vals)
  mean.n.month$MEAN.year[i] <- mean(B)
  mean.n.month$SD.year[i]   <- sd(B)
}

dat.mean.prop.fing <- dat
dat.sd.prop.fing   <- dat
dat.mean.prop.year <- dat
dat.sd.prop.year   <- dat

for(i in 1:max(dat.bin$year.reg.idx)){
  X <- which(rownames(dat)== mean.n.month$ocean.reg[i]) 
  Y <- which(colnames(dat)== mean.n.month$release_year[i]) 
  dat.mean.prop.fing[X,Y] <- mean.n.month$MEAN.fing[i]
  dat.sd.prop.fing[X,Y]   <- mean.n.month$SD.fing[i]
  dat.mean.prop.year[X,Y] <- mean.n.month$MEAN.year[i]
  dat.sd.prop.year[X,Y]   <- mean.n.month$SD.year[i]
}

###############################################################################
##### START PLOT
###############################################################################

pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/"," Juv mortality plots ",NAME,".pdf",sep=""),height = 6, width=8.5)

# plot mean mortality rate
  par(mar=c(5,5,1,1))
  z.lim	<- c(0,round(max(colMeans(samp$rel_year_all)),1))
  y.lab <- rownames(dat)
  x.lab	<- rel_yr$rel_year
  col.br <- colorRampPalette(c("blue",grey(0.9),"red"))
  par(mfrow=c(1,1),oma=c( 0,0,0,4) )
  image(z=t(dat.mean),y=1:nrow(dat.mean),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
        col=1,zlim=c(-100,-98))
  image(z=t(dat.mean),y=1:nrow(dat.mean),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim,add=T)
  box(bty="o",lwd=2)
  axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
  axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
  title(ylab="Origin",line=4)
  title(xlab="Release Year",line=3.5)
  title(main="Mean monthly mortality rate")
  par(mfrow=c(1,1),oma=c( 0,0,0,0) )
  ticks <- seq(min(z.lim),max(z.lim),length.out=6)
  image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
  
  # plot sd mortality rate
  par(mar=c(5,5,1,1))
  z.lim	<- c(0,max(dat.sd))
  y.lab <- rownames(dat)
  x.lab	<- rel_yr$rel_year
  col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  par(mfrow=c(1,1),oma=c( 0,0,0,4) )
  image(z=t(dat.sd),y=1:nrow(dat.mean),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
        col=1,zlim=c(-100,-98))
  image(z=t(dat.sd),y=1:nrow(dat.mean),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim,add=T)
  box(bty="o",lwd=2)
  axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
  axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
  title(ylab="Origin",line=4)
  title(xlab="Release Year",line=3.5)
  title(main="SD monthly mortality rate")
  par(mfrow=c(1,1),oma=c( 0,0,0,0) )
  ticks <- seq(min(z.lim),max(z.lim),length.out=6)
  image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
  
  #plot(dat.sd~dat.mean,xlim=c(0,max(dat.mean)),ylim=c(0,0.10))

  ###############################################################################
  ##### 
  ###############################################################################
  # plot mean mortality in proportion
  par(mar=c(5,5,1,1))
  z.lim	<- c(0,round(max(mean.n.month$MEAN.fing),1))
  y.lab <- rownames(dat)
  x.lab	<- rel_yr$rel_year
  col.br <- colorRampPalette(c("blue",grey(0.9),"red"))
  par(mfrow=c(1,1),oma=c( 0,0,0,4) )
  image(z=t(dat.mean.prop.fing),y=1:nrow(dat.mean.prop.fing),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
        col=1,zlim=c(-100,-98))
  image(z=t(dat.mean.prop.fing),y=1:nrow(dat.mean.prop.fing),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim,add=T)
  box(bty="o",lwd=2)
  axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
  axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
  title(ylab="Origin",line=4)
  title(xlab="Release Year",line=3.5)
  title(main="Proportion surviving to model start (fingerlings)")
  par(mfrow=c(1,1),oma=c( 0,0,0,0) )
  ticks <- seq(0,max(z.lim),length.out=6)
  image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
  
  # plot sd mortality rate
  par(mar=c(5,5,1,1))
  z.lim	<- c(0,max(dat.sd.prop.fing))
  y.lab <- rownames(dat)
  x.lab	<- rel_yr$rel_year
  col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  par(mfrow=c(1,1),oma=c( 0,0,0,4) )
  image(z=t(dat.sd.prop.fing),y=1:nrow(dat.mean.prop.fing),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
        col=1,zlim=c(-100,-98))
  image(z=t(dat.sd.prop.fing),y=1:nrow(dat.mean.prop.fing),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim,add=T)
  box(bty="o",lwd=2)
  axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
  axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
  title(ylab="Origin",line=4)
  title(xlab="Release Year",line=3.5)
  title(main="SD Proportion surviving to model start (fingerlings)")
  par(mfrow=c(1,1),oma=c( 0,0,0,0) )
  ticks <- seq(min(z.lim),max(z.lim),length.out=6)
  image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
  
  # plot mean mortality in proportion
  par(mar=c(5,5,1,1))
  z.lim	<- c(0,round(max(mean.n.month$MEAN.year),2))
  y.lab <- rownames(dat)
  x.lab	<- rel_yr$rel_year
  col.br <- colorRampPalette(c("blue",grey(0.9),"red"))
  par(mfrow=c(1,1),oma=c( 0,0,0,4) )
  image(z=t(dat.mean.prop.year),y=1:nrow(dat.mean.prop.year),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
        col=1,zlim=c(-100,-98))
  image(z=t(dat.mean.prop.year),y=1:nrow(dat.mean.prop.year),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim,add=T)
  box(bty="o",lwd=2)
  axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
  axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
  title(ylab="Origin",line=4)
  title(xlab="Release Year",line=3.5)
  title(main="Proportion surviving to model start (yearlings)")
  par(mfrow=c(1,1),oma=c( 0,0,0,0) )
  ticks <- seq(0,max(z.lim),length.out=6)
  image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
  
  # plot sd mortality rate
  par(mar=c(5,5,1,1))
  z.lim	<- c(0,max(dat.sd.prop.year))
  y.lab <- rownames(dat)
  x.lab	<- rel_yr$rel_year
  col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  par(mfrow=c(1,1),oma=c( 0,0,0,4) )
  image(z=t(dat.sd.prop.year),y=1:nrow(dat.mean.prop.year),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
        col=1,zlim=c(-100,-98))
  image(z=t(dat.sd.prop.year),y=1:nrow(dat.mean.prop.year),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim,add=T)
  box(bty="o",lwd=2)
  axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
  axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
  title(ylab="Origin",line=4)
  title(xlab="Release Year",line=3.5)
  title(main="SD Proportion surviving to model start (yearlings)")
  par(mfrow=c(1,1),oma=c( 0,0,0,0) )
  ticks <- seq(min(z.lim),max(z.lim),length.out=6)
  image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
  
  ############
  ### PLOT HIERARCHICAL juvenile mortality rates
  ############
  all.rates <- colMeans(samp$rel_year_all)
  x.lim <- c(0,0.6)
  XX   <- seq(0.01,0.7,length.out=1000)
  DENS <- dlnorm(XX,mean(samp$log_rel_year_mu),mean(samp$rel_year_sigma))
  
  hist(all.rates,breaks=seq(0,1,by=0.025),col=grey(0.5),xaxs="i",yaxs="i",axes=F,xlim=x.lim,xlab="",ylab="",main="")
  axis(1)
  axis(2,las=2)
  par(new=T)
  plot(DENS~XX,type="l",col=2,lwd=3,axes=F,xlim=x.lim,xlab="",ylab="",xaxs="i",yaxs="i")
  box(bty="o",lwd=2)
  title(ylab="Frequency")
  title(xlab="Monthly mortality rate")
  title(main="Among site variation and hierarchical, cross population estimate")

dev.off()


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
######## PLOT for PUB
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################


THESE.fing <- as.character(smolt.prod$region[which(smolt.prod$finger >0)])
THESE.fing[THESE.fing == "SWVI"] <- "VI"
THESE.fing[THESE.fing == "SOR" | THESE.fing == "COR" |THESE.fing == "NOR"] <- "OR"
THESE.fing <- unique(THESE.fing)

THESE.year <- as.character(smolt.prod$region[which(smolt.prod$yearling >0)])
THESE.year[THESE.year == "SWVI"] <- "VI"
THESE.year[THESE.year == "SOR" | THESE.year == "COR" | THESE.year == "NOR" ] <- "OR"
THESE.year <- unique(THESE.year)

# dat.mean.prop.fing
# dat.sd.prop.fing

#####################################################################
### FINGERLING
YEAR <- as.numeric(colnames(dat.mean.prop.fing))
PCH  <- c(21,22,23,24,25,21,22,23,24)
COL  <- c("red","red","black","blue","blue","black","darkorchid1","darkorchid1","black")
NUMB <- c(1,1,3,2,2,3,3,4,4)
y.lim=c(0,0.8)
x.lim=c(min(YEAR),max(YEAR))
CEX.PT <- 0.7
LWD=1.5

#### START PLOT
quartz(file=paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/Juv Mort Pub plot.jpeg",sep=""),type="jpeg",dpi=600,width=4,height=6)

par(mfrow=c(2,1),mar=c(2.5,2.5,0.75,0.5))
THESE.row <- match(THESE.fing,rownames(dat.mean.prop.fing))

for(i in 1:length(THESE.row)){
  temp <- data.frame(year=YEAR,prop=dat.mean.prop.fing[THESE.row[i],],sd=dat.sd.prop.fing[THESE.row[i],])
  plot(prop~year,data=temp[temp$prop>0,],type="b",ylim=y.lim,xlim=x.lim,
        pch=PCH[THESE.row[i]],bg=COL[THESE.row[i]],col=COL[THESE.row[i]],lwd=LWD,cex=CEX.PT,axes=F,xlab="",ylab="",yaxs="i")
  arrows(x0=temp$year,x1=temp$year,
            y0=temp$prop+temp$sd, y1=temp$prop-temp$sd,
            col=COL[THESE.row[i]],lwd=2,length=0)
  par(new=T)
}
par(new=F)
axis(1,tcl=-0.2,cex.axis=0.8,padj= -1.5)
axis(2,las=2,tcl=-0.2,hadj=0.35,cex.axis=0.8)
box(bty="o",lwd=2)
#title(xlab="Release year")
title(ylab="First year surivival",line=1.5)

LEG <- data.frame(loc=rownames(dat.mean.prop.fing),PCH=PCH,COL=as.character(COL),NUMB=NUMB)
LEG$COL <- as.character(LEG$COL)
LEG <- LEG[order(LEG$NUMB),]

LEG <- LEG[match(THESE.fing,LEG$loc),]
LEG <- LEG[is.na(LEG$PCH)==F,]
legend(x=1978,y=y.lim[2],legend=LEG$loc,pch=LEG$PCH,col=LEG$COL,pt.bg=LEG$COL,cex=CEX.PT,bty="n",lwd=2)
mtext("a) Fingerling",side=3,adj=0,cex=0.9)

#####################################################################
### YEARLING

THESE.row<- match(THESE.year,rownames(dat.mean.prop.year))
# THESE.row <- THESE.row[is.na(THESE.row)==F]

for(i in 1:length(THESE.row)){
  temp <- data.frame(year=YEAR,prop=dat.mean.prop.year[THESE.row[i],],sd=dat.sd.prop.year[THESE.row[i],])
  plot(prop~year,data=temp[temp$prop>0,],type="b",ylim=y.lim,xlim=x.lim,
       pch=PCH[THESE.row[i]],bg=COL[THESE.row[i]],col=COL[THESE.row[i]],lwd=LWD,cex=CEX.PT,axes=F,xlab="",ylab="",yaxs="i")
  arrows(x0=temp$year,x1=temp$year,
         y0=temp$prop+temp$sd, y1=temp$prop-temp$sd,
         col=COL[THESE.row[i]],lwd=2,length=0)
  par(new=T)
}
par(new=F)
axis(1,tcl=-0.2,cex.axis=0.8,padj= -1.5)
axis(2,las=2,tcl=-0.2,hadj=0.35,cex.axis=0.8)
box(bty="o",lwd=2)
title(xlab="Release year",line=1.5)
title(ylab="First year surivival",line=1.5)

LEG <- data.frame(loc=rownames(dat.mean.prop.year),PCH=PCH,COL=as.character(COL),NUMB=NUMB)
LEG$COL <- as.character(LEG$COL)
LEG <- LEG[order(LEG$NUMB),]

LEG <- LEG[match(THESE.year,LEG$loc),]
LEG <- LEG[is.na(LEG$PCH)==F,]
legend(x=1978,y=y.lim[2],legend=LEG$loc,pch=LEG$PCH,col=LEG$COL,pt.bg=LEG$COL,cex=CEX.PT,bty="n",lwd=2)
mtext("b) Yearling",side=3,adj=0,cex=0.9)

dev.off()

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
######## PLOT for PUB (Adult Mortality)
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
dat <- data.frame(cum_M2)
dat$cum_surv <- exp(-dat$cum_M2)

par(mfcol=c(2,2))
plot(cum_M2~age,data=dat,type="l",lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)")
abline(v=c(12,24,36,48),lty=2)
abline(v=7,col=2)
plot(cum_surv~age,data=dat,type="l",lwd=2,ylab="Cumulative Survival",xlab="Model Age (months)",ylim=c(0,1))
abline(v=c(12,24,36,48),lty=2)
abline(v=7,col=2)
plot(M2~age,data=dat,type="l",lwd=2,ylab="Instantaneous Mortality",xlab="Model Age (months)")
abline(v=c(12,24,36,48),lty=2)
abline(v=7,col=2)

  year.mort <- data.frame(age=1:4)
  year.mort$ann.surv[1] <- exp(-dat$cum_M2[dat$age == 12])
  year.mort$ann.surv[2] <- exp(-dat$cum_M2[dat$age == 24] + dat$cum_M2[dat$age == 12])
  year.mort$ann.surv[3] <- exp(-dat$cum_M2[dat$age == 36] + dat$cum_M2[dat$age == 24])
  year.mort$ann.surv[4] <- exp(-dat$cum_M2[dat$age == 48] + dat$cum_M2[dat$age == 36])

plot(ann.surv~age,data=year.mort,type="b",lwd=2,ylab="Annual Survival",xlab="Model Age (Year)")


