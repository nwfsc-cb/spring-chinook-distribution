library(lme4)

###### PLOT INITIAL MORTALITY
rel_year_all <- apply(samp$rel_year_all,2,median)
rel_year_mean <- apply(samp$rel_year_all,2,median)
rel_year_quantiles <- apply(samp$rel_year_all,2,quantile,probs=c(0.025,0.05,0.10,0.25,0.5,0.75,0.9,0.95,0.975))
rel_year_sd  <- apply(samp$rel_year_all,2,sd)

A<-data.frame(REL[,1:6],n_month=REL$n.month,m.median=rel_year_all,m.mean = rel_year_all, m.sd=rel_year_sd, t(rel_year_quantiles))
#write.csv(file="M estimates for Will.csv", A %>% filter(ocean.region %in% c("SFB","NCA") ))
#### Across all releases

pdf(paste(base.dir,"/spring-chinook-distribution/Output plots/"," Juv mortality plots ",NAME,".pdf",sep=""),height = 6, width=8.5)

par(mfrow=c(2,2),mar=c(4,4,1,1))
BREAKS = 100
#
hist(rel_year_all,main="All Sites",xlab="First year mortality (M)",breaks=BREAKS)
XXX <- seq(0,10, by=0.01)
#ZZZ <- dnorm(XXX,exp(log_rel_year_mu),rel_year_sigma)
#YYY <- dlnorm(XXX,log_rel_year_mu,exp(rel_year_sigma))
x.lim= c(0,max(rel_year_all))
# par(new=T)
# plot(ZZZ~XXX,col=2,axes=F,xlim=x.lim,type="l")

log_F_rec_mean <- mean(samp$log_F_rec_mean)
F_rec_sigma   <- mean(samp$F_rec_sigma)
hist(exp(-rel_year_all),main="All Sites",xlab="First year survivorship (exp(-M))",breaks=BREAKS)
#par(new=T)
#plot(YYY~XXX,col=2,axes=F,xlim=x.lim,type="l",lwd=2,xlab="",ylab="")

###################################

plot(rel_year_all~REL$n.month,ylab="Median M",xlab="Juvenile period (months)")
plot(exp(-rel_year_all)~REL$n.month,ylab="Median survivorship",xlab="Juvenile period (months)")

### Make plot by release region & year
par(mfrow=c(2,2),mar=c(4,4,1,1))
x.lim <- c(0,max(rel_year_all))
y.lim = c(0,0.4) #c(0,exp(-min(rel_year_all))
BREAKS.M = seq(0,max(rel_year_all),length.out=20)
BREAKS.S = seq(0,1,length.out=50)
for(i in 1:nrow(spawn_loc)){
  par(mfrow=c(2,2),mar=c(4,4,1,1))
    hist(rel_year_all[REL$ocean.region==spawn_loc$ocean.region[i]],main=spawn_loc$ocean.region[i],
       xlab="First year mortality (M)",breaks=BREAKS.M,xlim=x.lim)
  hist(exp(-rel_year_all[REL$ocean.region==spawn_loc$ocean.region[i]]),main=spawn_loc$ocean.region[i],
       xlab="First year survivorship (S)",breaks=BREAKS.S,xlim=c(0,exp(-min(rel_year_all))))
  plot(x= REL$brood_year[REL$ocean.region==spawn_loc$ocean.region[i]],y=exp(-rel_year_all[REL$ocean.region==spawn_loc$ocean.region[i]]),  
          ylab="First year survivorship (S)",xlab="Brood year",ylim=y.lim),xlim=c(min(REL$brood_year),max(REL$brood_year)))

  plot(exp(-rel_year_all[REL$ocean.region==spawn_loc$ocean.region[i]])~REL$n.month[REL$ocean.region==spawn_loc$ocean.region[i]],
       ylab="Median survivorship",xlab="Juvenile period (months)", ylim=y.lim),xlim=c(min(REL$n.month),max(REL$n.month)))
  }

dev.off()


#############################################################################################################################
#############################################################################################################################
# Juve Mort 2020-06
#############################################################################################################################
#############################################################################################################################

# this is the distribution juvenile mortality of overplotted with the heirarchical mean and SD

x.point <- seq(0.01,8,length.out=1000)
y.point <- dlnorm(x.point,log_rel_year_mu,log_rel_year_sigma)
y.adj <- y.point * 300
BRKS <- seq(0,10,by=0.2)

P.juv <- ggplot() +
      geom_histogram(aes(rel_year_all),color="black",fill=grey(0.8),breaks=BRKS) +
      geom_line(aes(x=x.point,y=y.adj),color="red") +
      scale_x_continuous(limits=c(0,10),breaks=0:10,expand=c(0,0.1)) +
      xlab(expression(paste("Mortality (",phi,")"))) +
      ylab("Count") + 
      geom_vline(xintercept=exp(log_rel_year_mu),color="red",linetype="dashed" ) +
      theme_bw()

P.juv

quartz(file=paste(base.dir,"/spring-chinook-distribution/Output plots/Juv Mort Histogram ",NAME,".jpeg",sep=""),type="jpeg",dpi=600,width=4,height=3)
  print(P.juv)
dev.off()



#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
######## BELOW HERE IS A BUNCH OF JUNK FOR OLD PLOTS IN BASE R... THERE MAY BE SOMETHING OF VALUE, but I'm not sure.
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
# 
# PROBS=c(0.025,0.25,0.5,0.75,0.975)
# PROB.nom <- c("q.025","q.25","q.50","q.75","q.975")
# release.year <- min(REL$release_year): (min(REL$release_year) + N_years_release-1)
# all.j.mort <- NULL
# for(i in 1:nrow(spawn_loc)){
#   for( j in 1:N_years_release){
#     temp<- NULL
#     these <- which(REL$ocean.region==spawn_loc$ocean.region[i] & REL$release_year==release.year[j])
#     if(length(these) > 0 ){
#       jmort <- samp$rel_year_all[,these]
#       if(length(these)==1){
#           temp <- as.matrix(t(quantile(exp(-jmort),probs=PROBS)))
#       }
#       if(length(these)>1){
#           temp <- as.matrix(t(apply((exp(-jmort)),2,quantile,probs=PROBS)))
#       }
#       
#       colnames(temp) <- PROB.nom
#       temp <- data.frame(temp)
#       temp$ocean.region <- spawn_loc$ocean.region[i] 
#       temp$release.year <- release.year[j]
#       
#       all.j.mort <- rbind(all.j.mort,temp)
#     }
#   }
# }
# 
# ## modify identifiers for OREGON and Van Is.
# all.j.mort$loc <- as.character(all.j.mort$ocean.region)
# 
# all.j.mort$loc[all.j.mort$loc == "NOR"|all.j.mort$loc == "COR" |all.j.mort$loc == "SOR"] <- "OR"
# #all.j.mort$loc[all.j.mort$loc == "NOR"|all.j.mort$loc == "SOR"] <- "OR"
# all.j.mort$loc[all.j.mort$loc == "SWVI"] <- "VI"
# 
# temp.A <- aggregate(all.j.mort$q.50,by=list(loc=all.j.mort$loc,release.year=all.j.mort$release.year),mean); colnames(temp.A)[3] <- "Mean"
# temp.B <- aggregate(all.j.mort$q.50,by=list(loc=all.j.mort$loc,release.year=all.j.mort$release.year),length); colnames(temp.B)[3] <- "N.obs"
# temp.C <- aggregate(all.j.mort$q.50,by=list(loc=all.j.mort$loc,release.year=all.j.mort$release.year),sd); colnames(temp.C)[3] <- "SD"
# 
# J.MORT <- data.frame(temp.A,N.obs=temp.B$N.obs,SE=temp.C$SD/sqrt(temp.B$N.obs))
# 
# ## Trim data so that only times with at least 3 observations are plotted
# J.MORT.trim <- J.MORT[J.MORT$N.obs>=3,]
# 
# LOC <- unique(spawn_loc_plot$nom)
# COL  <- viridis(n = length(LOC),option = "plasma")
#   #c("red","red","black","blue","blue","blue","black","darkorchid1","darkorchid1","black")
# NUMB <- c(1,1,3,2,2,2,3,3,4,4,4)
# 
# y.lim=c(0,0.8)
# x.lim=c(min(REL$release_year)-1,max(REL$release_year))
# CEX.PT <- 0.6
# LWD=1
# PCH  <- rep(c(21,22,23,24,25),10)[1:length(LOC)]
# 
# if(length(LOC)<length(PCH)){
#     PCH<-PCH[1:length(LOC)]
#     COL<-COL[1:length(LOC)]
#     NUMB<-NUMB[1:length(LOC)]
# }
# 
# #### START PLOT
# quartz(file=paste(base.dir,"/spring-chinook-distribution/Output plots/Juv Mort Pub plot ",NAME,".jpeg",sep=""),type="jpeg",dpi=600,width=4,height=3)
# 
# par(mfrow=c(1,1),mar=c(2,2.5,0.75,0.5))
# JIT <- seq(-.025,0.25,length.out=length(LOC))
# rel.temp <- data.frame(release.year=release.year)
# 
# for(i in 1:length(LOC)){
#   J.MORT.trim$release.year.jit <- J.MORT.trim$release.year + JIT[i]
#   
#   temp.MORT <-merge(rel.temp,J.MORT.trim[J.MORT.trim$loc==LOC[i],],all=T)
#   plot(Mean~release.year.jit,data=temp.MORT,type="b",ylim=y.lim,xlim=x.lim,
#         pch=PCH[i],bg=COL[i],col=COL[i],lwd=LWD,cex=CEX.PT,axes=F,xlab="",ylab="",yaxs="i")
#   arrows(x0=temp.MORT$release.year.jit,
#          x1=temp.MORT$release.year.jit,
#          y0=temp.MORT$Mean+temp.MORT$SE, 
#             y1=temp.MORT$Mean-temp.MORT$SE,
#             col=COL[i],lwd=LWD,length=0)
#   par(new=T)
# }
# par(new=F)
# axis(1,tcl=-0.2,cex.axis=0.7,padj= -2)
# axis(2,las=2,tcl=-0.2,hadj=0.35,cex.axis=0.7)
# box(bty="o",lwd=2)
# #title(xlab="Release year")
# title(ylab="First year surivival",line=1.5,cex.lab=0.8)
# title(xlab="Release year",line=1,cex.lab=0.8)
# 
# LEG <- data.frame(loc=LOC,PCH=PCH,COL=as.character(COL),NUMB=NUMB)
# LEG$COL <- as.character(LEG$COL)
# LEG <- LEG[order(LEG$NUMB),]
# 
# 
# legend(x=1977,y=y.lim[2],legend=LEG$loc,pch=LEG$PCH,col=LEG$COL,pt.bg=LEG$COL,cex=CEX.PT,bty="n",lwd=LWD)
# #mtext("a) Fingerling",side=3,adj=0,cex=0.9)
# dev.off()
# 
# ##################### PLOT ALL OF THE  DATA, REGARDLESS OF PRECISION.
# 
# ## Trim data so that only times with at least 3 observations are plotted
# J.MORT.trim <- J.MORT[J.MORT$N.obs>=3,]
# 
# #LOC <- unique(spawn_loc_plot$nom)
# #COL  <- c("red","red","black","blue","blue","black","darkorchid1","darkorchid1","black")
# #NUMB <- c(1,1,3,2,2,3,3,4,4)
# y.lim=c(0,0.8)
# x.lim=c(min(REL$release_year)-1,max(REL$release_year))
# CEX.PT <- 0.6
# LWD=1
# #PCH  <- c(21,22,23,24,25,21,22,23,24)
# 
# #### START PLOT
# quartz(file=paste(base.dir,"/spring-chinook-distribution/Output plots/Juv Mort Pub plot ALL ",NAME,".jpeg",sep=""),type="jpeg",dpi=600,width=4,height=3)
# 
# par(mfrow=c(1,1),mar=c(2,2.5,0.75,0.5))
# JIT <- seq(-.025,0.25,length.out=length(LOC))
# rel.temp <- data.frame(release.year=release.year)
# 
# for(i in 1:length(LOC)){
#   J.MORT$release.year.jit <- J.MORT$release.year + JIT[i]
#   
#   temp.MORT <-merge(rel.temp,J.MORT[J.MORT$loc==LOC[i],],all=T)
#   plot(Mean~release.year.jit,data=temp.MORT,type="b",ylim=y.lim,xlim=x.lim,
#        pch=PCH[i],bg=COL[i],col=COL[i],lwd=LWD,cex=CEX.PT,axes=F,xlab="",ylab="",yaxs="i")
#   # arrows(x0=temp.MORT$release.year.jit,
#   #        x1=temp.MORT$release.year.jit,
#   #        y0=temp.MORT$Mean+temp.MORT$SE, 
#   #        y1=temp.MORT$Mean-temp.MORT$SE,
#   #        col=COL[i],lwd=LWD,length=0)
#   par(new=T)
# }
# par(new=F)
# axis(1,tcl=-0.2,cex.axis=0.7,padj= -2)
# axis(2,las=2,tcl=-0.2,hadj=0.35,cex.axis=0.7)
# box(bty="o",lwd=2)
# #title(xlab="Release year")
# title(ylab="First year surivival",line=1.5,cex.lab=0.8)
# title(xlab="Release year",line=1,cex.lab=0.8)
# 
# LEG <- data.frame(loc=LOC,PCH=PCH,COL=as.character(COL),NUMB=NUMB)
# LEG$COL <- as.character(LEG$COL)
# LEG <- LEG[order(LEG$NUMB),]
# 
# 
# legend(x=1977,y=y.lim[2],legend=LEG$loc,pch=LEG$PCH,col=LEG$COL,pt.bg=LEG$COL,cex=CEX.PT,bty="n",lwd=LWD)
# #mtext("a) Fingerling",side=3,adj=0,cex=0.9)
# dev.off()
# 
# #########################################################################################
# #########################################################################################
# #########################################################################################
# #########################################################################################
# #########################################################################################
# ##################### ALTERNATE ANALYSIS
# #########################################################################################
# #########################################################################################
# #########################################################################################
# #########################################################################################
# #########################################################################################
# 
# log_rel_year_all <- apply(log(samp$rel_year_all),2,median)
# 
# dat.juv <- data.frame(REL[,c("ID","ocean.region","brood_year","release_year","N.released","Median.month.release","loc.numb","n.month")],
#                 log_M=log_rel_year_all)
# 
# dat.juv$reg.mod <- dat.juv$ocean.region
# dat.juv$reg.mod[dat.juv$reg.mod == "SOR" | 
#                   dat.juv$reg.mod == "COR" | 
#                   dat.juv$reg.mod == "NOR" ] <- "OR"
# 
# dat.juv$reg.mod <- as.factor(dat.juv$reg.mod)
# dat.juv$year         <- as.factor(dat.juv$release_year)
# 
# ### Some linear models
#   # This is the one in the pub.
# # mod <- lmer(log_M ~ (1|year) + (n.month|ocean.region) + n.month ,data=dat.juv)
# # summary(mod)
# # 
# # 
# # mod1 <- lmer(log_M ~ (1|year) + (1|ocean.region) + n.month ,data=dat.juv)
# # summary(mod1)
# 
# 
# # Calculations for PHI in manuscript
# exp(c(min(log_rel_year_all),max(log_rel_year_all)))
# mean(exp(log_rel_year_all))
# 
# exp(-exp(c(min(log_rel_year_all),max(log_rel_year_all))))
# 
# 
# ### Calculating weighted mean and variance.
# 
# temp<- data.frame(group_by(dat.juv,reg.mod,release_year) %>% summarise(yr.mean=mean(log_M),yr.var=var(log_M),N=length(reg.mod),tot.release=sum(N.released)))
# 
# w.mean <- function(x,v){
#               x <- x[is.na(v)==F]
#               v <- v[is.na(v)==F]         
#               w <- v^(-1)
#               w.mean <- 1/sum(w) * sum(x * w)
#               return(w.mean)
# }
# w.var <- function(x,v){
#             x <- x[is.na(v)==F]
#             v <- v[is.na(v)==F]            
#             w <- v^(-1)
#             w.mean  <- w.mean(x,v)
#             w.var   <- sum(w*(x-w.mean)^2) / (sum(w)-((sum(w^2)/sum(w))))
#             return(w.var)
# }
# 
# 
# juv.dat.sum <- data.frame(group_by(temp,reg.mod) %>% 
#               summarise(w.mean=w.mean(yr.mean,yr.var),w.var=w.var(yr.mean,yr.var),n.year=length(yr.var[is.na(yr.var)==F]) ))
# juv.dat.sum$w.sd <- sqrt(juv.dat.sum$w.var)
# juv.dat.sum$w.se <- juv.dat.sum$w.sd / sqrt(juv.dat.sum$n.year)
# 
# juv.dat.sum$loc.numb <- match(juv.dat.sum$reg.mod,LOCATIONS$location.name)
# juv.dat.sum$loc.numb[juv.dat.sum$reg.mod =="OR"] <- 7
# juv.dat.sum$loc.numb[juv.dat.sum$reg.mod =="MCOL"] <- 8.25
# juv.dat.sum$loc.numb[juv.dat.sum$reg.mod =="UPCOL"] <- 8.5
# juv.dat.sum <- juv.dat.sum[order(juv.dat.sum$loc.numb),]
# juv.dat.sum$numb.plot <- 1:nrow(juv.dat.sum)
# 
# ################ PLOTS.  
# #V1 Separate Panels for mean and variability
# 
# par(mfrow=c(1,2),mar=c(4,3.5,0.5,0.5))
# plot(numb.plot~w.mean,data=juv.dat.sum,xlim=c(0,1.5),axes=F,xlab="",ylab="",bg=1,pch=21)
# arrows(x0=juv.dat.sum$w.mean + juv.dat.sum$w.se, x1=juv.dat.sum$w.mean - juv.dat.sum$w.se,
#        y0=juv.dat.sum$numb.plot, y1 = juv.dat.sum$numb.plot,length=0,lwd=1.5)
# 
# axis(2,las=2,at=1:nrow(juv.dat.sum),labels=juv.dat.sum$reg.mod)
# axis(1)
# box(bty="o",lwd=2)
# 
# plot(numb.plot~w.sd,data=juv.dat.sum,xlim=c(0,1.0),axes=F,xlab="",ylab="",bg=1,pch=21)
# axis(2,las=2,at=1:nrow(juv.dat.sum),labels=rep(" ",nrow(juv.dat.sum)))
# axis(1)
# box(bty="o",lwd=2)
# 
# #V2 Single Panel 
# quartz(file=paste(base.dir,"/spring-chinook-distribution/Output plots/Juv Mort weighted pub plot",NAME,".jpeg",sep=""),type="jpeg",dpi=600,width=4,height=4)
# 
#   par(mfrow=c(1,1),mar=c(3,3,0.5,0.5))
#   plot(numb.plot~w.mean,data=juv.dat.sum,xlim=c(-0.5,1.5),axes=F,xlab="",ylab="",bg=1,pch=21)
#   arrows(x0=juv.dat.sum$w.mean + juv.dat.sum$w.sd, x1=juv.dat.sum$w.mean - juv.dat.sum$w.sd,
#        y0=juv.dat.sum$numb.plot, y1 = juv.dat.sum$numb.plot,length=0,lwd=1.5)
#   axis(2,las=2,at=1:nrow(juv.dat.sum),labels=juv.dat.sum$reg.mod,cex.axis=0.8,tcl=-0.3,hadj=0.7)
#   axis(1,cex.axis=0.8,padj=-1,tcl=-0.3)
#   box(bty="o",lwd=2)
#   title(xlab=expression("log("*phi*")"),cex.lab=1.1,line=1.7)
#   abline(v=mean(juv.dat.sum$w.mean),lty=2,lwd=2)
#   
# dev.off()
# ####################################################################
# 
# 
# #####################################################################
# ### YEARLING
# # 
# # THESE.row<- match(THESE.year,rownames(dat.mean.prop.year))
# # # THESE.row <- THESE.row[is.na(THESE.row)==F]
# # 
# # for(i in 1:length(THESE.row)){
# #   temp <- data.frame(year=YEAR,prop=dat.mean.prop.year[THESE.row[i],],sd=dat.sd.prop.year[THESE.row[i],])
# #   plot(prop~year,data=temp[temp$prop>0,],type="b",ylim=y.lim,xlim=x.lim,
# #        pch=PCH[THESE.row[i]],bg=COL[THESE.row[i]],col=COL[THESE.row[i]],lwd=LWD,cex=CEX.PT,axes=F,xlab="",ylab="",yaxs="i")
# #   arrows(x0=temp$year,x1=temp$year,
# #          y0=temp$prop+temp$sd, y1=temp$prop-temp$sd,
# #          col=COL[THESE.row[i]],lwd=2,length=0)
# #   par(new=T)
# # }
# # par(new=F)
# # axis(1,tcl=-0.2,cex.axis=0.8,padj= -1.5)
# # axis(2,las=2,tcl=-0.2,hadj=0.35,cex.axis=0.8)
# # box(bty="o",lwd=2)
# # title(xlab="Release year",line=1.5)
# # title(ylab="First year surivival",line=1.5)
# # 
# # LEG <- data.frame(loc=rownames(dat.mean.prop.year),PCH=PCH,COL=as.character(COL),NUMB=NUMB)
# # LEG$COL <- as.character(LEG$COL)
# # LEG <- LEG[order(LEG$NUMB),]
# # 
# # LEG <- LEG[match(THESE.year,LEG$loc),]
# # LEG <- LEG[is.na(LEG$PCH)==F,]
# # legend(x=1978,y=y.lim[2],legend=LEG$loc,pch=LEG$PCH,col=LEG$COL,pt.bg=LEG$COL,cex=CEX.PT,bty="n",lwd=2)
# # mtext("b) Yearling",side=3,adj=0,cex=0.9)
# # 
# # dev.off()
# # 
# # ##########
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # YEAR <- as.numeric(colnames(dat.mean.prop.fing))
# # spawn_loc_plot$PCH  <- c(21,22,23,24,25,21,22,23,24)
# # COL  <- c("red","red","black","blue","blue","black","darkorchid1","darkorchid1","black")
# # NUMB <- c(1,1,3,2,2,3,3,4,4)
# # y.lim=c(0,0.8)
# # x.lim=c(min(YEAR),max(YEAR))
# # CEX.PT <- 0.7
# # LWD=1.5
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 
# # rel_yr <- data.frame(rel_id = 1:length(unique(dat.bin$release_year)),rel_year = sort(unique(dat.bin$release_year)))
# # 
# # nom.all <- sort(unique(dat.bin$year.reg))
# # THESE   <- match(nom.all,dat.bin$year.reg)
# # nom     <- data.frame( dat.bin[THESE,c("year.reg.idx","ocean.reg","release_year","loc.spawn.idx")],nom = nom.all)
# # 
# # nom$ocean.reg[nom$ocean.reg == "NWVI" | nom$ocean.reg == "SWVI"] <- "VI"
# # nom$ocean.reg[nom$ocean.reg == "SOR" | nom$ocean.reg == "COR" |nom$ocean.reg == "NOR"] <- "OR"
# # nom <- nom[order(nom$loc.spawn.idx),]
# # 
# # dat <- matrix(-99,length(unique(spawn_loc$loc.spawn.idx)),nrow(rel_yr))
# # colnames(dat) <- rel_yr$rel_year
# # rownames(dat) <- unique(nom$ocean.reg)
# # dat.mean <- dat
# # dat.sd   <- dat
# # 
# # for(i in 1:max(dat.bin$year.reg.idx)){
# #   X <- which(rownames(dat)==nom$ocean.reg[i]) 
# #   Y <- which(colnames(dat)==nom$release_year[i]) 
# #   dat.mean[X,Y] <- mean(samp$rel_year_all[,nom$year.reg.idx[i]])
# #   dat.sd[X,Y]   <- sd(samp$rel_year_all[,nom$year.reg.idx[i]])
# # }
# # 
# # ###############################################################################
# # ##### CONVERT TO PROPORTION SURVIVING FROM RELEASE UP TO WHEN THE MODEL STARTS.
# # ###############################################################################
# # 
# # # Calculate number of outmigrant smolt from various regions....
# #   smolt.dat <- read.csv("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Total Smolt and Run Info/_smolt_migrants.csv")
# #   smolt.dat <- smolt.dat[smolt.dat$type=="independent",]
# #   smolt.dat$ocean.region <- as.character(smolt.dat$rel.region)
# #   smolt.dat$ocean.region[smolt.dat$ocean.region == "SOR" | smolt.dat$ocean.region == "COR" | smolt.dat$ocean.region == "NOR" ] <- "OR"
# #   smolt.dat$ocean.region[smolt.dat$ocean.region == "SWVI" | smolt.dat$ocean.region == "NWVI"] <- "VI"
# #   
# #   smolt.mod <- smolt.dat[smolt.dat$type == "independent",]
# #   smolt.mod$total.wild.hatch <- smolt.mod$total.releases.median * (1+smolt.mod$frac.wild.missing) 
# #   smolt.mod$finger <- smolt.mod$total.wild.hatch * (1 - smolt.mod$frac.yearling )
# #   smolt.mod$yearling <- smolt.mod$total.wild.hatch * smolt.mod$frac.yearling
# #   
# #   smolt.prod <- aggregate(smolt.mod[,c("finger","yearling")],
# #                           by=list(number=smolt.mod$location.number,region=smolt.mod$rel.region,
# #                                   n.mon.fing = smolt.mod$n.month.finger ,n.mon.year = smolt.mod$n.month.yearling 
# #                           ),sum)
# #   smolt.prod <- smolt.prod[order(smolt.prod$number),]
# #   
# #   dat.fing.year <- aggregate(smolt.dat[,c("n.month.finger","n.month.yearling")],by=list(ocean.region=smolt.dat$ocean.region),mean)
# # 
# #   temp <- REL[,c("n.month","ocean.region","release_year")]
# #   temp$ocean.reg <- temp$ocean.region
# #   temp$ocean.reg[temp$ocean.reg == "SOR" | temp$ocean.reg == "COR" | temp$ocean.reg == "NOR" ] <- "OR"
# #   temp$ocean.reg[temp$ocean.reg == "SWVI" | temp$ocean.reg == "NWVI"] <- "VI"
# #   mean.n.month  <- aggregate(temp$n.month,by=list(ocean.region=temp$ocean.reg,rel_year=temp$release_year),mean)
# #   colnames(mean.n.month)[3] <- "n.month"
# #   mean.n.month <- merge( mean.n.month,dat.fing.year)
# #   mean.n.month <- merge( mean.n.month,rel_yr)
# #   colnames(mean.n.month)[1:2] <- c("release_year","ocean.reg")
# #   mean.n.month <- merge(mean.n.month,nom)
# # 
# # for(i in  1: dim(samp$rel_year_all)[2]){
# #   vals <- sample(samp$rel_year_all[,mean.n.month$year.reg.idx[i]],1000)
# #   A    <-   exp(-mean.n.month$n.month.finger[i] * vals)
# #   mean.n.month$MEAN.fing[i] <- mean(A)
# #   mean.n.month$SD.fing[i]   <- sd(A)
# #   B    <-   exp(-mean.n.month$n.month.yearling[i] * vals)
# #   mean.n.month$MEAN.year[i] <- mean(B)
# #   mean.n.month$SD.year[i]   <- sd(B)
# # }
# # 
# # dat.mean.prop.fing <- dat
# # dat.sd.prop.fing   <- dat
# # dat.mean.prop.year <- dat
# # dat.sd.prop.year   <- dat
# # 
# # for(i in 1:max(dat.bin$year.reg.idx)){
# #   X <- which(rownames(dat)== mean.n.month$ocean.reg[i]) 
# #   Y <- which(colnames(dat)== mean.n.month$release_year[i]) 
# #   dat.mean.prop.fing[X,Y] <- mean.n.month$MEAN.fing[i]
# #   dat.sd.prop.fing[X,Y]   <- mean.n.month$SD.fing[i]
# #   dat.mean.prop.year[X,Y] <- mean.n.month$MEAN.year[i]
# #   dat.sd.prop.year[X,Y]   <- mean.n.month$SD.year[i]
# # }
# # 
# # ###############################################################################
# # ##### START PLOT
# # ###############################################################################
# # 
# # pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/"," Juv mortality plots ",NAME,".pdf",sep=""),height = 6, width=8.5)
# # 
# # # plot mean mortality rate
# #   par(mar=c(5,5,1,1))
# #   z.lim	<- c(0,round(max(colMeans(samp$rel_year_all)),1))
# #   y.lab <- rownames(dat)
# #   x.lab	<- rel_yr$rel_year
# #   col.br <- colorRampPalette(c("blue",grey(0.9),"red"))
# #   par(mfrow=c(1,1),oma=c( 0,0,0,4) )
# #   image(z=t(dat.mean),y=1:nrow(dat.mean),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
# #         col=1,zlim=c(-100,-98))
# #   image(z=t(dat.mean),y=1:nrow(dat.mean),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
# #         col=col.br(32),zlim=z.lim,add=T)
# #   box(bty="o",lwd=2)
# #   axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
# #   axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
# #   title(ylab="Origin",line=4)
# #   title(xlab="Release Year",line=3.5)
# #   title(main="Mean monthly mortality rate")
# #   par(mfrow=c(1,1),oma=c( 0,0,0,0) )
# #   ticks <- seq(min(z.lim),max(z.lim),length.out=6)
# #   image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
# #   
# #   # plot sd mortality rate
# #   par(mar=c(5,5,1,1))
# #   z.lim	<- c(0,max(dat.sd))
# #   y.lab <- rownames(dat)
# #   x.lab	<- rel_yr$rel_year
# #   col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
# #   par(mfrow=c(1,1),oma=c( 0,0,0,4) )
# #   image(z=t(dat.sd),y=1:nrow(dat.mean),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
# #         col=1,zlim=c(-100,-98))
# #   image(z=t(dat.sd),y=1:nrow(dat.mean),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
# #         col=col.br(32),zlim=z.lim,add=T)
# #   box(bty="o",lwd=2)
# #   axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
# #   axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
# #   title(ylab="Origin",line=4)
# #   title(xlab="Release Year",line=3.5)
# #   title(main="SD monthly mortality rate")
# #   par(mfrow=c(1,1),oma=c( 0,0,0,0) )
# #   ticks <- seq(min(z.lim),max(z.lim),length.out=6)
# #   image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
# #   
# #   #plot(dat.sd~dat.mean,xlim=c(0,max(dat.mean)),ylim=c(0,0.10))
# # 
# #   ###############################################################################
# #   ##### 
# #   ###############################################################################
# #   # plot mean mortality in proportion
# #   par(mar=c(5,5,1,1))
# #   z.lim	<- c(0,round(max(mean.n.month$MEAN.fing),1))
# #   y.lab <- rownames(dat)
# #   x.lab	<- rel_yr$rel_year
# #   col.br <- colorRampPalette(c("blue",grey(0.9),"red"))
# #   par(mfrow=c(1,1),oma=c( 0,0,0,4) )
# #   image(z=t(dat.mean.prop.fing),y=1:nrow(dat.mean.prop.fing),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
# #         col=1,zlim=c(-100,-98))
# #   image(z=t(dat.mean.prop.fing),y=1:nrow(dat.mean.prop.fing),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
# #         col=col.br(32),zlim=z.lim,add=T)
# #   box(bty="o",lwd=2)
# #   axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
# #   axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
# #   title(ylab="Origin",line=4)
# #   title(xlab="Release Year",line=3.5)
# #   title(main="Proportion surviving to model start (fingerlings)")
# #   par(mfrow=c(1,1),oma=c( 0,0,0,0) )
# #   ticks <- seq(0,max(z.lim),length.out=6)
# #   image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
# #   
# #   # plot sd mortality rate
# #   par(mar=c(5,5,1,1))
# #   z.lim	<- c(0,max(dat.sd.prop.fing))
# #   y.lab <- rownames(dat)
# #   x.lab	<- rel_yr$rel_year
# #   col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
# #   par(mfrow=c(1,1),oma=c( 0,0,0,4) )
# #   image(z=t(dat.sd.prop.fing),y=1:nrow(dat.mean.prop.fing),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
# #         col=1,zlim=c(-100,-98))
# #   image(z=t(dat.sd.prop.fing),y=1:nrow(dat.mean.prop.fing),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
# #         col=col.br(32),zlim=z.lim,add=T)
# #   box(bty="o",lwd=2)
# #   axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
# #   axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
# #   title(ylab="Origin",line=4)
# #   title(xlab="Release Year",line=3.5)
# #   title(main="SD Proportion surviving to model start (fingerlings)")
# #   par(mfrow=c(1,1),oma=c( 0,0,0,0) )
# #   ticks <- seq(min(z.lim),max(z.lim),length.out=6)
# #   image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
# #   
# #   # plot mean mortality in proportion
# #   par(mar=c(5,5,1,1))
# #   z.lim	<- c(0,round(max(mean.n.month$MEAN.year),2))
# #   y.lab <- rownames(dat)
# #   x.lab	<- rel_yr$rel_year
# #   col.br <- colorRampPalette(c("blue",grey(0.9),"red"))
# #   par(mfrow=c(1,1),oma=c( 0,0,0,4) )
# #   image(z=t(dat.mean.prop.year),y=1:nrow(dat.mean.prop.year),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
# #         col=1,zlim=c(-100,-98))
# #   image(z=t(dat.mean.prop.year),y=1:nrow(dat.mean.prop.year),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
# #         col=col.br(32),zlim=z.lim,add=T)
# #   box(bty="o",lwd=2)
# #   axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
# #   axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
# #   title(ylab="Origin",line=4)
# #   title(xlab="Release Year",line=3.5)
# #   title(main="Proportion surviving to model start (yearlings)")
# #   par(mfrow=c(1,1),oma=c( 0,0,0,0) )
# #   ticks <- seq(0,max(z.lim),length.out=6)
# #   image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
# #   
# #   # plot sd mortality rate
# #   par(mar=c(5,5,1,1))
# #   z.lim	<- c(0,max(dat.sd.prop.year))
# #   y.lab <- rownames(dat)
# #   x.lab	<- rel_yr$rel_year
# #   col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
# #   par(mfrow=c(1,1),oma=c( 0,0,0,4) )
# #   image(z=t(dat.sd.prop.year),y=1:nrow(dat.mean.prop.year),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
# #         col=1,zlim=c(-100,-98))
# #   image(z=t(dat.sd.prop.year),y=1:nrow(dat.mean.prop.year),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
# #         col=col.br(32),zlim=z.lim,add=T)
# #   box(bty="o",lwd=2)
# #   axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
# #   axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
# #   title(ylab="Origin",line=4)
# #   title(xlab="Release Year",line=3.5)
# #   title(main="SD Proportion surviving to model start (yearlings)")
# #   par(mfrow=c(1,1),oma=c( 0,0,0,0) )
# #   ticks <- seq(min(z.lim),max(z.lim),length.out=6)
# #   image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
# #   
# #   ############
# #   ### PLOT HIERARCHICAL juvenile mortality rates
# #   ############
# #   all.rates <- colMeans(samp$rel_year_all)
# #   x.lim <- c(0,0.6)
# #   XX   <- seq(0.01,0.7,length.out=1000)
# #   DENS <- dlnorm(XX,mean(samp$log_rel_year_mu),mean(samp$rel_year_sigma))
# #   
# #   hist(all.rates,breaks=seq(0,1,by=0.025),col=grey(0.5),xaxs="i",yaxs="i",axes=F,xlim=x.lim,xlab="",ylab="",main="")
# #   axis(1)
# #   axis(2,las=2)
# #   par(new=T)
# #   plot(DENS~XX,type="l",col=2,lwd=3,axes=F,xlim=x.lim,xlab="",ylab="",xaxs="i",yaxs="i")
# #   box(bty="o",lwd=2)
# #   title(ylab="Frequency")
# #   title(xlab="Monthly mortality rate")
# #   title(main="Among site variation and hierarchical, cross population estimate")
# # 
# # dev.off()
# # 
# # 
# # #############################################################################################################################
# # #############################################################################################################################
# # #############################################################################################################################
# # ######## PLOT for PUB
# # #############################################################################################################################
# # #############################################################################################################################
# # #############################################################################################################################
# # 
# # 
# # THESE.fing <- as.character(smolt.prod$region[which(smolt.prod$finger >0)])
# # THESE.fing[THESE.fing == "SWVI"] <- "VI"
# # THESE.fing[THESE.fing == "SOR" | THESE.fing == "COR" |THESE.fing == "NOR"] <- "OR"
# # THESE.fing <- unique(THESE.fing)
# # 
# # THESE.year <- as.character(smolt.prod$region[which(smolt.prod$yearling >0)])
# # THESE.year[THESE.year == "SWVI"] <- "VI"
# # THESE.year[THESE.year == "SOR" | THESE.year == "COR" | THESE.year == "NOR" ] <- "OR"
# # THESE.year <- unique(THESE.year)
# # 
# # # dat.mean.prop.fing
# # # dat.sd.prop.fing
# # 
# # #####################################################################
# # ### FINGERLING
# # YEAR <- as.numeric(colnames(dat.mean.prop.fing))
# # PCH  <- c(21,22,23,24,25,21,22,23,24)
# # COL  <- c("red","red","black","blue","blue","black","darkorchid1","darkorchid1","black")
# # NUMB <- c(1,1,3,2,2,3,3,4,4)
# # y.lim=c(0,0.8)
# # x.lim=c(min(YEAR),max(YEAR))
# # CEX.PT <- 0.7
# # LWD=1.5
# # 
# # #### START PLOT
# # quartz(file=paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/Juv Mort Pub plot.jpeg",sep=""),type="jpeg",dpi=600,width=4,height=6)
# # 
# # par(mfrow=c(2,1),mar=c(2.5,2.5,0.75,0.5))
# # THESE.row <- match(THESE.fing,rownames(dat.mean.prop.fing))
# # 
# # for(i in 1:length(THESE.row)){
# #   temp <- data.frame(year=YEAR,prop=dat.mean.prop.fing[THESE.row[i],],sd=dat.sd.prop.fing[THESE.row[i],])
# #   plot(prop~year,data=temp[temp$prop>0,],type="b",ylim=y.lim,xlim=x.lim,
# #         pch=PCH[THESE.row[i]],bg=COL[THESE.row[i]],col=COL[THESE.row[i]],lwd=LWD,cex=CEX.PT,axes=F,xlab="",ylab="",yaxs="i")
# #   arrows(x0=temp$year,x1=temp$year,
# #             y0=temp$prop+temp$sd, y1=temp$prop-temp$sd,
# #             col=COL[THESE.row[i]],lwd=2,length=0)
# #   par(new=T)
# # }
# # par(new=F)
# # axis(1,tcl=-0.2,cex.axis=0.8,padj= -1.5)
# # axis(2,las=2,tcl=-0.2,hadj=0.35,cex.axis=0.8)
# # box(bty="o",lwd=2)
# # #title(xlab="Release year")
# # title(ylab="First year surivival",line=1.5)
# # 
# # LEG <- data.frame(loc=rownames(dat.mean.prop.fing),PCH=PCH,COL=as.character(COL),NUMB=NUMB)
# # LEG$COL <- as.character(LEG$COL)
# # LEG <- LEG[order(LEG$NUMB),]
# # 
# # LEG <- LEG[match(THESE.fing,LEG$loc),]
# # LEG <- LEG[is.na(LEG$PCH)==F,]
# # legend(x=1978,y=y.lim[2],legend=LEG$loc,pch=LEG$PCH,col=LEG$COL,pt.bg=LEG$COL,cex=CEX.PT,bty="n",lwd=2)
# # mtext("a) Fingerling",side=3,adj=0,cex=0.9)
# # 
# # #####################################################################
# # ### YEARLING
# # 
# # THESE.row<- match(THESE.year,rownames(dat.mean.prop.year))
# # # THESE.row <- THESE.row[is.na(THESE.row)==F]
# # 
# # for(i in 1:length(THESE.row)){
# #   temp <- data.frame(year=YEAR,prop=dat.mean.prop.year[THESE.row[i],],sd=dat.sd.prop.year[THESE.row[i],])
# #   plot(prop~year,data=temp[temp$prop>0,],type="b",ylim=y.lim,xlim=x.lim,
# #        pch=PCH[THESE.row[i]],bg=COL[THESE.row[i]],col=COL[THESE.row[i]],lwd=LWD,cex=CEX.PT,axes=F,xlab="",ylab="",yaxs="i")
# #   arrows(x0=temp$year,x1=temp$year,
# #          y0=temp$prop+temp$sd, y1=temp$prop-temp$sd,
# #          col=COL[THESE.row[i]],lwd=2,length=0)
# #   par(new=T)
# # }
# # par(new=F)
# # axis(1,tcl=-0.2,cex.axis=0.8,padj= -1.5)
# # axis(2,las=2,tcl=-0.2,hadj=0.35,cex.axis=0.8)
# # box(bty="o",lwd=2)
# # title(xlab="Release year",line=1.5)
# # title(ylab="First year surivival",line=1.5)
# # 
# # LEG <- data.frame(loc=rownames(dat.mean.prop.year),PCH=PCH,COL=as.character(COL),NUMB=NUMB)
# # LEG$COL <- as.character(LEG$COL)
# # LEG <- LEG[order(LEG$NUMB),]
# # 
# # LEG <- LEG[match(THESE.year,LEG$loc),]
# # LEG <- LEG[is.na(LEG$PCH)==F,]
# # legend(x=1978,y=y.lim[2],legend=LEG$loc,pch=LEG$PCH,col=LEG$COL,pt.bg=LEG$COL,cex=CEX.PT,bty="n",lwd=2)
# # mtext("b) Yearling",side=3,adj=0,cex=0.9)
# # 
# # dev.off()
# # 
# # #############################################################################################################################
# # #############################################################################################################################
# # #############################################################################################################################
# # ######## PLOT for PUB (Adult Mortality)
# # #############################################################################################################################
# # #############################################################################################################################
# # #############################################################################################################################
# # dat <- data.frame(cum_M2)
# # dat$cum_surv <- exp(-dat$cum_M2)
# # 
# # par(mfcol=c(2,2))
# # plot(cum_M2~age,data=dat,type="l",lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)")
# # abline(v=c(12,24,36,48),lty=2)
# # abline(v=7,col=2)
# # plot(cum_surv~age,data=dat,type="l",lwd=2,ylab="Cumulative Survival",xlab="Model Age (months)",ylim=c(0,1))
# # abline(v=c(12,24,36,48),lty=2)
# # abline(v=7,col=2)
# # plot(M2~age,data=dat,type="l",lwd=2,ylab="Instantaneous Mortality",xlab="Model Age (months)")
# # abline(v=c(12,24,36,48),lty=2)
# # abline(v=7,col=2)
# # 
# #   year.mort <- data.frame(age=1:4)
# #   year.mort$ann.surv[1] <- exp(-dat$cum_M2[dat$age == 12])
# #   year.mort$ann.surv[2] <- exp(-dat$cum_M2[dat$age == 24] + dat$cum_M2[dat$age == 12])
# #   year.mort$ann.surv[3] <- exp(-dat$cum_M2[dat$age == 36] + dat$cum_M2[dat$age == 24])
# #   year.mort$ann.surv[4] <- exp(-dat$cum_M2[dat$age == 48] + dat$cum_M2[dat$age == 36])
# # 
# # plot(ann.surv~age,data=year.mort,type="b",lwd=2,ylab="Annual Survival",xlab="Model Age (Year)")
# # 
# # 
