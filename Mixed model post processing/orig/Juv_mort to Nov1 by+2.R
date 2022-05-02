library(lme4)

###### PLOT INITIAL MORTALITY
rel_year_all <- apply(samp$rel_year_all,2,median)
rel_year_sd  <- apply(samp$rel_year_all,2,sd)
#### Across all releases


# Need three components:
  # rel_year_all
  # M assumed
  # process error (epsilon)

# sample 1000 pulls from the MCMC

THESE <- seq(1,length(samp$sigma_pos),length.out=1000)

phi.samp <- -samp$rel_year_all[THESE,]
epsilon.samp <- samp$epsilon[THESE,,1:2]
epsilon.samp <- apply(epsilon.samp,c(1,2),sum) 
M.samp       <- -apply(samp$cum_M2_temp[THESE,1:3],1,sum)

dim(phi.samp)
dim(epsilon.samp)

mort.by2 <- phi.samp + epsilon.samp + M.samp
surv.by2 <- exp(mort.by2)

all.j.mort <- data.frame(ID=REL$ID,ocean.region=REL$ocean.region,
                  loc = as.character(REL$ocean.region),
                  brood_year=REL$brood_year,
                  release_year=REL$release_year, n.month=REL$n.month,
                  MEAN.M=colMeans(mort.by2),
                  MEDIAN.M =apply(mort.by2,2,median),
                  SD.M=apply(mort.by2,2,sd),
                  data.frame(t(apply(mort.by2,2,quantile,probs=c(0.025,0.05,0.25,0.75,0.95,0.975)))))

colnames(all.j.mort)[grep("X",colnames(all.j.mort))] <- c("q.025","q.05","q.25","q.75","q.95","q.975")
## modify identifiers for OREGON
all.j.mort$loc <- as.character(all.j.mort$loc)
all.j.mort$loc[all.j.mort$loc == "NOR"|all.j.mort$loc == "COR" |all.j.mort$loc == "SOR"] <- "OR"



### Calculating weighted mean and variance.
w.mean <- function(x,v){
  x <- x[is.na(v)==F]
  v <- v[is.na(v)==F]         
  w <- v^(-1)
  w.mean <- 1/sum(w) * sum(x * w)
  return(w.mean)
}
w.var <- function(x,v){
  x <- x[is.na(v)==F]
  v <- v[is.na(v)==F]            
  w <- v^(-1)
  w.mean  <- w.mean(x,v)
  w.var   <- sum(w*(x-w.mean)^2) / (sum(w)-((sum(w^2)/sum(w))))
  return(w.var)
}

mort_year_rel <- data.frame(group_by(all.j.mort,loc,release_year) %>% 
                    summarise(rel.mean=w.mean(MEAN.M,SD.M^2),rel.var=w.var(MEAN.M,SD.M^2),n.rel=length(MEAN.M[is.na(MEAN.M)==F])))
mort_year_rel$rel.sd <- sqrt(mort_year_rel$rel.var)
mort_year_rel$rel_mean_surv <-exp(mort_year_rel$rel.mean)
mort_year_rel$rel_mean_surv_seplus  <-exp(mort_year_rel$rel.mean + mort_year_rel$rel.sd/sqrt(mort_year_rel$n.rel))
mort_year_rel$rel_mean_surv_seminus <-exp(mort_year_rel$rel.mean - mort_year_rel$rel.sd/sqrt(mort_year_rel$n.rel))

mort_year_rel$rel_mean_surv_seplus[mort_year_rel$n.rel<2] <-NA
mort_year_rel$rel_mean_surv_seminus[mort_year_rel$n.rel<2] <-NA

mort_w_mean   <- data.frame(group_by(mort_year_rel[mort_year_rel$n.rel>1,],loc) %>% 
                   summarise(w.mean=w.mean(rel.mean,rel.var),w.var=w.var(rel.mean,rel.var),n.year=length(rel.var[is.na(rel.var)==F]) ))
mort_w_mean<-merge(mort_w_mean,spawn_loc[,c("ocean.region","init.loc")],by.x="loc",by.y="ocean.region",all=T)
mort_w_mean <- mort_w_mean[is.na(mort_w_mean$n.year)==F,]
mort_w_mean$init.loc[mort_w_mean$loc=="OR"] <- 3
mort_w_mean <- mort_w_mean[order(mort_w_mean$init.loc),]
mort_w_mean$plot.numb <- 1:nrow(mort_w_mean)

mort_w_mean$w.sd <- sqrt(mort_w_mean$w.var)
mort_w_mean


#############################################################################################################################
#############################################################################################################################
######## PLOT for PUB
#############################################################################################################################
#############################################################################################################################

COL = viridis(4,begin=0,end=0.6)                    
yLIM <- c(0,0.65)
xLIM <- c(1977.5,1990.5)
xBreaks <- seq(1978,1990,by=2)
min.year=2

temp <- mort_year_rel[mort_year_rel$loc =="SWVI"|
                        mort_year_rel$loc =="SGEO"|
                        mort_year_rel$loc =="PUSO"|
                        mort_year_rel$loc =="WAC",]
temp$plot_year[temp$loc =="SWVI"] <- temp$release_year[temp$loc =="SWVI"] - 0.15
temp$plot_year[temp$loc =="SGEO"] <- temp$release_year[temp$loc =="SGEO"] - 0.05
temp$plot_year[temp$loc =="PUSO"] <- temp$release_year[temp$loc =="PUSO"] + 0.05
temp$plot_year[temp$loc =="WAC"]  <- temp$release_year[temp$loc =="WAC"] + 0.15
colnames(temp)[grep("loc",colnames(temp))] <- "Region"

p1 <- ggplot(temp[temp$n.rel>=min.year,]) + 
        geom_line(aes(x=plot_year,y=rel_mean_surv,color=Region),size=1.1,alpha=0.8) +
        geom_errorbar(aes(x=plot_year, ymax=rel_mean_surv_seplus,ymin=rel_mean_surv_seminus,color=Region),width=0.1) +
        scale_color_manual(values=COL) +
        scale_y_continuous(limits=yLIM,expand=c(0,0.01))+
        scale_x_continuous(breaks=xBreaks, limits=xLIM,expand=c(0,0))+
        theme_bw() +
        labs(y="Survivorship",x=" ") +
        annotate("text", x = xLIM[1]+0.2, y = yLIM[2]-0.01, label = "a)") +
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),#legend.position="none",
          panel.border=element_rect(colour="black",size=1.5),
          plot.margin=unit(c(0.1, 0.12, 0.05, 0.01), "lines"))
p1

temp <- mort_year_rel[mort_year_rel$loc =="COL"|
                        mort_year_rel$loc =="MCOL"|
                        mort_year_rel$loc =="UPCOL",]
temp$plot_year[temp$loc =="COL"] <- temp$release_year[temp$loc =="COL"] - 0.1
temp$plot_year[temp$loc =="MCOL"] <- temp$release_year[temp$loc =="MCOL"] 
temp$plot_year[temp$loc =="UPCOL"] <- temp$release_year[temp$loc =="UPCOL"] + 0.1
colnames(temp)[grep("loc",colnames(temp))] <- "Region"

p2 <- ggplot(temp[temp$n.rel>=min.year,]) + 
  geom_line(aes(x=plot_year,y=rel_mean_surv,color=Region),size=1.1,alpha=0.8) +
  geom_errorbar(aes(x=plot_year, ymax=rel_mean_surv_seplus,ymin=rel_mean_surv_seminus,color=Region),width=0.1) +
  scale_color_manual(values=COL) +
  scale_y_continuous(limits=yLIM,expand=c(0,0.01))+
  scale_x_continuous(breaks=xBreaks, limits=xLIM,expand=c(0,0))+
  theme_bw() +
  labs(y="Survivorship",x=" ") +
  annotate("text", x = xLIM[1]+0.2, y = yLIM[2]-0.01, label = "b)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),#legend.position="none",
        panel.border=element_rect(colour="black",size=1.5),
        plot.margin=unit(c(0.1, 0.12, 0.05, 0.01), "lines"))

p2

yLIM <- c(0,0.8)
temp <- mort_year_rel[mort_year_rel$loc =="OR"|
                        mort_year_rel$loc =="NCA"|
                        mort_year_rel$loc =="SFB",]
temp$plot_year[temp$loc =="OR"] <- temp$release_year[temp$loc =="OR"] - 0.1
temp$plot_year[temp$loc =="NCA"] <- temp$release_year[temp$loc =="NCA"] 
temp$plot_year[temp$loc =="SFB"] <- temp$release_year[temp$loc =="SFB"] + 0.1
colnames(temp)[grep("loc",colnames(temp))] <- "Region"

p3 <- ggplot(temp[temp$n.rel>=min.year,]) + 
  geom_line(aes(x=plot_year,y=rel_mean_surv,color=Region),size=1.1,alpha=0.8) +
  geom_errorbar(aes(x=plot_year, ymax=rel_mean_surv_seplus,ymin=rel_mean_surv_seminus,color=Region),width=0.1) +
  scale_color_manual(values=COL) +
  scale_y_continuous(limits=yLIM,expand=c(0,0.01))+
  scale_x_continuous(breaks=xBreaks, limits=xLIM, expand=c(0,0))+
  theme_bw() +
  labs(y="Survivorship",x="Release year")+
  annotate("text", x = xLIM[1]+0.2, y = yLIM[2]-0.015, label = "c)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),#legend.position="none",
        panel.border=element_rect(colour="black",size=1.5),
        plot.margin=unit(c(0.1, 0.12, 0.05, 0.01), "lines"))

p3


pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/Juv Surv 2-year PUB ",NAME,".pdf",sep=""),height = 8, width=6)
    Layout= matrix(c(1,2,3),nrow=3,ncol=1)
    multiplot(p1,p2,p3 ,layout= Layout)
dev.off()















J.MORT <- data.frame(temp.A,N.obs=temp.B$N.obs,SE=temp.C$SD/sqrt(temp.B$N.obs))

## Trim data so that only times with at least 3 observations are plotted
J.MORT.trim <- J.MORT[J.MORT$N.obs>=3,]

LOC <- unique(spawn_loc_plot$nom)
COL  <- c("red","red","black","blue","blue","blue","black","darkorchid1","darkorchid1","black")
NUMB <- c(1,1,3,2,2,2,3,3,4,4)

y.lim=c(0,0.8)
x.lim=c(min(REL$release_year)-1,max(REL$release_year))
CEX.PT <- 0.6
LWD=1
PCH  <- c(21,22,23,24,21,25,21,22,23,24)

if(length(LOC)<length(PCH)){
    PCH<-PCH[1:length(LOC)]
    COL<-COL[1:length(LOC)]
    NUMB<-NUMB[1:length(LOC)]
    }

#### START PLOT
quartz(file=paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/Juv Mort Pub plot ",NAME,".jpeg",sep=""),type="jpeg",dpi=600,width=4,height=3)

par(mfrow=c(1,1),mar=c(2,2.5,0.75,0.5))
JIT <- seq(-.025,0.25,length.out=length(LOC))
rel.temp <- data.frame(release.year=release.year)

for(i in 1:length(LOC)){
  J.MORT.trim$release.year.jit <- J.MORT.trim$release.year + JIT[i]
  
  temp.MORT <-merge(rel.temp,J.MORT.trim[J.MORT.trim$loc==LOC[i],],all=T)
  plot(Mean~release.year.jit,data=temp.MORT,type="b",ylim=y.lim,xlim=x.lim,
        pch=PCH[i],bg=COL[i],col=COL[i],lwd=LWD,cex=CEX.PT,axes=F,xlab="",ylab="",yaxs="i")
  arrows(x0=temp.MORT$release.year.jit,
         x1=temp.MORT$release.year.jit,
         y0=temp.MORT$Mean+temp.MORT$SE, 
            y1=temp.MORT$Mean-temp.MORT$SE,
            col=COL[i],lwd=LWD,length=0)
  par(new=T)
}
par(new=F)
axis(1,tcl=-0.2,cex.axis=0.7,padj= -2)
axis(2,las=2,tcl=-0.2,hadj=0.35,cex.axis=0.7)
box(bty="o",lwd=2)
#title(xlab="Release year")
title(ylab="First year surivival",line=1.5,cex.lab=0.8)
title(xlab="Release year",line=1,cex.lab=0.8)

LEG <- data.frame(loc=LOC,PCH=PCH,COL=as.character(COL),NUMB=NUMB)
LEG$COL <- as.character(LEG$COL)
LEG <- LEG[order(LEG$NUMB),]


legend(x=1977,y=y.lim[2],legend=LEG$loc,pch=LEG$PCH,col=LEG$COL,pt.bg=LEG$COL,cex=CEX.PT,bty="n",lwd=LWD)
#mtext("a) Fingerling",side=3,adj=0,cex=0.9)
dev.off()

##################### PLOT ALL OF THE  DATA, REGARDLESS OF PRECISION.

## Trim data so that only times with at least 3 observations are plotted
J.MORT.trim <- J.MORT[J.MORT$N.obs>=3,]

#LOC <- unique(spawn_loc_plot$nom)
#COL  <- c("red","red","black","blue","blue","black","darkorchid1","darkorchid1","black")
#NUMB <- c(1,1,3,2,2,3,3,4,4)
y.lim=c(0,0.8)
x.lim=c(min(REL$release_year)-1,max(REL$release_year))
CEX.PT <- 0.6
LWD=1
#PCH  <- c(21,22,23,24,25,21,22,23,24)

#### START PLOT
quartz(file=paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/Juv Mort Pub plot ALL ",NAME,".jpeg",sep=""),type="jpeg",dpi=600,width=4,height=3)

par(mfrow=c(1,1),mar=c(2,2.5,0.75,0.5))
JIT <- seq(-.025,0.25,length.out=length(LOC))
rel.temp <- data.frame(release.year=release.year)

for(i in 1:length(LOC)){
  J.MORT$release.year.jit <- J.MORT$release.year + JIT[i]
  
  temp.MORT <-merge(rel.temp,J.MORT[J.MORT$loc==LOC[i],],all=T)
  plot(Mean~release.year.jit,data=temp.MORT,type="b",ylim=y.lim,xlim=x.lim,
       pch=PCH[i],bg=COL[i],col=COL[i],lwd=LWD,cex=CEX.PT,axes=F,xlab="",ylab="",yaxs="i")
  # arrows(x0=temp.MORT$release.year.jit,
  #        x1=temp.MORT$release.year.jit,
  #        y0=temp.MORT$Mean+temp.MORT$SE, 
  #        y1=temp.MORT$Mean-temp.MORT$SE,
  #        col=COL[i],lwd=LWD,length=0)
  par(new=T)
}
par(new=F)
axis(1,tcl=-0.2,cex.axis=0.7,padj= -2)
axis(2,las=2,tcl=-0.2,hadj=0.35,cex.axis=0.7)
box(bty="o",lwd=2)
#title(xlab="Release year")
title(ylab="First year surivival",line=1.5,cex.lab=0.8)
title(xlab="Release year",line=1,cex.lab=0.8)

LEG <- data.frame(loc=LOC,PCH=PCH,COL=as.character(COL),NUMB=NUMB)
LEG$COL <- as.character(LEG$COL)
LEG <- LEG[order(LEG$NUMB),]


legend(x=1977,y=y.lim[2],legend=LEG$loc,pch=LEG$PCH,col=LEG$COL,pt.bg=LEG$COL,cex=CEX.PT,bty="n",lwd=LWD)
#mtext("a) Fingerling",side=3,adj=0,cex=0.9)
dev.off()

#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
##################### ALTERNATE ANALYSIS
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################

log_rel_year_all <- apply(log(samp$rel_year_all),2,median)

dat.juv <- data.frame(REL[,c("ID","ocean.region","brood_year","release_year","N.released","Median.month.release","loc.numb","n.month")],
                log_M=log_rel_year_all)

dat.juv$reg.mod <- dat.juv$ocean.region
dat.juv$reg.mod[dat.juv$reg.mod == "SOR" | 
                  dat.juv$reg.mod == "COR" | 
                  dat.juv$reg.mod == "NOR" ] <- "OR"

dat.juv$reg.mod <- as.factor(dat.juv$reg.mod)
dat.juv$year         <- as.factor(dat.juv$release_year)

### Some linear models
mod <- lmer(log_M ~ (1|year) + (n.month|ocean.region) + n.month ,data=dat.juv)
summary(mod)

exp(c(min(log_rel_year_all),max(log_rel_year_all)))
mean(exp(log_rel_year_all))


### Calculating weighted mean and variance.

temp<- data.frame(group_by(dat.juv,reg.mod,release_year) %>% summarise(yr.mean=mean(log_M),yr.var=var(log_M),N=length(reg.mod),tot.release=sum(N.released)))

w.mean <- function(x,v){
              x <- x[is.na(v)==F]
              v <- v[is.na(v)==F]         
              w <- v^(-1)
              w.mean <- 1/sum(w) * sum(x * w)
              return(w.mean)
}
w.var <- function(x,v){
            x <- x[is.na(v)==F]
            v <- v[is.na(v)==F]            
            w <- v^(-1)
            w.mean  <- w.mean(x,v)
            w.var   <- sum(w*(x-w.mean)^2) / (sum(w)-((sum(w^2)/sum(w))))
            return(w.var)
}


juv.dat.sum <- data.frame(group_by(temp,reg.mod) %>% 
              summarise(w.mean=w.mean(yr.mean,yr.var),w.var=w.var(yr.mean,yr.var),n.year=length(yr.var[is.na(yr.var)==F]) ))
juv.dat.sum$w.sd <- sqrt(juv.dat.sum$w.var)
juv.dat.sum$w.se <- juv.dat.sum$w.sd / sqrt(juv.dat.sum$n.year)

juv.dat.sum$loc.numb <- match(juv.dat.sum$reg.mod,LOCATIONS$location.name)
juv.dat.sum$loc.numb[juv.dat.sum$reg.mod =="OR"] <- 7
juv.dat.sum$loc.numb[juv.dat.sum$reg.mod =="MCOL"] <- 8.25
juv.dat.sum$loc.numb[juv.dat.sum$reg.mod =="UPCOL"] <- 8.5
juv.dat.sum <- juv.dat.sum[order(juv.dat.sum$loc.numb),]
juv.dat.sum$numb.plot <- 1:nrow(juv.dat.sum)

################ PLOTS.  
#V1 Separate Panels for mean and variability

par(mfrow=c(1,2),mar=c(4,3.5,0.5,0.5))
plot(numb.plot~w.mean,data=juv.dat.sum,xlim=c(0,1.5),axes=F,xlab="",ylab="",bg=1,pch=21)
arrows(x0=juv.dat.sum$w.mean + juv.dat.sum$w.se, x1=juv.dat.sum$w.mean - juv.dat.sum$w.se,
       y0=juv.dat.sum$numb.plot, y1 = juv.dat.sum$numb.plot,length=0,lwd=1.5)

axis(2,las=2,at=1:nrow(juv.dat.sum),labels=juv.dat.sum$reg.mod)
axis(1)
box(bty="o",lwd=2)

plot(numb.plot~w.sd,data=juv.dat.sum,xlim=c(0,1.0),axes=F,xlab="",ylab="",bg=1,pch=21)
axis(2,las=2,at=1:nrow(juv.dat.sum),labels=rep(" ",nrow(juv.dat.sum)))
axis(1)
box(bty="o",lwd=2)

#V2 Single Panel 
quartz(file=paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/Juv Mort weighted pub plot",NAME,".jpeg",sep=""),type="jpeg",dpi=600,width=4,height=4)

  par(mfrow=c(1,1),mar=c(3,3,0.5,0.5))
  plot(numb.plot~w.mean,data=juv.dat.sum,xlim=c(-0.5,1.5),axes=F,xlab="",ylab="",bg=1,pch=21)
  arrows(x0=juv.dat.sum$w.mean + juv.dat.sum$w.sd, x1=juv.dat.sum$w.mean - juv.dat.sum$w.sd,
       y0=juv.dat.sum$numb.plot, y1 = juv.dat.sum$numb.plot,length=0,lwd=1.5)
  axis(2,las=2,at=1:nrow(juv.dat.sum),labels=juv.dat.sum$reg.mod,cex.axis=0.8,tcl=-0.3,hadj=0.7)
  axis(1,cex.axis=0.8,padj=-1,tcl=-0.3)
  box(bty="o",lwd=2)
  title(xlab=expression("log("*phi*")"),cex.lab=1.1,line=1.7)
  abline(v=mean(juv.dat.sum$w.mean),lty=2,lwd=2)
  
dev.off()
####################################################################


#####################################################################
### YEARLING
# 
# THESE.row<- match(THESE.year,rownames(dat.mean.prop.year))
# # THESE.row <- THESE.row[is.na(THESE.row)==F]
# 
# for(i in 1:length(THESE.row)){
#   temp <- data.frame(year=YEAR,prop=dat.mean.prop.year[THESE.row[i],],sd=dat.sd.prop.year[THESE.row[i],])
#   plot(prop~year,data=temp[temp$prop>0,],type="b",ylim=y.lim,xlim=x.lim,
#        pch=PCH[THESE.row[i]],bg=COL[THESE.row[i]],col=COL[THESE.row[i]],lwd=LWD,cex=CEX.PT,axes=F,xlab="",ylab="",yaxs="i")
#   arrows(x0=temp$year,x1=temp$year,
#          y0=temp$prop+temp$sd, y1=temp$prop-temp$sd,
#          col=COL[THESE.row[i]],lwd=2,length=0)
#   par(new=T)
# }
# par(new=F)
# axis(1,tcl=-0.2,cex.axis=0.8,padj= -1.5)
# axis(2,las=2,tcl=-0.2,hadj=0.35,cex.axis=0.8)
# box(bty="o",lwd=2)
# title(xlab="Release year",line=1.5)
# title(ylab="First year surivival",line=1.5)
# 
# LEG <- data.frame(loc=rownames(dat.mean.prop.year),PCH=PCH,COL=as.character(COL),NUMB=NUMB)
# LEG$COL <- as.character(LEG$COL)
# LEG <- LEG[order(LEG$NUMB),]
# 
# LEG <- LEG[match(THESE.year,LEG$loc),]
# LEG <- LEG[is.na(LEG$PCH)==F,]
# legend(x=1978,y=y.lim[2],legend=LEG$loc,pch=LEG$PCH,col=LEG$COL,pt.bg=LEG$COL,cex=CEX.PT,bty="n",lwd=2)
# mtext("b) Yearling",side=3,adj=0,cex=0.9)
# 
# dev.off()
# 
# ##########
# 
# 
# 
# 
# 
# 
# 
# YEAR <- as.numeric(colnames(dat.mean.prop.fing))
# spawn_loc_plot$PCH  <- c(21,22,23,24,25,21,22,23,24)
# COL  <- c("red","red","black","blue","blue","black","darkorchid1","darkorchid1","black")
# NUMB <- c(1,1,3,2,2,3,3,4,4)
# y.lim=c(0,0.8)
# x.lim=c(min(YEAR),max(YEAR))
# CEX.PT <- 0.7
# LWD=1.5
# 
# 
# 
# 
# 
# 
# 































# 
# rel_yr <- data.frame(rel_id = 1:length(unique(dat.bin$release_year)),rel_year = sort(unique(dat.bin$release_year)))
# 
# nom.all <- sort(unique(dat.bin$year.reg))
# THESE   <- match(nom.all,dat.bin$year.reg)
# nom     <- data.frame( dat.bin[THESE,c("year.reg.idx","ocean.reg","release_year","loc.spawn.idx")],nom = nom.all)
# 
# nom$ocean.reg[nom$ocean.reg == "NWVI" | nom$ocean.reg == "SWVI"] <- "VI"
# nom$ocean.reg[nom$ocean.reg == "SOR" | nom$ocean.reg == "COR" |nom$ocean.reg == "NOR"] <- "OR"
# nom <- nom[order(nom$loc.spawn.idx),]
# 
# dat <- matrix(-99,length(unique(spawn_loc$loc.spawn.idx)),nrow(rel_yr))
# colnames(dat) <- rel_yr$rel_year
# rownames(dat) <- unique(nom$ocean.reg)
# dat.mean <- dat
# dat.sd   <- dat
# 
# for(i in 1:max(dat.bin$year.reg.idx)){
#   X <- which(rownames(dat)==nom$ocean.reg[i]) 
#   Y <- which(colnames(dat)==nom$release_year[i]) 
#   dat.mean[X,Y] <- mean(samp$rel_year_all[,nom$year.reg.idx[i]])
#   dat.sd[X,Y]   <- sd(samp$rel_year_all[,nom$year.reg.idx[i]])
# }
# 
# ###############################################################################
# ##### CONVERT TO PROPORTION SURVIVING FROM RELEASE UP TO WHEN THE MODEL STARTS.
# ###############################################################################
# 
# # Calculate number of outmigrant smolt from various regions....
#   smolt.dat <- read.csv("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Total Smolt and Run Info/_smolt_migrants.csv")
#   smolt.dat <- smolt.dat[smolt.dat$type=="independent",]
#   smolt.dat$ocean.region <- as.character(smolt.dat$rel.region)
#   smolt.dat$ocean.region[smolt.dat$ocean.region == "SOR" | smolt.dat$ocean.region == "COR" | smolt.dat$ocean.region == "NOR" ] <- "OR"
#   smolt.dat$ocean.region[smolt.dat$ocean.region == "SWVI" | smolt.dat$ocean.region == "NWVI"] <- "VI"
#   
#   smolt.mod <- smolt.dat[smolt.dat$type == "independent",]
#   smolt.mod$total.wild.hatch <- smolt.mod$total.releases.median * (1+smolt.mod$frac.wild.missing) 
#   smolt.mod$finger <- smolt.mod$total.wild.hatch * (1 - smolt.mod$frac.yearling )
#   smolt.mod$yearling <- smolt.mod$total.wild.hatch * smolt.mod$frac.yearling
#   
#   smolt.prod <- aggregate(smolt.mod[,c("finger","yearling")],
#                           by=list(number=smolt.mod$location.number,region=smolt.mod$rel.region,
#                                   n.mon.fing = smolt.mod$n.month.finger ,n.mon.year = smolt.mod$n.month.yearling 
#                           ),sum)
#   smolt.prod <- smolt.prod[order(smolt.prod$number),]
#   
#   dat.fing.year <- aggregate(smolt.dat[,c("n.month.finger","n.month.yearling")],by=list(ocean.region=smolt.dat$ocean.region),mean)
# 
#   temp <- REL[,c("n.month","ocean.region","release_year")]
#   temp$ocean.reg <- temp$ocean.region
#   temp$ocean.reg[temp$ocean.reg == "SOR" | temp$ocean.reg == "COR" | temp$ocean.reg == "NOR" ] <- "OR"
#   temp$ocean.reg[temp$ocean.reg == "SWVI" | temp$ocean.reg == "NWVI"] <- "VI"
#   mean.n.month  <- aggregate(temp$n.month,by=list(ocean.region=temp$ocean.reg,rel_year=temp$release_year),mean)
#   colnames(mean.n.month)[3] <- "n.month"
#   mean.n.month <- merge( mean.n.month,dat.fing.year)
#   mean.n.month <- merge( mean.n.month,rel_yr)
#   colnames(mean.n.month)[1:2] <- c("release_year","ocean.reg")
#   mean.n.month <- merge(mean.n.month,nom)
# 
# for(i in  1: dim(samp$rel_year_all)[2]){
#   vals <- sample(samp$rel_year_all[,mean.n.month$year.reg.idx[i]],1000)
#   A    <-   exp(-mean.n.month$n.month.finger[i] * vals)
#   mean.n.month$MEAN.fing[i] <- mean(A)
#   mean.n.month$SD.fing[i]   <- sd(A)
#   B    <-   exp(-mean.n.month$n.month.yearling[i] * vals)
#   mean.n.month$MEAN.year[i] <- mean(B)
#   mean.n.month$SD.year[i]   <- sd(B)
# }
# 
# dat.mean.prop.fing <- dat
# dat.sd.prop.fing   <- dat
# dat.mean.prop.year <- dat
# dat.sd.prop.year   <- dat
# 
# for(i in 1:max(dat.bin$year.reg.idx)){
#   X <- which(rownames(dat)== mean.n.month$ocean.reg[i]) 
#   Y <- which(colnames(dat)== mean.n.month$release_year[i]) 
#   dat.mean.prop.fing[X,Y] <- mean.n.month$MEAN.fing[i]
#   dat.sd.prop.fing[X,Y]   <- mean.n.month$SD.fing[i]
#   dat.mean.prop.year[X,Y] <- mean.n.month$MEAN.year[i]
#   dat.sd.prop.year[X,Y]   <- mean.n.month$SD.year[i]
# }
# 
# ###############################################################################
# ##### START PLOT
# ###############################################################################
# 
# pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/"," Juv mortality plots ",NAME,".pdf",sep=""),height = 6, width=8.5)
# 
# # plot mean mortality rate
#   par(mar=c(5,5,1,1))
#   z.lim	<- c(0,round(max(colMeans(samp$rel_year_all)),1))
#   y.lab <- rownames(dat)
#   x.lab	<- rel_yr$rel_year
#   col.br <- colorRampPalette(c("blue",grey(0.9),"red"))
#   par(mfrow=c(1,1),oma=c( 0,0,0,4) )
#   image(z=t(dat.mean),y=1:nrow(dat.mean),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
#         col=1,zlim=c(-100,-98))
#   image(z=t(dat.mean),y=1:nrow(dat.mean),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
#         col=col.br(32),zlim=z.lim,add=T)
#   box(bty="o",lwd=2)
#   axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
#   axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
#   title(ylab="Origin",line=4)
#   title(xlab="Release Year",line=3.5)
#   title(main="Mean monthly mortality rate")
#   par(mfrow=c(1,1),oma=c( 0,0,0,0) )
#   ticks <- seq(min(z.lim),max(z.lim),length.out=6)
#   image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
#   
#   # plot sd mortality rate
#   par(mar=c(5,5,1,1))
#   z.lim	<- c(0,max(dat.sd))
#   y.lab <- rownames(dat)
#   x.lab	<- rel_yr$rel_year
#   col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
#   par(mfrow=c(1,1),oma=c( 0,0,0,4) )
#   image(z=t(dat.sd),y=1:nrow(dat.mean),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
#         col=1,zlim=c(-100,-98))
#   image(z=t(dat.sd),y=1:nrow(dat.mean),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
#         col=col.br(32),zlim=z.lim,add=T)
#   box(bty="o",lwd=2)
#   axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
#   axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
#   title(ylab="Origin",line=4)
#   title(xlab="Release Year",line=3.5)
#   title(main="SD monthly mortality rate")
#   par(mfrow=c(1,1),oma=c( 0,0,0,0) )
#   ticks <- seq(min(z.lim),max(z.lim),length.out=6)
#   image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
#   
#   #plot(dat.sd~dat.mean,xlim=c(0,max(dat.mean)),ylim=c(0,0.10))
# 
#   ###############################################################################
#   ##### 
#   ###############################################################################
#   # plot mean mortality in proportion
#   par(mar=c(5,5,1,1))
#   z.lim	<- c(0,round(max(mean.n.month$MEAN.fing),1))
#   y.lab <- rownames(dat)
#   x.lab	<- rel_yr$rel_year
#   col.br <- colorRampPalette(c("blue",grey(0.9),"red"))
#   par(mfrow=c(1,1),oma=c( 0,0,0,4) )
#   image(z=t(dat.mean.prop.fing),y=1:nrow(dat.mean.prop.fing),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
#         col=1,zlim=c(-100,-98))
#   image(z=t(dat.mean.prop.fing),y=1:nrow(dat.mean.prop.fing),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
#         col=col.br(32),zlim=z.lim,add=T)
#   box(bty="o",lwd=2)
#   axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
#   axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
#   title(ylab="Origin",line=4)
#   title(xlab="Release Year",line=3.5)
#   title(main="Proportion surviving to model start (fingerlings)")
#   par(mfrow=c(1,1),oma=c( 0,0,0,0) )
#   ticks <- seq(0,max(z.lim),length.out=6)
#   image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
#   
#   # plot sd mortality rate
#   par(mar=c(5,5,1,1))
#   z.lim	<- c(0,max(dat.sd.prop.fing))
#   y.lab <- rownames(dat)
#   x.lab	<- rel_yr$rel_year
#   col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
#   par(mfrow=c(1,1),oma=c( 0,0,0,4) )
#   image(z=t(dat.sd.prop.fing),y=1:nrow(dat.mean.prop.fing),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
#         col=1,zlim=c(-100,-98))
#   image(z=t(dat.sd.prop.fing),y=1:nrow(dat.mean.prop.fing),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
#         col=col.br(32),zlim=z.lim,add=T)
#   box(bty="o",lwd=2)
#   axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
#   axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
#   title(ylab="Origin",line=4)
#   title(xlab="Release Year",line=3.5)
#   title(main="SD Proportion surviving to model start (fingerlings)")
#   par(mfrow=c(1,1),oma=c( 0,0,0,0) )
#   ticks <- seq(min(z.lim),max(z.lim),length.out=6)
#   image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
#   
#   # plot mean mortality in proportion
#   par(mar=c(5,5,1,1))
#   z.lim	<- c(0,round(max(mean.n.month$MEAN.year),2))
#   y.lab <- rownames(dat)
#   x.lab	<- rel_yr$rel_year
#   col.br <- colorRampPalette(c("blue",grey(0.9),"red"))
#   par(mfrow=c(1,1),oma=c( 0,0,0,4) )
#   image(z=t(dat.mean.prop.year),y=1:nrow(dat.mean.prop.year),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
#         col=1,zlim=c(-100,-98))
#   image(z=t(dat.mean.prop.year),y=1:nrow(dat.mean.prop.year),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
#         col=col.br(32),zlim=z.lim,add=T)
#   box(bty="o",lwd=2)
#   axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
#   axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
#   title(ylab="Origin",line=4)
#   title(xlab="Release Year",line=3.5)
#   title(main="Proportion surviving to model start (yearlings)")
#   par(mfrow=c(1,1),oma=c( 0,0,0,0) )
#   ticks <- seq(0,max(z.lim),length.out=6)
#   image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
#   
#   # plot sd mortality rate
#   par(mar=c(5,5,1,1))
#   z.lim	<- c(0,max(dat.sd.prop.year))
#   y.lab <- rownames(dat)
#   x.lab	<- rel_yr$rel_year
#   col.br <- colorRampPalette(c("blue", "cyan", "yellow", "red"))
#   par(mfrow=c(1,1),oma=c( 0,0,0,4) )
#   image(z=t(dat.sd.prop.year),y=1:nrow(dat.mean.prop.year),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
#         col=1,zlim=c(-100,-98))
#   image(z=t(dat.sd.prop.year),y=1:nrow(dat.mean.prop.year),x=rel_yr$rel_id,axes=F,ylab="",xlab="",
#         col=col.br(32),zlim=z.lim,add=T)
#   box(bty="o",lwd=2)
#   axis(1,las=2,at=rel_yr$rel_id,labels=x.lab,hadj=0.85)
#   axis(2,las=2,at=1:nrow(dat.mean),labels=y.lab,hadj=0.85)
#   title(ylab="Origin",line=4)
#   title(xlab="Release Year",line=3.5)
#   title(main="SD Proportion surviving to model start (yearlings)")
#   par(mfrow=c(1,1),oma=c( 0,0,0,0) )
#   ticks <- seq(min(z.lim),max(z.lim),length.out=6)
#   image.plot(legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at= ticks))
#   
#   ############
#   ### PLOT HIERARCHICAL juvenile mortality rates
#   ############
#   all.rates <- colMeans(samp$rel_year_all)
#   x.lim <- c(0,0.6)
#   XX   <- seq(0.01,0.7,length.out=1000)
#   DENS <- dlnorm(XX,mean(samp$log_rel_year_mu),mean(samp$rel_year_sigma))
#   
#   hist(all.rates,breaks=seq(0,1,by=0.025),col=grey(0.5),xaxs="i",yaxs="i",axes=F,xlim=x.lim,xlab="",ylab="",main="")
#   axis(1)
#   axis(2,las=2)
#   par(new=T)
#   plot(DENS~XX,type="l",col=2,lwd=3,axes=F,xlim=x.lim,xlab="",ylab="",xaxs="i",yaxs="i")
#   box(bty="o",lwd=2)
#   title(ylab="Frequency")
#   title(xlab="Monthly mortality rate")
#   title(main="Among site variation and hierarchical, cross population estimate")
# 
# dev.off()
# 
# 
# #############################################################################################################################
# #############################################################################################################################
# #############################################################################################################################
# ######## PLOT for PUB
# #############################################################################################################################
# #############################################################################################################################
# #############################################################################################################################
# 
# 
# THESE.fing <- as.character(smolt.prod$region[which(smolt.prod$finger >0)])
# THESE.fing[THESE.fing == "SWVI"] <- "VI"
# THESE.fing[THESE.fing == "SOR" | THESE.fing == "COR" |THESE.fing == "NOR"] <- "OR"
# THESE.fing <- unique(THESE.fing)
# 
# THESE.year <- as.character(smolt.prod$region[which(smolt.prod$yearling >0)])
# THESE.year[THESE.year == "SWVI"] <- "VI"
# THESE.year[THESE.year == "SOR" | THESE.year == "COR" | THESE.year == "NOR" ] <- "OR"
# THESE.year <- unique(THESE.year)
# 
# # dat.mean.prop.fing
# # dat.sd.prop.fing
# 
# #####################################################################
# ### FINGERLING
# YEAR <- as.numeric(colnames(dat.mean.prop.fing))
# PCH  <- c(21,22,23,24,25,21,22,23,24)
# COL  <- c("red","red","black","blue","blue","black","darkorchid1","darkorchid1","black")
# NUMB <- c(1,1,3,2,2,3,3,4,4)
# y.lim=c(0,0.8)
# x.lim=c(min(YEAR),max(YEAR))
# CEX.PT <- 0.7
# LWD=1.5
# 
# #### START PLOT
# quartz(file=paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/Juv Mort Pub plot.jpeg",sep=""),type="jpeg",dpi=600,width=4,height=6)
# 
# par(mfrow=c(2,1),mar=c(2.5,2.5,0.75,0.5))
# THESE.row <- match(THESE.fing,rownames(dat.mean.prop.fing))
# 
# for(i in 1:length(THESE.row)){
#   temp <- data.frame(year=YEAR,prop=dat.mean.prop.fing[THESE.row[i],],sd=dat.sd.prop.fing[THESE.row[i],])
#   plot(prop~year,data=temp[temp$prop>0,],type="b",ylim=y.lim,xlim=x.lim,
#         pch=PCH[THESE.row[i]],bg=COL[THESE.row[i]],col=COL[THESE.row[i]],lwd=LWD,cex=CEX.PT,axes=F,xlab="",ylab="",yaxs="i")
#   arrows(x0=temp$year,x1=temp$year,
#             y0=temp$prop+temp$sd, y1=temp$prop-temp$sd,
#             col=COL[THESE.row[i]],lwd=2,length=0)
#   par(new=T)
# }
# par(new=F)
# axis(1,tcl=-0.2,cex.axis=0.8,padj= -1.5)
# axis(2,las=2,tcl=-0.2,hadj=0.35,cex.axis=0.8)
# box(bty="o",lwd=2)
# #title(xlab="Release year")
# title(ylab="First year surivival",line=1.5)
# 
# LEG <- data.frame(loc=rownames(dat.mean.prop.fing),PCH=PCH,COL=as.character(COL),NUMB=NUMB)
# LEG$COL <- as.character(LEG$COL)
# LEG <- LEG[order(LEG$NUMB),]
# 
# LEG <- LEG[match(THESE.fing,LEG$loc),]
# LEG <- LEG[is.na(LEG$PCH)==F,]
# legend(x=1978,y=y.lim[2],legend=LEG$loc,pch=LEG$PCH,col=LEG$COL,pt.bg=LEG$COL,cex=CEX.PT,bty="n",lwd=2)
# mtext("a) Fingerling",side=3,adj=0,cex=0.9)
# 
# #####################################################################
# ### YEARLING
# 
# THESE.row<- match(THESE.year,rownames(dat.mean.prop.year))
# # THESE.row <- THESE.row[is.na(THESE.row)==F]
# 
# for(i in 1:length(THESE.row)){
#   temp <- data.frame(year=YEAR,prop=dat.mean.prop.year[THESE.row[i],],sd=dat.sd.prop.year[THESE.row[i],])
#   plot(prop~year,data=temp[temp$prop>0,],type="b",ylim=y.lim,xlim=x.lim,
#        pch=PCH[THESE.row[i]],bg=COL[THESE.row[i]],col=COL[THESE.row[i]],lwd=LWD,cex=CEX.PT,axes=F,xlab="",ylab="",yaxs="i")
#   arrows(x0=temp$year,x1=temp$year,
#          y0=temp$prop+temp$sd, y1=temp$prop-temp$sd,
#          col=COL[THESE.row[i]],lwd=2,length=0)
#   par(new=T)
# }
# par(new=F)
# axis(1,tcl=-0.2,cex.axis=0.8,padj= -1.5)
# axis(2,las=2,tcl=-0.2,hadj=0.35,cex.axis=0.8)
# box(bty="o",lwd=2)
# title(xlab="Release year",line=1.5)
# title(ylab="First year surivival",line=1.5)
# 
# LEG <- data.frame(loc=rownames(dat.mean.prop.year),PCH=PCH,COL=as.character(COL),NUMB=NUMB)
# LEG$COL <- as.character(LEG$COL)
# LEG <- LEG[order(LEG$NUMB),]
# 
# LEG <- LEG[match(THESE.year,LEG$loc),]
# LEG <- LEG[is.na(LEG$PCH)==F,]
# legend(x=1978,y=y.lim[2],legend=LEG$loc,pch=LEG$PCH,col=LEG$COL,pt.bg=LEG$COL,cex=CEX.PT,bty="n",lwd=2)
# mtext("b) Yearling",side=3,adj=0,cex=0.9)
# 
# dev.off()
# 
# #############################################################################################################################
# #############################################################################################################################
# #############################################################################################################################
# ######## PLOT for PUB (Adult Mortality)
# #############################################################################################################################
# #############################################################################################################################
# #############################################################################################################################
# dat <- data.frame(cum_M2)
# dat$cum_surv <- exp(-dat$cum_M2)
# 
# par(mfcol=c(2,2))
# plot(cum_M2~age,data=dat,type="l",lwd=2,ylab="Cumulative Mortality",xlab="Model Age (months)")
# abline(v=c(12,24,36,48),lty=2)
# abline(v=7,col=2)
# plot(cum_surv~age,data=dat,type="l",lwd=2,ylab="Cumulative Survival",xlab="Model Age (months)",ylim=c(0,1))
# abline(v=c(12,24,36,48),lty=2)
# abline(v=7,col=2)
# plot(M2~age,data=dat,type="l",lwd=2,ylab="Instantaneous Mortality",xlab="Model Age (months)")
# abline(v=c(12,24,36,48),lty=2)
# abline(v=7,col=2)
# 
#   year.mort <- data.frame(age=1:4)
#   year.mort$ann.surv[1] <- exp(-dat$cum_M2[dat$age == 12])
#   year.mort$ann.surv[2] <- exp(-dat$cum_M2[dat$age == 24] + dat$cum_M2[dat$age == 12])
#   year.mort$ann.surv[3] <- exp(-dat$cum_M2[dat$age == 36] + dat$cum_M2[dat$age == 24])
#   year.mort$ann.surv[4] <- exp(-dat$cum_M2[dat$age == 48] + dat$cum_M2[dat$age == 36])
# 
# plot(ann.surv~age,data=year.mort,type="b",lwd=2,ylab="Annual Survival",xlab="Model Age (Year)")
# 
# 
