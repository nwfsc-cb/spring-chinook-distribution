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


mort_year_rel$loc <- factor(mort_year_rel$loc,
       levels = c("SWVI", "SGEO","PUSO","WAC","UPCOL","MCOL","COL","OR","NCA","SFB"))
COL = viridis(4,begin=0,end=0.8)                    
yLIM <- c(0,0.65)
xLIM <- c(1977.5,1990.5)
xBreaks <- seq(1978,1990,by=2)
min.year=3

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
        annotate("text", x = xLIM[1]+0.3, y = yLIM[2]-0.02, label = "a)") +
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),#legend.position="none",
          panel.border=element_rect(colour="black",size=1.5),
          plot.margin=unit(c(0.1, 0.1, 0.05, 0.01), "lines"))
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
  annotate("text", x = xLIM[1]+0.3, y = yLIM[2]-0.02, label = "b)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),#legend.position="none",
        panel.border=element_rect(colour="black",size=1.5),
        plot.margin=unit(c(0.1, -0.25, 0.05, 0.01), "lines"))

p2

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
  annotate("text", x = xLIM[1]+0.3, y = yLIM[2]-0.025, label = "c)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),#legend.position="none",
        panel.border=element_rect(colour="black",size=1.5),
        plot.margin=unit(c(0.1, 0.6, 0.05, 0.01), "lines"))

p3


quartz(file=paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/Juv Surv 2-year PUB min=3",NAME,".pdf",sep=""),height = 8, width=6,dpi=300,type="pdf")
    Layout= matrix(c(1,2,3),nrow=3,ncol=1)
    multiplot(p1,p2,p3 ,layout= Layout)
dev.off()













