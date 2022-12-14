library(ggplot2)

###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
#### In-River recoveries 
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################

# Trim the data based on brood, release and recovery years.
fresh.recover$dat.consolidated <- fresh.recover$dat.consolidated %>% 
                                        filter(brood.year %in% YEARS.BROOD,
                                               rel.year %in% YEARS.RELEASE)

fresh.recover$dat.fish <- fresh.recover$dat.fish %>% 
                              filter(brood.year %in% YEARS.BROOD,
                                    rel.year %in% YEARS.RELEASE)


river.entry <- fresh.recover$river.entry
RIV <- data.frame(ID=sort(unique(REL$ID)))

A <- aggregate(river.entry$est.numb,by=list(ID= river.entry$ID,rec.month=river.entry$rec.month),sum)
A <- A[order(A$ID,A$rec.month),]
B <- aggregate(A$x,by=list(ID=A$ID),max)

peak.month <- merge(A,B)
peak.month <- peak.month[order(peak.month$ID),]
colnames(peak.month)[3] <- "peak.month"

RIV <- merge(RIV,peak.month,all=T)

RIV$first.month <- RIV$peak.month -1
RIV$last.month <- RIV$peak.month 

# Manually adjust a few rivers based on visual examination
#RIV[RIV==13]<-1
#RIV[RIV== -1 ]<- 11

# Assert all fish enter river starting in August and lasting til October. 

#RIV$first.month <- 8
#RIV$last.month  <- 10

##### PARSE RIVER ENTRY FOR WORKING WITH AWG tag codes.

# RIV[RIV$ID == "Capilano",c("first.month","last.month")] <- c(6,9)
# RIV[RIV$ID == "Chilliwack",c("first.month","last.month")] <- c(7,10)
# RIV[RIV$ID == "Chetco",c("first.month","last.month")] <- c(9,10)
# RIV[RIV$ID == "Conuma",c("first.month","last.month")] <- c(9,10)
# #RIV[RIV$ID == "Cowichan",c("first.month","last.month")] <- c(7,10)
# #RIV[RIV$ID == "Cowlitz_small",c("first.month","last.month")] <- c(7,10)
# #RIV[RIV$ID == "Hoko",c("first.month","last.month")] <- c(10,11)
# #RIV[RIV$ID == "HoodCanal",c("first.month","last.month")] <- c(8,10)
# #RIV[RIV$ID == "Issaquah",c("first.month","last.month")] <- c(7,10)
# #RIV[RIV$ID == "Lyons_large",c("first.month","last.month")] <- c(8,11)
# #RIV[RIV$ID == "Lyons_small",c("first.month","last.month")] <- c(8,11)
# #RIV[RIV$ID == "Nanaimo",c("first.month","last.month")] <- c(8,11)
# RIV[RIV$ID == "Elk",c("first.month","last.month")] <- c(9,10)
# RIV[RIV$ID == "Nitinat",c("first.month","last.month")] <- c(7,8)
# RIV[RIV$ID == "Puntledge",c("first.month","last.month")] <- c(8,9)
# RIV[RIV$ID == "Quinsam",c("first.month","last.month")] <- c(10,11)
# RIV[RIV$ID == "Salmon_WA",c("first.month","last.month")] <- c(9,10)
# 
# 
# RIV[RIV$ID == "Salmon(OR)",c("first.month","last.month")] <- c(9,10)
# 
# # Columbia
# RIV[RIV$ID == "Stayton",c("first.month","last.month")] <- c(7,8)
# RIV[RIV$ID == "Stayton",c("first.month","last.month")] <- c(7,8)
# RIV[RIV$ID == "Stayton",c("first.month","last.month")] <- c(7,8)
# RIV[RIV$ID == "Stayton",c("first.month","last.month")] <- c(7,8)
# RIV[RIV$ID == "Stayton",c("first.month","last.month")] <- c(7,8)
# RIV[RIV$ID == "Stayton",c("first.month","last.month")] <- c(7,8)
# RIV[RIV$ID == "Stayton",c("first.month","last.month")] <- c(7,8)
# RIV[RIV$ID == "Stayton",c("first.month","last.month")] <- c(7,8)
# 
# RIV[RIV$ID == "Stayton",c("first.month","last.month")] <- c(8,9)
# #RIV[RIV$ID == "Soos",c("first.month","last.month")] <- c(9,10)
# 
# #Trinity, Klamath
# RIV[RIV$ID == "Irongate_small",c("first.month","last.month")] <- c(9,10)
# RIV[RIV$ID == "Irongate_large",c("first.month","last.month")] <- c(9,10)
# RIV[RIV$ID == "Trinity_small",c("first.month","last.month")] <- c(9,10)
# RIV[RIV$ID == "Trinity_large",c("first.month","last.month")] <- c(9,10)

dat  <- fresh.recover$dat.consolidated
dat$age <- dat$rec.year - dat$brood.year ##### RECOGNIZE THAT "OCEAN.YEAR" is actually just "age"

# Merge in information from the REL data.frame
dat <- dat %>% 
        left_join(., REL %>%
                    dplyr::select(ID,
                                  brood.year=brood_year,
                                  rel.year=release_year,
                                  ocean.region,
                                  Median.month.release,
                                  N.released,
                                  loc.numb)) %>%
        filter(!ocean.region=="PWS_spr")

# trim out really old fish as data entry errors
dat <- dat %>% filter(age<=7)

# Look at where the AWG data shows up by ocean.region

AWG_id <- dat %>% ungroup() %>% group_by(ocean.region,auxiliary,brood.year) %>%
    summarise(N_group = length(brood.year)) %>%
    group_by(ocean.region,auxiliary) %>%
    summarise(N_year = length(N_group)) %>%
    mutate(auxiliary=paste0("aux.",auxiliary)) %>%
    pivot_wider(.,id_cols="ocean.region",values_from="N_year",names_from="auxiliary") %>%
    left_join(.,REL %>% distinct(ocean.region,loc.numb) ) %>% 
    arrange(loc.numb) %>% as.data.frame()

dat.ratio <- dat %>% ungroup() %>% left_join(.,
                         REL %>% 
                           dplyr::select(ID,
                                         brood.year=brood_year,
                                         rel.year=release_year,
                                         ocean.region,
                                         N.released,
                                         loc.numb)) %>%
                      mutate(fw.prop = est.numb / N.released) 

# Combine the auxiliary == 0 and auxiliary == 1
dat <- dat %>% mutate(auxiliary=paste0("aux.",auxiliary)) %>% ungroup()
dat.sum <- dat %>% ungroup() %>% 
            pivot_wider(., 
                        id_cols=c("ID",
                          "brood.year",
                          "rel.year",
                          "rec.year",
                          "ocean.region",
                          "age",
                          "N.released"),
                        names_from="auxiliary",
                        values_from = "est.numb",
                        values_fill = list(est.numb = 0)) %>%
          mutate(est.numb.sum = aux.0 + aux.1,
                 prop.aux = aux.1 / est.numb.sum,
                 fw.prop.tot = est.numb.sum / N.released,
                 fw.prop.0 = aux.0 / N.released,
                 fw.prop.1 = aux.1 / N.released)

# Only include the groups in REL
dat.sum <- dat.sum %>% semi_join(.,REL)

# Add in data that never show up in the freshwater
dat.sum.extra <- anti_join(REL %>% dplyr::select(ID,
                                                 brood.year=brood_year,
                                                 rel.year=release_year,
                                                 ocean.region,N.released),
                           dat.sum %>% distinct(ID,brood.year,rel.year,ocean.region)
                          ) %>% 
                      mutate(age=-99,aux.0=0,aux.1=0,est.numb.sum=0,
                             fw.prop.tot=0,fw.prop.0=0,fw.prop.1=0)

dat.sum <- full_join(dat.sum,dat.sum.extra)

dat.sum$ocean.region <- factor(dat.sum$ocean.region,
                  levels= REL %>% 
                            distinct(ocean.region,loc.numb) %>% 
                            arrange(loc.numb,ocean.region) %>% 
                            pull(ocean.region))

# Try out a few plots of this data.
p1 <- ggplot(dat.sum ) +
        geom_point(aes(y=fw.prop.0,x=brood.year,color=as.factor(age)),alpha=0.5) +
        theme_bw() +
        ggtitle("No AWG only")+
        scale_y_continuous("Surv to FW (no aux)",trans="log",breaks=c(1e-6,1e-5,1e-4,1e-3,1e-2))+
        facet_wrap(~ocean.region)
p1

p2 <- ggplot(dat.sum ) +
  geom_point(aes(y=fw.prop.1,x=brood.year,color=as.factor(age)),alpha=0.5) +
  theme_bw() +
  ggtitle("AWG only")+
  scale_y_continuous("Surv to FW (aux only)",trans="log",breaks=c(1e-6,1e-5,1e-4,1e-3,1e-2))+
  facet_wrap(~ocean.region)
p2

p.sum <- ggplot(dat.sum) +
  geom_point(aes(y=fw.prop.tot,x=brood.year,color=as.factor(age)),alpha=0.5) +
  theme_bw() +
  scale_y_continuous("Surv to FW (ALL)",trans="log",breaks=c(1e-6,1e-5,1e-4,1e-3,1e-2))+
  facet_wrap(~ocean.region)
p.sum

p.sum2 <- ggplot(dat.sum %>% filter(age %in% c(2,3,4,5))) +
  geom_point(aes(y=fw.prop.tot,x=brood.year,color=as.factor(age)),alpha=0.5) +
  theme_bw() +
  scale_y_continuous(trans="log",breaks=c(1e-6,1e-5,1e-4,1e-3,1e-2))+
  facet_wrap(~ocean.region)
p.sum2


pdf(file=paste0(base.dir,"/spring-chinook-distribution/Output plots/FW_plots/Freshwater_recoveries_by_region-",GROUP,".pdf"),
                onefile=T,width=15,height=8.5)
    print(p1)
    print(p2)
    print(p.sum)
dev.off()





####
 S.to.N <- REL %>% 
            distinct(ocean.region,loc.numb) %>% 
            arrange(loc.numb,ocean.region) %>% 
            pull(ocean.region)
 
 # make a figure for each ocean.region faceted by wramp
 p_ID_by_ocean.region <- list()
 p_ID_by_ocean.region_aux0 <- list()
 p_ID_by_ocean.region_aux1 <- list()
 for(i in 1:length(S.to.N)){
  p_ID_by_ocean.region[[S.to.N[i]]] <- 
    ggplot(dat.sum %>% filter(ocean.region==S.to.N[i])) +
      geom_point(aes(y=fw.prop.tot,x=brood.year,color=as.factor(age)),alpha=0.75) +
      theme_bw() +
      scale_y_continuous("Surv to FW",trans="log",breaks=c(1e-6,1e-5,1e-4,1e-3,1e-2))+
      facet_wrap(~ID) +
      ggtitle(S.to.N[i])
  
  p_ID_by_ocean.region_aux0[[S.to.N[i]]] <- 
    ggplot(dat.sum %>% filter(ocean.region==S.to.N[i])) +
    geom_point(aes(y=fw.prop.0,x=brood.year,color=as.factor(age)),alpha=0.75) +
    theme_bw() +
    scale_y_continuous("Surv to FW",trans="log",breaks=c(1e-6,1e-5,1e-4,1e-3,1e-2))+
    facet_wrap(~ID) +
    ggtitle(S.to.N[i])

  p_ID_by_ocean.region_aux1[[S.to.N[i]]] <- 
    ggplot(dat.sum %>% filter(ocean.region==S.to.N[i])) +
    geom_point(aes(y=fw.prop.1,x=brood.year,color=as.factor(age)),alpha=0.75) +
    theme_bw() +
    scale_y_continuous("Surv to FW",trans="log",breaks=c(1e-6,1e-5,1e-4,1e-3,1e-2))+
    facet_wrap(~ID) +
    ggtitle(S.to.N[i])
  
  
 }
    
 pdf(file=paste0(base.dir,"/spring-chinook-distribution/Output plots/FW_plots/Freshwater_recoveries_by_region_ID-",GROUP,".pdf"),
                 onefile=T,width=11,height=8.5)
    for(i in 1:length(S.to.N)){
     print(p_ID_by_ocean.region[[S.to.N[i]]])
    }
 dev.off()
 
 pdf(file=paste0(base.dir,"/spring-chinook-distribution/Output plots/FW_plots/Freshwater_recoveries_by_region_ID_AUX0-",GROUP,".pdf"),
     onefile=T,width=11,height=8.5)
 for(i in 1:length(S.to.N)){
   print(p_ID_by_ocean.region_aux0[[S.to.N[i]]])
 }
 dev.off()
 
 pdf(file=paste0(base.dir,"/spring-chinook-distribution/Output plots/FW_plots/Freshwater_recoveries_by_region_ID_AUX1-",GROUP,".pdf"),
     onefile=T,width=11,height=8.5)
 for(i in 1:length(S.to.N)){
   print(p_ID_by_ocean.region_aux1[[S.to.N[i]]])
 }
 dev.off()

 
 
 

### Add in fish caught in terminal (or semi-terminal fisheries)
# escape.net$age <- escape.net$model.year
# escape.net$rel.month <- NA
# escape.net$rec.year  <- escape.net$rel.year + escape.net$age
# escape.net$count.zero <- NA
# escape.temp <- escape.net[,c("ID","brood.year","rel.year","rel.month","ocean.region","rec.year","est.numb","count","median.frac.samp","count.zero","age")]
# 
# dat <- rbind(dat,escape.temp)
# 
escape.dat <- aggregate(dat[,c("est.numb","count","count.zero")],by=list(
  ID = dat$ID,
  brood.year = dat$brood.year,
  rel.year = dat$rel.year,
  #rec.month = fresh.recover$dat$rec.month,
  rec.year = dat$rec.year,
  ocean.region = dat$ocean.region,
  age = dat$age),
  sum,na.rm=T)

escape.dat.frac <- aggregate(dat$median.frac.samp,by=list(
  ID = dat$ID,
  brood.year = dat$brood.year,
  rel.year = dat$rel.year,
  #rec.month = fresh.recover$dat$rec.month,
  rec.year = dat$rec.year,
  ocean.region = dat$ocean.region),
  mean,na.rm=T)

colnames(escape.dat.frac)[ncol(escape.dat.frac)] <- "mean.frac.samp"

### Do some adjustments for fish that appear in fresh water in november or later following year.
riv.temp <- river.entry[river.entry$rec.month >=11,]
riv.temp.spr <- riv.temp %>% filter(grepl("_spr",ID))
riv.temp.wint <- riv.temp %>% filter(grepl("_wint",ID))

riv.temp.spr.wint <- bind_rows(riv.temp.spr,riv.temp.wint)

adjust <- riv.temp.spr.wint %>% 
  group_by(ID,brood.year,rel.year,
           rec.year,ocean.region) %>%
  summarise(x= sum(est.numb))

adjust.add <- adjust
adjust.add$rec.year <-adjust.add$rec.year+1

escape.dat1<- merge(escape.dat,adjust.add,all=T)
escape.dat1$x[is.na(escape.dat1$x)==T] <- 0
escape.dat1$est.numb <- escape.dat1$est.numb + escape.dat1$x

escape.dat <- escape.dat1[,1:9]

escape.dat2<- merge(escape.dat,adjust,all=T)
escape.dat2$x[is.na(escape.dat2$x)==T] <- 0
escape.dat2$est.numb <- escape.dat2$est.numb - escape.dat2$x

escape.dat <- escape.dat1[,1:9]
escape.dat$est.numb[escape.dat$est.numb < 0] <- 0

escape.dat <- merge(escape.dat,escape.dat.frac)

#######################
#######################
# Manually inflate the variance of SFB returning fish (median frac samp = 0.3)
# also inflate NWVI (Conuma) to 0.01
#######################
#######################
# escape.dat$mean.frac.samp[escape.dat$ocean.region =="NCA"] <- 0.2
# escape.dat$mean.frac.samp[escape.dat$ocean.region =="SFB"] <- 0.2
# escape.dat$mean.frac.samp[escape.dat$ocean.region =="NWVI"] <- 0.01
###############################################################################################################

escape.dat$mod.year <- escape.dat$age 

# escape.dat$mod.year[escape.dat$model.age <= 7] <- 1
# escape.dat$mod.year[escape.dat$model.age >7 & escape.dat$model.age <=14 ]  <- 2
# escape.dat$mod.year[escape.dat$model.age >14 & escape.dat$model.age <=21 ] <- 3
# escape.dat$mod.year[escape.dat$model.age >21 & escape.dat$model.age <=28 ] <- 4
# escape.dat$mod.year[escape.dat$model.age >28 & escape.dat$model.age <=35 ] <- 5
# escape.dat$mod.year[escape.dat$model.age >35 & escape.dat$model.age <=42 ] <- 6

escape.year <- escape.dat %>% group_by(ID,mod.year,brood.year,rel.year) %>%
  summarise(est.numb=sum(est.numb), count=sum(count),count.zero=sum(count.zero))
escape.year.frac <- escape.dat %>% group_by(ID,mod.year,brood.year,rel.year) %>% 
  summarise(mean.frac.samp = mean(mean.frac.samp))
escape.year <- merge(escape.year,escape.year.frac)
escape.year$cal.year <- escape.year$rel.year + escape.year$mod.year - 1

escape.year <- escape.year[order(escape.year$ID, escape.year$rel.year,escape.year$cal.year),]

##### 
#Create Escapement Matrix 
##### 
N.mod.year <- 6

E <- array(NA,dim=c(N.REL,N.mod.year,5),dimnames = list(paste("rel.ID",1:N.REL,sep="."),1:N.mod.year,c("mod.year","est.numb","count","count.zero","lambda2")))
for(i in 1:N.REL){
  E[i,,1] <- 1:N.mod.year
  temp <- escape.year %>% filter(ID == REL$ID[i],
                                 brood.year == REL$brood_year[i],
                                 rel.year == REL$release_year[i])
  
  #  escape.year[escape.year$ID == REL$ID[i] & escape.year$brood.year == REL$brood_year[i] & escape.year$rel.year == REL$release_year[i],]
  for(j in 1:N.mod.year){
    if(nrow(temp[temp$mod.year == j,])==1){
      E[i,j,2] <- temp$est.numb[temp$mod.year == j]
      E[i,j,3] <- temp$count[temp$mod.year == j]
      E[i,j,4] <- temp$count.zero[temp$mod.year == j]
      E[i,j,5] <- (1/temp$mean.frac.samp[temp$mod.year == j])^2
    }
    #     if(is.na(RIV$peak.month[which(RIV$ID == REL$ID[i])])==T){
    #       E[i,j,2] <- 0
    #       E[i,j,3] <- 0
    #       E[i,j,4] <- 0
    #     }      
  }
}

N.mod.month <- max(OCEAN.MODEL.AGE$model.age)
tot.escape <- data.frame(apply(E[,,2]+E[,,4],1,sum,na.rm=T))
tot.escape.trim <- tot.escape

## Replace NA with zeros in the appropriate spots
E_true <- E[,,"est.numb"]
E_true[is.na(E_true)] <- 0
E_lambda2 <- E[,,"lambda2"]
E_lambda2[is.na(E_lambda2)] <- 0
E_count <- E[,,"count"]
E_count[is.na(E_count)] <- 0

E_var <- (E_count + 1) * E_lambda2 + 1
E_sd  <- sqrt(E_var)

#### Based on the visual examination of the total freshwater escapement, I decided these sites did not have freshwater recoveries 
#### (or had obviously erratic recoveries)

# TREAT THIS AS EFFECTIVELY UNINFORMATIVE ESCACAPEMENT DATA
# E_true[THESE.no.fresh,] <- 0
# E_lambda2[THESE.no.fresh,] <- 1e10
E_true_lab <- data.frame(REL[,c("ID_numb","ID","ocean.region")],E_true)

E_true_AWG <- E_true_lab[grep("AWG",E_true_lab$ID),]




