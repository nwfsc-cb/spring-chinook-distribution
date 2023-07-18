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


######
dat  <- fresh.recover$dat.consolidated
# Look at the distribution of FW recovery months
dat.month <- dat %>% group_by(ID,ocean.region)%>%
              summarise(min.month=min(rec.month,na.rm=T),
                        max.month=max(rec.month,na.rm=T)) %>% 
              left_join(.,REL %>% distinct(ocean.region,loc.numb)) %>%
              arrange(loc.numb)

# OK. Looking at dat.month, here are the weird things:
# 1) there are some with INF for month (e.g. Comox_sum).  Those are just data with no rec month.
# 2) most others have a min month of the late-spring / summer and a max of nov or earlier.
#     these are easy because FW recoveries fall in one recovery year.
# 3) There are others who have recoveries in dec/ jan / feb... which leads to problems with age of returns

# filter out the runs that don't have recoveries around the turn of the year.
dat.month.trim <- dat.month %>% filter(is.infinite(min.month)==F) %>%
                      filter(min.month<3 | max.month>11)
# This gets us down to 71 stocks.
# get rid of stocks that occuring only April or later or October or earlier
dat.month.trim <- dat.month.trim %>% 
                    filter(min.month < 4) %>%
                    filter(max.month > 10) 
# down to 34, dominated by Columbia river stocks and Central Valley stocks.

# Deal with this region by region.
#SFB first
sfb.dat <- dat %>% filter(ocean.region %in% c("SFB_wint","SFB_spr")) %>% 
              group_by(ID,ocean.region,rec.month,rec.year) %>%
              summarise(N=sum(est.numb))

ggplot(sfb.dat %>% filter(ocean.region=="SFB_wint")) +
  geom_point(aes(x=rec.month,y=rec.year,size=N))+
  facet_wrap(~ID)+
  scale_x_continuous(breaks=1:12)
    
 # this says that anything for the spring runs belong to runs of the previous years.
dat <- dat %>% mutate(rec.year= ifelse(ocean.region == "SFB_spr" & rec.month==1,rec.year-1,rec.year))
# Winter Runs are weird...move late year to year + 1 so they all occur in a single cal year.
dat <- dat %>% mutate(rec.year= ifelse(ocean.region == "SFB_wint" & rec.month>=11,rec.year+1,rec.year))

#sor next
sor.dat <- dat %>% filter(ocean.region %in% c("SOR_spr")) %>% 
  group_by(ID,ocean.region,rec.month,rec.year) %>%
  summarise(N=sum(est.numb))

ggplot(sor.dat) +
  geom_point(aes(x=rec.month,y=rec.year,size=log(N)))+
  facet_wrap(~ID)+
  scale_x_continuous(breaks=1:12)

# There are only two years with january recoveries for SOR.  I think there are not wrap arounds.
# Make no changes for these.

#nor next
nor.dat <- dat %>% filter(ocean.region %in% c("NOR_spr")) %>% 
  group_by(ID,ocean.region,rec.month,rec.year) %>%
  summarise(N=sum(est.numb))

ggplot(nor.dat) +
  geom_point(aes(x=rec.month,y=rec.year,size=log(N)))+
  facet_wrap(~ID)+
  scale_x_continuous(breaks=1:12)
# There is only one year with january recoveries for NOR.  I think there are not wrap arounds.
# Make no changes for these.

lcol.dat <- dat %>% filter(grepl("LCOL",ocean.region)) %>% 
  group_by(ID,ocean.region,rec.month,rec.year) %>%
  summarise(N=sum(est.numb))

ggplot(lcol.dat) +
  geom_point(aes(x=rec.month,y=rec.year,size=N))+
  facet_wrap(~ID)+
  scale_x_continuous(breaks=1:12)
# LCOL:January recoveries and Dec recoveries are very rare.  I think there are not wrap arounds.
# Make no changes for these.

mcol.dat <- dat %>% filter(grepl("MCOL",ocean.region)) %>% 
  group_by(ID,ocean.region,rec.month,rec.year) %>%
  summarise(N=sum(est.numb))

ggplot(mcol.dat) +
  geom_point(aes(x=rec.month,y=rec.year,size=N))+
  facet_wrap(~ID)+
  scale_x_continuous(breaks=1:12)
# MCOL:January recoveries and Dec recoveries are very rare.  I think there are not wrap arounds.
# Make no changes for these.

ucol.dat <- dat %>% filter(grepl("UCOL",ocean.region)) %>% 
  group_by(ID,ocean.region,rec.month,rec.year) %>%
  summarise(N=sum(est.numb))

ggplot(ucol.dat) +
  geom_point(aes(x=rec.month,y=rec.year,size=N))+
  facet_wrap(~ID)+
  scale_x_continuous(breaks=1:12)
# UCOL:January recoveries and Dec recoveries are very rare.  I think there are not wrap arounds.
# Make no changes for these.

will.dat <- dat %>% filter(grepl("WILL",ocean.region)) %>% 
  group_by(ID,ocean.region,rec.month,rec.year) %>%
  summarise(N=sum(est.numb))

ggplot(will.dat) +
  geom_point(aes(x=rec.month,y=rec.year,size=N))+
  facet_wrap(~ID)+
  scale_x_continuous(breaks=1:12)
# WILL:January recoveries and Dec recoveries are very rare.  I think there are not wrap arounds.
# Make no changes for these.

snak.dat <- dat %>% filter(grepl("SNAK",ocean.region)) %>% 
  group_by(ID,ocean.region,rec.month,rec.year) %>%
  summarise(N=sum(est.numb))

ggplot(snak.dat) +
  geom_point(aes(x=rec.month,y=rec.year,size=N))+
  facet_wrap(~ID)+
  scale_x_continuous(breaks=1:12)
# snak:January recoveries and Dec recoveries are very rare.  I think there are not wrap arounds.
# Make no changes for these.

fras_th.dat <- dat %>% filter(grepl("FRAS_TH_sum",ocean.region)) %>% 
  group_by(ID,ocean.region,rec.month,rec.year) %>%
  summarise(N=sum(est.numb))

ggplot(fras_th.dat) +
  geom_point(aes(x=rec.month,y=rec.year,size=N))+
  facet_wrap(~ID)+
  scale_x_continuous(breaks=1:12)
# fras_th:January recoveries and Dec recoveries are rare and only come from AWG data.  
# Make no changes for these.

sseak.dat <- dat %>% filter(grepl("SSEAK",ocean.region)) %>% 
  group_by(ID,ocean.region,rec.month,rec.year) %>%
  summarise(N=sum(est.numb))

ggplot(sseak.dat) +
  geom_point(aes(x=rec.month,y=rec.year,size=N))+
  facet_wrap(~ID)+
  scale_x_continuous(breaks=1:12)
# sseak:January recoveries and Dec recoveries are rare and only come from AWG data.  
# Make no changes for these.

dat$age <- dat$rec.year - dat$brood.year ##### RECOGNIZE THAT "OCEAN.YEAR" is actually just "age"

# Get rid of extra releases present in the FW data.
TMP <- REL %>% dplyr::select(ID,brood_year,release_year,ocean.region)
dat <- semi_join(dat,TMP)

# Merge in information from the REL data.frame
dat <- dat %>% 
        left_join(., REL %>%
                    dplyr::select(ID,
                                  brood.year=brood_year,
                                  rel.year=release_year,
                                  ocean.region,
                                  Median.month.release,
                                  N.released,
                                  loc.numb)) #%>%
        #filter(!ocean.region=="PWS_spr")

# trim out really old fish as data entry errors
dat <- dat %>% filter(age<=8)

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
dat.sum <- dat %>%  
            group_by(ID,
                     brood.year,
                     rel.year,
                     rec.year,
                     auxiliary,
                     ocean.region,
                     age,
                     N.released,
                     Median.month.release,
                     loc.numb) %>%
            summarise(est.numb=sum(est.numb)) %>%
            ungroup() %>%
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
p1 <- ggplot(dat.sum) +
        geom_point(aes(y=fw.prop.0,x=brood.year,color=as.factor(age)),alpha=0.5) +
        theme_bw() +
        ggtitle("No AWG only")+
        scale_y_continuous("Surv to FW (no aux)",trans="log",breaks=c(1e-6,1e-5,1e-4,1e-3,1e-2))+
        facet_wrap(~ocean.region)
p1

p2 <- ggplot(dat.sum) +
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

####
dat.sum$mod.year <- dat.sum$age - 1 #  is the new escape.dat 

##### 
#Create Escapement Matrix 
##### 
N.mod.year <- 6 

dat.E <- dat.sum %>% group_by(ID,brood.year,rel.year) %>% 
            # Drop fish that are spawning in the same year the model starts.
            # drop the small number of fish that are older than BY+7
            filter(mod.year >=1,mod.year<=N.mod.year) %>%
            # Could get rid of fish returning in the same year they are released here. 
            mutate(tot.est.numb = sum(est.numb.sum )) %>%
            dplyr::select(ID,brood.year,rel.year,rec.year,
                          ocean.region,age,est.numb.sum,
                          mod.year,tot.est.numb) %>%
            ungroup() %>%
            mutate(prop.age = est.numb.sum / tot.est.numb) 

# make an array padded with zeros
A <-left_join(expand.grid(ID_numb=1:N.REL,mod.year=1:N.mod.year),
          REL %>% dplyr::select(ID,ID_numb,
                                brood.year=brood_year,
                                rel.year=release_year))

# merge into dat.E
dat.E <- full_join(dat.E ,A) %>% 
          # Get rid of NA ID_numb in dat.
          filter(!is.na(ID_numb)) %>%
          mutate(est.numb.sum=ifelse(is.na(est.numb.sum),0,est.numb.sum)) %>%
          group_by(ID,brood.year,rel.year) %>%
          mutate(tot.est.numb=sum(est.numb.sum)) %>%
          ungroup() %>%
          mutate(prop.age=est.numb.sum / tot.est.numb) %>%
          arrange(ID_numb,mod.year)

ESCAPE <- dat.E



# E <- array(NA,dim=c(N.REL,N.mod.year,2),
#            dimnames = list(paste("rel.ID",1:N.REL,sep="."),
#                            paste0("X",1:N.mod.year),c("mod.year","est.numb")))
# for(i in 1:N.REL){
#   E[i,,1] <- 1:N.mod.year
#   temp <- dat.sum %>% filter(ID == REL$ID[i],
#                                  brood.year == REL$brood_year[i],
#                                  rel.year == REL$release_year[i])
#   
#   #  escape.year[escape.year$ID == REL$ID[i] & escape.year$brood.year == REL$brood_year[i] & escape.year$rel.year == REL$release_year[i],]
#   for(j in 1:(N.mod.year)){
#     if(nrow(temp[temp$mod.year == j,])==1){
#       E[i,j,2] <- temp$est.numb.sum[temp$mod.year == j]
# 
#     }
#   }
# }
# 
# N.mod.month <- max(OCEAN.MODEL.AGE$model.age)
# # tot.escape <- data.frame(apply(E[,,2]+E[,,4],1,sum,na.rm=T))
# # tot.escape.trim <- tot.escape
# 
# ## Replace NA with zeros in the appropriate spots
# E_true <- E[,,"est.numb"]
# E_true[is.na(E_true)] <- 0
# # E_lambda2 <- E[,,"lambda2"]
# # E_lambda2[is.na(E_lambda2)] <- 0
# # E_count <- E[,,"count"]
# # E_count[is.na(E_count)] <- 0
# # 
# # E_var <- (E_count + 1) * E_lambda2 + 1
# # E_sd  <- sqrt(E_var)
# 
# #### Based on the visual examination of the total freshwater escapement, I decided these sites did not have freshwater recoveries 
# #### (or had obviously erratic recoveries)
# 
# # TREAT THIS AS EFFECTIVELY UNINFORMATIVE ESCACAPEMENT DATA
# # E_true[THESE.no.fresh,] <- 0
# # E_lambda2[THESE.no.fresh,] <- 1e10
# E_true_lab <- data.frame(REL[,c("ID_numb","ID","ocean.region")],E_true)
# 
# #E_true_AWG <- E_true_lab[grep("AWG",E_true_lab$ID),]