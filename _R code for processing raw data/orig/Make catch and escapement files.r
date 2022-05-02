# Make Catch Files
### THESE ARE THE IMPORTANT ARRAY OF CATCH
C <- array(0,dim=c(N.REL,N.mod.month,N.LOC,N.GEAR),dimnames = list(paste("rel.ID",1:N.REL,sep="."),NULL,paste("loc",nom,sep="."),GEAR))
Z_catch <- array(0,dim=c(N.REL,N.mod.month,N.LOC,N.GEAR),dimnames = list(paste("rel.ID",1:N.REL,sep="."),NULL,paste("loc",nom,sep="."),GEAR))
Lambda2 <- array(0,dim=c(N.REL,N.mod.month,N.LOC,N.GEAR),dimnames = list(paste("rel.ID",1:N.REL,sep="."),NULL,paste("loc",nom,sep="."),GEAR))

## FILL THE C ARRAY WITH VALUES FROM EACH RELEASE GROUP
temp <- expand.grid(model.age=1:N.mod.month,rec.area.code=LOCATIONS$location.name)
temp$loc.numb <- LOCATIONS$location.number[match(temp$rec.area.code,LOCATIONS$location.name)]

for(k in 1:N.REL){
  print(paste(k,"of",N.REL))
  for(l in 1:N.GEAR){
    
    temp.2 <- melt(catch.dat[catch.dat$ID == REL$ID[k] & 
                               catch.dat$brood.year == REL$brood_year[k] &
                               catch.dat$rel.year == REL$release_year[k] &
                               catch.dat$fishery.type == GEAR[l], ],
                   id.var=c("model.age","rec.area.code"),measure.vars =c("est.numb"))
    temp.3 <- merge(temp,temp.2,all=T)
    temp.3 <- temp.3[order(temp.3$loc.numb,temp.3$model.age),]
    C[k,,,l] <- temp.3$value
    
    temp.2 <- melt(catch.dat[catch.dat$ID == REL$ID[k] & 
                               catch.dat$brood.year == REL$brood_year[k] &
                               catch.dat$rel.year == REL$release_year[k] &
                               catch.dat$fishery.type == GEAR[l], ],
                   id.var=c("model.age","rec.area.code"),measure.vars =c("count"))
    temp.3 <-merge(temp,temp.2,all=T)
    temp.3 <- temp.3[order(temp.3$loc.numb,temp.3$model.age),]
    Z_catch[k,,,l] <- temp.3$value
    
    
    temp.2 <- melt(catch.dat[catch.dat$ID == REL$ID[k] & 
                               catch.dat$brood.year == REL$brood_year[k] &
                               catch.dat$rel.year == REL$release_year[k] &
                               catch.dat$fishery.type == GEAR[l], ],
                   id.var=c("model.age","rec.area.code"),measure.vars =c("median.frac.samp"))
    temp.3 <-merge(temp,temp.2,all=T)
    temp.3 <- temp.3[order(temp.3$loc.numb,temp.3$model.age),]
    Lambda2[k,,,l] <- (1/temp.3$value)^2
  }
}

# These are the interesting arrays
Z_catch[is.na(C)]<-0 # Counts of individuals actually observed
Lambda2[is.na(C)]<-0 # Variance term (not used in the model at present.)
C[is.na(C)]<-0       # Expanded number of individual observed

###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
#### ESCAPEMENT 
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################

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
RIV[RIV==13]<-1
RIV[RIV== -1 ]<- 11

# Assert all fish enter river starting in August and lasting til October. 

RIV$first.month <- 8
RIV$last.month  <- 10

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
escape.dat <- merge(escape.dat,escape.dat.frac)

### Do some adjustments for fish that appear in fresh water in january, feb or march of the following year.

riv.temp <- river.entry[river.entry$rec.month <=3,]
adjust <- aggregate(riv.temp[,c("est.numb")],by=list(
  ID = riv.temp$ID, 
  brood.year = riv.temp$brood.year,
  rel.year = riv.temp$rel.year, 
  #rec.month = fresh.recover$dat$rec.month, 
  rec.year = riv.temp$rec.year, 
  ocean.region = riv.temp$ocean.region), 
  sum,na.rm=T)

adjust.add <- adjust
adjust.add$rec.year <-adjust.add$rec.year-1

escape.dat1<- merge(escape.dat,adjust.add,all=T)
escape.dat1$x[is.na(escape.dat1$x)==T] <- 0
escape.dat1$est.numb <- escape.dat1$est.numb + escape.dat1$x

escape.dat <- escape.dat1[,1:9]

escape.dat2<- merge(escape.dat,adjust,all=T)
escape.dat2$x[is.na(escape.dat2$x)==T] <- 0
escape.dat2$est.numb <- escape.dat2$est.numb - escape.dat2$x

escape.dat <- escape.dat1[,1:9]
escape.dat$est.numb[escape.dat$est.numb < 0] <- 0

#######################
#######################
# Manually inflate the variance of SFB returning fish (median frac samp = 0.3)
    # also inflate NWVI (Conuma) to 0.01
#######################
#######################
escape.dat$mean.frac.samp[escape.dat$ocean.region =="NCA"] <- 0.2
escape.dat$mean.frac.samp[escape.dat$ocean.region =="SFB"] <- 0.2
escape.dat$mean.frac.samp[escape.dat$ocean.region =="NWVI"] <- 0.01
###############################################################################################################

escape.dat$mod.year <- escape.dat$age 

# escape.dat$mod.year[escape.dat$model.age <= 7] <- 1
# escape.dat$mod.year[escape.dat$model.age >7 & escape.dat$model.age <=14 ]  <- 2
# escape.dat$mod.year[escape.dat$model.age >14 & escape.dat$model.age <=21 ] <- 3
# escape.dat$mod.year[escape.dat$model.age >21 & escape.dat$model.age <=28 ] <- 4
# escape.dat$mod.year[escape.dat$model.age >28 & escape.dat$model.age <=35 ] <- 5
# escape.dat$mod.year[escape.dat$model.age >35 & escape.dat$model.age <=42 ] <- 6
       
escape.year <- aggregate(escape.dat[,c("est.numb","count","count.zero")],by=list(ID=escape.dat$ID,mod.year=escape.dat$mod.year,brood.year=escape.dat$brood.year, rel.year=escape.dat$rel.year),sum)
escape.year.frac <- aggregate(escape.dat[,c("mean.frac.samp")],by=list(ID=escape.dat$ID,mod.year=escape.dat$mod.year,brood.year=escape.dat$brood.year,rel.year=escape.dat$rel.year),mean)
colnames(escape.year.frac)[ncol(escape.year.frac)] <- "mean.frac.samp"
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
  temp <- escape.year[escape.year$ID == REL$ID[i] & escape.year$rel.year == REL$release_year[i],]
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
  