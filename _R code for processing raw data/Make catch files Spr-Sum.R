# Trim the gears to avoid redundancy in trawl sectors.
GEAR <- GEAR[grep("Trawl",GEAR,invert=TRUE)]
N.GEAR <- length(GEAR)

# Make Catch Files
### THESE ARE THE IMPORTANT ARRAY OF CATCH
C <- array(0,dim=c(N.REL,N.mod.month,N.LOC,N.GEAR),dimnames = list(paste("rel.ID",1:N.REL,sep="."),NULL,paste("loc",nom,sep="."),GEAR))
Z_catch <- array(0,dim=c(N.REL,N.mod.month,N.LOC,N.GEAR),dimnames = list(paste("rel.ID",1:N.REL,sep="."),NULL,paste("loc",nom,sep="."),GEAR))

## FILL THE C ARRAY WITH VALUES FROM EACH RELEASE GROUP
temp <- expand.grid(model.age=1:N.mod.month,rec.area.code=LOCATIONS$location.name)
temp$loc.numb <- LOCATIONS$location.number[match(temp$rec.area.code,LOCATIONS$location.name)]

for(k in 1:N.REL){
  print(paste(k,"of",N.REL))
  for(l in 1:N.GEAR){
    
    A <- catch.dat[catch.dat$ID == REL$ID[k] & 
                     catch.dat$brood.year == REL$brood_year[k] &
                     catch.dat$rel.year == REL$release_year[k] &
                     catch.dat$fishery.type == GEAR[l], ] %>% as.data.table()
    temp.2 <- data.table::melt(A,
                   id.var=c("model.age","rec.area.code"),measure.vars =c("est.numb")) %>%
                    as.data.frame()
    temp.3 <- merge(temp,temp.2,all=T)
    temp.3 <- temp.3[order(temp.3$loc.numb,temp.3$model.age),]
    C[k,,,l] <- temp.3$value
    
    temp.2 <- data.table::melt(A,
                   id.var=c("model.age","rec.area.code"),measure.vars =c("count")) %>%
                    as.data.frame()
    temp.3 <-merge(temp,temp.2,all=T)
    temp.3 <- temp.3[order(temp.3$loc.numb,temp.3$model.age),]
    Z_catch[k,,,l] <- temp.3$value
    
  }
}

# These are the interesting arrays
Z_catch[is.na(C)]<-0 # Counts of individuals actually observed
C[is.na(C)]<-0       # Expanded number of individual observed
############################

#make some summaries




## 
if(MONTH.STRUCTURE=="FOUR"){
  samp.frac.dat <- catch.dat %>% as.data.frame()
  samp.frac.dat$season <- ""
  samp.frac.dat$season[samp.frac.dat$model.age %in% c(1,5,9,13,17)] <- "spring"
  samp.frac.dat$season[samp.frac.dat$model.age %in% c(2,6,10,14,18)] <- "summer"
  samp.frac.dat$season[samp.frac.dat$model.age %in% c(3,7,11,15,19)] <- "fall"
  samp.frac.dat$season[samp.frac.dat$model.age %in% c(4,8,12,16)] <- "winter"
}  

if(MONTH.STRUCTURE=="FRAM"){
  samp.frac.dat <- catch.dat %>% as.data.frame()
  samp.frac.dat$season <- ""
  samp.frac.dat$season[samp.frac.dat$model.age %in% c(1,5,9,13,17)] <- "summer"
  samp.frac.dat$season[samp.frac.dat$model.age %in% c(2,6,10,14,18)] <- "fall"
  samp.frac.dat$season[samp.frac.dat$model.age %in% c(3,7,11,15)] <- "winter"
  samp.frac.dat$season[samp.frac.dat$model.age %in% c(4,8,12,16)] <- "spring"
}  

if(MONTH.STRUCTURE=="SPRING"){
  samp.frac.dat <- catch.dat %>% as.data.frame()
  samp.frac.dat$season <- ""
  samp.frac.dat$season[samp.frac.dat$model.age %in% c(1,5,9,13,17,21)] <- "spring"
  samp.frac.dat$season[samp.frac.dat$model.age %in% c(2,6,10,14,18)] <- "summer"
  samp.frac.dat$season[samp.frac.dat$model.age %in% c(3,7,11,15,19)] <- "fall"
  samp.frac.dat$season[samp.frac.dat$model.age %in% c(4,8,12,16,20)] <- "winter"
}  


Lambda <- samp.frac.dat %>% group_by(rec.year.mod,season,fishery.type,rec.area.code) %>% 
                dplyr::summarize(Frac=mean(median.frac.samp),n.frac=length(median.frac.samp), sd.frac=sd(median.frac.samp),
                                 max=max(median.frac.samp),min=min(median.frac.samp)) %>%
                mutate(year.month=paste(rec.year.mod,"month",season,sep=".")) %>%
                as.data.frame()
Lambda[1:10,]  

A <- data.frame(year.month=rownames(K_troll_flat))
A$id.numb <- 1:nrow(K_troll_flat)
B <- merge(A,Lambda)
B$rec.area.code <- as.character(B$rec.area.code)

Lambda_troll_flat  <- matrix(NA,length(A$id.numb),N.LOC)
Lambda_treaty_flat <- Lambda_troll_flat
Lambda_rec_flat    <- Lambda_troll_flat
Lambda_net_flat    <- Lambda_troll_flat

for(i in 1:N.LOC){
  HERE <- LOCATIONS$location.name[LOCATIONS$location.number==i]
  THESE <- which(B$rec.area.code==HERE & B$fishery.type=="Troll")
  if(length(THESE)>0){
    Lambda_troll_flat[B$id.numb[ THESE],i] <- B$Frac[THESE]
  }
  HERE <- LOCATIONS$location.name[LOCATIONS$location.number==i]
  THESE <- which(B$rec.area.code==HERE & B$fishery.type=="Sport")
  if(length(THESE)>0){
    Lambda_rec_flat[B$id.numb[ THESE],i] <- B$Frac[THESE]
  }
  HERE <- LOCATIONS$location.name[LOCATIONS$location.number==i]
  THESE <- which(B$rec.area.code==HERE & B$fishery.type=="Treaty Troll")
  if(length(THESE)>0){
    Lambda_treaty_flat[B$id.numb[ THESE],i] <- B$Frac[THESE]
  }
  HERE <- LOCATIONS$location.name[LOCATIONS$location.number==i]
  THESE <- which(B$rec.area.code==HERE & B$fishery.type=="Gillnet & Seine & Other")
  if(length(THESE)>0){
    Lambda_net_flat[B$id.numb[ THESE],i] <- B$Frac[THESE]
  }
}

# Do something slightly different for the ashop and shoreside hake fleets.
    # See Make intermeidate recovery files CLIMATE.R for this information.
    # They should be entirely complete before they get here and shouldn't need to be touched.

colnames(Lambda_troll_flat)       <- colnames(K_troll_flat)
colnames(Lambda_rec_flat)         <- colnames(K_troll_flat)
colnames(Lambda_treaty_flat)      <- colnames(K_troll_flat)
colnames(Lambda_net_flat)         <- colnames(K_troll_flat)

rownames(Lambda_troll_flat) <- rownames(K_troll_flat)
rownames(Lambda_rec_flat)   <- rownames(K_troll_flat)
rownames(Lambda_treaty_flat) <- rownames(K_troll_flat)
rownames(Lambda_net_flat) <- rownames(K_troll_flat)

## interpolate sampling fraction for all areas that have no available sampling faction data.
Lambda_troll_flat_int    <- Lambda_troll_flat
Lambda_rec_flat_int      <- Lambda_rec_flat
Lambda_treaty_flat_int   <- Lambda_treaty_flat

# smooth over +/- 4 seasons, +/-1 area for troll and rec
N.se <- 4
N.ar <- 1

##3 TROLL
THESE <- NULL
for( i in 1:ncol(Lambda_troll_flat)){
  THESE <- which(is.na(Lambda_troll_flat[,i])==T)
  
  if(length(THESE)>0){
  for(j in 1:length(THESE)){
    start.c <- max(i-N.ar,1)
    stop.c  <- min(ncol(Lambda_troll_flat),i+N.ar)
    
    start.r <- max(THESE[j]-N.se,1)
    stop.r  <- min(THESE[j]+N.se,nrow(Lambda_troll_flat))
    
    Lambda_troll_flat_int[THESE[j],i] =   median(Lambda_troll_flat[start.r:stop.r,start.c:stop.c],na.rm=TRUE)
  } 
  }
}

##3 REC
THESE <- NULL
for( i in 1:ncol(Lambda_rec_flat)){
#print(i)
  THESE <- which(is.na(Lambda_rec_flat[,i])==T)
  if(length(THESE)>0){
    for(j in 1:length(THESE)){
    start.c <- max(i-N.ar,1)
    stop.c  <- min(ncol(Lambda_rec_flat),i+N.ar)
    
    start.r <- max(THESE[j]-N.se,1)
    stop.r  <- min(THESE[j]+N.se,nrow(Lambda_rec_flat))
    
    Lambda_rec_flat_int[THESE[j],i] =   median(Lambda_rec_flat[start.r:stop.r,start.c:stop.c],na.rm=TRUE)
  	} 
  }
}

##3 TREATY
# smooth over +/- 4 seasons, +/-2 area for troll and rec
N.se <- 4
N.ar <- 2

THESE <- NULL
for( i in 1:ncol(Lambda_treaty_flat)){
  THESE <- which(is.na(Lambda_treaty_flat[,i])==T)
  if(length(THESE)>0){
  for(j in 1:length(THESE)){
    start.c <- max(i-N.ar,1)
    stop.c  <- min(ncol(Lambda_treaty_flat),i+N.ar)
    
    start.r <- max(THESE[j]-N.se,1)
    stop.r  <- min(THESE[j]+N.se,nrow(Lambda_treaty_flat))
    
    Lambda_treaty_flat_int[THESE[j],i] =   median(Lambda_treaty_flat[start.r:stop.r,start.c:stop.c],na.rm=TRUE)
  } 
  }
}

Lambda_troll_flat_int[is.nan(Lambda_troll_flat_int)==T] <- NA
Lambda_treaty_flat_int[is.nan(Lambda_treaty_flat_int)==T] <- NA
Lambda_rec_flat_int[is.nan(Lambda_rec_flat_int)==T] <- NA 

### Resmooth the Troll file because there were a few important NAs remaining
##3 TROLL
THESE <- NULL
for( i in 1:ncol(Lambda_troll_flat_int)){
  THESE <- which(is.na(Lambda_troll_flat_int[,i])==T)
  if(length(THESE)>0){  
  for(j in 1:length(THESE)){
    start.c <- max(i-N.ar,1)
    stop.c  <- min(ncol(Lambda_troll_flat_int),i+N.ar)
    
    start.r <- max(THESE[j]-N.se,1)
    stop.r  <- min(THESE[j]+N.se,nrow(Lambda_troll_flat_int))
    
    Lambda_troll_flat_int[THESE[j],i] =   median(Lambda_troll_flat_int[start.r:stop.r,start.c:stop.c],na.rm=TRUE)
  } 
  }
}

