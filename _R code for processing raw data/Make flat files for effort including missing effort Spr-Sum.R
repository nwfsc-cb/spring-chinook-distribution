
# Add in trawl fleets to gear list here.
if(TRAWL.US == "TRUE"){GEAR <- c(GEAR,"ashop","shoreside");  N.GEAR = length(GEAR)}
if(TRAWL.AK == "TRUE"){GEAR <- c(GEAR,"pollock","rockfish.AK");  N.GEAR = length(GEAR)}

### DO SOME SERIOUS THINKING ABOUT THIS SECTION.  WHAT CAN I  / SHOULD I DO HERE.
QQ <- catch.dat %>% filter(rec.year.mod <= max(YEARS.RECOVER), rec.year.mod >= min(YEARS.RECOVER),
                           brood.year <=max(YEARS.BROOD), brood.year >= min(YEARS.BROOD)) %>%
                          left_join(.,XX %>% distinct(lab,model.year,model.age) %>% arrange(model.age, model.year))

# trim fish to include only releases in REL
val <- REL %>% distinct(ID,brood_year,release_year,n.year)
val <- val %>% mutate(filt=paste(ID,brood_year,release_year,sep="_"))
QQ  <- QQ %>% mutate(filt=paste(ID,brood.year,rel.year,sep="_"))

QQ <- QQ %>% filter(filt %in% val$filt)

RR <- QQ %>% distinct(fishery.type, rec.year = rec.year.mod, lab,rec.area.code)%>% as.data.frame()
RR$indicator <- 1

ZZ <- expand.grid(rec.year=YEARS.RECOVER,lab=MONTH,rec.area.code=LOCATIONS$location.name)
ZZ$month.lab[ZZ$lab == "month.spring"] <- 1
ZZ$month.lab[ZZ$lab == "month.summer"] <- 2
ZZ$month.lab[ZZ$lab == "month.fall"]   <- 3
ZZ$month.lab[ZZ$lab == "month.winter"] <- 4

ZZ$loc.numb <- LOCATIONS$location.number[match(ZZ$rec.area.code,LOCATIONS$location.name)]

ZZ.nets   <- merge(ZZ,RR[RR$fishery.type == "Gillnet & Seine & Other",],all=T)
ZZ.troll  <- merge(ZZ,RR[RR$fishery.type == "Troll",],all=T)
ZZ.treaty <- merge(ZZ,RR[RR$fishery.type == "Treaty Troll",],all=T)
ZZ.rec    <- merge(ZZ,RR[RR$fishery.type == "Sport",],all=T)
ZZ.ashop  <- merge(ZZ,RR[RR$fishery.type == "ashop",],all=T) 
ZZ.shoreside  <- merge(ZZ,RR[RR$fishery.type == "shoreside",],all=T) 
ZZ.pollock <-  merge(ZZ,RR[RR$fishery.type == "pollock",],all=T) 
ZZ.rockfish.AK <-  merge(ZZ,RR[RR$fishery.type == "rockfish.AK",],all=T) 

ZZ.troll$indicator[is.na(ZZ.troll$indicator)==T] <- 0
ZZ.treaty$indicator[is.na(ZZ.treaty$indicator)==T] <- 0
ZZ.rec$indicator[is.na(ZZ.rec$indicator)==T] <- 0
ZZ.nets$indicator[is.na(ZZ.nets$indicator)==T] <- 0
ZZ.ashop$indicator[is.na(ZZ.ashop$indicator)==T] <- 0
ZZ.shoreside$indicator[is.na(ZZ.shoreside$indicator)==T] <- 0
ZZ.pollock$indicator[is.na(ZZ.pollock$indicator)==T] <- 0
ZZ.rockfish.AK$indicator[is.na(ZZ.rockfish.AK$indicator)==T] <- 0

#make flat files.  These simply indicate whether there were CWT fish recovered in this area-time combination or not. (1 or 0)
A_troll_flat  <-  pivot_wider(ZZ.troll,id_cols=c("rec.year","lab","month.lab"),
                                       names_from="loc.numb",values_from="indicator") %>% 
                              arrange(rec.year,month.lab) %>% as.data.frame()
A_treaty_flat <-  pivot_wider(ZZ.treaty,id_cols=c("rec.year","lab","month.lab"),
                              names_from="loc.numb",values_from="indicator") %>% 
                      arrange(rec.year,month.lab) %>% as.data.frame()
A_rec_flat    <-  pivot_wider(ZZ.rec,id_cols=c("rec.year","lab","month.lab"),
                              names_from="loc.numb",values_from="indicator") %>% 
                      arrange(rec.year,month.lab) %>% as.data.frame()
A_hake_ashop_flat <-  pivot_wider(ZZ.ashop,id_cols=c("rec.year","lab","month.lab"),
                                         names_from="loc.numb",values_from="indicator") %>% 
                            arrange(rec.year,month.lab) %>% as.data.frame()
A_hake_shoreside_flat <- pivot_wider(ZZ.shoreside,id_cols=c("rec.year","lab","month.lab"),
                                     names_from="loc.numb",values_from="indicator") %>% 
                            arrange(rec.year,month.lab) %>% as.data.frame()
A_pollock_shoreside_flat <-  pivot_wider(ZZ.pollock,id_cols=c("rec.year","lab","month.lab"),
                                                  names_from="loc.numb",values_from="indicator") %>% 
                              arrange(rec.year,month.lab) %>% as.data.frame()
A_rockfish_AK_shoreside_flat <-    pivot_wider(ZZ.rockfish.AK,id_cols=c("rec.year","lab","month.lab"),
                                                  names_from="loc.numb",values_from="indicator") %>% 
                                      arrange(rec.year,month.lab) %>% as.data.frame()

# Start model in spring so drop first winter row
first.year = min(YEARS.RECOVER)
last.year=max(YEARS.RECOVER)
last.season="month.spring"
last.row = which(A_troll_flat$rec.year == last.year & A_troll_flat$lab==last.season) 

if(MONTH.STRUCTURE=="FOUR"){
  first.season = "month.spring"
  first.row = which(A_troll_flat$rec.year == first.year & A_troll_flat$lab==first.season) 
  last.season="month.fall"
  last.row = which(A_troll_flat$rec.year == last.year & A_troll_flat$lab==last.season) 
}
if(MONTH.STRUCTURE=="FRAM"){
  first.season = "month.summer"
  first.row = which(A_troll_flat$rec.year == first.year & A_troll_flat$lab==first.season)
  last.season="month.fall"
  last.row = which(A_troll_flat$rec.year == last.year & A_troll_flat$lab==last.season) 
}
if(MONTH.STRUCTURE=="SPRING"){
  first.season = "month.spring"
  first.row = which(A_troll_flat$rec.year == first.year & A_troll_flat$lab==first.season)
  last.season="month.spring"
  last.row = which(A_troll_flat$rec.year == last.year & A_troll_flat$lab==last.season) 
}

A_troll_flat   <- A_troll_flat[first.row:last.row,]
A_rec_flat     <- A_rec_flat[first.row:last.row,]
A_treaty_flat  <- A_treaty_flat[first.row:last.row,]
#A_net_flat     <- A_net_flat[first.row:last.row,]
A_hake_ashop_flat     <- A_hake_ashop_flat[first.row:last.row,]
A_hake_shoreside_flat <- A_hake_shoreside_flat[first.row:last.row,]
A_pollock_shoreside_flat     <- A_pollock_shoreside_flat[first.row:last.row,]
A_rockfish_AK_shoreside_flat <- A_rockfish_AK_shoreside_flat[first.row:last.row,]

rownames(A_troll_flat)  <- paste(A_troll_flat$rec.year,A_troll_flat$lab,sep=".")
rownames(A_rec_flat)    <- paste(A_rec_flat$rec.year,A_rec_flat$lab,sep=".")
rownames(A_treaty_flat) <- paste(A_treaty_flat$rec.year,A_treaty_flat$lab,sep=".")
#rownames(A_net_flat)    <- paste(A_net_flat$rec.year,A_net_flat$lab,sep=".")
rownames(A_hake_ashop_flat)     <- paste(A_hake_ashop_flat$rec.year,A_hake_ashop_flat$lab,sep=".")
rownames(A_hake_shoreside_flat) <- paste(A_hake_shoreside_flat$rec.year,A_hake_shoreside_flat$lab,sep=".")
rownames(A_pollock_shoreside_flat)     <- paste(A_pollock_shoreside_flat$rec.year,A_pollock_shoreside_flat$lab,sep=".")
rownames(A_rockfish_AK_shoreside_flat) <- paste(A_rockfish_AK_shoreside_flat$rec.year,A_rockfish_AK_shoreside_flat$lab,sep=".")

col.lab <- paste("loc.",nom,sep="")

### Compile the effort files and create a flat file for each gear type
# Add a 2D file type for all of the effort and fishing mortality types
base_mat <- matrix(0,N_years_recover * N_month,N.LOC)
K_troll_flat    <- base_mat
K_rec_flat      <- base_mat
K_rec_can_flat  <- base_mat
K_rec_can_irec_flat  <- base_mat
K_treaty_flat   <- base_mat
K_hake_ashop_flat    <- base_mat
K_hake_shoreside_flat<- base_mat
K_hake_ashop_flat    <- base_mat
K_hake_shoreside_flat<- base_mat
K_pollock_shoreside_flat    <- base_mat
K_rockfish_AK_shoreside_flat<- base_mat

row.lab <- paste(sort(rep(YEARS.RECOVER,N_month)),rep(MONTH.reorder,N_years_recover),sep=".")
#row.lab <- row.lab[c(2:length(row.lab))]

for(i in 1:N.LOC){
  K_troll_flat[,i]    <- c(t(K_troll[,,i]))
  K_rec_flat[,i]      <- c(t(K_rec[,,i]))
  K_rec_can_flat[,i]  <- c(t(K_rec_can[,,i]))
  K_rec_can_irec_flat[,i] <- c(t(K_rec_can_irec[,,i]))
  K_treaty_flat[,i]    <- c(t(K_treaty[,,i]))
  K_hake_ashop_flat[,i]     <- c(t(K_hake_ashop[,,i]))
  K_hake_shoreside_flat[,i] <- c(t(K_hake_shoreside[,,i]))
  K_pollock_shoreside_flat[,i]     <- c(t(K_pollock_shoreside[,,i]))
  K_rockfish_AK_shoreside_flat[,i] <- c(t(K_rockfish_AK_shoreside[,,i]))
}

K_rec_PUSO_flat <- base_mat 
K_rec_PUSO_flat[,c(grep("PUSO",LOCATIONS$location.name))] <- K_rec_flat[,c(grep("PUSO",LOCATIONS$location.name))]
K_rec_flat[,c(grep("PUSO",LOCATIONS$location.name))] <-0

###########################################################
###########################################################
##### Add labels back in.
###########################################################

# K_troll_flat   <- K_troll_flat[c(2:nrow(K_troll_flat)),]
# K_rec_flat     <- K_rec_flat[c(2:nrow(K_rec_flat)),] 
# K_rec_can_flat <- K_rec_can_flat[c(2:nrow(K_rec_can_flat)),]
# K_rec_can_irec_flat <- K_rec_can_irec_flat[c(2:nrow(K_rec_can_irec_flat)),]
# K_rec_PUSO_flat <-K_rec_PUSO_flat[c(2:nrow(K_rec_PUSO_flat)),]
# K_treaty_flat  <- K_treaty_flat[c(2:nrow(K_treaty_flat)),]

rownames(K_troll_flat)  <- row.lab
rownames(K_rec_flat)    <- row.lab
rownames(K_rec_can_flat)<- row.lab
rownames(K_rec_can_irec_flat)<- row.lab
rownames(K_rec_PUSO_flat)  <- row.lab
rownames(K_treaty_flat) <- row.lab
rownames(K_hake_ashop_flat) <- row.lab
rownames(K_hake_shoreside_flat) <- row.lab
rownames(K_pollock_shoreside_flat) <- row.lab
rownames(K_rockfish_AK_shoreside_flat) <- row.lab

colnames(K_troll_flat)  <- col.lab
colnames(K_rec_flat)    <- col.lab
colnames(K_rec_can_flat)<- col.lab
colnames(K_rec_can_irec_flat)<- col.lab
colnames(K_rec_PUSO_flat)<- col.lab
colnames(K_treaty_flat) <- col.lab
colnames(K_hake_ashop_flat) <- col.lab
colnames(K_hake_shoreside_flat) <- col.lab
colnames(K_pollock_shoreside_flat) <- col.lab
colnames(K_rockfish_AK_shoreside_flat) <- col.lab

# trim all flat effort files to be the same size as the recovery files and match the A_ files
first.row<- which(rownames(K_troll_flat) == paste(first.year,first.season,sep="."))
last.row<- which(rownames(K_troll_flat) == paste(last.year,last.season,sep="."))

K_troll_flat      <- K_troll_flat[first.row:last.row,]
K_rec_flat        <- K_rec_flat[first.row:last.row,]
K_rec_can_flat    <- K_rec_can_flat[first.row:last.row,]
K_rec_can_irec_flat <- K_rec_can_irec_flat[first.row:last.row,]
K_rec_PUSO_flat   <- K_rec_PUSO_flat[first.row:last.row,]
K_treaty_flat     <- K_treaty_flat[first.row:last.row,]
K_hake_ashop_flat     <- K_hake_ashop_flat[first.row:last.row,]
K_hake_shoreside_flat <- K_hake_shoreside_flat[first.row:last.row,]
K_pollock_shoreside_flat     <- K_pollock_shoreside_flat[first.row:last.row,]
K_rockfish_AK_shoreside_flat <- K_rockfish_AK_shoreside_flat[first.row:last.row,]

##########
N_season_total <- nrow(K_troll_flat)





# This is a way to break the ashop catchability into two parts - one with the foreign fleets, one with domestic.
ashop_year_break <- min(grep("1991",rownames(K_hake_ashop_flat)))
##########

###########################
# Find entries in troll file without effort data but observed catches. Compare the AA matrices with the appropriate K matrices.
K_troll_design <- K_troll_flat   
K_troll_design[K_troll_design > 0] <-1
temp <- (A_troll_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_troll_design
temp[temp<0] <-0
A_list_troll_trim <-NULL
for(i in 1:ncol(temp)){
  dat.temp <- which(temp[,i] == 1)
  A_list_troll_trim  <- rbind(A_list_troll_trim,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(A_list_troll_trim) <- c("year.month","loc.numb")
f_troll_param_idx <- A_list_troll_trim
N_f_troll_idx_param  = nrow(f_troll_param_idx)

### Enumerate which entries in the K_troll_flat > 0
f_troll_effort_idx  <- NULL
for(i in 1:ncol(K_troll_flat)){
  dat.temp <- which(K_troll_flat[,i] > 0)
  f_troll_effort_idx  <- rbind(f_troll_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
}
N_f_troll_effort_idx_param <- nrow(f_troll_effort_idx)

### Enumerate which entries in the K_treaty_flat > 0
K_treaty_design <- K_treaty_flat
K_treaty_design[K_treaty_design > 0] <-1
temp <- (A_treaty_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_treaty_design
temp[temp<0] <-0
A_list_treaty_trim <-NULL
for(i in 1:ncol(temp)){
  dat.temp <- which(temp[,i] == 1)
  A_list_treaty_trim  <- rbind(A_list_treaty_trim,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(A_list_treaty_trim) <- c("year.month","loc.numb")
f_treaty_param_idx <- A_list_treaty_trim
N_f_treaty_idx_param  = nrow(f_treaty_param_idx)

f_treaty_effort_idx  <- NULL
for(i in 1:ncol(K_treaty_flat)){
  dat.temp <- which(K_treaty_flat[,i] > 0)
  f_treaty_effort_idx  <- rbind(f_treaty_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
}
N_f_treaty_effort_idx_param <- nrow(f_treaty_effort_idx)

# Repeat for rec files 
# this is a file for all rec data  (includes effort in different units)
K_rec_all_flat <- K_rec_flat +K_rec_can_flat + K_rec_PUSO_flat+ K_rec_can_irec_flat

K_rec_design <- K_rec_flat + K_rec_PUSO_flat + K_rec_can_flat + K_rec_can_irec_flat
K_rec_design[K_rec_design > 0] <-1
temp <- (A_rec_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_rec_design
temp[temp<0] <-0
A_list_rec_trim <-NULL
for(i in 1:ncol(temp)){ # Remember that you deal with PUSO in a separate file effort matrix entirely.
  dat.temp <- which(temp[,i] == 1)
  A_list_rec_trim  <- rbind(A_list_rec_trim,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(A_list_rec_trim) <- c("year.month","loc.numb")

f_rec_param_idx <- A_list_rec_trim
N_f_rec_idx_param  = nrow(f_rec_param_idx)

### Enumerate which entries in the K_rec_can_flat > 0 and where K_rec_can_irec_flat > 0
f_rec_overlap_effort_idx  <- NULL
K_temp1 <- K_rec_can_flat ; K_temp1[K_temp1>0] <- 1
K_temp2 <- K_rec_can_irec_flat; K_temp2[K_temp2>0] <- 1 
K_temp <- K_temp1 + K_temp2

for(i in 1:ncol(K_temp)){
  dat.temp <- which(K_temp[,i] ==2)
  f_rec_overlap_effort_idx  <- rbind(f_rec_overlap_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(f_rec_overlap_effort_idx) <- c("year.month","loc.numb")
N_f_rec_overlap_effort_idx_param <- nrow(f_rec_overlap_effort_idx)

### Enumerate which entries in the K_rec_flat > 0 and where K_rec_can_flat >0
f_rec_effort_idx  <- NULL
K_temp <- K_rec_flat + K_rec_PUSO_flat
for(i in 1:ncol(K_temp)){
  dat.temp <- which(K_temp[,i] > 0)
  f_rec_effort_idx  <- rbind(f_rec_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(f_rec_effort_idx) <- c("year.month","loc.numb")
N_f_rec_effort_idx_param <- nrow(f_rec_effort_idx)

f_rec_can_effort_idx  <- NULL
for(i in 1:ncol(K_rec_flat)){
  dat.temp <- which(K_rec_can_flat[,i] > 0)
  f_rec_can_effort_idx  <- rbind(f_rec_can_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
}
N_f_rec_can_effort_idx_param <- nrow(f_rec_can_effort_idx)

### Enumerate which entries in the K_hake_ashop_flat > 0
K_hake_ashop_design <- K_hake_ashop_flat
K_hake_ashop_design[K_hake_ashop_design > 0] <-1
early.rows =  max(which(A_hake_ashop_flat$rec.year==TRIM.ASHOP))
A_hake_ashop_flat[1:early.rows,4:ncol(A_hake_ashop_flat)] <- 0
temp <- (A_hake_ashop_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_hake_ashop_design
temp[temp<0] <-0

A_list_hake_ashop_trim <-NULL
for(i in 1:ncol(temp)){
  dat.temp <- which(temp[,i] == 1)
  A_list_hake_ashop_trim  <- rbind(A_list_hake_ashop_trim,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(A_list_hake_ashop_trim) <- c("year.month","loc.numb")
f_hake_ashop_param_idx <- A_list_hake_ashop_trim
N_f_hake_ashop_idx_param  = nrow(f_hake_ashop_param_idx)

# These are helper files for a feature that I no longer use
# f_hake_ashop_effort_idx  <- NULL
# for(i in 1:ncol(K_hake_ashop_flat)){
#   dat.temp <- which(K_hake_ashop_flat[,i] > 0)
#   f_hake_ashop_effort_idx  <- rbind(f_hake_ashop_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
# }
# N_f_hake_ashop_effort_idx_param <- nrow(f_hake_ashop_effort_idx)

### Enumerate which entries in the K_hake_shoreside_flat > 0
K_hake_shoreside_design <- K_hake_shoreside_flat
K_hake_shoreside_design[K_hake_shoreside_design > 0] <-1
temp <- (A_hake_shoreside_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_hake_shoreside_design
temp[temp<0] <-0
A_list_hake_shoreside_trim <-NULL
for(i in 1:ncol(temp)){
  dat.temp <- which(temp[,i] == 1)
  A_list_hake_shoreside_trim  <- rbind(A_list_hake_shoreside_trim,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(A_list_hake_shoreside_trim) <- c("year.month","loc.numb")
f_hake_shoreside_param_idx <- A_list_hake_shoreside_trim
N_f_hake_shoreside_idx_param  = nrow(f_hake_shoreside_param_idx)

### Enumerate which entries in the K_polock_shoreside_flat > 0
K_pollock_shoreside_design <- K_pollock_shoreside_flat 
K_pollock_shoreside_design[K_pollock_shoreside_design > 0] <-1
temp <- (A_pollock_shoreside_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_pollock_shoreside_design
temp[temp<0] <-0
A_list_pollock_shoreside_trim <-NULL
for(i in 1:ncol(temp)){
  dat.temp <- which(temp[,i] == 1)
  A_list_pollock_shoreside_trim  <- rbind(A_list_pollock_shoreside_trim,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(A_list_pollock_shoreside_trim) <- c("year.month","loc.numb")
f_pollock_shoreside_param_idx <- A_list_pollock_shoreside_trim
N_f_pollock_shoreside_idx_param  = nrow(f_pollock_shoreside_param_idx)

### Enumerate which entries in the K_polock_shoreside_flat > 0
K_rockfish_AK_shoreside_design <- K_rockfish_AK_shoreside_flat 
K_rockfish_AK_shoreside_design[K_rockfish_AK_shoreside_design > 0] <-1
temp <- (A_rockfish_AK_shoreside_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_rockfish_AK_shoreside_design
temp[temp<0] <-0
A_list_rockfish_AK_shoreside_trim <-NULL
for(i in 1:ncol(temp)){
  dat.temp <- which(temp[,i] == 1)
  A_list_rockfish_AK_shoreside_trim  <- rbind(A_list_rockfish_AK_shoreside_trim,cbind(dat.temp,rep(i,length(dat.temp))))
}
colnames(A_list_rockfish_AK_shoreside_trim) <- c("year.month","loc.numb")
f_rockfish_AK_shoreside_param_idx <- A_list_rockfish_AK_shoreside_trim
N_f_rockfish_AK_shoreside_idx_param  = nrow(f_rockfish_AK_shoreside_param_idx)



# AT PRESENT THERE ARE NO LOCATIONS & TIMES IN WHICH HAVE OBSERVATIONS BUT NO EFFORT.  
# For these fleets:
  # Treaty Troll
  # Hake Shoreside,
  # Hake ASHOP
  # Pollock Shoreside
  
# CURRENTLY, DON"T INCLUDE IT IN THE MODEL.













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
# QQ <- ocean.recover$dat %>% filter(rec.year <= max(YEARS.RECOVER), brood.year <= max(YEARS.BROOD)) %>%
#   group_by(fishery.type, rec.year, rec.month,rec.area.code,model.age) %>% summarise(NUMB=sum(est.numb))
# QQ <- merge(XX[,c("lab","model.age")],QQ)
# 
# if(MONTH.STRUCTURE=="FOUR"){
#   QQ$rec.year[QQ$lab=="month.winter" & QQ$rec.month <= 3] <- QQ$rec.year[QQ$lab=="month.winter" & QQ$rec.month <=3] -1
# }
# if(MONTH.STRUCTURE=="FRAM"){
#   QQ$rec.year[QQ$lab=="month.winter" & QQ$rec.month <= 1] <- QQ$rec.year[QQ$lab=="month.winter" & QQ$rec.month <=1] -1
# }
# 
# RR <- QQ %>% group_by(fishery.type, rec.year, lab,rec.area.code) %>% summarise(N=sum(NUMB)) %>% as.data.frame()
# RR$indicator <- 1
# 
# ZZ <- expand.grid(rec.year=YEARS.RECOVER,lab=MONTH,rec.area.code=LOCATIONS$location.name)
# ZZ$month.lab[ZZ$lab == "month.spring"] <- 1
# ZZ$month.lab[ZZ$lab == "month.summer"] <- 2
# ZZ$month.lab[ZZ$lab == "month.fall"]   <- 3
# ZZ$month.lab[ZZ$lab == "month.winter"] <- 4
# 
# ZZ$loc.numb <- LOCATIONS$location.number[match(ZZ$rec.area.code,LOCATIONS$location.name)]
# 
# ZZ.nets   <- merge(ZZ,RR[RR$fishery.type == "Gillnet & Seine & Other",],all=T)
# ZZ.troll  <- merge(ZZ,RR[RR$fishery.type == "Troll",],all=T)
# ZZ.treaty <- merge(ZZ,RR[RR$fishery.type == "Treaty Troll",],all=T)
# ZZ.rec    <- merge(ZZ,RR[RR$fishery.type == "Sport",],all=T)
# ZZ.ashop  <- merge(ZZ,RR[RR$fishery.type == "ashop",],all=T) 
# ZZ.shoreside  <- merge(ZZ,RR[RR$fishery.type == "shoreside",],all=T) 
# ZZ.pollock <-  merge(ZZ,RR[RR$fishery.type == "pollock",],all=T) 
# ZZ.rockfish.AK <-  merge(ZZ,RR[RR$fishery.type == "rockfish.AK",],all=T) 
# 
# ZZ.nets   <- ZZ.nets[is.na(match(ZZ.nets$rec.year,YEARS.RECOVER))==F,]
# ZZ.troll  <- ZZ.troll[is.na(match(ZZ.troll$rec.year,YEARS.RECOVER))==F,]
# ZZ.treaty <- ZZ.treaty[is.na(match(ZZ.treaty$rec.year,YEARS.RECOVER))==F,]
# ZZ.rec    <- ZZ.rec[is.na(match(ZZ.rec$rec.year,YEARS.RECOVER))==F,]
# ZZ.ashop     <- ZZ.ashop[is.na(match(ZZ.ashop$rec.year,YEARS.RECOVER))==F,]
# ZZ.shoreside <- ZZ.shoreside[is.na(match(ZZ.shoreside$rec.year,YEARS.RECOVER))==F,]
# 
# ZZ.troll  <- ZZ.troll[order(ZZ.troll$rec.year,ZZ.troll$month.lab,ZZ.troll$loc.numb),]
# ZZ.treaty <- ZZ.treaty[order(ZZ.treaty$rec.year,ZZ.treaty$month.lab,ZZ.treaty$loc.numb),]
# ZZ.rec    <- ZZ.rec[order(ZZ.rec$rec.year,ZZ.rec$month.lab,ZZ.rec$loc.numb),]
# ZZ.nets   <- ZZ.nets[order(ZZ.nets$rec.year,ZZ.nets$month.lab,ZZ.nets$loc.numb),]
# ZZ.ashop  <- ZZ.ashop[order(ZZ.ashop$rec.year,ZZ.ashop$month.lab,ZZ.ashop$loc.numb),]
# ZZ.shoreside   <- ZZ.shoreside[order(ZZ.shoreside$rec.year,ZZ.shoreside$month.lab,ZZ.shoreside$loc.numb),]
# 
# ZZ.troll$indicator[is.na(ZZ.troll$indicator)==T] <- 0
# ZZ.treaty$indicator[is.na(ZZ.treaty$indicator)==T] <- 0
# ZZ.rec$indicator[is.na(ZZ.rec$indicator)==T] <- 0
# ZZ.nets$indicator[is.na(ZZ.nets$indicator)==T] <- 0
# ZZ.ashop$indicator[is.na(ZZ.ashop$indicator)==T] <- 0
# ZZ.shoreside$indicator[is.na(ZZ.shoreside$indicator)==T] <- 0
# 
# #make flat files.  These simply indicate whether there were CWT fish recovered in this area-time combination or not. (1 or 0)
# A_troll_flat  <-  dcast(ZZ.troll,rec.year+lab+month.lab~loc.numb,value.var = "indicator") %>% arrange(rec.year,month.lab)
# A_treaty_flat <-  dcast(ZZ.treaty,rec.year+lab+month.lab~loc.numb,value.var = "indicator") %>% arrange(rec.year,month.lab)
# A_rec_flat    <-  dcast(ZZ.rec,rec.year+lab+month.lab~loc.numb,value.var = "indicator") %>% arrange(rec.year,month.lab)
# A_hake_ashop_flat        <-  dcast(ZZ.ashop,rec.year+lab+month.lab~loc.numb,value.var = "indicator") %>% arrange(rec.year,month.lab)
# A_hake_shoreside_flat    <-  dcast(ZZ.shoreside,rec.year+lab+month.lab~loc.numb,value.var = "indicator") %>% arrange(rec.year,month.lab)
# 
# # A_net_flat    <-  dcast(ZZ.nets[ZZ.nets$rec.area.code !="CookInW" & ZZ.nets$rec.area.code !="PWS", ], 
# #                        rec.year+lab+month.lab~loc.numb,value.var = "indicator") %>% arrange(rec.year,month.lab)
# # Start model in spring so drop first winter row
# first.year = min(YEARS.RECOVER)
# last.year=max(YEARS.RECOVER)
# last.season="month.fall"
# last.row = which(A_troll_flat$rec.year == last.year & A_troll_flat$lab==last.season) 
# 
# if(MONTH.STRUCTURE=="FOUR"){
#   first.season = "month.spring"
#   first.row = which(A_troll_flat$rec.year == first.year & A_troll_flat$lab==first.season) 
# }
# if(MONTH.STRUCTURE=="FRAM"){
#   first.season = "month.summer"
#   first.row = which(A_troll_flat$rec.year == first.year & A_troll_flat$lab==first.season) 
# }
# A_troll_flat   <- A_troll_flat[first.row:last.row,]
# A_rec_flat     <- A_rec_flat[first.row:last.row,]
# A_treaty_flat  <- A_treaty_flat[first.row:last.row,]
# #A_net_flat     <- A_net_flat[first.row:last.row,]
# A_hake_ashop_flat     <- A_hake_ashop_flat[first.row:last.row,]
# A_hake_shoreside_flat <- A_hake_shoreside_flat[first.row:last.row,]
# 
# rownames(A_troll_flat)  <- paste(A_troll_flat$rec.year,A_troll_flat$lab,sep=".")
# rownames(A_rec_flat)    <- paste(A_rec_flat$rec.year,A_rec_flat$lab,sep=".")
# rownames(A_treaty_flat) <- paste(A_treaty_flat$rec.year,A_treaty_flat$lab,sep=".")
# #rownames(A_net_flat)    <- paste(A_net_flat$rec.year,A_net_flat$lab,sep=".")
# rownames(A_hake_ashop_flat)     <- paste(A_hake_ashop_flat$rec.year,A_hake_ashop_flat$lab,sep=".")
# rownames(A_hake_shoreside_flat) <- paste(A_hake_shoreside_flat$rec.year,A_hake_shoreside_flat$lab,sep=".")
# 
# col.lab <- paste("loc.",nom,sep="")
# 
# # colnames(A_troll_flat)  <- paste(A_troll_flat$rec.year,A_troll_flat$lab,sep=".")
# # colnames(A_rec_flat)    <- paste(A_rec_flat$rec.year,A_rec_flat$lab,sep=".")
# # colnames(A_treaty_flat) <- paste(A_treaty_flat$rec.year,A_treaty_flat$lab,sep=".")
# # colnames(A_net_flat)    <- paste(A_net_flat$rec.year,A_net_flat$lab,sep=".")
# # 
# # A_troll_flat  <- A_troll_flat %>% dplyr::select(-rec.year,-lab,-month.lab)
# # A_rec_flat    <- A_rec_flat %>% dplyr::select(-rec.year,-lab,-month.lab)
# # A_treaty_flat <- A_treaty_flat %>% dplyr::select(-rec.year,-lab,-month.lab)
# # A_net_flat    <- A_net_flat %>% dplyr::select(-rec.year,-lab,-month.lab)
# 
# 
# ### Compile the effort files and create a flat file for each gear type
# # Add a 2D file type for all of the effort and fishing mortality types
# base_mat <- matrix(0,N_years_recover * N_month,N.LOC)
# K_troll_flat    <- base_mat
# K_rec_flat      <- base_mat
# K_rec_can_flat  <- base_mat
# K_rec_can_irec_flat  <- base_mat
# K_treaty_flat   <- base_mat
# K_hake_ashop_flat    <- base_mat
# K_hake_shoreside_flat<- base_mat
# 
# row.lab <- paste(sort(rep(YEARS.RECOVER,N_month)),rep(MONTH.reorder,N_years_recover),sep=".")
# #row.lab <- row.lab[c(2:length(row.lab))]
# 
# for(i in 1:N.LOC){
#   K_troll_flat[,i]    <- c(t(K_troll[,,i]))
#   K_rec_flat[,i]      <- c(t(K_rec[,,i]))
#   K_rec_can_flat[,i]  <- c(t(K_rec_can[,,i]))
#   K_rec_can_irec_flat[,i] <- c(t(K_rec_can_irec[,,i]))
#   K_treaty_flat[,i]    <- c(t(K_treaty[,,i]))
#   K_hake_ashop_flat[,i]     <- c(t(K_hake_ashop[,,i]))
#   K_hake_shoreside_flat[,i] <- c(t(K_hake_shoreside[,,i]))
# }
# 
# K_rec_PUSO_flat <- base_mat 
# K_rec_PUSO_flat[,c(grep("PUSO",LOCATIONS$location.name))] <- K_rec_flat[,c(grep("PUSO",LOCATIONS$location.name))]
# K_rec_flat[,c(grep("PUSO",LOCATIONS$location.name))] <-0
# 
# ###########################################################
# ###########################################################
# ##### MODIFY K_REC to try and eliminate CBC no effort
# ###########################################################
# ###########################################################
# ###########################################################
# #K_rec_can_flat[,c(grep("CBC",LOCATIONS$location.name))] <- K_rec_can_flat[,c(grep("NWVI",LOCATIONS$location.name))]
# ###########################################################
# ###########################################################
# ###########################################################
# ###########################################################
# ###########################################################
# 
# # K_troll_flat   <- K_troll_flat[c(2:nrow(K_troll_flat)),]
# # K_rec_flat     <- K_rec_flat[c(2:nrow(K_rec_flat)),] 
# # K_rec_can_flat <- K_rec_can_flat[c(2:nrow(K_rec_can_flat)),]
# # K_rec_can_irec_flat <- K_rec_can_irec_flat[c(2:nrow(K_rec_can_irec_flat)),]
# # K_rec_PUSO_flat <-K_rec_PUSO_flat[c(2:nrow(K_rec_PUSO_flat)),]
# # K_treaty_flat  <- K_treaty_flat[c(2:nrow(K_treaty_flat)),]
# 
# rownames(K_troll_flat)  <- row.lab
# rownames(K_rec_flat)    <- row.lab
# rownames(K_rec_can_flat)<- row.lab
# rownames(K_rec_can_irec_flat)<- row.lab
# rownames(K_rec_PUSO_flat)  <- row.lab
# rownames(K_treaty_flat) <- row.lab
# rownames(K_hake_ashop_flat) <- row.lab
# rownames(K_hake_shoreside_flat) <- row.lab
# 
# colnames(K_troll_flat)  <- col.lab
# colnames(K_rec_flat)    <- col.lab
# colnames(K_rec_can_flat)<- col.lab
# colnames(K_rec_can_irec_flat)<- col.lab
# colnames(K_rec_PUSO_flat)<- col.lab
# colnames(K_treaty_flat) <- col.lab
# colnames(K_hake_ashop_flat) <- col.lab
# colnames(K_hake_shoreside_flat) <- col.lab
# 
# # trim all flat effort files to be the same size as the recovery files and match the A_ files
# first.row<- which(rownames(K_troll_flat) == paste(first.year,first.season,sep="."))
# last.row<- which(rownames(K_troll_flat) == paste(last.year,last.season,sep="."))
# 
# K_troll_flat      <- K_troll_flat[first.row:last.row,]
# K_rec_flat        <- K_rec_flat[first.row:last.row,]
# K_rec_can_flat    <- K_rec_can_flat[first.row:last.row,]
# K_rec_can_irec_flat <- K_rec_can_irec_flat[first.row:last.row,]
# K_rec_PUSO_flat   <- K_rec_PUSO_flat[first.row:last.row,]
# K_treaty_flat     <- K_treaty_flat[first.row:last.row,]
# K_hake_ashop_flat     <- K_hake_ashop_flat[first.row:last.row,]
# K_hake_shoreside_flat <- K_hake_shoreside_flat[first.row:last.row,]
# 
# ##########
# N_season_total <- nrow(K_troll_flat)
# 
# # This is a way to break the ashop catchability into two parts - one with the foreign fleets, one with domestic.
# ashop_year_break <- min(grep("1991",rownames(K_hake_ashop_flat)))
# ##########
# 
# ###########################
# # Find entries in troll file without effort data but observed catches. Compare the AA matrices with the appropriate K matrices.
# K_troll_design <- K_troll_flat   
# K_troll_design[K_troll_design > 0] <-1
# temp <- (A_troll_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_troll_design
# temp[temp<0] <-0
# A_list_troll_trim <-NULL
# for(i in 1:ncol(temp)){
#   dat.temp <- which(temp[,i] == 1)
#   A_list_troll_trim  <- rbind(A_list_troll_trim,cbind(dat.temp,rep(i,length(dat.temp))))
# }
# colnames(A_list_troll_trim) <- c("year.month","loc.numb")
# f_troll_param_idx <- A_list_troll_trim
# N_f_troll_idx_param  = nrow(f_troll_param_idx)
# 
# ### Enumerate which entries in the K_troll_flat > 0
# f_troll_effort_idx  <- NULL
# for(i in 1:ncol(K_troll_flat)){
#   dat.temp <- which(K_troll_flat[,i] > 0)
#   f_troll_effort_idx  <- rbind(f_troll_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
# }
# N_f_troll_effort_idx_param <- nrow(f_troll_effort_idx)
# 
# ### Enumerate which entries in the K_treaty_flat > 0
# K_treaty_design <- K_treaty_flat
# K_treaty_design[K_treaty_design > 0] <-1
# temp <- (A_treaty_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_treaty_design
# temp[temp<0] <-0
# A_list_treaty_trim <-NULL
# for(i in 1:ncol(temp)){
#   dat.temp <- which(temp[,i] == 1)
#   A_list_treaty_trim  <- rbind(A_list_treaty_trim,cbind(dat.temp,rep(i,length(dat.temp))))
# }
# colnames(A_list_treaty_trim) <- c("year.month","loc.numb")
# f_treaty_param_idx <- A_list_treaty_trim
# N_f_treaty_idx_param  = nrow(f_treaty_param_idx)
# 
# f_treaty_effort_idx  <- NULL
# for(i in 1:ncol(K_treaty_flat)){
#   dat.temp <- which(K_treaty_flat[,i] > 0)
#   f_treaty_effort_idx  <- rbind(f_treaty_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
# }
# N_f_treaty_effort_idx_param <- nrow(f_treaty_effort_idx)
# 
# # Repeat for rec files 
# # this is a file for all rec data  (includes effort in different units)
# K_rec_all_flat <- K_rec_flat +K_rec_can_flat + K_rec_PUSO_flat+ K_rec_can_irec_flat
# 
# K_rec_design <- K_rec_flat + K_rec_PUSO_flat + K_rec_can_flat + K_rec_can_irec_flat
# K_rec_design[K_rec_design > 0] <-1
# temp <- (A_rec_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_rec_design
# temp[temp<0] <-0
# A_list_rec_trim <-NULL
# for(i in 1:ncol(temp)){ # Remember that you deal with PUSO in a separate file effort matrix entirely.
#   dat.temp <- which(temp[,i] == 1)
#   A_list_rec_trim  <- rbind(A_list_rec_trim,cbind(dat.temp,rep(i,length(dat.temp))))
# }
# colnames(A_list_rec_trim) <- c("year.month","loc.numb")
# 
# f_rec_param_idx <- A_list_rec_trim
# N_f_rec_idx_param  = nrow(f_rec_param_idx)
# 
# ### Enumerate which entries in the K_rec_can_flat > 0 and where K_rec_can_irec_flat >0
# f_rec_overlap_effort_idx  <- NULL
# K_temp1 <- K_rec_can_flat ; K_temp1[K_temp1>0] <- 1
# K_temp2 <- K_rec_can_irec_flat; K_temp2[K_temp2>0] <- 1 
# K_temp <- K_temp1 + K_temp2
# 
# for(i in 1:ncol(K_temp)){
#   dat.temp <- which(K_temp[,i] ==2)
#   f_rec_overlap_effort_idx  <- rbind(f_rec_overlap_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
# }
# colnames(f_rec_overlap_effort_idx) <- c("year.month","loc.numb")
# N_f_rec_overlap_effort_idx_param <- nrow(f_rec_overlap_effort_idx)
# 
# ### Enumerate which entries in the K_rec_flat > 0 and where K_rec_can_flat >0
# f_rec_effort_idx  <- NULL
# K_temp <- K_rec_flat +K_rec_PUSO_flat
# for(i in 1:ncol(K_temp)){
#   dat.temp <- which(K_temp[,i] > 0)
#   f_rec_effort_idx  <- rbind(f_rec_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
# }
# N_f_rec_effort_idx_param <- nrow(f_rec_effort_idx)
# 
# f_rec_can_effort_idx  <- NULL
# for(i in 1:ncol(K_rec_flat)){
#   dat.temp <- which(K_rec_can_flat[,i] > 0)
#   f_rec_can_effort_idx  <- rbind(f_rec_can_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
# }
# N_f_rec_can_effort_idx_param <- nrow(f_rec_can_effort_idx)
# 
# ### Enumerate which entries in the K_hake_ashop_flat > 0
# K_hake_ashop_design <- K_hake_ashop_flat
# K_hake_ashop_design[K_hake_ashop_design > 0] <-1
# early.rows =  max(which(A_hake_ashop_flat$rec.year==TRIM.ASHOP))
# A_hake_ashop_flat[1:early.rows,4:ncol(A_hake_ashop_flat)] <- 0
# temp <- (A_hake_ashop_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_hake_ashop_design
# temp[temp<0] <-0
# 
# A_list_hake_ashop_trim <-NULL
# for(i in 1:ncol(temp)){
#   dat.temp <- which(temp[,i] == 1)
#   A_list_hake_ashop_trim  <- rbind(A_list_hake_ashop_trim,cbind(dat.temp,rep(i,length(dat.temp))))
# }
# colnames(A_list_hake_ashop_trim) <- c("year.month","loc.numb")
# f_hake_ashop_param_idx <- A_list_hake_ashop_trim
# N_f_hake_ashop_idx_param  = nrow(f_hake_ashop_param_idx)
# 
# # These are helper files for a feature that I no longer use
# # f_hake_ashop_effort_idx  <- NULL
# # for(i in 1:ncol(K_hake_ashop_flat)){
# #   dat.temp <- which(K_hake_ashop_flat[,i] > 0)
# #   f_hake_ashop_effort_idx  <- rbind(f_hake_ashop_effort_idx,cbind(dat.temp,rep(i,length(dat.temp))))
# # }
# # N_f_hake_ashop_effort_idx_param <- nrow(f_hake_ashop_effort_idx)
# 
# ### Enumerate which entries in the K_hake_shoreside_flat > 0
# K_hake_shoreside_design <- K_hake_shoreside_flat
# K_hake_shoreside_design[K_hake_shoreside_design > 0] <-1
# temp <- (A_hake_shoreside_flat %>% dplyr::select(-rec.year,-lab,-month.lab)) - K_hake_shoreside_design
# temp[temp<0] <-0
# A_list_hake_shoreside_trim <-NULL
# for(i in 1:ncol(temp)){
#   dat.temp <- which(temp[,i] == 1)
#   A_list_hake_shoreside_trim  <- rbind(A_list_hake_shoreside_trim,cbind(dat.temp,rep(i,length(dat.temp))))
# }
# colnames(A_list_hake_shoreside_trim) <- c("year.month","loc.numb")
# f_hake_shoreside_param_idx <- A_list_hake_shoreside_trim
# N_f_hake_shoreside_idx_param  = nrow(f_hake_shoreside_param_idx)
# 
# # AT PRESENT THERE ARE NO LOCATIONS & TIMES IN WHICH SHORESIDE HAS OBSERVATIONS BUT NO EFFORT.  DON"T INCLUDE IT IN THE MODEL.
