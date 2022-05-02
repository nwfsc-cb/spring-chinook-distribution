#### Calculate proportions for dirichlet proportions by cohort.

## Run the "Prep raw data for CPUE analysis.R"  file before you run the following.

library(sirt)

E_prop <- E_true / rowSums(E_true)
E_prop <- data.frame(REL[,c("ID_numb","ID","ocean.region")],E_prop,Tot_N=rowSums(E_true))
E_true_lab <- data.frame(REL[,c("ID_numb","ID","ocean.region")],E_true)


# remove low sample size cohorts from consideration
MIN <- 50
E_prop<- E_prop[E_prop$Tot_N >= MIN,]
rel <- unique(E_prop$ID)

DIRI <- NULL

for(i in 1:length(rel)){
  #print(paste(i))
  temp  <- E_prop[E_prop$ID== rel[i],]
  temp  <- temp[,grep("X",colnames(temp))]
  these <- which(colSums(temp)>0)
  temp  <- temp[,these]
  
  #   temp <- temp + runif(length(temp),1e-4,2e-4)
  #   temp <- temp / rowSums(temp)
  #print(colSums(temp))
  if(nrow(temp)>1){
  #temp <- temp[,2:5]
    diri.fit <- dirichlet.mle(temp,eps = 10^(-8))

    if(ncol(temp)==6){
          DIRI <- rbind(DIRI,c(rel[i],diri.fit$alpha))  
    }
    if(length(these)==5){
      if(min(these)==2){
          alpha <- c(0.001001* diri.fit$alpha0,diri.fit$alpha)
          DIRI <- rbind(DIRI,c(rel[i],alpha))  
      }
      if(max(these)==5){
        alpha <- c(diri.fit$alpha,0.001001* diri.fit$alpha0)
        DIRI <- rbind(DIRI,c(rel[i],alpha))
      }
    }
    if(length(temp)==4){
      if(min(these)==2 & max(these)==5){
        alpha <- c(0.001001* diri.fit$alpha0,diri.fit$alpha,0.001001* diri.fit$alpha0)
        DIRI <- rbind(DIRI,c(rel[i],alpha))
      }
      if(min(these)==1 & max(these)==4){
        alpha <- c(diri.fit$alpha,0.001001* diri.fit$alpha0,0.001001* diri.fit$alpha0)
        DIRI <- rbind(DIRI,c(rel[i],alpha))
      }
      if(min(these)==3 & max(these)==6){
        alpha <- c(0.001001* diri.fit$alpha0,0.001001* diri.fit$alpha0,diri.fit$alpha)
        DIRI <- rbind(DIRI,c(rel[i],alpha))
      }
    }
    if(length(temp)==3){
      if(min(these)==2 & max(these)==4){
        alpha <- c(0.001001* diri.fit$alpha0,diri.fit$alpha,0.001001* diri.fit$alpha0,0.001001* diri.fit$alpha0)
        DIRI <- rbind(DIRI,c(rel[i],alpha))
       }
     }
   }
 }

DIRI <- data.frame(DIRI)
colnames(DIRI)[1] <- "ID" 
#DIRI$ID <- rel[DIRI$ID]
colnames(DIRI)[2:7] <- paste("age",1:6,sep=".")
agg <- aggregate(REL[,c("ID","ocean.region")], by=list("ID"=REL$ID,"ocean.region"=REL$ocean.region),length)

DIRI <- merge(DIRI,agg[,c("ID","ocean.region")],all=T)
loc <- LOCATIONS
colnames(loc) <- c("ocean.region","number")

DIRI <- merge(loc,DIRI,all=T)
DIRI$number[DIRI$ocean.region == "UPCOL"] <- 8.5
DIRI <- DIRI[is.na(DIRI$ID)==F,]
DIRI <- DIRI[order(DIRI$number),]

DIRI[,"age.1"] <- as.numeric(as.character(DIRI[,"age.1"]))
DIRI[,"age.2"] <- as.numeric(as.character(DIRI[,"age.2"]))
DIRI[,"age.3"] <- as.numeric(as.character(DIRI[,"age.3"]))
DIRI[,"age.4"] <- as.numeric(as.character(DIRI[,"age.4"]))
DIRI[,"age.5"] <- as.numeric(as.character(DIRI[,"age.5"]))
DIRI[,"age.6"] <- as.numeric(as.character(DIRI[,"age.6"]))

DIRI.prop <- data.frame(DIRI[,c("ID","ocean.region","number")],DIRI[,grep("age",colnames(DIRI))] / rowSums(DIRI[,grep("age",colnames(DIRI))]))
DIRI.prop <- DIRI.prop[order(DIRI.prop$ocean.region),]
DIRI.prop.long <- melt(DIRI.prop,id = c("ID","ocean.region","number"))

DIRI.prop.long

######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
pdf(file="/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Escapement/Derived cohort escapement.pdf",width=7,height=7)

NUMB <- sort(unique(DIRI.prop.long$number))
for(i in 1:length(NUMB)){
  temp <- DIRI.prop.long[DIRI.prop.long$number == NUMB[i],]
  NOM <- unique(temp$ID)
  p <- ggplot(temp,aes(variable,value)) +
                geom_bar(stat="identity") +
                ggtitle(paste(temp$ocean.region)) +
                scale_y_continuous(limits=c(0,1)) +
                facet_wrap(~ID,ncol=2)

  print(p)
}

dev.off()


####################################################################### 
### Calculating dirichlet for each region with equal weighting for each hatchery. 
#######################################################################

# Treat each region separately except for NOR and COR and SOR ; SWVI and NWVI (combine because they only have 1 stock each)

DIRI.region <- NULL

reg <- unique(DIRI.prop$ocean.region)
for(i in 1:length(reg)){
 
  temp <- DIRI.prop[DIRI.prop$ocean.region==reg[i],]
  if(reg[i] == "COR" |reg[i] == "NOR" |reg[i] == "SOR" ){
    temp <- DIRI.prop[DIRI.prop$ocean.region=="COR" | DIRI.prop$ocean.region=="NOR" | DIRI.prop$ocean.region=="SOR",] 
  }
  if(reg[i] == "SWVI" |reg[i] == "NWVI" ){
    temp <- DIRI.prop[DIRI.prop$ocean.region=="NWVI" | DIRI.prop$ocean.region=="SWVI",] 
  }
  
  # Get rid of locations with no escapement estimates.
  temp <- temp[temp$ID != "Stayton" & 
                 temp$ID != "Feather_small" & 
                 temp$ID != "Feather_large" & 
                 temp$ID != "Nimbus" &
                 temp$ID != "Yakima_fall" &
                 temp$ID != "Bonneville_large" &
                 temp$ID != "Elwha" &
                 temp$ID != "Nanaimo" &
                 temp$ID != "Chehalis" &
                 temp$ID != "Cowichan" &
                 temp$ID != "Conuma" 
                   ,]
  
  temp <- temp[,grep("age",colnames(temp))]
  temp[temp < 0.002] <- 0
  
  these <- which(colSums(temp)>0)
  temp  <- temp[,these]
  
  diri.fit <- dirichlet.mle(temp,eps = 10^(-8))
  alpha    <- diri.fit$alpha
  
  print(alpha)
  
  if(length(these)==6){
    DIRI.region <- rbind(DIRI.region,alpha)  
  }
  if(length(these)==5){
     if(min(these)==2){
       alpha <- c(0.001001* diri.fit$alpha0,diri.fit$alpha)
       DIRI.region <- rbind(DIRI.region,alpha)  
     }
    if(min(these)==1){
      alpha <- c(diri.fit$alpha,0.001001* diri.fit$alpha0)
      DIRI.region <- rbind(DIRI.region,alpha)  
    }
  }
    
    if(length(these)==4){
      if(min(these)==2 & max(these)==5){
        alpha <- c(0.001001* diri.fit$alpha0,diri.fit$alpha,0.001001* diri.fit$alpha0)
        DIRI.region <- rbind(DIRI.region,alpha)  
      }
   }  
}


DIRI.region <- data.frame(ocean.region = as.character(reg), DIRI.region)
DIRI.region <- merge(loc,DIRI.region,all=T)
DIRI.region$number[DIRI.region$ocean.region == "UPCOL"] <- 8.5
DIRI.region <- DIRI.region[is.na(DIRI.region$age.1)==F,]

DIRI.region <- DIRI.region[order(DIRI.region$number),]

DIRI.region.prop <- data.frame(DIRI.region[,c("ocean.region","number")],DIRI.region[,grep("age",colnames(DIRI.region))] / rowSums(DIRI.region[,grep("age",colnames(DIRI.region))]))
DIRI.region.prop <- DIRI.region.prop[order(DIRI.region.prop$number),]

DIRI.region.prop.long <- melt(DIRI.region.prop,id = c("ocean.region","number"))

###################################################################
######################################################################
pdf(file="/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Escapement/Derived cohort escapement (REGION by number).pdf",width=5,height=7)

  temp <- DIRI.region.prop.long
  p <- ggplot(temp,aes(variable,value)) +
    geom_bar(stat="identity") +
    ggtitle("All Regions") +
    scale_y_continuous(limits=c(0,0.75)) +
    facet_wrap(~number,ncol=2)
  
  print(p)
dev.off()

######################################################################
pdf(file="/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Escapement/Derived cohort escapement (REGION by name).pdf",width=5,height=7)

temp <- DIRI.region.prop.long
p <- ggplot(temp,aes(variable,value)) +
  geom_bar(stat="identity") +
  ggtitle("All Regions") +
  scale_y_continuous(limits=c(0,0.75)) +
  facet_wrap(~ocean.region,ncol=2)

print(p)
dev.off()

#####################################################################

write.csv(DIRI.region,file="/Users/ole.shelton/GitHub/Orca_Salmon/_Simulation and Analysis/Releases and Recoveries/Escape_Dirichlet_region.csv",row.names = F)

# ALP <- DIRI.region[DIRI.region$ocean.region=="COL",grep("age",colnames(DIRI.region))]
# ALP <- DIRI.region[DIRI.region$ocean.region=="COR",grep("age",colnames(DIRI.region))]
# ALP <- DIRI.region[DIRI.region$ocean.region=="SFB",grep("age",colnames(DIRI.region))]
# ALP <- DIRI.region[DIRI.region$ocean.region=="WAC",grep("age",colnames(DIRI.region))]
# 
# X<- rdirichlet(1e6,as.numeric(ALP))
# apply(X,2,sd) 



