#### Calculate proportions for dirichlet proportions by cohort.

## Run the "02_Prep effort data, combine with CWT.R"  file before you run the following.
library(sirt)

# E_true_raw <- E_true
# # Get rid of age 1.  They are never observed.
# E_true <- E_true_raw[,2:ncol(E_true_raw)]
# E_prop     <- E_true / rowSums(E_true)
# E_prop     <- data.frame(REL[,c("ID_numb","ID","ocean.region")],E_prop,Tot_N=rowSums(E_true))
# E_true_lab <- data.frame(REL[,c("ID_numb","ID","ocean.region")],E_true)





# remove low sample size cohorts from consideration
MIN <- 10
E_prop<- ESCAPE[ESCAPE$tot.est.numb >= MIN,]
E_prop <- E_prop %>% mutate(x.mod.year=paste0("X",mod.year))
rel <- unique(E_prop$ID)

DIRI <- NULL
ID_all <- paste0("X",1:N.mod.year)

# Just calculate a point estimate of maturity based on the 

E_prop <- E_prop %>% mutate(n_year= rel.year - brood.year)
# fix two CA pops that get released in the same year as their brood.
E_prop <- E_prop %>% mutate(n_year = ifelse(n_year==0,1,n_year)) %>% 
              # calculate a weight for each year based on relative cohort sample size
              group_by(ID,n_year) %>%
              mutate(tot.N = sum(est.numb.sum), W=tot.est.numb / tot.N)
            
# Calculate a weigheted mean, weighted by the total number of fish in each cohort
prop.sum <- E_prop %>% group_by(ID,n_year,mod.year) %>% 
              summarise(Mean = weighted.mean(prop.age,W))

DIRI.prop.long <- left_join(prop.sum,
                            REL %>% dplyr::select(ID,ocean.region,number=loc.numb) %>%
                                distinct(ID,ocean.region,number)) %>%
                  dplyr::select(ID,ocean.region,number,n_year,mod.year,variable=mod.year,value=Mean) %>%
                  mutate(variable=paste0("mod.year.",variable)) %>%
                  # add tiny numbers and renormalize
                  mutate(value= value+0.0001) %>% 
                  group_by(ID,ocean.region,number,n_year) %>%
                  mutate(Sum=sum(value)) %>%
                  ungroup() %>%
                  mutate(value2 = value / Sum) %>%
                  dplyr::select(-Sum,-value,value=value2)
                  
######################################################################
pdf(file=paste(base.dir,"/spring-chinook-distribution/Processed Data/Escapement/Derived cohort escapement ",RUN.TYPE," ",GROUP,"min = ",MIN,".pdf",sep=""),width=7,height=7)

AAA <- DIRI.prop.long %>% filter(is.na(value)==F) %>% 
  distinct(ocean.region,number) %>% arrange(number)
for(i in 1:nrow(AAA)){
  temp <- DIRI.prop.long %>% filter(ocean.region == AAA$ocean.region[i])
  NOM <- unique(temp$ID)
  p <- ggplot(temp,aes(variable,value)) +
    geom_bar(stat="identity") +
    ggtitle(paste(AAA$ocean.region[i])) +
    scale_y_continuous(limits=c(0,1)) +
    facet_grid(ID~n_year)
  
  print(p)
}

dev.off()


####################################################################### 
### Calculating dirichlet for each region with equal weighting for each hatchery. 
#######################################################################

# Treat each region separately except for NOR and COR and SOR ; SWVI and NWVI (combine because they only have 1 stock each)

DIRI.region <- NULL
#reg <- unique(DIRI.prop$ocean.region)

DIRI.region <- DIRI.prop.long %>% group_by(ocean.region,number,n_year,variable) %>%
  dplyr::summarise(mean.prop = mean(value,na.rm=T),N_pop = length(value)) %>% 
  pivot_wider(., id_cols = c("ocean.region","number","N_pop","n_year"),names_from="variable",values_from="mean.prop") %>%
  arrange(number)


DIRI.region.prop.long <- pivot_longer(DIRI.region,cols = starts_with("mod"))

###################################################################
######################################################################
pdf(file=paste(base.dir,"/spring-chinook-distribution/Processed Data/Escapement/Derived cohort escapement (REGION by number) ",RUN.TYPE," ",GROUP,".pdf",sep=""),width=5,height=7,onefile=TRUE)

temp <- DIRI.region.prop.long
p <- list()
p[[1]] <- ggplot(temp %>% filter(n_year==1),aes(x=name,y=value)) +
  geom_bar(stat="identity") +
  ggtitle("All Regions, n_year=1") +
  #scale_y_continuous(limits=c(0,0.75)) +
  facet_wrap(~ocean.region,ncol=5)

p[[2]] <- ggplot(temp %>% filter(n_year==2),aes(x=name,y=value)) +
  geom_bar(stat="identity") +
  ggtitle("All Regions, n_year=2") +
  #scale_y_continuous(limits=c(0,0.75)) +
  facet_wrap(~ocean.region,ncol=5)


print(p)
dev.off()

######################################################################

#MAKE RULES FOR SHARING INFORMATION HERE:

#####################################################################
# Separate into n_year =1 and n_year==2

write.csv(DIRI.region %>% filter(n_year==1),file=paste(base.dir,"/spring-chinook-distribution/Processed Data/Escapement/Escape_Dirichlet_region ",RUN.TYPE," ",GROUP,"; n_year==1.csv",sep=""),row.names = F)
write.csv(DIRI.region %>% filter(n_year==2),file=paste(base.dir,"/spring-chinook-distribution/Processed Data/Escapement/Escape_Dirichlet_region ",RUN.TYPE," ",GROUP,"; n_year==2.csv",sep=""),row.names = F)

# ALP <- DIRI.region[DIRI.region$ocean.region=="COL",grep("age",colnames(DIRI.region))]
# ALP <- DIRI.region[DIRI.region$ocean.region=="COR",grep("age",colnames(DIRI.region))]
# ALP <- DIRI.region[DIRI.region$ocean.region=="SFB",grep("age",colnames(DIRI.region))]
# ALP <- DIRI.region[DIRI.region$ocean.region=="WAC",grep("age",colnames(DIRI.region))]
# 
# X<- rdirichlet(1e6,as.numeric(ALP))
# apply(X,2,sd) 
# 
 
# 
# 
# 
# ### THIS IS THE OLD WAY
# 
# 
# 
# 
# 
# 
# for(i in 1:length(rel)){
#   #print(paste(i))
#   
#   # Sort through each hatchery release
#   temp  <- E_prop[E_prop$ID== rel[i],]
#   temp <- pivot_wider(temp,id_cols=c("ID","brood.year","rel.year"),
#                       names_from="x.mod.year",values_from = "prop.age")
#   
#   temp  <- temp[,grep("X",colnames(temp))]
#   these <- which(colSums(temp)>0)
#   temp  <- temp[,these]
#   if(length(these)>1){
#     if(nrow(temp)>1 ){
#       #temp <- temp[,2:5]
#       diri.fit <- dirichlet.mle(temp,eps = 10^(-8))
#       
#       AAA <- left_join(data.frame(id = ID_all),
#                        data.frame(id=names(diri.fit$alpha),alpha=diri.fit$alpha))
#       AAA <- AAA %>% mutate(alpha=ifelse(is.na(alpha),0.001 * diri.fit$alpha0,alpha),
#                             ID = rel[i])
#       AAA <- pivot_wider(AAA,id_cols = ID, names_from=id,values_from = alpha)
#      
#     }
#     if(nrow(temp)==1){
#       print("singleton")
#       AAA <- left_join(data.frame(id = ID_all),
#                        data.frame(id=names(temp),alpha=t(temp)))
#       colnames(AAA)[2] <- "alpha"
#       AAA <- AAA %>% mutate(alpha=ifelse(is.na(alpha),0.001,alpha),
#                             ID =rel[i])
#       AAA <- pivot_wider(AAA,id_cols = ID, names_from=id,values_from = alpha)
#     }
#   }
#   if(length(these)==1){
#     print("only one age observed")
#     AAA <- left_join(data.frame(id = ID_all),
#                      data.frame(id=names(these),alpha=1*0.995))
#     colnames(AAA)[2] <- "alpha"
#     AAA <- AAA %>% mutate(alpha=ifelse(is.na(alpha),0.001,alpha),
#                           ID =rel[i])
#     AAA <- pivot_wider(AAA,id_cols = ID, names_from=id,values_from = alpha)
#   }
#   DIRI <- bind_rows(DIRI,AAA)
# }
# 
# DIRI <- data.frame(DIRI)   
# colnames(DIRI)[2:7] <- paste("mod.year",1:N.mod.year,sep=".")
# 
# agg.rel <- REL %>% distinct(ID,ocean.region)
# 
# DIRI <- full_join( DIRI,agg.rel)
# 
# loc <- LOCATIONS
# colnames(loc) <- c("ocean.region","number")
# 
# loc.and.numb <- REL %>% group_by(ocean.region,loc.numb) %>% summarize(N=length(ocean.region)) %>% 
#   mutate(number=loc.numb) %>% dplyr::select(ocean.region,number)
# 
# DIRI <- full_join(loc.and.numb,DIRI)
# # DIRI$number[DIRI$ocean.region == "LCOL"] <- 8.1
# # DIRI$number[DIRI$ocean.region == "MCOL"] <- 8.25
# # DIRI$number[DIRI$ocean.region == "UCOL"] <- 8.5
# # DIRI$number[DIRI$ocean.region == "SNAK"] <- 8.75
# # DIRI$number[DIRI$ocean.region == "URB"] <- 8.9
# DIRI <- DIRI[is.na(DIRI$ID)==F,]
# DIRI <- DIRI[order(DIRI$number),]
# 
# # DIRI[,"age.1"] <- as.numeric(as.character(DIRI[,"age.1"]))
# # DIRI[,"age.2"] <- as.numeric(as.character(DIRI[,"age.2"]))
# # DIRI[,"age.3"] <- as.numeric(as.character(DIRI[,"age.3"]))
# # DIRI[,"age.4"] <- as.numeric(as.character(DIRI[,"age.4"]))
# # DIRI[,"age.5"] <- as.numeric(as.character(DIRI[,"age.5"]))
# # DIRI[,"age.6"] <- as.numeric(as.character(DIRI[,"age.6"]))
# 
# DIRI.prop <- data.frame(DIRI[,c("ID","ocean.region","number")],DIRI[,grep("mod.year",colnames(DIRI))] / rowSums(DIRI[,grep("mod.year",colnames(DIRI))]))
# DIRI.prop <- DIRI.prop[order(DIRI.prop$ocean.region),]
# #DIRI.prop <- DIRI.prop %>% filter(is.na(age.3)==F)
# 
# DIRI.prop.long <- as.data.table(DIRI.prop) %>% 
#   data.table::melt(.,c("ID","ocean.region","number")) %>%
#   as.data.frame()
# 
# DIRI.prop.long
# #3 Get rid of instances with not enough data to estimate.
# #DIRI.prop.long <- DIRI.prop.long 
# 
# ######################################################################
# ######################################################################
# ######################################################################
# ######################################################################
# ######################################################################
# ######################################################################
# pdf(file=paste(base.dir,"/spring-chinook-distribution/Processed Data/Escapement/Derived cohort escapement ",RUN.TYPE," ",GROUP,"min = ",MIN,".pdf",sep=""),width=7,height=7)
# 
# AAA <- DIRI.prop.long %>% filter(is.na(value)==F) %>% 
#               distinct(ocean.region,number) %>% arrange(number)
# for(i in 1:nrow(AAA)){
#   temp <- DIRI.prop.long %>% filter(ocean.region == AAA$ocean.region[i])
#   NOM <- unique(temp$ID)
#   p <- ggplot(temp,aes(variable,value)) +
#                 geom_bar(stat="identity") +
#                 ggtitle(paste(AAA$ocean.region[i])) +
#                 scale_y_continuous(limits=c(0,1)) +
#                 facet_wrap(~ID,ncol=2)
# 
#   print(p)
# }
# 
# dev.off()
# 
# 
# ####################################################################### 
# ### Calculating dirichlet for each region with equal weighting for each hatchery. 
# #######################################################################
# 
# # Treat each region separately except for NOR and COR and SOR ; SWVI and NWVI (combine because they only have 1 stock each)
# 
# DIRI.region <- NULL
# reg <- unique(DIRI.prop$ocean.region)
# 
# 
# DIRI.region <- DIRI.prop.long %>% group_by(ocean.region,number,variable) %>%
#     dplyr::summarise(mean.prop = mean(value,na.rm=T),N_pop = length(value)) %>% 
#     pivot_wider(., id_cols = c("ocean.region","number","N_pop"),names_from="variable",values_from="mean.prop") %>%
#     arrange(number)
# 
# 
# DIRI.region.prop.long <- pivot_longer(DIRI.region,cols = starts_with("mod"))
# 
# ###################################################################
# ######################################################################
# pdf(file=paste(base.dir,"/spring-chinook-distribution/Processed Data/Escapement/Derived cohort escapement (REGION by number) ",RUN.TYPE," ",GROUP,".pdf",sep=""),width=5,height=7)
# 
#   temp <- DIRI.region.prop.long
#   p <- ggplot(temp,aes(x=name,y=value)) +
#     geom_bar(stat="identity") +
#     ggtitle("All Regions") +
#     #scale_y_continuous(limits=c(0,0.75)) +
#     facet_wrap(~ocean.region,ncol=5)
#   
#   print(p)
# dev.off()
# 
# ######################################################################
# 
# #MAKE RULES FOR SHARING INFORMATION HERE:
# 
# #####################################################################
# write.csv(DIRI.region,file=paste(base.dir,"/spring-chinook-distribution/Processed Data/Escapement/Escape_Dirichlet_region ",RUN.TYPE," ",GROUP,".csv",sep=""),row.names = F)
# 
# # ALP <- DIRI.region[DIRI.region$ocean.region=="COL",grep("age",colnames(DIRI.region))]
# # ALP <- DIRI.region[DIRI.region$ocean.region=="COR",grep("age",colnames(DIRI.region))]
# # ALP <- DIRI.region[DIRI.region$ocean.region=="SFB",grep("age",colnames(DIRI.region))]
# # ALP <- DIRI.region[DIRI.region$ocean.region=="WAC",grep("age",colnames(DIRI.region))]
# # 
# # X<- rdirichlet(1e6,as.numeric(ALP))
# # apply(X,2,sd) 
# 
# 
# 
