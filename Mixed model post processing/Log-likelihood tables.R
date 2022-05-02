# Contructing Log Likelihood tables

# Axes of interest
  # origin
  # region
  # age
  # season
  # year
  # gear
  
LOCATIONS.plot <- LOCATIONS %>%  mutate(location.name = as.character(location.name),
                                        location.name = ifelse(location.name=="PUSO_out","SJDF",location.name))
LOCATIONS.plot$location.name <- factor(LOCATIONS.plot$location.name,levels=LOCATIONS.plot$location.name) 
  
dat.ll.bern <- dat.all %>% group_by(origin=ocean.reg,location.number=location,year,time,season.idx,effort.type) %>% 
              summarise(n=length(origin),ll=sum(log.like.bern))
dat.ll.pos <- dat.pos %>% group_by(origin=ocean.reg,location.number=location,year,time,season.idx,effort.type) %>% 
                  summarise(n=length(origin),ll=sum(log.like.gamma))

dat.ll.bern <- dat.ll.bern %>% left_join(.,LOCATIONS.plot) %>% as.data.frame()
dat.ll.pos  <- dat.ll.pos %>% left_join(.,LOCATIONS.plot) %>% as.data.frame()


## BY ORIGIN
dat.origin.bern <- dat.ll.bern %>% group_by(origin) %>% summarise(LL.bern=sum(ll),N.bern=sum(n)) %>% mutate(origin=as.character(origin))
dat.origin.bern <- rbind(dat.origin.bern,
                             dat.origin.bern %>% summarise_at(c("LL.bern","N.bern"),sum) %>% 
                              unlist(.) %>% c("TOTAL",.)) %>% 
                              as_tibble()
dat.origin.bern <- dat.origin.bern %>% mutate(LL.bern=as.numeric(LL.bern), N.bern=as.numeric(N.bern)) 
                      
dat.origin.pos  <- dat.ll.pos %>% group_by(origin) %>% summarise(LL.pos=sum(ll),N.pos=sum(n)) %>% mutate(origin=as.character(origin))
dat.origin.pos <- rbind(dat.origin.pos,
                         dat.origin.pos %>% summarise_at(c("LL.pos","N.pos"),sum) %>% 
                           unlist(.) %>% c("TOTAL",.)) %>% as_tibble()
dat.origin.pos <- dat.origin.pos %>% mutate(LL.pos=as.numeric(LL.pos), N.pos=as.numeric(N.pos))

###
dat.origin.LL <- full_join(dat.origin.bern,dat.origin.pos) %>% mutate(LL.total = LL.bern + LL.pos)
####

## BY REGION
dat.region.bern <- dat.ll.bern %>% group_by(region=location.name) %>% summarise(LL.bern=sum(ll),N.bern=sum(n)) %>% mutate(region=as.character(region))
dat.region.bern <- rbind(dat.region.bern,
                         dat.region.bern %>% summarise_at(c("LL.bern","N.bern"),sum) %>% 
                           unlist(.) %>% c("TOTAL",.)) %>% 
                            as_tibble()
dat.region.bern <- dat.region.bern %>% mutate(LL.bern=as.numeric(LL.bern), N.bern=as.numeric(N.bern))

dat.region.pos  <- dat.ll.pos %>% group_by(region=location.name) %>% summarise(LL.pos=sum(ll),N.pos=sum(n)) %>% mutate(region=as.character(region))
dat.region.pos <- rbind(dat.region.pos,
                        dat.region.pos %>% summarise_at(c("LL.pos","N.pos"),sum) %>% 
                          unlist(.) %>% c("TOTAL",.)) %>% as_tibble()
dat.region.pos <- dat.region.pos %>% mutate(LL.pos=as.numeric(LL.pos), N.pos=as.numeric(N.pos))

###
dat.region.LL <- full_join(dat.region.bern,dat.region.pos) %>% mutate(LL.total = LL.bern + LL.pos)
####

## BY GEAR and REGION
dat.gear.region.bern <- dat.ll.bern %>% group_by(gear=effort.type,region=location.name) %>% summarise(LL.bern=sum(ll),N.bern=sum(n)) %>% mutate(region=as.character(region))
dat.gear.region.pos  <- dat.ll.pos %>% group_by(gear=effort.type,region=location.name) %>% summarise(LL.pos=sum(ll),N.pos=sum(n)) %>% 
                            mutate(region=as.character(region))
dat.gear.region.long <- full_join(dat.gear.region.bern,dat.gear.region.pos) %>% 
                                mutate(LL.bern = ifelse(is.na(LL.bern),0,LL.bern),
                                        LL.pos = ifelse(is.na(LL.pos),0,LL.pos),
                                        LL.total = LL.bern + LL.pos)

dat.gear.region.LL.wide <- pivot_wider(dat.gear.region.long,id_cols="region",names_from=c("gear"),values_from = "LL.total") %>% 
                            mutate_all(~replace(.,is.na(.),0))

dat.gear.region.N.wide <- pivot_wider(dat.gear.region.long,id_cols="region",names_from=c("gear"),values_from = "N.bern") %>% 
                              mutate_all(~replace(.,is.na(.),0))

NOM <- c("troll","rec","treaty","ashop","shoreside")

dat.gear.region.LL.wide <- dat.gear.region.LL.wide %>% dplyr::select("region",NOM)
dat.gear.region.N.wide <- dat.gear.region.N.wide %>% dplyr::select("region",NOM)

dat.gear.region.LL.wide <- rbind(dat.gear.region.LL.wide,
                                 dat.gear.region.LL.wide %>% summarise_at(NOM,sum) %>% 
                                  unlist(.) %>% c("TOTAL",.)) %>% 
                                  as_tibble() %>% 
                                  mutate_at(vars(NOM),funs(as.numeric)) 
                                    

dat.gear.region.N.wide <- rbind(dat.gear.region.N.wide,
                                 dat.gear.region.N.wide %>% summarise_at(NOM,sum) %>% 
                                   unlist(.) %>% c("TOTAL",.)) %>% 
                                as_tibble() %>% 
                                mutate_at(vars(NOM),funs(as.numeric))

####
## BY GEAR and ORIGIN
dat.gear.origin.bern <- dat.ll.bern %>% group_by(gear=effort.type,origin) %>% summarise(LL.bern=sum(ll),N.bern=sum(n)) %>% mutate(origin=as.character(origin))
dat.gear.origin.pos  <- dat.ll.pos %>% group_by(gear=effort.type,origin) %>% summarise(LL.pos=sum(ll),N.pos=sum(n)) %>% 
                        mutate(origin=as.character(origin))
dat.gear.origin.long <- full_join(dat.gear.origin.bern,dat.gear.origin.pos) %>%
                                  mutate(LL.bern = ifelse(is.na(LL.bern),0,LL.bern),
                                         LL.pos = ifelse(is.na(LL.pos),0,LL.pos),
                                         LL.total = LL.bern + LL.pos)

dat.gear.origin.LL.wide <- pivot_wider(dat.gear.origin.long,id_cols="origin",names_from=c("gear"),values_from = "LL.total") %>% 
  mutate_all(~replace(.,is.na(.),0))

dat.gear.origin.N.wide <- pivot_wider(dat.gear.origin.long,id_cols="origin",names_from=c("gear"),values_from = "N.bern") %>% 
  mutate_all(~replace(.,is.na(.),0))

NOM <- c("troll","rec","treaty","ashop","shoreside")

dat.gear.origin.LL.wide <- dat.gear.origin.LL.wide %>% dplyr::select("origin",NOM)
dat.gear.origin.N.wide <- dat.gear.origin.N.wide %>% dplyr::select("origin",NOM)

dat.gear.origin.LL.wide <- rbind(dat.gear.origin.LL.wide,
                                 dat.gear.origin.LL.wide %>% summarise_at(NOM,sum) %>% 
                                   unlist(.) %>% c("TOTAL",.)) %>% 
                                   as_tibble() %>% 
                                   mutate_at(vars(NOM),funs(as.numeric)) 

dat.gear.origin.N.wide <- rbind(dat.gear.origin.N.wide,
                                dat.gear.origin.N.wide %>% summarise_at(NOM,sum) %>% 
                                  unlist(.) %>% c("TOTAL",.)) %>% 
                                  as_tibble() %>% 
                                  mutate_at(vars(NOM),funs(as.numeric))

###########################################################################
#### Calculate the other likelihood components
###########################################################################

# Log_N_ratio
# ll.N.ratio <-  -log(REL$log_N_ratio_sd) - 0.5 * ((REL$log_N_ratio_mean - log_N_ratio)^2) / REL$log_N_ratio_sd
# ll.N.ratio <- cbind(COV,ll.N.ratio)
# ll.N.ratio.total <- sum(ll.N.ratio$ll.N.ratio)

# Spawning Maturity
ALPHA <- prop_D * diri_constant
E_prop <- as.matrix(E_alpha / rowSums(E_alpha))
E=E_prop

Alpha = ALPHA[1,]
E = E_prop[1,]

log.diri <- function(Alpha,E){
  ll <- lgamma(diri_constant) - sum(lgamma(Alpha)) + sum( (Alpha-1) * log(E) ) 
  return(ll)
}

ll.dirichlet <- 1:nrow(REL)
for(i in 1:nrow(REL)){
  ll.dirichlet[i] <- log.diri(ALPHA[i,],E_prop[COV$loc.spawn.idx[i],])
}
ll.dirichlet <- cbind(COV,ll.dirichlet)
ll.dirichlet.total <- sum(ll.dirichlet$ll.dirichlet)


#################################
### Write the necessary likelihood parts to file.  Provide the factor levels in order for good plotting mojo.
#################################

LEV.reg <- c(as.character(LOCATIONS.plot$location.name),"TOTAL")
LEV.origin <- c(as.character(spawn_loc$ocean.region),"TOTAL")
  
dat.origin.LL$origin <- factor(dat.origin.LL$origin,levels=LEV.origin)
dat.gear.origin.LL.wide$origin <- factor(dat.gear.origin.LL.wide$origin,levels=LEV.origin)

dat.region.LL$region <- factor(dat.region.LL$region,levels=LEV.reg)
dat.gear.region.LL.wide$region <- factor(dat.gear.region.LL.wide$region,levels=LEV.reg)


LL <- list(NAME=Output$NAME,
            dat.origin.LL = dat.origin.LL %>% arrange(origin),
            dat.region.LL = dat.region.LL %>% arrange(region),
            dat.gear.origin.LL.wide = dat.gear.origin.LL.wide %>% arrange(origin),
            dat.gear.region.LL.wide = dat.gear.region.LL.wide %>% arrange(region),
            ll.dirichlet=ll.dirichlet,
            ll.dirichlet.total=ll.dirichlet.total)
            # ll.N.ratio=ll.N.ratio,
            # ll.N.ratio.total=ll.N.ratio.total)

save(LL,file=paste0(markdown.dir,"/LL ",Output$NAME,".Rdata"))


