##### This is a plot of the predicted distribution under future oceans.
#####
library(gtable)
library(GGally)

 # read in the abundance data for use much later on (see last plotting section in this script)
load("../blue book/Abundance_data.RData")
TOT.ABUND<-Output.abund$all.pop %>% group_by(year) %>% summarise(SUM=sum(ocean.abund))
summary(TOT.ABUND)

ABUND.BY.POP<-Output.abund$all.pop %>% group_by(stock) %>% summarise(MEAN=mean(ocean.abund),SD=sd(ocean.abund))
ABUND.BY.POP
# releavant file is Output.abund

# First.  Go get the predicted termerature deviation for each season and ocean area
  dev.dat <- read.csv(paste0(results.dir,"/__Temperature projections.csv"))
  # dev.final is the important column in this data.
  
  # This rescales so the deviations are in shifted by 0.1.  This is done for the OISST data
  dev.dat <- dev.dat %>% mutate(dev.final.rescale = dev.final * 0.1) 
  
# Next, Pull the parameters from the MCMC necessary to calculate the projections for 
  THESE <- seq(1,dim(samp$origin_sea_int)[1],by=5)
  
  INT   <- samp$origin_sea_int[THESE,,,]
  SLOPE <- samp$origin_sea_slope[THESE,,,]

  ## Dimensions of this are [MCMC,SEASON,ORIGIN,LOCATION]
    # Seasons: 1=Spring-winter, 2=Summer, 3=Fall.

  
  # Cycle across projection years, then across origin populations.  
  YEARS <- as.numeric(unique(dev.dat$lab))
  
  ALL.PROBS <- NULL
  for( i in 1:length(YEARS)){
    DEV <- dev.dat %>% filter(season=="Sum",lab==YEARS[i]) %>% dplyr::select(region_numb,dev.final.rescale) %>% 
          left_join(LOCATIONS %>% rename(region_numb=location.number),.) %>% 
          mutate(dev.final.rescale=ifelse(is.na(dev.final.rescale)==T,0,dev.final.rescale)) %>% arrange(region_numb)
  
    for( j in 1:max(spawn_loc_plot$init.loc)){
      A <- t(INT[,2,j,]) 
      B <- t(SLOPE[,2,j,]) * DEV$dev.final.rescale 
  
      C <- exp(A+B)
      C.sum <- colSums(C)
      PROBS <- t(C)/ C.sum
      
      # divide by the marginal sums here.
      quants <- c(0.025,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.975)
      Q.LABS <- paste0("X.",quants)
      
      Q.PROBS <- apply(PROBS,2,quantile,probs=quants) %>% as.data.frame()
      
      Q.PROBS <-  rbind(Q.PROBS,colMeans(PROBS)) 
      colnames(Q.PROBS) <- LOCATIONS$location.name
      rownames(Q.PROBS)[nrow(Q.PROBS)] <- "Mean"
      Q.PROBS <- Q.PROBS %>% mutate(quant=c(Q.LABS,"Mean"),
                                    year=YEARS[i],
                                    origin=spawn_loc_plot$ocean.region[j],
                                    origin.numb=spawn_loc_plot$loc.spawn.idx[j] )
      
      ALL.PROBS <- bind_rows(ALL.PROBS,Q.PROBS)
      
    }
  }
  
  dat.proj <- pivot_longer(ALL.PROBS,cols = LOCATIONS$location.name,names_to="location")
  dat.proj$location <- as.factor(dat.proj$location)
  
  dat.proj.fin <- pivot_wider(dat.proj,id_cols=c("year","location","origin","origin.numb"),values_from = "value",names_from = "quant")
  dat.proj.fin$location <- factor(dat.proj.fin$location,levels=LOCATIONS$location.name)
  dat.proj.fin$location.numb <- as.numeric(dat.proj.fin$location)
  
  ############# COASTAL ONLY PLOTS  
  
  dat.proj.coast <- dat.proj.fin %>% filter(location.numb<=8|location.numb>=12) %>% arrange(location.numb) %>% 
    mutate(loc.mod=case_when(location.numb<=8~as.numeric(location.numb),
                             location.numb>=12~location.numb-3))
  
  
  
  ################################################
  ################################################
  ################################################
  ################################################
  #### OK.  Make plots of mean and specific years against future projections.
  ################################################
  ################################################
  ################################################
  ################################################
  
  # DAT.trim is from "Spatial location plots SS Climate.r" and has data from each year for summer.
    DAT.trim <- DAT.trim %>%  mutate(loc.mod=case_when(loc<=8~as.numeric(loc),
                                                          loc>=12~as.numeric(loc)-3))

    # DAT.MEAN.trim is from "Spatial location plots SS Climate.r" and has estimated average for summer
  
  YR <- c(1997,2008)
  YR.line <- c("solid","solid")
  YR.PROJ <- c(2030,2070)
  YR.PROJ.line <- c("dashed","dotted","twodash")
  
  COLS <- c("red","blue",rep("black",length(YR.PROJ)))
  if(length(c(YR,YR.PROJ))==5){
    LINES <- c("solid","solid","dashed","dotted","twodash")
  }
  if(length(c(YR,YR.PROJ))==4){
    LINES <- c("solid","solid","solid","dashed")
  }
  
  
  ########################## THIS IS FOR ORIGIN == 4
  
  ORIGIN <- c("SFB","NCA","LCOL","URB")
  BREAKS = sort(unique(DAT.trim$loc.mod))
  LABS <- levels(dat.proj.coast$location) %>% as.data.frame() %>% 
                rename(loc=".") %>% filter(!loc %in% c("SGEO","PUSO","PUSO_out")) %>% c(.)
  LABS <- LABS$loc
  i=1
  ALP.ribbon <- 0.2
  SIZE <- 1.1 # line weight
  XLAB <- ""
  YLAB <- "Proportion"

  ###
  all.dat <- DAT.trim %>% filter(ocean.region %in% ORIGIN, year %in% YR) %>%
              dplyr::select(year,ocean.region,loc.mod,prop,X.05,X.95) 
  
  dat.proj.coast.trim <- dat.proj.coast %>% rename(X.05 = X.0.05,X.95=X.0.95,ocean.region=origin,prop=Mean) %>% 
              filter(ocean.region %in% ORIGIN,
                     year %in% YR.PROJ) %>%
              dplyr::select(year,ocean.region,loc.mod,prop,X.05,X.95)
  
  all.dat <- bind_rows(all.dat,dat.proj.coast.trim)
  
  all.dat$year <- as.factor(all.dat$year)
  
  
  all.dat$ocean.region <- factor(all.dat$ocean.region,levels=ORIGIN)       
  
  future.proj.four<- 
    ggplot(all.dat) +
    #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
    geom_ribbon(aes(x=loc.mod,ymin = X.05, ymax = X.95,fill=year),alpha=ALP.ribbon) + 
    geom_point(aes(y=prop,x=loc.mod,color=year),alpha=ALP,size=1) +
    geom_line(aes(y=prop,x=loc.mod,color=year,linetype=year),alpha=ALP,size=SIZE) +
    scale_color_manual(name="Year",values=COLS) +
    scale_fill_manual(name="Year",values=COLS)+
    scale_linetype_manual(name="Year",values=LINES)+
    facet_wrap(~ocean.region,nrow=2,scales="free") +

    xlab(XLAB)+
    ylab(YLAB)+
    scale_y_continuous(expand = expand_scale(mult=c(0.02,0.1)))+
    scale_x_continuous(breaks=BREAKS,labels = LABS) +
    #annotation_custom(grob) +
    theme_bw()+
    #theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
    coord_flip()
    
  future.proj.four
  
  
  # For 6 Origins.
  ORIGIN.six <- c("SFB","NCA","LCOL","MCOL","SNAK","URB")
  ##
  all.dat <- DAT.trim %>% filter(ocean.region %in% ORIGIN.six, year %in% YR) %>%
    dplyr::select(year,ocean.region,loc.mod,prop,X.05,X.95) 
  
  dat.proj.coast.trim <- dat.proj.coast %>% rename(X.05 = X.0.05,X.95=X.0.95,ocean.region=origin,prop=Mean) %>% 
    filter(ocean.region %in% ORIGIN.six,
           year %in% YR.PROJ) %>%
    dplyr::select(year,ocean.region,loc.mod,prop,X.05,X.95)
  
  all.dat <- bind_rows(all.dat,dat.proj.coast.trim)
  
  all.dat$year <- as.factor(all.dat$year)
  
  all.dat$ocean.region <- factor(all.dat$ocean.region,levels=ORIGIN.six)       
  
  future.proj.six <- 
    ggplot(all.dat) +
    #geom_point(aes(y=prop,x=loc,group=year,color=year),alpha=ALP) +
    geom_ribbon(aes(x=loc.mod,ymin = X.05, ymax = X.95,fill=year),alpha=ALP.ribbon) + 
    geom_point(aes(y=prop,x=loc.mod,color=year),alpha=ALP,size=1) +
    geom_line(aes(y=prop,x=loc.mod,color=year,linetype=year),alpha=ALP,size=SIZE) +
    scale_color_manual(name="Year",values=COLS) +
    scale_fill_manual(name="Year",values=COLS)+
    scale_linetype_manual(name="Year",values=LINES)+
    facet_wrap(~ocean.region,nrow=2) +
    
    xlab(XLAB)+
    ylab(YLAB)+
    scale_y_continuous(expand = expand_scale(mult=c(0.02,0.1)))+
    scale_x_continuous(breaks=BREAKS,labels = LABS) +
    #annotation_custom(grob) +
    theme_bw()+
    #theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
    coord_flip()
 
  quartz(file=paste0(base.dir,"/Salmon-Climate/Output plots/Spatial Plot FUTURE FOUR ",NAME,".jpeg",sep=""),
         height=7,width=6,dpi=600,type="jpeg")
  print(future.proj.four)
  dev.off()

  
  quartz(file=paste0(base.dir,"/Salmon-Climate/Output plots/Spatial Plot FUTURE SIX ",NAME,".jpeg",sep=""),
         height=8,width=9,dpi=600,type="jpeg")
  print(future.proj.six)
  dev.off()
  
    
  ######################################################################################3
  ######################################################################################3
  ######################################################################################3
  ######################################################################################3
  ######################################################################################3
  ######################################################################################3
  ######################################################################################3
  ######################################################################################3
  ######################################################################################3
  ######################################################################################3
  ######################################################################################3
  ######################################################################################3
  
  # Aggregating and summarizing differences for targeted stocks
  
  ######################################################################################3
  ######################################################################################3
  ######################################################################################3
  ######################################################################################3

  ORIGIN <- c("SFB","NCA","LCOL","MCOL","SNAK","URB")
  YR.PROJ <- c(2030,2050,2070,2090)
  nom.agg <-c("C.CA","N.CA","OR","WA","SAL","WA.JDF","S.BC","N.BC","BC","AK","BC.AK")
  # Calculate estimated mean, projected mean for 2030 and 2070, calculated differencing.
  # Next, Pull the parameters from the MCMC necessary to calculate the projections for 
  
  INT   <- samp$origin_sea_int[THESE,,,]
  SLOPE <- samp$origin_sea_slope[THESE,,,]

  ## Dimensions of this are [MCMC,SEASON,ORIGIN,LOCATION]
  # Seasons: 1=Spring-winter, 2=Summer, 3=Fall.
  
  # Cycle across projection years, then across origin populations.  
  YEARS <- as.numeric(unique(dev.dat$lab))
  ALL.PROBS.proj <- NULL
  ALL.PROBS.mean <- NULL
  ALL.PROBS.diff <- NULL
  ALL.PROBS.diff.agg <- NULL
  
  for( i in 1:length(YEARS)){
    DEV <- dev.dat %>% filter(season=="Sum",lab==YEARS[i]) %>% dplyr::select(region_numb,dev.final.rescale) %>% 
      left_join(LOCATIONS %>% rename(region_numb=location.number),.) %>% 
      mutate(dev.final.rescale=ifelse(is.na(dev.final.rescale)==T,0,dev.final.rescale)) %>% arrange(region_numb)
    
    for( j in 1:max(spawn_loc_plot$init.loc)){
      A <- t(INT[,2,j,]) 
      B.proj <- t(SLOPE[,2,j,]) * DEV$dev.final 
      B.mean <- t(SLOPE[,2,j,]) * DEV$dev.final * 0 
      
      C.proj <- exp(A+B.proj)
      C.mean <- exp(A+B.mean)
      
      C.proj.sum  <- colSums(C.proj)
      C.mean.sum  <- colSums(C.mean)

      PROBS.proj <- t(C.proj)/ C.proj.sum
      PROBS.mean <- t(C.mean)/ C.mean.sum
      PROBS.diff <- PROBS.proj-PROBS.mean
      
      p.diff.agg <- PROBS.proj-PROBS.mean; colnames(p.diff.agg) <- LOCATIONS$location.name
      p.diff.agg <- p.diff.agg %>% as.data.frame() %>%
                          mutate(C.CA = MONT+SFB,
                                 N.CA = MEN+NCA,
                                 OR = SOR+NOR,
                                 WA = COL+WAC,
                                 WA.JDF=COL+WAC+PUSO_out,
                                 SAL= PUSO+PUSO_out+SGEO,
                                 S.BC = SWVI+NWVI,
                                 N.BC = CBC + NBC,
                                 BC = S.BC+N.BC,
                                 AK = SSEAK+NSEAK,
                                 BC.AK = BC+AK) %>%
                      dplyr::select(C.CA,N.CA,OR,WA,SAL,WA.JDF,S.BC,N.BC,BC,AK,BC.AK)

      PROBS.diff.agg <- p.diff.agg
      
      # divide by the marginal sums here.
      quants <- c(0.025,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.975)
      Q.LABS <- paste0("X.",quants)
      
      Q.PROBS.proj <- apply(PROBS.proj,2,quantile,probs=quants) %>% as.data.frame()
      Q.PROBS.mean <- apply(PROBS.mean,2,quantile,probs=quants) %>% as.data.frame()
      Q.PROBS.diff <- apply(PROBS.diff,2,quantile,probs=quants) %>% as.data.frame()
      Q.PROBS.diff.agg <- apply(PROBS.diff.agg,2,quantile,probs=quants) %>% as.data.frame()
      
      Q.PROBS.proj <-  rbind(Q.PROBS.proj,colMeans(PROBS.proj)) 
      colnames(Q.PROBS.proj) <- LOCATIONS$location.name
      rownames(Q.PROBS.proj)[nrow(Q.PROBS.proj)] <- "Mean"
      Q.PROBS.proj <- Q.PROBS.proj %>% mutate(quant=c(Q.LABS,"Mean"),
                                    year=YEARS[i],
                                    origin=spawn_loc_plot$ocean.region[j],
                                    origin.numb=spawn_loc_plot$loc.spawn.idx[j] )

      Q.PROBS.diff <-  rbind(Q.PROBS.diff,colMeans(PROBS.diff)) 
      colnames(Q.PROBS.diff) <- LOCATIONS$location.name
      rownames(Q.PROBS.diff)[nrow(Q.PROBS.diff)] <- "Mean"
      Q.PROBS.diff <- Q.PROBS.diff %>% mutate(quant=c(Q.LABS,"Mean"),
                                              year=YEARS[i],
                                              origin=spawn_loc_plot$ocean.region[j],
                                              origin.numb=spawn_loc_plot$loc.spawn.idx[j] )

      Q.PROBS.diff.agg <-  rbind(Q.PROBS.diff.agg,colMeans(PROBS.diff.agg)) 
      colnames(Q.PROBS.diff.agg) <- nom.agg
      rownames(Q.PROBS.diff.agg)[nrow(Q.PROBS.diff.agg)] <- "Mean"
      Q.PROBS.diff.agg <- Q.PROBS.diff.agg %>% mutate(quant=c(Q.LABS,"Mean"),
                                              year=YEARS[i],
                                              origin=spawn_loc_plot$ocean.region[j],
                                              origin.numb=spawn_loc_plot$loc.spawn.idx[j] )
      
                  
      if(i==1){
        Q.PROBS.mean <-  rbind(Q.PROBS.mean,colMeans(PROBS.mean)) 
        colnames(Q.PROBS.mean) <- LOCATIONS$location.name
        rownames(Q.PROBS.mean)[nrow(Q.PROBS.mean)] <- "Mean"
        Q.PROBS.mean <- Q.PROBS.mean %>% mutate(quant=c(Q.LABS,"Mean"),
                                              year=YEARS[i],
                                              origin=spawn_loc_plot$ocean.region[j],
                                              origin.numb=spawn_loc_plot$loc.spawn.idx[j] )
  
        Q.PROBS.mean$year <- as.character(Q.PROBS.mean$year)
        ALL.PROBS.mean <- bind_rows(ALL.PROBS.mean,Q.PROBS.mean)
        ALL.PROBS.mean$year <-"Base"
      }
            
      ALL.PROBS.proj <- bind_rows(ALL.PROBS.proj,Q.PROBS.proj)
      ALL.PROBS.diff <- bind_rows(ALL.PROBS.diff,Q.PROBS.diff)
      ALL.PROBS.diff.agg <- bind_rows(ALL.PROBS.diff.agg,Q.PROBS.diff.agg)
    }
  }
  
  
  dat.proj <- pivot_longer(ALL.PROBS.proj,cols = LOCATIONS$location.name,names_to="location")
  dat.proj$location <- as.factor(dat.proj$location)
  dat.proj.fin <- pivot_wider(dat.proj,id_cols=c("year","location","origin","origin.numb"),values_from = "value",names_from = "quant")
  dat.proj.fin$location <- factor(dat.proj.fin$location,levels=LOCATIONS$location.name)
  dat.proj.fin$location.numb <- as.numeric(dat.proj.fin$location)
  dat.proj.fin$type <- "projection"

  dat.mean <- pivot_longer(ALL.PROBS.mean,cols = LOCATIONS$location.name,names_to="location")
  dat.mean$location <- as.factor(dat.mean$location)
  dat.mean.fin <- pivot_wider(dat.mean,id_cols=c("year","location","origin","origin.numb"),values_from = "value",names_from = "quant")
  dat.mean.fin$location <- factor(dat.mean.fin$location,levels=LOCATIONS$location.name)
  dat.mean.fin$location.numb <- as.numeric(dat.mean.fin$location)
  dat.mean.fin$type <- "base"
  
  dat.diff <- pivot_longer(ALL.PROBS.diff,cols = LOCATIONS$location.name,names_to="location")
  dat.diff$location <- as.factor(dat.diff$location)
  dat.diff.fin <- pivot_wider(dat.diff,id_cols=c("year","location","origin","origin.numb"),values_from = "value",names_from = "quant")
  dat.diff.fin$location <- factor(dat.diff.fin$location,levels=LOCATIONS$location.name)
  dat.diff.fin$location.numb <- as.numeric(dat.diff.fin$location)
  dat.diff.fin$type <- "difference"

  dat.diff.agg <- pivot_longer(ALL.PROBS.diff.agg,cols = nom.agg,names_to="location")
  dat.diff.agg$location <- as.factor(dat.diff.agg$location)
  dat.diff.agg.fin <- pivot_wider(dat.diff.agg,id_cols=c("year","location","origin","origin.numb"),values_from = "value",names_from = "quant")
  dat.diff.agg.fin$location <- factor(dat.diff.agg.fin$location,levels=LOCATIONS$location.name)
  dat.diff.agg.fin$location.numb <- as.numeric(dat.diff.agg.fin$location)
  dat.diff.agg.fin$type <- "difference.agg"
  
  dat.mean.fin$year <- as.character(dat.mean.fin$year)
  dat.proj.fin$year <- as.character(dat.proj.fin$year)
  dat.diff.fin$year <- as.character(dat.diff.fin$year)
  
  dat.all.A <-   bind_rows(dat.mean.fin,dat.proj.fin,dat.diff.fin)
  
  ############# COASTAL ONLY PLOTS  
  dat.all.coast <- dat.all.A %>% filter(location.numb<=8|location.numb>=12) %>% arrange(location.numb) %>% 
                      mutate(loc.mod= case_when(location.numb<=8~as.numeric(location.numb),
                                                location.numb>=12~location.numb-3))
  dat.diff.agg.coast <- dat.diff.agg %>% filter(!location == "SAL")
    
  dat.diff.agg.coast$location = factor(dat.diff.agg.coast$location,levels=nom.agg)
  dat.diff.agg.coast$origin = factor(dat.diff.agg.coast$origin,levels=ORIGIN)
  ###################
  ###################
  ###################
  ###################
  ###3  MAKE PLOTS
  ###################
  ###################
  ###################
  ###################

  #Z.LIM <- max(abs(c(max(DAT$value),min(DAT$value)))) ; Z.LIM <- c(-Z.LIM,Z.LIM)

  XLAB = c("Central \nCalifornia", 
           "Northern \nCalifornia", 
           "Oregon",
           "Washington",
           "Southern \nBritish \nColumbia",
           "Northern \nBritish \nColumbia",
           "Alaska")

  BREAKS <- round(seq(-0.16,0.16, by=0.04),2)      
  Z.LIM <- c(-0.18,0.18)
  # XLAB = c("Central \nCalifornia", "Northern \nCalifornia", "Oregon","Washington","British \nColumbia","Alaska")
  # 
  # BREAKS <- round(seq(-0.16,0.16, by=0.04),2)      
  # Z.LIM <- c(-0.16,0.16)

  THM.1 <- theme(plot.margin = unit(c(0.2,0,0.2,0), "cm"),
               panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank(),
               axis.text.x = element_text(size = 7),
               axis.text.y = element_text(size = 8),
               axis.title= element_text(size=10),
               #legend.key.size = unit(1, "in"),
               legend.key.width= unit(0.5, "in"),
               legend.text = element_text(size=7),
               legend.title = element_text(size=8),
               legend.position = "bottom",
               legend.margin=margin(-50,-10,-10,-10),
               legend.box.margin=margin(0,0,0,0),
               plot.tag.position = c(0.05, 0.9))
  
  ##########
  LOC.remove <-c("BC","BC.AK","WA.JDF")
  
  
  DAT <- dat.diff.agg.coast %>% filter(origin %in% ORIGIN,year ==2070,quant=="Mean",!location %in% LOC.remove)
  snak.lab <- DAT %>% filter(origin =="SNAK",location=="AK") 
  prop.change.2070  <- ggplot(DAT %>% mutate(value=ifelse(value > max(Z.LIM),max(Z.LIM),value ))) +
      geom_tile(aes(x=location,y=origin,fill=value),alpha=0.8,color="black") +
      geom_text(data=snak.lab,aes(x=location,y=5,label=round(value,2))) +
      scale_fill_gradient2("Proportional \nChange", low = "darkred",mid=grey(0.95),high = "darkblue",
                           midpoint=0,
                           limits=Z.LIM,  
                           breaks=BREAKS, labels=BREAKS) +
      xlab("")+
      ylab("Stock")+
      scale_y_discrete(expand = c(0,0)) +
      scale_x_discrete(labels=XLAB,expand = c(0,0),position="top") +
      theme_bw()+
      labs(tag = "A")+
      THM.1
  prop.change.2070  
  
  DAT <- dat.diff.agg.coast %>% filter(origin %in% ORIGIN,year ==2050,quant=="Mean",!location %in% LOC.remove )
  snak.lab <- DAT %>% filter(origin =="SNAK",location=="AK") 
  prop.change.2050  <- ggplot(DAT %>% mutate(value=ifelse(value > max(Z.LIM),max(Z.LIM),value ))) +
    geom_tile(aes(x=location,y=origin,fill=value),alpha=0.8,color="black") +
    geom_text(data=snak.lab,aes(x=location,y=5,label=round(value,2))) +
    scale_fill_gradient2("Proportional \nChange", low = "darkred",mid=grey(0.95),high = "darkblue",
                         midpoint=0,
                         limits=Z.LIM,  
                         breaks=BREAKS, labels=BREAKS) +
    xlab("")+
    ylab("Stock")+
    scale_y_discrete(expand = c(0,0)) +
    scale_x_discrete(labels=XLAB,expand = c(0,0),position="top") +
    theme_bw()+
    labs(title = "A")+
    THM.1
  
  BREAKS <- round(seq(-0.08,0.08, by=0.04),2)      
  DAT <- dat.diff.agg.coast %>% filter(origin %in% ORIGIN,year ==2030,quant=="Mean",!location %in% LOC.remove)
  snak.lab <- DAT %>% filter(origin =="SNAK",location=="AK") 
  prop.change.2030  <- ggplot(DAT) +
    geom_tile(aes(x=location,y=origin,fill=value),alpha=0.8,color="black") +
  #  geom_text(data=snak.lab,aes(x=location,y=5,label=round(value,2))) +
    scale_fill_gradient2("Proportional \nChange", low = "darkred",mid=grey(0.95),high = "darkblue",
                         midpoint=0,
                         #limits=Z.LIM,  
                         breaks=BREAKS, labels=BREAKS) +
    xlab("")+
    ylab("Stock")+
    scale_y_discrete(expand = c(0,0)) +
    scale_x_discrete(labels=XLAB,expand = c(0,0),position="top") +
    theme_bw()+
    labs(tag = "A")+
    THM.1
  
  prop.change.2030
  prop.change.2050
  prop.change.2070
  ###################
  ###################
  ###################
  ###################
  ### COMBINE THE ABUNDANCE AND DISTRIBUTION DATA TO MAKE PROJECTIONS OF THE FUTURE.
  ################### THIS IS A PUBLICATION PLOT.
  ###################
  ###################
  ###################
  
  # Output.abund
  all.pop.summary <- Output.abund$quant.pop %>% t(.) %>% as.data.frame() %>% mutate(stock=rownames(.)) %>%
                        left_join(Output.abund$mid.pop,.)
  all.pop.summary$stock <- factor(all.pop.summary$stock,levels=ORIGIN)
  
  # horizontal bar plot for side of the grid
  abund.bar <- ggplot(all.pop.summary) +
                geom_bar(aes(y=Mean,x=stock),stat = "identity",width=1,color="black",fill=grey(0.5)) +
                geom_point(data=Output.abund$all.pop,aes(y=ocean.abund,x=stock),shape=21,size=0.8)+
                #geom_errorbar(aes(ymin=`5%`, ymax=`95%`,x=stock),width=0.1) +
                geom_errorbar(aes(ymin=`25%`, ymax=`75%`,x=stock),width=0.2,size=1.5) +
                scale_x_discrete("",expand = expand_scale(mult=c(0.1,0.1))) +
                scale_y_continuous("Abundance (thousands)",expand = expand_scale(mult=c(0,0.1))) +
                coord_flip() +
                 
                theme_bw()+
                theme(plot.margin = unit(c(0.2,0.1,0.25,-0.2), "cm"),
                      axis.text.y = element_blank(),
                      axis.text.x = element_text(size = 8),
                      axis.title= element_text(size=10))
  abund.bar
  
  g.grid <- ggplotGrob(prop.change.2070)
  g.bar  <- ggplotGrob(abund.bar)
  
  g <- gtable(widths=unit(c(5,3),c("in")),heights=unit(c(4.25),c("in")))
  max.height <- grid::unit.pmax(g.grid$heights, g.bar$heights)
  g.grid$heights <- as.list(max.height)
  g.bar$heights <- as.list(max.height)
  g <- gtable_add_grob(g,g.grid,t=1,l=1)
  g <- gtable_add_grob(g,g.bar,t=1,l=2 )
  
  plot(g)
  
  quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/Change in distribution heatmap.jpeg",sep=""),height=4,width=8,dpi=600,type="jpeg")
     print(plot(g))
  dev.off()

#############################################
############## Do Calculations for looking at change in cumulative change in abundance
#############################################
  # Data files needed
    # 1) simulations of parameters for distribution for each projected year.  
    #     Will use each draw to project current and future pop, calculate difference.
    # 2) Distribution of population sizes.
    #    Options: a) bootstrap from observed distribution (limits upside and downside)
    #             b) use multivariate log-normal draws
  
      # Next, These are the parameters needed to make distribution projections.
              # THEY HAVE ALREADY BEEN THINNED USING THE SPECIFIED VECTOR (THESE) 
              #INT
              #SLOPE 

              # INT   <- samp$origin_sea_int
              # SLOPE <- samp$origin_sea_slope
              # Trim for reduced sample size
              # INT.trim   <- INT[seq(1,nrow(INT),by=2),,,] 
              # SLOPE.trim <- SLOPE[seq(1,nrow(SLOPE),by=2),,,] 

          ## Dimensions of these are [MCMC,SEASON,ORIGIN,LOCATION]
          # Seasons: 1=Spring-winter, 2=Summer, 3=Fall.
      
      # Pull abundance values from a multivariate distribution that follows the input mean and covariance of spawning abundances.
          Output.abund$mid.pop$stock <- factor(Output.abund$mid.pop$stock,levels=ORIGIN) 
          Output.abund$mid.pop <- Output.abund$mid.pop %>% arrange(stock) 
          
          VAL.MINUS = 0.25 # fraction above and below observed values to exclude during simulation    
          VAL.PLUS  = 0.10
          min.max.vec <- Output.abund$all.pop %>% group_by(stock) %>% summarise(MIN = min(ocean.abund),MAX=max(ocean.abund)) %>%
                          mutate(MIN.ADJUST = (1-VAL.MINUS)*MIN,MAX.ADJUST=(1+VAL.PLUS)*MAX)
          
              ABUND <- exp(mvrnorm(1e5,Output.abund$mid.pop$log.median,Output.abund$cov.log.pop))
              ## Cull ABUND to exclude values <75% of min ocean.abund and values >125% of max ocean.abund
              summary(ABUND)
              
              ABUND <- ABUND %>% as.data.frame() %>% filter(SFB > min.max.vec$MIN.ADJUST[min.max.vec$stock=="SFB"],
                                        SFB < min.max.vec$MAX.ADJUST[min.max.vec$stock=="SFB"],
                                        NCA > min.max.vec$MIN.ADJUST[min.max.vec$stock=="NCA"],
                                        NCA < min.max.vec$MAX.ADJUST[min.max.vec$stock=="NCA"],
                                        LCOL > min.max.vec$MIN.ADJUST[min.max.vec$stock=="LCOL"],
                                        LCOL < min.max.vec$MAX.ADJUST[min.max.vec$stock=="LCOL"],
                                        MCOL > min.max.vec$MIN.ADJUST[min.max.vec$stock=="MCOL"],
                                        MCOL < min.max.vec$MAX.ADJUST[min.max.vec$stock=="MCOL"],
                                        SNAK > min.max.vec$MIN.ADJUST[min.max.vec$stock=="SNAK"],
                                        SNAK < min.max.vec$MAX.ADJUST[min.max.vec$stock=="SNAK"],
                                        URB > min.max.vec$MIN.ADJUST[min.max.vec$stock=="URB"],
                                        URB < min.max.vec$MAX.ADJUST[min.max.vec$stock=="URB"]) 
              N = nrow(INT)
              #N = nrow(INT.trim)
              ABUND.trim <- ABUND[1:N,]
              
              stock.pairs.sim = ggpairs(ABUND.trim, aes(alpha = 0.4),diag = list(continuous=wrap("barDiag",binwidth=50))) + theme_bw()
              
              # Compare simulated with input population sizes
                  ABUND.long <- pivot_longer(ABUND,everything(),names_to="stock",values_to="abund") %>% mutate(type="sim")
                  ABUND.long <- bind_rows(ABUND.long,
                                          Output.abund$all.pop %>% dplyr::select(stock,abund=ocean.abund)%>%mutate(type="obs"))
              
                  ABUND.long$stock <- factor(ABUND.long$stock,levels=ORIGIN)
                  
              compare.sim.obs <- ggplot(ABUND.long, aes(abund,stat(density),fill=type)) +
                          geom_histogram(alpha=0.5,position="identity") +
                          scale_fill_manual(values=c("red","blue"))+
                          facet_wrap(~stock,scales="free_y")+
                          xlab("Abundance (thousands)") +
                          theme_bw()
              
              compare.sim.obs.free <- ggplot(ABUND.long, aes(abund,stat(density),fill=type)) +
                geom_histogram(alpha=0.5,position="identity") +
                scale_fill_manual(values=c("red","blue"))+
                facet_wrap(~stock,scales="free")+
                xlab("Abundance (thousands)") +
                theme_bw()
              compare.sim.obs.free
              
              quartz(file=paste(base.dir,"/Salmon-Climate/blue book/Plots/Compare observed, simulated abundance.jpeg",sep=""),height=4,width=6,dpi=600,type="jpeg")
                print(compare.sim.obs)
              dev.off()
              
              quartz(file=paste(base.dir,"/Salmon-Climate/blue book/Plots/Compare observed, simulated abundance (free).jpeg",sep=""),height=4,width=6,dpi=600,type="jpeg")
                print(compare.sim.obs.free)
              dev.off()
              
              quartz(file=paste(base.dir,"/Salmon-Climate/blue book/Plots/Compare observed, simulated abundance pairs.jpeg",sep=""),height=7,width=7,dpi=600,type="jpeg")
                print(stock.pairs.sim)
              dev.off()
              
              
              #stock.pairs.log.sim = ggpairs(log(ABUND), aes(alpha = 0.4),diag = list(continuous=wrap("barDiag",binwidth=0.5))) + theme_bw()

  # Cycle across projection years, then across origin populations.  
  YEARS <- as.numeric(unique(dev.dat$lab))
  POPS  <- c("SFB","NCA","LCOL","MCOL","SNAK","URB")
  ALL.REF <- NULL
  ALL.DIFF <- NULL
  ALL.SUM.DIFF <- NULL
  ALL.REF.SUM <- NULL
  ALL.PRED.SUM <- NULL
  diff.sum.all <- NULL  
    
  for( i in 1:length(YEARS)){
    DEV <- dev.dat %>% filter(season=="Sum",lab==YEARS[i]) %>% dplyr::select(region_numb,dev.final.rescale) %>% 
                left_join(LOCATIONS %>% rename(region_numb=location.number),.) %>% 
                mutate(dev.final.rescale=ifelse(is.na(dev.final.rescale)==T,0,dev.final.rescale)) %>% arrange(region_numb)
    
    for( j in 1:length(POPS)){
      J = which(spawn_loc_plot$nom==POPS[j])
      
      A <- t(INT[,2,J,]) 
      B <- t(SLOPE[,2,J,]) * DEV$dev.final.rescale 

      C <- exp(A+B)
      C.ref <- colSums(exp(A))
      C.sum <- colSums(C)
      PROBS.ref <- t(exp(A))/ C.ref
      PROBS <- t(C)/ C.sum
      
      A <- ABUND.trim %>% dplyr::select(POPS[j])
      A <- as.matrix(A)
      N.ref  <- apply(PROBS.ref,2,"*",A)
      N.pred <- apply(PROBS,2,"*",A)
      diff   <- N.pred - N.ref
      
      rowSums(N.ref) %>% summary(.)
      
      #Aggregate reference
      colnames(N.ref) <- LOCATIONS$location.name
      N.ref.agg <- N.ref %>% as.data.frame() %>%
        mutate(origin=POPS[j],
               year=YEARS[i],
               replicate = 1:nrow(N.ref),
               C.CA = MONT+SFB,
               N.CA = MEN+NCA,
               OR = SOR+NOR,
               WA = COL+WAC,
               WA.JDF = COL+WAC+PUSO_out,
               SAL= PUSO+PUSO_out+SGEO,
               S.BC = SWVI+NWVI,
               N.BC = CBC + NBC,
               BC = S.BC+N.BC,
               AK = SSEAK+NSEAK,
               BC.AK = BC+AK) %>%
        dplyr::select(origin, year,replicate,C.CA,N.CA,OR,WA,SAL,WA.JDF,S.BC,N.BC,BC,AK,BC.AK)

      #Aggregate predictions
      colnames(N.pred) <- LOCATIONS$location.name
      N.pred.agg <- N.pred %>% as.data.frame() %>%
        mutate(origin=POPS[j],
               year=YEARS[i],
               replicate = 1:nrow(N.ref),
               C.CA = MONT+SFB,
               N.CA = MEN+NCA,
               OR = SOR+NOR,
               WA = COL+WAC,
               WA.JDF = COL+WAC+PUSO_out,
               SAL= PUSO+PUSO_out+SGEO,
               S.BC = SWVI+NWVI,
               N.BC = CBC + NBC,
               BC = S.BC+N.BC,
               AK = SSEAK+NSEAK,
               BC.AK = BC+AK) %>%
        dplyr::select(origin, year,replicate,C.CA,N.CA,OR,WA,SAL,WA.JDF,S.BC,N.BC,BC,AK,BC.AK)

      #Aggregate differences
      colnames(diff) <- LOCATIONS$location.name
      diff.agg <- diff %>% as.data.frame() %>%
        mutate(origin=POPS[j],
               year=YEARS[i],
               replicate = 1:nrow(N.ref),
               C.CA = MONT+SFB,
               N.CA = MEN+NCA,
               OR = SOR+NOR,
               WA = COL+WAC,
               WA.JDF = COL+WAC+PUSO_out,
               SAL= PUSO+PUSO_out+SGEO,
               S.BC = SWVI+NWVI,
               N.BC = CBC + NBC,
               BC = S.BC+N.BC,
               AK = SSEAK+NSEAK,
               BC.AK = BC+AK) %>%
        dplyr::select(origin, year,replicate,C.CA,N.CA,OR,WA,SAL,WA.JDF,S.BC,N.BC,BC,AK,BC.AK)
 
      if(j==1 & i==1){
        diff.all = diff.agg
        ref.all  = N.ref.agg
        pred.all = N.pred.agg
      }else{
        diff.all = bind_rows(diff.all,diff.agg)
        ref.all  = bind_rows(ref.all,N.ref.agg)
        pred.all = bind_rows(pred.all,N.pred.agg)
      }
      # ALL.REF <- bind_rows(ALL.REF,Q.REF)
      # ALL.DIFF <- bind_rows(ALL.DIFF,Q.DIFF)
    } # end POP loop
  }# End YEAR loop
   
   # convert to long-form
    ref.long <- pivot_longer(data=ref.all,cols = nom.agg,values_to = "abund",names_to="region")
    pred.long <- pivot_longer(data=pred.all,cols = nom.agg,values_to = "abund",names_to="region")
    diff.long <- pivot_longer(data=diff.all,cols = nom.agg,values_to = "abund",names_to="region")
    
    # SUMMARIZE 
    ref.agg.summary <- ref.long %>% group_by(year,replicate,region) %>% summarise(SUM = sum(abund)) %>%
                        group_by(year,region) %>% summarise(MEAN=mean(SUM),
                                                     SD=sd(SUM),
                                                     MEDIAN=median(SUM),
                                                     q.05=quantile(SUM,probs=0.05),
                                                     q.10=quantile(SUM,probs=0.10),
                                                     q.25=quantile(SUM,probs=0.25),
                                                     q.50=quantile(SUM,probs=0.50),
                                                     q.75=quantile(SUM,probs=0.75),
                                                     q.90=quantile(SUM,probs=0.90),
                                                     q.95=quantile(SUM,probs=0.95))
    
    pred.agg.summary <- pred.long %>% group_by(year,replicate,region) %>% summarise(SUM = sum(abund)) %>%
                            group_by(year,region) %>% summarise(MEAN=mean(SUM),
                                          SD=sd(SUM),
                                          MEDIAN=median(SUM),
                                          q.05=quantile(SUM,probs=0.05),
                                          q.10=quantile(SUM,probs=0.10),
                                          q.25=quantile(SUM,probs=0.25),
                                          q.50=quantile(SUM,probs=0.50),
                                          q.75=quantile(SUM,probs=0.75),
                                          q.90=quantile(SUM,probs=0.90),
                                          q.95=quantile(SUM,probs=0.95)) 
                            
    
    diff.agg.summary <- diff.long %>% group_by(year,replicate,region) %>% summarise(SUM = sum(abund)) %>%
                              group_by(year,region) %>% summarise(MEAN=mean(SUM),
                                          SD=sd(SUM),
                                          MEDIAN=median(SUM),
                                          q.05=quantile(SUM,probs=0.05),
                                          q.10=quantile(SUM,probs=0.10),
                                          q.25=quantile(SUM,probs=0.25),
                                          q.50=quantile(SUM,probs=0.50),
                                          q.75=quantile(SUM,probs=0.75),
                                          q.90=quantile(SUM,probs=0.90),
                                          q.95=quantile(SUM,probs=0.95))
    
    # This sums across the defined runs to calculate a reference total abundance
  #   Q.REF.SUM <- apply(ref.sum,2,quantile,probs=quants) %>% as.data.frame()
  #   Q.REF.SUM <- rbind(Q.REF.SUM,colMeans(ref.sum)) 
  #   rownames(Q.REF.SUM)[nrow(Q.REF.SUM)] <- "Mean"
  #   
  #   Q.REF.SUM <- Q.REF.SUM %>% mutate(quant=c(Q.LABS,"Mean"),
  #                             year=YEARS[i],
  #                             origin="SUM")
  #   
  #   ALL.REF.SUM <- bind_rows(ALL.REF.SUM,Q.REF.SUM)
  #   
  #   # This sums across the defined runs to calculate a predicted total abundance in the future
  #   Q.PRED.SUM <- apply(pred.sum,2,quantile,probs=quants) %>% as.data.frame()
  #   Q.PRED.SUM <- rbind(Q.PRED.SUM,colMeans(pred.sum)) 
  #   rownames(Q.PRED.SUM)[nrow(Q.PRED.SUM)] <- "Mean"
  #   
  #   Q.PRED.SUM <- Q.PRED.SUM %>% mutate(quant=c(Q.LABS,"Mean"),
  #                                     year=YEARS[i],
  #                                     origin="SUM")
  #   
  #   ALL.PRED.SUM <- bind_rows(ALL.PRED.SUM,Q.PRED.SUM)
  #   
  #       
  #   # This sums across the defined runs to calculate a cumulative change
  #   Q.SUM <- apply(diff.sum,2,quantile,probs=quants) %>% as.data.frame()
  #   Q.SUM <- rbind(Q.SUM,colMeans(diff.sum)) 
  #   rownames(Q.SUM)[nrow(Q.SUM)] <- "Mean"
  #   
  #   Q.SUM <- Q.SUM %>% mutate(quant=c(Q.LABS,"Mean"),
  #                                year=YEARS[i],
  #                                origin="SUM")
  #   
  #   ALL.SUM.DIFF <- bind_rows(ALL.SUM.DIFF,Q.SUM)
  #   
  #   diff.sum$year = YEARS[i]
  #   diff.sum.all <- bind_rows(diff.sum.all,diff.sum)
  # }


  ###################################################################
  ###################################################################
    # Make some plots
  YEAR <- 2070
  REGS <- c("C.CA","N.CA","OR","WA","S.BC","N.BC","AK")

  ref.agg.summary  <- ref.agg.summary %>% mutate(region = factor(region,levels=nom.agg))
  pred.agg.summary <- pred.agg.summary %>% mutate(region = factor(region,levels=nom.agg))
  diff.agg.summary <- diff.agg.summary %>% mutate(region = factor(region,levels=nom.agg))
  
  
  # pivot diff.sum.all to long form
  # diff.sum.plot <- diff.sum.all %>% dplyr::select(-SAL,-BC.AK,-BC) %>% 
  #                   pivot_longer(.,cols=REGS  ,names_to = "region",values_to = "abund")
  # diff.sum.plot$region <- factor(diff.sum.plot$region,levels=REGS)
  # 
  # all.ref.plot <- ALL.REF.SUM  %>% dplyr::select(-SAL,-BC.AK,-BC,-origin) %>% 
  #                   pivot_longer(.,cols=REGS  ,names_to = "region",values_to = "abund") %>% 
  #                   pivot_wider(.,names_from="quant",values_from="abund")
  # all.ref.plot$region <- factor(all.ref.plot$region,levels=REGS)
  # 
  # all.pred.plot <- ALL.PRED.SUM  %>% dplyr::select(-SAL,-BC.AK,-BC,-origin) %>% 
  #                   pivot_longer(.,cols=REGS  ,names_to = "region",values_to = "abund") %>% 
  #                   pivot_wider(.,names_from="quant",values_from="abund")
  # all.pred.plot$region <- factor(all.pred.plot$region,levels=REGS)
  # 
  # all.diff.plot <- ALL.SUM.DIFF  %>% dplyr::select(-SAL,-BC.AK,-BC,-origin) %>% 
  #                   pivot_longer(.,cols=REGS  ,names_to = "region",values_to = "abund") %>% 
  #                   pivot_wider(.,names_from="quant",values_from="abund")
  # all.diff.plot$region <- factor(all.diff.plot$region,levels=REGS)

#####  
  THM.2 <- theme_bw() +
          theme(plot.margin = unit(c(-0.2,0,-0.1,0), "cm"),
                axis.text.x = element_text(size=7),
                axis.title.y= element_text(size=10),
                plot.tag.position = c(0.05, 1))
  
  
  ts.proj <- ggplot(ref.agg.summary %>% filter(year==YEAR,region %in% REGS)) +
                geom_line(aes(x=region,y=MEAN,group=1)) +
                geom_ribbon(aes(x=region,ymin=q.25,ymax=q.75,group=1),alpha=0.4)+
                geom_ribbon(aes(x=region,ymin=q.05,ymax=q.95,group=1),alpha=0.4)+
                scale_y_continuous(limits = c(0,NA), expand = expand_scale(mult = c(0,0.1)))+
                geom_point(aes(x=region,y=MEAN),shape=21,fill="white") +
                ylab(paste("Predicted Abundance\n(current; thousands)"))+
                scale_x_discrete("" ,labels=rep("",length(XLAB)),expand = expand_scale(mult=c(0.1,0.1))) +
                THM.2
   ts.proj
  
  RE <- ref.agg.summary %>% filter(year==YEAR) %>% mutate(ID="Current")
  PR <- pred.agg.summary %>% filter(year==YEAR) %>% mutate(ID=YEAR)

  PR$ID <- as.character(PR$ID)
  RE.PR <- bind_rows(RE,PR) %>% mutate(id.numb= case_when(region=="C.CA"~1,
                                                          region=="N.CA"~2,
                                                          region=="OR"~3,
                                                          region=="WA"~4,
                                                          region=="S.BC"~5,
                                                          region=="N.BC"~6,
                                                          region=="AK"~7))
  RE.PR$ID <- factor(RE.PR$ID,levels=c("Current",as.character(YEAR)))
  
  ts.proj.both <- ggplot(RE.PR %>% filter(region %in% REGS)) +
    geom_point(aes(x=id.numb,y=MEAN),shape=21) +
    geom_line(aes(x=id.numb,y=MEAN,linetype=ID)) +
    geom_ribbon(aes(x=id.numb,ymin=q.25,ymax=q.75,fill=ID),alpha=0.4)+
    scale_x_continuous(labels=rep("",length(REGS)),breaks=1:length(REGS),expand = expand_scale(mult=c(0.1,0.1)))+
    scale_y_continuous(limits = c(0,NA), expand = expand_scale(mult = c(0,0.1)))+
    scale_fill_viridis_d("",begin=0,end=0.5) +
    scale_linetype_discrete("") +
    ylab(paste("Predicted Abundance\n(thousands)"))+
    xlab("")+
    #scale_x_discrete("" ,labels=rep("",length(XLAB)),expand = expand_scale(mult=c(0.1,0.1))) +
    THM.2+
    labs(tag="B")+
    theme(legend.position = c(0.25, 0.9),
          legend.background = element_rect(fill = "transparent"))
              
  ts.proj.both
  
  
  change.proj.A <-  ggplot(diff.agg.summary %>% filter(year==YEAR,region %in% REGS)) +
      geom_point(aes(x=region,y=q.50),shape="-",size=10) +
      geom_errorbar(aes(x=region,ymin=q.25,ymax=q.75),size=2,width=0)+
      geom_errorbar(aes(x=region,ymin=q.05,ymax=q.95),size=0.8,width=0.1)+
      geom_point(aes(x=region,y=MEAN),shape=21,size=3,fill="white") +
      geom_hline(yintercept = 0,linetype="dashed")+
      ylab(paste0("Change in Abundance\n(",YEAR,"-current; thousands)"))+
      scale_x_discrete("" ,labels=XLAB,expand = expand_scale(mult=c(0.1,0.1))) +
      labs(tag="C")+
      THM.2 
      
  change.proj.B <-  ggplot(diff.agg.summary %>% filter(year==YEAR,region %in% REGS)) +
    geom_boxplot(aes(x=region,ymin=q.05,lower=q.25,middle=q.50,upper=q.75,ymax=q.95),stat="identity")+
    geom_point(aes(x=region,y=MEAN),shape=21,size=2) +
    geom_hline(yintercept = 0,linetype="dashed")+
    ylab(paste0("Change in Abundance\n(",YEAR,"-current; thousands)"))+
    scale_x_discrete("" ,labels=XLAB,expand = expand_scale(mult=c(0.1,0.1))) +
    labs(tag="C")+
    THM.2 
  
  
  ##################### Write large figure to file.
  if(YEAR==2070){
    p.change <- prop.change.2070
  } 
  if(YEAR==2050){
    p.change <- prop.change.2050  
  }
  if(YEAR==2030){
      p.change <- prop.change.2030  
  }
    
  g.grid <- ggplotGrob(p.change)
  g.bar  <- ggplotGrob(abund.bar)
  g.ts     <- ggplotGrob(ts.proj.both)
  g.change <- ggplotGrob(change.proj.B)
  

  g <- gtable(widths=unit(c(4.5,2),c("in")),heights=unit(c(3.5,2.5,2.75),c("in")))
  max.height <- grid::unit.pmax(g.grid$heights, g.bar$heights)
  widths <- list()
  widths[[1]] <- g.grid$widths[2:5]
  widths[[2]] <- g.ts$widths[2:5]
  widths[[3]] <- g.change$widths[2:5]
  max.width <- do.call(grid::unit.pmax,widths)
  g.grid$widths[2:5] <-as.list(max.width)
  g.ts$widths[2:5] <-as.list(max.width)
  g.change$widths[2:5] <-as.list(max.width)
  
  g.grid$heights <- as.list(max.height)
  g.bar$heights <- as.list(max.height)
  g <- gtable_add_grob(g,g.grid,t=1,l=1)
  g <- gtable_add_grob(g,g.bar,t=1,l=2 )
  g <- gtable_add_grob(g,g.ts,t=2,l=1 )
  g <- gtable_add_grob(g,g.change,t=3,l=1 )
  
  plot(g)
  
  quartz(file=paste(base.dir,"/Salmon-Climate/Output plots/Change in distribution ",YEAR," heatmap+ts.jpeg",sep=""),height=9.5,width=6.6,dpi=600,type="jpeg")
    print(plot(g))
  dev.off()
  
  ################### Express changes in % terms
  
  pct.change.by.region <- ref.agg.summary %>% dplyr::select(year,region,ref.med=MEDIAN,ref.25=q.25,ref.75=q.75) %>%
                            full_join(.,diff.agg.summary%>% dplyr::select(year,region,diff.med=MEDIAN,diff.25=q.25,diff.75=q.75)) %>%
                            mutate(change.med.pct=(ref.med+diff.med)/ref.med-1)
                                   
  pct.change.by.region %>% filter(year==2050)                                      
                            
  
  ref.agg.summary 
 
  