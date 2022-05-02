library(viridis)
setwd(code.dir)
source("multiplot.r")

### File for plotting process errors

epsilon.dat <- data.frame(ocean.region=REL$ocean.region,
                          release_year=REL$release_year,
                          ocean.region.numb=paste(REL$ocean.region,1:nrow(REL),sep="."),
                          year.numb = (REL$brood_year +2) - (min(Output$YEARS.RELEASE)+1),
                          epsilon)
epsilon.dat <- melt(epsilon.dat,id.var=c("ocean.region","release_year","ocean.region.numb","year.numb"))
epsilon.dat$model.time <- as.numeric(substr(epsilon.dat$variable,2,3))
epsilon.dat$overall.model.time <- epsilon.dat$year.numb * 4 + epsilon.dat$model.time


pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/Process error ",NAME,".pdf",sep=""),height = 5, width=10,onefile=T)

#Across all releases (in order)
p1    <-  ggplot(data=epsilon.dat) + 
          geom_boxplot(aes(factor(model.time),value)) +
          theme_bw()+
          geom_hline(aes(yintercept=mean(value)))+
          geom_hline(aes(yintercept=0),linetype=2) 


YLIM <- c(min(epsilon.dat$value),max(epsilon.dat$value))

for(i in 1:nrow(spawn_loc)){
  p1    <-  ggplot(data=epsilon.dat[epsilon.dat$ocean.region==as.character(spawn_loc$ocean.region[i]),]) + 
    geom_boxplot(aes(factor(model.time),value)) +
    scale_y_continuous(limits=YLIM) +
    theme_bw() +
    geom_hline(aes(yintercept=mean(value))) +
    geom_hline(aes(yintercept=0),linetype=2) +
    ggtitle(spawn_loc$ocean.region[i])+
    ylab("Process Error (epsilon)")

  print(p1)
}


YEAR <- sort(unique(REL$release_year))
for(i in 1:length(YEAR)){
  p1    <-  ggplot(data=epsilon.dat[epsilon.dat$release_year==YEAR[i],]) + 
    geom_boxplot(aes(factor(model.time),value)) +
    scale_y_continuous(limits=YLIM) +
    theme_bw() +
    geom_hline(aes(yintercept=mean(value))) +
    geom_hline(aes(yintercept=0),linetype=2) +
    ggtitle(YEAR[i])+
    ylab("Process Error (epsilon)")
  
  print(p1)
}
 
#### MAKE HEATMAPS FOR ALL RELEASES

LIM <- c(-max(abs(epsilon.dat$value)),max(abs(epsilon.dat$value)))

ggplot(epsilon.dat, aes(ocean.region, model.time)) + 
          geom_tile(aes(fill = value)) +
          scale_fill_gradientn(colours=c("blue",grey(0.9),"red"),limits=LIM) +
          theme_bw()

for(i in 1:nrow(spawn_loc)){
  p <- ggplot(epsilon.dat[epsilon.dat$ocean.region == as.character(spawn_loc$ocean.region[i]),],
                          aes(ocean.region.numb, model.time)) + 
    geom_tile(aes(fill = value)) +
    scale_fill_gradientn(colours=c("blue",grey(0.9),"red"),limits=LIM) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
    ggtitle(spawn_loc$ocean.region[i])
  #ylab("Process Error (epsilon)")
  print(p)
}

dev.off()

###################################
###################################
###################################
###################################
###################################
####################################

pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/Process error 2 ",NAME,".pdf",sep=""),height = 5, width=10,onefile=T)
  #### MAKE HEATMAPS FOR ALL RELEASES
  
for(i in 1:nrow(spawn_loc)){
  p <- ggplot(epsilon.dat[epsilon.dat$ocean.region == as.character(spawn_loc$ocean.region[i]),],
              aes(ocean.region.numb, overall.model.time)) + 
    geom_tile(aes(fill = value)) +
    scale_fill_gradientn(colours=c("blue",grey(0.9),"red"),limits=LIM) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
    ggtitle(spawn_loc$ocean.region[i])
  #ylab("Process Error (epsilon)")
  print(p)
}

dev.off()



###################################################
###################################################
###################################################
###################################################
###################################################
### PUBLICATION PLOT
###################################################
###################################################
###################################################
###################################################
###################################################


epsilon.dat$model.time <- as.numeric(as.character(epsilon.dat$model.time))

epsilon.dat2 <- epsilon.dat
epsilon.dat2$ocean.region <- as.character(epsilon.dat2$ocean.region)
epsilon.dat2$ocean.region[epsilon.dat2$ocean.region == "SWVI"] <- "SWVI  "
epsilon.dat2$ocean.region[epsilon.dat2$ocean.region == "COR"]  <- "COR   "

epsilon.dat2$ocean.region <- factor(epsilon.dat2$ocean.region,
                                   levels = c("SWVI  ", "SGEO","PUSO","WAC","UPCOL","MCOL","COL",
                                              "NOR","COR   ","SOR","NCA","SFB"))
LIM=c(-1.1,1.1)
COL = viridis(4,begin=0,end=0.6)                    
xBreaks <- seq(2,19,by=2)
xLAB <- paste(c(rep(c("Sum","Wint"),4),"Sum"),"(",seq(2,18,by=2),")",sep="")

temp <- epsilon.dat2[epsilon.dat2$ocean.region =="SWVI  " |
                      epsilon.dat2$ocean.region =="SGEO" |
                      epsilon.dat2$ocean.region =="PUSO" |
                      epsilon.dat2$ocean.region =="WAC",]
temp <- summarise(group_by(temp,ocean.region,model.time),MEAN=mean(value),SD = sd(value))
temp <- temp[temp$model.time<19,]
colnames(temp)[grep("ocean",colnames(temp))] <- "Region"

p1 <- ggplot(temp) +
      geom_ribbon(aes(x=model.time,ymin = MEAN - SD, ymax = MEAN + SD,fill=Region),alpha=0.2)+
      geom_line(aes(y=MEAN,x=model.time,colour=Region),size=1.1)+
      geom_hline(yintercept=0,linetype=2,size=1.1 ) +
      scale_y_continuous(limits=LIM) +
      scale_x_continuous(breaks=xBreaks,labels=xLAB)+
      scale_fill_manual(values=COL) +
      scale_color_manual(values=COL) +
      annotate("text", x = 1, y = LIM[2], label = "a)") +
      labs(y=expression("Process error("*epsilon*")"),x=" ")+
      theme_bw() +
    theme(axis.text.x = element_text(angle = 00, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),#legend.position="none",
        panel.border=element_rect(colour="black",size=1.5),
        plot.margin=unit(c(0.1, 0.1, 0.05, 0.01), "lines"))
p1      

temp <- epsilon.dat2[epsilon.dat2$ocean.region =="COL" |
                      epsilon.dat2$ocean.region =="MCOL" |
                      epsilon.dat2$ocean.region =="UPCOL" |
                      epsilon.dat2$ocean.region =="NOR",]
temp <- summarise(group_by(temp,ocean.region,model.time),MEAN=mean(value),SD = sd(value))
temp <- temp[temp$model.time<19,]
colnames(temp)[grep("ocean",colnames(temp))] <- "Region"

p2 <- ggplot(temp) +
  geom_ribbon(aes(x=model.time,ymin = MEAN - SD, ymax = MEAN + SD,fill=Region),alpha=0.2)+
  geom_line(aes(y=MEAN,x=model.time,colour=Region),size=1.1)+
  geom_hline(yintercept=0,linetype=2,size=1.1 ) +
  scale_y_continuous(limits=LIM) +
  scale_fill_manual(values=COL) +
  scale_color_manual(values=COL) +
  scale_x_continuous(breaks=xBreaks,labels=xLAB)+
  annotate("text", x = 1, y = LIM[2], label = "b)") +
  labs(y=expression("Process error("*epsilon*")"),x=" ")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),#legend.position="none",
      panel.border=element_rect(colour="black",size=1.5),
      plot.margin=unit(c(0.1, -0.09, 0.05, 0.01), "lines"))

p2      

temp <- epsilon.dat2[epsilon.dat2$ocean.region =="COR   " |
                      epsilon.dat2$ocean.region =="SOR" |
                      epsilon.dat2$ocean.region =="NCA" |
                      epsilon.dat2$ocean.region =="SFB",]
temp <- summarise(group_by(temp,ocean.region,model.time),MEAN=mean(value),SD = sd(value))
temp <- temp[temp$model.time<19,]
colnames(temp)[grep("ocean",colnames(temp))] <- "Region"

p3 <- ggplot(temp) +
  geom_ribbon(aes(x=model.time,ymin = MEAN - SD, ymax = MEAN + SD,fill=Region),alpha=0.2)+
  geom_line(aes(y=MEAN,x=model.time,colour=Region),size=1.1)+
  geom_hline(yintercept=0,linetype=2,size=1.1 ) +
  scale_y_continuous(limits=LIM) +
  scale_fill_manual(values=COL) +
  scale_color_manual(values=COL) +
  scale_x_continuous(breaks=xBreaks,labels=xLAB)+
  annotate("text", x = 1, y = LIM[2], label = "c)") +
    labs(y=expression("Process error("*epsilon*")"),x="Season (Model season number)")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5),plot.title = element_text(hjust = 0,vjust=0,color="white",size=rel(0.9)),#legend.position="none",
        panel.border=element_rect(colour="black",size=1.5),
        plot.margin=unit(c(0.1, 0.12, 0.05, 0.01), "lines"))
p3      


### MAKE PLOTS of Process Error Time-series
quartz(file=paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/Process error TIME SERIES ",NAME,".pdf",sep=""),height = 8, width=6,dpi=300,type="pdf")
  Layout= matrix(c(1,2,3),nrow=3,ncol=1)
  multiplot(p1,p2,p3 ,layout= Layout)
dev.off()

