### THIS IS FOR EXAMINING ESTIMATED FISHING MORTALITY ALONG THE WHOLE COAST

### PLOT Effort heatmaps
plot.heatmap <- function(temp,Title,MONTH.STRUCTURE){
  
  z.lim	<- c(1e-5,max(temp))
  x.lab	=	LOCATIONS$location.name
  col.br<- colorRampPalette(c("blue", "cyan", "yellow", "red"))
  par(mfrow=c(1,1),oma=c( 0,0,0,4) )
  image(z=t(temp),x=1:nrow(t(temp)),y=LOCATIONS$location.number,axes=F,ylab="",xlab="",
        col=1,zlim=c(-9999,z.lim[1]))
  image(z=t(temp),x=1:nrow(t(temp)),y=LOCATIONS$location.number,axes=F,ylab="",xlab="",
        col=col.br(32),zlim=z.lim,add=T)
  box(bty="o",lwd=2)
  axis(2,las=2,at=1:nrow(temp),labels=x.lab)
  AT = seq(2,ncol(temp),by=4)
  axis(1,las=2,at=seq(2,ncol(temp),by=4),labels=years.recover[1:length(AT)],tcl=0,hadj=1.5)
  
  if(MONTH.STRUCTURE=="FOUR" | MONTH.STRUCTURE=="SPRING"){
    LAB <- rep(c("Spr","Sum","Fall","Wint"),N_years_recover) [1:ncol(temp)]
  }
  if(MONTH.STRUCTURE=="FRAM"){
    LAB <- rep(c("Sum","Fall","Wint","Spr"),N_years_recover) [1:ncol(temp)]
  } 
  
  axis(1,las=2,at=1:ncol(temp),labels= LAB ,tcl=0,cex.axis=0.5,hadj=0.4)
  title(main=Title)
  par(mfrow=c(1,1),oma=c( 0,0,0,0) )
  ticks <- seq(min(z.lim),max(z.lim),length.out=6)
  
  image.plot(temp,legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list( at= ticks,labels=c("1e-5",round(ticks[2:6],3))))
}


#############
quartz(file=paste(base.dir,"/spring-chinook-distribution/Output plots/F_mortality heatmap ",NAME,".pdf",sep=""),height = 8.5, width=11,type="pdf",dpi=300)
  plot.heatmap( temp=t(as.matrix(F_troll_array)),"Estimated Troll Mortality",MONTH.STRUCTURE)
  plot.heatmap( temp=t(as.matrix(F_rec_array)),"Estimated Recreational Mortality",MONTH.STRUCTURE)
  plot.heatmap( temp=t(as.matrix(F_treaty_array)),"Estimated Treaty Mortality",MONTH.STRUCTURE)
  plot.heatmap( temp=t(as.matrix(F_HAKE_ASHOP)),"Estimated Hake ASHOP Mortality",MONTH.STRUCTURE)
  plot.heatmap( temp=t(as.matrix(F_HAKE_SHORESIDE)),"Estimated Hake Shoreside Mortality",MONTH.STRUCTURE)
  plot.heatmap( temp=t(as.matrix(F_POLLOCK_GOA)),"Estimated Pollock GOA Shoreside Mortality",MONTH.STRUCTURE)
  plot.heatmap( temp=t(as.matrix(F_ROCKFISH_AK)),"Estimated Rockfish AK Shoreside Mortality",MONTH.STRUCTURE)
  plot.heatmap( temp=t(as.matrix(F_troll_array + F_rec_array + F_treaty_array +
                                   F_HAKE_ASHOP + F_HAKE_SHORESIDE + F_POLLOCK_GOA + F_ROCKFISH_AK)),
                                "Estimated Total Mortality",MONTH.STRUCTURE)
dev.off()



#######################################################################
######## MAKE TIME SERIES PLOT OF CHANGES TO LOG-Q for all gear types
#######################################################################

A     <- log_q_troll_start
SLOPE =  log_q_troll_slope
Q_INT = q_int
YR    <- q_year_vec*10 + years.recover[1]


start <- c(rep("log_q_troll_start",length(log_q_troll_start)),
           "log_q_treaty_start",
           rep("log_q_rec_start",length(log_q_rec_start)),
           "log_q_rec_can_start")
slope <- c(rep("log_q_troll_slope",length(log_q_troll_start)),
            "log_q_troll_slope",
            rep("log_q_rec_slope",length(log_q_rec_start)),
            "log_q_rec_can_slope")
ID.start <- c(1:length(log_q_troll_start),1,1:length(log_q_rec_start),1)
ID.slope <- c(rep(length(log_q_troll_slope),length(log_q_troll_start)),1,rep(1,length(log_q_rec_start)),1)

# Hake and other constant log_q_vlues
start.const <- c("log_q_rec_can_irec_start",
                 rep("log_q_hake_ashop_start",length(log_q_hake_ashop_start)),
                 rep("log_q_hake_shoreside_start",length(log_q_hake_shoreside_start)))
ID.start.const <- c(1:length(log_q_rec_can_irec_start),
                    1:length(log_q_hake_ashop_start),
                    1:length(log_q_hake_shoreside_start))
y.lim <- c(-15,-5)
x.lim= c(min(YR),max(YR))
LEG <- c(paste("Troll",1:length(log_q_troll_start)),
         "Treaty Troll",
         paste("US Rec",1:length(log_q_rec_start)),
             "CAN Rec",
              "CAN iRec",
         paste("Hake ASHOP",1:length(log_q_hake_ashop_start)),
              "Hake Shore")


quartz(file=paste0(base.dir,"/spring-chinook-distribution/Output Plots/F Log-q time-series", NAME,".pdf"),dpi=300, width = 8,height=6, type="pdf")

par(mfrow= c(1,1),mar=c(4,4,0.5,0.5))
COL <- c(rep(1,length(log_q_troll_start)),
         2,
         rep(3,length(log_q_rec_start)),
         4)
PCH <- c(21:(21+length(log_q_troll_start)-1),
         21,
         21:(21+length(log_q_rec_start)-1),
         21)
  
  COL.const <- c(4, # Can iREC
         rep(5,length(log_q_hake_ashop_start)),
         rep(6,length(log_q_hake_shoreside_start)))
  PCH.const <- c(22, # Can iREC
         rep(21:(21+length(log_q_hake_ashop_start)-1)),
         21)
  for(i in 1:length(start)){
    WWW <- eval(as.name(start[i]))[ID.start[i]] -  
                (eval(as.name(start[i]))[ID.start[i]] / (1 + exp(- eval(as.name(slope[i]))[ID.slope[i]] * (q_year_vec - q_int))) - eval(as.name(start[i]))[ID.start[i]]) 
    if(i>1){par(new=T)}
    plot(WWW~YR,xlab="",ylab="",type="b",xlim=x.lim,ylim=y.lim,col=COL[i],axes=F,lwd=1.5,pch=PCH[i])
  }

  # do the constant q for hake and othes
  for(j in 1:length(start.const)){
    par(new=T)   
  # if(start.const[j] == "log_q_hake_ashop_start"){
  #     if(ID.start.const[j]==1){
  #       WWW <- rep(eval(as.name(start.const[j]))[ID.start.const[j]],ashop_year_break-1)
  #       YR.temp <- YR[1:(ashop_year_break-1)]
  #     }else{
  #       WWW <- rep(eval(as.name(start.const[j]))[ID.start.const[j]],length(YR)-ashop_year_break+1)
  #       YR.temp <- YR[ashop_year_break:length(YR)]
  #     }
  #   plot(WWW~YR.temp,xlab="",ylab="",type="b",xlim=x.lim,ylim=y.lim,col=COL.const[j],axes=F,lwd=1.5,pch=PCH.const[j])
  # }else{
    WWW <- rep(eval(as.name(start.const[j]))[ID.start.const[j]],length(YR))
    plot(WWW~YR,xlab="",ylab="",type="b",xlim=x.lim,ylim=y.lim,col=COL.const[j],axes=F,lwd=1.5,pch=PCH.const[j])
  #}

}

axis(2,las=2)
axis(1)
box(bty="o",lwd=2)
title(xlab="Year",ylab="Catchability (log(q))")

legend(x=1980,y=-5,legend=LEG,
       col=c(COL,COL.const),
       pch = c(PCH,PCH.const),
       lwd=2,cex=0.7)
dev.off()


### Repeat for log_q time-series



