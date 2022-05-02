 # Vulnerability and Maturity plotting of Model output

#3 Maturity first

spawn_loc_plot <- spawn_loc
spawn_loc_plot$nom <- as.character(spawn_loc$ocean.region)
spawn_loc_plot$nom[substr(spawn_loc_plot$nom,2,3)=="OR"] <- "OR"
spawn_loc_plot$nom[substr(spawn_loc_plot$nom,3,4)=="VI"] <- "VI"

plot.name <- data.frame(loc.spawn.idx=unique(spawn_loc_plot$loc.spawn.idx))
plot.name$nom <- spawn_loc_plot$nom[match(plot.name$loc.spawn.idx,spawn_loc_plot$loc.spawn.idx)]

##
leave.ocean <- as.matrix(prob_age_year)

pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/"," Leave Ocean ",NAME,".pdf",sep=""),height = 7, width=7)

par(mfrow=c(3,3),mar=c(3,4,0.5,0.5))
y.lim <- c(0,0.75)
text.x <- 0.25
text.y <- y.lim[2]*0.95

for(i in 1:nrow(leave.ocean)){
    A <- barplot(leave.ocean[i,],ylim=y.lim,axes=F,names.arg="")
    axis(1,at=A[,1],1:ncol(leave.ocean),padj=-1,tcl=-0.2)
    axis(2,las=2,hadj=0.7,tcl=-0.2)
    text(x=text.x,y=text.y,plot.name$nom[i],pos=4)
    title(xlab="Age",plot.name$name[i],line=1.5) 
    title(ylab="Prop. leave ocean",plot.name$name[i],line=2) 
    
}
dev.off()

## Vulnerability
vuln_age <- sort(unique(dat.bin$age.month))

pdf(paste(base.dir,"/Orca_Salmon/Output plots/Mixed Model/"," Vulnerability ",NAME,".pdf",sep=""),height = 3.5, width=7)

par(mfrow=c(1,2),mar=c(3,4,0.5,0.5))
y.lim <- c(0,1)
x.lim <- c(0,48)

plot( vuln_mat[1,]~vuln_age,type="b",ylim=y.lim,xlim=x.lim,axes=F,lwd=2,xlab="",ylab="")
par(new=T)
plot( vuln_mat[2,]~vuln_age,type="b",pch=22,lty=2,ylim=y.lim,xlim=x.lim,axes=F,lwd=2,xlab="",ylab="")
axis(1)
axis(2,las=2)
box(bty="o",lwd=2)
title(xlab="Ocean age (months)",line=1.75)
title(ylab="Vulnerability",line=2.5)
legend(lty=c(1,2),x=25,y=0.7,legend=c("Troll South","Troll North"),bty="n",lwd=2,cex=0.8)

plot( vuln_mat[3,]~vuln_age,type="b",ylim=y.lim,xlim=x.lim,axes=F,lwd=2,xlab="",ylab="")
par(new=T)
plot( vuln_mat[4,]~vuln_age,type="b",pch=22,lty=2,ylim=y.lim,xlim=x.lim,axes=F,lwd=2,xlab="",ylab="")
axis(1)
axis(2,las=2)
box(bty="o",lwd=2)
title(xlab="Ocean age (months)",line=1.75)
title(ylab="Vulnerability",line=2.5)
legend(lty=c(1,2),x=25,y=0.7,legend=c("Rec South","Rec North"),bty="n",lwd=2,cex=0.8)

dev.off()

