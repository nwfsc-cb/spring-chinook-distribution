############# Writing plots to file.

pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/Effort heatmap.pdf",sep=""),height = 8.5, width=11)

  plot.heatmap( temp=log(t(as.matrix(K_troll_flat))+1e-15,10),"Troll Effort (Boat Days or Landings (AK))")
  plot.heatmap( log(t(as.matrix(K_treaty_flat))+1e-15,10),"Treaty Troll Effort (Deliveries)")
  plot.heatmap( log(t(as.matrix(K_rec_flat + K_rec_PUSO_flat))+1e-15,10),"Rec Effort, US (Angler-Days)")
  plot.heatmap( log(t(as.matrix(K_rec_can_flat))+1e-15,10),"Rec Effort, Canada (Boat Trips)")
  plot.heatmap( log(t(as.matrix(K_rec_can_irec_flat))+1e-15,10),"Rec Effort, iRec, Canada (license holder trips)")
  
  
  if(TRAWL.US == "TRUE"){
    plot.heatmap( log(t(as.matrix(K_hake_ashop_flat))+1e-15,10),"ASHOP Trawl effort (boat days proxy)")
    plot.heatmap( log(t(as.matrix(K_hake_shoreside_flat))+1e-15,10),"Shoreside Trawl effort (boat days proxy)")
  }
  if(TRAWL.AK == "TRUE"){
    plot.heatmap( log(t(as.matrix(K_pollock_shoreside_flat))+1e-15,10),"Pollock Shoreside Trawl effort (boat days proxy)")
    plot.heatmap( log(t(as.matrix(K_rockfish_AK_shoreside_flat))+1e-15,10),"AK Rockfish Trawl effort (boat days proxy)")
  }

dev.off()

# For each Season plot effort through time

SEAS <- c("summer","fall","winter","spring")

for(i in 1:length(SEAS)){
  THESE <- grep(SEAS[i],rownames(K_troll_flat))
  
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/Effort heatmap ",SEAS[i],".pdf",sep=""),height = 8.5, width=11)
  plot.heatmap( temp=log(t(as.matrix(K_troll_flat[THESE,]))+1e-15,10),paste0(SEAS[i]," Troll Effort (Boat Days)"),all.seas=FALSE)
  plot.heatmap( log(t(as.matrix(K_treaty_flat[THESE,]))+1e-15,10),paste0(SEAS[i]," Treaty Troll Effort (Deliveries)"),all.seas=FALSE)
  plot.heatmap( log(t(as.matrix(K_rec_flat[THESE,] + K_rec_PUSO_flat[THESE,]))+1e-15,10),paste0(SEAS[i]," Rec Effort, US (Angler-Days)"),all.seas=FALSE)
  plot.heatmap( log(t(as.matrix(K_rec_can_flat[THESE,]))+1e-15,10),paste0(SEAS[i]," Rec Effort, Canada (Boat Trips)"),all.seas=FALSE)
  plot.heatmap( log(t(as.matrix(K_rec_can_irec_flat[THESE,]))+1e-15,10),paste0(SEAS[i]," Rec Effort, iRec, Canada (license holder trips)"),all.seas=FALSE)

  if(TRAWL.US == "TRUE"){
    plot.heatmap( log(t(as.matrix(K_hake_ashop_flat[THESE,]))+1e-15,10),paste0(SEAS[i]," ASHOP Trawl effort (boat days proxy)"),all.seas=FALSE)
    plot.heatmap( log(t(as.matrix(K_hake_shoreside_flat[THESE,]))+1e-15,10),paste0(SEAS[i]," Shoreside Trawl effort (boat days proxy)"),all.seas=FALSE)
  }
  if(TRAWL.AK == "TRUE"){
    plot.heatmap( log(t(as.matrix(K_pollock_shoreside_flat[THESE,]))+1e-15,10),paste0(SEAS[i],"Pollock Shoreside Trawl effort (boat days proxy)"),all.seas=FALSE)
    plot.heatmap( log(t(as.matrix(K_rockfish_AK_shoreside_flat[THESE,]))+1e-15,10),paste0(SEAS[i],"AK Rockfish Trawl effort (boat days proxy)"),all.seas=FALSE)
  }
dev.off()
}


######################
# MAKE PLOTS OF OBSERVED RECOVERIES.
#####
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/All Troll + Treaty observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
  for(i in 1:N.REL){
    plot.heatmap.nolog( temp.all=C_troll_true +C_treaty_true,id=i)
  }
dev.off()

pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/All Rec observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.heatmap.nolog( temp.all=C_rec_true,id=i)
}
dev.off()

pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/All Trawl ASHOP observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.heatmap.nolog( temp.all=C_hake_ashop_true,id=i)
}
dev.off()

pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/All Trawl Shoreside observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.heatmap.nolog( temp.all=C_hake_shoreside_true,id=i)
}
dev.off()

pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/All Pollock Trawl Shoreside observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.heatmap.nolog( temp.all=C_pollock_GOA_true,id=i)
}
dev.off()

pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/All AK rockfish Trawl Shoreside observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.heatmap.nolog( temp.all=C_rockfish_AK_true,id=i)
}
dev.off()

# Sum across all fishery fleets
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/All FLEETS observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.heatmap.nolog( temp.all= C_troll_true +
                                C_treaty_true +        
                                C_rec_true +
                                C_hake_ashop_true +
                                C_hake_shoreside_true +
                                C_pollock_GOA_true +
                                C_rockfish_AK_true,id=i)
}
dev.off()


##############################################################################################
##############################################################################################
##############################################################################################
## MAKE PLOTS OF SAMPLING FRACTION.
#############

## Make a few plots of the sampling effort used for each area and time...
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/Sample Fraction heatmap.pdf",sep=""),height = 8.5, width=11)

  A <- Lambda_troll_flat;  A[is.na(A)] <- -99
  B <- Lambda_troll_flat_int;  B[is.na(B)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(A)),"Troll sampling fraction ")
  plot.heatmap.samp.frac( temp=t(as.matrix(B)),"Troll sampling fraction + interpolated ")

  A <- Lambda_rec_flat;  A[is.na(A)] <- -99
  B <- Lambda_rec_flat_int;  B[is.na(B)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(A)),"Rec sampling fraction ")
  plot.heatmap.samp.frac( temp=t(as.matrix(B)),"Rec sampling fraction + interpolated ")
  
  A <- Lambda_treaty_flat;  A[is.na(A)] <- -99
  B <- Lambda_treaty_flat_int;  B[is.na(B)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(A)),"Treaty sampling fraction ")
  plot.heatmap.samp.frac( temp=t(as.matrix(B)),"Treaty sampling fraction + interpolated ")

  
  A <- Lambda_hake_ashop_flat_int;  A[is.na(A)] <- -99
  B <- Lambda_hake_ashop_flat;  B[is.na(B)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(B)),"ASHOP sampling fraction")
  plot.heatmap.samp.frac( temp=t(as.matrix(A)),"ASHOP sampling fraction + interpolated")
  
  A <- Lambda_hake_shoreside_flat_int;  A[is.na(A)] <- -99
  B <- Lambda_hake_shoreside_flat;  B[is.na(B)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(B)),"Shoreside sampling fraction")
  plot.heatmap.samp.frac( temp=t(as.matrix(A)),"Shoreside sampling fraction + interpolated ")
 
  #A <- Lambda_pollock_GOA_flat_ing;  A[is.na(A)] <- -99
  B <- Lambda_pollock_GOA_flat;  B[is.na(B)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(B)),"Pollock GOA Shoreside sampling fraction")
  #plot.heatmap.samp.frac( temp=t(as.matrix(A)),"Pollock GOA Shoreside sampling fraction + interpolated ")
  
  #A <- Lambda_pollock_GOA_flat_ing;  A[is.na(A)] <- -99
  B <- Lambda_rockfish_AK_flat;  B[is.na(B)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(B)),"Rockfish AK Shoreside sampling fraction")
  #plot.heatmap.samp.frac( temp=t(as.matrix(A)),"Pollock GOA Shoreside sampling fraction + interpolated ")
   
  A <- Lambda_net_flat;  A[is.na(A)] <- -99
  plot.heatmap.samp.frac( temp=t(as.matrix(A)),"Net sampling fraction ")

dev.off()

##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
### Calculate CPUE for all 
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
# Make Plots of CPUE for each fleet.
#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
  for(i in 1:N.REL){
    plot.CPUE.heatmap( temp.all=C_troll_true,effort.all= K_troll_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
  }
dev.off()

#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Rec observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_rec_true,effort.all= K_rec_flat+K_rec_PUSO_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()

#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Rec Can observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_rec_true,effort.all= K_rec_can_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()

#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Rec Can iREC observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_rec_true,effort.all= K_rec_can_irec_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()

#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Treaty observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_treaty_true,effort.all= K_treaty_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()
#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Trawl ASHOP observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_hake_ashop_true,effort.all= K_hake_ashop_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()
#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Trawl Shoreside observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_hake_shoreside_true,effort.all= K_hake_shoreside_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()
#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Pollock Trawl GOA observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_pollock_GOA_true,effort.all= K_pollock_shoreside_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()
#################################
pdf(paste(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Pollock Trawl GOA observations heatmap ",GROUP,".pdf",sep=""),height = 8.5, width=8.5)
for(i in 1:N.REL){
  plot.CPUE.heatmap( temp.all=C_rockfish_AK_true,effort.all= K_rockfish_AK_shoreside_flat,id=i,MONTH.STRUCTURE=MONTH.STRUCTURE)
}
dev.off()
#############################################################################
#############################################################################
#############################################################################




#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
### Prettier Pub Plots - Vestigal from earlier version
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

library(fields)
library(RColorBrewer)

#############

# Plot CPUE for troll fisheries for 3 hatcheries over 2 release years (3 row x 2 column grid)

# id = 235 and 254 is Coleman 1980 and 2005
# id = 109 is Colemen small 1980


# Coleman plots
# setwd("/Users/ole.shelton/GitHub/Orca_Salmon/Output plots/")
# quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Coleman 1980.pdf"),height=4,width=5,dpi=600,type="pdf")
#       plot.CPUE.heatmap2( temp.all=C_troll_true,effort.all= K_troll_flat,id=235)
# dev.off()

# quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Coleman 1999.pdf"),height=4,width=5,dpi=600,type="pdf")
#   plot.CPUE.heatmap2( temp.all=C_troll_true,effort.all= K_troll_flat,id=254)
# dev.off()

# read in files combine into side by side figures
# p1 <- ggdraw() + draw_image(magick::image_read_pdf(paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Coleman 1980.pdf"), 
#                                                    density = 600))
# p2 <- ggdraw() + draw_image(magick::image_read_pdf(paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Coleman 1999.pdf"), 
#                                                    density = 600))
# 
# quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Coleman Combined.pdf"),height=4,width=10,dpi=600,type="pdf")
#   print(plot_grid(p1,p2))
# dev.off()

# LCOL "Big Creek_small" 1980 and 1999 are ID = 34,48
# quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Big Creek 1980.pdf"),height=4,width=5,dpi=600,type="pdf")
#   plot.CPUE.heatmap2( temp.all=C_troll_true,effort.all= K_troll_flat,id=34)
# dev.off()
# 
# quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Big Creek 1999.pdf"),height=4,width=5,dpi=600,type="pdf")
#   plot.CPUE.heatmap2( temp.all=C_troll_true,effort.all= K_troll_flat,id=48)
# dev.off()
# 
# # read in files combine into side by side figures
# p1 <- ggdraw() + draw_image(magick::image_read_pdf(paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Big Creek 1980.pdf"), 
#                                                    density = 600))
# p2 <- ggdraw() + draw_image(magick::image_read_pdf(paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Big Creek 1999.pdf"), 
#                                                    density = 600))
# 
# quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Big Creek Combined.pdf"),height=4,width=10,dpi=600,type="pdf")
#   print(plot_grid(p1,p2))
# dev.off()
# 
# # URB "Priest" 1980 and 1999 are ID = 34,48
# quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Priest 1980.pdf"),height=4,width=5,dpi=600,type="pdf")
#   plot.CPUE.heatmap2( temp.all=C_troll_true,effort.all= K_troll_flat,id=901)
# dev.off()
# 
# quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Priest 1999.pdf"),height=4,width=5,dpi=600,type="pdf")
#   plot.CPUE.heatmap2( temp.all=C_troll_true,effort.all= K_troll_flat,id=920)
# dev.off()
# 
# # read in files combine into side by side figures
# p1 <- ggdraw() + draw_image(magick::image_read_pdf(paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Priest 1980.pdf"), 
#                                                    density = 600))
# p2 <- ggdraw() + draw_image(magick::image_read_pdf(paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Priest 1999.pdf"), 
#                                                    density = 600))
# 
# quartz(file=paste0(base.dir,"/spring-chinook-distribution/Heatmaps/CPUE Troll observations heatmap PUB Priest Combined.pdf"),height=4,width=10,dpi=600,type="pdf")
# print(plot_grid(p1,p2))
# dev.off()
