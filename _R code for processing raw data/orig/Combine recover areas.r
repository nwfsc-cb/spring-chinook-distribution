# 			# Add Yakutat into Northern SEAK.
 		marine.all$rec.area.code[marine.all$rec.area.code == "YAK"]	<- "NSEAK"
			# Drop observations from PWS
 		marine.all	<-	 marine.all[marine.all$rec.area.code != "PWS",]

 		# This file allows vague recovery locations to be divided more precisely to particular ports.
 		samp.site.dat <- read.csv("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Recoveries/recovery_codes_sampling_site.csv")
 		
 		#COL NOR
 		temp.1	<-	marine.all[marine.all$rec.area.code =="COL NOR",]
 		temp.2	<-	marine.all[marine.all$rec.area.code =="COL NOR",]
 		if(nrow(temp.1)>0){
 		  temp.1$est.numb			<- temp.1$est.numb/2
 		  temp.1$count			  <- temp.1$count/2
 		  temp.1$rec.area.code	<-	"COL"
 		  temp.2$est.numb			<- temp.2$est.numb/2
 		  temp.2$count			  <- temp.2$count/2
 		  temp.2$rec.area.code	<-	"NOR"
 		  temp.all	<-	rbind(temp.1,temp.2)
 		  
 		  marine.all	<-	 marine.all[marine.all$rec.area.code != "COL NOR",]
 		  marine.all	<-	 rbind(marine.all,temp.all)
 		}	
 		# OR
 		temp.1	<-	marine.all[marine.all$rec.area.code =="OR",]
 		temp.2	<-	marine.all[marine.all$rec.area.code =="OR",]
 		temp.3	<-	marine.all[marine.all$rec.area.code =="OR",]
 		temp.4	<-	marine.all[marine.all$rec.area.code =="OR",]
 		if(nrow(temp.1)>0){
 		  temp.1$est.numb			<- temp.1$est.numb/4
 		  temp.1$count			<- temp.1$count/4
 		  temp.1$rec.area.code	<-	"COL"
 		  temp.2$est.numb			<- temp.2$est.numb/4
 		  temp.2$count			<- temp.2$count/4
 		  temp.2$rec.area.code	<-	"NOR"
 		  temp.3$est.numb			<- temp.3$est.numb/4
 		  temp.3$count			<- temp.3$count/4
 		  temp.3$rec.area.code	<-	"COR"
 		  temp.4$est.numb			<- temp.4$est.numb/4
 		  temp.4$count			<- temp.4$count/4
 		  temp.4$rec.area.code	<-	"SOR"
 		  
 		  temp.all	<-	rbind(temp.1,temp.2)
 		  temp.all	<-	rbind(temp.all,temp.3)
 		  temp.all	<-	rbind(temp.all,temp.4)
 		  
 		  marine.all	<-	 marine.all[marine.all$rec.area.code != "OR",]
 		  marine.all	<-	 rbind(marine.all,temp.all)
 		}
 		
 		#SWVI NWVI
 		temp.1	<-	marine.all[marine.all$rec.area.code =="SWVI NWVI",]
 		temp.2	<-	marine.all[marine.all$rec.area.code =="SWVI NWVI",]
 		if(nrow(temp.1)>0){
 		  temp.1$est.numb			<- temp.1$est.numb/2
 		  temp.1$count			<- temp.1$count/2
 		  temp.1$rec.area.code	<-	"SWVI"
 		  temp.2$est.numb			<- temp.2$est.numb/2
 		  temp.2$count			<- temp.2$count/2
 		  temp.2$rec.area.code	<-	"NWVI"
 		  temp.all	<-	rbind(temp.1,temp.2)
 		  
 		  marine.all	<-	 marine.all[marine.all$rec.area.code != "SWVI NWVI",]
 		  marine.all	<-	 rbind(marine.all,temp.all)
 		}
 		#SGEO CBC
 		temp.1	<-	marine.all[marine.all$rec.area.code =="SGEO CBC",]
 		temp.2	<-	marine.all[marine.all$rec.area.code =="SGEO CBC",]
 		if(nrow(temp.1)>0){
 		  temp.1$est.numb			<- temp.1$est.numb/2
 		  temp.1$count			<- temp.1$count/2
 		  temp.1$rec.area.code	<-	"SGEO"
 		  temp.2$est.numb			<- temp.2$est.numb/2
 		  temp.2$count			<- temp.2$count/2
 		  temp.2$rec.area.code	<-	"CBC"
 		  temp.all	<-	rbind(temp.1,temp.2)
 		  
 		  marine.all	<-	 marine.all[marine.all$rec.area.code != "SGEO CBC",]
 		  marine.all	<-	 rbind(marine.all,temp.all)
 		}
 		temp.1	<-	marine.all[marine.all$rec.area.code =="NBC CBC",]
 		temp.2	<-	marine.all[marine.all$rec.area.code =="NBC CBC",]
 		if(nrow(temp.1)>0){
 		  temp.1$est.numb			<- temp.1$est.numb/2
 		  temp.1$count			<- temp.1$count/2
 		  temp.1$rec.area.code	<-	"NBC"
 		  temp.2$est.numb			<- temp.2$est.numb/2
 		  temp.2$count			<- temp.2$count/2
 		  temp.2$rec.area.code	<-	"CBC"
 		  temp.all	<-	rbind(temp.1,temp.2)
 		  
 		  marine.all	<-	 marine.all[marine.all$rec.area.code != "NBC CBC",]
 		  marine.all	<-	 rbind(marine.all,temp.all)
 		}
 		
 		# For OREGON AND CALIFORNIA REGIONS, SPLIT VAGUE AREAS BY THE SAMPLING SITE (LANDING LOCATION)
 		# CA (currently fixes a single recover from STAYTON.  May need to revisit if change to earlier year range (ie. before 1979))
 		temp.1	<-	marine.all[marine.all$rec.area.code =="CA",]
 		temp.1$rec.area.code <- "NCA"
 		if(nrow(temp.1)>0){
 		  marine.all	<-	 marine.all[marine.all$rec.area.code != "CA",]
 		  marine.all	<-	 rbind(marine.all,temp.1)
 		}
 		
 		#NCA MEN
 		temp.1	<-	marine.all[marine.all$rec.area.code =="NCA MEN",]
 		temp.1$rec.area.code <- samp.site.dat$ole.area[match(temp.1$sampling_site,samp.site.dat$sampsite)]
 		if(nrow(temp.1)>0){
 		  marine.all	<-	 marine.all[marine.all$rec.area.code != "NCA MEN",]
 		  marine.all	<-	 rbind(marine.all,temp.1)
 		}
 		#SOR NCA
 		temp.1	<-	marine.all[marine.all$rec.area.code =="SOR NCA",]
 		temp.1$rec.area.code <- samp.site.dat$ole.area[match(temp.1$sampling_site,samp.site.dat$sampsite)]
 		
 		if(nrow(temp.1)>0){
 		  marine.all	<-	 marine.all[marine.all$rec.area.code != "SOR NCA",]
 		  marine.all	<-	 rbind(marine.all,temp.1)
 		}
 		#WAC PUSO
 		temp.1	<-	marine.all[marine.all$rec.area.code =="WAC PUSO",]
 		temp.2	<-	marine.all[marine.all$rec.area.code =="WAC PUSO",]
 		if(nrow(temp.1)>0){
 		  temp.1$est.numb			<- temp.1$est.numb/2
 		  temp.1$count			<- temp.1$count/2
 		  temp.1$rec.area.code	<-	"WAC"
 		  temp.2$est.numb			<- temp.2$est.numb/2
 		  temp.2$count			<- temp.2$count/2
 		  temp.2$rec.area.code	<-	"PUSO"
 		  temp.all	<-	rbind(temp.1,temp.2)
 		  
 		  marine.all	<-	 marine.all[marine.all$rec.area.code != "WAC PUSO",]
 		  marine.all	<-	 rbind(marine.all,temp.all)
 		}
		#MEN SFB MOB
			temp.1	<-	marine.all[marine.all$rec.area.code =="MEN SFB MOB",]
			temp.1$rec.area.code <- samp.site.dat$ole.area[match(temp.1$sampling_site,samp.site.dat$sampsite)]
			if(nrow(temp.1)>0){
 				marine.all	<-	 marine.all[marine.all$rec.area.code != "MEN SFB MOB",]
				marine.all	<-	 rbind(marine.all,temp.1)
			}
		#NCA MEN SFB MOB
			temp.1	<-	marine.all[marine.all$rec.area.code =="NCA MEN SFB MOB",]
			temp.1$rec.area.code <- samp.site.dat$ole.area[match(temp.1$sampling_site,samp.site.dat$sampsite)]
			if(nrow(temp.1)>0){
 				marine.all	<-	 marine.all[marine.all$rec.area.code != "NCA MEN SFB MOB",]
				marine.all	<-	 rbind(marine.all,temp.1)
			}


#####################################################################################
##### END Combine Areas
#####################################################################################
