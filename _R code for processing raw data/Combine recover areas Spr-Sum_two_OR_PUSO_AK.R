# 			# Add Yakutat and PWS into Northern SEAK. (because no effort data in PWS)

    ### ASSIGN PUSO.OUT to be part of WAC or PUSO or PUSO_out (St. of Juan de Fuca)
    marine.all$rec.area.code <- as.character(marine.all$rec.area.code)
    marine.all$rec.area.code[marine.all$rec.area.code =="PUSO.OUT"] <- "PUSO_out"
    marine.all$rec.area.code[marine.all$rec.area.code =="PUSO.IN"]  <- "PUSO"

    # marine.all %>% group_by(rec.area.code) %>% filter(rec.year >= 1993) %>% summarise(TOT = sum(est.numb)) %>% as.data.frame()
    # marine.all %>% filter(rec.area.code == "HSEA")
    
    marine.all$rec.area.code <- as.character(marine.all$rec.area.code)
  	#marine.all	<-	 marine.all[marine.all$rec.area.code != "PWS",]
 		
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
 		temp.2	<-	temp.1
 		#temp.3	<-	marine.all[marine.all$rec.area.code =="OR",]
 		temp.4	<-	temp.1
 		if(nrow(temp.1)>0){
 		  temp.1$est.numb			<- temp.1$est.numb/3
 		  temp.1$count			<- temp.1$count/3
 		  temp.1$rec.area.code	<-	"COL"
 		  temp.2$est.numb			<- temp.2$est.numb/3
 		  temp.2$count			<- temp.2$count/3
 		  temp.2$rec.area.code	<-	"NOR"
 		  # temp.3$est.numb			<- temp.3$est.numb/3
 		  # temp.3$count			<- temp.3$count/3
 		  # temp.3$rec.area.code	<-	"COR"
 		  temp.4$est.numb			<- temp.4$est.numb/3
 		  temp.4$count			<- temp.4$count/3
 		  temp.4$rec.area.code	<-	"SOR"
 		  
 		  temp.all	<-	rbind(temp.1,temp.2)
 		  #temp.all	<-	rbind(temp.all,temp.3)
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
 		if(nrow(temp.1)>0){
 		  temp.1$rec.area.code <- "NCA"
 		  if(nrow(temp.1)>0){
 		    marine.all	<-	 marine.all[marine.all$rec.area.code != "CA",]
 		    marine.all	<-	 rbind(marine.all,temp.1)
 		  }
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
 		# temp.1	<-	marine.all[marine.all$rec.area.code =="WAC PUSO",]
 		# temp.2	<-	marine.all[marine.all$rec.area.code =="WAC PUSO",]
 		# if(nrow(temp.1)>0){
 		#   temp.1$est.numb			<- temp.1$est.numb/2
 		#   temp.1$count			<- temp.1$count/2
 		#   temp.1$rec.area.code	<-	"WAC"
 		#   temp.2$est.numb			<- temp.2$est.numb/2
 		#   temp.2$count			<- temp.2$count/2
 		#   temp.2$rec.area.code	<-	"PUSO"
 		#   temp.all	<-	rbind(temp.1,temp.2)
 		# 
 		#   marine.all	<-	 marine.all[marine.all$rec.area.code != "WAC PUSO",]
 		#   marine.all	<-	 rbind(marine.all,temp.all)
 		# }
 		
 		#WAC PUSO
 		temp.1	<-	marine.all[marine.all$rec.area.code =="WAC PUSO.OUT",]
 		temp.2	<-	marine.all[marine.all$rec.area.code =="WAC PUSO.OUT",]
 		if(nrow(temp.1)>0){
 		  temp.1$est.numb			<- temp.1$est.numb/2
 		  temp.1$count			<- temp.1$count/2
 		  temp.1$rec.area.code	<-	"WAC"
 		  temp.2$est.numb			<- temp.2$est.numb/2
 		  temp.2$count			<- temp.2$count/2
 		  temp.2$rec.area.code	<-	"PUSO_out"
 		  temp.all	<-	rbind(temp.1,temp.2)
 		  
 		  marine.all	<-	 marine.all[marine.all$rec.area.code != "WAC PUSO.OUT",]
 		  marine.all	<-	 rbind(marine.all,temp.all)
 		}
 		
 		#NCA MEN SFB
 		temp.1	<-	marine.all[marine.all$rec.area.code =="NCA MEN SFB",]
 		temp.1$rec.area.code <- samp.site.dat$ole.area[match(temp.1$sampling_site,samp.site.dat$sampsite)]
 		if(nrow(temp.1)>0){
 		  marine.all	<-	 marine.all[marine.all$rec.area.code != "NCA MEN SFB",]
 		  marine.all	<-	 rbind(marine.all,temp.1)
 		}
 		#SFB MOB
 		temp.1	<-	marine.all[marine.all$rec.area.code =="SFB MOB",]
 		temp.1$rec.area.code <-  samp.site.dat$ole.area[match(temp.1$sampling_site,samp.site.dat$sampsite)]
 		if(nrow(temp.1)>0){
 		  marine.all	<-	 marine.all[marine.all$rec.area.code != "SFB MOB",]
 		  marine.all	<-	 rbind(marine.all,temp.1)
 		}
 		#MEN SFB MOB
			temp.1	<-	marine.all[marine.all$rec.area.code =="MEN SFB MOB",]
			temp.1$rec.area.code <-  samp.site.dat$ole.area[match(temp.1$sampling_site,samp.site.dat$sampsite)]
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

  ### Assigning HSEA recoveries to spatial areas based on latitude, longitude designations.
		# High Seas for California to Washington Coast
		# 	hsea.1.lab <- c(35, "MONT",
		#                 36, "MONT",
		#                 37, "MONT",
		#                 38, "SFB",
		#                 39, "MEN",
		#                 40, "MEN",
		#                 41, "NCA",
		#                 42, "SOR",
		#                 43, "SOR",
		#                 44, "SOR",
		#                 45, "NOR",
		#                 46, "COL",
		#                 47, "WAC",
		#                 48, "WAC",
		#                 49, "SWVI",
		#                 50, "NWVI",
		#                 51, "CBC",
		#                 52, "CBC",
		#                 53, "NBC",
		#                 54, "NBC")			
		# 	hsea.1.lab <-	data.frame(matrix(hsea.1.lab,length(hsea.1.lab)/2,2,byrow=T)); colnames(hsea.1.lab) <- c("lat","lab")
				
			### IMPORTANT.  REMOVE ALL OF THE HIGH SEAS AREA 1 becasue we have that data directly from the observer program
			marine.all <- marine.all %>% filter(!grepl("HIGH SEAS 1",recovery_location_name))			
			
			# hsea.1 <- marine.all %>% filter(grepl("HIGH SEAS 1",recovery_location_name)) %>%
			#       mutate(rec.area.code = case_when(grepl("35N|36N|37N",recovery_location_name) ~ "MONT",
			#                                  grepl("38N",recovery_location_name) ~ "SFB",
			#                                  grepl("39N|40N",recovery_location_name) ~ "MEN",
			#                                  grepl("41N",recovery_location_name) ~ "NCA",
			#                                  grepl("42N",recovery_location_name) ~ "NCA SOR",
			#                                  grepl("43N|44N",recovery_location_name) ~ "SOR",
			#                                  grepl("45N",recovery_location_name) ~ "NOR",
			#                                  grepl("46N",recovery_location_name) ~ "COL",
			#                                  grepl("47N|48N",recovery_location_name) ~ "WAC"))
			# 
			# if(nrow(hsea.1)>0){
			#   marine.all <- marine.all %>%  filter(!grepl("HIGH SEAS 1",recovery_location_name)) %>% bind_rows(.,hsea.1)
			# }

			# HSEA 3 is Gulf of Alaska			
			hsea.3 <- marine.GOA.HS

			hsea.3 <- hsea.3 %>% 
			  mutate(rec.area.code = case_when(grepl("610",recovery_location_name) ~ "WAPEN",
			                                   grepl("620",recovery_location_name) ~ "EAPEN",
			                                   grepl("621",recovery_location_name) ~ "EAPEN",
			                                   grepl("630",recovery_location_name) ~ "NWGOA",
			                                   grepl("640",recovery_location_name) ~ "NEGOA",
			                                   grepl("649",recovery_location_name) ~ "PWS",
			                                   grepl("148W|149W|150W|151W|152W|153W",recovery_location_name) ~ "NWGOA",
			                                   grepl("147W",recovery_location_name) ~ "NWGOA NEGOA",
			                                   grepl("144W|145W|146W",recovery_location_name) ~ "NEGOA",
			                                   grepl("154W",recovery_location_name) ~ "EAPEN NWGOA",
			                                   grepl("155W|156W|157W|158W",recovery_location_name) ~ "EAPEN",
			                                   grepl("159W",recovery_location_name) ~ "WAPEN EAPEN",
			                                   grepl("160W|161W|162W|163W|164W|165W|166W|167W|168W|169W",recovery_location_name) ~ "WAPEN",
			                                   grepl("170W|171W|172W|173W",recovery_location_name) ~ "ALEUT"))
			if(nrow(hsea.3)>0){
			  marine.GOA.HS <- hsea.3
			}

			# HSEA 4 is Bering Sea
			hsea.4 <- marine.all %>% filter(grepl("HIGH SEAS 4",recovery_location_name))
			
			hsea.4 <- hsea.4 %>% 
			  mutate(rec.area.code = case_when(grepl("508|509|510|511|512|513|514|515",recovery_location_name) ~ "BER",
			                                   grepl("516|517|518|519|520|521|522|523|530",recovery_location_name) ~ "BER",
			                                   grepl("541|542|543",recovery_location_name) ~ "ALEUT",
			                                   grepl("543",recovery_location_name) ~ "ALEUT",
			                                   grepl("56N|57N|58N",recovery_location_name) ~ "BER",
			                                   grepl("54N 164W|54N 165W|54N 166W",recovery_location_name) ~ "BER",
			                                   grepl("55N 163W|55N 164W|55N 165W|55N 166W|55N 167W|55N 168W",recovery_location_name) ~ "BER"))
			
			if(nrow(hsea.4)>0){
			  marine.all <- marine.all %>%  filter(!grepl("HIGH SEAS 4",recovery_location_name)) %>% bind_rows(.,hsea.4)
			}

			#Get rid of a few split areas based on crappy TRAWL LOCATION DATA
			
			#EAPEN NWGOA
			temp.1	<-	marine.GOA.HS[marine.GOA.HS$rec.area.code =="EAPEN NWGOA",]
			temp.2	<-	temp.1
			if(nrow(temp.1)>0){
			  # temp.1$est.numb			<- temp.1$est.numb/2
			  temp.1$count			  <- temp.1$count/2
			  temp.1$rec.area.code	<-	"EAPEN"
			  # temp.2$est.numb			<- temp.2$est.numb/2
			  temp.2$count			  <- temp.2$count/2
			  temp.2$rec.area.code	<-	"NWGOA"
			  temp.all	<-	rbind(temp.1,temp.2)
			  
			  marine.GOA.HS	<-	 marine.GOA.HS[marine.GOA.HS$rec.area.code != "EAPEN NWGOA",]
			  marine.GOA.HS	<-	 rbind(marine.GOA.HS,temp.all)
			}			

			#NWGOA NEGOA
			temp.1	<-	marine.GOA.HS[marine.GOA.HS$rec.area.code =="NWGOA NEGOA",]
			temp.2	<-	temp.1
			if(nrow(temp.1)>0){
			  #temp.1$est.numb			<- temp.1$est.numb/2
			  temp.1$count			  <- temp.1$count/2
			  temp.1$rec.area.code	<-	"EAPEN"
			  #temp.2$est.numb			<- temp.2$est.numb/2
			  temp.2$count			  <- temp.2$count/2
			  temp.2$rec.area.code	<-	"NWGOA"
			  temp.all	<-	rbind(temp.1,temp.2)
			  
			  marine.GOA.HS	<-	 marine.GOA.HS[marine.GOA.HS$rec.area.code != "NWGOA NEGOA",]
			  marine.GOA.HS	<-	 rbind(marine.GOA.HS,temp.all)
			}			
			
			
			### REPLACE COPPER WITH NEGOA, REPLACE PWS with NEGOA, REPLACE CISS with NWGOA.
			### All are from gillnet fleets inshore.  Will be removed from the model when gillnet fleets are removed
			
			marine.all <- marine.all %>% mutate(rec.area.code = ifelse(rec.area.code=="COPPER","NEGOA",rec.area.code),
			                                    rec.area.code = ifelse(rec.area.code=="PWS","NEGOA",rec.area.code),
			                                    rec.area.code = ifelse(rec.area.code=="CISS","NWGOA",rec.area.code))
			
				
			# make vector to search for in highseas 3
			# boundaries of EAPEN and WAPEN is 159W,
			  #NWGOA and EAPEN is 154W, 
			  # NWGOA and NWGOA is 147W.
			  # CAPE SUCKLING is about 144W
			  # Yakutat bay is about 140W
			

#####################################################################################
##### END Combine Areas
#####################################################################################
