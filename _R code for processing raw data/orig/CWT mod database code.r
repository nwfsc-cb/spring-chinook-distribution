#### SALMON CWT RECOVERY CODE


setwd("/Users/ole.shelton/Documents/Science/Active projects/Killer Whale/OceanHarvestData")

dat.1 <- read.csv("OceanRecoveriesSourced1973-2000.csv",header=T)
dat.2 <- read.csv("OceanRecoveriesSourced2001-2013.csv",header=T)

dat.1	<-	 dat.1[-c(1079059),]

dat.1.temp	<-	dat.1[,c("species",
					"brood_year",
					"run",
					"run_year",
					"recovery_date",
					"recovery_date_type",
					"record_code",
					"recovery_location_code",
					"length",
					"fishery",
					"gear",
					"adclip_selective_fishery",
					"release_location_rmis_region",
					"release_location_rmis_basin",  
					"hatchery_location_name",
					"stock_location_name",
					"hatchery_location_code",
					"stock_location_code",
					"last_release_date_year",
                    "format_version", 
                    "reporting_agency",
                    "sampling_agency",
                    "recovery_id")]    
dat.2.temp	<-	dat.2[,c("species",
					"brood_year",
					"run",
					"run_year",
					"recovery_date",
					"recovery_date_type",
					"record_code",
					"recovery_location_code",
					"length",
					"fishery",
					"gear",
					"adclip_selective_fishery",
					"release_location_rmis_region",
					"release_location_rmis_basin",  
					"hatchery_location_name",
					"stock_location_name",
					"hatchery_location_code",
					"stock_location_code",
					"last_release_date_year",
                    "format_version", 
                    "reporting_agency",
                    "sampling_agency",
                    "recovery_id")]    

dat.1.temp[,4]	<-	 as.numeric(as.character(dat.1.temp[,4]))
dat.1.temp[,5]	<-	 as.numeric(as.character(dat.1.temp[,5]))
dat.1.temp[,9]	<-	 as.numeric(as.character(dat.1.temp[,9]))

dat.all	<-	rbind(dat.1.temp,dat.2.temp)


dat.chin	<-	dat.all[dat.all$species == 1,]
dat.coho	<-	dat.all[dat.all$species == 2,]
dat.steel	<-	dat.all[dat.all$species == 3,]

write.csv(dat.chin,file="Ocean_Recover_Chinook_1973-2013.csv",row.names=F)
write.csv(dat.coho,file="Ocean_Recover_Coho_1973-2013.csv",row.names=F)
write.csv(dat.steel,file="Ocean_Recover_Steelhead_1973-2013.csv",row.names=F)