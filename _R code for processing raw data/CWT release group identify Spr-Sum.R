### CWT releases

## Identify rivers, releases of interest.

setwd("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Releases/")

all.release.dat	<-	read.csv("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Releases/all chinook releases.csv")
all.release.dat$hatchery_location_code	<- as.character(all.release.dat$hatchery_location_code)

### NEED NOTE ON HOW I IDENTIFIED THESE RELEASES IN PARTICULAR
chosen.release	<-	read.csv("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Releases/CLIMATE/river releases fall chinook CA+COL+PUSO CLIMATE.csv")
chosen.release$hatchery_location_code	<- as.character(chosen.release$hatchery_location_code)
chosen.release$stock_location_code	<- as.character(chosen.release$stock_location_code)
chosen.release$release_location_name	<-	as.character(chosen.release$release_location_name)

BROOD.YEAR.RANGE	<- c(1975,2010)

focal.releases		<-	NULL
for(i in 1:nrow(chosen.release)){

	all.release.dat$release.year			    <-	as.numeric(substr(all.release.dat$first_release_date,1,4))
	all.release.dat$first.release.month		<-	as.numeric(substr(all.release.dat$first_release_date,5,6))
	all.release.dat$last.release.month		<-	as.numeric(substr(all.release.dat$last_release_date,5,6))

	if(chosen.release$release_location_name[i] == ""){
		temp	<- all.release.dat[all.release.dat$hatchery_location_code == chosen.release$hatchery_location_code[i] &
		          all.release.dat$stock_location_code == chosen.release$stock_location_code[i] &
		          all.release.dat$study_integrity !="W" &
		          all.release.dat$study_integrity !="D" &
							all.release.dat$run == chosen.release$Run[i] &
							all.release.dat$brood_year >= BROOD.YEAR.RANGE[1] &
							all.release.dat$brood_year <= BROOD.YEAR.RANGE[2] &
							all.release.dat$first.release.month >= chosen.release$month.start[i] &
							all.release.dat$last.release.month <= chosen.release$month.stop[i] ,]
	}else{
  	temp	<- all.release.dat[all.release.dat$hatchery_location_code == chosen.release$hatchery_location_code[i] &
	            all.release.dat$stock_location_code == chosen.release$stock_location_code[i] &
							all.release.dat$study_integrity !="W" &
							all.release.dat$study_integrity !="D" &
							all.release.dat$release_location_name == chosen.release$release_location_name[i] &
							all.release.dat$run == chosen.release$Run[i] &
							all.release.dat$brood_year >= BROOD.YEAR.RANGE[1] &
							all.release.dat$brood_year <= BROOD.YEAR.RANGE[2] &
							all.release.dat$first.release.month >= chosen.release$month.start[i] &
							all.release.dat$last.release.month <= chosen.release$month.stop[i] ,]
	}

	if(nrow(temp) > 0 ){
	  temp	<-	temp[is.na(temp$tag_code_or_release_id)==F,]
	  temp$ID	<-	chosen.release$ID_name[i]
	  temp$cwt.released	<-	rowSums(temp[,c("cwt_1st_mark_count","cwt_2nd_mark_count")],na.rm=T)
	  temp$ocean.region 	<-	chosen.release$Ocean.region[i]
	  temp$river 			<-	chosen.release$River[i]

  	trim	<-	temp[,c("ID","river","ocean.region","run","stock_location_code","hatchery_location_code","tag_code_or_release_id",
						"cwt.released","brood_year","release.year","first.release.month","last.release.month",
						"release_stage","study_type","tag_reused","comments")]

	  focal.releases	<-	rbind(focal.releases,trim)
	}  
	
}

colnames(focal.releases)[which(colnames(focal.releases) == "tag_code_or_release_id")]	<- "tag_code"



## REMOVE THESE RELEASE GROUPS - DISEASED.
 # focal.releases <- focal.releases[focal.releases$tag_code != "063833" &
 # 									focal.releases$tag_code != "063838" &
 # 
 # 									  
 # 									   									focal.releases$tag_code != "051538" &
# 									focal.releases$tag_code != "051539" &
# 									focal.releases$tag_code != "051535" &
# 									focal.releases$tag_code != "051534" &
# 									focal.releases$tag_code != "051537" &
# 									focal.releases$tag_code != "074511" &
# 									focal.releases$tag_code != "074513" &
# 									focal.releases$tag_code != "074512" &
# 									focal.releases$tag_code != "074516" &
# 									focal.releases$tag_code != "074515" &
# 									focal.releases$tag_code != "074514" &
# 									focal.releases$tag_code != "081619" &
# 									focal.releases$tag_code != "081624" &
# 									focal.releases$tag_code != "081623", ]

write.csv(focal.releases,
          file = "/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Releases/CLIMATE/Tag codes fall chinook CA+COL+PUSO CLIMATE.csv",row.names=F)



# 074627                                                                    IHN POSTIVE
# 051536                                             EARLY RELEASE - BACT. GILL DISEASE
# 051538                                             EARLY RELEASE - BACT. GILL DISEASE
# 051539                                             EARLY RELEASE - BACT. GILL DISEASE
# 051535                                             EARLY RELEASE - BACT. GILL DISEASE
# 051534                                             EARLY RELEASE - BACT. GILL DISEASE
# 051537                                             EARLY RELEASE - BACT. GILL DISEASE
# 
# 074511                                                        HAD SOME ICH AT RELEASE
# 074513                                                       HAD SOME ICH  AT RELEASE
# 074512                                                        HAD SOME ICH AT RELEASE
# 074516                                                        HAD SOME ICH AT RELEASE
# 074515                                                        HAD SOME ICH AT RELEASE
# 074514                                                        HAD SOME ICH AT RELEASE
# 
#  081619                 BILTON EXP.- ACCELERATED REARED; HIGH MORTS SO NOT SIZE GRADED
#  081624 				BILTON EXP.- ACCELERATED REARED; HIGH MORTS SO NOT SIZE GRADED
#  081623                 BILTON EXP.- ACCELERATED REARED; HIGH MORTS SO NOT SIZE GRADED
 
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
# chosen.release	<-	read.csv("/Users/ole.shelton/Documents/Science/Active projects/Killer Whale/data/Releases/river releases.csv")
# chosen.release$hatchery_location_code	<- as.character(chosen.release$hatchery_location_code)
# chosen.release$release_location_name	<-	as.character(chosen.release$release_location_name)
# 
# i=1
# 
# # Explore

 	all.release.dat$release.year			    <-	as.numeric(substr(all.release.dat$first_release_date,1,4))
 	all.release.dat$first.release.month		<-	as.numeric(substr(all.release.dat$first_release_date,5,6))
	all.release.dat$last.release.month		<-	as.numeric(substr(all.release.dat$last_release_date,5,6))


names(all.release.dat)

seven <- all.release.dat[all.release.dat$run == 7 &
                           all.release.dat$study_integrity =="N" &
                           all.release.dat$brood_year >= BROOD.YEAR.RANGE[1] &
                           all.release.dat$brood_year <= BROOD.YEAR.RANGE[2],]

eight <- all.release.dat[all.release.dat$run == 8 &
                           all.release.dat$study_integrity =="N" &
                           all.release.dat$brood_year >= BROOD.YEAR.RANGE[1] &
                           all.release.dat$brood_year <= BROOD.YEAR.RANGE[2],]

temp[,c("run","tag_code_or_release_id","brood_year","first_release_date","last_release_date","avg_weight","cwt_1st_mark_count","cwt_2nd_mark_count",
         "release_location_name","hatchery_location_name","stock_location_name","hatchery_location_code")]

dim(seven)	


 temp	<- all.release.dat[all.release.dat$hatchery_location_code == chosen.release$hatchery_location_code[i],]
 dim(temp)

 hatch <- "3F42001  371381 H"  # Upper Columbia Brights (1987-1990) run = 7
 hatch <- "5F33201  H1     21" # Bonnevill Hatchery Columbia Upriver Brights, run = 8
 hatch <- "6FCSJMOK MRFI" ## Mokulme
 hatch <- "6FCSAAMN NBFH" # Nimbus
 hatch <- "3F42001  290131 H02" # Little White Salmon River (Near Bonneville)
 hatch <- "3F42001  440001 H04" # Wells hatchery (Columbia)
 
 
 
 hatch <- "6FCSAFEA FRFH"
 
 
 	temp	<- all.release.dat[ all.release.dat$release_location_rmis_region=="FRTH" &
 	          #all.release.dat$hatchery_location_code == hatch &
 	          all.release.dat$run == 3 &
 						#	all.release.dat$study_integrity =="N" &
 							all.release.dat$brood_year >= BROOD.YEAR.RANGE[1] &
 							all.release.dat$brood_year <= BROOD.YEAR.RANGE[2],]

 	A<- temp[,c("run","tag_code_or_release_id","brood_year","first_release_date","last_release_date","avg_weight","cwt_1st_mark_count","cwt_2nd_mark_count",
 	        "release_location_name","hatchery_location_name","stock_location_name","hatchery_location_code")]
 	A<-A[order(A$brood_year,A$first_release_date),]
  A 	
 	aggregate(A$cwt_1st_mark_count,by=list(A$brood_year),sum)
 	
 	
 	aggregate(temp$cwt_1st_mark_count,by=list(temp$brood_year),sum)
 	
 temp$run

 	temp	<- all.release.dat[all.release.dat$hatchery_location_code == chosen.release$hatchery_location_code[i] &
 							all.release.dat$study_integrity =="N" &
 							all.release.dat$run == chosen.release$Run[i] &
 							all.release.dat$brood_year >= BROOD.YEAR.RANGE[1] &
 							all.release.dat$brood_year <= BROOD.YEAR.RANGE[2],]

 temp
 hist(temp$brood_year,breaks=seq(1970,1995,by=1))

 plot(avg_weight~first.release.month,data=temp)
 plot(avg_weight~last.release.month,data=temp)

 temp$first.release.month

 temp$last.release.month

#Run
# 
# Code to indicate run of this release group; If present, must match one of the following:
# run
# ’1’ =Spring
# ’2’=Summer
# ’3’=Fall (includes type S Coho)
# ’4’=Winter
# ’5’=Hybrid
# ’6’=Landlocked
# ’7’=Late Fall (includes type N Coho)
# ’8’=Late Fall Upriver Bright Chinook
# 
# 21
# Release Stage
# 1
# No
# Lookup
# Code indicating stage of majority of release group at point of release; If present, must match one of the following:
# release_stage
# ’Z’=Zygote (eyed eggs)
# ’E’=Emergent fry
# ’F’=Fed fry
# ’G’=Fingerling
# ’V’=Advanced fingerling
# ’Y’=Yearling
# ’P’=Pre-smolt
# ’S’=Smolt
# ’A’=Adult
# ’M’=Multiple release stages
# 
# 
# Rearing Type
# Code indicating most prevalent rearing method for this release group; If present, must match one of the following:
# rearing_type
# ’H’=Hatchery reared fish (includes any wild fish reared in the hatchery)
# ’W’=Wild fish
# ’M’=Mixed hatchery & wild (downstream migrant or marine tagging)
# ’U’=Unknown (unavailable from release agency)
# 
# Study Type
# Code indicating type of study reflected by release group; If present, must match one of the following:
# study_type
# ’E’=Experimental
# ’P’=Production
# ’B’=Both experimental and production
# ’O’=Other
# ’K’=PSC key indicator stocks
# ’I’=Other index streams
