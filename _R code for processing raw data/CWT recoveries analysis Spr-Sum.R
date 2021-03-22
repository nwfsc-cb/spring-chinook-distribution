base.dir <- "/Users/ole.shelton/GitHub"
### Pull out recoveries for the tag groups of interest.

GROUP <- "COL"  ### Options "CA", "COL"

################### Go get the tag code file
 # this data comes from the "CWT release group identify.r"
	tag.dat	<- read.csv(paste("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Releases/Climate/Tag codes fall chinook ",GROUP," CLIMATE.csv",sep=""))
	
		A	<-	aggregate(tag.dat$cwt.released,by=list(ID=tag.dat$ID,ocean.region=tag.dat$ocean.region,brood_year=tag.dat$brood_year,
		                                            release_year=tag.dat$release.year,release_month=tag.dat$first.release.month),sum)
		B	<-	data.frame(expand.grid(brood_year=1977:1992,ID=sort(unique(A$ID))))
		C	<-	merge(B,A,all=TRUE)
		D	<-	aggregate(tag.dat$ocean.region,by=list(ocean.region=tag.dat$ocean.region,ID=tag.dat$ID),length)
		C$ocean.region	<-	D$ocean.region[match(C$ID,D$ID)]
		C	<-	C[order(C$ocean.region,C$ID,C$brood_year),]
		C$x[is.na(C$x)==T]	<-	0

		length(C$x[C$x>0])
		sum(C$x)
		sum.release.by.region	<-	aggregate(C$x,by=list(brood_year=C$brood_year,ocean.region =C$ocean.region),sum)

		#### Summarize releases
		colnames(A)[which(colnames(A)=="x")] <- "N.released"
		A <- A[order(A$ID,A$release_year,A$release_month),]
		B <- aggregate(A$release_month,by=list(ID=A$ID,ocean.region=A$ocean.region,brood_year=A$brood_year, release_year=A$release_year),median)
		colnames(B)[which(colnames(B)=="x")] <- "Median.month.release"
		C <- aggregate(A$N.released,by=list(ID=A$ID,ocean.region=A$ocean.region,brood_year=A$brood_year,release_year=A$release_year),sum)
		colnames(C)[which(colnames(C)=="x")] <- "N.released"
		D <- merge(B,C)
		
		releases <- list(releases=D)
		### Save Results to File
		save(releases,file=paste("/Users/ole.shelton/GitHub/Salmon-Climate/Processed Data/Releases ",GROUP,".RData",sep=""))	    
		
###########################################################################################################################################		
	#### Modify the tag code file here, if desired.
	code.all	<- unique(tag.dat$tag_code)

	# Go get the recovery codes from weitkamp 
	dat.loc.key	<- read.csv("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Recoveries/recovery codes-wietkamp+shelton 04-2017.csv",header=T)

################## First combine all of the recovery data files
setwd(paste("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Recoveries/Fall Chinook Climate/",GROUP,sep=""))
files	<- dir()

all.code.recoveries	<- NULL

if(GROUP=="CA"){
  for(i in 1:length(files)){
  	temp	<-	read.csv(files[i])
      if(i==4){
      temp.tag <- temp$tag_code
      temp.tag[nchar(temp.tag)==5] <- paste("0",temp.tag[nchar(temp.tag)==5],sep="")
      temp$tag_code <- temp.tag
      }
      temp.1 <- temp[is.na(match(temp$tag_code,code.all))==F,]
      all.code.recoveries	<-	rbind(temp.1,all.code.recoveries)
      
  }
}
  	
if(GROUP=="COL"){
    for(i in 1:length(files)){
      temp	<-	read.csv(files[i])

      temp.1 <- temp[is.na(match(temp$tag_code,code.all))==F,]
      all.code.recoveries	<-	rbind(temp.1,all.code.recoveries)
    }
}  

	all.code.recoveries$rec.year	<-	as.numeric(substr(all.code.recoveries$recovery_date,1,4))
	all.code.recoveries$rec.month	<-	as.numeric(substr(all.code.recoveries$recovery_date,5,6))
	all.code.recoveries$rec.day		<-	as.numeric(substr(all.code.recoveries$recovery_date,5,6))
	
	class(all.code.recoveries$tag_code)

##########################################################################################
### CONSOLIDATE RECOVERIES 
##########################################################################################
### Break into Marine and Freshwater recoveries
recover.marine	<-	all.code.recoveries[substr(all.code.recoveries$recovery_location_code,2,2)=="M",]
recover.fresh	  <-	all.code.recoveries[substr(all.code.recoveries$recovery_location_code,2,2)=="F",]

#######---  Marine ---####################################################################
	#Merge recovery code names with areas used.
	#### Assign recoveries into regions delineated by Weitkamp 2002 and 2010
	recover.marine$rec.area.code <- NA
	recover.marine$rec.area.code <- dat.loc.key$Rec.area.Shelton[match(recover.marine$recovery_location_code, dat.loc.key$location_code)]

	  # THIS IS A SPOT TO CHECK for missing locations.
#  	A <-recover.marine[is.na(recover.marine$rec.area.code)==T,]
#  	B <-recover.marine[is.na(recover.marine$rec.area.code)==F,]
# 		unique(A$recovery_location_code[A$fishery ==10])
#  	C	<-	aggregate(A$recovery_location_code,
#  	              by=list(rec_loc_code=A$recovery_location_code,rec_loc_name=A$recovery_location_name,fish=A$fishery),length)
#  		write.csv(C,file="temp recover.codes.csv")

	# Area, year, month, release group, fishery
	marine.1	<- aggregate(recover.marine$estimated_number,by=list(
							tag_code	= recover.marine$tag_code,
							fishery		= recover.marine$fishery,	
							rec.year  = recover.marine$rec.year,
							rec.month = recover.marine$rec.month,
							rec.area.code = recover.marine$rec.area.code,
							estimation.level = recover.marine$estimation_level,
							sampling_site = recover.marine$sampling_site,
# 							period.type = recover.marine$period_type,
# 							period = recover.marine$period,
							sample.type = recover.marine$sample_type),
							length)
	# Area, year, month, release group, fishery
	marine.2	<- aggregate(recover.marine$estimated_number,by=list(
							tag_code	  = recover.marine$tag_code,
							fishery		= recover.marine$fishery,	
							rec.year  = recover.marine$rec.year,
							rec.month = recover.marine$rec.month,
							rec.area.code = recover.marine$rec.area.code,
							estimation.level = recover.marine$estimation_level,
							sampling_site = recover.marine$sampling_site,
			#				period.type = recover.marine$period_type,
			#				period = recover.marine$period,
							sample.type = recover.marine$sample_type),
							sum,na.rm=T)
	 
	marine.2		<- marine.2[order(marine.2$tag_code,marine.2$rec.year,marine.2$fishery,marine.2$rec.month),]
	## Merge count with release group
		marine.merge.count	<- merge(tag.dat,marine.1,by="tag_code")
		marine.merge.count	<- marine.merge.count[order(marine.merge.count$tag_code,
													marine.merge.count$fishery,
													marine.merge.count$rec.year,
													marine.merge.count$rec.month),]
		colnames(marine.merge.count)[ncol(marine.merge.count)]	<-	"count"
		
	## Merge with release group
		marine.merge	<- merge(tag.dat,marine.2,by="tag_code")
		marine.merge	<- marine.merge[order(marine.merge$tag_code,marine.merge$fishery,marine.merge$rec.year,marine.merge$rec.month),]
		colnames(marine.merge)[ncol(marine.merge)]	<-	"est.numb"

		marine.all		<-	merge(marine.merge.count,marine.merge)
				
		marine.all		<-	marine.all[marine.all$sample.type !=5 &marine.all$sample.type !=4,]
		marine.all		<-	marine.all[marine.all$est.numb > 0,]
	##
		marine.all$frac.samp<-marine.all$count/marine.all$est.numb
    marine.all$frac.samp[marine.all$frac.samp > 1] <- 1
#####################################################################################
##### Combine Areas
#####################################################################################
	source(paste(base.dir,"/Salmon-Climate/_R code for processing raw data/Combine recover areas CLIMATE.r",sep=""))
#####################################################################################

# 		
# 		marine.by.hatchery	<-	marine.by.hatchery[order(marine.by.hatchery$ID,
# 															marine.by.hatchery$fishery,
# 															marine.by.hatchery$rel.year,
# 															marine.by.hatchery$rec.year,
# 															marine.by.hatchery$rec.month),]
# 		# Eliminate zero observations
# 		marine.by.hatchery	<-	marine.by.hatchery[marine.by.hatchery$x > 0 ,]
# 		
# 		
# 		marine.by.hatchery[marine.by.hatchery$rec.area.code == "OR",] # fair number of observations throughout the 80s... unclear about what to do with these (all troll)m
# 		marine.by.hatchery[marine.by.hatchery$rec.area.code == "PWS",] # few... all in gill net fisheries.
# 		marine.by.hatchery[marine.by.hatchery$rec.area.code == "CA",] # None after 1976
# 		marine.by.hatchery[marine.by.hatchery$rec.area.code == "BC",] # none after 1977
# 

	######################################################################################
		marine.by.hatchery.numb	<-	aggregate(marine.all$est.numb,by=list(
							ID		=	marine.all$ID,
							brood.year = marine.all$brood_year,
							rel.year = marine.all$release.year,
							rel.month = marine.all$first.release.month,
							ocean.region = marine.all$ocean.region,
							fishery		= marine.all$fishery,	
							rec.year  = marine.all$rec.year,
							rec.month = marine.all$rec.month,
							rec.area.code = marine.all$rec.area.code,
							estimation.level = marine.all$estimation.level),
							sum,na.rm=T)
		colnames(marine.by.hatchery.numb)[ncol(marine.by.hatchery.numb)]	<-	"est.numb"

		marine.by.hatchery.count	<-	aggregate(marine.all$count,by=list(
							ID		=	marine.all$ID,
							brood.year = marine.all$brood_year,
							rel.year = marine.all$release.year,
							rel.month = marine.all$first.release.month,
							ocean.region = marine.all$ocean.region,
							fishery		= marine.all$fishery,	
							rec.year  = marine.all$rec.year,
							rec.month = marine.all$rec.month,
							rec.area.code = marine.all$rec.area.code,
							estimation.level = marine.all$estimation.level),
							sum,na.rm=T)
		colnames(marine.by.hatchery.count)[ncol(marine.by.hatchery.count)]	<-	"count"

		marine.by.hatchery.frac	<-	aggregate(marine.all$frac.samp,by=list(
							ID		 	=	marine.all$ID,
							brood.year = marine.all$brood_year,
							rel.year 	= marine.all$release.year,
							rel.month = marine.all$first.release.month,
							ocean.region = marine.all$ocean.region,
							fishery		= marine.all$fishery,	
							rec.year 	= marine.all$rec.year,
							rec.month 	= marine.all$rec.month,
							rec.area.code 	= marine.all$rec.area.code,
							estimation.level = marine.all$estimation.level),
							median,na.rm=T)
		colnames(marine.by.hatchery.frac)[ncol(marine.by.hatchery.frac)]	<-	"median.frac.samp"

		marine.by.hatchery	<-	merge(marine.by.hatchery.numb,marine.by.hatchery.count)
		marine.by.hatchery	<-	merge(marine.by.hatchery,marine.by.hatchery.frac)

		####################################################
		####################################################
		####################################################
		####################################################
		####################################################
		####################################################
		### --- MAKE R DATA FOR USE IN ESTIMATION
		####################################################
		####################################################
		####################################################
		####################################################
		####################################################
		####################################################
		catch.by.region<- list()
		ocean.region <- sort(unique(marine.by.hatchery$ocean.region))
		for(i in 1:length(ocean.region)){
		  temp <- marine.by.hatchery[marine.by.hatchery$ocean.region == ocean.region[i],]
		  reg.ID <- sort(unique(temp$ID))
		  ID<- list()
		  for(j in 1:length(reg.ID)){
		    ID[[j]] <- list(temp[temp$ID == reg.ID[j],])
		  }
		  names(ID) <- reg.ID  
		  catch.by.region[[i]] <- ID
		}
		names(catch.by.region) <- ocean.region
    
		ocean.recover = list(ocean.recover=catch.by.region,dat=marine.by.hatchery)
    save(ocean.recover,file=paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/Releases and Recoveries/Ocean Recoveries.RData",sep=""))	    

    ####################################################
    ####################################################
    ####################################################
    ####################################################
    ####################################################
    ####################################################
    ####################################################
    ####################################################
    ####################################################
    ####################################################
#######---  FRESHWATER ---#################################################################
    ####################################################
    ####################################################
    ####################################################
    ####################################################
    ####################################################
    ####################################################
    ####################################################
    ####################################################
    ####################################################
    ####################################################
    
  #Merge recovery code names with areas used.
	#### Assign recoveries into regions delineated by Weitkamp 2002 and 2010
	recover.fresh$rec.area.code <- NA
  # get rid of sample type =5 to avoid double counting
    recover.fresh <- recover.fresh[recover.fresh$sample_type !=5,]
  
    # Area, year, month, release group, fishery
    fresh.1	<- aggregate(recover.fresh$estimated_number,by=list(
      tag_code  = recover.fresh$tag_code,
      fishery	  = recover.fresh$fishery,	
      rec.year  = recover.fresh$rec.year,
      #rec.month = recover.fresh$rec.month,
      rec.area.code = recover.fresh$recovery_location_code,
      #							estimation.level = recover.fresh$estimation_level,
      #							period.type = recover.fresh$period_type,
      #							period = recover.fresh$period,
       							sample.type = recover.fresh$sample_type),
      length)
    
	# Area, year, month, release group, fishery
	fresh.2	<- aggregate(recover.fresh$estimated_number,by=list(
							tag_code  = recover.fresh$tag_code,
							fishery	  = recover.fresh$fishery,	
							rec.year  = recover.fresh$rec.year,
 							#rec.month = recover.fresh$rec.month,
 							rec.area.code = recover.fresh$recovery_location_code),
#							estimation.level = recover.fresh$estimation_level,
#							period.type = recover.fresh$period_type,
#							period = recover.fresh$period,
# 							sample.type = recover.fresh$sample_type),
							sum,na.rm=T)
	fresh.2		<- fresh.2[order(fresh.2$tag_code,fresh.2$rec.year,fresh.2$fishery),]#fresh.2$rec.month),]
	
	################
	##### ADD IN AUXILIARY DATA FROM TRINITY AND KLAMATH RIVERS
	################
	
	nca.escape <- read.csv(paste("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Recoveries/Klamath river recoveries/KTFALL.KOHMNLM.01Sept.csv",sep=""))
	nca.escape <- nca.escape[nca.escape$HATCH_NAME=="TRH" | nca.escape$HATCH_NAME=="IGH",]
	
	nca.escape$AGE_OLE <- nca.escape$AGE + 1
	#	nca.escape$AGE_OLE[nca.escape$HATCH_NAME=="TRH" | nca.escape$HATCH_NAME=="IGH"] <-  nca.escape$AGE_OLE[nca.escape$HATCH_NAME=="TRH" | nca.escape$HATCH_NAME=="IGH"] + 1 # Why is trinity hatchery different?
	nca.escape <- nca.escape[,c("TAG_CODE","BROOD_YR","RELE_YEAR","AGE_OLE","CWT_RELE","RIVER_ESC","HATCHLENS")]
	nca.escape$est.numb <- nca.escape$RIVER_ESC
	nca.escape$est.numb[which(nca.escape$RIVER_ESC < nca.escape$HATCHLENS)] <- nca.escape$HATCHLENS[which(nca.escape$RIVER_ESC < nca.escape$HATCHLENS)]
	
	nca.escape$rec.year <- nca.escape$BROOD_YR + nca.escape$AGE_OLE -1
	nca.escape.trim <- nca.escape[,c("TAG_CODE","est.numb","rec.year")]
	colnames(nca.escape.trim)[1] <- "tag_code"
	
	fresh.nca <- merge(tag.dat,nca.escape.trim)
	fresh.nca$rec.area.code <- NA
	fresh.nca$sample.type <- NA
	fresh.nca$count <- fresh.nca$est.numb / 3
	fresh.nca$fishery <- 9999
	
	## Merge count with release group
	fresh.merge.count	<- merge(tag.dat,fresh.1,by="tag_code")
	fresh.merge.count	<- fresh.merge.count[order(fresh.merge.count$tag_code,
	                                               fresh.merge.count$fishery,
	                                               fresh.merge.count$rec.year
	                                              ),] #  fresh.merge.count$rec.month
	colnames(fresh.merge.count)[ncol(fresh.merge.count)]	<-	"count"
	
	## Merge with release group
	fresh.merge	<- merge(tag.dat,fresh.2,by="tag_code")
	fresh.merge	<- fresh.merge[order(fresh.merge$tag_code,fresh.merge$fishery,fresh.merge$rec.year),] #
	colnames(fresh.merge)[ncol(fresh.merge)]	<-	"est.numb"
	
	fresh.all		<-	merge(fresh.merge.count,fresh.merge)
	
	# Remove NCA recoveries from the RMIS database and replace with recoveries from Will S. Klamath dataset.
	fresh.nca <- fresh.nca[,colnames(fresh.all)]
	fresh.all <- fresh.all[fresh.all$ocean.region!= "NCA",]
	fresh.all <- rbind(fresh.all,fresh.nca)
	
	######################################################################################
	# Make one file for determining time of river entry
	# Area, year, month, release group, fishery
	fresh.river	<- aggregate(recover.fresh$estimated_number,by=list(
	  tag_code  = recover.fresh$tag_code,
	  fishery	  = recover.fresh$fishery,	
	  rec.year  = recover.fresh$rec.year,
	  rec.month = recover.fresh$rec.month,
	  rec.area.code = recover.fresh$recovery_location_code,
	  #							estimation.level = recover.fresh$estimation_level,
	  #							period.type = recover.fresh$period_type,
	  #							period = recover.fresh$period,
	  sample.type = recover.fresh$sample_type),
	  sum,na.rm=T)
	
	fresh.merge.river	<- merge(tag.dat,fresh.river,by="tag_code")
	fresh.merge.river	<- fresh.merge.river[order(fresh.merge.river$tag_code,
	                                             fresh.merge.river$fishery,
	                                             fresh.merge.river$rec.year
	),] #  fresh.merge.count$rec.month
	colnames(fresh.merge.river)[ncol(fresh.merge.river)]	<-	"est.numb"
	fresh.river <- fresh.merge.river
	
		fresh.by.hatchery.river	<-	aggregate(fresh.river$est.numb,by=list(
	  ID		=	fresh.river$ID,
	  brood.year = fresh.river$brood_year,
	  rel.year = fresh.river$release.year,
	  rel.month = fresh.river$first.release.month,
	  ocean.region = fresh.river$ocean.region,
	  fishery		= fresh.river$fishery,	
	  rec.year  = fresh.river$rec.year,
	  rec.month = fresh.river$rec.month),
	  #		  rec.area.code = fresh.river$rec.area.code,
	  #		  estimation.level = fresh.river$estimation.level),
	  sum,na.rm=T)
	colnames(fresh.by.hatchery.river)[ncol(fresh.by.hatchery.river)]	<-	"est.numb"
	
	######################################################################################
#	fresh.all		<-	fresh.all[fresh.all$sample.type !=5 &fresh.all$sample.type !=4,]
#	fresh.all		<-	fresh.all[fresh.all$est.numb > 0,]

	# Break the fresh recoveries into two groups.  One with estimated numbers, one without.
	fresh.zero <- fresh.all[fresh.all$est.numb==0,]
	fresh.pos <- fresh.all[fresh.all$est.numb > 0,]
	
	fresh.pos$frac.samp<-fresh.pos$count/fresh.pos$est.numb
	fresh.pos$frac.samp[fresh.pos$frac.samp > 1] <- 1

			######################################################################################
	# Deal with the positives first	
	fresh.by.hatchery.numb	<-	aggregate(fresh.pos$est.numb,by=list(
		  ID		=	fresh.pos$ID,
		  brood.year = fresh.pos$brood_year,
		  rel.year = fresh.pos$release.year,
		  rel.month = fresh.pos$first.release.month,
		  ocean.region = fresh.pos$ocean.region,
		  fishery		= fresh.pos$fishery,	
		  rec.year  = fresh.pos$rec.year),
#		  rec.month = fresh.pos$rec.month),
#		  rec.area.code = fresh.pos$rec.area.code,
#		  estimation.level = fresh.pos$estimation.level),
		  sum,na.rm=T)
		colnames(fresh.by.hatchery.numb)[ncol(fresh.by.hatchery.numb)]	<-	"est.numb"
		
		fresh.by.hatchery.count	<-	aggregate(fresh.pos$count,by=list(
		  ID		=	fresh.pos$ID,
		  brood.year = fresh.pos$brood_year,
		  rel.year = fresh.pos$release.year,
		  rel.month = fresh.pos$first.release.month,
		  ocean.region = fresh.pos$ocean.region,
		  fishery		= fresh.pos$fishery,	
		  rec.year  = fresh.pos$rec.year),
#		  rec.month = fresh.pos$rec.month),
#		  rec.area.code = fresh.pos$rec.area.code,
#		  estimation.level = fresh.pos$estimation.level),
		  sum,na.rm=T)
		colnames(fresh.by.hatchery.count)[ncol(fresh.by.hatchery.count)]	<-	"count"
		
		fresh.by.hatchery.frac	<-	aggregate(fresh.pos$frac.samp,by=list(
		  ID		 	=	fresh.pos$ID,
		  brood.year = fresh.pos$brood_year,
		  rel.year 	= fresh.pos$release.year,
		  rel.month = fresh.pos$first.release.month,
		  ocean.region = fresh.pos$ocean.region,
		  fishery		= fresh.pos$fishery,	
		  rec.year 	= fresh.pos$rec.year),
#		  rec.month 	= fresh.pos$rec.month),
#		  rec.area.code 	= fresh.pos$rec.area.code,
#		  estimation.level = fresh.pos$estimation.level),
		  median,na.rm=T)
		colnames(fresh.by.hatchery.frac)[ncol(fresh.by.hatchery.frac)]	<-	"median.frac.samp"
		
		fresh.by.hatchery	<-	merge(fresh.by.hatchery.numb,fresh.by.hatchery.count)
		fresh.by.hatchery	<-	merge(fresh.by.hatchery,fresh.by.hatchery.frac)
	
	# Deal with the zeros second
		fresh.by.hatchery.count.zero	<-	aggregate(fresh.zero$count,by=list(
		  ID		=	fresh.zero$ID,
		  brood.year = fresh.zero$brood_year,
		  rel.year = fresh.zero$release.year,
		  rel.month = fresh.zero$first.release.month,
		  ocean.region = fresh.zero$ocean.region,
		  fishery		= fresh.zero$fishery,	
		  rec.year  = fresh.zero$rec.year),
		  sum,na.rm=T)
		colnames(fresh.by.hatchery.count.zero)[ncol(fresh.by.hatchery.count.zero)]	<-	"count.zero"

		fresh.by.hatchery	<-	merge(fresh.by.hatchery,fresh.by.hatchery.count.zero,all=T)
		fresh.w.fishery   <- fresh.by.hatchery
	 
		######################################################################################
		#### COMBINE THE VARIOUS FISHERIES
		######################################################################################
		
		# Deal with the positives first	
		fresh.by.hatchery.numb	<-	aggregate(fresh.pos$est.numb,by=list(
		  ID		=	fresh.pos$ID,
		  brood.year = fresh.pos$brood_year,
		  rel.year = fresh.pos$release.year,
		  rel.month = fresh.pos$first.release.month,
		  ocean.region = fresh.pos$ocean.region,
	#	  fishery		= fresh.pos$fishery,	
		  rec.year  = fresh.pos$rec.year),
		  #		  rec.month = fresh.pos$rec.month),
		  #		  rec.area.code = fresh.pos$rec.area.code,
		  #		  estimation.level = fresh.pos$estimation.level),
		  sum,na.rm=T)
		colnames(fresh.by.hatchery.numb)[ncol(fresh.by.hatchery.numb)]	<-	"est.numb"
		
		fresh.by.hatchery.count	<-	aggregate(fresh.pos$count,by=list(
		  ID		=	fresh.pos$ID,
		  brood.year = fresh.pos$brood_year,
		  rel.year = fresh.pos$release.year,
		  rel.month = fresh.pos$first.release.month,
		  ocean.region = fresh.pos$ocean.region,
	#	  fishery		= fresh.pos$fishery,	
		  rec.year  = fresh.pos$rec.year),
		  #		  rec.month = fresh.pos$rec.month),
		  #		  rec.area.code = fresh.pos$rec.area.code,
		  #		  estimation.level = fresh.pos$estimation.level),
		  sum,na.rm=T)
		colnames(fresh.by.hatchery.count)[ncol(fresh.by.hatchery.count)]	<-	"count"
		
		fresh.by.hatchery.frac	<-	aggregate(fresh.pos$frac.samp,by=list(
		  ID		 	=	fresh.pos$ID,
		  brood.year = fresh.pos$brood_year,
		  rel.year 	= fresh.pos$release.year,
		  rel.month = fresh.pos$first.release.month,
		  ocean.region = fresh.pos$ocean.region,
	#	  fishery		= fresh.pos$fishery,	
		  rec.year 	= fresh.pos$rec.year),
		  #		  rec.month 	= fresh.pos$rec.month),
		  #		  rec.area.code 	= fresh.pos$rec.area.code,
		  #		  estimation.level = fresh.pos$estimation.level),
		  median,na.rm=T)
		colnames(fresh.by.hatchery.frac)[ncol(fresh.by.hatchery.frac)]	<-	"median.frac.samp"
		
		fresh.by.hatchery	<-	merge(fresh.by.hatchery.numb,fresh.by.hatchery.count)
		fresh.by.hatchery	<-	merge(fresh.by.hatchery,fresh.by.hatchery.frac)
		
		# Deal with the zeros second
		fresh.by.hatchery.count.zero	<-	aggregate(fresh.zero$count,by=list(
		  ID		=	fresh.zero$ID,
		  brood.year = fresh.zero$brood_year,
		  rel.year = fresh.zero$release.year,
		  rel.month = fresh.zero$first.release.month,
		  ocean.region = fresh.zero$ocean.region,
	#	  fishery		= fresh.zero$fishery,	
		  rec.year  = fresh.zero$rec.year),
		  sum,na.rm=T)
		colnames(fresh.by.hatchery.count.zero)[ncol(fresh.by.hatchery.count.zero)]	<-	"count.zero"
		
		fresh.by.hatchery	<-	merge(fresh.by.hatchery,fresh.by.hatchery.count.zero,all=T)
		
		fresh.consolidated <- fresh.by.hatchery
		
		####################################################
		####################################################
		####################################################
		####################################################
		####################################################
		### --- MAKE R DATA FOR USE IN ESTIMATION
		####################################################
		####################################################
		####################################################
		####################################################
		####################################################
		####################################################
# 		fresh.recover.complex   <- list()
# 		fresh.recover.simple    <- list()
# 		ocean.region <- sort(unique(fresh.consolidated$ocean.region))
# 		for(i in 1:length(ocean.region)){
# 		  temp1 <- fresh.w.fishery[fresh.w.fishery$ocean.region == ocean.region[i],]
# 		  temp2 <- fresh.consolidated[fresh.consolidated$ocean.region == ocean.region[i],]
# 		  reg.ID <- sort(unique(temp1$ID))
# 		  ID.1<- list()
# 		  ID.2<- list()
# 		  for(j in 1:length(reg.ID)){
# 		    ID.1[[j]] <- list(temp1[temp1$ID == reg.ID[j],])
# 		    ID.2[[j]] <- list(temp2[temp2$ID == reg.ID[j],])
# 		  }
# 		  names(ID.1) <- reg.ID  
# 		  names(ID.2) <- reg.ID  
# 		  fresh.recover.complex[[i]] <- ID.1
# 		  fresh.recover.simple[[i]]  <- ID.2
# 		}
# 		names(fresh.recover.complex) <- ocean.region
# 		names(fresh.recover.simple) <- ocean.region
		
		fresh.recover = list(#fresh.recover.simple=fresh.recover.simple,
		                     #fresh.recover.complex=fresh.recover.complex,
		                     dat.fish=fresh.w.fishery,
		                     dat.consolidated =fresh.consolidated,
		                     river.entry = fresh.by.hatchery.river)
		save(fresh.recover,file=paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/Releases and Recoveries/Fresh Recoveries.RData",sep=""))	    
		####################################################
		#############################################
		
# 	Sample Type
# Must match one of the following:
# sample_type
# ’1’ =In-sample recoveries from a sampled fishery with known catch; estimated_number must be absent or greater than ‘0’
# ’2’=Voluntary recoveries from a sampled fishery with known catch; Awareness estimates are available;
# 		estimated_number must be absent or greater than ‘0’ (e.g., Puget Sound Sport)
# ’3’=Voluntary recoveries from an unsampled fishery. Awareness approximations may be possible yielding non-zero
# 		estimated_number; otherwise estimated_number should be absent. (e.g., Hoh River freshwater sport fishery)
# ’4’=In-sample or voluntary recoveries from a sampled fishery with unknown catch;
# 		estimated_number must be absent. (e.g., Stream Survey)
# ’5’=Voluntary or select recoveries from a sampled fishery with known catch and no awareness estimates available; Use of these
# 		recoveries leads to double counting; see also Note #3 to follow
# 		estimated_number must be equal to ‘0’. (e.g., commercial voluntary recoveries);
# ’6’=Mark Incidence – Indirect Sample: Voluntary recoveries from indirectly sampled sport fishery; estimated_number are calculated
# 		from sport_mark_inc_sampl_obs_ads in sport_mark_incidence_sampl_size from the corresponding Catch Sample record
# ’7’ =Pass-Through Sample: Recoveries that are selectively removed from certain in-river sampling programs; The migrant fish are
# 		subject to subsequent destination sampling number_caught must equal number_sampled. see also Note #3 to follow
# 
# Notes for sample_type: (see also notes for Catch/Sample sample_type field #18)
# 1) Four keys are used to distinguish the type of sample:
# a) Sample: In-sample or Voluntary
# b) Fishery: Sampled or Unsampled
# c) Catch: Known or Unknown
# d) Awareness: Available or Unavailable
# 2) Awareness estimates (Sample Type Code 2) are based on current year’s data, while awareness approximations (Sample Type Code 3) are based on extrapolations of data from other periods or locations.
# 3) “Pass-through” Sampling (Sample Type Code 7) In certain sampling programs, some fish are released while selected fish are killed and snouts removed. The non-sampled fish are subject to subsequent destination sampling and the lack of reporting would result in underestimation of the tag codes. In this sampling situation, the number of fish pulled out of the pass-through equals the number sampled and generally gives an estimated number of 1.
# 4) Any associated Catch/Sample and Recovery records must have the same value of sample type.
	
	
# Estimation Level

# Level of resolution at which expansion is made; If present, must match one of the following:
# estimation_level
# ’2’=Level 2 (Sector)
# ’3’=Level 3 (Region)
# ’4’=Level 4 (Area)
# ’5’=Level 5 (Location)
# ’6’=Level 6 (Sub-Location)
# Must match the value in corresponding Catch/Sample data file estimation_level
# Required if estimated_number is greater than ’0’	


# Period Type

# Code to Indicate the type of time periods in which sampling occurred in the fishery / stratum for this tag recovery;
# period_type
# If present, must match one of the following:
# ’1’=Escapement period (across years possible)
# ’2’=Bi-weekly (statistical 2 week)
# ’3’=Semi-monthly (calendar)
# ’4’=Statistical month
# ’5’=Calendar month
# ’6’=Statistical week (beginning Monday)
# ’7’=Week (beginning Sunday)
# ’8’=Seasonal (Use for spring, summer, fall, or winter run periods)
# ’10’=Weekend (Saturday, Sunday & observed holiday(s))
# ’11’=Weekday (Monday – Friday excluding observed holiday(s))
# Required if sample_type is ’1’, ’2’, ’4’, or ’6’
# Required if period present;period_type and period must match that used in Catch/Sample data file for the given stratum

# Period

# Indicates the complete range of time in which sampling occurred in the fishery / stratum for this tag recovery; Possible Ranges:
# period
# n=’01’=Escapement period (across years possible)
# n=’01-26’=Bi-weekly (statistical 2 week)
# n=’01-24’=Semi-monthly (calendar)
# n=’01-12’=Statistical month
# n=’01-12’=Calendar month
# n=’01-54’=Statistical week (beginning Monday)
# n=’01-54’=Week (beginning Sunday)
# n=’01-04’=Seasonal periods ( 01=Spring, 02=Summer, 03=Fall, 04=Winter)
# n=’01-54’=Weekend beginning Saturday (or Friday if on observed holiday)
# n=’01-54’=Weekday beginning Monday (or first working day following observed holiday)
# Required to map across to sampling period range in the Catch/Sample data file
# Required if period_type present period_type and period must match that used in Catch/Sample data file
# for the given stratum	
	
#	write.csv(all.code.recoveries, "Fall Chinook consolidated recoveries.csv",row.names=F)
	
# 	RMIS “fishery” codes
# 
#         10      Ocean Troll (non-treaty)
#         11      Ocean Troll - Day Boat
#         12      Ocean Troll - Trip Boat
#         13      Ocean Troll - Freezer Boat
#         14      Ocean Troll - Ice Boat
#         15      Treaty Troll
#         16      Terminal Troll
#         17      Ocean Troll (treaty & non-treaty)
#         18      Aboriginal Troll
#         19      Other
#         20      Ocean Gillnet (non-treaty)
#         21      Columbia River Gillnet
#         22      Coastal Gillnet
#         23      Mixed Net and Seine
#         24      Freshwater Net
#         25      Commercial Seine
#         26      Terminal Seine
#         27      Freshwater Seine
#         28      Other Net
#         29      Other Seine
#         30      Aboriginal Seine
#         31      Aboriginal Gillnet
#         32      Aboriginal Mixed Net
#         33      Aboriginal Subsistence Net
#         34      Aboriginal Angler
#         39      Other Aboriginal
#         40      Ocean Sport
#         41      Sport (charter)
#         42      Sport (private)
#         43      Sport (jetty)
#         44      Columbia River Sport
#         45      Estuary Sport
#         46      Freshwater Sport
#         47      Freshwater Sport Snag
#         48      Terminal Sport
#         49      Other
#         50      Hatchery
#         51      Fish Screens
#         52      Fish Trap (freshwater)
#         53      Wild Broodstock Collection
#         54      Spawning Ground
#         55      Treaty Ceremonial
#         56      Treaty Subsistence
#         57      Mixed Wild Broodstock and Hatchery Returns
#         59      Other
#         60      Test Fishery Troll
#         61      Test Fishery Net
#         62      Test Fishery Seine
#         63      Test Fishery Trap
#         64      Test Fishery Unknown Multiple Gear
#         65      Dead Fish Survey
#         69      Other
#         70      Juvenile Sampling - Troll (marine)
#         71      Juvenile Sampling - Gillnet (marine)
#         72      Juvenile Sampling - Seine (marine)
#         73      Juvenile Sampling - Seine (freshwater)
#         74      Juvenile Sampling - trawl (marine)
#         79      Other
#         80      Hake Trawl Fishery, At-Sea component (CA/OR/WA)
#         800     Hake Trawl Fishery, Shoreside component (OR/WA)
#         802     Limited-Entry Rockfish Trawl (CA/OR/WA)
#         803     Limited-Entry Non-Hake Groundfish Trawl (CA/OR/WA)
#         804     Limited-Entry Sablefish Fixed Gear (CA/OR/WA)
#         805     State-Permitted Nearshore Grndfish Fishery (CA/OR)
#         81      Groundfish Observer (Gulf of Alaska)
#         812     Rockfish Fishery (Gulf of Alaska)
#         82      Groundfish Observer (Bering Sea/Aleutians)
#         83      Foreign Research Vessels
#         84      Foreign Mothership Vessels
#         85      Ocean Trawl By-Catch
#         86      Land Based Salmon
#         87      Squid Gillnet By-Catch
#         88      Juvenile Sampling - trawl
#         89      Other
#         90      Multiple Gear
#         91      PNP Cost Recovery
#         92      Columbia River Shad
#         93      Set-Line (Sturgeon)
#         94      Fish Trap (marine)
#         95      Confiscated
#         99      Other
		
		
		#############################################################################################
		#############################################################################################
		#############################################################################################
		#############################################################################################
		#############################################################################################
		#############################################################################################
		############################################
		### KLAMATH, TRINITY SPECIFIC DATA from Will
		############################################
		#############################################################################################
		#############################################################################################
		#############################################################################################
		#############################################################################################
		#############################################################################################
		#############################################################################################
		
# 		BASIN           'KLAM' OR 'TRIN'
# 		TAG_CODE        (CWT CODE)
# 		RACE            'FALL' OR 'SPRING'
# 		HATCH_NAME      HATCHERY NAME
# 		RELE_NAME       RELEASE LOCATION NAME
# 		BROOD_YR        BROOD YEAR
# 		AGE             2, 3, 4, OR 5*
# 		  * I believe the aging convention used here is that in months 1-8, age = current year - brood year.  In months 9-12, age= current year - brood year + 1
# 		RELE_YEAR       RELEASE YEAR
# 		RELE_MONTH      RELEASE MONTH
# 		RELE_STAGE      'F', 'Y', OR 'Y+'
# 		TOT_RELE        TOTAL RELEASE
# 		CWT_RELE        # RELEASED WITH CWT AND CLIP
# 		AVGRELEGM       RELEASE AVERAGE GRAMS WEIGHT
# 		
# 		OCNALLIMP       ALL OCEAN IMPACTS
# 		
# 		PREOCALIMP      OCEAN IMPACTS BEFORE AVERAGE DATE OF RIVER ENTRY
# 		(SPORT + COMMERCIAL;  CWT IMPACTS + NON-LANDED MORTALITIES)
# 		
# 		POSOCALIMP      OCEAN IMPACTS BEGINNING AVERAGE DATE OF RIVER ENTRY
# 		
# 		PREC_OTAGS      OCEAN COMMERCIAL CWT IMPACTS BEFORE AVERAGE DATE OF RIVER ENTRY
# 		PREC_ONLM       OCEAN COMMERCIAL NON-LANDED MORTALITIES BEFORE AVERAGE DATE OF RIVER ENTRY
# 		
# 		PRES_OTAGS      OCEAN SPORT CWT IMPACTS BEFORE AVERAGE DATE OF RIVER ENTRY
# 		PRES_ONLM       OCEAN SPORT NON-LANDED MORTALITIES BEFORE AVERAGE DATE OF RIVER ENTRY
# 		
# 		POSC_OTAGS      OCEAN COMMERCIAL CWT IMPACTS FROM AVERAGE DATE OF RIVER ENTRY ON
# 		POSC_ONLM       OCEAN COMMERCIAL NON-LANDED MORTALITIES FROM AVERAGE DATE OF RIVER ENTRY ON
# 		
# 		POSS_OTAGS      OCEAN SPORT CWT IMPACTS FROM AVERAGE DATE OF RIVER ENTRY ON
# 		POSS_ONLM       OCEAN SPORT NON-LANDED MORTALITIES FROM AVERAGE DATE OF RIVER ENTRY ON
# 		
# 		RIVER_ESC       RIVERNET+RIVERSPORT+RIVNATURAL+HATCHERY+FISHKILL
# 		
# 		RIVERNET        TRIBAL HARVEST + ESTIMATED NON-LANDED MORTALITIES
# 		RIVERSPORT      SPORT HARVEST + ESTIMATED NON-LANDED MORTALITIES
# 		RIVNATURAL      ESTIMATED NUMBER SPAWNING IN RIVER
# 		HATCHERY        TOTAL RETURN TO HATCHERY
# 		FISHKILL        MORTALITIES IN LOWER KLAMATH YEAR 2002
# 		
# 		RIVEROTH        RETURNS TO RIVER SYSTEMS OUTSIDE KLAMATH-TRINITY BASIN
# 		
# 		HATCHLENS       NUMBER OF MEASUREMENTS OF FISH RETURNING TO HATCHERY
# 		AVGLENMM        AVERAGE LENGTH OF FISH RETURNING TO HATCHERY
# 		VARLENMM        VARIANCE OF LENGTHS OF FISH RETURNING TO HATCHERY
# 		STDEVLENMM      STANDARD DEVIATION OF LENGTHS OF FISH RETURNING TO HATCHERY
# 		
# 		COHORTFLAG      'C'  = COMPLETED COHORT
# 		'ID' = INCOMPLETE COHORT, YEAR WITH    POTENTIAL DATA
# 		'IN' = INCOMPLETE COHORT, YEAR WITHOUT POTENTIAL DATA
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		