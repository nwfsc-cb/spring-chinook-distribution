# Pull in raw RMIS releases
rel.dat <- read.csv("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Total Smolt and Run Info/_ALL RMIS RELEASE 2005-2014 summer fall Chinook.csv")
rel.dat$tot_rel <- rowSums(rel.dat[,c("cwt_1st_mark_count","cwt_2nd_mark_count","non_cwt_1st_mark_count","non_cwt_2nd_mark_count")],na.rm=T)

dat <- rel.dat[,c("brood_year","tag_code_or_release_id","run","release_stage","release_location_state","release_location_rmis_region","release_location_rmis_basin","tot_rel")]

dat$stage.mod <- as.character(dat$release_stage)
dat$stage.mod[dat$stage.mod == "V"] <- "G"
dat$stage.mod[dat$stage.mod == "S" | dat$stage.mod == "P" ] <- "Y"

dat$region.mod <- as.character(dat$release_location_rmis_region)
dat$region.mod[dat$region.mod == "SAFA" | dat$region.mod == "SJOA" | dat$region.mod == "CECA" ] <- "SFB"
dat$region.mod[dat$region.mod == "NOCA" | dat$region.mod == "KLTR" ] <- "NCA"

dat$region.mod[dat$release_location_rmis_basin == "NEHA" | dat$release_location_rmis_basin == "TILN" | 
                 dat$release_location_rmis_basin == "SIYA" | dat$release_location_rmis_basin == "ALSE"] <- "NOR"
dat$region.mod[dat$release_location_rmis_basin == "SIUS" | dat$release_location_rmis_basin == "UMPQ" | dat$release_location_rmis_basin == "COOS" |
                 dat$release_location_rmis_basin == "COQU" | dat$release_location_rmis_basin == "SIXE"] <- "COR"
dat$region.mod[dat$release_location_rmis_basin == "ROGU" | dat$release_location_rmis_basin == "CHET" ] <- "SOR"

dat$region.mod[dat$region.mod == "UPCR" | dat$region.mod == "SNAK" ] <- "UPCOL"
dat$region.mod[dat$region.mod == "CECR" | dat$region.mod == "LOCR" | dat$region.mod == "CRGN" ] <- "COL"

dat$region.mod[dat$region.mod == "NWC" | dat$region.mod == "GRAY" | dat$region.mod == "WILP" ] <- "WAC"
dat$region.mod[dat$region.mod == "JUAN" | dat$region.mod == "HOOD" | 
                 dat$region.mod == "SPS" | dat$region.mod == "MPS" |
                 dat$region.mod == "NPS" | dat$region.mod == "SKAG" |
                 dat$region.mod == "NOWA" ] <- "PUSO"

dat$region.mod[dat$region.mod == "FRTH" | dat$region.mod == "GST" | dat$region.mod == "JNST" ] <- "SGEO"
dat$region.mod[dat$region.mod == "WCVI" ] <- "SWVI"

all.combined <-expand.grid(region = spawn_loc$ocean.region,brood_year=2005:2014,run=sort(unique(dat$run)),stage=sort(unique(dat$stage.mod)))
agg <- data.frame(aggregate(dat$tot_rel, by = list(brood_year=dat$brood_year,
                                                   run=dat$run,
                                                   stage=dat$stage.mod,
                                                   #state=dat$release_location_state,
                                                   region=dat$region.mod),sum))
agg.1   <- merge(all.combined,agg,all=T)
agg.1$x[is.na(agg.1$x ==T)] <- 0
agg.1 <- agg.1[agg.1$stage != "E",]

agg.1 <- agg.1[agg.1$region != "NASK" & agg.1$region != "UPYR" & agg.1$region != "COBC" & agg.1$region != "" & agg.1$region != "CAGN" ,]

tot.rel <- data.frame(aggregate(agg.1$x,by = list(run=agg.1$run,
                                                  brood_year= agg.1$brood_year,
                                                  #state=agg.1$state,
                                                  region=agg.1$region),sum))
agg.2 <- data.frame(aggregate(agg.1$x,by = list(run=agg.1$run,
                                                stage=agg.1$stage,
                                                #state=agg.1$state,
                                                region=agg.1$region),median))
agg.3 <- data.frame(aggregate(agg.1$x,by = list(run=agg.1$run,
                                                stage=agg.1$stage,
                                                #state=agg.1$state,
                                                region=agg.1$region),max))
agg.4 <- data.frame(aggregate(agg.1$x,by = list(run=agg.1$run,
                                                stage=agg.1$stage,
                                                #state=agg.1$state,
                                                region=agg.1$region),min))
agg.5 <- data.frame(aggregate(agg.1$x,by = list(run=agg.1$run,
                                                stage=agg.1$stage,
                                                #state=agg.1$state,
                                                region=agg.1$region),sd))

dat.agg <- agg.2
colnames(dat.agg)[which(colnames(dat.agg)=="x")] <- "MEDIAN"
dat.agg$SD <- agg.5$x
dat.agg$MIN <- agg.4$x
dat.agg$MAX <- agg.3$x

FALL <- dat.agg[dat.agg$run==3,]
LATE.FALL <- dat.agg[dat.agg$run ==7 | dat.agg$run ==8 ,]
SUMMER <- dat.agg[dat.agg$run ==2,]


# BC REGION NAMES

# Region	Region Name	Release
# Groups
# BCGN	British Columbia, general	
# COBC	Coastal British Columbia	
# FRTH	Fraser R, Thompson R	
# GST	Georgia Strait	
# JNST	Johnstone Strait	
# NASK	Nass R - Skeena R	
# QCI	Queen Charlotte Islands	
# TRAN	Transboundary Rivers in Canada	
# WCVI	Western Vancouver Island