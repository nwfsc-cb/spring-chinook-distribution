# Make file defining which Spring Chinook runs to use in the spatial model

library(readxl)
library(dplyr)

#### Consolideted CWT releases file for Climate analysis
base.dir <- "/Users/ole.shelton/GitHub"

#A CLUGE SO CAN READ IN THE SAME DATA BUT END UP WITH A SLIGHTLY DIFFERENT DATA FRAME
loc_18 <- "AK_NCA_SOR_PUSO" ##_18loc" ## options for this are a null string ("", should be default) 
## or "_18loc" for using 2 areas for PUSO, 
## or "_two_OR_PUSO" for two OR areas (SOR and NOR) and two PUSOs.
## or "_NCA_SOR_PUSO" for two OR areas (COR and NOR) and two PUSOs.  NCA and SOR are combined
## or "AK_NCA_SOR_PUSO" for two OR areas (COR and NOR) and two PUSOs.  NCA and SOR are combined + Includes Alaksa areas (YAK, PWS, KOD, CHIR, AKPEN, ALEUT,BER)
### CWT releases
## Identify rivers, releases of interest.

setwd("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Releases/") 

all.release.dat	<-	read.csv("/Users/ole.shelton/GitHub/rmis-data/data/chinook/all_releases.csv")

spr.sum.dat <- read.csv("CLIMATE/river releases spring-summer chinook FRAM_v2 CLIMATE.csv")
# This is the older file I used pre-12/2019:
#all.release.dat	<-	read.csv("/Users/ole.shelton/GitHub/Orca_Salmon_DATA/Releases/all chinook releases.csv")
all.release.dat$hatchery_location_code	<- as.character(all.release.dat$hatchery_location_code)

dat.spr <- all.release.dat %>% filter(run==1 | run ==2 | run ==4) 

dat.spr.hatch <- dat.spr %>% group_by(run,release_location_state, release_location_rmis_region,release_location_rmis_basin,
                                              hatchery_location_code,stock_location_code, 
                                              hatchery_location_name,stock_location_name,release_location_name) %>%
                                summarize(min.byear=min(brood_year),max.byear=max(brood_year),n.year=length(unique(brood_year)),
                                           avg.tagged.clip=mean(tagged_adclipped),sum.tagged.clip=sum(tagged_adclipped) ,
                                          avg.tagged.unclip=mean(tagged_unclipped),sum.tagged.unclip=sum(tagged_unclipped)) %>% 
                                arrange(run,release_location_state,desc(avg.tagged.clip)) %>% as.data.frame() 

dat.spr.hatch.simp <- dat.spr %>% group_by(release_location_state, release_location_rmis_region,
                                                   hatchery_location_code,stock_location_code) %>%
                      summarize(min.byear=min(brood_year),max.byear=max(brood_year),n.year=length(unique(brood_year)),
                                    avg.tagged.clip=mean(tagged_adclipped),sum.tagged.clip=sum(tagged_adclipped)) %>% 
                      arrange(release_location_state,desc(avg.tagged.clip)) %>% as.data.frame() 


#3 LOOK AT ALASKA RELEASES.
dat.spr.ak <- dat.spr.hatch %>% filter(release_location_state=="AK")
dat.spr.ak <- dat.spr.ak %>% arrange(release_location_rmis_region,
                                     stock_location_code,n.year,avg.tagged.clip)

dat.spr.ak.to.file <- dat.spr.ak %>%  filter(avg.tagged.clip>0) %>% mutate(avg.tagged.clip=round(avg.tagged.clip,0)) %>% #filter(is.na(release_location_state)==F) %>% 
  arrange(run,release_location_rmis_region,hatchery_location_code,stock_location_code,n.year,avg.tagged.clip)



A <- left_join(dat.spr.ak.to.file,spr.sum.dat %>% filter(State=="AK")%>%
        dplyr::select(stock_location_code,hatchery_location_code,hatchery_location_name,stock_location_name))



# these are the rmis regions for Alaska
reg <- unique(dat.spr.ak.to.file$release_location_rmis_region)
# [1] ALSR CEAK CHIL CNAK LOYR SEAK STUN TAWH WEAK
# Go through them one by one. # Manually pick out hatchery-stock-release cominations 

dat.spr.ak.to.file %>% filter(release_location_rmis_region=="ALSR")
# only 2 releases.  Both very small < 10000.  Ignore these
A <- dat.spr.ak.to.file %>% filter(release_location_rmis_region=="CEAK")
# 83 groups.
# Elmendorf - Deception creek was excluded because stock from Cook inlet is released in PWS.
A<-dat.spr.ak.to.file %>% filter(release_location_rmis_region=="CHIL")
# Only 10.  mix of wild and hatchery.
A<-dat.spr.ak.to.file %>% filter(release_location_rmis_region=="CNAK")
# Only Two... tiny releases. Ignore
A<-dat.spr.ak.to.file %>% filter(release_location_rmis_region=="LOYR")
# OMIT THE YUKON FOR NOW.

A<-dat.spr.ak.to.file %>% filter(release_location_rmis_region=="SEAK")
### This is complicated.  There are a relatively small number of origin stocks... but they get released all over the place.  
# Tried to limit included stocks to those released "near" thier origin streams.  In general
A <- dat.spr.ak.to.file %>% filter(release_location_rmis_region=="STUN")
## A very few wild runs (Unuk, Stikine)
A <- dat.spr.ak.to.file %>% filter(release_location_rmis_region=="TAWH")
# Taku wild tagging
A <- dat.spr.ak.to.file %>% filter(release_location_rmis_region=="WEAK")
write.csv(file="TEMP.csv",A)

########################################################################
###########################  BC
########################################################################
dat.spr.bc <- dat.spr.hatch %>% filter(release_location_state=="BC")
dat.spr.bc <- dat.spr.bc %>% arrange(release_location_rmis_region,
                                     stock_location_code,n.year,avg.tagged.clip)

dat.spr.bc.to.file <- dat.spr.bc %>%  filter(avg.tagged.clip>0) %>% mutate(avg.tagged.clip=round(avg.tagged.clip,0)) %>% #filter(is.na(release_location_state)==F) %>% 
  arrange(run,release_location_rmis_region,hatchery_location_code,stock_location_code,n.year,avg.tagged.clip)


# these are the rmis regions for Alaska
reg <- unique(dat.spr.bc.to.file$release_location_rmis_region)
# [1]      COBC FRTH GST  JNST NASK QCI  TRAN UPYR WCVI
# Go through them one by one. # Manually pick out hatchery-stock-release cominations 

B<-dat.spr.bc.to.file %>% filter(release_location_rmis_region=="")

B<-dat.spr.bc.to.file %>% filter(release_location_rmis_region=="COBC")
# This is the central BC coast (rivers inlet, others.)  relatively sparse.
B<-dat.spr.bc.to.file %>% filter(release_location_rmis_region=="FRTH") 
# Selected the largest and most frequent fraser releases.  Could easily add more here if needed
B<-dat.spr.bc.to.file %>% filter(release_location_rmis_region=="GST") 
# Focused on big releases from common locations.  Mostly avoided marine rearing releases.
B<-dat.spr.bc.to.file %>% filter(release_location_rmis_region=="JNST") 
# 
B<-dat.spr.bc.to.file %>% filter(release_location_rmis_region=="NASK") 
# Not too many options. Variety early on becoming more uniform through time.  Selected most of the big ones.
B<-dat.spr.bc.to.file %>% filter(release_location_rmis_region=="QCI") 
# Only 3 or 4 options. 2 of which are released on Haida Gwai.
B<-dat.spr.bc.to.file %>% filter(release_location_rmis_region=="TRAN") 
# VERY SMALL NUMBERS.
B<-dat.spr.bc.to.file %>% filter(release_location_rmis_region=="UPYR") 
# Not that many.  Did not include these for now.
B<-dat.spr.bc.to.file %>% filter(release_location_rmis_region=="WCVI") 
# 
write.csv(file="TEMP.csv",B)

########################################################################
###########################  WA
########################################################################
dat.spr.wa <- dat.spr.hatch %>% filter(release_location_state=="WA") %>% 
                arrange(release_location_rmis_region,stock_location_code,n.year,avg.tagged.clip)

dat.spr.wa.to.file <- dat.spr.wa %>%  filter(release_location_state=="WA") %>% mutate(avg.tagged.clip=round(avg.tagged.clip,0),avg.tagged.unclip=round(avg.tagged.unclip,0)) %>% #filter(is.na(release_location_state)==F) %>% 
  arrange(release_location_rmis_region,hatchery_location_code,stock_location_code,n.year,avg.tagged.clip)

# these are the rmis regions for Alaska
reg <- unique(dat.spr.wa.to.file$release_location_rmis_region)
# [1] CECR CRGN GRAY HOOD JUAN LOCR MPS         NOWA NPS  NWC  SKAG SNAK SPS  UPCR WAGN WILP
# Go through them one by one. # Manually pick out hatchery-stock-release cominations 

B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="CECR")
#3 This is Klickitat, little white salmon, Carson, spring creek.  MCOL
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="CRGN")
### Three or four large hatchery release groups, mostly UCOL released somewhere.
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="GRAY")
# Few releases of modest size - Humptulips, satsop, wishkah
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="HOOD")
# Dominated by George Adams and Hoodsport.  Some mixing and matching of rivers and releases
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="JUAN")
# Hoko, Elwha, Dungeness.  Some weirdness with releases in different rivers.
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="LOCR")
# Lots of releases here... some are just from slightly upstream origins (little white salmon)
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="MPS")
# Soos, Issaquah, Puyallup, others.  Few relatively large
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="NOWA")
# Samish, Kendall Creek, Glenwood.  Could get some more if desired.
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="NPS")
# Wallace R , Whitehors, Harvey Creek.  Stillaguamish, snohomish, Skykomish.  Omitted Bernie Gobin (to many mix and matches.)
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="NWC")
# Quinault, Queets, Solduc, Tsoo-yess.  A few other options.
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="SKAG")
# Marblemount and county line ponds
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="SNAK")
# Lyons Ferry, lower snake, Tucannon
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="SPS")
#Clear, Kalama, McAllister, Garrison, Tumwater, Hupp, Minter... includes white river
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="UPCR")
# Dominated by Wells but Entiait Winthrop Chelan, Similkameen, Methow, Priest, Cle Elum, Wenatchee, Chiwawa
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="WAGN")
# Early years, rare.  Used none.
B<-dat.spr.wa.to.file %>% filter(release_location_rmis_region=="WILP")
# Two releases.  worthless
dim(B)
write.csv(file="TEMP.csv",B)


#######################################################################
###########################  ID
########################################################################
dat.spr.id <- dat.spr.hatch %>% filter(release_location_state=="ID") %>% 
  arrange(release_location_rmis_region,stock_location_code,n.year,avg.tagged.clip)

dat.spr.id.to.file <- dat.spr.id %>%  filter(release_location_state=="ID",avg.tagged.clip>0) %>% mutate(avg.tagged.clip=round(avg.tagged.clip,0)) %>% #filter(is.na(release_location_state)==F) %>% 
  arrange(release_location_rmis_region,hatchery_location_code,stock_location_code,n.year,avg.tagged.clip)

# these are the rmis regions for Alaska
reg <- unique(dat.spr.id.to.file$release_location_rmis_region)
# 
B <-dat.spr.id.to.file %>% filter(release_location_rmis_region=="SNAK")
#  NPT Mccall Sawtooth, rapid river, clearwater, dworshak, pahsimeroi

dim(B)
write.csv(file="TEMP.csv",B)

######################################################################
###########################  OR
########################################################################
dat.spr.or <- dat.spr.hatch %>% filter(release_location_state=="OR") %>% 
  arrange(release_location_rmis_region,stock_location_code,n.year,avg.tagged.clip)

dat.spr.or.to.file <- dat.spr.or %>%  filter(release_location_state=="OR") %>% mutate(avg.tagged.clip=round(avg.tagged.clip,0)) %>% #filter(is.na(release_location_state)==F) %>% 
  arrange(release_location_rmis_region,hatchery_location_code,stock_location_code,n.year,avg.tagged.clip)

# these are the rmis regions for Oregon
reg <- unique(dat.spr.or.to.file$release_location_rmis_region)
# CECR LOCR NOOR ORGN SNAK SOOR UPCR
B <-dat.spr.or.to.file %>% filter(release_location_rmis_region=="CECR")
# Warm springs, round butte, Irrigon, Umatilla, Bonneville

B <-dat.spr.or.to.file %>% filter(release_location_rmis_region=="LOCR")
# Willamette, predominantly, some lower Col

B <-dat.spr.or.to.file %>% filter(release_location_rmis_region=="NOOR")
# Trask, Cedar cr, salmon R, some other options left out.
B <-dat.spr.or.to.file %>% filter(release_location_rmis_region=="SNAK")
# Lookingglass... a few from  Irrigon
B <-dat.spr.or.to.file %>% filter(release_location_rmis_region=="UPCR")
# Ignore
B <-dat.spr.or.to.file %>% filter(release_location_rmis_region=="SOOR")
## Surprising number of fish from Southern Oregon.  Rogue, Umpqua, Coos, Chetco, Coquille, Elk.
# Relatively straight forward.  Could fairly easily add small numbers with other codes.. particularly for early 80s

dim(dat.spr.or.to.file)
dim(B)
write.csv(file="TEMP.csv",B)

######################################################################
###########################  CA
########################################################################
dat.spr.ca <- dat.spr.hatch %>% filter(release_location_state=="CA") %>% 
  arrange(release_location_rmis_region,stock_location_code,n.year,avg.tagged.clip)

dat.spr.ca.to.file <- dat.spr.ca %>%  filter(avg.tagged.clip>0) %>% mutate(avg.tagged.clip=round(avg.tagged.clip,0)) %>% #filter(is.na(release_location_state)==F) %>% 
  arrange(release_location_rmis_region,hatchery_location_code,stock_location_code,n.year,avg.tagged.clip)

# these are the rmis regions for California
reg <- unique(dat.spr.ca.to.file$release_location_rmis_region)
# CAGN CECA KLTR NOCA SAFA SJOA
B <-dat.spr.ca.to.file %>% filter(release_location_rmis_region=="CAGN")
# These are three releasses of fish in the net pen system.
B <-dat.spr.ca.to.file %>% filter(release_location_rmis_region=="CECA")
# Nimbus, Coleman, Feather, Mokelumne... majority of Central CA releases... in ocean  - SEE SAFA for river releases
B <-dat.spr.ca.to.file %>% filter(release_location_rmis_region=="KLTR")
## Trinity, Irongate plus a few sundries.
B <-dat.spr.ca.to.file %>% filter(release_location_rmis_region=="NOCA")
## Mad. Smith, Eel.  Lots of dispersed, tiny releases.  Could scrape the bottom of the well should we want to.
B <-dat.spr.ca.to.file %>% filter(release_location_rmis_region=="SAFA")
# In river releases for Nimbus, Coleman, Feather,  majority of Central CA releases  See CECA too # 
B <-dat.spr.ca.to.file %>% filter(release_location_rmis_region=="SJOA")
## These are largely Merced releases and other hatcheries released in the southern basins

dim(dat.spr.to.file)
dim(B)
write.csv(file="TEMP.csv",B)



