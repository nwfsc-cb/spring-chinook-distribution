# This script joins RMIS recoveries for pollock with sampling fraction information from the observer and fish ticket data.

#################################################################################################################################################################
# STEP 1: Join Release, Recoveries, and Location information. 
################################################################################################################################################################
#
# recoveries for the tag code of interest are already present in the data.frame
#recover.marine 

# These are needed to match recoveries from RMIS ("marine.by.hatchery.AK.trawl")

poll.samp.frac   <- readRDS(paste0(base.dir,"/spring-chinook-distribution/Processed Data/Effort Data/Sample_Fraction_Pollock_Summarized.RDS"))
poll.effort      <- readRDS(paste0(base.dir,"/spring-chinook-distribution/Processed Data/Effort Data/Shoreside_Pollock_Effort_Summarized.RDS"))

poll.effort  <- poll.effort %>% mutate(effort_frac = observed_boat_days / total_effort) %>%
                    rename(rec.year=year,rec.month=month) %>%
                    mutate(rec.area.code = case_when(region=="W.APEN"~"WAPEN",
                                                     region=="E.APEN"~"EAPEN",
                                                     region=="NW.GOA"~"NWGOA",
                                                     region=="NE.GOA"~"NEGOA",
                                                     TRUE ~ region)) 

poll.samp.frac <- poll.samp.frac %>% 
                      mutate(rec.area.code = case_when(region=="W.APEN"~"WAPEN",
                                                       region=="E.APEN"~"EAPEN",
                                                       region=="NW.GOA"~"NWGOA",
                                                       region=="NE.GOA"~"NEGOA",
                                                       TRUE ~ region)) %>% 
                      mutate(rec.year  = as.numeric(substr(year.month,1,4)),
                             rec.month = as.numeric(substr(year.month,6,7)))

poll.frac.both <- full_join(poll.samp.frac,
                            poll.effort %>% dplyr::select(rec.year,rec.month,rec.area.code,effort_frac)) %>%
                  mutate(sample.frac.both = sample_fraction * effort_frac)

marine.by.hatchery.AK.pollock <- left_join(marine.by.hatchery.AK.trawl %>% filter(fishery==81), 
                                         poll.frac.both %>% dplyr::select(rec.year,rec.month,rec.area.code,sample.frac.both))

# Interpolate from the nearest months for the few observartions that occur 
# in year-area-month combinations that have no quantified sampling fractions.

temp <- marine.by.hatchery.AK.pollock %>% filter(is.na(sample.frac.both)) %>% 
            group_by(rec.area.code,rec.year,rec.month) %>% dplyr::summarise(N=length(rec.year))

temp$sample_fraction2 <- NA
for(i in 1:nrow(temp)){
  temp1 <- temp[i,]
  A <- poll.frac.both %>% filter(rec.year==temp1$rec.year,
                                 rec.month >= temp1$rec.month - 1,
                                 rec.month <= temp1$rec.month + 1,
                                 rec.area.code == temp1$rec.area.code)
  if(nrow(A)>0){
  temp$sample_fraction2[i] <- A %>% dplyr::select(sample.frac.both) %>% unlist() %>% 
                                        mean(.,na.rm = T) %>% as.numeric()
  }
}

# Merge back in with the full data

marine.by.hatchery.AK.pollock <- left_join(marine.by.hatchery.AK.pollock,temp %>% dplyr::select(-N)) %>% 
                                    mutate(samp.frac.both = ifelse(is.na(sample.frac.both)==T,sample_fraction2,sample.frac.both)) %>%
                                    dplyr::select(-sample_fraction2)
