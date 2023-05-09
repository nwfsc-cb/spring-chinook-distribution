
### Script for parsing trawl fish ticket data.

library(dplyr)
library(ggplot2)

#base.dir <- "~/Documents/GitHub/Chinook_Bycatch/"
data.dir <- "/Users/ole.shelton/Github/Orca_Salmon_DATA/Effort info/Trawl-US/Shoreside"
setwd(data.dir)

load("trawl_fish_tix_81_16.RData")
load("trawl_fish_tix_14_21.RData")
#Raw CSV for 2014-21 is here:
#trawl_fish_tix_14_21 <- read.csv("shelton_pf_ft_2014_2021.csv")

dat.1 <- trawl_fish_tix_81_16[[1]]
dat.2 <- trawl_fish_tix_14_21

dat.1 <- dat.1 %>% filter(LANDING_YEAR<=2013)

dat <- rbind(dat.1,dat.2)

# I don't have fishery sectors for 2017 and on, so base this on 
# This section is redundant with what is done in "Process shoreside CA OR WA.R"

# A <- dat %>% filter(grepl("WHITING",PACFIN_SPECIES_COMMON_NAME)) %>%
#                 filter(LANDING_YEAR>=2017)
# IDS <- A %>% filter(PACFIN_GEAR_CODE %in% c("GFT","MDT","OTW","RLT")) %>% pull(FTID)
# 
# dat.2017.on <- dat %>% filter(FTID %in% IDS) %>% ungroup() %>%
#                 group_by(FTID) %>%
#                 mutate(tot.wgt= sum(LANDED_WEIGHT_LBS),
#                        frac=LANDED_WEIGHT_LBS / tot.wgt) %>%
#                 filter(grepl("WHITING",PACFIN_SPECIES_COMMON_NAME))%>%
#                 filter(frac>0.5)
              

fish.sectors <- read.csv("FT_sct_def_Shelton_2018-11-13.csv")
                         
dat <- left_join(dat,fish.sectors)
### THERE ARE 44 instances in which the years don't match up.... I'm not too worried about that.
dat %>% filter((YEAR-LANDING_YEAR)!=0) %>% dim(.)

### Pull out the FTID with midwater hake identified.
  dat.hake <- dat %>% filter(sct.simpl == "Midwater Hake")
  # This flag is only identified 2011 and later.  Irritating.

### Sector identifications are not very useful pre-2011
A <- dat %>% group_by(sct.simpl,PACFIN_GEAR_CODE,LANDING_YEAR) %>% summarise(length(LANDING_YEAR)) %>% as.data.frame()

# Below the proposal section are some data manipulations and plotting that Ole did to understand the data.

##################################################
# Ole proposes for Shoreside Hake.

# Steps: 
#     1) Determine which tickets & vessel can be linked to the hake fleet.
#         Use Kayleigh's designations for recent years (2011-2016)
#       For earlier years:
#         Identify tickets with preponderance of hake as the species landed.
#         -there are problems in that a single boat delivers hake as a separate ticket number sometimes.
#         -this can mean that tickets that look like they are pure hake are not... they need to be combined with
#             other rows in the data.
#         - Suggest:
#             summarize by vessel,landing year,month,day,port
#             keep gear codes "GFT","MDT","OTW", keep track of FTID numbers.
#             sum up catch by species
#             calculate proportion hake out of that total that has been combined across all landings by a vessel
#                    on a particular day
#             keep vessel-trip if the proportion assigned to hake is above a threshold (0.5? 0.75?)
                    # Management definition is > 0.5 hake since 2013-14
#             identify which FTID numbers are associated with a vessel trip.
#             check total biomass of landed hake.... make a minimum cutoff for landed biomass.  Hake trips should be big biomass.          


# 
#     2) Use the trips identified in part one to Identify the duration of trips
#         Looking for a way to calculate effort (units of boat-days)
#         One idea: Look at boats that deliver multiple trips in a row. 
#         Develop a ballpark estimate of the distribution of trip lengths
#             Does this distribution vary by year or area?
#         This will mean that some trips have an estimated length, others do not
#             Develop an algorithm for applying estimated effort for each unobserved trip.
#             
#     3) Once a length of each trip has been identified, apportion effort for each trip to spatial and temporal boxes 
#         Apportion to PFMC areas listed, then 
#         Apportion to areas useful for Salmon.
#             This could be based on observed catch of hake in each area
#              Or divided equally among spatial components listed.
#             Or some other metric.


# Interesting comparisons -
    # For years with decent catch locations, compare ASHOP with the Shoreside in terms of effort.
    # Might be able to borrow information from early ASHOP data to inform locations of early Shoreside
    # fleet effort if the correlation is high enough.

    # Kayleigh suggests splitting off mothership v. catcher-processor in ASHOP data.
    # motherships likely more comparable to shoreside.

#     Notes.  
#     Only need to consider gear codes "GFT","MDT","OTW", possibly very rarely "RLT"
#     warning signs that something is wrong.
#            Species composition is exactly 1 (no bycatch at all)










######################
# Below here is Ole trying to parse the fish tickets to get an idea of how they work.
######################
### Find All Whiting devliveries
# whiting.tix <- dat %>% filter(PACFIN_SPECIES_SCIENTIFIC_NAME=="MERLUCCIUS PRODUCTUS") %>%
#                   dplyr::select(FISH_TICKET_ID) %>% group_by(FISH_TICKET_ID) %>% summarise(length(FISH_TICKET_ID))
#                 
# whiting.trawls <- dat %>% filter(FISH_TICKET_ID %in% whiting.tix$FISH_TICKET_ID)
# 
# 
# whiting.all.sp <-  whiting.trawls %>% 
#         group_by(FISH_TICKET_ID,
#                  FTID,
#                  VESSEL_REGISTRATION_ID,
#                  VESSEL_ID,
#                  VESSEL_NUM,
#                 LANDING_YEAR, 
#                 LANDING_MONTH, 
#                 LANDING_DAY, 
#                 NUM_OF_DAYS_FISHED,
#                 PACFIN_GEAR_DESCRIPTION,
#                 PACFIN_GEAR_CODE,
#                 PACFIN_CATCH_AREA_CODE,
#                 PACFIN_CATCH_AREA_DESCRIPTION,
#                 PACFIN_GROUP_CATCH_AREA_CODE, 
#                 INPFC_AREA_TYPE_CODE, 
#                 PORT_CODE, 
#                 PORT_NAME, 
#                 PACFIN_PORT_CODE,
#                 PACFIN_SPECIES_CODE,
#                 PACFIN_SPECIES_COMMON_NAME,
#                 PACFIN_SPECIES_SCIENTIFIC_NAME) %>%
#           summarize(WEIGHT = sum(LANDED_WEIGHT_LBS)) %>% as.data.frame()
# 
# whiting.total.sp <-  whiting.trawls %>% 
#     group_by(FISH_TICKET_ID,
#              FTID,
#              VESSEL_REGISTRATION_ID,
#              VESSEL_ID,
#              VESSEL_NUM,
#              LANDING_YEAR, 
#              LANDING_MONTH, 
#              LANDING_DAY, 
#              NUM_OF_DAYS_FISHED,
#              PACFIN_GEAR_DESCRIPTION,
#              PACFIN_GEAR_CODE,
#              PACFIN_CATCH_AREA_CODE,
#              PACFIN_CATCH_AREA_DESCRIPTION,
#              PACFIN_GROUP_CATCH_AREA_CODE, 
#              INPFC_AREA_TYPE_CODE, 
#              PORT_CODE, 
#              PORT_NAME, 
#              PACFIN_PORT_CODE) %>%
#           summarize(TOT.WEIGHT = sum(LANDED_WEIGHT_LBS)) %>% as.data.frame()
# 
# #THOMSON_FISHERY_CODE is FUCKED
# 
# whiting.all.sp <- left_join(whiting.all.sp,whiting.total.sp) %>% mutate(prop.weight = WEIGHT / TOT.WEIGHT)
# whiting.all.sp <- whiting.all.sp %>% mutate(year.jit=LANDING_YEAR+runif(nrow(whiting.all.sp),-0.25,0.25))
# # lets look at the distribution of gear types through time.
# gear.by.year <- whiting.all.sp %>% filter(PACFIN_SPECIES_SCIENTIFIC_NAME == "MERLUCCIUS PRODUCTUS") %>%
#                 group_by(LANDING_YEAR,PACFIN_GEAR_DESCRIPTION,PACFIN_GEAR_CODE) %>% 
#                 summarize(N.obs = length(prop.weight),Mean.hake = mean(WEIGHT), total.hake =sum(WEIGHT))
# 
# gear.by.year.prop <- whiting.all.sp %>% filter(PACFIN_SPECIES_SCIENTIFIC_NAME == "MERLUCCIUS PRODUCTUS") %>%
#   group_by(LANDING_YEAR,PACFIN_GEAR_DESCRIPTION,PACFIN_GEAR_CODE) %>% 
#   summarize(N.obs = length(prop.weight),N.large=length(prop.weight[prop.weight>0.5]),Mean.hake = mean(prop.weight))
# 
# 
# p1 <- ggplot(gear.by.year) +
#         geom_line(aes(x=LANDING_YEAR,y=total.hake ,color=PACFIN_GEAR_CODE)) +
#         theme_bw()
# p1
# p2 <- ggplot(gear.by.year.prop) +
#   geom_line(aes(x=LANDING_YEAR,y=N.obs ,color=PACFIN_GEAR_CODE)) +
#   geom_line(aes(x=LANDING_YEAR,y=N.large ,color=PACFIN_GEAR_CODE),linetype="dashed") +
#   theme_bw()
# p2
# 
# p2b <- p2 + facet_wrap(~PACFIN_GEAR_CODE,scales="free_y")
# p2b
# 
# ### These plots tell me that we can ignore all gear types except GFT, MDT, and OTW for the shoreside hake fleet
# # Cull all except the focal 3 gear types
# 
# whiting.all.sp <- whiting.all.sp %>% filter(PACFIN_GEAR_CODE %in% c("GFT","MDT","OTW"))
# gear.by.year <- whiting.all.sp %>% filter(PACFIN_SPECIES_SCIENTIFIC_NAME == "MERLUCCIUS PRODUCTUS") %>%
#   group_by(LANDING_YEAR,PACFIN_GEAR_DESCRIPTION,PACFIN_GEAR_CODE) %>% 
#   summarize(N.obs = length(prop.weight),Mean.hake = mean(WEIGHT), total.hake =sum(WEIGHT))
# 
# gear.by.year <- whiting.all.sp %>% filter(PACFIN_SPECIES_SCIENTIFIC_NAME == "MERLUCCIUS PRODUCTUS") %>%
#   group_by(LANDING_YEAR,PACFIN_GEAR_DESCRIPTION,PACFIN_GEAR_CODE) %>% 
#   summarize(N.obs = length(prop.weight),Mean.hake = mean(WEIGHT), total.hake =sum(WEIGHT))
# 
# gear.by.year.prop <- whiting.all.sp %>% filter(PACFIN_SPECIES_SCIENTIFIC_NAME == "MERLUCCIUS PRODUCTUS") %>%
#   group_by(LANDING_YEAR,PACFIN_GEAR_DESCRIPTION,PACFIN_GEAR_CODE) %>% 
#   summarize(N.obs = length(prop.weight),N.large=length(prop.weight[prop.weight>0.9]),Mean.hake = mean(prop.weight))
# 
# p3 <- ggplot(gear.by.year.prop) +
#   geom_line(aes(x=LANDING_YEAR,y=N.obs ,color=PACFIN_GEAR_CODE)) +
#   geom_point(aes(x=LANDING_YEAR,y=N.obs ,color=PACFIN_GEAR_CODE)) +
#   geom_line(aes(x=LANDING_YEAR,y=N.large ,color=PACFIN_GEAR_CODE),linetype="dashed") +
#   geom_point(aes(x=LANDING_YEAR,y=N.large ,color=PACFIN_GEAR_CODE)) +
#   theme_bw()
# p3
# 
# p3b <- p3 + facet_wrap(~PACFIN_GEAR_CODE,scales="free_y")
# p3b
# 
# p4 <- ggplot(gear.by.year) +
#   geom_line(aes(x=LANDING_YEAR,y=total.hake ,color=PACFIN_GEAR_CODE)) +
#   theme_bw()
# p4
# 
# ##############################################
# ##############################################
# ##############################################
# # Start to look at individual boats and determine if you can extra days fished from just delivery information
# 
# # Cull deliveries to only include the Whiting catches
# 
# whiting.only <- whiting.all.sp %>% filter(PACFIN_SPECIES_SCIENTIFIC_NAME=="MERLUCCIUS PRODUCTUS") %>% arrange(FTID)
#     # Ole looked and FTID and FISH_TICKET_ID have identical information.  Use FTID.
# 
# # Individual rows may be from the same trip but in different areas.
# # Consolidate trips such that each row is a unique fish ticket.
# 
# whiting.trips <- whiting.only %>% group_by(FTID,VESSEL_ID,PACFIN_GEAR_CODE,LANDING_YEAR,LANDING_MONTH,LANDING_DAY) %>%
#                     summarize(trip.hake=sum(WEIGHT),trip.total=sum(TOT.WEIGHT)) %>% mutate(prop.whiting=trip.hake/trip.total)
# 
# whiting.a <- whiting.only %>% group_by(FISH_TICKET_ID,FTID,PACFIN_GEAR_CODE,LANDING_YEAR) %>% summarize(x=length(FTID))
# 
# 
# ### THIS EXAMPLE SHOWS THAT A SINGLE VESSEL LANDING ON A GIVEN DAY AND PORT HAS TWO DIFFERENT FTIDs.  
# ### ONE FOR WHITING, A DIFFERENT ONE FOR ALL OTHER SPECIES. THIS APPEARS TO OCCUR IN 1981-1983 at least.  
# ### It is unclear how widespread and issue this is.
# 
# #1981
# A <- dat %>% filter(VESSEL_ID==21218845,LANDING_YEAR==1981) %>% 
#             dplyr::select(LANDING_YEAR,LANDING_MONTH,LANDING_DAY,FTID,
#                           PACFIN_SPECIES_COMMON_NAME,PACFIN_SPECIES_SCIENTIFIC_NAME,LANDED_WEIGHT_LBS)
# A %>% filter(PACFIN_SPECIES_SCIENTIFIC_NAME=="MERLUCCIUS PRODUCTUS")
# A %>% filter(LANDING_MONTH==7,LANDING_DAY==5)
# 
# #1982
# A1 <- dat %>% filter(VESSEL_ID==21224680,LANDING_YEAR==1982) %>% 
#   dplyr::select(LANDING_YEAR,LANDING_MONTH,LANDING_DAY,FTID,
#                 PACFIN_SPECIES_COMMON_NAME,PACFIN_SPECIES_SCIENTIFIC_NAME,LANDED_WEIGHT_LBS)
# A1 %>% filter(PACFIN_SPECIES_SCIENTIFIC_NAME=="MERLUCCIUS PRODUCTUS")
# A1 %>% filter(LANDING_MONTH==2,LANDING_DAY==17)
# 
# #1983
# A2 <- dat %>% filter(VESSEL_ID==21223484,LANDING_YEAR==1983) %>% 
#   dplyr::select(LANDING_YEAR,LANDING_MONTH,LANDING_DAY,FTID,
#                 PACFIN_SPECIES_COMMON_NAME,PACFIN_SPECIES_SCIENTIFIC_NAME,LANDED_WEIGHT_LBS)
# A2 %>% filter(PACFIN_SPECIES_SCIENTIFIC_NAME=="MERLUCCIUS PRODUCTUS")
# A2 %>% filter(LANDING_MONTH==8,LANDING_DAY==31)
# 
# 
# #1990  No evidence of multiple FTID for single trips.
# A3 <- dat %>% filter(VESSEL_ID==21227079,LANDING_YEAR==1990) %>% 
#   dplyr::select(LANDING_YEAR,LANDING_MONTH,LANDING_DAY,FTID,
#                 PACFIN_SPECIES_COMMON_NAME,PACFIN_SPECIES_SCIENTIFIC_NAME,LANDED_WEIGHT_LBS)
# A3 %>% filter(PACFIN_SPECIES_SCIENTIFIC_NAME=="MERLUCCIUS PRODUCTUS")
# A3 %>% filter(LANDING_MONTH==10,LANDING_DAY==26)
# 
# 
# Q <- dat %>% filter(VESSEL_ID==21227079,LANDING_YEAR==1990) %>% 
#   dplyr::select(LANDING_YEAR,LANDING_MONTH,LANDING_DAY,FTID,
#                 PACFIN_SPECIES_COMMON_NAME,PACFIN_SPECIES_SCIENTIFIC_NAME,LANDED_WEIGHT_LBS,
# 
# CATCH_AREA_CODE,                CATCH_AREA_DESCRIPTION  ,      
# AREA_TYPE_CODE  ,               AREA_TYPE_NAME           ,      ORIG_PACFIN_CATCH_AREA_CODE   ,
# PACFIN_CATCH_AREA_CODE  ,       PACFIN_CATCH_AREA_NAME    ,     PACFIN_CATCH_AREA_DESCRIPTION ,
# PACFIN_GROUP_CATCH_AREA_CODE  , INPFC_AREA_TYPE_CODE      ,     COUNCIL_CODE     ,             
# PORT_CODE ,                     PORT_NAME                  ,    PACFIN_PORT_CODE ,             
# PACFIN_PORT_NAME  ,             PACFIN_PORT_DESCRIPTION     ,   PACFIN_GROUP_PORT_CODE)
# 
# Q[1:100,]
# 
# 
# 
# 
# ###############################################################################
# ###############################################################################
# ##### summarize the data to figure out when there are multiple FTID landed by a single boat on a single day.
# B <- dat %>% filter(PACFIN_GEAR_CODE %in% c("GFT","MDT","OTW")) %>% 
#         group_by(VESSEL_ID,FTID,PACFIN_GEAR_CODE,LANDING_YEAR,LANDING_MONTH,LANDING_DAY,PACFIN_PORT_CODE) %>%
#         summarize(weight=sum(LANDED_WEIGHT_LBS)) %>%
#         group_by(VESSEL_ID,PACFIN_GEAR_CODE,LANDING_YEAR,LANDING_MONTH,LANDING_DAY,PACFIN_PORT_CODE) %>%
#         summarize(n.FTID = length(FTID))
# 
# C <- B %>% group_by(PACFIN_GEAR_CODE,LANDING_YEAR,n.FTID) %>%
#   summarize(length(n.FTID)) %>% as.data.frame()
# 
# # Pull out some specific example to look at:
# B %>% filter(n.FTID>1,LANDING_YEAR==2002)
# 
# 
# # THIS ONE INVOLVES SELLING TO MULTIPLE DEALERS SO THUS MULTIPLE FISH TIX
# A3 <- dat %>% filter(VESSEL_ID==21217605,LANDING_YEAR==1984,LANDING_MONTH==7,LANDING_DAY==11) %>% 
#   dplyr::select(LANDING_YEAR,LANDING_MONTH,LANDING_DAY,FTID,
#                 PACFIN_SPECIES_COMMON_NAME,PACFIN_SPECIES_SCIENTIFIC_NAME,LANDED_WEIGHT_LBS)
# 
# # This one involves multiple dealers with sorting by species
# A4 <- dat %>% filter(VESSEL_ID==21217513,LANDING_YEAR==1991,LANDING_MONTH==5,LANDING_DAY==14) %>% 
#   dplyr::select(LANDING_YEAR,LANDING_MONTH,LANDING_DAY,FTID,
#                 PACFIN_SPECIES_COMMON_NAME,PACFIN_SPECIES_SCIENTIFIC_NAME,LANDED_WEIGHT_LBS)
# # This one involves multiple dealers with sorting by species
# A5 <- dat %>% filter(VESSEL_ID==21217513,LANDING_YEAR==1991,LANDING_MONTH==5,LANDING_DAY==14) %>% 
#   dplyr::select(LANDING_YEAR,LANDING_MONTH,LANDING_DAY,FTID,
#                 PACFIN_SPECIES_COMMON_NAME,PACFIN_SPECIES_SCIENTIFIC_NAME,LANDED_WEIGHT_LBS)
# # This one involves overage designation for bycatch on one ticket and regular sales on another
# A6 <- dat %>% filter(VESSEL_ID==21221573,LANDING_YEAR==2010,LANDING_MONTH==6,LANDING_DAY==16) %>% 
#   dplyr::select(LANDING_YEAR,LANDING_MONTH,LANDING_DAY,FTID,
#                 PACFIN_SPECIES_COMMON_NAME,PACFIN_SPECIES_SCIENTIFIC_NAME,LANDED_WEIGHT_LBS)
# 
# # This one involves pacific cod separated out onto a different ticket number
# A7 <- dat %>% filter(VESSEL_ID==21217620,LANDING_YEAR==2002,LANDING_MONTH==2,LANDING_DAY==22) %>% 
#   dplyr::select(LANDING_YEAR,LANDING_MONTH,LANDING_DAY,FTID,
#                 PACFIN_SPECIES_COMMON_NAME,PACFIN_SPECIES_SCIENTIFIC_NAME,LANDED_WEIGHT_LBS)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#   
# ggplot(B) +
#     geom_point(aes(y=n.FTID,x=LANDING_YEAR)) +
#     facet_wrap(~PACFIN_GEAR_CODE)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ggplot(whiting.a) +
#     geom_point(aes(y=x,x=LANDING_YEAR),alpha=0.1) +
#   facet_wrap(~PACFIN_GEAR_CODE)
# 
# length(whiting.only$FTID)
# 
# whiting.a %>% filter(PACFIN_GEAR_CODE=="MDT")
# 
# whiting.only %>% filter(FTID==3949342)
# 
# dim(whiting.only)
# length(unique(whiting.only$FTID))
# 
# hist(whiting.a %>% filter(PACFIN_GEAR_CODE=="MDT") %>% as.data.frame() %>% 
#        dplyr::select(x) %>% unlist(.) %>% as.numeric(as.character(.)))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# whiting.target <- whiting.all.sp %>% mutate(prop.weight = WEIGHT / TOT.WEIGHT) #%>% 
#                     filter(PACFIN_SPECIES_SCIENTIFIC_NAME=="MERLUCCIUS PRODUCTUS")
# 
# A <- whiting.target %>% group_by(THOMSON_FISHERY_CODE,LANDING_YEAR,LANDING_MONTH) %>% 
#       summarise(N=length(LANDING_DAY),MASS=sum(WEIGHT))
# A$FRAC_YEAR_MONTH = (A$LANDING_MONTH / 13 ) + A$LANDING_YEAR
# 
# 
# ### LOOKING AT THOMSON_FISHERY_CODES
# 
# ggplot(whiting.target) +
#     geom_point(aes(x=prop.weight,y=log(TOT.WEIGHT,10),alpha=0.5)) + 
#     facet_grid(LANDING_YEAR~THOMSON_FISHERY_CODE)
# 
# ggplot(A) +
#     geom_point(aes(y=N,x=FRAC_YEAR_MONTH),alpha=0.5) +
#     facet_wrap(~THOMSON_FISHERY_CODE,scales = 'free')
# 
# ggplot(A) +
#   geom_point(aes(y=MASS,x=FRAC_YEAR_MONTH),alpha=0.5) +
#   facet_wrap(~THOMSON_FISHERY_CODE)#,scales = 'free')
# 
# 
# ## OK.  I looked a bunch at the whiting.target file and decided that I could exclude
# 
# 
# 
# 
# 
# gte = c("FTS","SST","RLT","DNT","SHT")
# whiting.target <- whiting.target %>% filter(!PACFIN_GEAR_CODE %in% gte)
# 


#Fleet type (limited entry = "LE", open access = "OA", trl Indian = "TI", research = "R", unknown = "XX")

# Thomson Fishery Code allows one to pull out particular fleets.
# '01' = Dungeness Crab Pot, 
# '02' = Other Crab Pot, 
# '03' = Lobster Pot, 
# '04' = Prawn Pot, 
# '05' = Pink Shrimp Trawl, 
# '06' = Prawn Trawl, 
# '07' = Whiting Trawl, 
# '08' = DTS Trawl, 
# '09' = Other Groundfish Trawl, 
# '10' = Sablefish Pot, 
# '11' = Sablefish Hook & Line, 
# '12' = NearShore Rockfish Pot,
# '13' = NearShore Rockfish Hook & Line, 
# '14' = Non NearShore Rockfish Port, 
# '15' = Non NearShore Rockfish Hook & Line, 
# '16' = Halibut Hook & Line, 
# '17' = Halibut Trawl, 
# '18' = Halibut Net, 
# '19' = Sturgeon Net, 
# '20' = Salmon Troll, 
# '21' = Salmon Net, 
# '22' = Squid Seine, 
# '23' = CPS Seine, 
# '24' = Herring, 
# '25' = WS Bass, 
# '26' = Tuna Troll, 
# '27' = Tuna Seine, 
# '28' = Shark Net, 
# '29' = Hagfish Pot, 
# '30' = Swordfish Net, 
# '31' = Swordfish Other, 
# '32' = Clam Dredge, 
# '33' = Clam Scallop Other, 
# '34' = Oyster, 
# '35' = Scallop Trawl, 
# '36' = Abalone, 
# '37' = Urchin, 
# '38' = Sea Cucumber, 
# '39' = Groundfish Net, 
# '40' = Groundfish Troll, 
# '41' = Bait Ghost Shrimp, 
# '42' = Bait Shrimp, 
# '00' = Everything Else

