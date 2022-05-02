# source("~/Documents/GitHub/Chinook_Bycatch/scripts/ashop_base.R") #this will be used later
# source("~/Documents/GitHub/Chinook_Bycatch/scripts/ashop_chinook_base.R") #this will be used later
library(here)
library(tidyverse)
library(dplyr)
library(gplots)
library(DataCombine)
###################################################################################################################################################################

#Step 1: LOAD ASHOP AND ASSIGN DATA INTO REGIONS
###################################################################################################################################################################
base.dir <- "/Users/ole.shelton/Github"
data.dir <- paste0(base.dir,"/Orca_Salmon_DATA/Effort info/Trawl-US/ASHOP")
write.dir <- paste0(base.dir,"/spring-chinook-distribution/Processed Data/Effort Data")

setwd(data.dir)

ashop = read.csv("all_hauls_1979-2017_AOS.csv", stringsAsFactors = FALSE)

ashop$region <- cut(ashop$lat.end,breaks=c(-Inf,
                                           37.1833,
                                           38.958333,
                                           40.08333,
                                           42,
                                           42.6725,
                                           44.015,
                                           45.76666,
                                           46.64,
                                           50), labels=c("MONT",
                                                         "SFB",
                                                         "MEN",
                                                         "NCA",
                                                         "SOR",
                                                         "COR",
                                                         "NOR",
                                                         "COL",
                                                         "WAC"))
###################################################################################################################################################################
#Step 2 QUANTIFY EFFORT IN BOAT DAYS
###################################################################################################################################################################

#calculate effort based on tows/day and then run lm() to look at tow day boat day correlation
#df_tow<- ashop %>%
# dplyr::group_by(region, Year, Month, Day) %>% #do not sum across season because winter wont come out as consecutive because months cross over year
#  dplyr::summarise(tow_count = sum(n.tows))
#df_effort<- left_join(df_tow, df_boat)

#df_effort$Year <- as.character(df_effort$Year)
#ggplot(df_effort, aes(x=boats_day, y=tow_count, color= Year)) +
#  geom_point(size=2, shape=23)

#linearMod <- lm(boats_day ~ tow_count, data=df_effort)  # build linear regression model on full data
#print(linearMod)
#summary(linearMod)

#calculate effort based on boats/day
observed_effort <- ashop %>%
  dplyr::group_by(Year, Month, Day, region) %>%
  filter(!Day == 0) %>%
  dplyr::summarise(boats_day = n_distinct(CRUISE)) %>%
  dplyr::group_by(Year, Month, region) %>%
  dplyr::summarise(observed_boat_days = sum(boats_day))

#DATA FOR EXPANSION RATES, ALREADY IN ASHOP DF.

expansion = read.csv("A-SHOP_expansion_rates.csv", stringsAsFactors = FALSE)

#CALCULATE TOTAL EFFORT (UNOBSERVED + OBSERVED)
sum_tows_weight <- ashop %>%
  group_by(Year, Month, region, period, hauls_expansion, weight_expansion) %>%
  dplyr::summarise(tow_sum= sum(n.tows),
            wt_sum = sum(OFFICIAL_TOTAL_CATCH_MT)) %>%
  mutate(Month = as.integer(Month))

sum1 <- left_join(sum_tows_weight, observed_effort, by = c("Year", "Month", "region"))

total_effort <- sum1 %>%
  dplyr::mutate(tow_sum = as.numeric(tow_sum),
                unobs_haul = tow_sum * hauls_expansion,
                unobs_wt = wt_sum * weight_expansion,
                unobserved_haul_or_wt = unobs_wt + unobs_haul,
                observed_haul_or_wt = case_when(period == 1 ~ tow_sum,
                                                period == 2  ~ wt_sum,
                                                TRUE ~ 0)) %>%
  dplyr::select(-c(unobs_haul, unobs_wt, tow_sum, wt_sum)) %>%
  filter(!observed_haul_or_wt == 0) %>%
  dplyr::mutate(unobserved_boat_days = (observed_boat_days * unobserved_haul_or_wt)/observed_haul_or_wt,
                total_boat_days_region=unobserved_boat_days+observed_boat_days)

###################################################################################################################################################################
#Step 3: PLOT
###################################################################################################################################################################
#Step 3A: PLOT OBSERVED EFFORT IN HEATMAP
###################################################################################################################################################################
#do year.month combo to merge in
temp.year <- as.character(c(1979:2017))
temp.month <- as.character(c(01:12))
year.month<- expand.grid(temp.year, temp.month) #fully factorial match between month and year
year.month$Var2<- str_pad(year.month$Var2, width= 2, pad = "0", side="left") #add a zero before month 1-9 so that I can keep months in order
year.month <- unite(year.month, "year.month", c("Var1", "Var2"), sep = ".", remove= TRUE) #join month and year in its own dataframe

#create year.month for df_boat
observed_effort$Month<- str_pad(observed_effort$Month, width= 2, pad = "0", side="left") #add a zero before month 1-9 so that I can keep months in order
observed_effort <- observed_effort %>%
  tidyr::unite("year.month", c("Year", "Month"), sep = ".", remove= FALSE) #create year.month so this can be merged with fully factorial year.month and create blanks

#merge the two to get fully factorial year.months
merge.observed <- merge(year.month, observed_effort, all = TRUE)
merge.observed$region.use <- recode(merge.observed$region, 'WAC'=1, 'COL'=2, 'NOR'=3, 'COR'=4,'SOR'=5, 'NCA'= 6, 'MEN'=7, 'SFB' = 8, 'MONT' = 9)


#create an label for x axis with blanks so you can see years
temp.lab = as.data.frame(c(1979:2017)) #create df with all years
temp <- as.data.frame(temp.lab[rep(1:nrow(temp.lab),1,each=12),]) #duplicate each row 12 times
temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`[duplicated(temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`)] <- " " #fill duplicates with a space
temp$label<-  temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`  #change column name and remove old column
temp <- temp %>%
  dplyr::select(-c(`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`))

#recode regions so they are numbered with North = 1, so north is on top.
temp.region <- c('WAC','COL','NOR','COR','SOR', 'NCA', 'MEN', 'SFB', 'MONT')

spread.observed <- merge.observed %>%
  dplyr::select(-c(Year, Month, region)) %>%
  tidyr::spread(year.month, observed_boat_days)

#set up matrix for heatmap
spread.observed <- spread.observed[1:nrow(spread.observed),]
region<- spread.observed$region.use
spread.observed<- spread.observed[,2:468]
matrix <- data.matrix(spread.observed)
row.names(matrix) <- region
matrix[matrix < 0.1] <- NA #turn 0 to NA so that it doesnt get messed up in log function

matrix <- log(matrix) #log matrix values so that scale

col.br= colorRampPalette(c("white", "blue","purple", "red"))
# year_list <- as.list()
# par(mar=c(10,4,4,2))
heatmap.2(matrix, Rowv=FALSE, Colv=FALSE,
          xlab = "Year", ylab = "Region",
          scale="none", margins=c(4,6),
          dendrogram="none", trace="none",
          col=col.br(20), #breaks=pairs.breaks,
          na.color = "black",
          key =TRUE,
          keysize = 1.5, #labRow = year_list,
          key.title = "log scale",
          adjCol = c(0.5,1.1),
          labCol = temp$label, #tell it to use dummy variable for X axis years
          labRow = temp.region, #use dummy variable for Y axis regions so they can be north to south
          cexCol = 0.6,
          sepcolor = "black",
          main = "ASHOP Observed Effort\n(log boat days)",
          density.info = ("none"),
          #( "bottom.margin", "left.margin", "top.margin", "right.margin" )
          key.par=list(mar=c(2,1, 1,0.1)))

###################################################################################################################################################################
#Step 3B: PLOT TOTAL (OBESRVED + UNOBSERVED) EFFORT IN HEATMAP
###################################################################################################################################################################
#do year.month combo to merge in
temp.year <- as.character(c(1979:2017))
temp.month <- as.character(c(01:12))
year.month<- expand.grid(temp.year, temp.month) #fully factorial match between month and year
year.month$Var2<- str_pad(year.month$Var2, width= 2, pad = "0", side="left") #add a zero before month 1-9 so that I can keep months in order
year.month <- unite(year.month, "year.month", c("Var1", "Var2"), sep = ".", remove= TRUE) #join month and year in its own dataframe

#create year.month for df_boat
total_effort$Month<- str_pad(total_effort$Month, width= 2, pad = "0", side="left") #add a zero before month 1-9 so that I can keep months in order
total_effort <- total_effort %>%
  tidyr::unite("year.month", c("Year", "Month"), sep = ".", remove= FALSE) #create year.month so this can be merged with fully factorial year.month and create blanks

#merge the two to get fully factorial year.months
merge.total <- merge(year.month, total_effort, all = TRUE)
merge.total$region.use <- recode(merge.total$region, 'WAC'=1, 'COL'=2, 'NOR'=3,'COR'=4, 'SOR'=5, 'NCA'= 6, 'MEN'=7, 'SFB' = 8, 'MONT' = 9)

#create an label for x axis with blanks so you can see years
temp.lab = as.data.frame(c(1979:2017)) #create df with all years
temp <- as.data.frame(temp.lab[rep(1:nrow(temp.lab),1,each=12),]) #duplicate each row 12 times
temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`[duplicated(temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`)] <- " " #fill duplicates with a space
temp$label<-  temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`  #change column name and remove old column
temp <- temp %>%
  dplyr::select(-c(`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`))
#recode regions so they are numbered with North = 1, so north is on top.
temp.region <- c('WAC','COL','NOR','COR','SOR', 'NCA', 'MEN', 'SFB', 'MONT')

spread.total <- merge.total %>%
  dplyr::select(-c(Year, Month, region, period, hauls_expansion, observed_haul_or_wt, weight_expansion, total_boat_days_region, unobserved_haul_or_wt)) %>%
  tidyr::spread(year.month, total_boat_days_region)

#set up matrix for heatmap
spread.total<- spread.total[1:nrow(spread.total),]
region<- spread.total$region.use
spread.total<- spread.total[,2:469]
matrix <- data.matrix(spread.total)
row.names(matrix) <- region
matrix[matrix < 0.1] <- NA #turn 0 to NA so that it doesnt get messed up in log function

matrix <- log(matrix) #log matrix values so that scale

col.br= colorRampPalette(c("white", "blue","purple", "red"))
# year_list <- as.list()
# par(mar=c(10,4,4,2))
heatmap.2(matrix, Rowv=FALSE, Colv=FALSE,
          xlab = "Year", ylab = "Region",
          scale="none", margins=c(4,6),
          dendrogram="none", trace="none",
          col=col.br(20), #breaks=pairs.breaks,
          na.color = "black",
          key =TRUE,
          keysize = 1.5, #labRow = year_list,
          key.title = "log scale",
          adjCol = c(0.5,1.1),
          labCol = temp$label, #tell it to use dummy variable for X axis years
          labRow = temp.region, #use dummy variable for Y axis regions so they can be north to south
          cexCol = 0.6,
          sepcolor = "black",
          main = "ASHOP Total Effort\n(log boat days)",
          density.info = ("none"),
          #( "bottom.margin", "left.margin", "top.margin", "right.margin" )
          key.par=list(mar=c(2,1, 1,0.1)))

###################################################################################################################################################################
#Step 4: GET MEDIAN SAMPLE FRACTION
###################################################################################################################################################################
library(plyr)
fraction_year_summ <- ddply(ashop, c("Year"), summarise,
                            tow_sum    = sum(n.tows),
                            wt_sum = sum(OFFICIAL_TOTAL_CATCH_MT))
#setwd("~/GitHub/Chinook_Bycatch/data/ASHOP")
#setwd("~/Documents/GitHub/Chinook_Bycatch")
#setwd("~/GitHub/Chinook_Bycatch/data/ASHOP") # OS

#EARLY
#using expansion rate get the total tows/year including observed and unobserved tows
expansion$Year <- as.numeric(expansion$Year)
early_expansion <- expansion %>%
  dplyr::filter(period == 1) #this is expansion based on hauls
#issue: two different expansion rates for 1990- take the mean
early_expansion <- ddply(early_expansion, c("Year"), summarise,
                         hauls_expansion    = mean(hauls_expansion)) #TAKES MEAN OF different expansion rates for 1990 because we dont know what season the different rates refer to

fraction_year_summ$Year <- as.numeric(fraction_year_summ$Year)

early_expansion_summ <- left_join(fraction_year_summ, early_expansion, by = "Year")

early_expansion_summ <- early_expansion_summ %>%
  dplyr::filter(!is.na(hauls_expansion)) %>%
  dplyr::mutate(total_tow_est = hauls_expansion * tow_sum) %>%
  dplyr::mutate(tow_frac_observed = tow_sum / total_tow_est)
#how many tows occured that year (with and without observers)

###################################################################################################################################################################
#Step 4a: LOAD AND FORMAT BYCATCH DATA TO ASSIGN TO REGIONS AND GET WEIGHT/YEAR ACTUALLY SURVEYED BY OBSERVERS
###################################################################################################################################################################
chinook_early = read.csv("A-SHOP_Salmon_haul+catch data_1979-1990_092718_Chinook.csv", stringsAsFactors = FALSE)
chinook_late = read.csv("A-SHOP_Salmon_haul+catch data_1990-2017_092718_Chinook.csv", stringsAsFactors = FALSE)

chinook_early<- chinook_early %>%
  separate(LATITUDE, c("lat1", "lat2"), 2) %>%
  separate(LONGITUDE, c("lon1", "lon2"), 3) %>%
  unite("LATITUDE", c("lat1", "lat2"), sep = ".") %>%
  unite("LONGITUDE", c("lon1", "lon2"), sep = ".") %>%
  mutate(LATITUDE = as.numeric(LATITUDE)) %>%
  mutate(LONGITUDE = as.numeric(LONGITUDE)) %>%
  mutate(Long = ifelse(LONGITUDE > 1, LONGITUDE * -1, LONGITUDE)) %>% #fix the (-)'s that are missing from some longitudes
  dplyr::select(-c(LONGITUDE)) %>%
  mutate(EXTRAPOLATED_WEIGHT_KG = SPECIES_HAUL_WEIGHT)
chinook_early$region <- cut(chinook_early$LATITUDE, breaks=c(-Inf,
                                                             37.1833,
                                                             38.958333,
                                                             40.08333,
                                                             42,
                                                             42.6725,
                                                             44.015,
                                                             45.76666,
                                                             46.64,
                                                             50), labels=c("MONT",
                                                                           "SFB",
                                                                           "MEN",
                                                                           "NCA",
                                                                           "SOR",
                                                                           "COR",
                                                                           "NOR",
                                                                           "COL",
                                                                           "WAC"))



#FORMAT LATE DATA FOR MERGE
chinook_late <- chinook_late %>%
  mutate(WEIGHT_IN_SAMPLE = SAMPLE_WEIGHT_KG) %>%
  mutate(FISHING_DEPTH_M = FISHING_DEPTH_FATHOMS * 1.8288) %>% #FATHOMS TO M
  mutate(LATITUDE = as.numeric(LATDD_END)) %>%
  mutate(LONGITUDE = LONDD_END) %>%
  mutate(DURATION = DURATION_IN_MIN)%>%
  mutate(LONGITUDE = as.numeric(LONGITUDE)) %>%
  mutate(Long = ifelse(LONGITUDE > 1, LONGITUDE * -1, LONGITUDE)) %>% #fix the (-)'s that are missing from some longitudes
  dplyr::select(-c(LONGITUDE))


chinook_late$region <- cut(chinook_late$LATITUDE, breaks=c(-Inf,
                                                           37.1833,
                                                           38.958333,
                                                           40.08333,
                                                           42,
                                                           42.6725,
                                                           44.015,
                                                           45.76666,
                                                           46.64,
                                                           50), labels=c("MONT",
                                                                         "SFB",
                                                                         "MEN",
                                                                         "NCA",
                                                                         "SOR",
                                                                         "COR",
                                                                         "NOR",
                                                                         "COL",
                                                                         "WAC"))



#GET WEIGHT/YEAR ACTUALLY SURVEYED BY OBSERVERS
chinook_early <- chinook_early %>%
  dplyr::mutate(wt_sampled =  ((WEIGHT_SAMPLED_KG/1000))) %>%
  dplyr::mutate(fraction_sampled =  wt_sampled / OFFICIAL_TOTAL_CATCH_MT)
#MT of how much catch was actually surveyed

# OCCASIONALLY FOR EARLY DATA THE MT SURVEYED IS ENTERED AS BEING GREATER THAN THE OFFICIAL TOTAL CATCH...
# FOR NOW, IF THE FRACTION_SAMPLED > 1, IT IS FILTERED OUT.
chinook_early <- chinook_early %>% filter(!fraction_sampled > 1)
#________________________________________________________________________________________________________________________________________________________
###########    MONTH // EARLY
chinook_early_month <- ddply(chinook_early, c("region", "Month"), summarise,
                             N= length(fraction_sampled),
                             mean_chinook_fraction =mean(fraction_sampled),
                             median = median(fraction_sampled),
                             sd = sd(fraction_sampled),
                             se = sd / sqrt(N)) #deals with different expansion rates for 1990 - dont know what season the different rates refer to

#There is definitley a better way to do this!!!

chinook_early_month$'1979' <- (chinook_early_month$median * 0.707)
chinook_early_month$'1980' <- (chinook_early_month$median * 0.707)
chinook_early_month$'1981' <- (chinook_early_month$median * 0.707)
chinook_early_month$'1982' <- (chinook_early_month$median * 0.707)
chinook_early_month$'1983' <- (chinook_early_month$median * 0.707)
chinook_early_month$'1984' <- (chinook_early_month$median * 0.674)
chinook_early_month$'1985' <- (chinook_early_month$median * 0.697)
chinook_early_month$'1986' <- (chinook_early_month$median * 0.735)
chinook_early_month$'1987' <- (chinook_early_month$median * 0.724)
chinook_early_month$'1988' <- (chinook_early_month$median * 0.718)
chinook_early_month$'1989' <- (chinook_early_month$median * 0.722)
chinook_early_month$'1990' <- (chinook_early_month$median * 0.513)

chinook_early_gather_mnth <- gather(chinook_early_month, "year", "fraction", 8:19)

#now weed out months and regions that didnt actually have data
#EARLY
chinook_early_group <- chinook_early %>%
  dplyr::group_by(Year, region, Month) %>%
  dplyr::count(SPECIES)

chinook_early_group <- chinook_early_group %>% unite("id", c("Year", "region", "Month"), remove = FALSE)


chinook_early_gather_mnth <- chinook_early_gather_mnth %>% unite("id", c("year", "region", "Month"), remove = FALSE)

chinook_early_fraction_mnth <- semi_join(chinook_early_gather_mnth, chinook_early_group, by = "id")     #DROPS THE SEASONS IN REGIONS WHEN THEY DONT SHOW UP IN CERTAIN YEARS

chinook_early_fraction_mnth <- chinook_early_fraction_mnth %>%
  dplyr::select(-c(id))

#________________________________________________________________________________________________________________________________________________________

#LATE
#observed_frac: CATCH WITH OBSERVERS ON BOARD/TOTAL CATCH PER YEAR

late_expansion <- expansion %>%  #pull out expansion from the late period
  dplyr::filter(period == 2) #this is expansion based on weight

fraction_year_summ$Year <- as.numeric(fraction_year_summ$Year) #fraction year sum has the sum of MT catch where observers were onboard/year

late_expansion_summ <- left_join(fraction_year_summ, late_expansion, by = "Year") #join so that I can multiply MT by expansion rate and get total MT for each year.

late_expansion_summ <- late_expansion_summ %>%
  dplyr::filter(!is.na(weight_expansion)) %>%
  dplyr::select(-c(hauls_expansion)) %>%
  dplyr::mutate(total_wt_est = weight_expansion * wt_sum) # total_wt_est = TOTAL CATCH/YEAR (MT)  (OBSERVED AND UNOBSERVED)

late_expansion_summ <- late_expansion_summ %>%
  dplyr::mutate(observed_frac = wt_sum / total_wt_est) #CATCH WITH OBSERVERS ON BOARD/TOTAL CATCH PER YEAR
late_expansion_select <- late_expansion_summ %>%
  dplyr::select(Year, observed_frac)

chinook_late <- chinook_late %>%
  dplyr::mutate(chinook_expansion =  ((EXTRAPOLATED_WEIGHT_KG/1000) / (WEIGHT_IN_SAMPLE/1000))) %>% #get my own expansion rate for the amount of total catch that was sub-sampled for chinook.
  #can figure that out based on the extrapolated counts that they provide - divide extrapolated wt by actual sample weight yield the expansion rate they used for that haul
  dplyr::mutate(wt_sampled =  OFFICIAL_TOTAL_CATCH_MT/ chinook_expansion) %>% #use expansion rate to calculate how much weight in MT was surveyd for chinook.
  #fraction_sampled is the fraction of each haul that was actually surveyed for chinook
  dplyr::mutate(fraction_sampled =  wt_sampled / OFFICIAL_TOTAL_CATCH_MT)

#________________________________________________________________________________________________________________________________________________________
###########    MONTH // LATE
chinook_late_month <- ddply(chinook_late, c("region", "Month"), summarise,
                            N= length(fraction_sampled),
                            mean_chinook_fraction =mean(fraction_sampled),
                            median = median(fraction_sampled),
                            sd = sd(fraction_sampled),
                            se = sd / sqrt(N)) #deals with different expansion rates for 1990 - dont know what season the different rates refer to

chinook_late_month$'1990' <- (chinook_late_month$median * 0.412)
chinook_late_month$'1991' <- (chinook_late_month$median * 0.489)
chinook_late_month$'1992' <- (chinook_late_month$median * 0.641)
chinook_late_month$'1993' <- (chinook_late_month$median * 0.605)
chinook_late_month$'1994' <- (chinook_late_month$median * 0.514)
chinook_late_month$'1995' <- (chinook_late_month$median * 0.581)
chinook_late_month$'1996' <- (chinook_late_month$median * 0.596)
chinook_late_month$'1997' <- (chinook_late_month$median * 0.738)
chinook_late_month$'1998' <- (chinook_late_month$median * 0.808)
chinook_late_month$'1999' <- (chinook_late_month$median * 0.780)
chinook_late_month$'2000' <- (chinook_late_month$median * 0.915)
chinook_late_month$'2001' <- (chinook_late_month$median * 0.972)
chinook_late_month$'2002' <- (chinook_late_month$median * 0.995)
chinook_late_month$'2003' <- (chinook_late_month$median * 0.991)
chinook_late_month$'2004' <- (chinook_late_month$median * 0.998)
chinook_late_month$'2005' <- (chinook_late_month$median * 0.999)
chinook_late_month$'2006' <- (chinook_late_month$median * 0.985)
chinook_late_month$'2007' <- (chinook_late_month$median * 0.996)
chinook_late_month$'2008' <- (chinook_late_month$median * 0.993)
chinook_late_month$'2009' <- (chinook_late_month$median * 0.999)
chinook_late_month$'2010' <- (chinook_late_month$median * 0.999)
chinook_late_month$'2011' <- (chinook_late_month$median * 0.999)
chinook_late_month$'2012' <- (chinook_late_month$median * 0.992)
chinook_late_month$'2013' <- (chinook_late_month$median * 0.997)
chinook_late_month$'2014' <- (chinook_late_month$median * 0.998)
chinook_late_month$'2015' <- (chinook_late_month$median * 0.998)
chinook_late_month$'2016' <- (chinook_late_month$median * 0.999)
chinook_late_month$'2017' <- (chinook_late_month$median * 0.998)

chinook_late_gather_mnth <- gather(chinook_late_month, "year", "fraction", 8:35)

#now weed out years that didnt actually have data
chinook_late_group <- chinook_late %>%
  dplyr::group_by(Year, region, Month) %>%
  dplyr::count(SPECIES)
chinook_late_group <- chinook_late_group %>% unite("id", c("Year", "region", "Month"), remove = FALSE)

chinook_late_gather_mnth <- chinook_late_gather_mnth %>% unite("id", c("year", "region", "Month"), remove = FALSE)

chinook_late_fraction_mnth <- semi_join(chinook_late_gather_mnth, chinook_late_group, by = "id")     #COMBINE IN SEMI-JOIN AND IT DROPS THE SEASONS IN REGIONS WHEN THEY DONT SHOW UP IN CERTAIN YEARS

chinook_late_fraction_mnth <- chinook_late_fraction_mnth %>%
  dplyr::select(-c(id))

#CBIND ALL
test <- rbind(chinook_early_gather_mnth, chinook_late_gather_mnth)

#region month
chinook_fraction_all_RM <- rbind(chinook_early_fraction_mnth, chinook_late_fraction_mnth)
chinook_fraction_all_RM$Month <-as.numeric(chinook_fraction_all_RM$Month)
#FRACTION COLUMN IS TRUE SAMPLE FRACTION (variable of interest)

###################################################################################################################################################################
#Step 5: FORMAT AND MERGE CHINOOK BYCATCH DATA + PLOT
###################################################################################################################################################################
#CHINOOK EARLY AND LATE ARE LOADED, FORMATTED, AND REGIONS ARE ASSIGNED IN STEP 4A.
chinook_early1<- chinook_early %>%
  mutate(LONGITUDE = Long) %>%
  dplyr::select(c(CRUISE, HAUL, HAUL_JOIN, SPECIES, NUMBER_IN_SAMPLE, region, fraction_sampled, wt_sampled, FISHING_DEPTH_M, EXTRAPOLATED_WEIGHT_KG, Year, Month, Day, DURATION, LATITUDE, LONGITUDE))

chinook_late1 <- chinook_late %>%
  mutate(LONGITUDE = Long) %>%
  mutate(NUMBER_IN_SAMPLE = SAMPLE_COUNT) %>%
  dplyr::select(c(CRUISE, HAUL, HAUL_JOIN, SPECIES, NUMBER_IN_SAMPLE, region, fraction_sampled, wt_sampled, FISHING_DEPTH_M, EXTRAPOLATED_WEIGHT_KG, Year, Month, Day, DURATION, LATITUDE, LONGITUDE))

chinook_bycatch <- rbind(chinook_early1, chinook_late1)
chinook_bycatch <- chinook_bycatch %>%
  filter(!Year<1901)

#HEATMAP - chinook bycatch by region and year
#create dummy variable to merge in
temp.year <- as.character(c(1979:2016))
temp.month <- as.character(c(01:12))
year.month<- expand.grid(temp.year, temp.month) #fully factorial match between month and year
year.month$Var2<- str_pad(year.month$Var2, width= 2, pad = "0", side="left") #add a zero before month 1-9 so that I can keep months in order
year.month <- unite(year.month, "year.month", c("Var1", "Var2"), sep = ".", remove= TRUE) #join month and year in its own dataframe


chinook_bycatch$Month<- str_pad(chinook_bycatch$Month, width= 2, pad = "0", side="left") #add a zero before month 1-9 so that I can keep months in order
chinook_temp <- chinook_bycatch %>%
  dplyr::filter(!Year == 1900) %>%
  filter(!is.na(region))%>%
  tidyr::unite("year.month", c("Year", "Month"), sep = ".", remove= FALSE) #create year.month so this can be merged with fully factorial year.month and create blanks

merge.chinook <- merge(year.month, chinook_temp, all = TRUE)
merge.chinook$region.use <- recode(merge.chinook$region, 'WAC'=1, 'COL'=2, 'NOR'=3,'SOR'=4, 'NCA'= 5, 'MEN'=6, 'SFB' =7, 'MONT' =8)

chinook_spread <- merge.chinook %>%
  dplyr::group_by(year.month, region.use) %>%
  dplyr::summarise(total_weight_kg = sum(EXTRAPOLATED_WEIGHT_KG)) %>%
  tidyr::spread(year.month, total_weight_kg)

#create an label for x axis with blanks so you can see years
temp.lab = as.data.frame(c(1979:2017)) #create df with all years
temp <- as.data.frame(temp.lab[rep(1:nrow(temp.lab),1,each=12),]) #duplicate each row 12 times
temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`[duplicated(temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`)] <- " " #fill duplicates with a space
temp$label<-  temp$`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`  #change column name and remove old column
temp <- temp %>%
  dplyr::select(-c(`temp.lab[rep(1:nrow(temp.lab), 1, each = 12), ]`))

#recode regions so they are numbered with North = 1, so north is on top.
temp.region <- c('WAC','COL','NOR','SOR', 'NCA', 'MEN', 'SFB', 'MONT')

#set up matrix for heatmap
chinook_spread<- chinook_spread[1:6,]
region<- chinook_spread$region.use
chinook_spread<- chinook_spread[,2:463]
chinook_matrix <- data.matrix(chinook_spread)
row.names(chinook_matrix) <- region
chinook_matrix[chinook_matrix < 0.1] <- NA #turn 0 to NA so that it doesnt get messed up in log function

#add two rows to chinook_spread to show that SFB and MONT did not have any bycatch weasier to compare to effort plot
chinook_matrix <- rbind(chinook_matrix, rep(NA,ncol(chinook_matrix)))   ##add row of NAs
chinook_matrix <- rbind(chinook_matrix, rep(NA,ncol(chinook_matrix)))
rownames(chinook_matrix) <- 1:8

chinook_matrix <- log(chinook_matrix) #log matrix values so that scale

col.br= colorRampPalette(c("white", "blue","purple", "red"))
# year_list <- as.list()
# par(mar=c(10,4,4,2))
heatmap.2(chinook_matrix, Rowv=FALSE, Colv=FALSE,
          xlab = "Year", ylab = "Region",
          scale="none", margins=c(4,6),
          dendrogram="none", trace="none",
          col=col.br(20), #breaks=pairs.breaks,
          na.color = "black",
          key =TRUE,
          keysize = 1.5, #labRow = year_list,
          key.title = "log scale",
          adjCol = c(0.5,1.1),
          labCol = temp$label, #tell it to use dummy variable for X axis years
          labRow = temp.region, #use dummy variable for Y axis regions so they can be north to south
          cexCol = 0.6,
          sepcolor = "black",
          main = "Chinook Bycatch\n(log of estimated haul weight)",
          density.info = ("none"),
          #( "bottom.margin", "left.margin", "top.margin", "right.margin" )
          key.par=list(mar=c(2,1, 1,0.1)))

#WRITE CSV'S
setwd(write.dir)
# Write to Processed Data file in working (not purely data) Git Repo
write.csv(ashop, file="ashop_all_cleaned.csv")
write.csv(total_effort, file="ashop_total_effort.csv")
write.csv(chinook_fraction_all_RM, file="ashop_sample_fraction.csv") #FRACTION OF ALL HAULS SURVEYED FOR CHINOOK DIVIDED BY MONTH, YEAR, AND REGION
write.csv(chinook_bycatch, file="ashop_chinook_bycatch.csv") #FRACTION OF ALL HAULS SURVEYED FOR CHINOOK DIVIDED BY MONTH, YEAR, AND REGION
