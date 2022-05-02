### This is a script to read in, process and make descriptive plot of the Ocean
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(fields)
library(RColorBrewer)
library(gridExtra)
library(lubridate)
library(GGally)
#base.dir <- "/Users/ole.shelton/GitHub"

TYPE ="MEAN"

# read in data and identifiers
# dat.temp <- read.csv(paste(base.dir,"/Salmon-Climate/Coastwide SST/regions_depth_zones_SST_PAT.csv",sep=""))
pdo.dat    <- read.csv(paste(base.dir,"/Salmon-Climate/Coastwide SST/PDO monthly.csv",sep=""))
npgo.dat    <- read.csv(paste(base.dir,"/Salmon-Climate/Coastwide SST/NPGO monthly.csv",sep=""))
dat.temp   <- read.csv(paste(base.dir,"/Salmon-Climate/Coastwide SST/Seasonal SST by regions & depth zones from OISST V2 daily data.csv",sep=""))
locations  <-  read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations.csv",sep=""))
if(loc_18=="TWO_OR"){ # deal with combining the two OREGON regions if appropriate
  locations  <-  read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations_plus_two_OR.csv",sep=""))
  locations <- locations %>% filter( location.name != "PUSO_out")
  locations$location.number <- 1:nrow(locations)
  dat.temp <- dat.temp %>% mutate(Region_num = ifelse(Region_num>=6,Region_num-1,Region_num))
}
if(loc_18 =="NCA_SOR_PUSO"){  # deal with combining the NCA and SOR regions if appropriate
  locations <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations_plus_NCA_SOR_PUSO.csv",sep=""))
  locations <- locations %>% filter( location.name != "PUSO_out")
  locations$location.number <- 1:nrow(locations)
  dat.temp <- dat.temp %>% mutate(Region_num = ifelse(Region_num>=5,Region_num-1,Region_num))
}

# This file identifies grid ids that touch land and allows those to be excluded
touchland.dat <- read.csv(paste(base.dir,"/Salmon-Climate/Coastwide SST/touch land.csv",sep=""))
# 
# THESE <- unique(touchland.dat$Grid_id)
# dat.temp <- dat.temp %>% filter(!Grid_id %in% THESE)

dat.area <- dat.temp %>% group_by(Region_num,Depth_zone) %>% summarise(tot_area_m2=sum(Area_m2))
dat.area$tot_area_shallow_km2 <- dat.area$tot_area_m2 * 1e-6
dat.area.deep  <- dat.temp %>% group_by(Region_num) %>% summarise(tot_area_deep_m2=sum(Area_m2))
dat.area.deep$tot_area_deep_km2 <- dat.area.deep$tot_area_deep_m2 * 1e-6

dat.temp <- dat.temp[dat.temp$Water == 1,]
dat.temp <- dat.temp %>% dplyr::select(-one_of(c("Gridcode","Water"))) %>%
              melt(.,id.vars=c("Region_num","Depth_zone","Grid_id","Area_m2"))  
dat.temp$variable <- as.character(dat.temp$variable)
dat.temp$Area_km2 <- dat.temp$Area_m2 * 1e-6

dat.temp$season.year <- dat.temp$variable
dat.temp$season.year <- substr(dat.temp$season.year,1,7)


if(TYPE=="MEAN"){
  dat.temp <- dat.temp[grep("x",dat.temp$variable),]
}
if(TYPE=="MEDIAN"){
  dat.temp <- dat.temp[grep("med",dat.temp$variable),]
}

dat.temp$year <- as.character(substr(dat.temp$season.year,nchar(dat.temp$season.year)-3,nchar(dat.temp$season.year)))
dat.temp$year[substr(dat.temp$year,3,4) > 50] <- paste(19,substr(dat.temp$year[substr(dat.temp$year,3,4) > 50],3,4),sep="")
dat.temp$year[substr(dat.temp$year,3,4) < 50] <- paste(20,substr(dat.temp$year[substr(dat.temp$year,3,4) < 50],3,4),sep="")

dat.temp$year <- as.numeric(dat.temp$year)
dat.temp$season <- as.character(substr(dat.temp$variable,1,3)) 
dat.temp <- dat.temp %>% mutate(year=case_when(season =="Win" ~ year-1,
                                               season !="Win" ~ year )) ### This makes winter correspond to the year with december.

dat.temp <- merge(dat.temp,dat.area)
dat.temp <- merge(dat.temp,dat.area.deep)
dat.temp$frac.shallow <- dat.temp$Area_km2 / dat.temp$tot_area_shallow_km2
dat.temp$frac.deep    <- dat.temp$Area_km2 / dat.temp$tot_area_deep_km2

temp.sum.shallow  <- dat.temp %>% filter(Depth_zone == "0 to 200 m") %>% mutate(AW_temp = frac.shallow * value) %>%
                      group_by(Region_num, year, season, season.year) %>% summarise(AW_Mean = sum(AW_temp))
temp.sum.deep     <- dat.temp  %>% mutate(AW_temp = frac.deep * value) %>%
                      group_by(Region_num, year, season, season.year) %>% summarise(AW_Mean = sum(AW_temp))

### Get rid of Winter 1981 data.
temp.sum.shallow <- temp.sum.shallow %>% filter(year>=1982)
temp.sum.deep <- temp.sum.deep %>% filter(year>=1982)
#######################################################################################
#### Process PDO values to make guesses for the years 1979 to 1981.

pdo.dat$year  <- substr(as.character(pdo.dat$Date),1,4)
pdo.dat$month <- substr(as.character(pdo.dat$Date),5,6)
if(MONTH.STRUCTURE=="FOUR"){
  pdo.dat$season[pdo.dat$month == "01" | pdo.dat$month == "02" | pdo.dat$month == "03" | pdo.dat$month == "11" | pdo.dat$month == "12"  ] <- "Win"
  pdo.dat$season[pdo.dat$month == "04" | pdo.dat$month == "05"  ] <- "Spr"
  pdo.dat$season[pdo.dat$month == "06" | pdo.dat$month == "07"  ] <- "Sum"
  pdo.dat$season[pdo.dat$month == "08" | pdo.dat$month == "09" | pdo.dat$month == "10"] <- "Fal"
}
if(MONTH.STRUCTURE=="FRAM"){
  pdo.dat$season[pdo.dat$month == "01" | pdo.dat$month == "10" | pdo.dat$month == "11" | pdo.dat$month == "12"  ] <- "Win"
  pdo.dat$season[pdo.dat$month == "02" | pdo.dat$month == "03" |pdo.dat$month == "04" ] <- "Spr"
  pdo.dat$season[pdo.dat$month == "05" | pdo.dat$month == "06"] <- "Sum"
  pdo.dat$season[pdo.dat$month == "07"  | pdo.dat$month == "08" | pdo.dat$month == "09" ] <- "Fal"
}

pdo.dat$year <- as.numeric(pdo.dat$year)
pdo.dat$year.1 <- pdo.dat$year
if(MONTH.STRUCTURE=="FOUR"){
  pdo.dat$year.1[pdo.dat$month == "01" | pdo.dat$month == "02" | pdo.dat$month == "03"] <- pdo.dat$year.1[pdo.dat$month == "01" | pdo.dat$month == "02" | pdo.dat$month == "03"] - 1
}

if(MONTH.STRUCTURE=="FRAM"){
  pdo.dat$year.1[pdo.dat$month == "01" ] <- pdo.dat$year.1[pdo.dat$month == "01" ] - 1
}
pdo.dat$year.plot = pdo.dat$year + as.numeric(as.character(pdo.dat$month))/13

# plot(Value~year.plot,data=pdo.dat,type="l")
# plot(Value~year.plot,data=pdo.dat[pdo.dat$year.plot>YEARS.RECOVER & pdo.dat$year.plot<2010,],type="l")
# plot(Value~year.plot,data=pdo.dat[pdo.dat$year.plot>1979,],type="l")

pdo.season <- pdo.dat %>% mutate(year=year.1) %>% dplyr::select(-year.1) %>%
                  group_by(year,season) %>% summarise(PDO.mean = mean(Value)) %>% as.data.frame()

### REPEAT FOR NPGO

npgo.dat <- npgo.dat %>%  rename(year=YEAR,month=MONTH)

if(MONTH.STRUCTURE=="FOUR"){
  npgo.dat$season[npgo.dat$month == 1 | npgo.dat$month == 2 | npgo.dat$month == 3 | npgo.dat$month == 11 | npgo.dat$month == 12  ] <- "Win"
  npgo.dat$season[npgo.dat$month == 4 | npgo.dat$month == 5  ] <- "Spr"
  npgo.dat$season[npgo.dat$month == 6 | npgo.dat$month == 7  ] <- "Sum"
  npgo.dat$season[npgo.dat$month == 8 | npgo.dat$month == 9 | npgo.dat$month == 10] <- "Fal"
}
if(MONTH.STRUCTURE=="FRAM"){
  npgo.dat$season[npgo.dat$month == "01" | npgo.dat$month == "10" | npgo.dat$month == "11" | npgo.dat$month == "12"  ] <- "Win"
  npgo.dat$season[npgo.dat$month == "02" | npgo.dat$month == "03" |npgo.dat$month == "04" ] <- "Spr"
  npgo.dat$season[npgo.dat$month == "05" | npgo.dat$month == "06"] <- "Sum"
  npgo.dat$season[npgo.dat$month == "07"  | npgo.dat$month == "08" | npgo.dat$month == "09" ] <- "Fal"
}

npgo.dat$year.1 <- npgo.dat$year
if(MONTH.STRUCTURE=="FOUR"){
  npgo.dat$year.1[npgo.dat$month == "01" | npgo.dat$month == "02" | npgo.dat$month == "03"] <- npgo.dat$year.1[npgo.dat$month == "01" | npgo.dat$month == "02" | npgo.dat$month == "03"] - 1
}

if(MONTH.STRUCTURE=="FRAM"){
  npgo.dat$year.1[npgo.dat$month == "01" ] <- npgo.dat$year.1[npgo.dat$month == "01" ] - 1
}

#npgo.dat$year.1[npgo.dat$MONTH == "01"] <- npgo.dat$year.1[npgo.dat$MONTH == "01"] - 1
npgo.dat$year.plot = npgo.dat$year + npgo.dat$month/13

plot(NPGO.index~year.plot,data=npgo.dat,type="l")
plot(NPGO.index~year.plot,data=npgo.dat[npgo.dat$year.plot>1979 & npgo.dat$year.plot<2010,],type="l")
plot(NPGO.index~year.plot,data=npgo.dat[npgo.dat$year.plot>1979,],type="l")

npgo.season <- npgo.dat %>% mutate(year=year.1) %>% dplyr::select(-year.1) %>%
  group_by(year,season) %>% summarise(NPGO.mean = mean(NPGO.index)) %>% as.data.frame()

# merge PDO and NPGO
ocean.cov <- merge(pdo.season,npgo.season)

# Merge together PDO, NPGO and temperature data
temp.sum.shallow <- merge(temp.sum.shallow, ocean.cov)
temp.sum.deep    <- merge(temp.sum.deep, ocean.cov)

# Make a matrix of estimated coefficients for predicting local temperature from the PDO - temp relationships
COEF.shal <- NULL
COEF.deep <- NULL

SEASON <- c("Win","Spr","Sum","Fal")
for(i in 1:max(temp.sum.shallow$Region_num)){
  print(i)
    for(j in 1:length(SEASON)){
      A <- lm(AW_Mean~PDO.mean+NPGO.mean, data=temp.sum.shallow[temp.sum.shallow$season ==SEASON[j] & temp.sum.shallow$Region_num == i,])
      COEF.shal <- rbind(COEF.shal,c(i, SEASON[j],A$coefficients))
      B <- lm(AW_Mean~PDO.mean+NPGO.mean, data=temp.sum.deep[temp.sum.deep$season ==SEASON[j] & temp.sum.deep$Region_num == i,])
      COEF.deep <- rbind(COEF.deep,c(i, SEASON[j],B$coefficients))
      #print(summary(A))
      
    }
}

colnames(COEF.shal) <- c("Region_num","season","Int","Slope.PDO","Slope.NPGO")
COEF.shal <- as.data.frame(COEF.shal)
COEF.shal$Int <- as.numeric(as.character(COEF.shal$Int)) ; 
COEF.shal$Slope.PDO <- as.numeric(as.character(COEF.shal$Slope.PDO))
COEF.shal$Slope.NPGO <- as.numeric(as.character(COEF.shal$Slope.NPGO))

colnames(COEF.deep) <- colnames(COEF.shal)
COEF.deep <- as.data.frame(COEF.deep)
COEF.deep$Int <- as.numeric(as.character(COEF.deep$Int)) 
COEF.deep$Slope.PDO <- as.numeric(as.character(COEF.deep$Slope.PDO))
COEF.deep$Slope.NPGO <- as.numeric(as.character(COEF.deep$Slope.NPGO))

#Plot cross correlations between seasonal PDO and area weighted mean shallow temperatures.
P <- ggplot(data=temp.sum.shallow) +
        geom_point(aes(PDO.mean,AW_Mean,col=season)) + 
        facet_wrap(~Region_num)
print(P)

P <- ggplot(data=temp.sum.deep) +
  geom_point(aes(PDO.mean,AW_Mean,col=season)) + 
  facet_wrap(~Region_num)
print(P)


G <- ggplot(data=temp.sum.shallow) +
  geom_point(aes(NPGO.mean,AW_Mean,col=season)) + 
  facet_wrap(~Region_num)
print(P)

G <- ggplot(data=temp.sum.deep) +
  geom_point(aes(NPGO.mean,AW_Mean,col=season)) + 
  facet_wrap(~Region_num)
print(G)



# Make predictions for temperature for years 1979 - 1981 using the estimated relationships
cov.pred.shal <- NULL
cov.pred.deep <- NULL
THESE <- YEARS.RECOVER[YEARS.RECOVER <=1981]
for(i in 1:max(temp.sum.shallow$Region_num)){
  cov.temp <- ocean.cov %>% filter(year %in% THESE )
  cov.temp$Region_num <- i 
  cov.temp.shal <- merge(cov.temp,COEF.shal)
  cov.temp.deep <- merge(cov.temp,COEF.deep)

  cov.pred.shal <- rbind(cov.pred.shal,cov.temp.shal)
  cov.pred.deep <- rbind(cov.pred.deep,cov.temp.deep)
}

  cov.pred.shal$Pred <- cov.pred.shal$Int + cov.pred.shal$PDO.mean * cov.pred.shal$Slope.PDO + cov.pred.shal$NPGO.mean * cov.pred.shal$Slope.NPGO
  cov.pred.deep$Pred <- cov.pred.deep$Int + cov.pred.deep$PDO.mean * cov.pred.deep$Slope.PDO + cov.pred.deep$NPGO.mean * cov.pred.deep$Slope.NPGO


  #######################################################################################
  #######################################################################################
  #######################################################################################
  # MERGE TOGETHER PDO pred and observations.
  temp.sum.shallow <- merge(temp.sum.shallow,cov.pred.shal %>% dplyr::select(season,Region_num,year,Pred),all=T)
  temp.sum.shallow$AW_Mean[is.na(temp.sum.shallow$AW_Mean)] <- 0
  temp.sum.shallow$Pred[is.na(temp.sum.shallow$Pred)] <- 0
  temp.sum.shallow$Mean <- temp.sum.shallow$AW_Mean + temp.sum.shallow$Pred
  
  temp.sum.shallow <- merge(temp.sum.shallow,locations,by.x="Region_num",by.y="location.number")
  temp.sum.shallow$location.name  <-  factor(temp.sum.shallow$location.name, 
                                             levels = as.character(locations$location.name))
  temp.sum.shallow$season         <-  factor(temp.sum.shallow$season, 
                                             levels = c("Win","Spr","Sum","Fal"))
  
  temp.sum.shallow <- temp.sum.shallow[order(temp.sum.shallow$Region_num,temp.sum.shallow$year,temp.sum.shallow$season),]
  
  # Make wide format temperature time series for shallow
  temp.shallow.wide <-dcast(temp.sum.shallow, year+season~location.name,value.var = "Mean")
  
  ########### DEEP AREAS.
  temp.sum.deep <- merge(temp.sum.deep,cov.pred.deep %>% dplyr::select(season,Region_num,year,Pred),all=T)
  temp.sum.deep$AW_Mean[is.na(temp.sum.deep$AW_Mean)] <- 0
  temp.sum.deep$Pred[is.na(temp.sum.deep$Pred)] <- 0
  temp.sum.deep$Mean <- temp.sum.deep$AW_Mean + temp.sum.deep$Pred
  
  temp.sum.deep <- merge(temp.sum.deep,locations,by.x="Region_num",by.y="location.number")
  temp.sum.deep$location.name  <-  factor(temp.sum.deep$location.name, 
                                          levels = as.character(locations$location.name))
  temp.sum.deep$season         <-  factor(temp.sum.deep$season, 
                                          levels = c("Win","Spr","Sum","Fal"))
  
  temp.sum.deep <- temp.sum.deep[order(temp.sum.deep$Region_num,temp.sum.deep$year,temp.sum.deep$season),]
  
  # Make wide format temperature time series for deep
  temp.deep.wide <-dcast(temp.sum.deep, year+season~location.name,value.var = "Mean")
  
  
  ########################################################
  ########################################################
  ########################################################
  ########################################################
  ## -------- DATA FOR St. of Georgia, Puget Sound
  ### From Lighthouses, some buoy data.
  ########################################################
  ########################################################
  ########################################################
  ########################################################
  # Go get the Canadian Lighthouse data.
  C.dat    <- read.csv(paste(base.dir,"/Salmon-Climate/Coastwide SST/Canadian lighthouses/Chrome_Island_-_Daily_Sea_Surface_Temperature_and_Salinity_1961-2019.csv",sep=""))
  D.dat    <- read.csv(paste(base.dir,"/Salmon-Climate/Coastwide SST/Canadian lighthouses/Departure_Bay_PBS_-_Daily_Sea_Surface_Temperature_and_Salinity_1914-2019.csv",sep=""))
  E.dat    <- read.csv(paste(base.dir,"/Salmon-Climate/Coastwide SST/Canadian lighthouses/Entrance_Island_-_Daily_Sea_Surface_Temperature_and_Salinity_1936-2019.csv",sep=""))
  R.dat    <- read.csv(paste(base.dir,"/Salmon-Climate/Coastwide SST/Canadian lighthouses/Race_Rocks_-_Daily_Sea_Surface_Temperature_and_Salinity_1921-2019.csv",sep=""))
  
  C.dat <- C.dat %>% dplyr::select(DATE="DATE..YYYY.MM.DD.",temp.C = "TEMPERATURE...C..") %>% mutate(site="Chrome Island")
  D.dat <- D.dat %>% dplyr::select(DATE="DATE..YYYY.MM.DD.",temp.C = "TEMPERATURE...C..") %>% mutate(site="Departure Bay")
  E.dat <- E.dat %>% dplyr::select(DATE="DATE..YYYY.MM.DD.",temp.C = "TEMPERATURE...C..") %>% mutate(site="Entrance Island")
  R.dat <- R.dat %>% dplyr::select(DATE="DATE..YYYY.MM.DD.",temp.C = "TEMPERATURE...C..") %>% mutate(site="Race Rocks")
  
  light.dat <- bind_rows(C.dat,D.dat,E.dat,R.dat) %>% as.data.frame()
  light.dat <- light.dat %>% mutate(yr=as.numeric(substr(DATE,nchar(DATE)-1,nchar(DATE))),
                                    m = substr(DATE,1,2)) %>%
                             mutate(month=case_when(m== "1/"~1,
                                                m== "2/"~2,
                                                m== "3/"~3,
                                                m== "4/"~4,
                                                m== "5/"~5,
                                                m== "6/"~6,
                                                m== "7/"~7,
                                                m== "8/"~8,
                                                m== "9/"~9,
                                                m== "10"~10,
                                                m== "11"~11,
                                                m== "12"~12,
                                                TRUE~as.numeric(m))) %>%
                            mutate(year = ifelse(yr<=20,yr+2000,yr+1900))
  
  if(MONTH.STRUCTURE=="FOUR"){
    light.summary <- light.dat %>% mutate(season= case_when(month <= 3 | month>=11 ~ "Win",
                                                                month <= 5 & month>=4  ~ "Spr",
                                                                month <= 7 & month>=6  ~ "Sum",
                                                                month <= 10 & month>=8 ~ "Fal"))
  }
  if(MONTH.STRUCTURE=="FRAM"){
    light.summary <- light.dat %>% mutate(season= case_when(month <= 1 | month>=10 ~ "Win",
                                                                month <= 4 & month>=2  ~ "Spr",
                                                                month <= 6 & month>=5  ~ "Sum",
                                                                month <= 7 & month>=9 ~ "Fal"))
  }
  
  
  light.summary <-  light.summary %>% dplyr::select(year,month,season,temp.C,site) %>% 
      filter(temp.C != 99.9,year >= min(YEARS.RECOVER)) %>% 
      group_by(site,year,month,season) %>% 
      summarise(mean.C = mean(temp.C),sd.C=sd(temp.C),N=length(temp.C)) %>%
      mutate(year.plot = year + (month-1)/12)
  light.summary <- light.summary %>% mutate(year.mod = year,year.mod =ifelse(season=="Win" & month<6,year.mod-1,year.mod)) %>%
          group_by(year.mod,season,site) %>% summarise(Mean=mean(mean.C))
  
  ggplot(light.summary) +
    geom_line(aes(x=year.mod,y=Mean,group=site,color=site)) +
    facet_wrap(~season)
  
  JDF.SST <- light.summary %>% filter(site== "Race Rocks") %>% dplyr::select(-site) %>% rename(mean.C=Mean) %>% mutate(sd.C=0,region="JDF")
  
  SGEO.SST <- light.summary %>% filter(!site== "Race Rocks") %>% group_by(year.mod,season) %>% 
                      summarise(mean.C=mean(Mean),sd.C=sd(Mean)) %>%
                      mutate(region="SGEO")
  
  ########################################################
  ########################################################
  ########################################################
  ########################################################
  ## -------- DATA FOR Puget Sound.
  ### From Marine Water Monitoring program (1999 on)
  ### and from Blake ICOADS
  ########################################################
  ########################################################
  ########################################################
  ########################################################

  # GET MWM data: 
  NOMS <- dir(paste0(base.dir,"/Salmon-Climate/Coastwide SST/Puget Sound MWM/Focal Sites"))

  MWM.dat <- NULL
  for(i in 1:length(NOMS)){
    raw.MWM.dat    <- read.csv(paste0(base.dir,"/Salmon-Climate/Coastwide SST/Puget Sound MWM/Focal Sites/",NOMS[i]))
    # summarize MWM data  
    MWM.temp <- raw.MWM.dat %>% dplyr::select(site=Location_ID,Field_Collection_Date,Depth_Value,Temp.C=Result_Value,
                                           Lat=contains("Latitude"),Lon=contains("Longitude"))
    MWM.temp$site <- as.character(MWM.temp$site)
    MWM.dat <- bind_rows(MWM.dat,MWM.temp)
  }
  
  MWM.dat$Date = as.Date(MWM.dat$Field_Collection_Date,"%m/%d/%Y")
  MWM.dat$month = month(MWM.dat$Date)
  MWM.dat$year = year(MWM.dat$Date)
  
  loc.MWM <- MWM.dat %>% group_by(site) %>% summarise(LAT=mean(Lat),LON=mean(Lon))  

  # MWM data ##########################################################333
  # Make a plot of where these sites are...
  library(maps)
  states <- map_data("state")
  WA <- subset(states, region %in% c("washington"))
  WA.base <- ggplot(data = WA) +
    geom_polygon(aes(x=long, y = lat,  group = group)) +
    coord_fixed(1.3,xlim=c(-123.2,-122),ylim=c(47,49))
  
  WA.base + 
    geom_point(data=loc.MWM, aes(x=LON,y=LAT),color="red") +
    geom_text(data=loc.MWM, aes(x=LON,y=LAT,label=site),color="red",nudge_x=0.15)

  # subset the sites.  Pick these bases on Steph Moore's paper ()
  # Moore et al. 2008. Local and large-scale climate forcing of Puget Sound oceanographic properties on seasonal to interdecadal timescales
  # Limnol. Oceanogr., 53(5), 2008, 1746–1758
  
  SITES <- c("ADM001","ADM003","PSS019","CMB003")
  # THESE FOUR SITES ARE AVAILABLE FOR THE ENTIRE SERIES AND ARE IN MAJOR PARTS OF THE CENTRAL SOUND OR NEARBY
  
  MWM.trim <- MWM.dat %>% filter(site %in% SITES)
  loc.trim.MWM <- MWM.trim %>% group_by(site) %>% summarise(LAT=mean(Lat),LON=mean(Lon))  
  
  WA.base + 
    geom_point(data=loc.trim.MWM, aes(x=LON,y=LAT),color="red") +
    geom_text(data=loc.trim.MWM, aes(x=LON,y=LAT,label=site),color="red",nudge_x=0.15)
  
  MWM.summ <- MWM.trim %>% filter(Depth_Value <= 5) %>% group_by(site,month,year) %>% 
                summarise(N=length(Temp.C),Mean = mean(Temp.C)) %>%
                mutate(year.plot = year+ (month-1)/12)
  
  if(MONTH.STRUCTURE=="FOUR"){
      MWM.summ <- MWM.summ %>% mutate(season= case_when(month <= 3 | month>=11 ~ "Win",
                                            month <= 5 & month>=4  ~ "Spr",
                                            month <= 7 & month>=6  ~ "Sum",
                                            month <= 10 & month>=8 ~ "Fal"))
    }
  if(MONTH.STRUCTURE=="FRAM"){
      MWM.summ <- MWM.summ %>% mutate(season= case_when(month <= 1 | month>=10 ~ "Win",
                                                            month <= 4 & month>=2  ~ "Spr",
                                                            month <= 6 & month>=5  ~ "Sum",
                                                            month <= 7 & month>=9 ~ "Fal"))
  }
                    
  # adjust year 
  MWM.summ <- MWM.summ %>% mutate(year.mod = year,year.mod =ifelse(season=="Win" & month<6,year.mod-1,year.mod)) %>%
      group_by(year.mod,season,site) %>% summarise(MEAN=mean(Mean)) %>% rename(Mean=MEAN)
  
  
  ggplot(MWM.summ) +
    geom_point(aes(y=Mean,x=year.mod,color=site))+
    geom_line(aes(y=Mean,x=year.mod,color=site,group=site)) +
    facet_wrap(~season)
  
  PUSO.MWM <- MWM.summ %>% group_by(year.mod,season) %>% summarise(mean.C=mean(Mean),sd.C=sd(Mean)) %>% mutate(region="PUSO")
  
  ggplot(PUSO.MWM) +
    geom_point(aes(y=mean.C,x=year.mod,))+
    geom_line(aes(y=mean.C,x=year.mod,group=season)) +
    facet_wrap(~season)

  #########################################################################################################
  #########################################################################################################
  #########################################################################################################
  #########################################################################################################
  ###  Look at the King County data from  Steph Moore.
  #########################################################################################################
  #########################################################################################################
  #########################################################################################################
  #########################################################################################################
  
  kc.dat <- read.csv(paste0(base.dir,"/Salmon-Climate/Coastwide SST/King County WA/SM old temp data.csv")) 
  kc.loc <- read.csv(paste0(base.dir,"/Salmon-Climate/Coastwide SST/King County WA/SM old temp data coords.csv")) 
  
  kc.dat$DATE  <- as.Date(kc.dat$DATE)
  kc.dat$year  <- year(kc.dat$DATE)
  kc.dat$month <- month(kc.dat$DATE)
  kc.dat$day   <- day(kc.dat$DATE)

  kc.mod.start <- kc.dat %>% rename(area=SITE,loc =LOCATOR, C = Sample.Temperature..Field) 
  kc.mod <- kc.mod.start %>% 
              group_by(area,loc,year,month,day) %>% summarise(C.val = mean(C)) %>%
              group_by(area,loc,year,month) %>% summarise(N.obs =length(C.val),C = mean(C.val)) %>%
              mutate(year.plot = year + (month-1)/12)

  ## OK.  We will use these areas because all of the observations occur in the central sound off of Elliot bay
  # trim between latitudes 47.75 and 47.50.
  # These codes include sites that are observed for the vast majority of the time series that matter (post-1980, mostly continuous)
  THESE <- c("ALKI PT OFFSHORE","AMBIENT OFFSHORE","CARK PK OFFSHORE","RENTON OFFSHORE","WEST PT OFFSHORE")
  
  kc.loc.mod <- kc.loc %>% filter(SITE %in% THESE,LATITUDE >= 47.50, LATITUDE <= 47.75) 
  kc.trim <- kc.mod.start%>% filter(loc %in% kc.loc.mod$SAMPLOCATO) %>%
                  group_by(area,loc,year,month,day) %>% summarise(C.val = mean(C)) %>%
                  group_by(year,month) %>% summarise(N.obs =length(C.val),C = mean(C.val)) %>%
                  mutate(year.plot = year + (month-1)/12)
  
  if(MONTH.STRUCTURE=="FOUR"){
    kc.trim <- kc.trim %>% mutate(season= case_when(month <= 3 | month>=11 ~ "Win",
                                                      month <= 5 & month>=4  ~ "Spr",
                                                      month <= 7 & month>=6  ~ "Sum",
                                                      month <= 10 & month>=8 ~ "Fal"))
  }
  if(MONTH.STRUCTURE=="FRAM"){
    MWM.summ <- MWM.summ %>% mutate(season= case_when(month <= 1 | month>=10 ~ "Win",
                                                      month <= 4 & month>=2  ~ "Spr",
                                                      month <= 6 & month>=5  ~ "Sum",
                                                      month <= 7 & month>=9 ~ "Fal"))
  }
  
  # adjust year0
  kc.by.season <- kc.trim%>% 
                    mutate(year.mod = year,year.mod =ifelse(season=="Win" & month<6,year.mod-1,year.mod)) %>%
                    group_by(year.mod,season) %>% summarise(SST=mean(C))
  
  #### THIS IS THE WINNER!
  PUSO.SST <- kc.by.season  %>% mutate(region="PUSO") 
    
  ############# SOME PLOTS OF DATA AND LOCATIONS  
  ggplot(kc.mod) + 
      geom_point(aes(x=year.plot,y=N.obs),alpha=0.5) +
      facet_wrap(~area)
  
  ggplot(kc.mod) + 
    geom_point(aes(x=year.plot,y=C),alpha=0.5) +
    facet_wrap(~area)
  
  ggplot(kc.trim) + 
    geom_point(aes(x=year.plot,y=C),alpha=0.5) +
    geom_line(aes(x=year.plot,y=C),alpha=0.5) 
    
  ggplot(kc.by.season) + 
    geom_point(aes(x=year.mod,y=SST),alpha=0.5) +
    geom_line(aes(x=year.mod,y=SST),alpha=0.5) +
    facet_wrap(~season)
  

  # WA.base + 
  #   coord_fixed(1.3,xlim=c(-122.75,-122),ylim=c(47.2,48.2))+
  #   geom_point(data=kc.loc %>% filter(SITE=="AMBIENT OFFSHORE"), aes(x=LONGITUDE,y=LATITUDE),color="red") +
  #   geom_point(data=kc.loc %>% filter(SITE=="CARK PK OFFSHORE"), aes(x=LONGITUDE,y=LATITUDE),color="blue") +
  #   geom_point(data=kc.loc %>% filter(SITE=="RENTON OFFSHORE"), aes(x=LONGITUDE,y=LATITUDE),color="green") +
  #   geom_point(data=kc.loc %>% filter(SITE=="ALKI PT OFFSHORE"), aes(x=LONGITUDE,y=LATITUDE),color="yellow") +
  #   geom_point(data=kc.loc %>% filter(SITE=="WEST PT OFFSHORE"), aes(x=LONGITUDE,y=LATITUDE),color="orange") 
  # 
  # WA.base + 
  #   coord_fixed(1.3,xlim=c(-122.75,-122),ylim=c(47.2,48.2))+
  #   geom_point(data=kc.loc.mod, aes(x=LONGITUDE,y=LATITUDE),color="red") 
  #   #geom_text(data=kc.loc, aes(x=LONGITUDE,y=LATITUDE,label=SAMPLOCATO),color="red",nudge_x=0.15)
  
  #########################################################################################################
  #########################################################################################################
  #########################################################################################################
  #########################################################################################################
  ###  Get into the Ship data
  #########################################################################################################
  #########################################################################################################
  #########################################################################################################
  #########################################################################################################
  
  # Get Ship data
  ship.dat <- read.csv(paste0(base.dir,"/Salmon-Climate/Coastwide SST/Salish Ships data ICOADS/ICOADS IMMA SST data 1975to2017.csv"))
  
  # Make some maps
  canada <- map_data("world", "Canada")
  states <- map_data("state")
  WA.BC <- subset(states, region %in% c("washington")) %>% bind_rows(.,canada)
  
  
  # WA.BC.base <- ggplot(data = WA.BC) +
  #   geom_polygon(aes(x=long, y = lat,  group = group)) +
  #   coord_fixed(1.3,xlim=c(-126,-122),ylim=c(47,51)) +
  #   theme_bw()
  # 
  # all.obs  <-  WA.BC.base + 
  #   geom_point(data=ship.dat %>% filter(REG_NAME=="SGEO"), aes(x=LON,y=LAT),color="blue",alpha=0.1) +
  #   geom_point(data=ship.dat %>% filter(REG_NAME=="PUSO"), aes(x=LON,y=LAT),color="red",alpha=0.1)+
  #   geom_point(data=ship.dat %>% filter(REG_NAME=="JDF"), aes(x=LON,y=LAT),color="green",alpha=0.1)
  #   
  
  # Summarize all observartions by region, day
  ship.dat.trim <- ship.dat %>% rename(year=YEAR,month=MO,day=DAY,season.composite=Season.composite,region=REG_NAME,SST.raw=SST..C.) %>%
    group_by(region,year,month,day,season.composite) %>% summarise(N=length(region),SST.d=mean(SST.raw)) %>%
    group_by(region,year,month,season.composite) %>% summarise(N.day=length(N),SST.m=mean(SST.d)) %>%
    mutate(year.plot = year + (month-1)/12)
  
  
  if(MONTH.STRUCTURE=="FOUR"){
    ship.dat.trim <- ship.dat.trim %>% mutate(season= case_when(month <= 3 | month>=11 ~ "Win",
                                                      month <= 5 & month>=4  ~ "Spr",
                                                      month <= 7 & month>=6  ~ "Sum",
                                                      month <= 10 & month>=8 ~ "Fal"))
  }
  if(MONTH.STRUCTURE=="FRAM"){
    ship.dat.trim <- ship.dat.trim %>% mutate(season= case_when(month <= 1 | month>=10 ~ "Win",
                                                      month <= 4 & month>=2  ~ "Spr",
                                                      month <= 6 & month>=5  ~ "Sum",
                                                      month <= 7 & month>=9 ~ "Fal"))
  }
  
    ship.dat.trim <- ship.dat.trim %>% mutate(year.mod = year,year.mod =ifelse(season=="Win" & month<6,year.mod-1,year.mod))
    
    ### Merge in a list of site x seasons to add zeros
    
    ship.dat.season <- ship.dat.trim %>% group_by(region,year.mod,season) %>% summarise(SST=mean(SST.m),N.month=length(N.day)) %>%
                          full_join(.,expand.grid(region=c("JDF","PUSO","SGEO"),
                                        season=c("Win","Spr","Sum","Fal"),
                                        year.mod=(min(ship.dat$YEAR) -1):max(ship.dat$YEAR))) %>%
                          mutate(year.plot=case_when(season=="Win"~year.mod +0.75,
                                   season=="Spr"~year.mod,
                                   season=="Sum"~year.mod+0.25,
                                   season=="Fal"~year.mod+0.5)) %>%
                          mutate(N.month=ifelse(is.na(N.month),0,N.month))
  
  # Data availability  
  ggplot(ship.dat.season) + 
    geom_line(aes(x=year.plot,y=N.month,group=region)) +
    facet_wrap(~region+season)
  
  ggplot(ship.dat.season) + 
    geom_line(aes(x=year.plot,y=SST,group=region)) +
    facet_wrap(~region+season)
  
  ### Pairwise correlations
  ship.dat.season.wide  <- ship.dat.season %>% 
                            pivot_wider(id_cols=c("year.mod","season"),names_from=c("region"),values_from = c("SST"))

  ship.p <- list()  
  ship.p[["Spr"]] <- ggpairs(ship.dat.season.wide %>%filter(season=="Spr"),columns=c("PUSO","JDF","SGEO"))
  ship.p[["Sum"]] <- ggpairs(ship.dat.season.wide %>%filter(season=="Sum"),columns=c("PUSO","JDF","SGEO"))
  ship.p[["Fal"]] <- ggpairs(ship.dat.season.wide %>%filter(season=="Fal"),columns=c("PUSO","JDF","SGEO"))
  ship.p[["Win"]] <- ggpairs(ship.dat.season.wide %>%filter(season=="Win"),columns=c("PUSO","JDF","SGEO"))
    
  #######################################################
  #######################################################
  #######################################################
  #######################################################
  #######################################################
  #######################################################
  
  #######################################################
  #######################################################
  #######################################################
  #######################################################
  #######################################################
  #######################################################
  #######################################################
  #######################################################

  # Combine  ship data and fixed station data
   #After a lot of dinking around with this data, I think using the the lighthouse data for 
      #SGEO - the three lighthouses up along the east coast of Van Is. 
      #JDF  - Race Rocks lighthouse
  # These are easy calculations because they are just summaries of observations and don't involve any interpolations
  
  salish.sst <- bind_rows(SGEO.SST %>% dplyr::select(year.mod,season,SST=mean.C,region),
                          JDF.SST  %>% dplyr::select(year.mod,season,SST=mean.C,region),
                          PUSO.SST) %>% 
                filter(year.mod>=min(YEARS.RECOVER))
  salish.sst.mean.by.season <- salish.sst %>% group_by(region,season) %>% summarise(grand.mean=mean(SST))
  
  salish.sst.wide <- left_join(salish.sst,salish.sst.mean.by.season) %>% mutate(SST.cent=SST - grand.mean) %>%
                pivot_wider(id_cols=c("year.mod","season"),names_from=c("region"),values_from = c("SST.cent"))
  
  ggpairs(salish.sst.wide %>%filter(season=="Win"),columns=c("PUSO","JDF","SGEO"))
  ggpairs(salish.sst.wide %>%filter(season=="Spr"),columns=c("PUSO","JDF","SGEO"))
  ggpairs(salish.sst.wide %>%filter(season=="Sum"),columns=c("PUSO","JDF","SGEO"))
  ggpairs(salish.sst.wide %>%filter(season=="Fal"),columns=c("PUSO","JDF","SGEO"))
  
  
  
  
  # EXTRA CODE
  
  # salish.join.sst <- full_join(salish.sst,ship.dat.season) %>% rename(SST.station=mean.C,SST.ship=SST) %>%
  #   dplyr::select(-N.month,-year.plot)
  # 
  # ggplot(salish.join.sst) +
  #   geom_point(aes(x=SST.station,y=SST.ship),alpha=0.5) +
  #   geom_abline(intercept=0,slope=1,linetype="dashed") +
  #   facet_wrap(region~season,scales="free")
  # 
  # ggplot(salish.join.sst) +
  #   geom_line(aes(x=year.mod,y=SST.station),color="red") +
  #   geom_line(aes(x=year.mod,y=SST.ship),color="blue") +
  #   facet_wrap(region~season)+
  #   theme_bw()
  # 
  # 
  # ggpairs(salish.join.sst,columns)
  # 

  
#######################################################################################
  #######################################################################################
  #######################################################################################
  #######################################################################################
  # MERGE TOGETHER the predictions from the OISST data with the in situ observations for the Salish sea.  
  # Replace the old OISST-derived data with Salish Data.
  salish.sst <- salish.sst %>% rename(year=year.mod,Mean=SST,location.name=region) %>% 
                    mutate(location.name = ifelse(location.name=="JDF","PUSO_out",location.name))
  
  temp.sum.shallow <- temp.sum.shallow %>% filter(!location.name %in% c("PUSO","SGEO")) %>% dplyr::select(year,season,Mean,location.name) %>%
                            bind_rows(., salish.sst) %>%
                            filter(year>=min(YEARS.RECOVER),year<=max(YEARS.RECOVER))
                                   
  temp.shallow.wide <-dcast(temp.sum.deep, year+season~location.name,value.var = "Mean")
  
  temp.sum.shallow$location.name <- factor(temp.sum.shallow$location.name,levels=LOCATIONS$location.name)
  ########### DEEP AREAS.
  temp.sum.deep <- temp.sum.deep %>% filter(!location.name %in% c("PUSO","SGEO")) %>% dplyr::select(year,season,Mean,location.name) %>%
                        bind_rows(., salish.sst)  %>%
                        filter(year>=min(YEARS.RECOVER),year<=max(YEARS.RECOVER))
  temp.deep.wide <-dcast(temp.sum.deep, year+season~location.name,value.var = "Mean")
  temp.sum.deep$location.name <- factor(temp.sum.deep$location.name,levels=LOCATIONS$location.name)
  
  # Plot time series 
  YLIM=c(4,18)
  ########################################################
  ########################################################
  all.ts.shallow <-  ggplot(temp.sum.shallow %>% filter(year>=1982)) + 
          geom_line(aes(y=Mean,x=year,color=location.name)) +
          theme_bw() +
          #coord_cartesian(ylim=YLIM)+
          facet_wrap(~season,ncol=2)
all.ts.shallow


YLIM=c(8,18)
all.ts.shallow.summer <-  ggplot(temp.sum.shallow %>% 
                                   filter(season=="Sum", 
                                          #Region_num>4,
                                          #Region_num<15,
                                          #Region_num!=9,Region_num!=10,Region_num!=11,
                                          year>=1981) ) + 
  geom_line(aes(y=Mean,x=year,color=location.name)) +
  scale_color_discrete(name="Region") +
  ylab("Summer SST (mean, C)") +
  xlab("Year") +
  theme_bw() #+
  #coord_cartesian(ylim=YLIM)+
  #facet_wrap(~season,ncol=2)
all.ts.shallow.summer

all.ts.shallow.fall <-  ggplot(temp.sum.shallow %>% 
                                   filter(season=="Fal", 
                                          #Region_num>4,
                                          #Region_num<15,
                                          #Region_num!=9,Region_num!=10,Region_num!=11,
                                          year>=1981) ) + 
  geom_line(aes(y=Mean,x=year,color=location.name)) +
  scale_color_discrete(name="Region") +
  ylab("Fall SST (mean, C)") +
  xlab("Year") +
  theme_bw() #+
#coord_cartesian(ylim=YLIM)+
#facet_wrap(~season,ncol=2)
all.ts.shallow.fall

pdf(file=paste(base.dir,"/Salmon-Climate/Coastwide SST/_Plots/Temperature plots summer mean -",TYPE,".pdf",sep=""),onefile=T,width=6,height=4)
  print(all.ts.shallow.summer)
dev.off()
pdf(file=paste(base.dir,"/Salmon-Climate/Coastwide SST/_Plots/Temperature plots fall mean -",TYPE,".pdf",sep=""),onefile=T,width=6,height=4)
  print(all.ts.shallow.fall)
dev.off()

all.ts.deep <-  ggplot(temp.sum.deep) +
          geom_line(aes(y=Mean,x=year,color=location.name)) +
          theme_bw() +
  #coord_cartesian(ylim=YLIM)+
          facet_wrap(~season,ncol=2)
all.ts.deep

#######################################################################################
# Mean and temporal SD by site and season
#########################################################################################
time.avg.shallow <- temp.sum.shallow %>% filter(year>=1982) %>% group_by(location.name,season) %>%
                        summarise(MEAN=mean(Mean),SD=sd(Mean))

time.avg.deep    <- temp.sum.deep %>% filter(year>=1982) %>% group_by(location.name,season) %>%
                        summarise(MEAN=mean(Mean),SD=sd(Mean))

# calculate for the time period matching the future projections (1982-2005)
time.avg.shallow.trim <-  temp.sum.shallow %>% filter(year>=1982,year<=2005) %>% group_by(location.name,season) %>%
                                summarise(MEAN.trim=mean(Mean),SD.trim=sd(Mean))

time.avg.deep.trim    <- temp.sum.deep %>% filter(year>=1982,year<=2005) %>% group_by(location.name,season) %>%
                          summarise(MEAN.trim=mean(Mean),SD.trim=sd(Mean))


time.avg.shallow <- left_join(time.avg.shallow,time.avg.shallow.trim)
time.avg.deep    <- left_join(time.avg.deep,time.avg.deep.trim)


time.avg.plot.shallow <- ggplot(time.avg.shallow) +
                  geom_point(aes(x=location.name,y=MEAN)) +
                  geom_errorbar(aes(x=location.name,ymin=MEAN-SD, ymax=MEAN+SD), width=.1) +
                  theme_bw() +
                  facet_wrap(~season,ncol=2) +
                  theme(axis.text.x = element_text(angle = 90, hjust = 1))
time.avg.plot.shallow

time.avg.plot.deep <- ggplot(time.avg.deep) +
                  geom_point(aes(x=location.name,y=MEAN)) +
                  geom_errorbar(aes(x=location.name,ymin=MEAN-SD, ymax=MEAN+SD), width=.1) +
                  theme_bw() +
                  facet_wrap(~season,ncol=2) +
                  theme(axis.text.x = element_text(angle = 90, hjust = 1))
time.avg.plot.deep
# 

#########################################################################################
##### CENTER AND EXAMINE TEMPERATURES AS DEVIATIONS FROM MEAN
#########################################################################################
temp.dev.shallow <- time.avg.shallow %>% 
                      dplyr::select(-SD,-SD.trim) %>%
                      rename(ts.mean = MEAN,ts.mean.trim=MEAN.trim) %>%
                      mutate(diff.temp = ts.mean.trim - ts.mean)
              
temp.dev.deep    <- time.avg.deep %>%
                      dplyr::select(-SD,-SD.trim) %>%
                      rename(ts.mean = MEAN,ts.mean.trim=MEAN.trim) %>%
                      mutate(diff.temp = ts.mean.trim - ts.mean)

  # For making file of deviations with deviations from winter being relative from the mean of Spring.
  temp.dev.3seas <- temp.dev.deep
  loc.nom <- unique(temp.dev.3seas$location.name)
  for( i in 1:length(loc.nom)){
    temp.dev.3seas$ts.mean[temp.dev.3seas$location.name == loc.nom[i] & temp.dev.3seas$season == "Win"] <- 
      temp.dev.3seas$ts.mean[temp.dev.3seas$location.name == loc.nom[i] & temp.dev.3seas$season == "Spr"] 
  }
  temp.dev.3seas <- merge(temp.sum.deep,temp.dev.3seas)
  temp.dev.3seas$dev <- temp.dev.3seas$Mean - temp.dev.3seas$ts.mean 
  ###
  temp.dev.shallow <- merge(temp.sum.shallow,temp.dev.shallow)
  temp.dev.deep    <- merge(temp.sum.deep,temp.dev.deep)

  temp.dev.shallow$dev <- temp.dev.shallow$Mean - temp.dev.shallow$ts.mean 
  temp.dev.deep$dev <- temp.dev.deep$Mean - temp.dev.deep$ts.mean 

dev.shallow <- ggplot(temp.dev.shallow) +
  geom_point(aes(x=year,y=dev,color=location.name)) +
  geom_line (aes(x=year,y=dev,color=location.name)) +
  facet_wrap(~season,ncol=2) +
  theme_bw() +
  geom_hline(yintercept = 0,linetype=2)
dev.shallow
  
dev.deep <- ggplot(temp.dev.deep) +
  geom_point(aes(x=year,y=dev,color=location.name)) +
  geom_line (aes(x=year,y=dev,color=location.name)) +
  facet_wrap(~season,ncol=2) +
  theme_bw() +
  geom_hline(yintercept = 0,linetype=2)
dev.deep

#######################################################################################
#######################################################################################
### Cross Correlations for deviations 
#######################################################################################
#######################################################################################

CCOR.shallow <-dcast(temp.dev.shallow, year+season~location.name,value.var = "dev")
CCOR.deep    <-dcast(temp.dev.deep, year+season~location.name,value.var = "dev")

# There are a few missing datapoints in the temperature series for PUSO.
  # They occur on Fall 1987 and Summer 1985
  # We interpolate these data from the deviations of the previous season and the following season
  
      NANA <- apply(is.na(CCOR.shallow), 2, which)
      ALL <- NULL
      for(i in 1:length(NANA)){
          rows = NANA[[i]]
          if(length(rows)>0){
              cols =  rep(which(colnames(CCOR.shallow)==names(NANA)[i]),length(rows))
              new <- data.frame(location.name=names(NANA)[i],rows=rows,cols=cols)
              ALL <- bind_rows(ALL,new)
          }
      }
      
      ALL.SHAL <- NULL
      ALL.DEEP <- NULL
      for(i in 1:nrow(ALL)){
        shal.val = CCOR.shallow %>% dplyr::select(ALL$cols[i]) %>% slice((ALL$rows[i]-1):(ALL$rows[i]+1)) %>% unlist() %>%  mean(.,na.rm=T)
        deep.val = CCOR.deep %>% dplyr::select(ALL$cols[i]) %>% slice((ALL$rows[i]-1):(ALL$rows[i]+1)) %>% unlist() %>%  mean(.,na.rm=T)
          CCOR.shallow[ALL$rows[i],ALL$cols[i]] <- shal.val
          CCOR.deep[ALL$rows[i],ALL$cols[i]] <- deep.val
      
          ALL.SHAL <- bind_rows(ALL.SHAL,
                                CCOR.shallow %>% slice(ALL$rows[i]) %>% dplyr::select(year,season,dev=as.character(ALL$location.name[i])))
          ALL.DEEP <- bind_rows(ALL.DEEP,
                                CCOR.deep %>% slice(ALL$rows[i]) %>% dplyr::select(year,season,dev=as.character(ALL$location.name[i])))
          }
          ALL.SHAL$location.name <- ALL$location.name
          ALL.DEEP$location.name <- ALL$location.name
          

         ### MERGE THESE BACK INTO THE temp.dev.deep and temp.dev.shallow
        temp.dev.deep <- full_join(temp.dev.deep,ALL.DEEP)
        temp.dev.shallow <- full_join(temp.dev.shallow,ALL.SHAL)
      ## Add these interpolated deviations back into the files that get passed into STAN
      
#####################


SEASON <- c("Spr","Sum","Fal","Win")
pair.cor <- NULL
for(i in 1:length(SEASON)){
      A <-    CCOR.shallow %>% filter(.,SEASON[i] %in% season) %>% dplyr::select(.,-season,-year) %>%
            cor(.) %>% melt(.) 
      A$season = SEASON[i]
      pair.cor <- rbind(pair.cor,A)
}

pair.cor$season   <-  factor(pair.cor$season, 
                              levels = c("Win","Spr","Sum","Fal"))

cross.corr <- ggplot(pair.cor) +
              geom_tile(aes(fill=value,x=Var1,y=Var2)) + 
              facet_wrap(~season,ncol=2) +
              scale_fill_gradientn(colors=c(grey(0.8),"yellow","orange","red"),limits=c(0,1))
cross.corr


pdf(file=paste(base.dir,"/Salmon-Climate/Coastwide SST/_Plots/Temperature plots coastwide -",TYPE,".pdf",sep=""),onefile=T,width=7,height=7)
  print(all.ts.shallow)
  print(time.avg.plot.shallow)
  print(time.avg.plot.deep)
  print(dev.shallow)
  print(dev.deep)
  print(cross.corr)    
dev.off()    
  
############################## Write temperature deviations to file...
### Make a grid that is similar to the effort data

Cor.3seas <-dcast(temp.dev.3seas, year+season~location.name,value.var = "dev")
Cor.4seas <-dcast(temp.dev.deep, year+season~location.name,value.var = "dev")

DEV.3seas.deep <- Cor.3seas[order(Cor.3seas$year,Cor.3seas$season),]
DEV.4seas.deep <- Cor.4seas[order(Cor.4seas$year,Cor.4seas$season),]

# write.csv(DEV.3seas.shallow,file=paste(base.dir,"/Salmon-Climate/Processed Data/Temperature/Temperature Deviations 3seas Shallow.csv",sep=""),row.names=F)
# write.csv(DEV.4seas.shallow,file=paste(base.dir,"/Salmon-Climate/Processed Data/Temperature/Temperature Deviations 4seas Shallow.csv",sep=""),row.names=F)
write.csv(DEV.3seas.deep,file=paste(base.dir,"/Salmon-Climate/Processed Data/Temperature/Temperature Deviations 3seas Deep.csv",sep=""),row.names=F)
write.csv(DEV.4seas.deep,file=paste(base.dir,"/Salmon-Climate/Processed Data/Temperature/Temperature Deviations 4seas Deep.csv",sep=""),row.names=F)
write.csv(temp.shallow.wide,file=paste(base.dir,"/Salmon-Climate/Processed Data/Temperature/Temperature Shallow.csv",sep=""),row.names=F)
write.csv(temp.deep.wide,file=paste(base.dir,"/Salmon-Climate/Processed Data/Temperature/Temperature Deep.csv",sep=""),row.names=F)

#############################################################################
#############################################################################
#############################################################################
#############################################################################
# THis is useful for making labels and getting areas in the right order. (swaps SJDF for PUSO_out)
LOCATIONS.plot <- LOCATIONS %>% mutate(location.name= as.character(location.name),location.name = ifelse(location.name=="PUSO_out","SJDF",location.name)) %>%
  arrange(desc(location.number))


plot.temp.heatmap <- function(temp.all,TITLE,PUSO=FALSE){
  Title = TITLE
  x.lab.year <- unique(temp.all$year)
  dev  <- as.matrix(temp.all %>% dplyr::select(-year,-season))
  x.lab <- paste(temp.all$season)


  MAX <- max(abs(c(min(dev),max(dev))))
  z.lim	<- c(-MAX,MAX)
  col.br<- colorRampPalette(c("blue", grey(0.9), "red"))
  
  if(PUSO==FALSE){
    LOC <- LOCATIONS.plot %>% filter(!location.name %in% c("PUSO","SGEO","SJDF")) %>% arrange(location.number)
    LOC$location.number <- 1:nrow(LOC)
  }else{
    LOC <- LOCATIONS.plot %>% arrange(location.number)
  }
  
  par(mfrow=c(1,1),oma=c( 0,0,0,3),mar=c(4,4,0.5,2) )
    image(z=dev,x=1:nrow(dev),y=LOC$location.number,axes=F,ylab="",xlab="",zlim=z.lim,col=col.br(32))
  box(bty="o",lwd=2)
  axis(1,las=2,at= 1:nrow(dev),labels=x.lab,tcl=0,cex.axis=0.5,hadj=0.3)
  axis(1,las=2,at= seq(2,nrow(dev),by=4),labels=x.lab.year,cex.axis=0.9,hadj=1.5,tcl=0)
  axis(2,las=2,at=1:ncol(dev),labels= colnames(dev) ,tcl= -0.2,cex.axis=0.7,hadj=0.8)
  
  #title(main=Title)
  par(mfrow=c(1,1), oma=c( 0,1,0,0))
  op <- par(cex = 0.75)
  ticks <- round(seq(min(z.lim),max(z.lim),length.out=7),2)
  image.plot(dev,legend.only=T,col=col.br(32),zlim=z.lim,axis.args=list(at=ticks,labels=ticks),
             #legend.args=list(text="",cex=0.8),
             add=T,legend.shrink=0.8)
}
#############

# temp.all <- DEV.4seas.shallow
# # Cull winter 1979 and winter 1979
# temp.all <- temp.all[temp.all$year!=2017,]
# #temp.all <- temp.all[2:nrow(temp.all),]
# temp.all.shal <- temp.all
# 
# quartz(file=paste(base.dir,"/Salmon-Climate/Coastwide SST/Temperature Deviations Shallow.pdf",sep=""),height=8,width=11.5,dpi=600,type="pdf")
#   plot.temp.heatmap( temp.all=temp.all.shal,TITLE="Shallow")
# dev.off()

temp.all <- DEV.4seas.deep %>% filter(year >=1982, year <=2016) 
temp.all$season <- factor(temp.all$season,levels=c("Spr","Sum","Fal","Win"))
temp.all <- temp.all %>% arrange(year,season)
temp.all <- temp.all %>% dplyr::select(year,season,c(as.character(LOCATIONS$location.name))) %>% rename("SJDF"="PUSO_out")


quartz(file=paste(base.dir,"/Salmon-Climate/Coastwide SST/_Plots/Temperature Deviations Deep (no PUSO).pdf",sep=""),height=7,width=10,dpi=600,type="pdf")
  plot.temp.heatmap( temp.all = temp.all %>% dplyr::select(-PUSO,-SGEO,-SJDF),TITLE="Deep")
dev.off()

quartz(file=paste(base.dir,"/Salmon-Climate/Coastwide SST/_Plots/Temperature Deviations Deep (all).pdf",sep=""),height=7,width=10,dpi=600,type="pdf")
  plot.temp.heatmap( temp.all = temp.all,TITLE="Deep",PUSO=TRUE)
dev.off()


#

# Plot without SGEO and PUSO
# 
# temp.all <- DEV.4seas.deep %>% dplyr::select(-SGEO,-PUSO)
# # Cull winter 1979 and winter 1979
# temp.all <-temp.all[temp.all$year!=2017,]
# #temp.all <- temp.all[2:nrow(temp.all),]
# temp.all.deep <- temp.all
# 
# quartz(file=paste(base.dir,"/Salmon-Climate/Coastwide SST/Temperature Deviations Deep -no SGEO,PUSO.pdf",sep=""),height=8,width=11.5,dpi=600,type="pdf")
# plot.temp.heatmap( temp.all=temp.all.deep,TITLE="Deep")
# dev.off()

#######################################################################################
#######################################################################################
#######################################################################################
################################## MAKE PLOTS THAT COULD BE USED IN MANUSCRIPT
#######################################################################################
#######################################################################################
#######################################################################################

## FOCUS ON JUST THE COASTAL AREAs
avg.deep <- time.avg.deep %>% filter(!season == "Win",!location.name %in% c("PUSO","PUSO_out","SJDF","SGEO")) %>%
                mutate(season.plot=case_when(season=="Sum"~"Summer",
                                        season=="Spr"~"Spring",
                                        season=="Fal"~"Fall"))
avg.deep$season.plot <- factor(avg.deep$season.plot,levels=c("Spring","Summer","Fall"))

avg.deep.plus.salish <- time.avg.deep %>% filter(!season == "Win") %>%
  mutate(season.plot=case_when(season=="Sum"~"Summer",
                               season=="Spr"~"Spring",
                               season=="Fal"~"Fall"))
avg.deep$season.plot <- factor(avg.deep$season.plot,levels=c("Spring","Summer","Fall"))


time.avg.plot.deep <- ggplot(avg.deep) +
  geom_point(aes(x=location.name,y=MEAN)) +
  geom_errorbar(aes(x=location.name,ymin=MEAN-SD, ymax=MEAN+SD), width=.1) +
  theme_bw() +
  facet_wrap(~season.plot,ncol=1) +
  ylab("Temperature (C)") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
time.avg.plot.deep

time.avg.plot.deep.salish <- ggplot(avg.deep.plus.salish)+
  geom_point(aes(x=location.name,y=MEAN)) +
  geom_errorbar(aes(x=location.name,ymin=MEAN-SD, ymax=MEAN+SD), width=.1) +
  theme_bw() +
  facet_wrap(~season.plot,ncol=1) +
  ylab("Temperature (C)") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))

dev.deep.trim <- temp.dev.deep %>% filter(!season == "Win",!location.name %in% c("PUSO","PUSO_out","SGEO"),year>=1982) %>%
                  mutate(season.plot=case_when(season=="Sum"~"Summer",
                                          season=="Spr"~"Spring",
                                          season=="Fal"~"Fall"))
dev.deep.trim$season.plot <- factor(dev.deep.trim$season.plot,levels=c("Spring","Summer","Fall"))
dev.deep.trim$location.name <- factor(dev.deep.trim$location.name,levels=
                                        LOCATIONS.plot$location.name)
dev.deep.trim <- dev.deep.trim %>% arrange(location.name)

dev.deep.trim.salish <- temp.dev.deep %>% filter(!season == "Win",year>=1982) %>%
  mutate(season.plot=case_when(season=="Sum"~"Summer",
                               season=="Spr"~"Spring",
                               season=="Fal"~"Fall"))
dev.deep.trim.salish$season.plot <- factor(dev.deep.trim.salish$season.plot,levels=c("Spring","Summer","Fall"))

dev.deep.trim.salish <- dev.deep.trim.salish %>% mutate(location.name = as.character(location.name), 
                                                        location.name = ifelse(location.name=="PUSO_out","SJDF",location.name))
dev.deep.trim.salish$location.name <- factor(dev.deep.trim.salish$location.name,levels=
                                        LOCATIONS.plot$location.name)
dev.deep.trim.salish <- dev.deep.trim.salish %>% arrange(location.name)

dev.deep <- ggplot(dev.deep.trim.salish) +
  geom_point(aes(x=year,y=dev,color=location.name)) +
  geom_line (aes(x=year,y=dev,color=location.name)) +
  facet_wrap(~season.plot,ncol=1) +
  scale_color_viridis_d("Location",alpha=0.7,begin=0,end=0.8) +
  scale_x_continuous("Year",breaks=c(seq(1985,2015,by=5)))+
  ylab("Temperature Deviation (C)") +
  xlab("")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
  geom_hline(yintercept = 0,linetype=2)
dev.deep


g2 <- ggplotGrob(time.avg.plot.deep)
g3 <- ggplotGrob(dev.deep)
g <- cbind(g2, g3, size = "first")
g$heights <- unit.pmax(g2$heights, g3$heights)
grid.newpage()
grid.draw(g)

quartz(file=paste(base.dir,"/Salmon-Climate/Coastwide SST/_Plots/Temperature and Deviation.pdf",sep=""),height=6,width=8,dpi=600,type="pdf")
  print(grid.draw(g))
dev.off()
#
####################################################3
####################################################3
####################################################3
####################################################3
####################################################3
# Pull in temperature projections
####################################################3
####################################################3
####################################################3
####################################################3
####################################################3

source(paste0(base.dir,"/Salmon-Climate/Climate R Scripts/Link OISST and projections.R"),local=T)
### The important data.frame here is "dat.proj"

dat.proj.base <- left_join(dat.proj, locations.all %>% rename(region_numb = location.number))

# Adjust for the fact that the projection data uses 1975-2005 and the OISST derived mean is 1982-2016.
dat.proj <- left_join(dat.proj.base, temp.dev.deep %>% 
                        filter(!season=="Win") %>% group_by(season,location.name) %>% 
                        summarise(ts.mean=mean(ts.mean),ts.mean.trim=mean(ts.mean.trim),ts.mean.diff=mean(diff.temp)))
dat.proj <- dat.proj %>% mutate(dev.final = dev+ts.mean.diff)  
  
X <- 2020
dat.proj <- dat.proj %>% mutate(year.lab = case_when(lab==2030 ~ X,
                                                     lab==2050 ~ X+2,
                                                     lab==2070 ~ X+4,
                                                     lab==2090 ~ X+6) )

SEQ     <- c(1982,seq(1985,2015,by=5),X + c(0,2,4,6))
SEQ.LAB <- c(1982,seq(1985,2015,by=5),2030,2050,2070,2090)

#dat.proj$location.name <- factor(dat.proj$location.name,levels=locations$location.name)
dat.proj <- dat.proj %>% arrange(location.name) %>% mutate(season.plot= case_when(season=="Spr"~"Spring",
                                                                                  season=="Sum"~"Summer",
                                                                                  season=="Fal"~"Fall"))
dat.proj$season.plot <- factor(dat.proj$season.plot,levels=c("Spring","Summer","Fall"))
dat.proj$year.lab.jitt = dat.proj$year.lab + runif(nrow(dat.proj),-0.5,0.5)
dat.proj$location.name.plot <- dat.proj$location.name 
dat.proj <- dat.proj %>% mutate(location.name.plot=as.character(location.name.plot), 
                                location.name.plot = ifelse(location.name=="PUSO_out","SJDF",location.name.plot))
dat.proj$location.name.plot <- factor(dat.proj$location.name.plot,levels=LOCATIONS.plot$location.name)

# Fix labels for plotting
dev.deep.trim.salish$location.name <- factor(dev.deep.trim.salish$location.name,levels=LOCATIONS.plot$location.name)
#dev.deep.trim <- dev.deep.trim %>% arrange(location.name)

proj.deep <- ggplot() +
  geom_point(data=dev.deep.trim.salish,aes(x=year,y=dev,color=location.name)) +
  geom_line (data=dev.deep.trim.salish,aes(x=year,y=dev,color=location.name)) +
  geom_errorbar(data=dat.proj,aes(x=year.lab.jitt,ymin=dev-sd.temp,ymax=dev+sd.temp,color=location.name.plot),alpha=0.2) +
  geom_point(data=dat.proj,aes(x=year.lab.jitt,y=dev,color=location.name.plot)) +
  facet_wrap(~season.plot,ncol=1) +
  scale_color_viridis_d("Location",alpha=0.7,begin=0,end=0.8) +
  scale_x_continuous("Year",breaks=SEQ,labels=SEQ.LAB)+
  scale_y_continuous("Temperature Deviation (C)",breaks=c(-4:4))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10))+
  geom_hline(yintercept = 0,linetype=2)
proj.deep


write.csv(dat.proj,file= paste0(base.dir,"/Salmon-Climate/Output files/_Fit objects/__Temperature projections.csv"),row.names=F)

quartz(file=paste(base.dir,"/Salmon-Climate/Coastwide SST/_Plots/Temperature Deviation and Projection.pdf",sep=""),height=6,width=7,dpi=600,type="pdf")
  print(proj.deep)
dev.off()

########################################################
### Combine average Temperatures across space x season and temperature deviations.
########################################################


## FOCUS ON JUST THE COASTAL AREAs
avg.deep <- time.avg.deep %>% filter(!location.name %in% c("PUSO","PUSO_out","SJDF","SGEO")) %>%
  mutate(season.plot=case_when(season=="Win"~"Winter",
                               season=="Sum"~"Summer",
                               season=="Spr"~"Spring",
                               season=="Fal"~"Fall")) %>% 
  mutate(loc.numb=as.numeric(location.name),loc.numb=ifelse(loc.numb>=9,loc.numb-3,loc.numb)) %>%
  mutate(loc.numb.mod=case_when(season.plot=="Winter"~loc.numb-0.05,
                            season.plot=="Spring"~loc.numb-0.025,
                            season.plot=="Summer"~loc.numb+0.025,
                            season.plot=="Fall"~loc.numb+0.05))
avg.deep$season.plot <- factor(avg.deep$season.plot,levels=c("Spring","Summer","Fall","Winter"))

BREAK <- as.numeric(sort(unique(avg.deep$loc.numb)))
X.LAB <- unique(avg.deep$location.name)

time.avg.plot.deep <- ggplot(avg.deep) +
  geom_point(aes(x=loc.numb.mod,y=MEAN,color=season.plot)) +
  geom_line(aes(x=loc.numb.mod,y=MEAN,color=season.plot)) +
  geom_errorbar(aes(x=loc.numb.mod,ymin=MEAN-SD, ymax=MEAN+SD,color=season.plot), width=.1) +
  theme_bw() +
  scale_color_viridis_d("Season",alpha=0.7,begin=0,end=0.8) +
  ylab("Temperature (C)") +
  xlab("") +
  scale_x_continuous("",breaks=BREAK,labels = X.LAB)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),
        legend.key.height = unit(0.75,"line"),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10))
time.avg.plot.deep

dev.deep <- ggplot(dev.deep.trim %>% filter(season.plot=="Summer")) +
  geom_point(aes(x=year,y=dev,color=location.name)) +
  geom_line (aes(x=year,y=dev,color=location.name)) +
  #facet_wrap(~season.plot,ncol=1) +
  scale_color_viridis_d("Location",alpha=0.7,begin=0,end=0.8) +
  scale_x_continuous("Year",breaks=c(seq(1985,2015,by=5)))+
  ylab("Temperature Deviation (C)") +
  xlab("")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
  geom_hline(yintercept = 0,linetype=2)
dev.deep

dev.deep.plus.salish <- ggplot(dev.deep.trim.salish %>% filter(season.plot=="Summer")) +
  geom_point(aes(x=year,y=dev,color=location.name)) +
  geom_line (aes(x=year,y=dev,color=location.name)) +
  #facet_wrap(~season.plot,ncol=1) +
  scale_color_viridis_d("Location",alpha=0.7,begin=0,end=0.8) +
  scale_x_continuous("Year",breaks=c(seq(1985,2015,by=5)))+
  ylab("Temperature Deviation (C)") +
  xlab("")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
  geom_hline(yintercept = 0,linetype=2)
dev.deep.plus.salish


g2 <- ggplotGrob(time.avg.plot.deep)
g3 <- ggplotGrob(dev.deep)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()
grid.draw(g)

quartz(file=paste(base.dir,"/Salmon-Climate/Coastwide SST/_Plots/Temperature (4 season) and Summer Deviation.pdf",sep=""),
                          height=5,width=5,dpi=600,type="pdf")
print(grid.draw(g))
dev.off()
#


######################################3
#### Temperature + Projections.
######################################3


source(paste0(base.dir,"/Salmon-Climate/Climate R Scripts/Link OISST and projections.R"),local=T)
### The important data.frame here is "dat.proj"

dat.proj <- left_join(dat.proj, locations.all %>% rename(region_numb = location.number)) %>% 
                mutate(location.name=as.character(location.name),location.name= ifelse(location.name=="PUSO_out","SJDF",location.name))
# Adjust for the fact that the projection data uses 1975-2005 and the OISST derived mean is 1982-2016.
dat.proj <- left_join(dat.proj, temp.dev.deep %>% 
                        filter(!season=="Win") %>% group_by(season,location.name) %>% 
                        summarise(ts.mean=mean(ts.mean),ts.mean.trim=mean(ts.mean.trim),ts.mean.diff=mean(diff.temp)))
dat.proj <- dat.proj %>% mutate(dev.final = dev+ts.mean.diff)  

X <- 2020
dat.proj <- dat.proj %>% mutate(year.lab = case_when(lab==2030 ~ X,
                                                     lab==2050 ~ X+2,
                                                     lab==2070 ~ X+4,
                                                     lab==2090 ~ X+6) )
SEQ     <- c(1982,seq(1985,2015,by=5),X + c(0,2,4,6))
SEQ.LAB <- c(1982,seq(1985,2015,by=5),2030,2050,2070,2090)

dat.proj$location.name <- factor(dat.proj$location.name,levels=LOCATIONS.plot$location.name)
dat.proj <- dat.proj %>% arrange(location.name) %>% mutate(season.plot= case_when(season=="Spr"~"Spring",
                                                                                  season=="Sum"~"Summer",
                                                                                  season=="Fal"~"Fall"))
dat.proj$season.plot <- factor(dat.proj$season.plot,levels=c("Spring","Summer","Fall"))
dat.proj$year.lab.jitt = dat.proj$year.lab + runif(nrow(dat.proj),-0.5,0.5)

proj.deep <- ggplot() +
  geom_point(data=dev.deep.trim.salish %>% filter(season.plot=="Summer"),aes(x=year,y=dev,color=location.name)) +
  geom_line (data=dev.deep.trim.salish %>% filter(season.plot=="Summer"),aes(x=year,y=dev,color=location.name)) +
  geom_errorbar(data=dat.proj %>% filter(season.plot=="Summer"),aes(x=year.lab.jitt,ymin=dev-sd.temp,ymax=dev+sd.temp,color=location.name),alpha=0.2) +
  geom_point(data=dat.proj%>% filter(season.plot=="Summer"),aes(x=year.lab.jitt,y=dev,color=location.name)) +
  #facet_wrap(~season.plot,ncol=1) +
  scale_color_viridis_d("Location",alpha=0.7,begin=0,end=0.8) +
  scale_x_continuous("Year",breaks=SEQ,labels=SEQ.LAB)+
  scale_y_continuous("Temperature Deviation (C)",breaks=c(-4:4))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),
        legend.key.height = unit(0.75,"line"),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10))+
  geom_hline(yintercept = 0,linetype=2)
proj.deep


###
g2 <- ggplotGrob(time.avg.plot.deep)
g3 <- ggplotGrob(proj.deep)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()
grid.draw(g)

quartz(file=paste(base.dir,"/Salmon-Climate/Coastwide SST/_Plots/Temperature (4 season) and Summer Projection.jpeg",sep=""),height=5,width=5,dpi=600,type="jpeg")
  print(grid.draw(g))
dev.off()

############################################# 
#### MAKE Vertically oriented Temperature Plot
############################################# 

dev.deep.trim$location.name <- factor(dev.deep.trim$location.name,
                                      levels=LOCATIONS.plot$location.name)

proj.deep <- ggplot() +
  geom_point(data=dev.deep.trim.salish %>% filter(season.plot=="Summer"),aes(x=year,y=dev,color=location.name)) +
  geom_line (data=dev.deep.trim.salish %>% filter(season.plot=="Summer"),aes(x=year,y=dev,color=location.name)) +
  geom_errorbar(data=dat.proj %>% filter(season.plot=="Summer"),aes(x=year.lab.jitt,ymin=dev-sd.temp,ymax=dev+sd.temp,color=location.name),alpha=0.2) +
  geom_point(data=dat.proj%>% filter(season.plot=="Summer"),aes(x=year.lab.jitt,y=dev,color=location.name)) +
  #facet_wrap(~season.plot,ncol=1) +
  scale_color_viridis_d("Location",alpha=0.7,begin=0,end=0.8) +
  scale_x_continuous("Year",breaks=SEQ,labels=SEQ.LAB)+
  scale_y_continuous("Temperature Deviation (C)",breaks=c(-4:4))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),
        legend.key.height = unit(0.65,"line"),
        legend.text=element_text(size=7.5),
        legend.title=element_text(size=10))+
  geom_hline(yintercept = 0,linetype=2)
proj.deep


time.avg.plot.deep.tall <- ggplot(avg.deep %>% arrange(loc.numb.mod)) +
  geom_point(aes(x=loc.numb.mod,y=MEAN,color=season.plot)) +
  geom_line(aes(x=loc.numb.mod,y=MEAN,color=season.plot)) +
  geom_errorbar(aes(x=loc.numb.mod,ymin=MEAN-SD, ymax=MEAN+SD,color=season.plot), width=.1) +
  theme_bw() +
  scale_color_viridis_d("Season",alpha=0.7,begin=0,end=0.8) +
  ylab("Temperature (C)") +
  xlab("") +
  scale_x_continuous("",breaks=BREAK,labels = X.LAB)+
  theme(legend.key.height = unit(0.75,"line"),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10),
        legend.position=c(0.25,0.1),
        plot.margin = margin(0.1, 0.15, 0.1, -0.4, "cm")) +
  coord_flip()

time.avg.plot.deep.tall

quartz(file=paste(base.dir,"/Salmon-Climate/Coastwide SST/_Plots/Temperature Vertical (4 season) and Summer Projection.jpeg",sep=""),height=5,width=5,dpi=600,type="jpeg")
  print(grid.draw(g))
dev.off()

#################################33

g.tall <- ggplotGrob(time.avg.plot.deep.tall)
g.proj <- ggplotGrob(proj.deep)

g <- gtable(widths=unit(c(3.5,2.5),c("in")),heights=unit(c(6,3),c("in")))
g <- gtable_add_grob(g,g.tall,t=1,l=2)
g <- gtable_add_grob(g,g.proj,t=2,l=1,b=2,r=2)

quartz(file=paste(base.dir,"/Salmon-Climate/Coastwide SST/_Plots/Temperature Vertical (4 season) and Summer Projection TALL.jpeg",sep=""),height=9,width=6,dpi=600,type="jpeg")
  print(plot(g))
dev.off()


