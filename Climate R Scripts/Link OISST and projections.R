### This script is to examine the projected ocean temperatures and link them to the OISST data.  
### Basic gis data was conducted by B. Feist.


###### OISST WORK

### This is a script to read in, process and make descriptive plot of the Ocean
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(fields)
library(RColorBrewer)


base.dir <- "/Users/ole.shelton/GitHub"

TYPE ="MEAN"

# read in data and identifiers
# dat.temp <- read.csv(paste(base.dir,"/Salmon-Climate/Coastwide SST/regions_depth_zones_SST_PAT.csv",sep=""))
pdo.dat    <- read.csv(paste(base.dir,"/Salmon-Climate/Coastwide SST/PDO monthly.csv",sep=""))
npgo.dat    <- read.csv(paste(base.dir,"/Salmon-Climate/Coastwide SST/NPGO monthly.csv",sep=""))
dat.temp   <- read.csv(paste(base.dir,"/Salmon-Climate/Coastwide SST/Seasonal SST by regions & depth zones from OISST V2 daily data.csv",sep=""))
locations  <-  read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations.csv",sep=""))
if(loc_18=="TWO_OR"){ # deal with combining the two OREGON regions if appropriate
  locations  <-  read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations_plus_two_OR.csv",sep=""))
  locations.all <- locations ### this is for merging with the original numbering system.
  locations <- locations %>% filter( location.name != "PUSO_out")
  locations$location.number <- 1:nrow(locations)
  dat.temp <- dat.temp %>% mutate(Region_num = ifelse(Region_num>=6,Region_num-1,Region_num))
}
if(loc_18 =="NCA_SOR_PUSO"){  # deal with combining the NCA and SOR regions if appropriate
  locations <- read.csv(paste(base.dir,"/Salmon-Climate/Processed Data/locations_plus_NCA_SOR_PUSO.csv",sep=""))
  locations.all <- locations
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


###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
##### Go get the various projections and manipulate.
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################

dat.75.05 <- read.csv(paste0(base.dir,"/Salmon-Climate/Coastwide SST/Future Projections/SST_mpi-esm-mr_1975-2005.csv"))
dat.19.00 <- read.csv(paste0(base.dir,"/Salmon-Climate/Coastwide SST/Future Projections/SST_mpi-esm-mr_2019-2100.csv"))

dat.75.05.long <- pivot_longer(dat.75.05,cols=5:ncol(dat.75.05),names_to = "month_year", values_to="K")
dat.75.05.long <- dat.75.05.long %>% 
                      mutate(Month=substr(month_year,1,3)) %>% 
                      mutate(Year=as.numeric(as.character(substr(month_year,nchar(month_year)-3,nchar(month_year)))))

dat.19.00.long <- pivot_longer(dat.19.00,cols=5:ncol(dat.19.00),names_to = "month_year", values_to="K")
dat.19.00.long <- dat.19.00.long %>% 
                      mutate(Month=substr(month_year,1,3)) %>% 
                      mutate(Year=as.numeric(as.character(substr(month_year,nchar(month_year)-3,nchar(month_year)))))

# Fix regions..... THIS gets rid of area 5 (SOR) and merges it in with NCA and adjusts the various regions north of their appropriately. 

dat.75.05.long <- dat.75.05.long %>% 
                        mutate(region_numb= 
                                 case_when(Ole_region == 5 ~ 4,
                                           Ole_region >= 6 & Ole_region <= 9 ~ Ole_region-1,
                                           Ole_region >=12 | Ole_region <=4  ~ as.numeric(Ole_region)))
dat.19.00.long <- dat.19.00.long %>% 
                        mutate(region_numb= 
                            case_when(Ole_region == 5 ~ 4,
                                      Ole_region >= 6 & Ole_region <= 9 ~ Ole_region-1,
                                      Ole_region >=12 | Ole_region <=4 ~ as.numeric(Ole_region)))

dat.75.05.long <- dat.75.05.long %>% 
  mutate(month_numb = 
           case_when(Month == "Jan" ~ 1,
                     Month == "Feb" ~ 2,
                     Month == "Mar" ~ 3,
                     Month == "Apr" ~ 4,
                     Month == "May" ~ 5,
                     Month == "Jun" ~ 6,
                     Month == "Jul" ~ 7,
                     Month == "Aug" ~ 8,
                     Month == "Sep" ~ 9,
                     Month == "Oct" ~ 10,
                     Month == "Nov" ~ 11,
                     Month == "Dec" ~ 12))


dat.19.00.long <- dat.19.00.long %>% 
  mutate(month_numb = 
           case_when(Month == "Jan" ~ 1,
                     Month == "Feb" ~ 2,
                     Month == "Mar" ~ 3,
                     Month == "Apr" ~ 4,
                     Month == "May" ~ 5,
                     Month == "Jun" ~ 6,
                     Month == "Jul" ~ 7,
                     Month == "Aug" ~ 8,
                     Month == "Sep" ~ 9,
                     Month == "Oct" ~ 10,
                     Month == "Nov" ~ 11,
                     Month == "Dec" ~ 12))

#### AGGREGATE by month and year and area.  Use all data, convert to Centigrade
dat.75.05.sum <- dat.75.05.long %>% group_by(Month,Year,month_numb,region_numb) %>%
                          summarise(mean.temp = mean(K) - 273.15,sd.temp = sd(K), n.obs = length(K)) %>%
                          mutate(year.dec = Year + (month_numb-1)/12)
dat.19.00.sum <- dat.19.00.long %>% group_by(Month,Year,month_numb,region_numb) %>%
                          dplyr::summarize(mean.temp = mean(K)- 273.15,sd.temp = sd(K), n.obs = length(K)) %>%
                          mutate(year.dec = Year + (month_numb-1)/12)


ggplot(dat.75.05.sum, aes(y=mean.temp,x=year.dec,color=as.factor(region_numb))) +
    geom_line()
ggplot(dat.19.00.sum, aes(y=mean.temp,x=year.dec,color=as.factor(region_numb))) +
  geom_line()


##### Combine model files, add in seasonal component to link to observed OISST data.

dat.mod <- bind_rows(dat.75.05.sum,dat.19.00.sum)

# Make seasons
### THIS IS FOR THE "FOUR" MONTH.STRUCTURE. COULD REPLACE WITH "FRAM" STRUCTURE IF YOU WANTED.

dat.mod <- dat.mod %>% mutate(season=case_when(Month %in% c("Jan","Feb","Mar","Nov","Dec") ~ "Win",
                                               Month %in% c("Apr","May") ~ "Spr",
                                               Month %in% c("Jun","Jul") ~ "Sum",
                                               Month %in% c("Aug","Sep","Oct") ~ "Fal")) %>%
                        mutate(year.mod=case_when(Month %in% c("Jan","Feb","Mar") ~ Year-1,
                                                  !Month %in% c("Jan","Feb","Mar") ~ Year))


ggplot(dat.mod, aes(y=mean.temp,x=year.dec)) +
  geom_line()+
  facet_wrap(~region_numb)

# summarize by season - simple average of average within months.  Could also weight by variance within a month-year
dat.mod.season <- dat.mod %>% group_by(year.mod, season, region_numb) %>% summarize(mod.temp = mean(mean.temp))

### Merge in the data from OISST.
# temp.sum.shallow is the operative data for shallow depths (< 200m)  Deep is for <400m

dat.all <- dat.mod.season %>% 
                left_join(.,
                  temp.sum.shallow %>% rename(year.mod=year,region_numb=Region_num, OISST.shallow = AW_Mean) %>%  dplyr::select(year.mod,region_numb,season,OISST.shallow)) %>%
                left_join(.,
                  temp.sum.deep %>% rename(year.mod=year,region_numb=Region_num, OISST.deep = AW_Mean) %>%  dplyr::select(year.mod,region_numb,season,OISST.deep))

### Compare data from OISST and model output.

## Make some plots

P1 <- ggplot(dat.all) +
          geom_point(aes(x=OISST.shallow,y=mod.temp,color=season))+
          geom_abline(slope=1,intercept=0,color="red") +
          facet_wrap(~season)
P1


P2 <- ggplot(dat.all) +
  geom_point(aes(x=OISST.deep,y=mod.temp,color=season))+
  geom_abline(slope=1,intercept=0,color="red") +
  facet_wrap(~season)
P2

P3 <- ggplot(dat.all) +
  geom_point(aes(x=OISST.shallow,y=OISST.deep,color=season))+
  geom_abline(slope=1,intercept=0,color="red") +
  facet_wrap(~season)
P3

# Compare within season by area
ALP = 0.5 

Q1 <- ggplot(dat.all %>% filter(season=="Spr")) +
  geom_point(aes(x=OISST.shallow,y=mod.temp),alpha=ALP)+
  geom_abline(slope=1,intercept=0,color="red") +
  ggtitle("Spring by region number")+
  ylab("Model Temperature") +
  xlab("OISST temperature (0-200m)")+
  theme_bw() +
  facet_wrap(~region_numb)
Q1


Q2 <- ggplot(dat.all %>% filter(season=="Sum")) +
      geom_point(aes(x=OISST.shallow,y=mod.temp),alpha=ALP)+
      geom_abline(slope=1,intercept=0,color="red") +
      ggtitle("Summer by region number")+
      ylab("Model Temperature") +
      xlab("OISST temperature (0-200m)")+
      theme_bw() +
      facet_wrap(~region_numb)
Q2

Q3 <- ggplot(dat.all %>% filter(season=="Fal")) +
  geom_point(aes(x=OISST.shallow,y=mod.temp),alpha=ALP)+
  geom_abline(slope=1,intercept=0,color="red") +
  ggtitle("Fall by region number")+
  ylab("Model Temperature") +
  xlab("OISST temperature (0-200m)")+
  theme_bw() +
  facet_wrap(~region_numb)
Q3

### Calculate pairwise correlations between temperature data
cor.dat<- dat.all %>% filter(is.na(OISST.shallow)==F,is.na(OISST.deep)==F) %>% group_by(season,region_numb) %>% 
                      summarize(cor.mod.shallow=cor(x=OISST.shallow,y=mod.temp),
                                cor.mod.deep=cor(x=OISST.deep,y=mod.temp))


C1  <- ggplot(cor.dat) +
    geom_histogram(aes(cor.mod.shallow)) +
    facet_wrap(~season) +
    xlab("Correlation") +
    geom_vline(xintercept=0,color="red") +
    theme_bw()

  pdf(file=paste0(base.dir,"/Salmon-Climate/Coastwide SST/_Plots/Correlation between OISST and model output 1983-2005.pdf"),onefile = T,width=7,height=7)
    print(Q1)
    print(Q2)
    print(Q3)
    print(C1)
  dev.off()

###########################################################################################
##########################################################################################
############################################################################################
###########################################################################################
### Calculate deviation from 1982:2005 at various years in the future. Use projection model
###########################################################################################
##########################################################################################
############################################################################################
###########################################################################################

dat.trim <- dat.mod.season %>% filter(year.mod>=1982,!season == "Win") 
  
dat.base <- dat.trim %>% filter(year.mod<=2005) %>% group_by(season,region_numb) %>% summarise(base.mean=mean(mod.temp))

lab <- c(2030,2050,2070,2090)
start <- c(2025,2045,2065,2085)
end   <- c(2034,2054,2074,2094)

dat.proj <- NULL

for(i in 1: length(start)){
  temp <-  dat.trim %>% 
            filter(year.mod >= start[i],year.mod <= end[i]) %>% 
            group_by(season,region_numb) %>% 
            summarise(avg.temp = mean(mod.temp),sd.temp=sd(mod.temp)) %>%
            mutate(lab = lab[i])
  
  dat.proj <- bind_rows(dat.proj,temp)
}

dat.proj <- left_join(dat.proj,dat.base)
dat.proj <- dat.proj %>% mutate(dev = avg.temp - base.mean) 
############################ 

#### make predictions for the Salish Sea (SJDF, SGEO, PUSO) that are simply the average of each season-projection year across all locations

dat.temp <- dat.proj %>% group_by(season,lab) %>% summarise(dev=mean(dev))
dat.salish.proj <-  full_join(dat.temp,
                                expand.grid(season=dat.temp %>% dplyr::select(season) %>% unique(.) %>% unlist(),
                                            lab=dat.temp %>% as.data.frame() %>% dplyr::select(lab) %>% unique(.) %>% unlist(),
                                            region_numb=LOCATIONS %>% filter(location.name %in% c("PUSO","PUSO_out","SGEO")) %>% dplyr::select(location.number) %>% unlist())
                                )

# MERGE THE SIMPLE SALISH PROJECTIONS BACK into the full projected temperature data.frame

dat.proj <- full_join(dat.proj, dat.salish.proj) %>% arrange(season,lab,region_numb)


