#this script runs rmis_base which adds locations to associated rmis data by recovery code 
setwd("~/Documents/GitHub/rmis/scripts_gs")
source("rmis_base.R")

#region assignment for rmis
source("assigning_regions.R")

#then we parce snoutbase data and assign it to rmis data and replace some rmis data with more accurate lat and long recovery locations
setwd("~/Documents/GitHub/Chinook_Bycatch/scripts")
source("snoutbase_rmis_join.R")

#AFTER RUNNING ALL OF THIS DAT IS THE FILE THAT HAS COMPLETE DATA SET WITH UPDATED LAT AND LONGS FROM SNOUTBASE AND REGIONS ASSIGNED

