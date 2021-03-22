library(tidyverse)
# add in our regions assigned using this map - http://www.pac.dfo-mpo.gc.ca/fm-gp/commercial/ground-fond/index-eng.html
#S. ANDERSON did the groupings for us though.

base.dir <- "/Users/ole.shelton/Github"
data.dir <- paste0(base.dir,"/Orca_Salmon_DATA/Effort info/Trawl-CAN")
write.dir <- paste0(base.dir,"/spring-chinook-distribution/Processed Data/Effort Data") 


#comment in or out the month structure that you want to use 
# month_group1 <-  c(4, 4, 4, 1, 1, 2, 2, 3, 3, 3, 4, 4))
setwd(data.dir)
`bc.hake.eff1` <- readRDS("hake-2019-09-12-ole-1.rds") %>%
  dplyr::mutate(region = case_when(
    area_grouped == "4B"  ~ "SGEO",
    area_grouped == "3C"  ~ "SWVI",
    area_grouped == "3D"  ~ "NWVI",
    area_grouped == "5ABC"  ~ "CBC",
    TRUE ~ "NBC")) 


# month_group2 = c(4, 1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4))

`bc.hake.eff2` <- readRDS("hake-2019-09-12-ole-2.rds") %>%
  dplyr::mutate(region = case_when(
    area_grouped == "4B"  ~ "SGEO",
    area_grouped == "3C"  ~ "SWVI",
    area_grouped == "3D"  ~ "NWVI",
    area_grouped == "5ABC"  ~ "CBC",
    TRUE ~ "NBC")) 


#View(bc.hake.eff)
setwd(write.dir)
write.csv(bc.hake.eff1, "bc.hake.eff1.csv" )
write.csv(bc.hake.eff2, "bc.hake.eff2.csv" )

