library(tidyr)
library(dplyr)

# rmis
d = read.csv("/users/eric.ward/downloads/all_releases.txt", stringsAsFactors =FALSE)
d$year = as.numeric(substr(d$first_release_date,1,4))
d$period = 0 # teresa's periods
d$period[which(d$year %in% 1985:1995)] = 1
d$period[which(d$year %in% 1996:2007)] = 2
d$period[which(d$year %in% 2008:2017)] = 3

# 
basins = read.csv("/users/eric.ward/downloads/rmis_basins.csv", stringsAsFactors =FALSE)
basins$period1 = NA # for aggregate totals
basins$period2 = NA
basins$period3 = NA

for(i in 1:nrow(basins)) {
	runs = 0
	if(length(grep("Spring", basins$RunType[i])) > 0) runs = c(runs, 1)
	if(length(grep("Summer", basins$RunType[i])) > 0) runs = c(runs, 2)
	if(length(grep("Fall", basins$RunType[i]) > 0)) runs = c(runs, c(3,7,8))
	if(length(grep("Winter", basins$RunType[i])) > 0) runs = c(runs, c(4))

    # strsplit the basin names into a list and concatenate
    basn = unlist(strsplit(basins$Basin[i], ", "))
	if(basn[1] %in% c("CECR Region", NA) == FALSE) {
		g = filter(d, run %in% runs[-1], release_location_rmis_basin %in% basn) %>%
		group_by(year, release_location_rmis_basin) %>% 
		summarize(period = period[1], 
		s = sum(c(cwt_1st_mark_count,
		cwt_2nd_mark_count,
		non_cwt_1st_mark_count,
		non_cwt_2nd_mark_count), na.rm=T)) %>%
		group_by(period, release_location_rmis_basin) %>%
		summarize(s = mean(s, na.rm=T)/1000000) %>%
		filter(period > 0, release_location_rmis_basin!="") %>%
		spread(period, s) %>% as.data.frame()
		
		if(length(grep("1", names(g)) > 0)) basins$period1[i] = g[1, grep("1", names(g))]
		if(length(grep("2", names(g)) > 0)) basins$period2[i] = g[1, grep("2", names(g))]
		if(length(grep("3", names(g)) > 0)) basins$period3[i] = g[1, grep("3", names(g))]				
	}		
	
	if("CECR Region" %in% basn) {
		g = filter(d, run %in% runs[-1], release_location_rmis_region %in% basn) %>%
		group_by(year, release_location_rmis_region) %>% 
		summarize(period = period[1], 
		s = sum(c(cwt_1st_mark_count,
		cwt_2nd_mark_count,
		non_cwt_1st_mark_count,
		non_cwt_2nd_mark_count), na.rm=T)) %>%
		group_by(period, release_location_rmis_region) %>%
		summarize(s = mean(s, na.rm=T)/1000000) %>%
		filter(period > 0, release_location_rmis_region!="") %>%
		spread(period, s) %>% as.data.frame()
		
		if(length(grep("1", names(g)) > 0)) basins$period1[i] = g[1, grep("1", names(g))]
		if(length(grep("2", names(g)) > 0)) basins$period2[i] = g[1, grep("2", names(g))]
		if(length(grep("3", names(g)) > 0)) basins$period3[i] = g[1, grep("3", names(g))]				
	}			
}


