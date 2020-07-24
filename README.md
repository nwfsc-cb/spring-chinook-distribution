# rmis
Repository for joining RMIS release, recovery, and location data

All data taken from RMIS, http://www.rmpc.org/ 

"identify_spring_releases.R" is a script for parsing the total releases and identifying releases that are spring and have adequate numbers of CW tags and sufficient temporal resolution. The releases identified by this script are summarized in the RData file "focal_spring_chinook_releases.RData" with a summary contained in the CSV file "focal_spring_chinook_releases_summary.csv"

"focal_sp_release_recover.R" is a script to combine releases and recoveries, filters the run's of interest based on what Ole identified in "identify_spring_releases.R" also adds CWT by row to get total releases
dat_focal.csv has combination of releases and recoveries filtered for focal stocks_ 
"plotting_recoveries.R" is a script to plot recovery counts through time by gear type and state. Plots from this saved as "Recovery Counts through Time with Gear Type.pdf" and "Recovery by gear type and state.pdf"


Scripts in /scripts_gs :_

"AK_compare_squashdata_to_RMIS.R" is a script that compares the squash data set that Jordan sent us and to RMIS data. The data are redundant but squashData can help us get to the fraction of fish sampled. Data that Jordan sent is summarized in "JW Script for Ole.Rmd

"Plot for JW presentation.R" - Creates plots that summarize RMIS recoveries in certian regions, Jordan used some of these plots in a slide for a by catch meeting 

"reassign recoveries_NMFS bounds.R" This script creates a new column in the recovery code database "recovery codes wietkamp and shelton.csv" where AK codes are assigned into NMFS Statistical Regions, then it merges in non-alaska recovery codes where the assignment didn't change

"RMIS_plot.Rmd" plots new assignments from "reassign recoveries_NMFS bounds.R_" to make sure everything is looking good. 

"RMISallstocks.R" was used to pull RMIS data for all stocks (instead of just focal spring stocks) so GS could make some plots for folks at AFSC interested in bycatch. 

"Spatial Boundary Map.R" will plot our current spatial assignments on a map, just plots boundaries but can use this as a base to overlay points/data 


Alaska Data Processing: 

Pollock Effort: "AK_pollock_effort_sample_fraction.RMD" This file gets at the first half of the sample fraction - What fraction of boat days had an observer onboard?  

Rockfish Effort:  "AK_Rockfish_Effort.RMD" 2013- 2017 summarized rockfish effort into boat days. 100% observer census in fishery so summarizing total effort into appropriate space and time scales is all we need to do. 

ALASKA CWT Data Flow:

- Pollock CWT Recoveries: Michele Masuda provided CWT recoveries with vessel information so that we can match the RMIS fishery recovery code 81 to pollock trawl fleets.
Using observer data from AFSC we matched trip target codes with CWT Recoveries, this matched about 50% of the data. For the rest of the data use pollock fishing dates to assign a recovery based on if it occurred within the pollock season or not.  

script: "AK_assign_RMIS_trip_targets_CWT.R" 
resulting RDS with recoveries assigned to a fleet: AK_CWT_Pollock.RDS


- Rockfish CWT Recoveries: Can use RMIS recoveries. 