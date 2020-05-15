# rmis
Repository for joining RMIS release, recovery, and location data

All data taken from RMIS, http://www.rmpc.org/ 


"identify_spring_releases.R" is a script for parsing the total releases and identifying releases that are spring and have adequate numbers of CW tags and sufficient temporal resolution. The releases identified by this script are summarized in the RData file "focal_spring_chinook_releases.RData" with a summary contained in the CSV file "focal_spring_chinook_releases_summary.csv"

"focal_sp_release_recover.R" is a script to combine releases and recoveries, filters the run's of interest based on what Ole identified in "identify_spring_releases.R" also adds CWT by row to get total releases
dat_focal.csv has combination of releases and recoveries filtered for focal stocks_ 
"plotting_recoveries.R" is a script to plot recovery counts through time by gear type and state. Plots from this saved as "Recovery Counts through Time with Gear Type.pdf" and "Recovery by gear type and state.pdf"


Scripts in /scripts_gs :_

"AK_RMIS_parce.Rmd" is a script that parses data that Jordan sent us and compares it to our RMIS data base. Data that Jordan sent is summarized in "JW Script for Ole.Rmd

"Plot for JW presentation.R" - Creates plots that summarize RMIS recoveries in certian regions, Jordan used some of these plots in a slide for a by catch meeting 

"reassign recoveries_NMFS bounds.R" This script creates a new column in the recovery code database "recovery codes wietkamp and shelton.csv" where AK codes are assigned into NMFS Statistical Regions, then it merges in non-alaska recovery codes where the assignment didn't change

"RMIS_plot.Rmd" plots new assignments from "reassign recoveries_NMFS bounds.R_" to make sure everything is looking good _

"RMIS.R"_is a script that joins releases and recoveries from our focal species and then matches in the recovery code spatial assignments and parces RMIS so it is ready for further use

"Spatial Boundary Map.R" will plot our current spatial assignments on a map, just plots boundaries but can use this as a base to overlay points/data 


GS Notes 5/15/20 re AK data
CWT Recoveries: "Ak_match_trawl_cwt.R" this uses AKFIN data to match RMIS CWT Recoveries to specific trawl fleets. as of 5/15/20 GS has matched ~80% but I think I can go back and refine some of it. 

Pollock Effort: "AK_pollock_effort_sample_fraction.RMD" Takes data from Jason Gasper and uses the column with # of observers onboard to differentiate effort between obs and unobs effort. Summarizes different types of effort, plots, and calculates sample fraction based of effort and observer protocols re JW.

Rockfish Effort: We have CWT Data and I believe we can get obs and unobs effort from Gasper's data. Jordan may have passed it off already in a different form, but if he did I forget/didn't transfer that data to my WFH computer, so if we want to use it I will do some more work with this! 