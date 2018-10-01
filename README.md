# rmis
Repository for joining RMIS release, recovery, and location data

All data taken from RMIS, http://www.rmpc.org/ 


"identify_spring_releases.R" is a script for parsing the total releases and identifying releases that are spring and have adequate numbers of CW tags and sufficient temporal resolution. The releases identified by this script are summarized in the RData file "focal_spring_chinook_releases.RData" with a summary contained in the CSV file "focal_spring_chinook_releases_summary.csv"

"focal_sp_release_recover.R" is a script to combine releases and recoveries, filters the run's of interest based on what Ole identified in "identify_spring_releases.R" also adds CWT by row to get total releases 
"plotting_recoveries.R" is a script to plot recovery counts through time by gear type and state. Plots from this saved as "Recovery Counts through Time with Gear Type.pdf" and "Recovery by gear type and state.pdf"