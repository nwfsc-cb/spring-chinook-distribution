library(dplyr)
library(ggplot2)

# Load release data
release = read.csv("data/chinook/all_releases.txt", header=T, stringsAsFactors = FALSE) 

# Pull out spring run Chinook (run == 1), summer Chinook (run == 2), and missing (run == is.na)
  release.spr <- release %>% filter(run==1 | run==2 |is.na(run)==T)

  # Cull releases with zero CWT releases.
  release.spr <- release.spr %>% 
                  mutate(cwt_1st_mark_count =if_else(is.na(cwt_1st_mark_count),0,as.numeric(cwt_1st_mark_count)),
                  cwt_2nd_mark_count =if_else(is.na(cwt_2nd_mark_count),0,as.numeric(cwt_2nd_mark_count))) %>%
                  mutate(total_cwt_rel = cwt_1st_mark_count + cwt_2nd_mark_count) %>% 
                  filter(total_cwt_rel >0 )
  
  ## Find the releases with NA run type and fix these....
  # Replace Alaska runs with run=1 (spring run) because there are no other run types in AK
  release.spr <- release.spr %>% mutate(.,run=ifelse(release_location_state=="AK",1,run)) 
  # Replace missing states with values derived from the hatchery and release locations
  release.spr <- release.spr %>% mutate(.,release_location_state=ifelse(is.na(release_location_state)==T & is.na(run)==T & substr(hatchery_location_code,1,1) == "3","WA",release_location_state))
  ## Cull wild releases because their run type is unclear in most cases.
  release.spr <- release.spr %>% filter(is.na(run)==F | (is.na(run)==T & rearing_type != "W"))
  # That leaves 49 entries that have no run type: dim(release.spr %>% filter(is.na(run)==T))
  ## Ole is ok dropping these 49 entries out of > 14,000.  Most of them are from Idaho (>40)
  release.spr <- release.spr %>% filter(is.na(run)==F)
  
  # summarize the releases by run, region, and year
    release.spr.summary <- release.spr %>% 
                            group_by(run,brood_year,release_location_state) %>% 
                            summarise(total_cwt_rel = sum(total_cwt_rel)) %>% as.data.frame()
    # Plot Total tagged release
    ggplot(release.spr.summary %>% filter(brood_year >1975, run==1)) +
        geom_line(aes(y=total_cwt_rel,x=brood_year)) +
        facet_wrap(~release_location_state)
                                   
###### Let's divide the releases into state specific chunks for easier manipulation.
    release.spr.ca <- release.spr %>% filter(release_location_state=="CA")
    release.spr.or <- release.spr %>% filter(release_location_state=="OR")      
    release.spr.id <- release.spr %>% filter(release_location_state=="ID")
    release.spr.wa <- release.spr %>% filter(release_location_state=="WA")
    release.spr.bc <- release.spr %>% filter(release_location_state=="BC")
    release.spr.ak <- release.spr %>% filter(release_location_state=="AK")
############
    
    # Start with California
    rel.spr.ca.summ <- release.spr.ca %>% group_by(release_location_rmis_region,release_location_rmis_basin,brood_year,run) %>% 
          summarise(TOT=sum(total_cwt_rel)) %>% arrange(release_location_rmis_region,brood_year,run) %>% as.data.frame()
    
    ggplot(rel.spr.ca.summ) +
        geom_line(aes(y=TOT,x=brood_year,color=release_location_rmis_basin)) +
        geom_point(aes(y=TOT,x=brood_year,color=release_location_rmis_basin)) +
        facet_grid(release_location_rmis_region~.)
    ## This plot says a lot- Klamath-Trinity are constant and regular.  
    ## Others are sporadice and mostly absent from the early years.
    
    rel.spr.ca.summary   <- release.spr.ca %>% group_by(hatchery_location_code,hatchery_location_name,stock_location_code,stock_location_name,brood_year) %>% 
                      summarize(TOT =sum(total_cwt_rel)) %>% group_by(hatchery_location_code,hatchery_location_name,stock_location_code,stock_location_name) %>% 
                      summarize(N.year = length(TOT), Total.rel = sum(TOT)) %>% as.data.frame()
    
    # So there are only three hatcheries of spring Chinook of note in California.
    
    
    
    
# release = select(release, tag_code_or_release_id, run, brood_year, first_release_date,
#                  release_location_code, stock_location_code, cwt_1st_mark_count, cwt_2nd_mark_count,
#                  non_cwt_1st_mark_count, non_cwt_2nd_mark_count, release_location_name,
#                  stock_location_name, release_location_state, release_location_rmis_region, 
#                  release_location_rmis_basin) %>% 
#   rename(tag_code = tag_code_or_release_id)