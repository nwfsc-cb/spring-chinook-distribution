library(tidyverse)
library(here)
library(gmodels)
#will want to match this stuff with:spring_chinook_distribution/AK_Effort_Trawl/Alaska trawl observed effort
#that data is organized by: YEAR	season	rec.area	TARGET	PROCESSING_SECTOR	VESSEL_LENGTH_CATEGORY	catch_MT	obs_days	vessel_count
#this data is from the observed and unobserved data
#regional office only does species counts-not weights. that only exists for observed data. 
#obs program does weights 
df <- readRDS("data/AK_Effort_Trawl/Gasper_data/Chinooky_Rates_merged.RDS") %>%
      filter(fmp_sub_area_code == "GOA") %>%
      mutate(length_category = case_when(ves_akr_length < 125 & year < 2013 ~ "Small",
                                         ves_akr_length > 124 & year < 2013 ~ "Large",
                                         ves_akr_length > 59 & year > 2012 ~ "Large",
                                         ves_akr_length < 60 & year > 2012 ~ "Small",
                                         TRUE ~ as.character(ves_akr_length)),
              region = case_when(reporting_area_code == "610" ~ "E.APEN", 
                                reporting_area_code == "620" ~ "W.APEN",  
                                reporting_area_code %in% c("630") ~ "NW.GOA", 
                                reporting_area_code %in% c("640", "640 ") ~ "NE.GOA", 
                                reporting_area_code %in% c("649") ~ "NE.NW.GOA", #there are about 600 records from this stat area- need to divide them in half between NE and NW GOA, can do this based on ADFG numbers. check w ole before doing this.  
                                TRUE ~ "TBD")) %>% #there are none of these. 
      separate(trip_target_date, into = c("year_target", "month", "day"), sep = "-") 

#unique(explore$adfg_stat_area_code)
#intersted in rate, that is basically the expansion factor. 
 
 sp_count <- df%>% group_by(region, year, month, length_category, target, processing_sector) %>%
  summarise(estimate = sum(pscnq_estimate), #sometimes this has a NULL value bc it was not estimated (re JW's email with Cathy) so use species count value instead. 
              sp_count = sum(species_count)) #species count is reported and estimated chinook..so use that one? 

 # explore <- df %>%
#   filter(reporting_area_code == "649")
 
 
 #plot rate: 
 
 #plot the rate distribution...there are alot greater than 1. 
  df  %>%
   ggplot(aes(rate)) +
   geom_histogram(binwidth=3) + 
   theme_classic()
 
 #plot rate by year and sector, small vs large.
 df %>% 
    group_by(region, year, length_category, target, processing_sector) %>%
    dplyr::summarise(rate_mean = mean(rate),
                     lowCI = ci(rate,confidence=0.90)[2],  
                     hiCI = ci(rate,confidence=0.90)[3]) %>% 
    filter(target == "pollock", length_category == "Small" ) %>%
    ggplot(aes(year,rate_mean)) +
      geom_bar(stat="identity",alpha = 0.8) +
      facet_wrap(~region, scales = "free") +
      geom_errorbar(aes(ymin=lowCI, ymax=hiCI),position="dodge", stat="identity", alpha = 0.8) + 
      ggtitle("Small Pollock Vessels") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
 df %>% 
   group_by(region, year, length_category, target, processing_sector) %>%
   dplyr::summarise(rate_mean = mean(rate),
                    lowCI = ci(rate,confidence=0.90)[2],  
                    hiCI = ci(rate,confidence=0.90)[3]) %>% 
    filter(target == "pollock", length_category == "Large") %>%
    ggplot(aes(year,rate_mean)) +
      geom_bar(position="dodge", stat="identity",alpha = 0.8) +
      geom_errorbar(aes(ymin=lowCI, ymax=hiCI),position="dodge", stat="identity",alpha = 0.8) + 
      facet_grid(processing_sector~region, scales = "free") +
      ggtitle("Large Pollock Vessels") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
 unique(weights$reporting_area_code)

# NA's for the fishery target code=0.4% in whole dataset, GOA specific is also 4%

 weights <- readRDS("data/AK_Effort_Trawl/Gasper_data/Chinook_extrapolated_weight.RDS") %>%
  separate(haul_date, into = c("year", "month", "day"), sep = "-") %>%
  mutate(length_category = case_when(ves_akr_length < 125 & year < 2013 ~ "Small",
                                      ves_akr_length > 124 & year < 2013 ~ "Large",
                                      ves_akr_length > 59 & year > 2012 ~ "Large",
                                      ves_akr_length < 60 & year > 2012 ~ "Small",
                                      TRUE ~ as.character(ves_akr_length)),
          region = case_when(reporting_area_code == "610" ~ "E.APEN", 
                             reporting_area_code == "620" ~ "W.APEN",  
                             reporting_area_code %in% c("630") ~ "NW.GOA", 
                             reporting_area_code %in% c("640", "640 ") ~ "NE.GOA", 
                             reporting_area_code %in% c("649") ~ "NE.NW.GOA", #there are about 600 records from this stat area- need to divide them in half between NE and NW GOA, can do this based on ADFG numbers. check w ole before doing this.  
                             TRUE ~ "AI.or.BS")) %>%
  filter(!region == "AI.or.BS", !is.na(target_fishery_code), target_fishery_code == "P") %>%
   mutate(expansion_factor = sample_weight/extrapolated_weight) 
 #  filter(year > 2002) #jordan indicated somethign with other data changed in 2003, see if that is the case here
 
 #something changed with the data in 2008
#A fair amount of 0's for the weight, like the got fish and did not weigh them but were still able to extrapoalte a weight, this started in 2008, 4800 data points so pretty significant amount of data
 test <- weights %>%
filter(sample_weight == 0) #!is.na(target_fishery_code))
 test %>%
   count(year) #count the 0's
  
 
# weights %>%
#   filter(is.na(extrapolated_weight)) %>%
#   count(extrapolated_weight)
 
#by year
weights %>% 
   group_by(region, year, length_category, target_fishery_code, pscnq_processing_sector) %>%
   dplyr::summarise(expansion_factor_mean = mean(expansion_factor),
                    lowCI = ci(expansion_factor,confidence=0.90)[2],  
                    hiCI = ci(expansion_factor,confidence=0.90)[3]) %>% 
   filter(target_fishery_code == "P", length_category == "Small" ) %>%
   ggplot(aes(year,expansion_factor_mean)) +
   geom_bar(stat="identity",alpha = 0.8) +
   facet_wrap(~region, scales = "free") +
   geom_errorbar(aes(ymin=lowCI, ymax=hiCI),position="dodge", stat="identity", alpha = 0.8) + 
   ggtitle("Small Pollock Vessels") +
   theme_classic() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))

#by month 
 weights %>% 
   group_by(region, year, length_category, target, pscnq_processing_sector) %>%
   dplyr::summarise(expansion_factor_mean = mean(expansion_factor),
                    lowCI = ci(expansion_factor,confidence=0.90)[2],  
                    hiCI = ci(expansion_factor,confidence=0.90)[3]) %>% 
   filter(target_fishery_code == "P", length_category == "Large") %>%
   ggplot(aes(year,expansion_factor_mean)) +
   geom_bar(position="dodge", stat="identity",alpha = 0.8) +
   geom_errorbar(aes(ymin=lowCI, ymax=hiCI),position="dodge", stat="identity",alpha = 0.8) + 
   facet_grid(processing_sector~region, scales = "free") +
   ggtitle("Large Pollock Vessels") +
   theme_classic() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))

