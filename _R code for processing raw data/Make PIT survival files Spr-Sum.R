# This file is for processing the PIT tag data from the Fish Passage Center
# and mapping in onto the appropriate release groups.

# This is the relevant data.
dat.pit <- pit.sar.data$dat.pit 

# make a a data.frame including all of the release groups.
dat.pit.stan <- REL %>% 
              dplyr::select(ID,ocean.region,brood_year,release_year,n.year,
                            n.month,Median.month.release, ID_numb)

#Trim to include only relevant groups from the COL 
dat.trim <- dat.pit.stan %>% filter(grepl("UCOL|MCOL|SNAK",ocean.region))
 


# map MCOL first
dat.mcol <- dat.trim %>% filter(grepl("MCOL",ocean.region)) %>%
              mutate(MigrYr=release_year,MigrYr = ifelse(n.year==1,MigrYr+1,MigrYr)) %>%
              mutate(GroupDescription =
                       case_when(ID == "Carson_spr" ~ "Carson Hatchery Spring Chinook",
                                 ID == "Cle_Elum_spr" ~ "Cle Elum Hatchery Spring Chinook",
                                 ID == "Klickitat_spr" ~ "Carson Hatchery Spring Chinook",
                                 ID == "Ltl_White_Salmon_spr" ~ "Carson Hatchery Spring Chinook",
                                 ID == "Round_Butte_spr" ~ "Warm Springs Hatchery Spring Chinook",
                                 ID == "Umatilla_spr" ~ "Cle Elum Hatchery Spring Chinook",
                                 ID == "Warm_springs_spr" ~ "Warm Springs Hatchery Spring Chinook")) %>%
            left_join(., 
                      dat.pit %>% 
                        filter(area == "MCOL",!grepl("Fall",GroupDescription)) %>%
                        dplyr::select(-SpeciesCode))                        

# map UCOL second
dat.pit %>% filter(area == "UCOL",!grepl("Fall",GroupDescription)) %>% 
            distinct(GroupDescription,SAR_Reach) 
dat.pit %>% filter(area == "UCOL",grepl("Hatch_Wild Summer Chinook",GroupDescription)) %>% 
  distinct(GroupDescription,MigrYr) 
dat.trim %>% filter(grepl("UCOL",ocean.region)) %>% distinct(ID)

dat.ucol <- dat.trim %>% filter(grepl("UCOL",ocean.region)) %>%
  mutate(MigrYr=release_year,MigrYr = ifelse(n.year==1,MigrYr+1,MigrYr)) %>%
  mutate(GroupDescription =
           case_when(ID == "Chiwawa_spr" ~ "Combined Hatch_Wild Spring Chinook tagged at Rock Island Dam",
                     ID == "Entiat_spr" ~ "Combined Hatch_Wild Spring Chinook tagged at Rock Island Dam",
                     ID == "Leavenworth_spr" ~ "Combined Hatch_Wild Spring Chinook tagged at Rock Island Dam",
                     ID == "Methow_Okanog_wild_sum_awg" ~ "Entiat and Methow River Wild Spring Chinook",
                     ID == "Similkameen_sum_awg" ~ "Combined Hatch_Wild Summer Chinook tagged at Rock Island Dam",
                     ID == "Wells_sum_awg" ~ "Combined Hatch_Wild Summer Chinook tagged at Rock Island Dam",
                     ID == "Winthrop_spr" ~ "Winthrop Hatchery Spring Chinook")) %>%
  left_join(., 
            dat.pit %>% 
              filter(area == "UCOL",!grepl("Fall",GroupDescription)) %>%
              dplyr::select(-SpeciesCode))                

# map SNAK second
dat.pit %>% filter(area == "SNAK",!grepl("Fall",GroupDescription)) %>% 
  distinct(GroupDescription,SAR_Reach) 
dat.pit %>% filter(area == "SNAK",grepl("Hatch_Wild Summer Chinook",GroupDescription)) %>% 
  distinct(GroupDescription,MigrYr) 
dat.trim %>% filter(grepl("SNAK",ocean.region)) %>% distinct(ID)

dat.snak <- dat.trim %>% filter(grepl("SNAK",ocean.region)) %>%
  mutate(MigrYr=release_year,MigrYr = ifelse(n.year==1,MigrYr+1,MigrYr)) %>%
  mutate(GroupDescription =
           case_when( ID == "Clearwater_clear_spr"~ "Clearwater Hatchery Spring Chinook",
                      ID == "Clearwater_pow_spr" ~ "Clearwater Hatchery Spring Chinook",
                      ID == "Dworshak_clearwater_spr" ~ "Dworshak Hatchery Spring Chinook",
                      ID == "Dworshak_spr"~ "Dworshak Hatchery Spring Chinook",
                      ID == "Kooskia_spr"~ "Clearwater Hatchery Spring Chinook", # because Kooskia doesn't start tagging til 2014
                      ID == "Lookingglass_Lostine_spr"~ "Catherine Creek Hatchery Spring Chinook",
                      ID == "Lookingglass_rapid_spr"~ "Catherine Creek Hatchery Spring Chinook",
                      ID == "Lookingglass_Ronde_spr"~ "Catherine Creek Hatchery Spring Chinook",
                      ID == "Lookingglass_spr"~ "Catherine Creek Hatchery Spring Chinook",
                      ID == "McCall_sum"~ "McCall Hatchery Summer Chinook",
                      ID == "Pashimeroi_sum"~ "Pahsimeroi Hatchery Summer Chinook",
                      ID == "Rapid_river_spr"~ "Rapid River Hatchery Spring Chinook")) %>%
    left_join(., 
            dat.pit %>% 
              filter(area == "SNAK",!grepl("Fall",GroupDescription)) %>%
              dplyr::select(-SpeciesCode)) 

### Trim and Combine the mcol, ucol, snak dataframes

PIT.dat.fin <- bind_rows(dat.snak,dat.ucol,dat.mcol) %>% filter(!is.na(SARwoJacks)) 

# Add an indicator variable to the REL data.frame for instances with PIT tags.1
REL <- PIT.dat.fin %>% dplyr::select(ID,ocean.region,ID_numb) %>% mutate(PIT_dat = 1) %>%
              left_join(REL,.) %>% mutate(PIT_dat = ifelse(is.na(PIT_dat)==T,0,PIT_dat))



