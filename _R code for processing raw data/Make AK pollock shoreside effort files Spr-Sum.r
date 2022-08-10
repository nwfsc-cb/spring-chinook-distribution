### SCRIPT FOR CREATING FILES THAT CAN BE READ IN BY STAN

# Rec Effort data first
#print(base.dir)


pol.eff <- readRDS("./Processed Data/Effort Data/Shoreside_Pollock_GOA_Effort_Summarized.RDS")
pol.samp <-readRDS("./Processed Data/Effort Data/Sample_Fraction_Pollock_GOA_Summarized.RDS")

pol.eff <- pol.eff %>% mutate(month.char = as.character(month),
                              month.char = ifelse(nchar(month.char) ==1,paste0("0",month.char),month.char),
                              month.char = paste0("month.",month.char))


if(MONTH.STRUCTURE=="SPRING"){
  pol.eff <- pol.eff %>% mutate(season = case_when(month.char %in% WINTER.MONTH[1:2] ~"winter.2",
                                                   month.char %in% WINTER.MONTH[3:4] ~"winter.1",
                                                   month.char %in% SPRING.MONTH ~ "month.spring",
                                                   month.char %in% SUMMER.MONTH ~ "month.summer",
                                                   month.char %in% FALL.MONTH ~ "month.fall"))
}

effort.pollock <- pol.eff %>% mutate(year = ifelse(season =="winter.2",year-1,year)) %>%
  group_by(year,rec.area.code,season) %>% 
  summarise(tot.eff = sum(total_effort)) %>% 
  pivot_wider(., id_cols=c("year","rec.area.code"),names_from="season",values_from=c("tot.eff"),) %>%
  replace(is.na(.),0) %>%
  mutate(month.winter = winter.1 + winter.2) %>%
  dplyr::select(-winter.1,-winter.2) %>% 
  dplyr::select(year,area.code = rec.area.code, month.winter, month.spring, month.summer, month.fall)
  

# Move on to sampling fractions

pol.samp <- pol.samp %>% mutate(month.char = as.character(month),
                              month.char = ifelse(nchar(month.char) ==1,paste0("0",month.char),month.char),
                              month.char = paste0("month.",month.char))

if(MONTH.STRUCTURE=="SPRING"){
  pol.samp <- pol.samp %>% mutate(season = case_when(month.char %in% WINTER.MONTH[1:2] ~"winter.2",
                                                   month.char %in% WINTER.MONTH[3:4] ~"winter.1",
                                                   month.char %in% SPRING.MONTH ~ "month.spring",
                                                   month.char %in% SUMMER.MONTH ~ "month.summer",
                                                   month.char %in% FALL.MONTH ~ "month.fall"))
}



pollock.sample.frac <- pol.samp %>% mutate(year = ifelse(season =="winter.2",year-1,year)) %>%
  group_by(year,rec.area.code,season) %>% 
  summarise(samp.frac = median(final.samp.fraction)) %>% 
  pivot_wider(., id_cols=c("year","rec.area.code"),names_from="season",values_from=c("samp.frac")) %>%
  replace(is.na(.),0) %>%
  mutate(month.winter = median(winter.1, winter.2,na.rm=T)) %>%
  dplyr::select(-winter.1,-winter.2) %>% 
  dplyr::select(year,area.code = rec.area.code, month.winter, month.spring, month.summer, month.fall)

# Merge in missing years and locations
temp<- expand.grid(year=YEARS.RECOVER,area.code=LOCATIONS$location.name)

effort.pollock <- left_join(temp,effort.pollock) %>% 
                  left_join(.,LOCATIONS,by=c("area.code"="location.name")) %>% 
                  rename("area.numb"="location.number")%>%
                  replace(is.na(.),0)

pollock.sample.fraction <- left_join(temp,pollock.sample.frac) %>% 
                       left_join(.,LOCATIONS,by=c("area.code"="location.name")) %>% 
                       rename("area.numb"="location.number") %>%
                       replace(is.na(.),0)

