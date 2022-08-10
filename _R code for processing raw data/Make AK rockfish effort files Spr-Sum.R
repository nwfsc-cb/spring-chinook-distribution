### SCRIPT FOR CREATING FILES THAT CAN BE READ IN BY STAN

# Rec Effort data first
#print(base.dir)

# This is effort for the shoreside only fleet: unobserved!
rock.eff1 <- readRDS("./Processed Data/Effort Data/rockfish_shoreside_2007_2012_summarized.RDS")

# This is effort for the shoreside and CP fleet: 100% observed
rock.eff2 <- readRDS("./Processed Data/Effort Data/rockfish_shoreside_CP_2013_2020_summarized.RDS")

# rock.p <- rock.eff2 %>% mutate(dec.month = month/12, val = year + dec.month)  
# ggplot(rock.p) +
#   geom_point(aes(x=val,y=observed_boat_days,color=pscnq_processing_sector)) +
#   facet_wrap(~rec.area.code)


shore2 <- rock.eff2 %>% filter(pscnq_processing_sector == "S") %>% 
              ungroup() %>%            
              dplyr::select(-pscnq_processing_sector) %>% 
              rename(total_boat_days=observed_boat_days)

rock.shore <- bind_rows(rock.eff1, shore2)

rock.CP <-  rock.eff2 %>% filter(pscnq_processing_sector == "CP") %>% 
                ungroup() %>%            
                dplyr::select(-pscnq_processing_sector) %>% 
                rename(total_boat_days=observed_boat_days)

# aggregate months to the specified season structure.

rock.shore <- rock.shore %>% mutate(month.char = as.character(month),
                                  month.char = ifelse(nchar(month.char) ==1,paste0("0",month.char),month.char),
                                  month.char = paste0("month.",month.char))
rock.CP <- rock.CP %>% mutate(month.char = as.character(month),
                                    month.char = ifelse(nchar(month.char) ==1,paste0("0",month.char),month.char),
                                    month.char = paste0("month.",month.char))

if(MONTH.STRUCTURE=="SPRING"){
  rock.shore <- rock.shore %>% mutate(season = case_when(month.char %in% WINTER.MONTH[1:2] ~"winter.2",
                                                     month.char %in% WINTER.MONTH[3:4] ~"winter.1",
                                                     month.char %in% SPRING.MONTH ~ "month.spring",
                                                     month.char %in% SUMMER.MONTH ~ "month.summer",
                                                     month.char %in% FALL.MONTH ~ "month.fall"))

  rock.CP <- rock.CP %>% mutate(season = case_when(month.char %in% WINTER.MONTH[1:2] ~"winter.2",
                                                         month.char %in% WINTER.MONTH[3:4] ~"winter.1",
                                                         month.char %in% SPRING.MONTH ~ "month.spring",
                                                         month.char %in% SUMMER.MONTH ~ "month.summer",
                                                         month.char %in% FALL.MONTH ~ "month.fall"))
  
}

#Shoreside
effort.rock.shore <- rock.shore %>% mutate(year = ifelse(season =="winter.2",year-1,year)) %>%
  group_by(year,rec.area.code,season) %>% 
  summarise(tot.eff = sum(total_boat_days)) %>% 
  pivot_wider(., id_cols=c("year","rec.area.code"),names_from="season",values_from=c("tot.eff"),) %>%
  replace(is.na(.),0) 

if(length(grep("winter",colnames(effort.rock.shore))) == 1){
  effort.rock.shore <- effort.rock.shore %>% mutate(month.winter = winter.1)
}else if(length(grep("winter",colnames(effort.rock.shore))) == 2){
  effort.rock.shore <- effort.rock.shore %>% mutate(month.winter = winter.1 + winter.2)
}

  effort.rock.shore <-  effort.rock.shore %>%
          dplyr::select(year,
                        area.code = rec.area.code, 
                        month.winter, 
                        month.spring, 
                        month.summer, 
                        month.fall)

# CP
effort.rock.CP <- rock.CP %>% mutate(year = ifelse(season =="winter.2",year-1,year)) %>%
    group_by(year,rec.area.code,season) %>% 
    summarise(tot.eff = sum(total_boat_days)) %>% 
    pivot_wider(., id_cols=c("year","rec.area.code"),names_from="season",values_from=c("tot.eff"),) %>%
    replace(is.na(.),0) 
  
  if(length(grep("winter",colnames(effort.rock.CP))) == 1){
    effort.rock.CP <- effort.rock.CP %>% mutate(month.winter = winter.1)
  }else if(length(grep("winter",colnames(effort.rock.shore))) == 2){
    effort.rock.CP <- effort.rock.CP %>% mutate(month.winter = winter.1 + winter.2)
  }
  effort.rock.CP <-  effort.rock.CP %>%
    dplyr::select(year,
                  area.code = rec.area.code, 
                  month.winter, 
                  month.spring, 
                  month.summer, 
                  month.fall)
  
##################################################################################3
### Sampling fraction  
##################################################################################3
  # 0nly 2013 on have bycatch sampling

rock.shoreside.sample.fraction <- effort.rock.shore %>% 
                        mutate(month.winter = ifelse(month.winter >0 & year>=2013, 1, 0),
                                  month.spring = ifelse(month.spring >0 & year>=2013, 1, 0),
                                  month.summer = ifelse(month.summer >0 & year>=2013, 1, 0),
                                  month.fall   = ifelse(month.fall >0 & year>=2013, 1, 0))
  
rock.CP.sample.fraction <- effort.rock.CP %>% 
                                    mutate(month.winter = ifelse(month.winter >0 & year>=2013, 1, 0),
                                           month.spring = ifelse(month.spring >0 & year>=2013, 1, 0),
                                           month.summer = ifelse(month.summer >0 & year>=2013, 1, 0),
                                           month.fall   = ifelse(month.fall >0 & year>=2013, 1, 0))

# Merge in missing years and locations
temp<- expand.grid(year=YEARS.RECOVER,area.code=LOCATIONS$location.name)

effort.rock.shore <- left_join(temp,effort.rock.shore) %>% 
  left_join(.,LOCATIONS,by=c("area.code"="location.name")) %>% 
  rename("area.numb"="location.number")%>%
  replace(is.na(.),0)

rock.shoreside.sample.fraction <- left_join(temp,rock.shoreside.sample.fraction) %>% 
  left_join(.,LOCATIONS,by=c("area.code"="location.name")) %>% 
  rename("area.numb"="location.number") %>%
  replace(is.na(.),0)

effort.rock.CP <- left_join(temp,effort.rock.CP) %>% 
  left_join(.,LOCATIONS,by=c("area.code"="location.name")) %>% 
  rename("area.numb"="location.number")%>%
  replace(is.na(.),0)

rock.CP.sample.fraction <- left_join(temp,rock.CP.sample.fraction) %>% 
  left_join(.,LOCATIONS,by=c("area.code"="location.name")) %>% 
  rename("area.numb"="location.number") %>%
  replace(is.na(.),0)


