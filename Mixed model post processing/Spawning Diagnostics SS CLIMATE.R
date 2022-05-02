# Spawning Diagnostic SS CLIMATE.R


colnames(D) <- paste0("age.",(1:ncol(D))+1)
colnames(D_sd) <- paste0("age.",(1:ncol(D_sd))+1,".sd")


D_dat <- REL %>% dplyr::select(ID,
                              ocean.region,
                              brood_year,
                              release_year,
                              Median.month.release,
                              N.released,
                              n.year,
                              n.month,
                              start_year) %>%
                bind_cols(.,as.data.frame(D),as.data.frame(D_sd))

D_mean <- pivot_longer(D_dat %>% dplyr::select(-contains("sd")),cols = starts_with("age."),
                            names_to = c("Age"),
                            names_pattern = "age.?(.)",
                            values_to = c("Mean"))
head(D_mean)

D_SD <- pivot_longer(D_dat,cols = ends_with(".sd"),
                       names_to = c("Age"),
                       names_pattern = "age.?(.)",
                       values_to = c("SD")) %>% dplyr::select(-contains("age."))
head(D_SD)


D_dat <- left_join(D_mean,D_SD) 
D_dat$Age <- as.numeric(D_dat$Age)
D_dat <- D_dat %>% mutate(river.year = brood_year + Age)

D_river_year_age <- D_dat %>% group_by(ocean.region,Age,river.year) %>% dplyr::summarise(total = sum(Mean))
D_river_year <- D_dat %>% group_by(ocean.region,river.year) %>% dplyr::summarise(total.river.run = sum(Mean))


D_river_year$ocean.region <- factor(D_river_year$ocean.region,levels=spawn_loc$ocean.region)


# Plot of returns
ggplot(D_river_year) +
    geom_line(aes(x=river.year,y=total.river.run/1000,group=ocean.region,color=ocean.region)) +
    facet_wrap(~ocean.region,scales="free_y")+
    ylab("Thousands")+
    theme_bw()