#in script RMIS.R after codes area assigned, some still do not get assigned, this can be used to add in those assignments. 
library(here)
library(tidyverse)

na <- read.csv("data/missing_codes.csv")

base <- world <- map_data("world") %>%  subset(region %in% c("USA", "Canada")) %>%
  filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
  filter(!long > -115) %>%
  filter(!lat < 34) %>%
  filter(!lat > 62) %>%
  filter(!long < -173) %>% 
  ggplot( ) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  coord_fixed(1.3)

base +  geom_point(data = na, mapping = aes(x = longitude, y = latitude, color = latitude)) +
  theme_bw()

na<-na %>% 
mutate(region = case_when(
  longitude > -155 & longitude < -145 ~ "PWS",
  longitude < -170 ~ "ALEUT", 
  TRUE ~ "FIX_ME")) 