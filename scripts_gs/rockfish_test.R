library(here)
library(tidyverse)
#a significant amount of the CWT recoveries reported in the rockfish dataframe that look like they never made it to RMIS. 
#spike in recoveries in 2013 that are not in RMIS
rockfish<- read_csv("data/Rockfish CWT mod trim.csv")
rockfish_all <- read_csv("data/Rockfish CWT.csv")

rockfish_all<- rockfish_all %>%
  mutate(Long=-1*Long) %>%
  separate(recovery_date, into =c("rec_year", "rec_monthrec_day"), sep = 4) 


rmis<- read_csv("data/RMIS.csv")
rmis <- rmis %>%
  mutate(rec_year = as.character(rec_year))

highseas <- rmis %>%
  filter(fishery_type == "high_seas") %>%
  mutate(data_set="rmis")


rockfish1 <- rockfish %>%
mutate(tag_code= str_pad(tag_code, pad = "0", width = 6, side = c("left"))) %>%
separate(recovery_date, into =c("rec_year_rockfish", "rec_monthrec_day"), sep = 4) %>%
 # separate(rec_monthrec_day, into =c("rec_month", "rec_day"), sep = 2) %>%
 # filter(rec_year == "2013") %>%
filter(!rec_year_rockfish %in% c("2018","2019")) %>%
  mutate(data_set="rockfish") %>%
select(-c(fishery, detection_method))
# rename(tag_code_rockfishcwt = "tag_code")%>%
   
test<-left_join(rockfish1,highseas,by = c("recovery_id")) %>%
  mutate(source = case_when(data_set.x == "rockfish" & data_set.y=="rmis" ~ "both",
                            data_set.x == "rockfish" & is.na(data_set.y) ~ "rockfish",
                            TRUE ~ "rmis")) %>%
  mutate(rec_year = case_when(is.na(rec_year) ~ rec_year_rockfish,
                              TRUE ~ rec_year)) %>%
  separate(rec_monthrec_day, into =c("rec_month", "rec_day"), sep = 2) %>%
  select(c("source", "rec_year","rec_month", "recovery_id")) %>%
  group_by(rec_year) %>%
    count(source) %>%
  filter(!is.na(rec_year))

ggplot(test, aes(x=rec_month, y = n)) +
  geom_bar(aes(fill=factor(source, levels=c("rockfish","both"))), stat= "identity")+
  facet_wrap(~rec_year)+
 labs(fill=" ")+
  theme_classic()

#plot all counts/year just from rockfish data
ggplot(test, aes(x=rec_year, y=n)) +
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Recovery Year', y= 'Instance of Fish recovery', title = 'Rockfish CWT Recoveries- Data from jordan') +
  theme_bw()



#plot map of recoveries in rmis and in extra rockfish data
world <- map_data("world")
north_america <- subset(world, region %in% c("USA", "Canada"))

north_america %>%
  filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
  filter(!long > -115) %>%
  filter(!lat < 46) %>%
  filter(!lat > 62) %>%
  filter(!long < -173) %>%
  ggplot( ) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  coord_fixed(1.3) +
  geom_point(data=rockfish_all, aes(x=Long, y = Lat,color=rec_year)) +
  theme_classic() +
  facet_wrap(~rec_year)
