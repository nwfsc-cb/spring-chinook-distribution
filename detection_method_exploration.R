experimental<- dat_everything %>%
  filter(rec_year > 1998) %>%
  filter(fishery_type =="high_seas") %>%
  group_by(rec_year, fishery, detection_method) %>%
count(detection_method)


  
write.csv(experimental, "detection_method.csv" )
getwd()
