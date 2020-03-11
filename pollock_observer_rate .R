library(tidyverse)
library(here)
pollock.observer.load <- read.csv("data/SALMON_RATE_JWATSON.csv")  
pollock.observer<-pollock.observer.load %>%
  rename(VESSEL_CODE = CATCHER_VESSEL_ID) %>%
  left_join(norpac, by = c("VESSEL_CODE")) 

norpac <- norpac %>%
  mutate(VESSEL_CODE= as.integer(VESSEL_CODE))


test <- pollock.observer %>%
  filter(is.na(LENGTH))
