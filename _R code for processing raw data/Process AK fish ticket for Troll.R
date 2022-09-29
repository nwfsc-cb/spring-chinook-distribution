library(tidyverse)

# Read in the raw fish ticket data for all available years

data.dir <- "/Users/ole.shelton/Github/Orca_Salmon_DATA/Effort info/Alaska/Raw Fish Ticket Data/"

setwd(data.dir)

A <- dir()
A <- A[grep("19",A)]

early <- A[1:16]
late <- A[17:length(A)]

# pull out data from hand troll (gearn=5) and powertroll (gearn=15) from AK fish ticket data and combine into a single file.
all_troll_AK_75_90 <- NULL
for(i in 1:length(early)){
    temp <- read.csv(early[i])
    temp <- temp %>% filter(gearn %in% c(5,15))
    temp$procid <- as.character(temp$procid)
    temp$size <- as.character(temp$size)
    temp$origadfg <- as.character(temp$origadfg)
    all_troll_AK_75_90 <- bind_rows(all_troll_AK_75_90,temp)
    print(early[i])
}

all_troll_AK_91_99 <- NULL
for(i in 1:length(late)){
  temp <- read.csv(late[i])
  temp <- temp %>% filter(gearn %in% c(5,15))
  temp$tickyear <- as.character(temp$tickyear)
  # temp$size <- as.character(temp$size)
  # temp$origadfg <- as.character(temp$origadfg)
  all_troll_AK_91_99 <- bind_rows(all_troll_AK_91_99,temp)
  print(late[i])
}

# trim data to include only Chinook
cnk_troll_AK_75_90 <- all_troll_AK_75_90 %>% filter(spec == "CHNK" | specn ==410)
cnk_troll_AK_91_99 <- all_troll_AK_91_99 %>% filter(SPEC == "CHNK" | specn ==410)

# write trimmed data to file
saveRDS(cnk_troll_AK_75_90,file="Troll_Chinook_Landings_AK_75_90.rds")
saveRDS(cnk_troll_AK_91_99,file="Troll_Chinook_Landings_AK_91_99.rds")

