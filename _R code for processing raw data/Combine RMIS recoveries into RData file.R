## Combine annual recoveries files into a giant .RData file

base.dir  <- "/Users/ole.shelton/GitHub"

#setwd(paste0(base.dir,"/Orca_Salmon_DATA/Recoveries/_All Chinook"))
setwd(paste0(base.dir,"/rmis-data/data/chinook"))

NAMES <- dir()[grep("recoveries",dir())]

ALL <- NULL
for(i in 1:length(NAMES)){
  temp <- read.csv(NAMES[i])
  if(!NAMES[i]%in%c("recoveries_2011.csv","recoveries_2012.csv","recoveries_2013.csv",
                    "recoveries_2014.csv","recoveries_2015.csv","recoveries_2016.csv","recoveries_2017.csv")){
    temp <- temp %>% dplyr::select(-X)
  }
  temp <- temp %>% dplyr::select(-catch_sample_id)
  print("is_integer ==",class(temp$tag_code)=="integer")
  if(class(temp$tag_code)=="integer"){
    temp$tag_code <- as.character(temp$tag_code)
    temp$tag_code[is.na(temp$tag_code)==T] <- ""
    temp$tag_code[nchar(temp$tag_code)==5] <- paste("0",temp$tag_code[nchar(temp$tag_code)==5],sep="")
  }
  print(NAMES[i])
  print(dim(temp))
  ALL <- rbind(ALL,temp)
}

all_chinook <-list(ALL)
save(all_chinook,file="all_chinook.Rdata")
