# # Make Catch Files

#################################################################################3
if(MONTH.STRUCTURE == "SPRING") {
  ### THIS IS SOME JUNK FOR COMBINING OBSERVATIONS IN THE WINTER MONTHS
  XX <- data.frame(cal.month = c(rep(c(MONTH.START:12,1:(MONTH.START-1)),5),MONTH.START:5), ocean.age = c(0:(length(c(rep(c(MONTH.START:12,1:(MONTH.START-1)),5),MONTH.START:5))-1))+1 )
  XX <- data.frame(XX,lab = c(rep(c("month.spring","month.spring","month.spring","month.summer","month.summer","month.fall","month.fall","month.fall","month.winter","month.winter","month.winter","month.winter"),5),
                              "month.spring","month.spring","month.spring"),
                   model.year = c(rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,12),rep(6,3)))
  YY <- data.frame(lab = c(rep(c("month.spring","month.summer","month.fall","month.winter"),5),
                           c("month.spring")),
                   model.year = c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,1)))
  YY$model.age <- 1:nrow(YY)
  
  XX <- merge(XX,YY)
  OCEAN.MODEL.AGE <- XX[order(XX$model.age),]
  model_year <- aggregate(OCEAN.MODEL.AGE$model.year,by=list(OCEAN.MODEL.AGE$model.age),max)$x
  
  ocean.recover$dat$ocean.age <- (ocean.recover$dat$rec.year - ocean.recover$dat$brood.year - 2 ) * 12 + ocean.recover$dat$rec.month - MONTH.START +1
  #ocean.recover$dat$ocean.age[ocean.recover$dat$brood.year == ocean.recover$dat$rel.year] <- 
  #        (ocean.recover$dat$rec.year[ocean.recover$dat$brood.year == ocean.recover$dat$rel.year] - ocean.recover$dat$brood.year[ocean.recover$dat$brood.year == ocean.recover$dat$rel.year] - 1 ) * 12 + 
  #        ocean.recover$dat$rec.month[ocean.recover$dat$brood.year == ocean.recover$dat$rel.year] - MONTH.START +1
  
  ocean.recover$dat$model.age <- XX$model.age[match(ocean.recover$dat$ocean.age, XX$ocean.age)]
  # This produces about 66,600 recoveries.
  
  ### Cull recoveries for very old fish and really young fish
  cull.old <- ocean.recover$dat %>% filter(ocean.age > max(OCEAN.MODEL.AGE$ocean.age))
      # You are cutting 151 recoveries of very old fish
  cull.young <- ocean.recover$dat %>% filter(ocean.age <= 0)
      # You are cutting 37 recoveries of very young fish mostly lost during juvenile sampling and a few obvious data entry errors
      # e.g. recoveries occur before releases
  ocean.recover$dat <- ocean.recover$dat %>% filter(ocean.age >0, ocean.age <=max(OCEAN.MODEL.AGE$ocean.age))
  
  # Lump Gear Types into main gear types.
  ocean.recover$dat$fishery.type<- ""
  for(i in 1:N.GEAR){
    THESE <- which(is.na(gear.func(GEAR[i],ocean.recover$dat$fishery))==F)
    ocean.recover$dat$fishery.type[THESE] <- GEAR[i]
  }  
  
  #### REPEAT WITH TRAWL FISHERIES (Hake)
  ocean.recover.trawl$dat$ocean.age <- (ocean.recover.trawl$dat$rec.year - ocean.recover.trawl$dat$brood.year - 2 ) * 12 + as.numeric(ocean.recover.trawl$dat$rec.month) - MONTH.START +1
  # ocean.recover$dat$ocean.age[ocean.recover$dat$brood.year == ocean.recover$dat$rel.year] <- 
  #   (ocean.recover$dat$rec.year[ocean.recover$dat$brood.year == ocean.recover$dat$rel.year] - ocean.recover$dat$brood.year[ocean.recover$dat$brood.year == ocean.recover$dat$rel.year] - 1 ) * 12 + 
  #   ocean.recover$dat$rec.month[ocean.recover$dat$brood.year == ocean.recover$dat$rel.year] - MONTH.START +1
  
  ocean.recover.trawl$dat$model.age <- XX$model.age[match(ocean.recover.trawl$dat$ocean.age, XX$ocean.age)]
  
  ### Cull recoveries for very old fish and really young fish
  ocean.recover.trawl$dat  <- ocean.recover.trawl$dat %>% filter(ocean.age <= max(XX$ocean.age), ocean.age >=1 )
  
  # Lump Gear Types into main gear types.
  ocean.recover.trawl$dat$fishery.type<- ocean.recover.trawl$dat$fishery
  ocean.recover.trawl$dat <- ocean.recover.trawl$dat %>% as.data.frame()
  
  #### REPEAT WITH TRAWL FISHERIES (Pollock and Rockfish - AK)
  ocean.recover.pollock.trawl$dat$ocean.age <- (ocean.recover.pollock.trawl$dat$rec.year - ocean.recover.pollock.trawl$dat$brood.year - 2 ) * 12 + as.numeric(ocean.recover.pollock.trawl$dat$rec.month) - MONTH.START +1
  ocean.recover.pollock.trawl$dat$model.age <- XX$model.age[match(ocean.recover.pollock.trawl$dat$ocean.age, XX$ocean.age)]

  ocean.recover.rockfish.AK.shoreside$dat$ocean.age <- (ocean.recover.rockfish.AK.shoreside$dat$rec.year - ocean.recover.rockfish.AK.shoreside$dat$brood.year - 2 ) * 12 + as.numeric(ocean.recover.rockfish.AK.shoreside$dat$rec.month) - MONTH.START +1
  ocean.recover.rockfish.AK.shoreside$dat$model.age <- XX$model.age[match(ocean.recover.rockfish.AK.shoreside$dat$ocean.age, XX$ocean.age)]
  
  ### Cull recoveries for very old fish and really young fish
  ocean.recover.pollock.trawl$dat  <- ocean.recover.pollock.trawl$dat %>% filter(ocean.age <= max(XX$ocean.age), ocean.age >=1 )
  ocean.recover.rockfish.AK.shoreside$dat  <- ocean.recover.rockfish.AK.shoreside$dat %>% filter(ocean.age <= max(XX$ocean.age), ocean.age >=1 )
  
  # Lump Gear Types into main gear types.
  ocean.recover.pollock.trawl$dat$fishery.type<- ocean.recover.pollock.trawl$dat$fishery
  ocean.recover.pollock.trawl$dat <- ocean.recover.pollock.trawl$dat %>% as.data.frame()

  ocean.recover.rockfish.AK.shoreside$dat$fishery.type<- ocean.recover.rockfish.AK.shoreside$dat$fishery
  ocean.recover.rockfish.AK.shoreside$dat <- ocean.recover.rockfish.AK.shoreside$dat %>% as.data.frame()
}

CALENDAR <- XX %>% arrange(ocean.age)

### THIS SECTION IS TO ADD IN MEDIAN FRACTION SAMPLED FOR EACH MISSING MEDIAN FRACTION SAMPLED IN
### THE TRAWL FISHERIES.
if(MONTH.STRUCTURE =="FOUR"){
  SPR <- c(4,5)
  SUM <- c(6,7)
  FAL <- c(8,9,10)
  WIN <- c(11,12,1,2,3)
}
if(MONTH.STRUCTURE =="FRAM"){
  SPR <- c(2,3,4)
  SUM <- c(5,6)
  FAL <- c(7,8,9)
  WIN <- c(10,11,12,1)
}
if(MONTH.STRUCTURE =="SPRING"){
  SPR <- c(3,4,5)
  SUM <- c(6,7)
  FAL <- c(8,9,10)
  WIN <- c(11,12,1,2)
}

ocean.recover.trawl$dat <- ocean.recover.trawl$dat %>% mutate(season="") %>% 
                              mutate(rec.month =as.numeric(rec.month)) %>%
                              mutate(season, season = ifelse(rec.month %in% SPR,"month.spring",season)) %>%
                              mutate(season,season = ifelse(rec.month %in% SUM,"month.summer",season)) %>%
                              mutate(season,season = ifelse(rec.month %in% FAL,"month.fall",season)) %>%
                              mutate(season,season = ifelse(rec.month %in% WIN,"month.winter",season)) %>%
                              mutate(rec.year2 =rec.year) %>% 
                              mutate(rec.year2 = ifelse(rec.month %in% WIN & rec.month<=3,rec.year2-1,rec.year2))

# ASHOP Make a flat file equivalent to Lambda that will be made for the other gear groups in the 
# Make catch and escapement files script later.
A<-    pivot_longer(ashop.sample.fraction,cols = starts_with("month"),
          names_to = "season",values_to="median.frac.samp") %>% as.data.frame()

A <- A %>% mutate(season.numb=0) %>% 
          mutate(season.numb = ifelse(season == "month.spring",1,season.numb)) %>%
          mutate(season.numb = ifelse(season == "month.summer",2,season.numb)) %>%
          mutate(season.numb = ifelse(season == "month.fall",3,season.numb)) %>%
          mutate(season.numb = ifelse(season == "month.winter",4,season.numb))

B <-  A %>% arrange(area.numb) %>%
            pivot_wider(.,
              id_cols=c("year","season","season.numb"),
              values_from="median.frac.samp",
              names_from="area.numb")
B <- B %>% arrange(year,season.numb)

Lambda_hake_ashop_flat <- B %>% as.data.frame()
rownames(Lambda_hake_ashop_flat) <- paste(B$year,B$season,sep=".")
# get rid of later than spring in final recovery year
Lambda_hake_ashop_flat <- Lambda_hake_ashop_flat %>% filter(year<max(YEARS.RECOVER)) %>% 
                          bind_rows(.,
                                    Lambda_hake_ashop_flat %>% filter(year == max(YEARS.RECOVER),season=="month.spring"))
Lambda_hake_ashop_flat <- Lambda_hake_ashop_flat %>% dplyr::select(-year,-season,-season.numb)
colnames(Lambda_hake_ashop_flat) <- paste0("loc",".",nom)

trim.B <- B %>% dplyr::select(-year,-season.numb,-season) %>% as.matrix()
trim.B.mod <- trim.B
for(i in 1:nrow(trim.B)){
  for(j in 1:ncol(trim.B)){
    if(trim.B[i,j] == 0){
      temp <- c(trim.B[i,max((j-2),1):min((j+2),N.LOC)],
                trim.B[max((i-3),1):min((i+3),nrow(trim.B)),j])
      temp <- temp[temp>0]
      if(length(temp)>0){trim.B.mod[i,j] <- mean(temp)}
    }  
  }
}

Lambda_hake_ashop_flat_int <- trim.B.mod %>% as.data.frame()
rownames(Lambda_hake_ashop_flat_int) <- paste(B$year,B$season,sep=".")
# get rid of later than spring in final recovery year
Lambda_hake_ashop_flat_int <- Lambda_hake_ashop_flat_int[1:nrow(Lambda_hake_ashop_flat),]
colnames(Lambda_hake_ashop_flat_int) <- paste0("loc",".",nom)
Lambda_hake_ashop_flat_int[is.na(Lambda_hake_ashop_flat_int)==T] <- 0

##
B.smoothed <- cbind(B %>% dplyr::select(year,season.numb),trim.B.mod)

temp2 <- pivot_longer(B.smoothed,cols=3:ncol(B.smoothed),
                names_to = "area.numb",values_to="median.frac.samp2")
temp2$area.numb <- as.integer(temp2$area.numb)

ASHOP.smoothed <- left_join(A,temp2) %>% rename(rec.area.code=area.code, rec.year2=year)
D <- left_join(ocean.recover.trawl$dat %>% filter(fishery.type=="ashop"),
                  ASHOP.smoothed %>% dplyr::select(-median.frac.samp,-season.numb))
D$est.numb2 <- D$count / D$median.frac.samp2

# Pull out only recoveries occurring after TRIM.ASHOP
D <- D %>% filter(rec.year2>TRIM.ASHOP)

ASHOP.fin <- D

# SHORESIDE HAKE
A <-pivot_longer(shoreside.sample.fraction,cols=starts_with("month"),
             names_to = "season",values_to="median.frac.samp")

A <- A %>% mutate(season.numb=0) %>% 
  mutate(season.numb = ifelse(season == "month.spring",1,season.numb)) %>%
  mutate(season.numb = ifelse(season == "month.summer",2,season.numb)) %>%
  mutate(season.numb = ifelse(season == "month.fall",3,season.numb)) %>%
  mutate(season.numb = ifelse(season == "month.winter",4,season.numb))

B <- A %>% arrange(area.numb) %>%
            pivot_wider(.,
                 id_cols=c("year","season","season.numb"),
                 values_from="median.frac.samp",
                 names_from="area.numb")
B <- B %>% arrange(year,season.numb)

Lambda_hake_shoreside_flat <- B %>% as.data.frame()
rownames(Lambda_hake_shoreside_flat) <- paste(B$year,B$season,sep=".")
Lambda_hake_shoreside_flat <- Lambda_hake_shoreside_flat %>% filter(year<max(YEARS.RECOVER)) %>% 
    bind_rows(.,Lambda_hake_shoreside_flat %>% filter(year == max(YEARS.RECOVER),season=="month.spring"))
Lambda_hake_shoreside_flat <- Lambda_hake_shoreside_flat %>% dplyr::select(-year,-season,-season.numb)
colnames(Lambda_hake_shoreside_flat) <- paste0("loc",".",nom)


trim.B <- B %>% dplyr::select(-year,-season,-season.numb) %>% as.matrix()
trim.B.mod <- trim.B
for(i in 1:nrow(trim.B)){
  for(j in 1:ncol(trim.B)){
    if(trim.B[i,j] == 0){
      temp <- c(trim.B[i,max((j-1),1):min((j+1),N.LOC)],
                trim.B[max((i-2),1):min((i+2),nrow(trim.B)),j])
      temp <- temp[temp>0]
      if(length(temp)>0){trim.B.mod[i,j] <- mean(temp)}
    }  
  }
}

Lambda_hake_shoreside_flat_int <- trim.B.mod %>% as.data.frame()
rownames(Lambda_hake_shoreside_flat_int) <- paste(B$year,B$season,sep=".")
Lambda_hake_shoreside_flat_int <- Lambda_hake_shoreside_flat_int[1:nrow(Lambda_hake_shoreside_flat),]
colnames(Lambda_hake_shoreside_flat_int) <- paste0("loc",".",nom)
Lambda_hake_shoreside_flat_int[is.na(Lambda_hake_shoreside_flat_int)==T] <- 0

B.smoothed <- cbind(B %>% dplyr::select(year,season.numb),trim.B.mod)

temp2 <- pivot_longer(B.smoothed,cols=3:ncol(B.smoothed),
             names_to = "area.numb",values_to="median.frac.samp2")

temp2$area.numb <- as.integer(temp2$area.numb)

SHORE.smoothed <- left_join(A,temp2) %>% rename(rec.area.code=area.code, rec.year2=year)
D <- left_join(ocean.recover.trawl$dat %>% filter(fishery.type=="shoreside"),
               SHORE.smoothed %>% dplyr::select(-median.frac.samp,-season.numb))
D$est.numb2 <- D$count / D$median.frac.samp2

SHORESIDE.fin <- D

# Replace raw data with modified data that reflects average 
ocean.recover.trawl$dat <- rbind(ASHOP.fin,SHORESIDE.fin) %>% dplyr::select(-est.numb,-median.frac.samp,-rec.year2,-season,-area.numb) %>%
        rename(est.numb=est.numb2,median.frac.samp=median.frac.samp2) %>% 
        dplyr::select(c("ID","brood.year","rel.year","rel.month","ocean.region","fishery","rec.year",
          "rec.month","rec.area.code","N.samp","est.numb","count","median.frac.samp",
          "ocean.age","model.age","fishery.type"))  

  # Combine trawl and directed fisheries for US west coast.
  ocean.recover$dat <- rbind(ocean.recover$dat %>% #dplyr::select(-estimation.level) %>%
                              mutate(fishery = as.character(fishery)),
                           ocean.recover.trawl$dat %>% dplyr::select(-N.samp)%>% mutate(rec.month = as.numeric(rec.month)))

  #### WORK WITH THE ALAKSA TRAWL RECOVERIES - SHORESIDE POLLOCK, ROCKFISH TRAWL.
  ocean.recover.pollock.trawl$dat <- ocean.recover.pollock.trawl$dat %>% mutate(season="") %>% 
    mutate(season, season = ifelse(rec.month %in% SPR,"month.spring",season)) %>%
    mutate(season,season = ifelse(rec.month %in% SUM,"month.summer",season)) %>%
    mutate(season,season = ifelse(rec.month %in% FAL,"month.fall",season)) %>%
    mutate(season,season = ifelse(rec.month %in% WIN,"month.winter",season))
   
  ocean.recover.rockfish.AK.shoreside$dat <- ocean.recover.rockfish.AK.shoreside$dat %>% mutate(season="") %>% 
    mutate(season, season = ifelse(rec.month %in% SPR,"month.spring",season)) %>%
    mutate(season,season = ifelse(rec.month %in% SUM,"month.summer",season)) %>%
    mutate(season,season = ifelse(rec.month %in% FAL,"month.fall",season)) %>%
    mutate(season,season = ifelse(rec.month %in% WIN,"month.winter",season))
    
  # GOA Pollock Make a flat file equivalent to Lambda that will be made for the other gear groups in the 
  # Make catch and escapement files script later.
  A <- pivot_longer(pollock.sample.fraction,cols = starts_with("month"),
                      names_to = "season",values_to="median.frac.samp") %>% as.data.frame()
  
  A <- A %>% mutate(season.numb=0) %>% 
    mutate(season.numb = ifelse(season == "month.spring",1,season.numb)) %>%
    mutate(season.numb = ifelse(season == "month.summer",2,season.numb)) %>%
    mutate(season.numb = ifelse(season == "month.fall",3,season.numb)) %>%
    mutate(season.numb = ifelse(season == "month.winter",4,season.numb))
  
  B <-  A %>% arrange(area.numb) %>%
    pivot_wider(.,
                id_cols=c("year","season","season.numb"),
                values_from="median.frac.samp",
                names_from="area.numb")
  B <- B %>% arrange(year,season.numb)
  
  Lambda_pollock_GOA_flat <- B %>% as.data.frame()
  rownames(Lambda_pollock_GOA_flat) <- paste(B$year,B$season,sep=".")
  Lambda_pollock_GOA_flat <- Lambda_pollock_GOA_flat %>% filter(year<max(YEARS.RECOVER)) %>% 
    bind_rows(.,Lambda_pollock_GOA_flat %>% filter(year == max(YEARS.RECOVER),season=="month.spring"))
  Lambda_pollock_GOA_flat <- Lambda_pollock_GOA_flat %>% dplyr::select(-year,-season,-season.numb)
  colnames(Lambda_pollock_GOA_flat) <- paste0("loc",".",nom)
  
  # GOA Rockfish Make a flat file equivalent to Lambda that will be made for the other gear groups in the 
  # Make catch and escapement files script later.
  A<-    pivot_longer(rock.shoreside.sample.fraction,cols = starts_with("month"),
                      names_to = "season",values_to="median.frac.samp") %>% as.data.frame()
  
  A <- A %>% mutate(season.numb=0) %>% 
    mutate(season.numb = ifelse(season == "month.spring",1,season.numb)) %>%
    mutate(season.numb = ifelse(season == "month.summer",2,season.numb)) %>%
    mutate(season.numb = ifelse(season == "month.fall",3,season.numb)) %>%
    mutate(season.numb = ifelse(season == "month.winter",4,season.numb))
  
  B <-  A %>% arrange(area.numb) %>%
    pivot_wider(.,
                id_cols=c("year","season","season.numb"),
                values_from="median.frac.samp",
                names_from="area.numb")
  B <- B %>% arrange(year,season.numb)
  
  Lambda_rockfish_AK_flat <- B %>% as.data.frame()
  rownames(Lambda_rockfish_AK_flat) <- paste(B$year,B$season,sep=".")
  Lambda_rockfish_AK_flat <- Lambda_rockfish_AK_flat %>% filter(year<max(YEARS.RECOVER)) %>% 
    bind_rows(.,Lambda_rockfish_AK_flat %>% filter(year == max(YEARS.RECOVER),season=="month.spring"))
  Lambda_rockfish_AK_flat <- Lambda_rockfish_AK_flat %>% dplyr::select(-year,-season,-season.numb)
  colnames(Lambda_rockfish_AK_flat) <- paste0("loc",".",nom)
  
  # Merge in AK trawl recoveries into the larger set of RMIS and southern CWT recoveries
    catch.dat <- bind_rows(ocean.recover.pollock.trawl$dat,ocean.recover.rockfish.AK.shoreside$dat) %>% dplyr::select(-season) %>%
                mutate(estimation.level=NA) %>%              
                dplyr::select(c("ID","brood.year","rel.year","rel.month","ocean.region","fishery","rec.year",
                                  "rec.month","rec.area.code","estimation.level","est.numb","count","median.frac.samp",
                                   "ocean.age","model.age","fishery.type")) %>%
                bind_rows(ocean.recover$dat,.)
      
  # Aggregate recoveries
  # catch.dat <- aggregate(ocean.recover$dat[,c("est.numb","count")],by=list(
  #   ID = ocean.recover$dat$ID, 
  #   brood.year = ocean.recover$dat$brood.year,
  #   rel.year = ocean.recover$dat$rel.year, 
  #   #  rel.month = ocean.recover$dat$rel.month, 
  #   ocean.region = ocean.recover$dat$ocean.region, 
  #   fishery.type = ocean.recover$dat$fishery.type,
  #   #  rec.year = ocean.recover$dat$rec.year, 
  #   #  rec.month = ocean.recover$dat$rec.month, 
  #   rec.area.code = ocean.recover$dat$rec.area.code, 
  #   model.age = ocean.recover$dat$model.age),
  #   sum)
  # 
  # catch.dat.frac <- aggregate(ocean.recover$dat$median.frac.samp,by=list(
  #   ID = ocean.recover$dat$ID, 
  #   brood.year = ocean.recover$dat$brood.year,
  #   rel.year = ocean.recover$dat$rel.year, 
  #   #  rel.month = ocean.recover$dat$rel.month, 
  #   ocean.region = ocean.recover$dat$ocean.region, 
  #   fishery.type = ocean.recover$dat$fishery.type ,
  #   #  rec.year = ocean.recover$dat$rec.year, 
  #   #  rec.month = ocean.recover$dat$rec.month, 
  #   rec.area.code = ocean.recover$dat$rec.area.code, 
  #   model.age = ocean.recover$dat$model.age),
  #   median)
  
  catch.dat$rec.year.mod <- catch.dat$rec.year
  
  if(MONTH.STRUCTURE=="FOUR"){
    catch.dat$rec.year.mod[catch.dat$rec.month %in% c(1,2,3)] <- catch.dat$rec.year.mod[catch.dat$rec.month %in% c(1,2,3)] - 1
  }
  if(MONTH.STRUCTURE=="FRAM"){
    catch.dat$rec.year.mod[catch.dat$rec.month == c(1)] <- catch.dat$rec.year.mod[catch.dat$rec.month== c(1)] - 1
  }
  if(MONTH.STRUCTURE=="SPRING"){
    catch.dat$rec.year.mod[catch.dat$rec.month %in% c(1,2)] <- catch.dat$rec.year.mod[catch.dat$rec.month %in% c(1,2)] - 1
  }
  
  catch.dat <- catch.dat %>% 
                group_by(ID,brood.year,rel.year,ocean.region,fishery.type,rec.area.code,model.age,rec.year.mod) %>%
                dplyr::summarize(est.numb=sum(est.numb),count=sum(count),median.frac.samp = median(median.frac.samp)) %>%
                as.data.frame()
  catch.dat$model.year <- model_year[catch.dat$model.age]
  
  N.mod.month <- max(OCEAN.MODEL.AGE$model.age)
  
  #A <- merge(catch.dat,catch.dat2,all=T)
  # colnames(catch.dat.frac)[ncol(catch.dat.frac)] <- "median.frac.samp"
  # catch.dat <- merge(catch.dat,catch.dat.frac)
  # catch.dat$model.year <- model_year[catch.dat$model.age]
  

    ######## PULL OUT OBVIOUS TERMINAL FISHERIES SO THAT THEY CAN BE ADDED TO THE ESCAPEMENT FILES LATER
  # catch.net <- catch.dat[catch.dat$fishery.type == "Gillnet & Seine & Other",]
  # catch.dat <- catch.dat[catch.dat$fishery.type != "Gillnet & Seine & Other",]

  ############### BELOW IS VESTIGIAL, IGNORE
  
  #   
  # catch.net$ocean.region  <- as.character(catch.net$ocean.region)
  # catch.net$rec.area.code <- as.character(catch.net$rec.area.code)
  # 
  # catch.pair <- catch.net[catch.net$ocean.region == catch.net$rec.area.code,]
  # catch.net  <- catch.net[catch.net$ocean.region != catch.net$rec.area.code,]
  # 
  # escape.net <- catch.pair[catch.pair$model.age == 3 |
  #                            catch.pair$model.age == 7|
  #                            catch.pair$model.age == 11|
  #                            catch.pair$model.age == 15|
  #                            catch.pair$model.age == 19,]
  # 
  # catch.temp <- catch.pair[catch.pair$model.age != 3 &
  #                            catch.pair$model.age != 7 &
  #                            catch.pair$model.age != 11 &
  #                            catch.pair$model.age != 15 &
  #                            catch.pair$model.age != 19,]
  # 
  # ### Assert that fish caught in SGEO by net fisheries for fish going to PUSO (and vice versa) were terminal 
  # temp      <- catch.net[catch.net$ocean.region == "PUSO" | catch.net$ocean.region == "SGEO",]
  # catch.net <- catch.net[catch.net$ocean.region != "PUSO" & catch.net$ocean.region != "SGEO",]
  # temp.some <- temp[temp$rec.area.code == "SGEO" | temp$rec.area.code == "PUSO",]
  # catch.net <- rbind(catch.net, temp[temp$rec.area.code != "SGEO" & temp$rec.area.code != "PUSO",])
  # 
  # escape.net <- rbind(escape.net,temp.some[temp.some$model.age == 3 |
  #                                            temp.some$model.age == 7|
  #                                            temp.some$model.age == 11|
  #                                            temp.some$model.age == 15|
  #                                            temp.some$model.age == 19,])
  # 
  # catch.net <- rbind(catch.net,temp.some[temp.some$model.age != 3 &
  #                                           temp.some$model.age != 7 &
  #                                           temp.some$model.age != 11 &
  #                                           temp.some$model.age != 15 &
  #                                           temp.some$model.age != 19,])
  # 
  # catch.net <- rbind(catch.net,catch.temp)
  # catch.dat <- rbind(catch.dat,catch.net)
  
  #### These are the important final files:
   # escape.net is the fish that are caught in (presumed) terminal fisheries in front of thier river
   # catch.net is the net fisheries catch
   #  catch.dat

#}