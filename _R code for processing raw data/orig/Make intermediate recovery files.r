# Make Catch Files

library(reshape2)

if(MONTH.STRUCTURE == "EIGHT"){
### THIS IS SOME JUNK FOR COMBINING OBSERVATIONS IN THE WINTER MONTHS
XX <- data.frame(cal.month = c(rep(c(MONTH.START:12,1:(MONTH.START-1)),4),MONTH.START + 0:5), ocean.age = c(0:(length(c(rep(c(MONTH.START:12,1:(MONTH.START-1)),4),MONTH.START + 0:5))-1)) )
XX <- data.frame(XX,lab = c(rep(c("month.05","month.06","month.07","month.08","month.09","month.10","month.wint","month.wint","month.wint","month.wint","month.wint","month.04"),4),
                            "month.05","month.06","month.07","month.08","month.09","month.10"),
                            model.year = c(rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,6)))
YY <- data.frame(lab = c(rep(c("month.05","month.06","month.07","month.08","month.09","month.10","month.wint","month.04"),4),c("month.05","month.06","month.07","month.08","month.09","month.10")),
                 model.year = c(rep(1,length(MONTH)),rep(2,length(MONTH)),rep(3,length(MONTH)),rep(4,length(MONTH)),rep(5,6)))
YY$model.age <- 1:nrow(YY)

XX <- merge(XX,YY)
OCEAN.MODEL.AGE <- XX[order(XX$model.age),]

ocean.recover$dat$ocean.age <- (ocean.recover$dat$rec.year - ocean.recover$dat$brood_yea - 2 ) * 12 + ocean.recover$dat$rec.month - MONTH.START
ocean.recover$dat$model.age <- XX$model.age[match(ocean.recover$dat$ocean.age, XX$ocean.age)]

### Cull recoveries for very old fish and really young fish
ocean.recover$dat <- ocean.recover$dat[ocean.recover$dat$ocean.age <= max(XX$ocean.age),]
ocean.recover$dat <- ocean.recover$dat[ocean.recover$dat$ocean.age >= 0,]

# Lump Gear Types into main gear types.
ocean.recover$dat$fishery.type<- ""
for(i in 1:N.GEAR){
  THESE <- which(is.na(gear.func(GEAR[i],ocean.recover$dat$fishery))==F)
  ocean.recover$dat$fishery.type[THESE] <- GEAR[i]
}

# Aggregate  recoveries
catch.dat <- aggregate(ocean.recover$dat[,c("est.numb","count")],by=list(
  ID = ocean.recover$dat$ID, 
  brood.year = ocean.recover$dat$brood.year,
  rel.year = ocean.recover$dat$rel.year, 
  #    rel.month = ocean.recover$dat$rel.month, 
  ocean.region = ocean.recover$dat$ocean.region, 
  fishery.type = ocean.recover$dat$fishery.type,
  #    rec.year = ocean.recover$dat$rec.year, 
  #  rec.month = ocean.recover$dat$rec.month, 
  rec.area.code = ocean.recover$dat$rec.area.code, 
  model.age = ocean.recover$dat$model.age),
  sum)

catch.dat.frac <- aggregate(ocean.recover$dat$median.frac.samp,by=list(
  ID = ocean.recover$dat$ID, 
  brood.year = ocean.recover$dat$brood.year,
  rel.year = ocean.recover$dat$rel.year, 
  #    rel.month = ocean.recover$dat$rel.month, 
  ocean.region = ocean.recover$dat$ocean.region, 
  fishery.type = ocean.recover$dat$fishery.type ,
  #    rec.year = ocean.recover$dat$rec.year, 
  #  rec.month = ocean.recover$dat$rec.month, 
  rec.area.code = ocean.recover$dat$rec.area.code, 
  model.age = ocean.recover$dat$model.age),
  median)

colnames(catch.dat.frac)[ncol(catch.dat.frac)] <- "median.frac.samp"
catch.dat <- merge(catch.dat,catch.dat.frac)  

N.mod.month <- max(OCEAN.MODEL.AGE$model.age)
}
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################

if(MONTH.STRUCTURE == "FOUR"){
  ### THIS IS SOME JUNK FOR COMBINING OBSERVATIONS IN THE WINTER MONTHS
  XX <- data.frame(cal.month = c(rep(c(MONTH.START:12,1:(MONTH.START-1)),4),MONTH.START + 0:6), ocean.age = c(0:(length(c(rep(c(MONTH.START:12,1:(MONTH.START-1)),4),MONTH.START + 0:6))-1)) )
  XX <- data.frame(XX,lab = c(rep(c("month.spring","month.spring","month.summer","month.summer","month.fall","month.fall","month.fall","month.winter","month.winter","month.winter","month.winter","month.winter"),4),
                              "month.spring","month.spring","month.summer","month.summer","month.fall","month.fall","month.fall"),
                   model.year = c(rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,7)))
  YY <- data.frame(lab = c(rep(c("month.spring","month.summer","month.fall","month.winter"),4),
                   c("month.spring","month.summer","month.fall")),
                   model.year = c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,3)))
  YY$model.age <- 1:nrow(YY)
  
  XX <- merge(XX,YY)
  OCEAN.MODEL.AGE <- XX[order(XX$model.age),]
  model_year <- aggregate(OCEAN.MODEL.AGE$model.year,by=list(OCEAN.MODEL.AGE$model.age),max)$x
  
  ocean.recover$dat$ocean.age <- (ocean.recover$dat$rec.year - ocean.recover$dat$brood.year - 2 ) * 12 + ocean.recover$dat$rec.month - MONTH.START
  ocean.recover$dat$model.age <- XX$model.age[match(ocean.recover$dat$ocean.age, XX$ocean.age)]
  
  ### Cull recoveries for very old fish and really young fish
  ocean.recover$dat <- ocean.recover$dat[ocean.recover$dat$ocean.age <= max(XX$ocean.age),]
  ocean.recover$dat <- ocean.recover$dat[ocean.recover$dat$ocean.age >= 0,]
  
  # Lump Gear Types into main gear types.
  ocean.recover$dat$fishery.type<- ""
  for(i in 1:N.GEAR){
    THESE <- which(is.na(gear.func(GEAR[i],ocean.recover$dat$fishery))==F)
    ocean.recover$dat$fishery.type[THESE] <- GEAR[i]
  }
  
  # Aggregate recoveries
  catch.dat <- aggregate(ocean.recover$dat[,c("est.numb","count")],by=list(
    ID = ocean.recover$dat$ID, 
    brood.year = ocean.recover$dat$brood.year,
    rel.year = ocean.recover$dat$rel.year, 
    #  rel.month = ocean.recover$dat$rel.month, 
    ocean.region = ocean.recover$dat$ocean.region, 
    fishery.type = ocean.recover$dat$fishery.type,
    #  rec.year = ocean.recover$dat$rec.year, 
    #  rec.month = ocean.recover$dat$rec.month, 
    rec.area.code = ocean.recover$dat$rec.area.code, 
    model.age = ocean.recover$dat$model.age),
    sum)
  
  catch.dat.frac <- aggregate(ocean.recover$dat$median.frac.samp,by=list(
    ID = ocean.recover$dat$ID, 
    brood.year = ocean.recover$dat$brood.year,
    rel.year = ocean.recover$dat$rel.year, 
    #  rel.month = ocean.recover$dat$rel.month, 
    ocean.region = ocean.recover$dat$ocean.region, 
    fishery.type = ocean.recover$dat$fishery.type ,
    #  rec.year = ocean.recover$dat$rec.year, 
    #  rec.month = ocean.recover$dat$rec.month, 
    rec.area.code = ocean.recover$dat$rec.area.code, 
    model.age = ocean.recover$dat$model.age),
    median)
  
  colnames(catch.dat.frac)[ncol(catch.dat.frac)] <- "median.frac.samp"
  catch.dat <- merge(catch.dat,catch.dat.frac)  
  
  N.mod.month <- max(OCEAN.MODEL.AGE$model.age)
  
  catch.dat$model.year <- model_year[catch.dat$model.age]
  
  
  ######## PULL OUT OBVIOUS TERMINAL FISHERIES SO THAT THEY CAN BE ADDED TO THE ESCAPEMENT FILES LATER
  catch.net <- catch.dat[catch.dat$fishery.type == "Gillnet & Seine & Other",]
  catch.dat <- catch.dat[catch.dat$fishery.type != "Gillnet & Seine & Other",]
  
  catch.net$ocean.region  <- as.character(catch.net$ocean.region)
  catch.net$rec.area.code <- as.character(catch.net$rec.area.code)
  
  catch.pair <- catch.net[catch.net$ocean.region == catch.net$rec.area.code,]
  catch.net  <- catch.net[catch.net$ocean.region != catch.net$rec.area.code,]
  
  escape.net <- catch.pair[catch.pair$model.age == 3 |
                             catch.pair$model.age == 7|
                             catch.pair$model.age == 11|
                             catch.pair$model.age == 15|
                             catch.pair$model.age == 19,]
  
  catch.temp <- catch.pair[catch.pair$model.age != 3 &
                             catch.pair$model.age != 7 &
                             catch.pair$model.age != 11 &
                             catch.pair$model.age != 15 &
                             catch.pair$model.age != 19,]
  
  ### Assert that fish caught in SGEO by net fisheries for fish going to PUSO (and vice versa) were terminal 
  temp      <- catch.net[catch.net$ocean.region == "PUSO" | catch.net$ocean.region == "SGEO",]
  catch.net <- catch.net[catch.net$ocean.region != "PUSO" & catch.net$ocean.region != "SGEO",]
  temp.some <- temp[temp$rec.area.code == "SGEO" | temp$rec.area.code == "PUSO",]
  catch.net <- rbind(catch.net, temp[temp$rec.area.code != "SGEO" & temp$rec.area.code != "PUSO",])
  
  escape.net <- rbind(escape.net,temp.some[temp.some$model.age == 3 |
                                             temp.some$model.age == 7|
                                             temp.some$model.age == 11|
                                             temp.some$model.age == 15|
                                             temp.some$model.age == 19,])
  
  catch.net <- rbind(catch.net,temp.some[temp.some$model.age != 3 &
                                            temp.some$model.age != 7 &
                                            temp.some$model.age != 11 &
                                            temp.some$model.age != 15 &
                                            temp.some$model.age != 19,])
  
  catch.net <- rbind(catch.net,catch.temp)
  catch.dat <- rbind(catch.dat,catch.net)
  
  #### These are the important final files:
   # escape.net is the fish that are caught in (presumed) terminal fisheries in front of thier river
   # catch.net is the net fisheries catch
   #  catch.dat

}
