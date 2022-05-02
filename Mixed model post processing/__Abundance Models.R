#### Project abundances of Pacific salmon data from PFMC bluebook data.

# Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)

base.dir <- "/Users/ole.shelton/GitHub/"
blue.book.data.dir <- paste0(base.dir,"Salmon-Climate/blue book")
### Go get the CSVs of PFMC data.

setwd(blue.book.data.dir)
SFB <- read.csv("CV fall harvest and total size.csv")
NCA <- read.csv("Klamath fall harvest and total size.csv")
COL <- read.csv("Columbia abundances and exploitation rates.csv")
SNAK <- read.csv("Snake abundances.csv")

FIRST.YEAR <- 1983
LAST.YEAR  <- 2017
### TRIM SNAKE TO GET RID OF EARLY YEARS CLOSE TO EXINCTION.
#SNAK <- SNAK %>% filter(Year>=2000)

## Calculate the ocean exploitation rates
# SFB
# This are a range of scenarios which bracket the fraction of total catch that occurred after 
# June 1 . ALLOCATION.PROP 
ALLOCATION.PROP <- c(0.9,0.75,0.6,0.5)  
NOM <- paste0("june.allocation.",ALLOCATION.PROP)
SFB <- SFB %>% mutate(river.run= river.harvest + total.escape,
                        run.size= total.catch + river.harvest + total.escape,
                ocean.exploit.rate=total.catch / run.size)
june.allocation <- data.frame(SFB$ocean.exploit.rate %*% t(ALLOCATION.PROP))
colnames(june.allocation) <- NOM
ocean.abund <- SFB$river.run / (1-june.allocation) ; 
colnames(ocean.abund) <- paste0("ocean.abund.",ALLOCATION.PROP)

SFB <- bind_cols(SFB,june.allocation,ocean.abund)
              
# NCA 
# For NCA, I decided to do things on a age-specific basis, 
# because ocean-harvest was available that way. 
NCA  <- NCA %>% mutate(river.run.age.4.5 = river.run.age.4 + river.run.age.5)
june.allocation.3 <- NCA$harvest.rate.age.3 %*% t(ALLOCATION.PROP) 
colnames(june.allocation.3) <- paste0("june.allocation.age3.",ALLOCATION.PROP)
june.allocation.45 <- NCA$harvest.rate.age.4.5 %*% t(ALLOCATION.PROP) 
colnames(june.allocation.45) <- paste0("june.allocation.age45.",ALLOCATION.PROP)

ocean.abund.3  <- data.frame(NCA$river.run.age.3 / (1-june.allocation.3))
colnames(ocean.abund.3) <- paste0("ocean.abund.age3.",ALLOCATION.PROP)
ocean.abund.45 <- data.frame(NCA$river.run.age.4.5 / (1-june.allocation.45))
colnames(ocean.abund.45) <- paste0("ocean.abund.age45.",ALLOCATION.PROP)
ocean.abund.3.plus <- ocean.abund.3+ocean.abund.45
colnames(ocean.abund.3.plus) <- paste0("ocean.abund.3plus.",ALLOCATION.PROP)

ocean.abund <- bind_cols(ocean.abund.3,ocean.abund.45,ocean.abund.3.plus)

NCA <- bind_cols(NCA,ocean.abund)

### COLUMBIA
# Shared adjustments:
COL <- COL %>% filter(!is.na(Ocean.ER), year>= 1982) %>% mutate(ocean.er = Ocean.ER /100)


# basic plot of exploitation rates in the ocean.
ggplot(COL) +
    geom_line(aes(x=year,y=ocean.er,color=stock)) + facet_wrap(~stock)

# Expand abundance to June 1 using the allocation proportions above (ALLOCATION.PROP)

june.allocation <- COL$ocean.er  %*% t(ALLOCATION.PROP) 
colnames(june.allocation) <- paste0("june.allocation.",ALLOCATION.PROP)

ocean.abund.3  <- data.frame(COL$term.run.age.3 / (1-june.allocation))
colnames(ocean.abund.3) <- paste0("ocean.abund.age3.",ALLOCATION.PROP)
ocean.abund.4 <- data.frame(COL$term.run.age.4 / (1-june.allocation))
colnames(ocean.abund.4) <- paste0("ocean.abund.age4.",ALLOCATION.PROP)
ocean.abund.5 <- data.frame(COL$term.run.age.5 / (1-june.allocation))
colnames(ocean.abund.5) <- paste0("ocean.abund.age5.",ALLOCATION.PROP)

ocean.abund <- bind_cols(ocean.abund.3,ocean.abund.4,ocean.abund.5)

COL <- bind_cols(COL,ocean.abund)

# Separate out the URB, MCB, LCOL

LCOL <- COL %>% filter(stock %in% c("CWF","BON","SPR"))
MCOL  <-  COL %>% filter(stock %in% c("MCB"))
URB  <-  COL %>% filter(stock %in% c("URB"))

# Work with the Snake data and adjust the URB based on the SNAKE Calculations.
  # this is the conversion rate between bonneville dam and lower granite dam.  
  # So this this a little bit of a lower rate than the distance to lower monumental, but that's what I got
mean.conv.rate = SNAK$conversion.rate.BON.LG[SNAK$Year>=1988 & SNAK$Year<=2000] %>% mean()
#SNAK$mean.conv.rate = mean.conv.rate

# Get rid of NAs for the early period by replacing missing values with the long term mean.
SNAK <- SNAK %>% mutate(conversion.rate.BON.LG = ifelse(is.na(conversion.rate.BON.LG)==T,mean.conv.rate,conversion.rate.BON.LG))

SNAK <- SNAK %>% rename(year=Year) %>% 
          mutate(term.run.total = (lower.monumental.adults/conversion.rate.BON.LG)/(1-(net.term.harvest/100+sport.term.harvest/100))) %>%
          left_join(.,URB %>% dplyr::select(year,ocean.er))

june.allocation <- SNAK$ocean.er  %*% t(ALLOCATION.PROP) 
colnames(june.allocation) <- paste0("june.allocation.",ALLOCATION.PROP)

ocean.abund  <- data.frame(SNAK$term.run.total / (1-june.allocation))
colnames(ocean.abund) <- paste0("ocean.abund.",ALLOCATION.PROP)

SNAK <- bind_cols(SNAK,ocean.abund) %>% mutate(stock="SNAK")


#######################################################################
####### Choose the scenario for each river to extrapolate from
#######################################################################
# SFB first
# Fraction captured after June 1: 
FRAC.SFB = 0.6
SFB.trim <- SFB %>%  dplyr::select(year=Year,run.size,ocean.exploit.rate,grep(paste(FRAC.SFB),colnames(SFB))) %>% 

                rename(ocean.abund=grep("abund",colnames(.))) %>%
                filter(year>=FIRST.YEAR)
# NCA next
# Fraction captured after June 1: 
FRAC.NCA = 0.6
NCA.trim <- NCA %>% dplyr::select(year,river.run.size=river.run.total.adult,contains("harvest"),grep(paste(FRAC.NCA),colnames(NCA))) %>% 

                rename_at(vars(contains('plus')), funs(paste("ocean.abund"))) %>%
                filter(year>=FIRST.YEAR)
# LCOL
# Fraction captured after June 1: 
FRAC.LCOL <- 0.75 
LCOL.trim <-LCOL %>% dplyr::select(year,stock,term.run.age.3,term.run.age.4,term.run.age.5,ocean.er,grep(paste(FRAC.LCOL),colnames(LCOL))) %>% 
                 mutate(Ocean.abund=rowSums(.[grep("abund",names(.))])) %>% 
                 group_by(year) %>% summarise(ocean.abund=sum(Ocean.abund)/1000) %>%
                 filter(year>=FIRST.YEAR)

# URB
# Fraction captured after June 1: 
FRAC.URB.MCB.SNAK <- 0.75 # Shared for all of the fish upriver from Bonneville
# Do the Snake first.
SNAK.trim <- SNAK %>% dplyr::select(year,stock,term.run.total,ocean.er,grep(paste(FRAC.URB.MCB.SNAK),colnames(SNAK))) %>% 
  mutate(Ocean.abund=rowSums(.[grep("abund",names(.))])) %>% 
  group_by(year) %>% summarise(ocean.abund=sum(Ocean.abund)/1000) %>%
  filter(year>=FIRST.YEAR)

# URB (subtract off the SNAK abundances from the URB)
URB.trim <- URB %>% dplyr::select(year,stock,term.run.age.3,term.run.age.4,term.run.age.5,ocean.er,grep(paste(FRAC.URB.MCB.SNAK),colnames(URB))) %>% 
              mutate(Ocean.abund=rowSums(.[grep("abund",names(.))])) %>% 
              group_by(year) %>% summarise(ocean.abund=sum(Ocean.abund)/1000) %>% 
              left_join(.,SNAK.trim %>% rename(snake.abund = ocean.abund)) %>%
              mutate(ocean.abund.fin = ifelse(is.na(snake.abund)==F,ocean.abund - snake.abund,ocean.abund)) %>% 
              dplyr::select(-ocean.abund,-snake.abund) %>% rename(ocean.abund=ocean.abund.fin) %>%
              filter(year>=FIRST.YEAR)
# MCOL
# Fraction captured after June 1: 
MCOL.trim <- MCOL %>% dplyr::select(year,stock,term.run.age.3,term.run.age.4,term.run.age.5,ocean.er,grep(paste(FRAC.URB.MCB.SNAK),colnames(MCOL))) %>% 
                mutate(Ocean.abund=rowSums(.[grep("abund",names(.))])) %>% 
                group_by(year) %>% summarise(ocean.abund=sum(Ocean.abund)/1000) %>%
                filter(year>=FIRST.YEAR)

#### Combine all of the populations into one for plotting
all.pop <- bind_rows(NCA.trim %>% dplyr::select(year,ocean.abund) %>% mutate(stock="NCA",frac=FRAC.NCA),
                      SFB.trim %>% dplyr::select(year,ocean.abund) %>% mutate(stock="SFB",frac=FRAC.SFB),
                      LCOL.trim %>% dplyr::select(year,ocean.abund) %>% mutate(stock="LCOL",frac=FRAC.LCOL),
                      MCOL.trim %>% dplyr::select(year,ocean.abund) %>% mutate(stock="MCOL",frac=FRAC.URB.MCB.SNAK),
                      URB.trim %>% dplyr::select(year,ocean.abund) %>% mutate(stock="URB",frac=FRAC.URB.MCB.SNAK),
                      SNAK.trim %>% dplyr::select(year,ocean.abund) %>% mutate(stock="SNAK",frac=FRAC.URB.MCB.SNAK)) %>% 
            filter(year>=FIRST.YEAR,year<=LAST.YEAR) %>%
            mutate(log.ocean.abund=log(ocean.abund))

wide.pop <- pivot_wider(all.pop,id_cols = "year",names_from="stock",values_from = "ocean.abund")
wide.log.pop <- pivot_wider(all.pop,id_cols = "year",names_from="stock",values_from = "log.ocean.abund")


POP <- c("SFB","NCA","LCOL","MCOL","SNAK","URB")

mid.pop <- all.pop %>% group_by(stock) %>% summarise(Median=median(ocean.abund),Mean=mean(ocean.abund),
                                                     log.median=median(log.ocean.abund),log.Mean=mean(log.ocean.abund))
cor.pop <- cor(wide.pop %>% dplyr::select(POP) ,use="pairwise.complete.obs")
cor.log.pop <- cor(wide.log.pop %>% dplyr::select(POP) ,use="pairwise.complete.obs")
cov.pop <- cov(wide.pop %>% dplyr::select(POP) ,use="pairwise.complete.obs")
cov.log.pop <- cov(wide.log.pop %>% dplyr::select(POP) ,use="pairwise.complete.obs")
quant.pop <- apply(wide.pop[2:ncol(wide.pop)],2,quantile,probs=c(0.05,0.25,0.75,0.95),na.rm=T) %>% as.data.frame()
quant.log.pop <- apply(wide.log.pop[2:ncol(wide.log.pop)],2,quantile,probs=c(0.05,0.25,0.75,0.95),na.rm=T) %>% as.data.frame()


stock.pairs = ggpairs(wide.pop %>% dplyr::select(POP), aes(alpha = 0.4),diag = list(continuous=wrap("barDiag",binwidth=75))) + theme_bw()

stock.pairs.log = ggpairs(wide.log.pop %>% dplyr::select(POP), aes(alpha = 0.4),diag = list(continuous=wrap("barDiag",binwidth=0.5))) + theme_bw()

stock.ts = ggplot(all.pop) +
            geom_line(aes(x=year,y=ocean.abund,color=stock))+
            ylab("Ocean Abundance (thousands)")+
            scale_color_discrete("") +
            theme_bw()
print(stock.ts)

stock.log.ts = ggplot(all.pop) +
            geom_line(aes(x=year,y=log.ocean.abund,color=stock))+
            ylab("Ocean Abundance (log(thousands))")+
            scale_color_discrete("") +
            theme_bw()
print(stock.log.ts)


pdf(file=paste(base.dir,"Salmon-Climate/blue book/Plots/Observed time-series.pdf",sep=""),onefile=T)
  print(stock.ts)
  print(stock.pairs)
  print(stock.log.ts)
  print(stock.pairs.log)
dev.off()

quartz(file=paste(base.dir,"Salmon-Climate/blue book/Plots/Compare observed time-series.jpeg",sep=""),height=4,width=6,dpi=600,type="jpeg")
  print(stock.ts)
dev.off()

quartz(file=paste(base.dir,"Salmon-Climate/blue book/Plots/Compare observed pairs.jpeg",sep=""),height=7,width=7,dpi=600,type="jpeg")
  print(stock.pairs)
dev.off()

####
tot.pop.by.year <- all.pop %>% filter(year>=1982) %>% group_by(year) %>% summarise(Sum=sum(ocean.abund))

Output.abund <- list( tot.pop.by.year =tot.pop.by.year,
                      stock.pairs =stock.pairs,
                      stock.ts = stock.ts,
                      stock.pairs.log =stock.pairs.log,
                      stock.log.ts = stock.log.ts,
                      all.pop  = all.pop,
                      wide.pop = wide.pop,
                      wide.log.pop = wide.log.pop,
                      mid.pop  = mid.pop,
                      cor.pop  = cor.pop,
                      cor.log.pop  = cor.log.pop,
                      cov.pop  = cov.pop,
                      cov.log.pop  = cov.log.pop,
                      quant.pop = quant.pop,
                      quant.log.pop = quant.log.pop)
 
save(Output.abund,file="Abundance_data.RData")





