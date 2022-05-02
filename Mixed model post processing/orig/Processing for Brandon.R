# Extract Data for Brandon 10-24-2016

#### # MAKING SALMON DISTRIBUTION SURFACES
library(ggplot2)
library(reshape2)
library(dplyr)

# READ IN POSTERIOR FILE FROM MODEL FIT OF INTEREST:
base.dir    <- "/Users/ole.shelton/GitHub"
results.dir <- "/Users/ole.shelton/GitHub/Orca_Salmon/Output files/_Mixed Results"
code.dir    <- "/Users/ole.shelton/GitHub/Orca_Salmon_Code/Mixed model post processing"


setwd(results.dir)
load("Binomial+Positive_output-1978-90 Troll_Rec_Treaty_6year_STATE_SPACE_E25_M2EST_originP0_vuln1_90BIGFIN_10-22-2016.RData")
#load("Binomial+Positive_output-1978-90 Troll_Rec_Treaty_6year_STATE_SPACE_E25_M2EST_originP0_vuln1_90FIN.RData")
LOCATIONS <- read.csv(paste(base.dir,"/Orca_Salmon/_Simulation and Analysis/locations.csv",sep=""))

release.summary <- REL %>% group_by(loc.numb,ocean.region) %>% summarise( N=n())
release.summary$rel.ID <- 1:nrow(release.summary)


origin_by_loc <- apply(samp$origin_loc,c(2,3,4),mean)
origin_by_loc_sd <- apply(samp$origin_loc,c(2,3,4),sd)
# rows are from, columns are to, pages are seasons
dimnames(origin_by_loc) <- list(c("Wint.Spr","Sum","Fall"),
                                  as.character(release.summary$ocean.region),
                                  as.character(LOCATIONS$location.name))
dimnames(origin_by_loc_sd) <- dimnames(origin_by_loc)

# Calculate juvenile survival in a couple of ways. 
juv.surv <- data.frame(ID= REL$ID, rel.region = REL$ocean.region,rel.year=REL$release_year,
                       Mean = apply(exp(-samp$rel_year_all),2,mean), 
                       SD = apply(exp(-samp$rel_year_all),2,sd))
juv.surv.summary <- data.frame(grand.median = median(juv.surv$Mean),
                               summarise(group_by(juv.surv,rel.region),Median=median(Mean)))
colnames(juv.surv.summary)[2:3] <- c("region","median_by_region")

# Calculate Adult survival
adult.surv <- data.frame(age.month =1:length(cum_M2),
                         cum.mortality=cum_M2,
                         cum.survival= exp(-cum_M2))

adult.surv$monthly.mort <- c(cum_M2[1],cum_M2[2:length(cum_M2)]-cum_M2[1:(length(cum_M2)-1)])
adult.surv$monthly.surv <- exp(-adult.surv$monthly.mort)

# Probability of leaving the ocean.
NOM <- spawn_loc[,c("ocean.region","loc.spawn.idx")]
NOM$ocean.region <-  as.character(NOM$ocean.region)
NOM$ocean.region[NOM$ocean.region == "SOR" |NOM$ocean.region == "COR" |NOM$ocean.region == "NOR" ] <- "OR"

NOM <- summarise(group_by(NOM,ocean.region,loc.spawn.idx),length(loc.spawn.idx))
NOM <- NOM[order(NOM$loc.spawn.idx),]

rownames(prob_age_year) <- NOM$ocean.region
colnames(prob_age_year) <- paste("age",2:6,sep=".")
  
### River entry locations
river_entry_summary <- NULL

for(i in 1:length(LOCATIONS$location.name)){
  river_entry_summary <- rbind(river_entry_summary,river_entry[which(REL$ocean.region == as.character(LOCATIONS$location.name)[i])[1],])
}
colnames(river_entry_summary) <- LOCATIONS$location.name
rownames(river_entry_summary) <- LOCATIONS$location.name


colnames(F_rec_median_mat) <- LOCATIONS$location.name
colnames(F_troll_median_mat) <- LOCATIONS$location.name

Data <- list(origin_by_loc=origin_by_loc,
             origin_by_loc_sd=origin_by_loc_sd,
             juv.surv=juv.surv,
             juv.surv.summary=juv.surv.summary,
             adult.surv=adult.surv,
             prob_age_year=prob_age_year,
             river_entry_summary=river_entry_summary,
             F_troll_median_mat = F_troll_median_mat,
             F_rec_median_mat = F_rec_median_mat)
setwd("/Users/ole.shelton/GitHub/Orca_Salmon/For Brandon 10-24-2016")  
save(Data,file=paste("Files for Brandon 10-2016.RData",sep=""))


