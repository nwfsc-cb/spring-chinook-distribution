library(here)
library(tidyverse)
library(ggpubr)
#plot summaries for late CWT Recoveries
  #this is from match_ak_trawl_cwt.R

#######################################################################################################
# LATE RECOVERY PLOTS
#######################################################################################################
df <- all_cwt_late %>%
  filter(processing_sector %in% c("CP", "M", "S"))

#source("match_ak_trawl_cwt.R")
uniq <- unique(df$processing_sector)
lapply(seq_along(uniq), function(x) {
  filter(df, df$processing_sector == uniq[x])
}
) -> df_list
names(df_list) <- uniq

  plot = list()
for (i in 1:length(df_list)) {
plot[[i]] <-   df_list[[i]] %>%
  filter(!length_category == "NA") %>%
  group_by(year, length_category, region) %>%
  count(trip_target_code) %>%
  ggplot(aes(x=year, y = n)) +
  geom_bar(aes(fill = trip_target_code, group = trip_target_code), stat="identity") +
  scale_fill_manual(values = c(pollock = "skyblue", other="grey")) +
  facet_grid(region~length_category# ,scales = "free_y"
             ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle(paste(as.character(uniq[[i]])))
}
  plot[[]] 
  
  
pdf("ak_cwt_plots_YEAR.pdf")
for (i in 1:3){
 plot(plot[[i]])
}
dev.off()


#by month 
plot = list()
for (i in 1:length(df_list)) {
  plot[[i]] <-   df_list[[i]] %>%
    filter(!length_category == "NA") %>%
    group_by(month, length_category, region) %>%
    count(trip_target_code) %>%
    ggplot(aes(x=month, y = n)) +
    geom_bar(aes(fill = trip_target_code, group = trip_target_code), stat="identity") +
    scale_fill_manual(values = c(pollock = "skyblue", other="grey")) +
    facet_grid(region~length_category#,scales = "free_y"
               ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(paste(as.character(uniq[[i]])))
}

pdf("ak_cwt_plots_MONTH.pdf")
for (i in 1:3){
  plot(plot[[i]])
}
dev.off()
