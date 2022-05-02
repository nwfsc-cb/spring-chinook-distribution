#exploring estimation levels from dat recovery
  
#Produces PDF called estimation levels shows what estimation numbers we have for each fleet
  #DF is a dataframe that quantifies all the zeros/fleet
    #maybe also get percent zeros

library(ggplot2)
library(dplyr)
dat_recovery = read.csv("dat_recovery.csv", stringsAsFactors = FALSE)

#NOTES
#use binwidth =0.1, while graph is a little harder to read it shows where zeros are because they aren't getting binned if bindwidth = 1
#run this script and df dataframe shows how many zeros there are in each estimated numbers category

p <- ggplot(data= dat_recovery, aes(estimated_number)) +
  geom_histogram(binwidth = 1)+
  #scale_x_continuous(limits = c(0,40)) +
  facet_wrap(.~fishery_type, scales="free")+
  theme_bw()
p


# loop to plot estimated numbers for each fishery/fleet

col.filters <- unique(dat_recovery$fishery_type) 

lapply(seq_along(col.filters), function(x) {
  filter(dat_recovery, fishery_type == col.filters[x])
}
) -> df_list

names(df_list) <- col.filters

list2env(df_list, .GlobalEnv)


#plot by fishery list     
plotdf=list()

for(i in 1: length(df_list)) 
{
  df = as.data.frame(df_list[[i]])
  NAME <- unique(df$fishery_type)
  
  plotdf[[i]] <-ggplot(data= df, aes(estimated_number)) +
    geom_histogram(binwidth = 0.1)+
    #scale_x_continuous(limits = c(0,40)) +
    facet_wrap(.~fishery_name, scales="free_y",labeller=label_wrap_gen())+
    theme_bw()+
    ggtitle(paste(as.character(NAME))) 
}

print(plotdf[[7]]) 

#PRINTING WORKS BUT STOPPED IT BECAUSE I HAVE SAVED THE PDF 
        #saved in RMIS/plots

#print all above graphs to one PDF
#pdf("Estimation_numbers.pdf", width=13, height=8.5)
#for (i in 1:8) {
#  print(plotdf[[i]])
#}
#dev.off()


#___________________________________________________________________________________________
### quantify how many zeros are in each group
zdf=list()
ct_df=list()


for(i in 1: length(df_list)) 
{
  df = as.data.frame(df_list[[i]])
zdf[[i]] <- df %>%  
  filter(estimated_number == '0') %>%
  group_by(fishery_name) 
ct_df[[i]] <-count(zdf[[i]], fishery_name) 
}

#take lists into a single data frame
df <- do.call("rbind", ct_df)
df<- df %>% filter(!is.na(fishery_name))
#df shows how many zeros there are in each category  
 



