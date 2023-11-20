####  IN-RIVER RECOVERY PLOTS FOR AWG AND PIT DATA.

# merge in only the AWG stocks


AWG_dat_long$ocean.region <- factor(AWG_dat_long$ocean.region,levels=ORIGIN.GROUPS$ocean.reg)

p_AWG <- ggplot(AWG_dat_long) +
  geom_point(aes(x=pred_mean_count,y=obs_count,color=ocean.region),alpha=0.3) +
  facet_wrap(~mod.year,scales="free") +
  geom_abline(intercept=0,slope=1,linetype="dashed") +
  scale_x_continuous("Predicted",trans="sqrt") +
  scale_y_continuous("Observed",trans="sqrt") +
  facet_grid(mod.year ~ocean.region,scales="free") +
  theme_bw()

OC.REG <- ORIGIN.GROUPS %>% filter(ocean.reg %in% AWG_dat_long$ocean.region) 

AWG_plot <- list()
for(i in 1:nrow(OC.REG)){
  temp <- AWG_dat_long %>% filter( ocean.region == OC.REG$ocean.reg[i])
  
  if(nrow(temp) > 0){
    AWG_plot[[OC.REG$ocean.reg[i]]] <- ggplot(temp) +
               geom_point(aes(x=pred_mean_count,y=obs_count,color=brood_year),alpha=0.8) +
               facet_grid(n.year~mod.year,scales="free") +
               geom_abline(intercept=0,slope=1,linetype="dashed") +
               scale_x_continuous("Predicted",trans="sqrt") +
               scale_y_continuous("Observed",trans="sqrt") +
               scale_color_viridis_c() +
               ggtitle(OC.REG$ocean.reg[i]) +
               theme_bw()
  }
}

pdf(file=paste(base.dir,"/spring-chinook-distribution/Output plots/","In-river AWG ALL ",NAME,".pdf",sep=""),height = 6, width=24)
  print(p_AWG)
dev.off()

pdf(file=paste(base.dir,"/spring-chinook-distribution/Output plots/","In-river AWG by stock ",NAME,".pdf",sep=""),height = 6, width=12,onefile=TRUE)
  print(AWG_plot)
dev.off()



##########################################
##########################################
##########################################
##########################################
##########################################


prop_PIT <- data.frame(Mean=apply(samp$prop_PIT,2,mean),
                       SD = apply(samp$prop_PIT,2,sd)) %>%  
  mutate(pit_idx = 1:nrow(.))
PIT.dat.fin <- left_join(PIT.dat.fin,prop_PIT)

p_PIT <- ggplot(PIT.dat.fin) + 
  geom_point(aes(x=SARwJacks/100,y=Mean,color=ocean.region)) +
  geom_abline(intercept=0,slope=1,linetype="dashed") +
  geom_errorbarh(aes(xmin=SARwJacks_LCI/100,xmax=SARwJacks_UCI/100,y=Mean),alpha=0.5) +
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD,x=SARwJacks/100),alpha=0.5) +
  scale_y_continuous(trans="sqrt") +
  scale_x_continuous(trans="sqrt") +
  facet_wrap(~ocean.region) +
  theme_bw()


pdf(file=paste(base.dir,"/spring-chinook-distribution/Output plots/","In-river PIT ",NAME,".pdf",sep=""),height = 6, width=12,onefile=TRUE)
  print(p_PIT)
dev.off()




