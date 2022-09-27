


A <-  data.frame(apply(C,c(1,4),sum))
A <- A %>% mutate(Tot = rowSums(.))
A$ID <- REL$ID
A$ID_numb <- REL$ID_numb

REL.mod <- REL %>% left_join(.,A) %>% 
  mutate(z.ind = ifelse(Tot ==0,"zero","pos"))
REL.sum <-  REL.mod %>% group_by(ocean.region) %>% 
  summarise(N.tot= length(z.ind),N.zero = Tot)
REL.sum <- full_join(REL.sum,REL.mod)


BREAKS <- c(100,1000,10000,100000,1000000)
ggplot(REL.mod) + 
  geom_histogram(aes(N.released),bins=100) +
  scale_x_continuous(trans="log",breaks=BREAKS) 

BREAKS <- c(10,100,1000,10000,100000,1000000)
ggplot(REL.mod) + 
  geom_point(aes(y=Tot,x=N.released,color=release_year)) +
  scale_x_continuous(trans="log",breaks= BREAKS) +
  scale_y_continuous(trans="log",breaks= BREAKS) +
  #  scale_color_continuous()
  facet_wrap(~ocean.region) +
  ggtitle("All ocean recoveries")

#############
B <-  data.frame(apply(C_ocean,c(1,4),sum))
B <-B %>% mutate(Tot = rowSums(.))
B$ID <- REL$ID
B$ID_numb <- REL$ID_numb

REL.mod.ocean <- REL %>% left_join(.,B) %>% 
  mutate(z.ind = ifelse(Tot ==0,"zero","pos"))
REL.sum.ocean <-  REL.mod.ocean %>% group_by(ocean.region) %>% 
  summarise(N.tot= length(z.ind),N.zero = Tot)
REL.sum.ocean <- full_join(REL.sum.ocean,REL.mod.ocean)

BREAKS <- c(100,1000,10000,100000,1000000)
ggplot(REL.mod.ocean) + 
  geom_histogram(aes(N.released),bins=100) +
  scale_x_continuous(trans="log",breaks=BREAKS) +
  geom_vline(xintercept=5000)

BREAKS <- c(10,100,1000,10000,100000,1000000)
ggplot(REL.mod.ocean) + 
  geom_point(aes(y=Tot,x=N.released,color=release_year)) +
  scale_x_continuous(trans="log",breaks= BREAKS) +
  scale_y_continuous(trans="log",breaks= BREAKS) +
  #  scale_color_continuous()
  facet_wrap(~ocean.region) +
  ggtitle("Select ocean recoveries (no nets, no terminal)")



A <- A[order(A$Tot,decreasing = F),]

# WHAT IF I CUT OUT THE RELEASES < 10K

REL.small <- REL.mod %>% filter(N.released < 10000)
REL.lg <- REL.mod %>% filter(N.released >= 10000)

DD <- REL.small %>% group_by(ocean.region) %>% summarise(N.sm = length(ocean.region))
EE <- REL.lg %>% group_by(ocean.region) %>% summarise(N.lg = length(ocean.region))

full_join(DD,EE) %>% as.data.frame()
