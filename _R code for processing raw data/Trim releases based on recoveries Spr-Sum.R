# Trim recovery files to eliminate releases that have few (< 5) or no ocean recoveries.

#### SHARED ACROSS ALL GROUPS
# Eliminate releases with fewer than 4,000 fish
REL <- REL %>% filter(N.released > 4000) %>% as.data.frame()

# This culling occurs because of the lack of in-river recoveries.
if(GROUP == "FRAM_2022_12" | GROUP == "FRAM_2023_05_TEST" | GROUP == "FRAM_2023_07"){
  # Also eliminate some Alaskan groups that have virtually no recoveries in the ocean.
  REL <- REL %>% filter(!ocean.region %in% 
                          c("CHIG_spr","KOD_spr","PWS_spr","COOK_spr","COPP_spr","YAK_spr",
                            "NSEAK_W_spr", # (Medvejie, Hidden Falls SITKA)
                            "FRAS_L_spr", # Birkenhead
                            "FRAS_L_sum",  #Chilliwack, Inch
                            "FRAS_U_sum",  #Quesnel, Chilko, Stuart
                            "HAIDA_sum",
                            "JUAN_spr"))

  # Trim by specific regions (ordered N-S)
  
  #NSEAK
  REL <- REL %>% filter(# Cull some of "NSEAK_N_spr",
                        !ID %in% c("Big_Boulder_spr",
                                   "Burro_spr",
                                   "Chilkat_hatch_spr",
                                   "Chilkat_wild_sum",
                                   "King_Salmon_R_AK_spr",
                                   "Macaulay_Chilkat_hatch_spr",
                                   "Macaulay_spr",
                                   "Myers_Chilkat_hatch_spr",
                                   "Snett_spr",
                                   "Taku_Can_Wild_spr",
                                   "Taku_wild_spr"))
  
  #SSEAK  
  REL <- REL %>% filter(# Cull some of SSEAK stocks
    !ID %in% c("Crystal_Crystal_spr",
               "Snett_Crystal_spr",
               "Chickamin_wild",
               "Deer_mtn_spr",
               "LPW_Chickamin_spr",
               "LPW_Unuk_spr",
               "Neets_spr",
               "Neets_Whitman_spr",
               "Tamgas_Unuk_spr",
               "Whitman_Chickamin_spr",
               "Whitman_spr",
               "Stikine_Can_Wild_spr",
               "Stikine_wild_spr"))
                        
  # NBC
  REL <- REL %>% filter(# Cull some of "NBC_sum and NBC_spr",
                      !ID %in% c("Babine_sum",
                                 "Kispiox_sum",
                                 "Kitsumkalum_sum",
                                 "Cedar_spr",
                                 "Morice_spr",
                                 "Nass_spr"))
  
  # CBC
  REL <- REL %>% filter(# Cull some of "CBC_sum",
            !ID %in% c("Antarko_low_sum",
                       "Chuckwalla_sea_sum",
                       "Kilbella_sea_sum",
                       "Shotbolt_sum",
                       "Snootli_Antarko_low_sum",
                       "Snootli_Antarko_up_sum",
                       "Snootli_Chuckwalla_sum",
                       "Snootli_Nusatsum_sum"))
  
  # SGEO and FRASER
  REL <- REL %>% filter(# Cull some of SGEO and FRAS,
          !ID %in% c("Clearwater_spr",
               "Deadman_spr",
               "Eagle_spr",
               "Spius_Coldwater_spr",
               "Spius_Nicola_spr",
               "Spius_Salmon_spr",
               "Spius_spr",
               "Clearwater_sum",
               "Eagle_sum",
               "Dome_spr",
               "Quesnel_Bowron_spr",
               "Quesnel_Cariboo_spr",
               "Quesnel_Chilcotin_spr",
               "Salmon_BC-F_spr",
               "Quesnel_Chilko_sum",
               "Quesnel_sum",
               "Stuart_sum",
               "Comox_sum",
               "Nanaimo_sum",
               "Porteau_sum",
               "Puntledge_sum",
               "Tenderfoot_sum"))
  
  # PUGET SOUND
  REL <- REL %>% filter(# Cull some of SGEO and FRAS,
    !ID %in% c("Kendall_Nooksack_spr",
               "Kendall_spr",
               #"Marblemount_Clark_spr_awg",
               "Hupp_spr",
               "Bernie_G_Sky_sum",
               "Bernie_G_Wallace_sum",
               "Harvey_sum"))
  
  # WAC
  
  
  # COLUMBIA
  # SNAKE
    REL <- REL %>% filter(# Cull some of SNAKE 
      !ID %in% c("Snake_low_spr",
               "Salmon_ID_spr"))
    # UCOL
    REL <- REL %>% filter(# Cull some of UCOL
      !ID %in% c("Leavenworth_wind_spr",
                 "Methow_spr",
                 "Carlton_sum",
                 "Chelan_sum",
                 "Dryden_sum",
                 "Eastbank_sum",
                 "Methow_Okanog_wild_sum",
                 "Similkameen_sum",
                 "Turtle_Rock_sum",
                 "Wells_sum"))
    # MCOL
    REL <- REL %>% filter(# Cull some of MCOL
      !ID %in% c("Ltl_White_Umatilla_spr",
                 "Parkdale_descutes_spr",
                 "Parkdale_hood_spr",
                 "Round_Butte_hood_spr"))
    
    # Willamette
    REL <- REL %>% filter(# Cull some of WILL_spr
      !ID %in% c("CEDC_pens_spr",
                 "Clackamas_spr",
                 "Dexter_will_spr",
                 "Leaburg_spr",
                 "Marion_spr",
                 "Marion_spr_awg",
                 "McKenzie_spr",
                 "S_Santiam_spr",
                 "Willamette_santiam_spr",
                 "Willamette_Will_spr"))
                 
    REL <- REL %>% filter(# Cull some of WILL_spr
      !ID %in% c("OPSR_spr",
                 "Rogue_spr"))

    ### Get rid of some outliers in terms of release timing
    
    REL <- REL %>% mutate(lab =paste0(ocean.region,"_",n.year)) %>%
            filter(!lab %in% c("NCA_spr_2","SOR_spr_2","LCOL_spr_1","SNAK_low_spr_1",
                         "UCOL_spr_1","WAC_sum_2","FRAS_TH_spr_1",
                         "SSEAK_Stikine_spr_1","NSEAK_Taku_spr_1","SFB_spr_2")) %>%
            dplyr::select(!lab)
                      
    
    
    ### FIND CATCH DATA and only include releases that have non-zero ocean recoveries.
    # catch.dat.mod <- catch.dat %>% mutate(matchy = paste(ID,brood.year,rel.year,sep="_")) %>%
    #                     filter(!fishery.type %in% c("Gillnet & Seine & Other","Terminal")) %>%
    #                     group_by(matchy) %>% summarise(Sum =sum(est.numb),count=sum(count)) %>%
    #                     arrange(Sum,matchy) %>% as.data.frame()
    #                     
    # REL <- REL %>% mutate(matchy = paste(ID,brood_year,release_year,sep="_"))
    # REL2 <- left_join(REL,catch.dat.mod) %>% filter(is.na(Sum))
    # REL <- REL %>% filter(!matchy %in% REL2$matchy) %>% dplyr::select(-matchy)
}

if(GROUP == "FRAM_2022_05"){
# Also eliminate some Alaskan groups that have virtually no recoveries in the ocean.
  REL <- REL %>% filter(!ocean.region %in% 
                          c("CHIG_spr","KOD_spr","PWS_spr"))
                          #"COPP_spr","YAK_spr","PWS_spr"))
}

####### THIS SECTION REMOVES RELEASES THAT HAVE ZERO (or close to zero) RECOVERIES.

if(GROUP == "CA+"){
  # Eliminate observations with 0 marine recoveries:  
  REL <- REL[-which(REL$ID =="Conuma" & REL$brood_year==1996),]
  REL <- REL[-which(REL$ID =="Conuma" & REL$brood_year==1997),]
  REL <- REL[-which(REL$ID =="Feather" & REL$brood_year==1992),]
  REL <- REL[-which(REL$ID =="Trinity" & REL$brood_year==1989),]
  REL <- REL[-which(REL$ID =="Coleman" & REL$brood_year==2005),]
  REL <- REL[-which(REL$ID =="Irongate" & REL$brood_year==2005),]
  REL <- REL[-which(REL$ID =="Merced" & REL$brood_year==2005),]
  REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2003),]
  REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2004),]
  REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2005),]
  REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2002),]
  REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2003),]
  REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2004),]
  
  # A <-  data.frame(apply(C,c(1,4),sum))
  # A$ID <- 1:nrow(A)
  # A <- A %>% mutate(Tot = Troll+Treaty.Troll+Sport+Gillnet...Seine...Other)
  # A <- A[order(A$Tot),]
  # THESE <- (A$ID[A$Tot<1])
  # REL[THESE,]
}


if(GROUP == "CA+COL" | GROUP =="CA+COL_AWG"){
  if( SHORT == "NO"){ # for all brood years after 1977
    # Eliminate observations with <0 marine recoveries:  
    REL <- REL[-which(REL$ID =="Conuma" & REL$brood_year==1996),]
    REL <- REL[-which(REL$ID =="Conuma" & REL$brood_year==1997),]
    
    REL <- REL[-which(REL$ID =="Feather" & REL$brood_year==1992),]
    REL <- REL[-which(REL$ID =="Trinity" & REL$brood_year==1989),]
    REL <- REL[-which(REL$ID =="Coleman" & REL$brood_year==2005),]
    REL <- REL[-which(REL$ID =="Irongate" & REL$brood_year==2005),]
    REL <- REL[-which(REL$ID =="Mad_large" & REL$brood_year==1989),]
    REL <- REL[-which(REL$ID =="Merced" & REL$brood_year==2005),]
    REL <- REL[-which(REL$ID =="Merced" & REL$brood_year==2006),]
    REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2003),]
    REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2004),]
    REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2005),]
    REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2002),]
    REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2003),]
    REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2004),]
    REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2006),]
    
    REL <- REL[-which(REL$ID =="Umatilla_small" & REL$brood_year==1994),]
    REL <- REL[-which(REL$ID =="Youngs Bay" & REL$brood_year==1991),]
    REL <- REL[-which(REL$ID =="Toutle" & REL$brood_year==1989),]
    REL <- REL[-which(REL$ID =="Toutle" & REL$brood_year==1994),]
    REL <- REL[-which(REL$ID =="Bonneville_large" & REL$brood_year==1991),]
    REL <- REL[-which(REL$ID =="Bonneville_small" & REL$brood_year==1993),]
    REL <- REL[-which(REL$ID =="Ringold" & REL$brood_year==2005),]
    
    REL <- REL[-which(REL$ID =="Irongate" & REL$brood_year==1989),]
    REL <- REL[-which(REL$ID =="Abernathy" & REL$brood_year==1995),]
    REL <- REL[-which(REL$ID =="Trinity" & REL$brood_year==2005),]
    
    REL <- REL[-which(REL$ID =="Grays_large" & REL$brood_year==1992),]
    REL <- REL[-which(REL$ID =="Grays_small" & REL$brood_year==1992),]
    REL <- REL[-which(REL$ID =="Stayton" & REL$brood_year==1991),]
    REL <- REL[-which(REL$ID =="Stayton" & REL$brood_year==1992),]
    REL <- REL[-which(REL$ID =="Irongate_large" & REL$brood_year==2005),]
    
    REL <- REL[-which(REL$ID =="Makah" & REL$brood_year==2004),]
    
    # A <-  data.frame(apply(C,c(1,4),sum))
    # A$ID <- 1:nrow(A)
    # A <- A %>% mutate(Tot = Troll+Treaty.Troll+Sport+Gillnet...Seine...Other)
    # A <- A[order(A$Tot,decreasing = F),]
    # THESE <- (A$ID[A$Tot<5])
    # REL[THESE,]
  }
}

if(GROUP == "CA+COL" | GROUP =="CA+COL_AWG"){
  if( SHORT == "YES"){ # for brood years 1977-1994
    # Eliminate observations with 0 marine recoveries:  
    REL <- REL[-which(REL$ID =="Conuma" & REL$brood_year==1996),]
    REL <- REL[-which(REL$ID =="Conuma" & REL$brood_year==1997),]
    REL <- REL[-which(REL$ID =="Feather" & REL$brood_year==1992),]
    REL <- REL[-which(REL$ID =="Trinity" & REL$brood_year==1989),]
    REL <- REL[-which(REL$ID =="Coleman" & REL$brood_year==2005),]
    REL <- REL[-which(REL$ID =="Irongate" & REL$brood_year==2005),]
    REL <- REL[-which(REL$ID =="Merced" & REL$brood_year==2005),]
    REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2003),]
    REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2004),]
    REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2005),]
    REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2002),]
    REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2003),]
    REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2004),]
    
    REL <- REL[-which(REL$ID =="Umatilla_small" & REL$brood_year==1994),]
    REL <- REL[-which(REL$ID =="Youngs Bay" & REL$brood_year==1991),]
    REL <- REL[-which(REL$ID =="Toutle" & REL$brood_year==1989),]
    REL <- REL[-which(REL$ID =="Bonneville_large" & REL$brood_year==1991),]
    REL <- REL[-which(REL$ID =="Ringold" & REL$brood_year==2005),]
    
    # A <-  data.frame(apply(C,c(1,4),sum))
    # A$ID <- 1:nrow(A)
    # A <- A %>% mutate(Tot = Troll+Treaty.Troll+Sport+Gillnet...Seine...Other)
    # A <- A[order(A$Tot),]
    # THESE <- (A$ID[A$Tot<1])
    # REL[THESE,]
  }
}


if(GROUP == "CA+COL+PUSO" ){
  if( SHORT == "NO"){ # for brood years 
    # Eliminate observations with 0 marine recoveries:  
    REL <- REL[-which(REL$ID =="Conuma" & REL$brood_year==1996),]
    REL <- REL[-which(REL$ID =="Conuma" & REL$brood_year==1997),]
    REL <- REL[-which(REL$ID =="Feather" & REL$brood_year==1992),]
    REL <- REL[-which(REL$ID =="Trinity" & REL$brood_year==1989),]
    REL <- REL[-which(REL$ID =="Coleman" & REL$brood_year==2005),]
    REL <- REL[-which(REL$ID =="Irongate" & REL$brood_year==2005),]
    REL <- REL[-which(REL$ID =="Merced" & REL$brood_year==2005),]
    REL <- REL[-which(REL$ID =="Merced" & REL$brood_year==2006),]
    REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2003),]
    REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2004),]
    REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2005),]
    REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2002),]
    REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2003),]
    REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2004),]
    REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2006),]
    
    REL <- REL[-which(REL$ID =="Umatilla_small" & REL$brood_year==1994),]
    REL <- REL[-which(REL$ID =="Youngs Bay" & REL$brood_year==1991),]
    REL <- REL[-which(REL$ID =="Toutle" & REL$brood_year==1989),]
    REL <- REL[-which(REL$ID =="Toutle" & REL$brood_year==1994),]
    REL <- REL[-which(REL$ID =="Bonneville_large" & REL$brood_year==1991),]
    REL <- REL[-which(REL$ID =="Ringold" & REL$brood_year==2005),]
    REL <- REL[-which(REL$ID =="Elwha" & REL$brood_year==2006 & REL$release_year==2007),]
    REL <- REL[-which(REL$ID =="Elwha" & REL$brood_year==2004 & REL$release_year==2005),]
    
    # Eliminate releases with fewer than 25,000 fish
    REL <- REL %>% filter(N.released > 25000) %>% as.data.frame()
    
    
    # Eliminate observations with < 5 recoveries total...
    # REL <- REL[-which(REL$ID =="Irongate" & REL$brood_year==1989),]
    # REL <- REL[-which(REL$ID =="Abernathy" & REL$brood_year==1995),]
    # REL <- REL[-which(REL$ID =="Trinity" & REL$brood_year==2005),]
    # 
    # REL <- REL[-which(REL$ID =="Grays_large" & REL$brood_year==1992),]
    # REL <- REL[-which(REL$ID =="Grays_small" & REL$brood_year==1992),]
    # REL <- REL[-which(REL$ID =="Stayton" & REL$brood_year==1991),]
    # REL <- REL[-which(REL$ID =="Stayton" & REL$brood_year==1992),]
    # REL <- REL[-which(REL$ID =="Irongate_large" & REL$brood_year==2005),]
    # REL <- REL[-which(REL$ID =="Bonneville_small" & REL$brood_year==1993),]
    # REL <- REL[-which(REL$ID =="Mad_large" & REL$brood_year==1989),]
    # 
    # REL <- REL[-which(REL$ID =="Elwha" & REL$brood_year==2003),]
    # REL <- REL[-which(REL$ID =="Elwha" & REL$brood_year==2002 & REL$release_year==2003),]
    # REL <- REL[-which(REL$ID =="Elwha" & REL$brood_year==2005 & REL$release_year==2007),]
    
    # This is some code to check for releases with very few recoveries.
    # A <-  data.frame(apply(C,c(1,4),sum))
    # A$ID <- 1:nrow(A)
    # A <- A %>% mutate(Tot = Troll+Treaty.Troll+Sport+Gillnet...Seine...Other)
    # A <- A[order(A$Tot,decreasing = F),]
    # THESE <- (A$ID[A$Tot<1])
    # REL[THESE,]
  }
}


if(GROUP == "FRAM_v1" ){
  # Eliminate observations with 0 marine recoveries:  
   REL <- REL[-which(REL$ID =="Big Creek_small" & REL$brood_year==1985),]
   REL <- REL[-which(REL$ID =="Conuma" & REL$brood_year==1996),]
   REL <- REL[-which(REL$ID =="Conuma" & REL$brood_year==1997),]
   REL <- REL[-which(REL$ID =="Bonneville_large" & REL$brood_year==1991),]
   REL <- REL[-which(REL$ID =="Coleman" & REL$brood_year==2005),] 
   REL <- REL[-which(REL$ID =="Merced" & REL$brood_year==2005),]
   REL <- REL[-which(REL$ID =="Merced" & REL$brood_year==2006),]
   REL <- REL[-which(REL$ID =="Makah" & REL$brood_year==2004),]
   REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2003),]
   REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2005),]
   REL <- REL[-which(REL$ID =="Samish" & REL$brood_year==2005),]
   REL <- REL[-which(REL$ID =="Toutle" & REL$brood_year==1989),]
   REL <- REL[-which(REL$ID =="Umatilla_large" & REL$brood_year==1994),]
   REL <- REL[-which(REL$ID =="Youngs Bay" & REL$brood_year==1991),]
   REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2003),]
   REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2006),]
   
   
   
   
  
  # REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2004),]
  # REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2002),]
  # REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2004),]

  #REL <- REL[-which(REL$ID =="Umatilla_small" & REL$brood_year==1994),]
  # REL <- REL[-which(REL$ID =="Toutle" & REL$brood_year==1994),]
  # 
  # REL <- REL[-which(REL$ID =="Ringold" & REL$brood_year==2005),]
  # REL <- REL[-which(REL$ID =="Elwha" & REL$brood_year==2006 & REL$release_year==2007),]
  # REL <- REL[-which(REL$ID =="Elwha" & REL$brood_year==2004 & REL$release_year==2005),]
  # 
  # REL <- REL[-which(REL$ID =="Makah" & REL$brood_year==2004 & REL$release_year==2005),]
  
  # Eliminate observations with < 5 recoveries total...
  # REL <- REL[-which(REL$ID =="Irongate" & REL$brood_year==1989),]
  # REL <- REL[-which(REL$ID =="Abernathy" & REL$brood_year==1995),]
  # REL <- REL[-which(REL$ID =="Trinity" & REL$brood_year==2005),]
  # 
  # REL <- REL[-which(REL$ID =="Grays_large" & REL$brood_year==1992),]
  # REL <- REL[-which(REL$ID =="Grays_small" & REL$brood_year==1992),]
  # REL <- REL[-which(REL$ID =="Stayton" & REL$brood_year==1991),]
  # REL <- REL[-which(REL$ID =="Stayton" & REL$brood_year==1992),]
  # REL <- REL[-which(REL$ID =="Irongate_large" & REL$brood_year==2005),]
  # REL <- REL[-which(REL$ID =="Bonneville_small" & REL$brood_year==1993),]
  # REL <- REL[-which(REL$ID =="Mad_large" & REL$brood_year==1989),]
  # 
  # REL <- REL[-which(REL$ID =="Elwha" & REL$brood_year==2003),]
  # REL <- REL[-which(REL$ID =="Elwha" & REL$brood_year==2002 & REL$release_year==2003),]
  # REL <- REL[-which(REL$ID =="Elwha" & REL$brood_year==2005 & REL$release_year==2007),]
  
  # # This is some code to check for releases with very few recoveries.
  # A <-  data.frame(apply(C,c(1,4),sum))
  # A$ID <- 1:nrow(A)
  # A <- A %>% mutate(Tot = Troll+Treaty.Troll+Sport+Gillnet...Seine...Other)
  # A <- A[order(A$Tot,decreasing = F),]
  # THESE <- (A$ID[A$Tot<1])
  # THESE <- (A$ID[A$Tot>10000])
  # REL[THESE,]
}

#REL<-REL.start
if(GROUP %in% c("FRAM_v2","FRAM_v3")){
  # Eliminate observations with 0 marine recoveries:  
  #REL <- REL[-which(REL$ID =="Big Creek_small" & REL$brood_year==1985),]
  REL <- REL[-which(REL$ID =="Big Creek_small" & REL$brood_year==1995),]
  REL <- REL[-which(REL$ID =="Conuma" & REL$brood_year==1996),]
  REL <- REL[-which(REL$ID =="Conuma" & REL$brood_year==1997),]
  # REL <- REL[-which(REL$ID =="Bonneville_large" & REL$brood_year==1991),]
  REL <- REL[-which(REL$ID =="Coleman" & REL$brood_year==2005),] 
  REL <- REL[-which(REL$ID =="Merced" & REL$brood_year==2005),]
  REL <- REL[-which(REL$ID =="Merced" & REL$brood_year==2006),]
  REL <- REL[-which(REL$ID =="Makah" & REL$brood_year==2004),]
  REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2003),]
  REL <- REL[-which(REL$ID =="Mokelumne" & REL$brood_year==2005),]
  REL <- REL[-which(REL$ID =="Samish" & REL$brood_year==2005),]
  REL <- REL[-which(REL$ID =="Toutle" & REL$brood_year==1989),]
  REL <- REL[-which(REL$ID =="Umatilla_large" & REL$brood_year==1994),]
  REL <- REL[-which(REL$ID =="Youngs Bay" & REL$brood_year==1991),]
  REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2002),]
  REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2003),]
  REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2004),]
  REL <- REL[-which(REL$ID =="Yuba" & REL$brood_year==2006),]
  REL <- REL[-which(REL$ID =="Ringold_white_salmon" & REL$brood_year==2005),]
  REL <- REL[-which(REL$ID =="Samish" & REL$brood_year==1997),]
  
  # < 5 group
  REL <- REL[-which(REL$ID =="Samish" & REL$brood_year==2003),]
  REL <- REL[-which(REL$ID =="Samish" & REL$brood_year==2002),]
  REL <- REL[-which(REL$ID =="Samish" & REL$brood_year==1996),]
  REL <- REL[-which(REL$ID =="White_salmon_small" & REL$brood_year==1984),]
  REL <- REL[-which(REL$ID =="Kalama" & REL$brood_year==1994),]
  REL <- REL[-which(REL$ID =="Irongate" & REL$brood_year==1989),]
  REL <- REL[-which(REL$ID =="Abernathy" & REL$brood_year==1995),]
  REL <- REL[-which(REL$ID =="Grays_large" & REL$brood_year==1992),]
  REL <- REL[-which(REL$ID =="Upper_Snake_late_large" & REL$brood_year==2006),]
  REL <- REL[-which(REL$ID =="Grays_small" & REL$brood_year==1992),]
  REL <- REL[-which(REL$ID =="Stayton" & REL$brood_year==1992),]
  REL <- REL[-which(REL$ID =="Irongate" & REL$brood_year==2005),]
  REL <- REL[-which(REL$ID =="Skagit" & REL$brood_year==1999),]
  REL <- REL[-which(REL$ID =="Stayton" & REL$brood_year==1991),]
  REL <- REL[-which(REL$ID =="Toutle" & REL$brood_year==1994),]
  REL <- REL[-which(REL$ID =="Bonneville_small" & REL$brood_year==1995),]

  # This is some code to check for releases with very few recoveries.
  # A <-  data.frame(apply(C,c(1,4),sum))
  # A$ID <- 1:nrow(A)
  # A <- A %>% mutate(Tot = Troll+Treaty.Troll+Sport+ashop+shoreside)
  # A <- A[order(A$Tot,decreasing = F),]
  # THESE <- (A$ID[A$Tot<5])
  # #THESE <- (A$ID[A$Tot>10000])
  # REL[THESE,]
}


