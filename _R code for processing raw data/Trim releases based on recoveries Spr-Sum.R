


# Trim recovery files to eliminate releases that have few (< 5) or no ocean recoveries.



#### SHARED ACROSS ALL GROUPS
# Eliminate releases with fewer than 4,000 fish
REL <- REL %>% filter(N.released > 4000) %>% as.data.frame()

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


