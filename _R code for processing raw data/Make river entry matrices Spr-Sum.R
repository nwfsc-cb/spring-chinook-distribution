

river_entry <- matrix(0,N.REL,N.LOC)

if(SPAWN == "BLOCK"){
  for(i in 1:N.REL){
    if(REL$ocean.region[i] == "SFB"){these <- c(1,2,3)}
    if(REL$ocean.region[i] == "NCA"){these <- c(3,4,5,6)}
    if(REL$ocean.region[i] == "SOR"){these <- c(4,5,6,7)}
    if(REL$ocean.region[i] == "COR"){these <- c(4,5,6,7)}
    if(REL$ocean.region[i] == "NOR"){these <- c(5,6,7,8)}
    if(REL$ocean.region[i] == "SNAK"){these <- c(7,8,9)}
    if(REL$ocean.region[i] == "LCOL"){these <- c(7,8,9)}
    if(REL$ocean.region[i] == "MCOL"){these <- c(7,8,9)}
    if(REL$ocean.region[i] == "UCOL"){these <- c(7,8,9)}
    if(REL$ocean.region[i] == "WAC"){these  <- c(8,9,10,11,12)}
    if(REL$ocean.region[i] == "PUSO"){these <- c(9,10,11,12)}
    if(REL$ocean.region[i] == "SGEO"){these <- c(9,10,11,12,14)}
    if(REL$ocean.region[i] == "SWVI"){these <- c(9,10,11,12,13,14)}
    
    river_entry[i,these] <- 1 
  }
  if(loc_18=="TRUE"){
    river_entry <- matrix(0,N.REL,N.LOC)
    for(i in 1:N.REL){
      if(REL$ocean.region[i] == "SFB"){these <- c(1,2,3)}
      if(REL$ocean.region[i] == "NCA"){these <- c(3,4,5,6)}
      if(REL$ocean.region[i] == "SOR"){these <- c(4,5,6,7)}
      if(REL$ocean.region[i] == "COR"){these <- c(4,5,6,7)}
      if(REL$ocean.region[i] == "NOR"){these <- c(5,6,7,8)}
      if(REL$ocean.region[i] == "SNAK"){these <- c(7,8,9)}
      if(REL$ocean.region[i] == "LCOL"){these <- c(7,8,9)}
      if(REL$ocean.region[i] == "MCOL"){these <- c(7,8,9)}
      if(REL$ocean.region[i] == "UCOL"){these <- c(7,8,9)}
      if(REL$ocean.region[i] == "URB"){these <- c(7,8,9)}
      if(REL$ocean.region[i] == "WAC"){these  <- c(8,9,11,12)}
      if(REL$ocean.region[i] == "PUSO"){these <- c(9,10,11,12,13)}
      if(REL$ocean.region[i] == "SGEO"){these <- c(9,10,11,12,15)}
      if(REL$ocean.region[i] == "SWVI"){these <- c(9,11,12,13,14)}
      
      river_entry[i,these] <- 1 
    }
  }
  if(loc_18=="TWO_OR"){
    river_entry <- matrix(0,N.REL,N.LOC)
    for(i in 1:N.REL){
      if(REL$ocean.region[i] == "SFB"){these <- c(1,2,3)}
      if(REL$ocean.region[i] == "NCA"){these <- c(3,4,5)}
      if(REL$ocean.region[i] == "SOR"){these <- c(4,5,6,7)}
      #if(REL$ocean.region[i] == "COR"){these <- c(4,5,6,7)}
      if(REL$ocean.region[i] == "NOR"){these <- c(5,6,7,8)}
      if(REL$ocean.region[i] == "SNAK"){these <- c(6,7,8)}
      if(REL$ocean.region[i] == "LCOL"){these <- c(6,7,8)}
      if(REL$ocean.region[i] == "MCOL"){these <- c(6,7,8)}
      if(REL$ocean.region[i] == "UCOL"){these <- c(6,7,8)}
      if(REL$ocean.region[i] == "URB"){these  <- c(6,7,8)}
      if(REL$ocean.region[i] == "WAC"){these  <- c(7,8,10,12,13)}
      if(REL$ocean.region[i] == "PUSO_out"){these <- c(8,9,10,11,12)}
      if(REL$ocean.region[i] == "PUSO"){these <- c(8,9,10,11,12)}
      if(REL$ocean.region[i] == "SGEO"){these <- c(8,9,10,11,12,13,14)}
      if(REL$ocean.region[i] == "SWVI"){these <- c(8,10,11,12,13,14)}
      
      river_entry[i,these] <- 1 
    }
  }
  
  if(loc_18=="NCA_SOR_PUSO"){
    river_entry <- matrix(0,N.REL,N.LOC)
    for(i in 1:N.REL){
      if(REL$ocean.region[i] == "SFB"){these <- c(1,2,3)}
      if(REL$ocean.region[i] == "NCA"){these <- c(3,4,5)}
      if(REL$ocean.region[i] == "SOR"){these <- c(3,4,5,6)}
      if(REL$ocean.region[i] == "COR"){these <- c(4,5,6,7)}
      if(REL$ocean.region[i] == "NOR"){these <- c(5,6,7,8)}
      if(REL$ocean.region[i] == "SNAK"){these <- c(6,7,8)}
      if(REL$ocean.region[i] == "SAB"){these <- c(6,7,8)}
      if(REL$ocean.region[i] == "LCOL"){these <- c(6,7,8)}
      if(REL$ocean.region[i] == "MCOL"){these <- c(6,7,8)}
      if(REL$ocean.region[i] == "UCOL"){these <- c(6,7,8)}
      if(REL$ocean.region[i] == "URB"){these  <- c(6,7,8)}
      if(REL$ocean.region[i] == "WAC"){these  <- c(7,8,10,12,13)}
      if(REL$ocean.region[i] == "PUSO_out"){these <- c(8,9,10,11,12)}
      if(REL$ocean.region[i] == "PUSO_N"){these <- c(8,9,10,11,12)}
      if(REL$ocean.region[i] == "PUSO_S"){these <- c(8,9,10,11,12)}
      if(REL$ocean.region[i] == "SGEO_N"){these <- c(8,9,10,11,12,13,14)}
      if(REL$ocean.region[i] == "SGEO_S"){these <- c(8,9,10,11,12,13,14)}
      if(REL$ocean.region[i] == "SWVI"){these <- c(8,10,11,12,13,14)}
      
      river_entry[i,these] <- 1 
    }
  }
} # End BLOCK if statement

if(SPAWN=="SMOOTH"){
  if(loc_18=="NCA_SOR_PUSO"){
    river_entry <- matrix(0,N.REL,N.LOC)
    for(i in 1:N.REL){
      if(REL$ocean.region[i] == "SFB"){these <-  c(1,0,1,2,3,4,5,6, 8,7,8, 7,8,9,10,11,12)}
      if(REL$ocean.region[i] == "NCA"){these <-  c(3,2,1,0,1,2,3,4, 6,5,6, 5,6,7,8,9,10)}
      if(REL$ocean.region[i] == "SOR"){these <-  c(3,2,1,0,1,2,3,4, 6,5,6, 5,6,7,8,9,10)}
      if(REL$ocean.region[i] == "COR"){these <-  c(4,3,2,1,0,1,2,3, 5,4,5, 4,5,6,7,8,9)}
      if(REL$ocean.region[i] == "NOR"){these <-  c(5,4,3,2,1,0,1,2, 4,3,4, 3,4,5,6,7,8)}
      if(REL$ocean.region[i] == "SNAK"){these <- c(6,5,4,3,2,1,0,1, 3,2,3, 2,3,4,5,6,7)}
      if(REL$ocean.region[i] == "SAB"){these <-  c(6,5,4,3,2,1,0,1, 3,2,3, 2,3,4,5,6,7)}
      if(REL$ocean.region[i] == "LCOL"){these <- c(6,5,4,3,2,1,0,1, 3,2,3, 2,3,4,5,6,7)}
      if(REL$ocean.region[i] == "MCOL"){these <- c(6,5,4,3,2,1,0,1, 3,2,3, 2,3,4,5,6,7)}
      if(REL$ocean.region[i] == "UCOL"){these <- c(6,5,4,3,2,1,0,1, 3,2,3, 2,3,4,5,6,7)}
      if(REL$ocean.region[i] == "URB"){these <-  c(6,5,4,3,2,1,0,1, 3,2,3, 2,3,4,5,6,7)}
      if(REL$ocean.region[i] == "WAC"){these <-      c(7,6,5,4,3,2,1,0, 2,1,2, 1,2,3,4,5,6)}
      if(REL$ocean.region[i] == "PUSO_out"){these <- c(8,7,6,5,4,3,2,1, 0,0,0, 1,2,3,4,5,6)}
      if(REL$ocean.region[i] == "PUSO_N"){these <-   c(8,7,6,5,4,3,2,1, 0,0,0, 1,2,2,3,4,5)}
      if(REL$ocean.region[i] == "PUSO_S"){these <-   c(8,7,6,5,4,3,2,1, 0,0,0, 1,2,2,3,4,5)}
      if(REL$ocean.region[i] == "SGEO_N"){these <-   c(9,8,7,6,5,4,3,2, 1,0,0, 1,2,1,2,3,4)}
      if(REL$ocean.region[i] == "SGEO_S"){these <-   c(9,8,7,6,5,4,3,2, 1,0,0, 1,2,1,2,3,4)}
      if(REL$ocean.region[i] == "SWVI"){these <-     c(8,7,6,5,4,3,2,1, 2,1,2, 0,1,2,3,4,5)}
      
      river_entry[i,] <- these
    }
  }
  
  if(loc_18=="_two_OR_PUSO_AK"){
    river_entry <- matrix(0,N.REL,N.LOC)
    for(i in 1:N.REL){
      
      river_entry[i,] <- 
        case_when(REL$loc.numb[i] == 2 ~  c(1,0,1,2,3,4,5,6, 8,7,8, 7,8,9,10,11,12,13,14,15,16), #SFB stocks
                  REL$loc.numb[i] == 3 ~  c(2,1,0,1,2,3,4,6, 7,6,7, 6,7,8,9,10,11,12,13,14,15), # MEN stocks
                  REL$loc.numb[i] == 4 ~  c(3,2,1,0,1,2,3,4, 6,5,6, 5,6,7,8,9,10,11,12,13,14), # NCA stocks
                  REL$loc.numb[i] == 5 ~  c(4,3,2,1,0,1,2,3, 5,4,5, 4,5,6,7,8,9,10,11,12,13),  # SOR/COR stocks
                  REL$loc.numb[i] == 6 ~  c(5,4,3,2,1,0,1,2, 4,3,4, 3,4,5,6,7,8,9,10,11,12), # NOR stocks
                  REL$loc.numb[i] == 7 ~  c(6,5,4,3,2,1,0,1, 3,2,3, 2,3,4,5,6,7,8,9,10,11), # COL stocks
                  REL$loc.numb[i] == 8 ~  c(7,6,5,4,3,2,1,0, 2,1,2, 1,2,3,4,5,6,7,8,9,10), # WAC stocks
                  REL$loc.numb[i] == 9 ~  c(8,7,6,5,4,3,2,1, 0,0,0, 1,2,3,4,5,6,7,8,9,10), # PUSO stocks
                  REL$loc.numb[i] == 10 ~ c(8,7,6,5,4,3,2,1, 0,0,0, 1,2,3,4,5,6,7,8,9,10), # PUSO_out stocks
                  REL$loc.numb[i] == 11 ~ c(9,8,7,6,5,4,3,2, 1,0,0, 1,2,1,2,3,4,5,6,7,8), # SGEO stocks
                  REL$loc.numb[i] == 12 ~ c(8,7,6,5,4,3,2,1, 2,1,2, 0,1,2,3,4,5,6,7,8,9), #SWVI stocks
                  REL$loc.numb[i] == 13 ~ c(9,8,7,6,5,4,3,2, 3,2,2, 1,0,1,2,3,4,5,6,7,8), #NWVI stocks
                  REL$loc.numb[i] == 14 ~ c(10,9,8,7,6,5,4,3, 2,2,1, 2,1,0,1,2,3,4,5,6,7), #CBC stocks
                  REL$loc.numb[i] == 15 ~ c(11,10,9,8,7,6,5,4, 3,3,2, 3,2,1,0,1,2,3,4,5,6), #NBC stocks
                  REL$loc.numb[i] == 16 ~ c(12,11,10,9,8,7,6,5, 4,4,3, 4,3,2,1,0,1,2,3,4,5), #SSEAK stocks
                  REL$loc.numb[i] == 17 ~ c(13,12,11,10,9,8,7,6, 5,5,4, 5,4,3,2,1,0,1,2,3,4), #NSEAK stocks
                  REL$loc.numb[i] == 18 ~ c(14,13,12,11,10,9,8,7, 6,6,5, 6,5,4,3,2,1,0,1,2,3), #NEGOA stocks
                  REL$loc.numb[i] == 19 ~ c(15,14,13,12,11,10,9,8, 7,7,6, 7,6,5,4,3,2,1,0,1,2), #NWGOA stocks
                  REL$loc.numb[i] == 20 ~ c(16,15,14,13,12,11,10,9, 8,8,7, 8,7,6,5,4,3,2,1,0,1), #EAPEN stocks
                  REL$loc.numb[i] == 21 ~ c(17,16,15,14,13,12,11,10, 9,9,8, 9,8,7,6,5,4,3,2,1,0) #WAPEN stocks
        )
    }
  }
}