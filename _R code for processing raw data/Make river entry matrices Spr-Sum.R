

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
  
  if(loc_18=="TWO_OR"){
    river_entry <- matrix(0,N.REL,N.LOC)
    for(i in 1:N.REL){
      if(REL$ocean.region[i] == "SFB"){these <-  c(1,0,1,2,3,4,5,6, 8,7,8, 7,8,9,10,11,12)}
      if(REL$ocean.region[i] == "NCA"){these <-  c(3,2,1,0,1,2,3,4, 6,5,6, 5,6,7,8,9,10)}
      if(REL$ocean.region[i] == "SOR"){these <-  c(4,3,2,1,0,1,2,3, 5,4,5, 4,5,6,7,8,9)}
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
}