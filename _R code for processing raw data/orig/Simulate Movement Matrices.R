#### VISUALIZE MOVEMENT MATRICES

if(N.knot == 4){

  # Make projection matrix for each 
  theta_move  = 20
  theta_spawn = 20
  
  d	<-	seq(0,20,length.out=1000)
  C.1	<-	cor.func(d,theta_move)
  #plot(C.1~d)

  # Make Covariance among knots for movement
  COV	<-	cor.func(knots.dist,theta_move)
  inv.COV	<-	solve(COV)

  # Make Covariance among knots for spawning
  COV.spawn <- array(0,dim=c(N.move.group,N_knot_spawn,N_knot_spawn))
  inv.COV.spawn <- array(0,dim=c(N.move.group,N_knot_spawn,N_knot_spawn))
  for(i in 1:N.move.group){
    COV.spawn[i,,]	  <-	cor.func(knots.dist.spawn[i,,],theta_spawn)
    inv.COV.spawn[i,,]	<-	solve(COV.spawn[i,,])
  }  
    # As long as the spacing among spawn knots is identical across release groups, can replace above with single 
        COV.spawn <- cor.func(knots.dist.spawn[i,,],theta_spawn)
        inv.COV.spawn	<-	solve(COV.spawn)
        
  #### MOVEMENT matrices 
    # Make mean vector
    W.move.true    <- c( 0, 0, 0, -3, # TO 3
                         0, 0.5, 0, 0, # To 7
                         0, 0, 0.5,  0, # To 11
                        -3, 0, 0,  0) # to 15
                        #rep(0,N.knots)

    # Make simulated knots for all of the movement matrices.
    for(i in 1:N.move.group){
      for(j in 1:N_month){
        w_move[i,,j] <- mvrnorm(1,W.move.true,COV)
      }
    }
    
    # Make mean vector
    W.spawn.int    <- c(0,0,0)
    W.spawn.slope  <- 1.0 
    #rep(0,N.knots)
    spawn.name <- paste("Y.",1:N.mod.year,sep="")
    
    # Make simulated knots for all of the spawning matrices.
    for(i in 1:N.move.spawn){
      for(j in 1:N.mod.year){
        w_spawn[i,,j] <-  W.spawn.int + W.spawn.slope * (j-1)#mvrnorm(1,W.spawn.int + W.spawn.slope * (j-1),COV.spawn)
      }
    }

  project.move <- list()
  for(i in 1:N.move.group){
    temp <- data.frame(LOC,origin=rep(move_id_idx[i],N.LOC*N.LOC))
    temp.project <- NULL
    for(j in 1:N_month){
      temp.project <- cbind(temp.project,t(cor.func(knots.to.centers,theta_move)) %*% inv.COV %*% w_move[i,,j])
    }
    colnames(temp.project) <- MONTH
    temp <- data.frame(temp,temp.project)
    project.move[[i]] <- temp
  }

    project.spawn <- list()
    for(i in 1:N.move.spawn){
      temp <- data.frame(LOC,origin=rep(move_id$loc.spawn[i],N.LOC*N.LOC))
      temp.project <- NULL
      for(j in 1:N.mod.year){
        temp.project <- t(cor.func(knots.to.centers.spawn[i,,],theta_spawn)) %*% inv.COV.spawn %*% w_spawn[i,,j]
        colnames(temp.project) <- spawn.name[j]
        temp <- data.frame(temp,temp.project)
      }
      project.spawn[[i]] <- temp
    }
  
  #########################################################################################################
  Z <- list()
  ### PLOTTING HEATMAPS of MOVEMENT MATRICES
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")

  for(i in 1: N.move.group){
    zp <- list()
      for(j in 1: N_month){
      temp <- data.frame(project.move[[i]])
      temp <- temp[,c("TO","FROM","origin",MONTH[j])]; colnames(temp)[4] <- "value"
      zp1 <- ggplot(temp,
                  aes(x = FROM, y = TO, fill = value))
      zp1 <- zp1 + geom_tile()
      zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
      zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
      zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
      zp1 <- zp1 + coord_equal()
      zp1 <- zp1 + theme_bw()

      zp[[j]] <- zp1  
    }
    names(zp) <- MONTH
    Z[[i]] <- zp
  }
    names(Z) <- paste("origin.",move_id_idx,sep="")

  ################ CONVERT TO MOVEMENT PROPORTIONS  
    for( i in 1:N.move.group){
      temp <- project.move[[i]]
      for(j in 1:N_month){
        mat <- matrix(temp[,MONTH[j]],N.LOC,N.LOC)
        mat <- exp(mat)
        for(k in 1:N.LOC){
          mat[,k] <- mat[,k] / sum(mat[,k])
        }
        project.move[[i]][,paste(MONTH[j],"prop",sep=".")] <- as.vector(mat)
      }
    }

    Z.prop <- list()    
    for(i in 1: N.move.group){
      zp <- list()
      for(j in 1: N_month){
        temp <- data.frame(project.move[[i]])
        temp <- temp[,c("TO","FROM","origin",paste(MONTH[j],"prop",sep="."))]; colnames(temp)[4] <- "value"
        zp1 <- ggplot(temp,
                    aes(x = FROM, y = TO, fill = value))
        zp1 <- zp1 + geom_tile()
        zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
        zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
        zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
        zp1 <- zp1 + coord_equal()
        zp1 <- zp1 + theme_bw()
      
        zp[[j]] <- zp1  
      }
      names(zp) <- MONTH
      Z.prop[[i]] <- zp
    }
    names(Z.prop) <- paste("origin.",move_id_idx,sep="")   
   
  # REPEAT FOR THE SPAWNING MOVEMENT MONTHS   
  Z.spawn <- list()
  ################ CONVERT TO MOVEMENT PROPORTIONS IN SPAWNING MONTH
  for( i in 1:N.move.spawn){
    temp <- project.move[[move_id$move_id_idx[i]]][,c("TO","FROM","origin","month.fall")]
    temp <- cbind(temp,temp$month.fall +  project.spawn[[i]][,grep("Y.",colnames(project.spawn[[i]]))])
    for(j in 1:N.mod.year){
      mat <- matrix(temp[,spawn.name[j]],N.LOC,N.LOC)
      mat <- exp(mat)
      for(k in 1:N.LOC){
        mat[,k] <- mat[,k] / sum(mat[,k])
      }
      project.spawn[[i]][,paste(spawn.name[j],"spawn.prop",sep=".")] <- as.vector(mat)
    }
  }
  
  Z.spawn.prop <- list()    
  for(i in 1: N.move.spawn){
    zp <- list()
    for(j in 1: N.mod.year){
      temp <- data.frame(project.spawn[[i]])
      temp <- temp[,c("TO","FROM","origin",paste(spawn.name[j],"spawn.prop",sep="."))]; colnames(temp)[4] <- "value"
      zp1 <- ggplot(temp,
                    aes(x = FROM, y = TO, fill = value))
      zp1 <- zp1 + geom_tile()
      zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
      zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
      zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
      zp1 <- zp1 + coord_equal()
      zp1 <- zp1 + theme_bw()
      zp[[j]] <- zp1  
    }
    names(zp) <- spawn.name
    Z.spawn.prop[[i]] <- zp
  }

  names(Z.spawn.prop) <- paste("origin.",move_id$loc.spawn,sep="")   
  
} # end if knot = 4 statement  

#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#### VISUALIZE MOVEMENT MATRICES

if(N.knot == 5){
  
  # Make projection matrix for each 
  theta_move  = 20
  theta_spawn = 20
  
  d	<-	seq(0,20,length.out=1000)
  C.1	<-	cor.func(d,theta_move)
  #plot(C.1~d)
  
  # Make Covariance among knots for movement
  COV	<-	cor.func(knots.dist,theta_move)
  inv.COV	<-	solve(COV)
  
  # Make Covariance among knots for spawning
  COV.spawn <- array(0,dim=c(N.move.group,N_knot_spawn,N_knot_spawn))
  inv.COV.spawn <- array(0,dim=c(N.move.group,N_knot_spawn,N_knot_spawn))
  for(i in 1:N.move.group){
    COV.spawn[i,,]	  <-	cor.func(knots.dist.spawn[i,,],theta_spawn)
    inv.COV.spawn[i,,]	<-	solve(COV.spawn[i,,])
  }  
  # As long as the spacing among spawn knots is identical across release groups, can replace above with single 
  COV.spawn <- cor.func(knots.dist.spawn[i,,],theta_spawn)
  inv.COV.spawn	<-	solve(COV.spawn)
  
  #### MOVEMENT matrices 
  # Make mean vector
  W.move.true    <- c( 0, 0,  0,  -3,  -5, # TO 2.6
                       0, 0.5, 0,  0,  -3,# To 5.8
                       0, 0, 0.5,  0, 0, # To 9.0
                      -3, 0, 0,  0.5,  0, # to 12.2
                      -5,-3, 0,   0, 0) # to 15
  #rep(0,N.knots)
  
  # Make simulated knots for all of the movement matrices.
  for(i in 1:N.move.group){
    for(j in 1:N_month){
      w_move[i,,j] <- mvrnorm(1,W.move.true,COV)
    }
  }
  
  # Make mean vector
  W.spawn.int    <- c(0,0,0)
  W.spawn.slope  <- 1.0 
  #rep(0,N.knots)
  spawn.name <- paste("Y.",1:N.mod.year,sep="")
  
  # Make simulated knots for all of the spawning matrices.
  for(i in 1:N.move.spawn){
    for(j in 1:N.mod.year){
      w_spawn[i,,j] <-  W.spawn.int + W.spawn.slope * (j-1)#mvrnorm(1,W.spawn.int + W.spawn.slope * (j-1),COV.spawn)
    }
  }
  
  project.move <- list()
  for(i in 1:N.move.group){
    temp <- data.frame(LOC,origin=rep(move_id_idx[i],N.LOC*N.LOC))
    temp.project <- NULL
    for(j in 1:N_month){
      temp.project <- cbind(temp.project,t(cor.func(knots.to.centers,theta_move)) %*% inv.COV %*% w_move[i,,j])
    }
    colnames(temp.project) <- MONTH
    temp <- data.frame(temp,temp.project)
    project.move[[i]] <- temp
  }
  
  project.spawn <- list()
  for(i in 1:N.move.spawn){
    temp <- data.frame(LOC,origin=rep(move_id$loc.spawn[i],N.LOC*N.LOC))
    temp.project <- NULL
    for(j in 1:N.mod.year){
      temp.project <- t(cor.func(knots.to.centers.spawn[i,,],theta_spawn)) %*% inv.COV.spawn %*% w_spawn[i,,j]
      colnames(temp.project) <- spawn.name[j]
      temp <- data.frame(temp,temp.project)
    }
    project.spawn[[i]] <- temp
  }
  
  #########################################################################################################
  Z <- list()
  ### PLOTTING HEATMAPS of MOVEMENT MATRICES
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
  
  for(i in 1: N.move.group){
    zp <- list()
    for(j in 1: N_month){
      temp <- data.frame(project.move[[i]])
      temp <- temp[,c("TO","FROM","origin",MONTH[j])]; colnames(temp)[4] <- "value"
      zp1 <- ggplot(temp,
                    aes(x = FROM, y = TO, fill = value))
      zp1 <- zp1 + geom_tile()
      zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
      zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
      zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
      zp1 <- zp1 + coord_equal()
      zp1 <- zp1 + theme_bw()
      
      zp[[j]] <- zp1  
    }
    names(zp) <- MONTH
    Z[[i]] <- zp
  }
  names(Z) <- paste("origin.",move_id_idx,sep="")
  
  ################ CONVERT TO MOVEMENT PROPORTIONS  
  for( i in 1:N.move.group){
    temp <- project.move[[i]]
    for(j in 1:N_month){
      mat <- matrix(temp[,MONTH[j]],N.LOC,N.LOC)
      mat <- exp(mat)
      for(k in 1:N.LOC){
        mat[,k] <- mat[,k] / sum(mat[,k])
      }
      project.move[[i]][,paste(MONTH[j],"prop",sep=".")] <- as.vector(mat)
    }
  }
  
  Z.prop <- list()    
  for(i in 1: N.move.group){
    zp <- list()
    for(j in 1: N_month){
      temp <- data.frame(project.move[[i]])
      temp <- temp[,c("TO","FROM","origin",paste(MONTH[j],"prop",sep="."))]; colnames(temp)[4] <- "value"
      zp1 <- ggplot(temp,
                    aes(x = FROM, y = TO, fill = value))
      zp1 <- zp1 + geom_tile()
      zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
      zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
      zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
      zp1 <- zp1 + coord_equal()
      zp1 <- zp1 + theme_bw()
      
      zp[[j]] <- zp1  
    }
    names(zp) <- MONTH
    Z.prop[[i]] <- zp
  }
  names(Z.prop) <- paste("origin.",move_id_idx,sep="")   
  
  # REPEAT FOR THE SPAWNING MOVEMENT MONTHS   
  Z.spawn <- list()
  ################ CONVERT TO MOVEMENT PROPORTIONS IN SPAWNING MONTH
  for( i in 1:N.move.spawn){
    temp <- project.move[[move_id$move_id_idx[i]]][,c("TO","FROM","origin","month.fall")]
    temp <- cbind(temp,temp$month.fall +  project.spawn[[i]][,grep("Y.",colnames(project.spawn[[i]]))])
    for(j in 1:N.mod.year){
      mat <- matrix(temp[,spawn.name[j]],N.LOC,N.LOC)
      mat <- exp(mat)
      for(k in 1:N.LOC){
        mat[,k] <- mat[,k] / sum(mat[,k])
      }
      project.spawn[[i]][,paste(spawn.name[j],"spawn.prop",sep=".")] <- as.vector(mat)
    }
  }
  
  Z.spawn.prop <- list()    
  for(i in 1: N.move.spawn){
    zp <- list()
    for(j in 1: N.mod.year){
      temp <- data.frame(project.spawn[[i]])
      temp <- temp[,c("TO","FROM","origin",paste(spawn.name[j],"spawn.prop",sep="."))]; colnames(temp)[4] <- "value"
      zp1 <- ggplot(temp,
                    aes(x = FROM, y = TO, fill = value))
      zp1 <- zp1 + geom_tile()
      zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
      zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
      zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
      zp1 <- zp1 + coord_equal()
      zp1 <- zp1 + theme_bw()
      zp[[j]] <- zp1  
    }
    names(zp) <- spawn.name
    Z.spawn.prop[[i]] <- zp
  }
  
  names(Z.spawn.prop) <- paste("origin.",move_id$loc.spawn,sep="")   
  
} # end if knot = 5 statement  












  
################################################################################################
  ################################################################################################
  ################################################################################################
  ### CREATE MOVEMENT ARRAY for each ocean location, month and year
  ################################################################################################
  ################################################################################################
################################################################################################

  move.array <- array(0,dim=c(N.move.group,N_month,N.LOC,N.LOC),dimnames=list(move_id_idx,MONTH,nom,nom))
  spawn.array <- array(0,dim=c(N.move.spawn,N.mod.year,N.LOC,N.LOC),dimnames=list(move_id$loc.spawn,spawn.name,nom,nom))
  
  move.all <- NULL
  spawn.all <- NULL
  for(i in 1:N.move.group){
    move.all <- rbind(move.all,project.move[[i]])
  }
  move.all  <- data.frame(move.all)
  
  for(i in 1:N.move.spawn){
    spawn.all <- rbind(spawn.all,project.spawn[[i]])
  }
  spawn.all <- data.frame(spawn.all)

  for(i in 1: N.move.group){
    move.array[i,1,,] <- move.all$month.spring.prop[move.all$origin == move_id_idx[i]]
    move.array[i,2,,] <- move.all$month.summer.prop[move.all$origin == move_id_idx[i]]
    move.array[i,4,,] <- move.all$month.winter.prop[move.all$origin == move_id_idx[i]]
  }
  
  for(i in 1: N.move.spawn){
    for(j in 1:N.mod.year){
      spawn.array[i,j,,] <- spawn.all[spawn.all$origin == move_id$loc.spawn[i],paste(spawn.name[j],"spawn.prop",sep=".")]
      #print(colSums(spawn.array[i,j,,]))
    }
  }
  
  
