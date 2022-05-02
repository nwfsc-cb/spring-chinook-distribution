#########################################################################################
###################
# DEFINE GROUPS OF RELEASE LOCATIONS FOR ADULT MOVEMENT
###################
#########################################################################################
#####
X <- 1:N.LOC
loc.1 <- expand.grid(TO=X,FROM=X)

## MAKE A DIAGONAL-BIASED movement matrix.

D <- seq(1+0.5,N.LOC-0.5,length.out=8)
E <- NULL
for(i in 2:length(D)){
  E[i-1] <- mean(D[(i-1):i])
}

dist.perp <- 1.5
E.x <- E - dist.perp
E.y <- E + dist.perp

start.x <- 8
temp.y  <- start.x+(seq(1,N.LOC-start.x,length.out=4))
temp.x  <- (seq(1,N.LOC-start.x,length.out=4))
F.x <- NULL
F.y <- NULL
for(i in 2:length(temp.x)){
      F.x[i-1]  <- mean(temp.x[(i-1):i])
      F.y[i-1]  <- mean(temp.y[(i-1):i])
}

KNOT.temp <- cbind(c(D,E.x,E.y,F.x,F.y,1.5,N.LOC+0.5),c(D,E.y,E.x,F.y,F.x,N.LOC+0.5,1.5))
plot(KNOT.temp[,1],KNOT.temp[,2])

KNOT.loc <- data.frame(KNOT.temp)
colnames(KNOT.loc) <- c("TO","FROM")
KNOT.loc$prior.mean <- c(rep(0,length(D)),rep(0,2*length(E.x)),rep(-3,2*length(F.x)),-5,-5)

KNOT.loc	<-	KNOT.loc[order(KNOT.loc$TO,KNOT.loc$FROM),]
N.knots	<-	nrow(KNOT.loc)

# L		<- (N.LOC-1)/(N.knot)
# 
# # This is for knots not on the edges (can cause troubles for corners...)
# KNOT	<-	NULL
# for(i in 1:N.knot){
#   if(i==1){	KNOT[i]	<-	1 + L/2}
#   if(i > 1 ){ KNOT[i]= KNOT[i-1]+L}
# }
# 
# ### TRY SPACING KNOTS with end point at the southern and northern edges
# KNOT <- seq(1,N.LOC,length.out=N.knot)
# 
# KNOT.loc	<-	expand.grid(TO=KNOT,FROM=KNOT)
# KNOT.loc	<-	KNOT.loc[order(KNOT.loc$TO,KNOT.loc$FROM),]

cor.func	<-	function(D,THETA){ # D is a distance matrix
  R.1	<-	exp(-D^2*(1/THETA))
  return(R.1)} 

### MAKE KNOTS
km.knots			<-	data.frame(KNOT.loc[,c("TO","FROM")])
LOC <- loc.1

# Stuff for spawning matrices (also defines the number of movement matrices we need.)
N_knot_spawn <- 5
#########################################################################################
### MAKE A DISTANCE MATRIX FOR THE KNOTS to each location of interest.
#########################################################################################
knots.dist		  <-	as.matrix(dist(km.knots))
locations.dist	<-	as.matrix(dist(loc.1))

# this makes and names a matrix of distances for knots to points for each year of data.
knots.to.centers	<-	matrix(0,nrow(KNOT.loc),nrow(loc.1))
for(i in 1:dim(km.knots)[1]){
  knots.to.centers[i,]		<-	sqrt((km.knots$FROM[i] - loc.1$FROM)^2 + (km.knots$TO[i] - loc.1$TO)^2)
}

# Make containers to hold the knots for each of the movement times.
w_move <- array(0,dim=c(N.move.group,N.knots,N_month),dimnames=list(move_id_name,paste("w_move.",1:N.knots,sep=""),MONTH))

#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
### THIS SECTION MAKES THE SPAWNING MOVEMENT MATRICES (RETURN TO NATAL RIVERS FROM THE OCEAN)
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################

# make arrays to hold the w_spawn values, projection matrices
w_spawn <- array(0,dim=c(N.move.spawn,N_knot_spawn,max_age),dimnames=list(move_id_spawn,paste("w_spawn.",1:N_knot_spawn,sep=""),paste("age.",1:max_age,sep="")))
knot_loc_spawn <- array(0,dim=c(N.move.spawn,N_knot_spawn,2),dimnames=list(move_id_spawn,paste("w_spawn.",1:N_knot_spawn,sep=""),c("TO","FROM")))
knots.to.centers.spawn <- array(0,dim=c(N.move.spawn,N_knot_spawn,nrow(loc.1)),dimnames=list(move_id_spawn,paste("w_spawn.",1:N_knot_spawn,sep=""),NULL))

knots.dist.spawn  <- array(0,dim=c(N.move.spawn,N_knot_spawn,N_knot_spawn),dimnames=list(move_id_spawn,paste("w_spawn.",1:N_knot_spawn,sep=""),paste("w_spawn.",1:N_knot_spawn,sep="")))

for(i in 1:N.move.spawn){
  knot_loc_spawn[i,,1] <- move_id$loc.spawn[i]
  if(move_id$loc.spawn[i]==8.5){knot_loc_spawn[i,,1] <- 8} # hack for upper Columbia and snake fish.
  if(N_knot_spawn == 3){
    knot_loc_spawn[i,,2] <- knot_loc_spawn[i,,1] + c(-2,0,2)
    if(move_id$loc.spawn[i]==2){
      knot_loc_spawn[i,,2] <- knot_loc_spawn[i,,1] + c(-1,1,3)  # Change things a little for SFB fish.
    }
  }
  if(N_knot_spawn == 5){
    knot_loc_spawn[i,,2] <- knot_loc_spawn[i,,1] + c(-3,-1.5,0,1.5,3)
    if(move_id$loc.spawn[i]==2){
      knot_loc_spawn[i,,2] <- knot_loc_spawn[i,,1] + c(-1,0.5,2,3.5,5)  # Change things a little for SFB fish.
    }
  }
  
}

for(i in 1: N.move.spawn){
  knots.dist.spawn[i,,]		<-	as.matrix(dist(knot_loc_spawn[i,,]))
}

# make projection matrices for knots to grid centers
for(j in 1:N.move.spawn){
  #knots.spawn.dist		  <-	as.matrix(dist(knot_loc_spawn[j,,]))
  # this makes and names a matrix of distances for knots to points for each year of data.
  knots.to.centers.temp	<-	matrix(0,N_knot_spawn,nrow(loc.1))
  for(i in 1:N_knot_spawn){
    knots.to.centers.temp[i,]		<- sqrt((knot_loc_spawn[j,,"FROM"][i] - loc.1$FROM)^2 + (knot_loc_spawn[j,,"TO"][i] - loc.1$TO)^2)
  }
  knots.to.centers.spawn[j,,]   <- knots.to.centers.temp
}

#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
### THIS SECTION MAKES THE INITIAL MOVEMENT MATRIX (DISPERSAL FROM NATAL RIVERS)
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################

# Make Initial distribution matrix using the same predictive process trick.
#N.knot.init.FROM <- N.knot.init

loc.init <- as.numeric(sort(LOCATIONS$location.number[match(unique(REL$ocean.region),LOCATIONS$location.name)]))
if(sum((move_id$ocean_region == "UPCOL")>0)>0){ loc.init <- sort(c(loc.init,8.5)) }
N.init   <- length(loc.init)

N.knot.init.FROM <- N.init
N.knot.init.TO   <- N.knot.init
loc.2 <- expand.grid(TO=X,FROM=loc.init)

#if(N.init <= N.knot.init){ 
    N.knot.init.FROM <- N.init
    N.LOC.INIT  <- length(loc.init)
    N.knots.init	<-	N.knot.init.FROM * N.knot.init.TO
  
    L.TO		<- (N.LOC-1)/(N.knot.init.TO)
    
    KNOT.FROM <- loc.init
    KNOT.TO	<-	NULL
  for(i in 1:N.knot.init.TO){
    if(i==1){	KNOT.TO[i]	<-	1 + L.TO/2}
    if(i > 1 ){ KNOT.TO[i]= KNOT.TO[i-1]+L.TO}
  }
#}

# if(N.init > N.knot.init){ 
#   N.knots.init	<-	N.knot.init.FROM * N.knot.init.TO
#   N.LOC.INIT    <- length(loc.init)
#   
#   L.FROM  <- (max(loc.init) - min(loc.init)) / N.knot.init.FROM
#   L.TO		<- (N.LOC-1)/(N.knot.init.TO)
# 
#   KNOT.TO	<-	NULL
#   for(i in 1:N.knot.init.TO){
#     if(i==1){	KNOT.TO[i]	<-	1 + L.TO/2}
#     if(i > 1 ){ KNOT.TO[i]= KNOT.TO[i-1]+L.TO}
#   }
#   KNOT.FROM	<-	NULL
#   for(i in 1:N.knot.init.FROM){
#     if(i==1){	KNOT.FROM[i]	<-	min(loc.init) + L.FROM/2}
#     if(i > 1 ){ KNOT.FROM[i]= KNOT.FROM[i-1]+L.FROM}
#   }  
# }  

  KNOT.init.loc	<-	expand.grid(TO=KNOT.TO,FROM=KNOT.FROM)
  KNOT.init.loc	<-	KNOT.init.loc[order(KNOT.init.loc$TO,KNOT.init.loc$FROM),]

  ### MAKE KNOTS
  km.init.knots			<-	data.frame(KNOT.init.loc)

  #########################################################################################
  ### MAKE A DISTANCE MATRIX FOR THE KNOTS to each location of interest.
  #########################################################################################
  knots.dist.init		<-	  as.matrix(dist(km.init.knots))
  locations.init.dist	<-	as.matrix(dist(loc.init))
  
  # this makes and names a matrix of distances for knots to points for each year of data.
  knots.to.centers.init	<-	matrix(0,N.knots.init,nrow(loc.2))
  for(i in 1:dim(km.init.knots)[1]){
    knots.to.centers.init[i,]		<-	sqrt((km.init.knots$FROM[i] - loc.2$FROM)^2 + (km.init.knots$TO[i] - loc.2$TO)^2)
  }
  
  LOC.INIT <-loc.2
  ### HELPER FILES
  
   N.loc.proj      <- nrow(loc.1)
   N.loc.proj.init <- nrow(loc.2)
  
   knots.dist.init2 <- knots.dist.init^2
   knots.to.centers.init2 <- knots.to.centers.init^2
  
# COV	<-	cor.func(knots.dist,theta)
# inv.COV	<-	solve(COV)
# 
# N.time <- 10
# W<-matrix(0,N.knots,N.time)
# W[,1]		<-runif(N.knots,-1,1)
# 
# # Make it a time series of realizations
# rho=0.9
# for(i in 2:N.time){
#   W[,i] <- mvrnorm(1,rho*W[,i-1],COV)
# }
# 
# 
# project <- NULL
# for(i in 1: N.time){
#   project <- cbind(project,t(cor.func(knots.to.centers,theta)) %*% inv.COV %*% W[,i])
# }
# 
# ERROR <- rnorm(length(project[,1]),0,0.01)
# LOC$value	<-	project[,1] + ERROR
