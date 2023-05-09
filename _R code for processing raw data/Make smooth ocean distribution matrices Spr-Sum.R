

######## Define Knots and projection locations

### Remove predictions for the Salish Sea locations.  We will add them back in in the STAN program 
k.pred.index <- LOCATIONS %>% filter( !location.name %in% c("PUSO","PUSO_out","SGEO"))  %>% arrange(location.number ) %>% mutate(knot.idex = 1:nrow(.))
k.pred.index.salish <- LOCATIONS %>% filter( location.name %in% c("PUSO","PUSO_out","SGEO"))  %>% arrange(location.number ) 
N_pred_loc <- as.numeric(k.pred.index %>% summarize(length(location.name)))
N_pred_loc_salish <- as.numeric(k.pred.index.salish %>% summarize(length(location.name)))

# user Defined number of knots to smooth over.
N_knot_sf    <- 8 #round( (2/3)* N_pred_loc) # Summer-Fall
N_knot_ws    <- 5 #round( (2/3)* N_pred_loc) # Winter-Spring

# Prediction locations 
pred.loc <- k.pred.index$knot.idex
knot.loc.sum.fall <- seq(min(pred.loc -0.25), max(pred.loc +0.25),length.out=N_knot_sf)
knot.loc.wint.spr <- seq(min(pred.loc -0.25), max(pred.loc +0.25),length.out=N_knot_ws)


# Matrices of distance among knots and between knots and prediction locations.
# Summer Fall
d_knot_knot_sf <- as.matrix(dist(knot.loc.sum.fall,upper=T))
d_pred_knot_sf <- as.matrix(dist(c(pred.loc,knot.loc.sum.fall),upper=T))
d_pred_knot_sf <- d_pred_knot_sf[(N_pred_loc+1):nrow(d_pred_knot_sf),1:N_pred_loc]

d_knot_knot_sf2 <- d_knot_knot_sf^2
d_pred_knot_sf2 <- d_pred_knot_sf^2

# Winter Spring
d_knot_knot_ws <- as.matrix(dist(knot.loc.wint.spr,upper=T))
d_pred_knot_ws <- as.matrix(dist(c(pred.loc,knot.loc.wint.spr),upper=T))
d_pred_knot_ws <- d_pred_knot_ws[(N_pred_loc+1):nrow(d_pred_knot_ws),1:N_pred_loc]

d_knot_knot_ws2 <- d_knot_knot_ws^2
d_pred_knot_ws2 <- d_pred_knot_ws^2

# Worked example
theta = 3
V     = 1
cov.knot.to.knot <- exp(- d_knot_knot_sf2 / theta^2) * V^2
cov.knot.to.pred <- exp(- d_pred_knot_sf2 / theta^2) * V^2
w_star <- mvrnorm(1,rep(0,N_knot_sf),cov.knot.to.knot)
A<- t(cov.knot.to.pred) %*% solve(cov.knot.to.knot) %*% w_star

cov.knot.to.knot <- exp(- d_knot_knot_ws2 / theta^2) * V^2
cov.knot.to.pred <- exp(- d_pred_knot_ws2 / theta^2) * V^2
w_star <- mvrnorm(1,rep(0,N_knot_ws),cov.knot.to.knot)
B<- t(cov.knot.to.pred) %*% solve(cov.knot.to.knot) %*% w_star

# This it the matrix algebra that makes the projection from knots to locations.
par(mfrow=c(2,1))
plot(A~pred.loc)
plot(B~pred.loc)
