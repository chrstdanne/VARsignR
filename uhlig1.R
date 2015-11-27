#--- UHLIG CODE WITHOUT SIGN RESTRICTIONS ---#
# VERSION NOV 17 2015
# CHRISTIAN DANNE
# DANNEC@TCD.IE


#--- BEGIN FUNCTION ---#
bvar <- function(Y=NULL, nlags=4, draws=1000, constant=TRUE, steps=24, shock=1){
#
#--- SANITY CHECK ---#
sanity.check.bvar(Y=Y, nlags=nlags, draws=draws, constant=constant, steps=steps, shock=shock)

#--- SET UP PARAS ---# 
n1 <- draws
nstep <- steps
nlags <- nlags 
shock <- shock 
nvar <- ncol(Y)
nobs <- nrow(Y)
nnobs0 <- nlags + 1
nnobs <- nobs - nlags
nnvar0 <- nvar + 1
#
if(constant == FALSE){
CONS <- "F"
ncoef <- nvar * nlags
nncoef <- nvar * nlags
nnvar1 <- nvar * (nlags + 1)
}else{
CONS <- "T"
ncoef <- nvar * (nlags+1)
nncoef <- nvar * nlags + 1
nnvar1 <- nvar * (nlags + 1) + 1
}
#
#---REDUCED FORM VAR MODEL ---#
model <- rfvar(Y,lags=nlags, const=CONS)
bcoef <- model$By # same order as above but w/const and nvar x nvar x lags
resid <- model$u # same as above
data <- model$X
xx <- model$xx
 
#--- SIGMA and SXX ---#
uu <- crossprod(resid)
# sigma <- (1/(nnobs-nncoef))*uu
sigma <- (1/nnobs)*uu

#--- SET UP MCMC OF VAR ---#
sxx <-  chol(xx)
sv <- solve(uu)
svt <-  chol(sv)
betaols <- t(bcoef)
best <- betaols
wishdof <- nnobs-nncoef

#--- MATRICES FOR DRAWS ---#
goodresp <- array(NA, c(n1, nstep, nvar))
BDraws <- array(NA, c(n1, nncoef, nvar))
SDraws <- array(NA, c(n1, nvar, nvar))
imp <- matrix(NA, nrow=nstep, ncol=nvar)

#--- Monte CARLO INTEGRATION ---#
message('\n Starting MCMC, ', date(),'.', sep="")
pb <- txtProgressBar(min = 0, max = n1, style = 3)
for(draws in 1:n1){
  setTxtProgressBar(pb, draws)

  #--- sigma draws ---#
  sigmad  <- solve(matrix(rWishart(1, wishdof, sv), nrow=nvar, ncol=nvar))
  swish   <- chol(sigmad)

  #--- beta draws ---#
  swsxx <- sigmad  %x% xx
  bd <- rep(0, nrow(swsxx))
  betau <- matrix(mvrnormR(1,0,swsxx), nrow=nncoef, ncol=nvar)
  betadraw <- betaols + betau
  bhat <- betadraw

  #--- irfs ---#
  imfhat <- fn.impulse(bhat, swish, c(nvar, nlags, nstep))
  impulses <-  array(imfhat, dim=c(nstep,nvar,nvar))

  imp <- impulses[,,shock]

goodresp[draws, ,] <- imp
BDraws[draws, , ] <- betadraw
SDraws[draws, , ] <- sigmad
}#end draws
close(pb)
return(list(IRFS=goodresp, BDraws=BDraws, SDraws=SDraws))
}#end of function
#
#--- END OF SCRIPT ---#
