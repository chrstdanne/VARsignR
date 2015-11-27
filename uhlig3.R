#--- UHLIG PENALTY FUNCTION---#
# VERSION NOV 22 2015
# CHRISTIAN DANNE
# DANNEC@TCD.IE

uhlig.penalty <- function(Y=NULL, nlags=4, draws=2000, subdraws=1000, nkeep=1000, KMIN=1, KMAX=4, constrained=NULL, constant=TRUE, steps=24, penalty=100, crit=0.001){
#
#--- SANITY CHECK ---#
sanity.check.uhlig.penalty(Y=Y, nlags=nlags, draws=draws, subdraws=subdraws, nkeep=nkeep, KMIN=KMIN, KMAX=KMAX, constrained=constrained, constant=constant, steps=steps, penalty=penalty, crit=crit)

#--- SET UP PARAS ---# 
n1 <- draws
n2 <- subdraws
nstep <- steps
nlags <- nlags 
nvar <- ncol(Y)
nobs <- nrow(Y)
nnobs0 <- nlags + 1
nnobs <- nobs - nlags
nnvar0 <- nvar + 1
pen <- penalty
ntot <- n1 
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
goodresp <- array(NA, c(nkeep, nstep, nvar))
BDraws <- array(NA, c(nkeep, nncoef, nvar))
SDraws <- array(NA, c(nkeep, nvar, nvar))
imp <- matrix(NA, nrow=nstep, ncol=nvar)
#--- WEIGHTING MATRIX ---#
YD <- apply(Y, 2, diff)
scales <-  matrix(apply(matrix(apply(YD, 2, var)),1,sqrt))

# PROJECTION to the unit sphere in R^n.
g <- matrix(1, nrow=nvar-1, ncol=1)

#--- Monte CARLO INTEGRATION ---#
accept <- 0
convcnt <- 0
expt <- round(0.003858333 * n1)
message('Starting MCMC, ', date(),'. Takes approximately ', expt, ' mins.', sep="")
pb0 <- txtProgressBar(min = 0, max = n1, style = 3)
for(draws in 1:n1){
  setTxtProgressBar(pb0, draws)

  #   #--- comment out below and try this ---# turn this into an oject to the bdraws, sdraws (and fevds).
  #   impulses <- sdraws(wishdof, sv, nvar, xx, nncoef, betaols, nstep)

  #--- sigma draws ---#
  sigmad  <- solve(matrix(rWishart(1, wishdof, sv), nrow=nvar, ncol=nvar))
  swish   <- chol(sigmad)

  #--- beta draws ---#
  swsxx <-   sigmad  %x% xx
  bd <- rep(0, nrow(swsxx))
  betau <- matrix(mvrnormR(1,0,swsxx), nrow=nncoef, ncol=nvar)
  betadraw <- betaols + betau
  bhat <- betadraw

  #--- irfs ---#
  imfhat <- fn.impulse(bhat, swish, c(nvar, nlags, nstep))
  impulses <-  array(imfhat, dim=c(nstep,nvar,nvar))

#--- PENALTY FUNCTION EVAL ---#
penaltyfunc <- optim(g, fn=UhligPenalty,   method ="Nelder-Mead", control = list(maxit = n2, trace = F), hessian = F, first=KMIN, last=KMAX, constrained=constrained, impulses=impulses, scales=scales, pen=pen)

betat <- penaltyfunc$par
UAT <- penaltyfunc$value
convt <- penaltyfunc$convergence

penaltyfunc <- optim(betat, fn=UhligPenalty,   method ="Nelder-Mead", control = list(maxit = n2, trace = F),  hessian = F, first=KMIN, last=KMAX, constrained=constrained, impulses=impulses, scales=scales, pen=pen)

beta <- penaltyfunc$par
UA <- penaltyfunc$value
conv <- penaltyfunc$convergence

convcnt <- convcnt + max(c(convt,conv))

if(convt==1 | conv==1){
  next
}

if(abs(UAT-UA)<=crit){
  a <- stereo(beta)
  for(j in 1:nstep){
    imp[j,] <- t(impulses[j,,]%*%a)
  }
  accept <- accept+1
  goodresp[accept, ,] <-  imp
}

if(accept>=nkeep){
  break
}
}# END DRAWS
close(pb0)
#
#--- WARNING MESSAGE IN CASE OF TOO FEW DRAWS ---#
if(accept<nkeep){
  if(accept==0){
    stop("\n Not enough accepted draws to proceed!")
  }else{
    goodresp <- goodresp[1:accept, , ]
    message('\n Warning! Had only ', accept,' accepted draw(s) out of ',ntot,'. ', convcnt, ' draws did not converge.', sep="")
  }
}
#
if(accept>=nkeep & convcnt>0){
  goodresp <- goodresp[1:accept, , ]
    BDraws <- BDraws[1:accept, , ]  
  SDraws <- SDraws[1:accept, , ] 
  message('\n Warning! ', convcnt, ' draws did not converge.', sep="")
}
return(list(IRFS=goodresp, BDraws=BDraws, SDraws=SDraws))
}#end of function 
#--- END OF SCRIPT ---#
