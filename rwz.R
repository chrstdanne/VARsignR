#--- RWZ SIGN RESTRICTIONS ---#
# VERSION NOV 22 2015
# CHRISTIAN DANNE
# DANNEC@TCD.IE

rwz.reject <- function(Y=NULL, nlags=4, draws=200, subdraws=200, nkeep=1000, KMIN=1, KMAX=4, constrained=NULL, constant=TRUE, steps=24){
#
#SANITY CHECK ---#
sanity.check.reject(Y=Y, nlags=nlags, draws=draws, subdraws=subdraws, nkeep=nkeep, KMIN=KMIN, KMAX=KMAX, constrained=constrained, constant=constant, steps=steps)

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
ntot <- n1*n2
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
#--- MCMC INTEGRATION ---#
accept <- 0
message('Starting MCMC, ', date(),'.', sep="")
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
  # betau <- matrix(mvnfast::rmvn(1, bd, swsxx), nrow=nncoef, ncol=nvar)
  betadraw <- betaols + betau
  bhat <- betadraw

  #--- irfs ---#
  imfhat <- fn.impulse(bhat, swish, c(nvar, nlags, nstep))
  impulses <-  array(imfhat, dim=c(nstep,nvar,nvar))

  #SUBDRAWS START HERE. # you might have to change this into a matrix of the form nvar x nvar
  for(subdraws in 1:n2){
    a <- matrix(rnorm(nvar,mean=0,sd=1), nvar, 1) # IS THAT CORRECT OR SHOULD YOU DRAW ON SPHERE?
    RWZA <- RWZAccept(a,KMIN,KMAX, constrained, impulses)
    RWZ <- RWZA$acc
    q <- RWZA$Q
    #
    if(RWZ==1){
      for(j in 1:nstep){
        imp[j,] <- t(impulses[j,,]%*%q)
      }
      accept <- accept+1
      goodresp[accept, ,] <-  imp
    }else{
      next
    }
    if(accept>=nkeep){
      break
    }
  } # end subdraws
if(accept>=nkeep){
  break
}
}#END DRAWS
close(pb0)
#
#--- WARNING MESSAGE IN CASE OF TOO FEW DRAWS ---#
if(accept<nkeep){
  if(accept==0){
    stop("\n Not enough accepted draws to proceed!")
  }else{
    goodresp <- goodresp[1:accept, , ]
      BDraws <- BDraws[1:accept, , ]  
  SDraws <- SDraws[1:accept, , ] 
    message('\n Warning! Had only ', accept,' accepted draw(s) out of ',ntot,'.', sep="")
  }
}
return(list(IRFS=goodresp, BDraws=BDraws, SDraws=SDraws))
}#end of function 
#--- END OF SCRIPT ---#

