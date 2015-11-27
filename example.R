#--- EXAMPLE FILE ---#
# VERSION NOV 25 2015
# CHRISTIAN DANNE
# DANNEC@TCD.IE

#--- NOTES ---#
# This file demonstrates the useage of VARsignR. You will have to change the path below in "setwd()" to the directory where you have saved the contents of VARsignR and install the "HI" package.

#--- PREAMBLE ---#
rm(list = ls())
set.seed(12345)
setwd("/home/christian/VARsignR")

#--- SOURCING FUNCTIONS OF VARsignR ---#
library("HI")
source("signfunc.R", echo=F)
source("plotfunc.R", echo=T)
source("fnimpulse.R", echo=F)
source("rfvar.R", echo=F)
source("uhlig1.R", echo=F)
source("uhlig2.R", echo=F)
source("uhlig3.R", echo=F)
source("rwz.R", echo=F)
source("sanity.R", echo=T)

#--- READ IN DATA ---#
load("uhligdata.RData")
order <- c("y", "yd", "p", "i", "rnb","rt") # variable ordering in data. corresponds to cholesky decomp in standard BVAR.

vl <- c("GDP","GDP Deflator","Comm.Pr.Index","Fed Funds Rate", "NB Reserves", "Total Reserves") # variable labels for plots

#--- SIGN RESTRICTIONS ---#
# any number <= number of variables are allowed.
# number indicates the variable to be constrained (ordering in the data.frame)
# sign of integer indicates the sign restriction of the variable.
# first element is the shock of interest. you MUST specify the sign restriction for the shock of interest.

constr <- c(+4,-3,-2,-5)

#--- STANDARD REDUCED FORM VAR WITH FLAT WISHART PRIOR ---#
model1 <- bvar(Y=Y, nlags=12, draws=1000, constant=FALSE, steps=60, shock=4)

irfdata1 <- model1$IRFS

irfplot(irfdraws=irfdata1,type= "mean", labels=vl, save=FALSE, bands=c(0.16, 0.84), grid=TRUE, bw=FALSE)

# #--- UHLIG'S REJECTION METHOD ---#
# model2 <- uhlig.reject(Y=Y, nlags=12, draws=200, subdraws=200, nkeep=1000, KMIN=1, KMAX=6, constrained=constr, constant=FALSE, steps=60)
#
# irfdata2 <- model2$IRFS
#
# irfplot(irfdraws=irfdata2,type="mean", labels=vl, save=FALSE, bands=c(0.16, 0.84), grid=TRUE, bw=FALSE)

# #--- UHLIG'S PENALTY FUNCTION APPROACH ---#
# model3 <- uhlig.penalty(Y=Y, nlags=12, draws=2000, subdraws=1000, nkeep=1000, KMIN=1, KMAX=6, constrained=constr, constant=FALSE, steps=60, penalty=100, crit=0.001)
#
# irfdata3 <- model3$IRFS
#
# irfplot(irfdraws=irfdata3,type="mean", labels=vl, save=FALSE, bands=c(0.16, 0.84), grid=TRUE, bw=FALSE)

# #--- RWZ REJECTION APPROACH ---#
# model4 <- rwz.reject(Y=Y, nlags=12, draws=200, subdraws=200, nkeep=1000, KMIN=1, KMAX=6, constrained=constr, constant=FALSE, steps=60)
#
# irfdata4 <- model4$IRFS
#
# irfplot(irfdraws=irfdata4,type="mean", labels=vl, save=FALSE, bands=c(0.16, 0.84), grid=TRUE, bw=FALSE)

#--- END OF SCRIPT ---#


