#VARsignR
Estimating VARs using sign restrictions in R

Version Nov 15 2015
Christian Danne
dannec@tcd.ie

##DESCRIPTION##
This toolbox provides a set of functions for identifying structural shocks in Vector Autoregressions (VARs) using sign restrictions in R. Currently, it implements Uhlig's rejection method, Uhlig's penalty function approach, and Rubio-Ramirez, Waggoner, and Zha's QR-based rejection algorithm. Estimation of the underlying VAR model is Bayesian using a flat Wishart prior. For more information on the three methods see: 

Rubio-Ramirez, J., et al. (2010), "Structural Vector Autoregressions: Theory of Identification and Algorithms for Inference", Review of Economic Studies, 77, 665-696.

Uhlig, H. (2005), "What are the effects of monetary policy on output? Results from an agnostic identification procedure", Journal of Monetary Economics, 52, 381-419.

Full package development is currently on its way. More routines and features will be added to the repository once they are finished and tested. 

##IMPORTANT NOTE##
This is a beta version and does not contain many sanity checks. If you find any errors, have suggestions, or if you want to contribute, please contact me at dannec@tcd.ie. 

##AKNOWLEDGEMENTS## 
I would like to thank Tom Doan, Chris Sims, and Tao Zha for making their programme codes available online. 

##CONTENTS##
 **example.R:** example code demonstrating the use of the models and potting the results. 
 **fnimpulse.R:** creates impulse responses from the estimated reduced form VAR. 
 **plotfunc.R:** contains functions for plotting the impulse responses and saving the plots.
 **rfvar.R:** estimates a standard reduced for VAR via OLS. Code is based on Chris Sims' R codes.
 **signfuncs.R:** contains most of the functions employing the sign restrictions. Functions largely are based on Tom Doan's RATS codes and Tao Zha's MATLAB codes. 
 **uhligdata.RData:** Monthly macroeconomic time series used in Uhlig (2005).
 **uhlig1.R:** standard BVAR with a flat wishart prior. 
 **uhlig2.R:** identifies structural VAR shocks using Uhlig's rejection method.
 **uhlig3.R:** identifies structural VAR shocks using Uhlig's penalty function approach. 
 **rwz.R:** identifies stuctural VAR shocks using the Rubio et al.'s algorithm. 
 


 


 
