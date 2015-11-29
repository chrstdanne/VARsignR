#VARsignR
Estimating VARs using sign restrictions in R

Christian Danne (dannec@tcd.ie)

Version: Nov 29 2015

###Description
This toolbox provides a set of functions for identifying structural shocks in Vector Autoregressions (VARs) using sign restrictions in R. Currently, it implements Uhlig's rejection method, Uhlig's penalty function approach, and Rubio-Ramirez, Waggoner, and Zha's QR-based rejection algorithm. Estimation of the underlying VAR model is Bayesian using a flat Wishart prior. For more information on the three methods see: 

Rubio-Ramirez, J., et al. (2010), "Structural Vector Autoregressions: Theory of Identification and Algorithms for Inference", Review of Economic Studies, 77, 665-696.

Uhlig, H. (2005), "What Are the Effects of Monetary Policy on Output? Results from an Agnostic Identification Procedure", Journal of Monetary Economics, 52, 381-419.

###Important notes
This is a beta version. More routines and features will be added to the package once they are finished and tested. If you find any errors, have suggestions, or if you want to contribute, please contact me at dannec@tcd.ie. 

###Installation instructions
VARsignR depends on the HI package by Giovanni Petris and Luca Tardella (https://cran.r-project.org/web/packages/HI/) and the minqa package by Douglas Bates et al. (https://cran.r-project.org/web/packages/minqa/). You will have to install both packages prior to installing VARsignR.


###Aknowledgements 
I would like to thank Tom Doan, Chris Sims, and Tao Zha for making their programme codes available online. 

 
 


 


 
