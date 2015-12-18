#VARsignR
Estimating VARs using sign restrictions in R

Christian Danne (dannec@tcd.ie)

Version: Dec 18 2015

###Description
This toolbox provides a set of functions for identifying structural shocks in Vector Autoregressions (VARs) using sign restrictions. Currently, it implements Uhlig's (2005) rejection method, Uhlig's (2005) penalty function approach, Rubio-Ramirez et al's (2010) QR-based rejection algorithm, and Fry and Pagan's (2011) median target method. Inference is Bayesian using a flat Normal-Wishart prior.  

For more information on the three methods, please check the accompanying vignette and  

Fry, R. and Pagan, A. (2011), "Sign restrictions in structural vector autoregressions: A critical review", *Journal of Economic Literature*, 49, 938-960.

Rubio-Ramirez, J., Waggoner, D., Zha, T. (2010), "Structural Vector Autoregressions: Theory of Identification and Algorithms for Inference", *Review of Economic Studies*, 77, 665-696.

Uhlig, H. (2005), "What Are the Effects of Monetary Policy on Output? Results from an Agnostic Identification Procedure", *Journal of Monetary Economics*, 52, 381-419.

###Important notes
This is a **beta** version. The package does not come with any warranty (see GNU General Public License). More routines and features will be added to the package once they are finished and tested. If you find any errors, have suggestions, or if you want to contribute, please contact me at dannec@tcd.ie. 

###Installation instructions
VARsignR depends on the [HI](https://cran.r-project.org/web/packages/HI) package by Giovanni Petris and Luca Tardella, [minqa](https://cran.r-project.org/web/packages/minqa/) package by Douglas Bates et al, and [mvnfast](https://cran.r-project.org/web/packages/mvnfast) by Fasiolo et al. You will have to install both packages prior to installing VARsignR. See VARsignR-vignette.pdf for more information. 

###Aknowledgements 
I would like to thank Tom Doan, Chris Sims, and Tao Zha for making their programme codes available online. 

###Near term development plan
Multiple shocks for rwz.reject, uhlig.reject
Zero restrictions (Uhlig, 2005; Arias et al, 2014, FED WP)
Mountford and Uhlig (2009, JAE)

 
###Changelog
2015-11-29 Initial release.    
2015-12-19 Minor bug fixes. FEVDs added. Vignette added. Shock extraction added. Fry and Pagans median target method added. MVN draws from mvnfast.  
 

 
 


 


 
