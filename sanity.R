#--- SANITY CHECKS ---#
# VERSION NOV 26 2015
# CHRISTIAN DANNE
# DANNEC@TCD.IE

# is.data.frame(df) & nrow(df)==0 # empty data.frame
#--- CHECK BASIC VAR ---#
sanity.check.bvar <- function(Y=Y, nlags=nlags, draws=draws, constant=constant, steps=steps, shock=shock){

  Yts <-is.ts(Y)
  Ydf <- is.data.frame(Y)

if(Yts==FALSE & Ydf==FALSE){
  options(error=NULL)
  message(" ")
    stop(" Data has to be a data.frame() or ts() object.\n", call. = FALSE)
}

  if(ncol(Y)<2){
    options(error=NULL)
    message(" ")
    stop(" Need more than 1 variable.\n", call. = FALSE)
  }

    Yna <- any(is.na(Y)==TRUE)
    Ynan <- any(is.nan(Y)==TRUE)
    Ynum <-  is.numeric(Y)

    if(Yna==TRUE | Ynan==TRUE){
      options(error=NULL)
      message(" ")
        stop(" Data must not contain missing values.\n", call. = FALSE)
    }

    if(Ynum!=TRUE){
      options(error=NULL)
      message(" ")
        stop(" Data must not contain strings.\n", call. = FALSE)
    }

    nlagsint <- nlags%%1==0
    nlagsobs <- nrow(Y)-nlags

    if(nlagsint==FALSE){
      options(error=NULL)
      message(" ")
        stop("Number of lags must be integer.\n", call. = FALSE)
    }

      if(nlagsobs<=0){
        options(error=NULL)
        message(" ")
        stop(" Number of lags cannot be larger than the number of observations.\n", call. = FALSE)
    }

  if(nlags<1){
    options(error=NULL)
    message(" ")
    stop(" Need at least 1 lag.\n")
}
        if(nlags>nrow(Y)){
          options(error=NULL)
          message(" ")
    stop(" Number of lags have to be smaller than number of observations.\n", call. = FALSE)
  }

    drawsint <- draws%%1==0

      if(drawsint!=TRUE){
        options(error=NULL)
        message(" ")
        stop(" Number of draws must be integer.\n", call. = FALSE)
    }

      if(draws<=0){
        options(error=NULL)
        message(" ")
        stop(" Number of draws must geater than zero.\n", call. = FALSE)
    }

     stepsint <- steps%%1==0

      if(stepsint!=TRUE){
        options(error=NULL)
        message(" ")
        stop(" Number of steps must be integer.\n", call. = FALSE)
    }

      if(steps<=0){
        options(error=NULL)
        message(" ")
        stop(" Number of steps must geater than zero.\n", call. = FALSE)
    }

     shockint <- shock%%1==0

      if(shockint!=TRUE){
        options(error=NULL)
        message(" ")
        stop(" Shock of interest must be integer.\n", call. = FALSE)
    }

      if(shock<=0 | shock>ncol(Y)){
        options(error=NULL)
        message(" ")
        stop(" Shock of interest must be greater than zero and smaller or equal number variables.\n", call. = FALSE)
      }
     options(error=traceback)
 return()
}#end of function

sanity.check.reject  <- function(Y=Y, nlags=nlags, draws=draws, subdraws=subdraws, nkeep=nkeep, KMIN=KMIN, KMAX=KMAX, constrained=constrained, constant=constant, steps=steps){

  Yts <-is.ts(Y)
  Ydf <- is.data.frame(Y)

if(Yts==FALSE & Ydf==FALSE){
  options(error=NULL)
  message(" ")
    stop(" Data has to be a data.frame() or ts() object.\n", call. = FALSE)
}

  if(ncol(Y)<2){
    options(error=NULL)
    message(" ")
    stop(" Need more than 1 variable.\n", call. = FALSE)
  }

    Yna <- any(is.na(Y)==TRUE)
    Ynan <- any(is.nan(Y)==TRUE)
    Ynum <-  is.numeric(Y)

    if(Yna==TRUE | Ynan==TRUE){
      options(error=NULL)
      message(" ")
        stop(" Data must not contain missing values.\n", call. = FALSE)
    }

    if(Ynum!=TRUE){
      options(error=NULL)
      message(" ")
        stop(" Data must not contain strings.\n", call. = FALSE)
    }

    nlagsint <- nlags%%1==0
    nlagsobs <- nrow(Y)-nlags

    if(nlagsint==FALSE){
      options(error=NULL)
      message(" ")
        stop("Number of lags must be integer.\n", call. = FALSE)
    }

      if(nlagsobs<=0){
        options(error=NULL)
        message(" ")
        stop(" Number of lags cannot be larger than the number of observations.\n", call. = FALSE)
    }

  if(nlags<1){
    options(error=NULL)
    message(" ")
    stop(" Need at least 1 lag.\n")
}
        if(nlags>nrow(Y)){
          options(error=NULL)
          message(" ")
    stop(" Number of lags have to be smaller than number of observations.\n", call. = FALSE)
  }

    drawsint <- draws%%1==0

      if(drawsint!=TRUE){
        options(error=NULL)
        message(" ")
        stop(" Number of draws must be integer.\n", call. = FALSE)
    }

      if(draws<=0){
        options(error=NULL)
        message(" ")
        stop(" Number of draws must geater than zero.\n", call. = FALSE)
    }

     stepsint <- steps%%1==0

      if(stepsint!=TRUE){
        options(error=NULL)
        message(" ")
        stop(" Number of steps must be integer.\n", call. = FALSE)
    }

      if(steps<=0){
        options(error=NULL)
        message(" ")
        stop(" Number of steps must geater than zero.\n", call. = FALSE)
    }

      
      subdrawsint <- subdraws%%1==0

      if(subdrawsint!=TRUE){
        options(error=NULL)
        message(" ")
        stop(" Number of subdraws must be integer.\n", call. = FALSE)
    }

      if(subdraws<=0){
        options(error=NULL)
        message(" ")
        stop(" Number of subdraws must geater than zero.\n", call. = FALSE)
    }

      nkeepint <- nkeep%%1==0

      if(nkeepint!=TRUE){
        options(error=NULL)
        message(" ")
        stop(" Number of nkeep must be integer.\n", call. = FALSE)
    }

      if(nkeep<=0){
        options(error=NULL)
        message(" ")
        stop(" Number of nkeep must geater than zero.\n", call. = FALSE)
    }
 

      KMINint <- KMIN%%1==0

      if(KMINint!=TRUE){
        options(error=NULL)
        message(" ")
        stop("KMIN must be integer.\n", call. = FALSE)
    }

      if(KMIN<=0 | KMIN>KMAX){
        options(error=NULL)
        message(" ")
        stop("KMIN must be greater than zero and smaller than KMAX.\n", call. = FALSE)
      }

 
      KMAXint <- KMAX%%1==0

      if(KMAXint!=TRUE){
        options(error=NULL)
        message(" ")
        stop("KMAX must be integer.\n", call. = FALSE)
    }

      if(KMIN<=0 | KMIN>KMAX){
        options(error=NULL)
        message(" ")
        stop("KMAX must be greater than zero and greater than KMIN.\n", call. = FALSE)
      }

 
 cnsl <- length(constrained)

    if(cnsl<=0){
           options(error=NULL)
      message(" ")
        stop("Number of constraints must at least 1.\n", call. = FALSE)   
    }
 
        if(cnsl>ncol(Y)){
           options(error=NULL)
      message(" ")
        stop("Number of constraints cannot be greater than number of variables.\n", call. = FALSE)   
    }

   if(max(abs(constrained))>=ncol(Y) | min(abs(constrained))==0){
           options(error=NULL)
      message(" ")
        stop("Constraints must be between 1 and the number of variables.\n", call. = FALSE)   
    }
 
 cnscns <- all(constrained%%1==0)

    if(cnscns==FALSE){
              options(error=NULL)
      message(" ")
        stop("All constraints must be integers.\n", call. = FALSE)  
 }

cnsdup <- anyDuplicated(abs(constrained))
    if(cnsdup>0){
              options(error=NULL)
      message(" ")
        stop("Cannot provide multiple constraints for the same variable.\n", call. = FALSE)  
 }


     options(error=traceback)
 return()

}#end of function 

sanity.check.uhlig.penalty  <- function(Y=Y, nlags=nlags, draws=draws, subdraws=subdraws, nkeep=nkeep, KMIN=KMIN, KMAX=KMAX, constrained=constrained, constant=constant, steps=steps, penalty=penalty, crit=crit){

  Yts <-is.ts(Y)
  Ydf <- is.data.frame(Y)

if(Yts==FALSE & Ydf==FALSE){
  options(error=NULL)
  message(" ")
    stop(" Data has to be a data.frame() or ts() object.\n", call. = FALSE)
}

  if(ncol(Y)<2){
    options(error=NULL)
    message(" ")
    stop(" Need more than 1 variable.\n", call. = FALSE)
  }

    Yna <- any(is.na(Y)==TRUE)
    Ynan <- any(is.nan(Y)==TRUE)
    Ynum <-  is.numeric(Y)

    if(Yna==TRUE | Ynan==TRUE){
      options(error=NULL)
      message(" ")
        stop(" Data must not contain missing values.\n", call. = FALSE)
    }

    if(Ynum!=TRUE){
      options(error=NULL)
      message(" ")
        stop(" Data must not contain strings.\n", call. = FALSE)
    }

    nlagsint <- nlags%%1==0
    nlagsobs <- nrow(Y)-nlags

    if(nlagsint==FALSE){
      options(error=NULL)
      message(" ")
        stop("Number of lags must be integer.\n", call. = FALSE)
    }

      if(nlagsobs<=0){
        options(error=NULL)
        message(" ")
        stop(" Number of lags cannot be larger than the number of observations.\n", call. = FALSE)
    }

  if(nlags<1){
    options(error=NULL)
    message(" ")
    stop(" Need at least 1 lag.\n")
}
        if(nlags>nrow(Y)){
          options(error=NULL)
          message(" ")
    stop(" Number of lags have to be smaller than number of observations.\n", call. = FALSE)
  }

    drawsint <- draws%%1==0

      if(drawsint!=TRUE){
        options(error=NULL)
        message(" ")
        stop(" Number of draws must be integer.\n", call. = FALSE)
    }

      if(draws<=0){
        options(error=NULL)
        message(" ")
        stop(" Number of draws must geater than zero.\n", call. = FALSE)
    }

     stepsint <- steps%%1==0

      if(stepsint!=TRUE){
        options(error=NULL)
        message(" ")
        stop(" Number of steps must be integer.\n", call. = FALSE)
    }

      if(steps<=0){
        options(error=NULL)
        message(" ")
        stop(" Number of steps must geater than zero.\n", call. = FALSE)
    }

      
      subdrawsint <- subdraws%%1==0

      if(subdrawsint!=TRUE){
        options(error=NULL)
        message(" ")
        stop(" Number of subdraws must be integer.\n", call. = FALSE)
    }

      if(subdraws<=0){
        options(error=NULL)
        message(" ")
        stop(" Number of subdraws must geater than zero.\n", call. = FALSE)
    }

      nkeepint <- nkeep%%1==0

      if(nkeepint!=TRUE){
        options(error=NULL)
        message(" ")
        stop(" Number of nkeep must be integer.\n", call. = FALSE)
    }

      if(nkeep<=0){
        options(error=NULL)
        message(" ")
        stop(" Number of nkeep must geater than zero.\n", call. = FALSE)
    }
 

      KMINint <- KMIN%%1==0

      if(KMINint!=TRUE){
        options(error=NULL)
        message(" ")
        stop("KMIN must be integer.\n", call. = FALSE)
    }

      if(KMIN<=0 | KMIN>KMAX){
        options(error=NULL)
        message(" ")
        stop("KMIN must be greater than zero and smaller than KMAX.\n", call. = FALSE)
      }

 
      KMAXint <- KMAX%%1==0

      if(KMAXint!=TRUE){
        options(error=NULL)
        message(" ")
        stop("KMAX must be integer.\n", call. = FALSE)
    }

      if(KMIN<=0 | KMIN>KMAX){
        options(error=NULL)
        message(" ")
        stop("KMAX must be greater than zero and greater than KMIN.\n", call. = FALSE)
      }

 
 cnsl <- length(constrained)

    if(cnsl<=0){
           options(error=NULL)
      message(" ")
        stop("Number of constraints must at least 1.\n", call. = FALSE)   
    }
 
        if(cnsl>ncol(Y)){
           options(error=NULL)
      message(" ")
        stop("Number of constraints cannot be greater than number of variables.\n", call. = FALSE)   
    }

   if(max(abs(constrained))>=ncol(Y) | min(abs(constrained))==0){
           options(error=NULL)
      message(" ")
        stop("Constraints must be between 1 and the number of variables.\n", call. = FALSE)   
    }
 
 cnscns <- all(constrained%%1==0)

    if(cnscns==FALSE){
              options(error=NULL)
      message(" ")
        stop("All constraints must be integers.\n", call. = FALSE)  
 }

cnsdup <- anyDuplicated(abs(constrained))
    if(cnsdup>0){
              options(error=NULL)
      message(" ")
        stop("Cannot provide multiple constraints for the same variable.\n", call. = FALSE)  
 }

 if(penalty<=0){
    options(error=NULL)
      message(" ")
        stop("Penalty must be greater than 0.\n", call. = FALSE)    
 }
 
 if(crit<0){
    options(error=NULL)
      message(" ")
        stop("Crit must be greater than 0.\n", call. = FALSE)    
 }

     options(error=traceback)
 return()

}#end of function 

# #--- IRF CHECK ---#
sanity.check.irfplot <- function(irfdraws=irfdraws,type=type, labels=labels,save=save, bands=bands, grid=grid, bw=bw){
#irfdraws
    Idim <- is.null(dim(irfdraws))
 
    if(Idim==TRUE){
     options(error=NULL)
      message(" ")
       stop(" Irfdraws must be draws x steps x nvar .\n", call. = FALSE)  
    }

   Idiml <- length(dim(irfdraws))

   if(Idiml!=3){
     options(error=NULL)
      message(" ")
       stop(" Irfdraws must be of dimensions (draws x steps x nvar) .\n", call. = FALSE)  
    }

    Ina <- any(is.na(irfdraws)==TRUE)
    Inan <- any(is.nan(irfdraws)==TRUE)
    Inum <-  is.numeric(irfdraws)

    if(Ina==TRUE | Inan==TRUE){
      options(error=NULL)
      message(" ")
        stop(" Irfdraws must not contain missing values.\n", call. = FALSE)
    }

    #type
    if(type!="median" & type!="mean"){
      options(error=NULL)
      message(" ")
        stop("IRF type must be  mean or  median.\n", call. = FALSE)
    }
    #labels
    lbll <- length(labels)

    if(lbll < dim(irfdraws)[3]){
           options(error=NULL)
      message(" ")
        stop("Number of labels must be equal number of variables in the model.\n", call. = FALSE)   
    }
    #bands >0 <1 length 2 
    bndsl <- length(bands)

    if(bndsl !=2){
           options(error=NULL)
      message(" ")
        stop("Error bands must contain only two values c(lower, upper).\n", call. = FALSE)   
    }


   if(max(bands)>=1 | min(bands)<=0){
           options(error=NULL)
      message(" ")
        stop("Error bands must be between 0 and 1.\n", call. = FALSE)   
    }
    return()
}#end of function 

#--- END OF SCRIPT ---#
