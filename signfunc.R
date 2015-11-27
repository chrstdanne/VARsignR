#--- SIGN FUNCS ---#
# VERSION NOV 15 2015
# CHRISTIAN DANNE
# DANNEC@TCD.IE

#--- MULTIVARIATE NORMAL ---#
mvrnormR <- function(n, mu, sigma){
  ncols <- ncol(sigma)
  mu <- rep(mu, each = n)
  mvndraw <- mu + matrix(rnorm(n * ncols), ncol = ncols) %*% chol(sigma)
  return(mvndraw)
}

#--- STEREOGRAPHIC PROJECTION ---#
stereo <- function(v){
  llength <- length(v)
  ll <- llength + 1
  s <- matrix(NA, nrow=ll, ncol=1)
  for(i in 1:llength){
    s[i] <- 2*v[i] / (sqrt(sum(v^2))^2 +1)
  }
  s[ll] <- (sqrt(sum(v^2))^2 -1) / (sqrt(sum(v^2))^2 +1)
  return(s)
}

#--- UHLIG ACCEPT ---#
UhligAccept <- function(Q, first, last, constrained, impulses){#ok
  for(k in first:last){#ok
    ik <- impulses[k, , ]%*%Q#ok
    for(i in 1:length(constrained)){#ok
      if(constrained[i]<0){#ok
        value <- ik[-1.0 * constrained[i]]#ok
      }else{#ok
        value <- -1.0 * ik[constrained[i]]#ok
      }#ok
      if(value>0.0){#ok
        if(k==first & i==1){#ok
          Q <- -1.0 * Q#ok
          ik <- -1.0 * ik#ok
        }else{#ok
          acc <- 0
          uar <- list(Q=Q, acc=acc)
          return(uar)
        }#ok
      }#ok
    }#end i #ok
  }#end k #ok
  acc <- 1
  uar <- list(Q=Q, acc=acc)
  return(uar)
}

#--- PENALTY FUNCTION ---#
UhligPenalty <- function(g, first, last, constrained, impulses, scales, pen){
  func <- 0.0
  q <- matrix(stereo(v=g))
  for(k in first:last){
    ik <- (impulses[k, , ]%*%q) / scales
    for(i in 1:length(constrained)){
      if(constrained[i]<0){
        value <- ik[-1.0*constrained[i]]
      }else{
        value <- -1.0 * ik[constrained[i]]
      }
      if(value<0){
        func <- func +  value
      }else{
        func <- func + pen * value
      }
    }
  }
  acc <- func
  return(acc)
}

#something's wrong with this one.
# UhligPenalty <- function(g, first, last, constrained, impulses, scales, pen){#ok
#   func <- 0.0 #ok
#   q <- matrix(stereo(v=g)) #that should work in here
#   for(k in first:last){
#     ik <- (impulses[k, , ]%*%q) / scales #ok
#     for(i in 1:length(constrained)){#ok
#       if(constrained[i]<0){#ok
#         value <- ik[-1.0*constrained[i]]#ok
#       }else{#ok
#         value <- -1.0 * ik[constrained[i]]#ok
#         if(value<0){#ok
#           func <- func +  value#ok
#         }else{ #ok
#           func <- func + pen * value #ok
#         } #ok
#       } #ok
#     } #ok
#   }#ok
#   acc <- func #ok
#   return(acc)#ok
# }#ok

#--- RWZ SIGN RESTRICTIONS ---#
RWZAccept <- function(a, first, last, constrained, impulses){
  QR <- qr(a)
  R <- qr.R(QR)
  Q <- qr.Q(QR)
  #
  if(R<0){Q  <- -1.0 * Q}
  #
  for(k in first:last){
    ik <- impulses[k, , ]%*%Q
    for(i in 1:length(constrained)){
      if(constrained[i]<0){
        value <- ik[-1.0*constrained[i]]
      }else{
        value <- -1.0 * ik[constrained[i]]
      }
      #
      if(value<0.0){
        if(k==first & i==1){
          Q <- 1.0 * Q
          ik <- 1.0 * ik
        } # comment this one out and uncomment bracket below.
      }else{
        acc <- 0
        rwz <- list(Q=Q, acc=acc)
        return(rwz)
        # } # comment out this
      }
    }
  }
  acc <- 1
  rwz <- list(Q=Q, acc=acc)
  return(rwz)
}#END OF FUNCTION

#--- PAGAN FRY GAP FUNCTION ---#
PFAccept <- function(g, rescale, targets, imppagan){
  gap <- 0.0
  a <- matrix(stereo(v=g))
  for(k in 1:nstep){
    scal  <-  matrix(rescale[k,] * (targets[k,] - imppagan[k,] %*% a))
    scal <- sum(scal^2)
    gap <- gap + scal
  }
  return(gap)
}

# #--- END OF SCRIPT ---#





