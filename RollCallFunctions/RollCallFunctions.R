#' Functions to manipulate roll call data
#'
#' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
#' Source: https://raw.github.com/shaptonstahl/R
#'
#' Call with:
#'   source("http://www.haptonstahl.org/R/RollCallFunctions/RollCallFunctions.R")

#' AgreementScores: Given m x n matrix of m legislators and n roll call votes, 
#'   returns m x m symetric matrix with fraction of votes on which 
#'   ij^th legislators agree.
#' ChangeIdentification: Given a vector of ideal points identified with 
#'   old.peg.values, convert to a vector of ideal points identified with new.peg.values.
#' DoubleCenterSqrdDist: Given m x n matrix of m legislators and n roll call 
#'   votes, returns m x m symetric matrix with double-centered distances.
#' DropIdealLegislator: Given the results of a call to 'pscl::ideal' and a 
#'   list of legislator ids return the results having dropped the listed
#'   legislators. Used to remove legislators inserted to force identification.
#' InitializeIdeals: Given an object of class rollcall(pscl) return good 
#'   initial estimates for the ideal points and bill parameters
#' PegMinMax: Rescale so min and max are set values
#' RollCallEigen: Given a rollcall  object return the relevant eigenvalues.

source("http://www.haptonstahl.org/R/usePackage/usePackage.R")    #' like 'library' except that it first installs the package if necessary
UsePackage("pscl")

AgreementScores <- function(votes) {
  #' Given m x n matrix of m legislators and n roll call votes,
  #' returns m x m symetric matrix with fraction of votes on which 
  #' ij^th legislators agree.
  #'
  #' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  #' source("http://www.haptonstahl.org/R/RollCallFunctions/RollCallFunctions.R")
  
  #' recode, identifying vote codes by group
  votes[votes==2] <- 1
  votes[votes==3] <- 1
  votes[votes==5] <- 4
  votes[votes==6] <- 4
  votes[votes==8] <- 7
  votes[votes==9] <- 7
  
  #' initialize output
  out <- diag(1, nrow(votes))
  
  #' generate bottom half, copy to top half
  for(i in 2:nrow(votes)) {
    for(j in 1:(i-1)) {
      out[i,j] <- mean(votes[i,]==votes[j,])
      out[j,i] <- out[i,j]
    }
  }
  return(out)
}

ChangeIdentification <- function(x, old.peg.values, new.peg.values) {
  #' Given a vector or matrix of 1-d ideal points identified with old.peg.values, 
  #' convert to a vector or matrix of ideal points identified with new.peg.values.
  #'
  #'  ex: rescaled.ideals <- ChangeIdentification(ideals, c(-1,1), c(-2,2))
  #'
  #' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  #' source("http://www.haptonstahl.org/R/RollCallFunctions/RollCallFunctions.R")
  return( (x - old.peg.values[1]) / (old.peg.values[2] - old.peg.values[1]) * (new.peg.values[2] - new.peg.values[1]) + new.peg.values[1] )
}

DoubleCenterSqrdDist <- function(votes) {
  #' Given m x n matrix of m legislators and n roll call votes,
  #' returns m x m symetric matrix with double-centered distances
  #'
  #' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  #' source("http://www.haptonstahl.org/R/RollCallFunctions/RollCallFunctions.R")
  work <- AgreementScores(votes)
  work <- (1 - work)^2
  out <- sweep(work, 1, rowMeans(work))
  out <- sweep(out, 2, colMeans(work))
  out <- sweep(out, c(1,2), -mean(work))
  return(out / -2)
}

DropIdealLegislator <- function(ideal.output, legs.to.drop) {
  #' Given the results of a call to 'pscl::ideal' and a 
  #' list of legislator ids return the results having dropped the listed
  #' legislators. Used to remove legislators inserted to force identification.
  
  # Clean up list of legislators to drop
  legs.to.drop <- legs.to.drop[legs.to.drop > 0 & legs.to.drop <= ideal.output$n]
  legs.to.drop <- sort(unique(legs.to.drop))
  n.legs.to.drop <- length(legs.to.drop)
  
  legs.to.keep <- rep(TRUE, ncol(ideal.output$x))
  legs.to.keep[legs.to.drop] <- FALSE
  
  out <- ideal.output
  out$n <- out$n - n.legs.to.drop
  out$x <- out$x[,legs.to.keep,]
  out$xbar <- out$xbar[legs.to.keep,]
  
  return( out )
}
  
IdentifyXNormalized <- function(x, positive.peg=max(x)) {
  old.peg.values <- c(min(x), positive.peg)
  new.peg.values <- (old.peg.values - mean(x)) / sd(x)
  return( ChangeIdentification(x, old.peg.values, new.peg.values) )
}

InitializeIdeals <- function(rc, anchors, anchor.values=cbind(c(-1, rep(0, d-1)), diag(d)), d=1, lop=.005) {
  #' Given an object of class rollcall(pscl) return good initial 
  #' estimates for the ideal points and bill parameters
  #' 
  #' 'anchors' is a length = d+1 integer vector of indices of the 
  #' legislators whose ideal points will be fixed for identification.
  #' 
  #' anchor.values is a d x (d+1) matrix of whose columns are the 
  #' images of the ideal points specified by 'anchors'.
  #'
  #' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  #' source("http://www.haptonstahl.org/R/RollCallFunctions/RollCallFunctions.R")
  
  if( class(rc) != "rollcall" ) stop("Object 'rc' must be of class rollcall(pscl).")
  if( !missing(anchors) && length(anchors) != d+1 ) stop("Exactly d+1 legislators' ideal points must be specified for identification.")
  
  purged.rc <- dropRollCall(rc, dropList=list(lop=ceiling(rc$n * lop)))
  ds.agreement <- DoubleCenterSqrdDist(purged.rc$votes)
  
  eg <- eigen(ds.agreement)
  
  x <- sapply(1:d, function(this.d) eg$vectors[,this.d] / sqrt(eg$values[this.d]))
  if( missing(anchors) ) {
    anchors <- min(which(x[,1] == min(x[,1])))
    for(this.d in 1:d) {
      next.value <- (x[-anchors,this.d])[min(which(x[-anchors,this.d] == max(x[-anchors,this.d])))]
      anchors[this.d+1] <- min(which(x[,this.d] == next.value))
    }
  }
  c.1 <- anchor.values[,1]
  x.1 <- x[anchors[1],]
  c.a <- t(matrix(anchor.values[,2:(d+1)], ncol=d)) - matrix(rep(anchor.values[,1], d), ncol=d)
  x.a <- t(matrix(x[anchors[-1],], ncol=d)) - matrix(rep(x.1, d), ncol=d)
  
  scaling.matrix <- t(solve(t(x.a), t(c.a)))
  scaled.x <- apply(x, 1, function(this.x) c.1 + scaling.matrix %*% (this.x - x.1))
  if(1 == d) { 
    scaled.x <- matrix(scaled.x, ncol=1)
  } else {
    scaled.x <- t(scaled.x)
  }
  
  #' Now we have the initial estimates of the ideal points.
  #' Initial estimates of the bill parameters come from a sequence
  #' of probits.
  y <- matrix(ifelse(purged.rc$votes %in% purged.rc$codes$yea, 1, ifelse(purged.rc$votes %in% purged.rc$codes$nay, 0, NA)), nrow=purged.rc$n)
  bill.params <- apply(y, 2, function(this.y) {
    glm.result <- glm(this.y ~ ., family=binomial(link="probit"), data=data.frame(scaled.x))
    if( glm.result$converged ) {
      return( - glm.result$coef )
    } else {
      return( rep(0, d+1) )
    }
  })
  row.names(bill.params) <- c("alpha", paste("beta", 1:d, sep=""))
  
  return( list(ideal.points=scaled.x, bill.params=bill.params) )
}
#' res <- InitializeIdeals(rc); res

PegMinMax <- function(x, new.peg.values=c(-1,1)) {
  #' Rescale so min and max are set values
  return( ChangeIdentification(x, c(min(x), max(x)), new.peg.values) )
}

RollCallEigen <- function(rc, lop=0.005) {
  #' Given a rollcall  object return the relevant eigenvalues.
  #' 
  #' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  #' source("http://www.haptonstahl.org/R/RollCallFunctions/RollCallFunctions.R")
  purged.rc <- dropRollCall(rc, dropList=list(lop=ceiling(rc$n * lop)))
  eigen(DoubleCenterSqrdDist(purged.rc$votes))$values
}
