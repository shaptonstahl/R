# MapFeatures: given some variables creates interactions terms
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/MapFeatures/MapFeatures.R")

MapBinaryFeatures <- function(X, degree=2) {
  #' Given a numeric matrix or data.frame with at least 
  #' two columns returns the same with all the two-column 
  #' interaction terms up to degree `degree`, including a column 
  #' of 1s in the first column.
  #' 
  #' Caution: the output grows exponentially with degree and the
  #' number of columns, so choose smaller degrees.  The number of 
  #' columns returned is
  #' 
  #' ncol(out) = 1 + degree * (degree-1) * ncol(X) * (ncol(X)-1) / 4
  #' 
  #' so it is quadratic in each of the number of columns and the
  #' degree.
  if(is.data.frame(X)) {
    X <- as.matrix(X)
    was.data.frame <- TRUE
  } else{
    was.data.frame <- FALSE
  }
  stopifnot(is.numeric(X))
  stopifnot(ncol(X) > 1)
  stopifnot(degree >= 1)
  
  out <- rep(1, nrow(X))
  
  for(d in 1:degree) {
    out <- cbind(out, X^d)
  }
#  print(out)
  
  for(i in 1:(ncol(X)-1)) {
    for(j in (i+1):ncol(X)) {
      #' Generating on columns i and j
      cat("Generating on columns", i, "and", j, "\n")
      for(term.degree in 2:degree) {
        #' generate terms of degree term.degree
        cat("Generating terms with degree", term.degree, "\n")
        for(i.factor.degree in seq(from=term.degree-1, to=1, by=-1)) {
          out <- cbind(out, X[,i]^i.factor.degree * X[,j]^(term.degree - i.factor.degree))
        }
        print(out)
      }
    }
  }
  if(was.data.frame) out <- as.data.frame(out)
  return(out)
}

MapFeatures <- function(X, degree=2) {
  #' Given a numeric matrix or data.frame with at least 
  #' two columns returns the same with all interaction terms
  #' up to degree `degree`, including a column of 1s in the 
  #' first column.
  #' 
  #' The number of columns returned is huge, so test it's
  #' recommended that you test first on a smaller data set.
  #' 
  if(is.data.frame(X)) {
    X <- as.matrix(X)
    was.data.frame <- TRUE
  } else{
    was.data.frame <- FALSE
  }
  stopifnot(is.numeric(X))
  stopifnot(ncol(X) > 1)
  stopifnot(degree >= 1)
  
  out <- rep(1, nrow(X))
  for(d in 1:degree) {
    cat("Generating terms with degree", d, "\n")
    factor.array <- as.matrix(expand.grid(replicate(d, 1:ncol(X), simplify=FALSE)))
    for(i in 1:nrow(factor.array)) factor.array[i,] <- sort(factor.array[i,])
    deduped.factor.array <- factor.array[!duplicated(factor.array),, drop=FALSE]
    for(i in 1:nrow(deduped.factor.array)) {
      out <- cbind(out, apply(X[,deduped.factor.array[i,], drop=FALSE], 1, prod))
    }
  }
  if(was.data.frame) out <- as.data.frame(out)
  return(out)
}
