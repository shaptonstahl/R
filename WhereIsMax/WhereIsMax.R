# WhereIsMax
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/WhereIsMax/WhereIsMax.R")

WhereIsMax <- function(X, na.rm=FALSE) {
  # Return indices of maximum entry
  stopifnot(is.array(X))
  return(which(X==max(X, na.rm=na.rm), arr.ind=TRUE))
}

# test.X <- matrix(0, nrow=4, ncol=4)
# test.X[2,3] <- 1
# WhereIsMax(X=test.X)
# 
# test.X <- matrix(0, nrow=4, ncol=4)
# test.X[1,1] <- 1
# WhereIsMax(X=test.X)
# 
# test.X <- matrix(0, nrow=4, ncol=4)
# test.X[4,3] <- 1
# WhereIsMax(X=test.X)
# 
# test.X <- matrix(0, nrow=4, ncol=4)
# test.X[4,1] <- 1
# WhereIsMax(X=test.X)
# 
# test.X <- matrix(0, nrow=4, ncol=4)
# test.X[1,3] <- 1
# WhereIsMax(X=test.X)
