# WhereIsMax
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://raw.github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/WhereIsMax/WhereIsMax.R")

WhereIsMax <- function(X) {
  # Return indices of maximum entry
  stopifnot(is.matrix(X))
  linear.index.of.max <- which.max(X)
  i <- linear.index.of.max %% nrow(X)
  if(i == 0) i <- nrow(X)
  j <- ceiling(linear.index.of.max / ncol(X))
  return(c(i, j))
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
