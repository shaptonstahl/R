# Save a matrix as a java-formatted two-dimensional array
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Load using:  source("http://sheer.ucdavis.edu/svn/software/public/MatrixToJava/MatrixToJava.R")

toPHP <- function(X, file=file.choose(), dest.name="X") {
  # Given a vector or matrix 'X', creates a text file 'file' containing PHP 
  # code initializing a one- or two-dimensional array containing the R object
  # and named 'dest.name'.
  
  if( !is.matrix(X) & ! is.vector(X) ) stop("X must be a vector or a matrix")
  if( !is.character(dest.name) ) stop("dest.name must be a length-1 character vector")
  if( length(dest.name) != 1 ) stop("dest.name must be a length-1 character vector")
  
  if( is.vector(X) ) {
    X.out <- paste("$", dest.name, " = array(", sep="")
    for( i in 1:(length(X)-1) ) {
      X.out <- c(X.out, paste("  ", X[i], ",", sep=""))
    }
    X.out <- c(X.out, paste("  ", X[i+1], "", sep=""), ");")
  } else {
    X.out <- paste("$", dest.name, " = array(", sep="")
    for( i in 1:(nrow(X)-1) ) {
      new.row <- "  array( "
      for( j in 1:(ncol(X)-1) ) {
        new.row <- paste(new.row, X[i,j], ", ", sep="")
      }
      new.row <- paste(new.row, X[i,ncol(X)], " ),", sep="")
      X.out <- c(X.out, new.row)
    }
    # Last row of matrix
    new.row <- "  array( "
    for( j in 1:(ncol(X)-1) ) {
      new.row <- paste(new.row, X[nrow(X),j], ", ", sep="")
    }
    new.row <- paste(new.row, X[nrow(X),ncol(X)], " )", sep="")
    X.out <- c(X.out, new.row, ");")
  }
  
  writeLines(X.out, con=file)
  invisible()
}

# test.matrix <- matrix(1:9, ncol=3, byrow=TRUE)
# test.matrix
# toPHP(X=test.matrix)

# test.vector <- 1:9
# toPHP(X=test.vector, dest.name="thingy")
