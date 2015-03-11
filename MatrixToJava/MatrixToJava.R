# Save a matrix as a java-formatted two-dimensional array
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/MatrixToJava/MatrixToJava.R")

MatrixToJava <- function(X, file=file.choose(), java.matrix.name="final double X[][]") {
  # Given a matrix 'X', creates a text file 'file' containing Java code 
  # initializing a two-dimensional array containing the matrix and named
  # 'java.matrix.name'.
  if( !is.matrix(X) ) stop("X must be a matrix")
  if( !is.character(java.matrix.name) ) stop("java.matrix.name must be a length-1 character vector")
  if( length(java.matrix.name) != 1 ) stop("java.matrix.name must be a length-1 character vector")
  
  X.out <- paste(java.matrix.name, "= {")
  for( i in 1:(nrow(X)-1) ) {
    new.row <- "    { "
    for( j in 1:(ncol(X)-1) ) {
      new.row <- paste(new.row, X[i,j], ", ", sep="")
    }
    new.row <- paste(new.row, X[i,ncol(X)], " },", sep="")
    X.out <- c(X.out, new.row)
  }
  # Last row of data
  new.row <- "    { "
  for( j in 1:(ncol(X)-1) ) {
    new.row <- paste(new.row, X[i,j], ", ", sep="")
  }
  new.row <- paste(new.row, X[i,ncol(X)], " }", sep="")
  # End the array
  X.out <- c(X.out, new.row, "};")
  
  writeLines(X.out, con=file)
  invisible()
}

# test.X <- matrix(1:9, ncol=3, byrow=TRUE)
# test.X
# MatrixToJava(X=test.X)
