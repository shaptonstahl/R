# insertColumn
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/insertColumn/insertColumn.R")

insertColumn <- function(X, new.col.name, after.column=ncol(X), default=NA) {
  # Adds columns in the middle of a data frame
  # 
  #  X            = a data frame to which you want to add columns
  #  new.col.name = a character vector of the names of the columns to add
  #  after.column = an integer in 0:ncol(X) giving the column of X that will be immediately 
  #    to the left of the new columns; alternatively, the exact name of the column that will
  #    be immediately to the left of the new columns
  #  default      = value inserted in the new columns
  # 
  # Returns a data.frame with old and new columns
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://www.haptonstahl.org/R/insertColumn/insertColumn.R")
  
  n.new.cols <- length(new.col.name)
  n.original.cols <- ncol(X)
  original.names <- names(X)
  
  if(is.character(after.column)) {
    possible.after.column <- grep(paste("^", after.column, "$", sep=""), names(X))
    if(0 == length(possible.after.column)) stop("insertColumn: after.column not found in names(X)")
    else after.column <- possible.after.column
  }
  
  if( 0 == after.column ) {
    X <- data.frame(matrix(default, nrow=nrow(X), ncol=n.new.cols), X, stringsAsFactors=FALSE)
    names(X) <- c(new.col.name, original.names)
  } else if( ncol(X) == after.column ) {
    X <- data.frame(X, matrix(default, nrow=nrow(X), ncol=n.new.cols), stringsAsFactors=FALSE)
    names(X) <- c(original.names, new.col.name)
  } else {
    new.names <- c(names(X)[1:after.column], new.col.name, names(X)[(after.column+1):ncol(X)])
    X <- data.frame(X[,1:after.column], matrix(default, nrow=nrow(X), ncol=n.new.cols), X[(after.column+1):ncol(X)], stringsAsFactors=FALSE)
    names(X) <- new.names
  }
  return(X)
}
