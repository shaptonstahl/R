# SortedDataFrame
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://raw.github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/SortedDataFrame/SortedDataFrame.R")

#####  SortedDataFrame  #####
#' Sort the rows of a data.frame
#' 
#' Given a data.frame and the names or indices of some columns
#' return the same data.frame except the rows are sorted by the
#' columns specified
#'
#' @param X Object type, then description of \code{arg1}.
#' @param column.names character, ordered vector of names of columns to sort on
#' @param column.indices numeric, ordered vector of indices of columns to sort on
#' @return data.frame, X with rows sorted
#' @export
#' @seealso \code{\link{Related function}}
#' @references
#' Include papers, Web sites, etc.
#' \url{http://www.haptonstahl.org/R}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' data(iris)   # provides example data
#' 
#' SortedDataFrame(iris, column.names="Sepal.Length")
#' SortedDataFrame(iris, column.indices=1)               # These two commands do the same thing
#' 
#' SortedDataFrame(iris, column.names=c("Petal.Length", "Sepal.Width"))
#' SortedDataFrame(iris, column.indices=c(3,2))                            # These two commands do the same thing
#' 
SortedDataFrame <- function(X,
                            column.names="",
                            column.indices=1) {
  # Guardians
  is.count <- function(x) {
    return( is(x, "numeric") && length(x) == 1 && x >= 1 && x %% 1 == 0 )
  }
  stopifnot(is(X, "data.frame"),
            is(column.names, "character"),
            all(sapply(column.indices, is.count))
  )
  
  # deal with default and missing values
  if( missing(column.names) ) {
    column.indices <- unique(column.indices)
    column.indices <- column.indices[column.indices %in% 1:ncol(X)]
  } else {
    column.names <- unique(column.names)
    column.names <- column.names[column.names %in% colnames(X)]
    column.indices <- sapply(column.names, function(this.name) which(this.name == colnames(X)))
  }
  # perform the function
  out <- X[do.call(order, lapply(column.indices, function(this.i) X[,this.i])),]
  
  # prepare and return the output
  return(out)
}
#####
