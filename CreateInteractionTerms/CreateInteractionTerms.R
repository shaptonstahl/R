#' CreateInteractionTerms
#' Create interaction terms up to a degree specified for all the variables in a data.frame
#' 
#' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
#' Source: https://github.com/shaptonstahl/R
#'
#' Call with:
#'   source("http://www.haptonstahl.org/R/CreateInteractionTerms/CreateInteractionTerms.R")
#'   
#'   Provides:
#'   - CreateInteractionTerms
#'   - ExtractInteractionExponents

library("plyr")

CreateInteractionTerms <- function(X, degree=2, remove.zero.variance=TRUE) {
  #' Given a data.frame with numeric entries create a data frame with
  #' columns representing every element-wise product of columns up to degree
  #' at a time.
  
  #' warn if there are numbers in the column names
  if(any(grepl('[0-9]', names(X)))) warning('You have numbers in the column names. You will not be able to automatically extract the exponents after the fact using ExtractInteractionExponents')
  
  #' create list of columns for expand.grid with values of the degrees needed
  columns <- lapply(1:ncol(X), function(i) 0:degree)
  names(columns) <- names(X)
  #' create all combinations including those of too high degree
  all <- expand.grid(columns)
  #' limit to those with desired degree or less
  keep <- all[rowSums(all) <= degree & rowSums(all) > 0,]
  #' define function to take column-wise product of selected variables
  rowProd <- function(x) apply(x, 1, prod)
  #' The inner sapply takes one row of keep and plucks out the desired variables
  #' to the desired powers.  The row prod takes the plucked out variables for one
  #' row of keep and takes the element-wise product.  The outer sapply iterates over 
  #' the rows of keep to get every desired combination.
  out <- sapply(1:nrow(keep), function(i) 
    rowProd(sapply(1:ncol(keep), function(j) 
      X[,j]^keep[i,j])))
  #' Let's get the names the same way, shall we?
  colnames(out) <- sapply(1:nrow(keep), function(i) 
    paste(sapply(1:ncol(keep), function(j) 
      paste(names(X)[j], keep[i,j], sep='')), collapse='.'))
  if(remove.zero.variance) {
    out <- out[,(apply(out, 2, sd) > 0)]
  }
  return(as.data.frame(out))
}
#' X.test <- data.frame(a=1:3, b=2:4)
#' CreateInteractionTerms(X.test)
#' CreateInteractionTerms(X.test, degree=3)
#' CreateInteractionTerms(X.test, degree=5)

ExtractInteractionExponents <- function(x) {
  #' Given a vector of column names resulting from CreateInteractionTerms
  #' return a data.frame where the columns are the constituent variables,
  #' the rows are the columns of the interaction term set, and the values
  #' are the exponents.
  
  var.names <- strsplit(x[1], '[0-9]+\\.')[[1]] # last will have number still attached
  n.vars <- length(var.names)
  var.names[n.vars] <- sub('[0-9]+$', '', var.names[n.vars])
  
  x <- substring(x, nchar(var.names[1]) + 1)
  exponent.nchar <- regexpr('[^0-9]', x) - 1
  out <- data.frame(as.numeric(substring(x, 1, exponent.nchar)))
  if(length(var.names) > 1) {
    for(var.name in var.names[2:n.vars]) {
      x <- substring(x, exponent.nchar + nchar(var.name) + 2)
      if(var.name == var.names[n.vars]) {
        out <- cbind(out, as.numeric(x))
      } else {
        exponent.nchar <- regexpr('[^0-9]', x) - 1
        out <- cbind(out, as.numeric(substring(x, 1, exponent.nchar)))
      }
    }
  }
  names(out) <- var.names
  return(out)
}
#' X.test <- data.frame(a=1:3, b=2:4)
#' inter.test <- CreateInteractionTerms(X.test)
#' ExtractInteractionExponents(inter.test)
