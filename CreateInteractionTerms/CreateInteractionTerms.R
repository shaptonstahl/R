#' CreateInteractionTerms
#' Create interaction terms up to a degree specified for all the variables in a data.frame
#' 
#' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
#' Source: https://github.com/shaptonstahl/R
#'
#' Call with:
#'   source("http://www.haptonstahl.org/R/CreateInteractionTerms/CreateInteractionTerms.R")


library("combinat")
library("plyr")

CreateInteractionTerms <- function(X, degree=2) {
  #' create lsit of columns for expand.grid with values of the degrees needed
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
  return(as.data.frame(out))
}
#' X.test <- data.frame(a=1:3, b=2:4)
#' CreateInteractionTerms(X.test)
#' CreateInteractionTerms(X.test, degree=3)
#' CreateInteractionTerms(X.test, degree=5)