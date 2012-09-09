# guid: generate globally unique identifiers
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://raw.github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/guid/guid.R")

guid <- function(n=1, length=32, all.numeric=FALSE) {
  if(all.numeric) char.list <- 0:9
  else char.list <- c(letters, 0:9)
  sapply(1:n, function(i) paste(sample(char.list, length, replace=TRUE), collapse=""))
}
