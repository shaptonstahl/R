# statMode: Calculate the most commonly occuring value
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/statMode/statMode.R")

source("http://www.haptonstahl.org/R/usePackage/usePackage.R")
usePackage("nnet")

statMode <- function(x, break.ties.randomly=F) {
  # Calculate the most commonly occuring value
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://www.haptonstahl.org/R/statMode/statMode.R")
  tx <- table(x)
  if(break.ties.randomly) {
    out <- row.names(tx)[which.is.max(tx)]
  } else {
    out <- row.names(tx)[which.max(tx)]
  }
  mode(out) <- mode(x)
  return( out )
}
