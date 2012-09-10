# is.being.sourced
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://raw.github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/is.being.sourced/is.being.sourced.R")

is.being.sourced <- function() {
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Call with: source("http://www.haptonstahl.org/R/is.being.sourced/is.being.sourced.R")
  #
  # Returns TRUE if called when from a 'source' call, FALSE otherwise.
  
  return( any(grepl("^source\\(", sys.calls())) )
}
