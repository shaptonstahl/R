# Test the parallel regressions assumption for an ordinal model fit using polr(MASS)
# May 15, 2010
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://raw.github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/PolrParallelRegressions/PolrParallelRegressions.R")

PolrParallelRegressions <- function(x) {
  # guardians
  if( 'polr' != class(x) ) stop('x must be an object returned by polr(MASS)')

  return( TRUE )
}
