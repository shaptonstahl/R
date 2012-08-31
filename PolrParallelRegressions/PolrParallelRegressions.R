# Test the parallel regressions assumption for an ordinal model fit using polr(MASS)
# Stephen R. Haptonstahl
# May 15, 2010

PolrParallelRegressions <- function(x) {
  # guardians
  if( 'polr' != class(x) ) stop('x must be an object returned by polr(MASS)')

  return( TRUE )
}
