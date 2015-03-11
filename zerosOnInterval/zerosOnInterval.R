# zerosOnInterval
# Try to find all of the zeros of a function on a bounded interval
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/zerosOnInterval/zerosOnInterval.R")

zerosOnInterval <- function(f, 
  lower=0, upper=1,
  delta.in=.01,
  sig.dig=8,
  max.zeros=Inf) 
{
  # Given a function f, search the interval [lower, upper] for zeros.
  # Start by evaluating the sign of f at seq(from=lower, to=upper, by=delta.in).
  # If there is a sign change in an interval, use binary search to find root(s) in that interval.
  # You can set max.zeros to constrain how many it will search for; this can speed up the
  # algorithm by letting it stop once it has found this number of zeros.
  # Input values are estimated to sig.dig digits of accuracy.
  # 
  # The function f can be any function from [lower,upper] to the real numbers; no derivatives
  # are necessary or used.
  # 
  # Returns a numeric vector containing the input values x such that f(x) == 0.
  # If no zeros are found, returns numeric(0).
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://www.haptonstahl.org/R/zerosOnInterval/zerosOnInterval.R")
  
  # Trap errors
  if(upper <= lower) stop("'upper' must be greater than 'lower'.")
  
  if( upper-lower < abs(lower)/(10^(sig.dig+1)) ) # See if lower to upper is less than desired accuracy
  {
    if( f(upper) * f(lower) < 0 ) return( (upper-lower)/2 )
    else return( numeric(0) )
  }  # After this we know to search between lower and upper
  
  
  
}
