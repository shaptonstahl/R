# nderiv: numerically differentiate a function at a point
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://raw.github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/nderiv/nderiv.R")

nderiv <- function(a, f, pm=.01, ...) {
  # Evaluates the approximate gradient of a function 'f' at a point 'a'.
  # If 'a' is a vector, the derivative is evaluated at each of the values of 'a'.
  # Other arguments can be passed to 'f' in the '...'.
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://www.haptonstahl.org/R/nderiv/nderiv.R")
  return( (f(a + pm) - f(a - pm)) / (2 * pm) )
}

# g <- function(x) x^2
# nderiv(a=2, f=g)
# nderiv(a=c(1:3), f=g)

ngrad <- function(a, f, pm=.01, ...) {
  # Evaluates the approximate gradient of a function 'f' at a point 'a'.
  # Other arguments can be passed to 'f' in the '...'.
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://www.haptonstahl.org/R/nderiv/nderiv.R")
  sapply(1:length(a), function(i) {
    a.plus <- a.minus <- a
    a.plus[i] <- a.plus[i] + pm
    a.minus[i] <- a.minus[i] - pm
    return( (f(a.plus, ...) - f(a.minus, ...)) / (2 * pm) )
  })
}

# h <- function(x) x[1]+2*x[2]+3*x[3]
# ngrad(a=c(1,1,1), f=h)
