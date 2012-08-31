# RoundNicely and RoundBoundsNicely
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Load using: source("http://sheer.ucdavis.edu/svn/software/public/RoundBoundsNicely/RoundBoundsNicely.R")

RoundNicely <- function(x, down=FALSE) {
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://sheer.ucdavis.edu/svn/software/public/RoundBoundsNicely/RoundBoundsNicely.R")
  if(length(x) > 1) {
    return(sapply(x, function(v) roundNicely(v, down)))
  } else {
    if(sign(x) == 0) return(0)
    expOnTen <- floor(log10(abs(x)))
    mantissa <- abs(x)/10^expOnTen
    if(sign(x) < 0) down <- !down
    if(down) {
      if(mantissa < 1.25) mantissa <- 1
      else mantissa <- floor(2 * mantissa) / 2
    } else {
      if(mantissa < 1.25) mantissa <- 1.25
      else mantissa <- ceiling(2 * mantissa) / 2
    }
    return(sign(x) * mantissa * 10^expOnTen)
  }
}

RoundBoundsNicely <- function(x) {
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://sheer.ucdavis.edu/svn/software/public/RoundBoundsNicely/RoundBoundsNicely.R")
  
  # Given a length=2 vector, return slightly larger but nicer bounds.
  
  if ( length(x) !=2 ) stop("RoundBoundsNicely is intended to work on vectors of length 2.")
  return( c(RoundNicely(x[1], down=TRUE), RoundNicely(x[2], down=FALSE)) )
}

RoundDrawsNicely <- function(x) {
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://sheer.ucdavis.edu/svn/software/public/RoundBoundsNicely/RoundBoundsNicely.R")
  
  # Given something numeric, calculate the range, then round the bounds nicely to a 
  # range slightly larger.

  if( !is.numeric(x) ) stop("Argument must be numeric.")
  
  return( RoundBoundsNicely(range(x)) )
}
