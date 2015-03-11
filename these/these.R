# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/these/these.R")
#' #########################################################

#' inverse of 'which'
#'
#' Given some locations and a length gives a vector of FALSEs 
#' with the length given and with the locations set to TRUE
#' 
#' @param locations numeric vector, listing locations to set to TRUE.
#' @param len numeric, giving length of output.
#' @param default numeric or logical, value for all places except \code{locations}
#' @param set numeric or logical, value for \code{locations}
#' @return vector, all values set to \code{default} except those at \code{locations} set to \code{set}.
#' @export
#' @seealso \code{\link{Related function}}
#' @references
#' Include papers, Web sites, etc.
#' \url{http://www.haptonstahl.org/R}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' i <- 10
#' these(i, 20)
#' these(2:5, 50)
these <- function(locations,
                  len,
                  default=FALSE,
                  set=TRUE) {
  # Guardians
  stopifnot(is(locations, "numeric"),
            is(len, "numeric"),
            class(default) == class(set),
            max(locations) <= len,
            1 == length(default),
            1 == length(set),
            1 == length(len)
  )
  # perform the function
  out <- rep(default, len)
  out[locations] <- set
  
  # prepare and return the output
  return(out)
}