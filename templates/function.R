# Template for a new function

#' Short description of the function
#'
#' A longer description of the function.  This can be perhaps
#' a paragraph, perhaps more than one.
#' 
#' @param arg1 Object type, then descrioption of \code{arg1}.
#' @param arg2 Object type, then descrioption of \code{arg2}.
#' @param arg3 Object type, then descrioption of \code{arg3}.
#' @return Object type, then description
#' @export
#' @seealso \code{\link{Related function}}
#' @references
#' Include papers, Web sites, etc.
#' \url{http://www.haptonstahl.org/R}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' data(iris)   # provides example data
#' x <- Function(1, 2)     # Does something simple
#' x                       # Display result
#' 
#' y <- Function(1, 2, 3)  # Does something more complicated
#' y                       # Display result
Function <- function(arg1,
                     arg2,
                     arg3=arg1) {
  # Guardians
  stopifnot(class(arg1) == "this" && length(arg1) == 1 && arg1 >= 0 && arg1 <= 1,
            class(arg2) == "that",
            class(arg3) == "this"
    )
  
  # deal with default and missing values
  
  # perform the function
  
  # prepare and return the output
  out <- NULL
  return(out)
}