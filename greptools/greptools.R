# tools to make grep in R easier to use
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/greptools/greptools.R")

cat("\ngreptools by Stephen Haptonstahl (srh@haptonstahl.org)\n\n")

grepstring <- function(pattern, x, ...) {
  #' Given a regular expression and a character vector
  #' return the first match in each element or "" if there 
  #' is no match
  return(sapply(x, function(this.x) {
    re <- regexpr(pattern, this.x, ...)
    if(re > 0) {
      return(substring(this.x, re, re + attr(re, "match.length") - 1))
    } else {
      return("")
    }
  }))
}
