# getWorkunit
# Work with PHP and MySQL to select a unit of work
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/getWorkunit/getWorkunit.R")

getWorkunit <- function(project, get.from.url, pw="") {
  # Work with PHP and MySQL to select a unit of work
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://www.haptonstahl.org/R/getWorkunit/getWorkunit.R")
  if( missing(project) | missing(get.from.url) ) stop("Must specify project title and URL")
  library(RCurl)
  return( postForm(uri=get.from.url, pw=pw, project=project) )
}
