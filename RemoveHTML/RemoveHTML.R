#' Remove HTML from a character vector
#'
#' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
#' Source: https://github.com/shaptonstahl/R
#'
#' Call with:
#'   source("http://www.haptonstahl.org/R/RemoveHTML/RemoveHTML.R")

library("XML")

RemoveHTML <- function(x) {
  #' remove HTML
  out <- gsub("<.*?>", "", x)
  #' convert HTML entities
  out <- xpathApply(htmlParse(paste('<html>', out, '</html>', sep=''), 
                              asText=TRUE),
                    "//body//text()", xmlValue)[[1]]
  return(out)
}
