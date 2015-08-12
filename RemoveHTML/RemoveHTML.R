#' Remove HTML from a character vector
#'
#' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
#' Source: https://github.com/shaptonstahl/R
#'
#' Call with:
#'   source("http://www.haptonstahl.org/R/RemoveHTML/RemoveHTML.R")

library("XML")
library('stringr')

RemoveHTML <- function(x) {
  #' remove HTML
  out <- str_trim(gsub("<.*?>", "", x))
  #' convert HTML entities
  out <- sapply(out, function(this) {
    if(is.na(this)) return(NA)
    if('' == this) return('')
    xpathApply(htmlParse(paste('<html>', this, '</html>', sep=''), 
                         asText=TRUE),
               "//body//text()", xmlValue)[[1]]
  })
  names(out) <- NULL
  return(out)
}
