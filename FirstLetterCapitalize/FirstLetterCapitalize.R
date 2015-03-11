# FirstLetterCapitalize
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/FirstLetterCapitalize/FirstLetterCapitalizet.R")

FirstLetterCapitalize <- function(x, 
                                  lower.except.inner=c("a", "an", "the", 
                                                       "and", "or", "but", "as", "both", "for", "how", "if", "nor", "now", "so", "than", "that", "till", "when", "yet",
                                                       "to", "at", "in", "with", "by", "down", "from", "into", "lest", "like", "mid", "near", "next", "of", "off", "on", "onto", "out", "over", "pace", "past", "per", "plus", "pro", "qua", "sans", "save", "than", "unto", "up", "upon", "via", "vice", "with", "ere"),
                                  lower.always,
                                  upper.always) {
  stopifnot(is.character(x <- as.character(x)),
            is.character(lower.except.inner),
            missing(lower.always) || is.character(lower.always),
            missing(upper.always) || is.character(upper.always) )
  out <- gsub(pattern="([a-zA-Z])(\\w+)", 
              replacement="\\U\\1\\E\\L\\2\\E", 
              x, perl=TRUE)
  out <- gsub(pattern=paste("([^^]+)(", paste(lower.except.inner, collapse="|"), ")([^$]+)", sep=""), 
              replacement="\\1\\L\\2\\E\\3", 
              ignore.case=TRUE,
              x=out, perl=TRUE)
  if( !missing(lower.always) ) {
    out <- gsub(pattern=paste("\\b(", paste(lower.always, collapse="|"), ")\\b", sep=""), 
                replacement="\\L\\1\\E",
                ignore.case=TRUE,
                x=out, perl=TRUE)
  }
  if( !missing(upper.always) ) {
    out <- gsub(pattern=paste("\\b(", paste(upper.always, collapse="|"), ")\\b", sep=""), 
                replacement="\\U\\2\\E", 
                ignore.case=TRUE,
                x=out, perl=TRUE)
  }
  return( out )
}
