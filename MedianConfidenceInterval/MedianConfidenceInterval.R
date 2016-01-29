#' MedianConfidenceInterval and QuantileConfidenceInterval
#'
#' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
#' Source: https://github.com/shaptonstahl/R
#'
#' Call with:
#'   source("http://www.haptonstahl.org/R/MedianConfidenceInterval/MedianConfidenceInterval.R")
#' 
#' Ref: http://www.milefoot.com/math/stat/ci-medians.htm

MedianConfidenceInterval <- function(x, conf=.95) {
  #' Given a set of numbers x return a confidence interval for the median
  QuantileConfidenceInterval(x, p=.5, conf=conf)
}

QuantileConfidenceInterval <- function(x, p=.5, conf=.95) {
  #' Given a set of numbers x and a quantile p return a confidence interval
  #' for that quantile
  stopifnot(is.numeric(x))
  n <- length(x)
  y <- sort(as.vector(x))
  interval.probabilities <- dbinom(0:n, size=n, prob=p)
                                         #' i as an index is in the i+1st position 
                                         #' in this vector so i=5 is in the 6th 
                                         #' position
  sorted.indices.for.ips <- sort(interval.probabilities, decreasing=TRUE, index.return=TRUE)$ix
  num.needed.intervals <- min(which(cumsum(interval.probabilities[sorted.indices.for.ips]) > conf))
  resulting.conf <- cumsum(interval.probabilities[sorted.indices.for.ips])[num.needed.intervals]
  keep.indices.ips <- sorted.indices.for.ips[1:num.needed.intervals]
  if(1 %in% keep.indices.ips) {
    if((n+1) %in% keep.indices.ips) {
      #' conf int has no upper or lower bound in the sample
      lower <- -Inf
      upper <- Inf
    } else {
      #' conf interval has no lower bound in the sample
      lower <- -Inf
      upper <- y[max(keep.indices.ips)]
    }
  } else if((n+1) %in% keep.indices.ips) {
    #' conf int has no upper bound in the sample
    lower <- y[min(keep.indices.ips) - 1]
    upper <- Inf
  } else {
    lower <- y[min(keep.indices.ips) - 1]
    upper <- y[max(keep.indices.ips)]
  }
  return(list(lower=lower, upper=upper, conf=resulting.conf))
}

#' Examples from the reference:
#' x <- c(24, 38, 61, 22, 16, 57, 31, 29, 35)
#' MedianConfidenceInterval(x, conf=.8)
#' QuantileConfidenceInterval(x, p=.75, conf=.8)
