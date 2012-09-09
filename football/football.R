# football: Draw two circles and shade the region of intersection
# Hack solution
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://raw.github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/football/footballt.R")

football <- function(x, y, r, fdensity=NULL, fcol='blue', nv=10000) {
  # x = vector of circle center x-coordinates
  # y = vector of circle center y-coordinates
  # r = vector of circle radii
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://sheer.ucdavis.edu/svn/software/public/football/football.R")
  
  # plot circles
  library(plotrix)
  p1 <- draw.circle(x[1], y[1], r[1], nv=nv)
  p2 <- draw.circle(x[2], y[2], r[2], nv=nv)
  keep.p1 <- sapply(1:length(p1$x), function(i) {
    test.pt <- c(p1$x[i], p1$y[i])
    return( sum((test.pt-c(x[2],y[2]))^2) < r[2]^2 )
  })
  keep.p2 <- sapply(1:length(p2$x), function(i) {
    test.pt <- c(p2$x[i], p2$y[i])
    return( sum((test.pt-c(x[1],y[1]))^2) < r[1]^2 )
  })
  football.x <- c(p1$x[keep.p1], p2$x[keep.p2])
  football.y <- c(p1$y[keep.p1], p2$y[keep.p2])
  polygon(football.x, football.y, density=fdensity, col=fcol)
}

# Example:
# plot(-2:2, -2:2, type='n')
# football(c(-1,0), c(1,0), c(.5,1))
