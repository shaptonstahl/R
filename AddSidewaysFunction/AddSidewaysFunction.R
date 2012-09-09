# AddSidewaysFunction
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://raw.github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/AddSidewaysFunction/AddSidewaysFunction.R")

AddSidewaysFunction <- function(f, origin, ylim, max.width, left=FALSE, axis=FALSE, ...) {
  # Plot 'f' sideways with origin at 'origin' over the range 'ylim'.
  # 'max.width' sets the number of units that the mode over the 
  #   plotted range will deviate horizontally from 'origin'
  # Plots on right side unless 'left=TRUE'.
  # ... are passed to 'lines' to set color, width, etc.
  
  plot.y <- seq(from=min(ylim), to=max(ylim), length.out=100)
  x <- plot.y - origin[2]
  y <- f(x)
  y <- y / max(abs(y)) * max.width
  
  if(TRUE == left) {
    plot.x <- origin[1] - y
  } else {
    plot.x <- origin[1] + y
  }
  
  lines(plot.x, plot.y, ...)
  
  if(TRUE==axis) abline(v=origin[1], lty=2, ...)
  
  invisible(NULL)
}
