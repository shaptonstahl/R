#' Plot the location and distribution of clusters of data
#'
#' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
#' Source: https://github.com/shaptonstahl/R
#'
#' Call with:
#'   source("http://www.haptonstahl.org/R/PlotClusterMetadata/PlotClusterMetadata.R")

source("http://www.haptonstahl.org/R/RoundBoundsNicely/RoundBoundsNicely.R")
library('ellipse')

PlotClusterMetadata <- function(membership,
                                meta.x,
                                meta.y,
                                xlab='', ylab='', main='',
                                color.ends = c('red', 'blue'),
                                legend.xy) {
  classes <- sort(unique(membership))
  n.classes <- length(classes)
  colors <- colorRampPalette(color.ends)(n.classes)
  
  plot(1, 1,
       xlim=RoundDrawsNicely(meta.x),
       ylim=RoundDrawsNicely(meta.y),
       xlab=xlab, ylab=ylab, main=main)
  for(i in 1:n.classes) {
    this.class <- classes[i]
    this.x <- meta.x[membership==this.class]
    this.y <- meta.y[membership==this.class]
    this.center <- c(mean(this.x, na.rm=TRUE), mean(this.y, na.rm=TRUE))
    this.cov <- cov(this.x, this.y)
    this.ellipse <- ellipse(x=this.cov, centre=this.center)
    lines(this.ellipse, lwd=2,
          col=colors[(i-1)/(n.classes-1)])
    points(this.center[1], this.center[2], 
           pch=2, col=colors[(i-1)/(n.classes-1)])
  }
  if(missing(legend.xy)) {
    legend.xy <- c(min(meta.x) + .1 * (max(meta.x) - min(meta.x)),
                   min(meta.y) + .9 * (max(meta.y) - min(meta.y)))
  }
  legend(legend.xy, legend=classes,
         col=sapply(seq(0,1,length.out=n.classes), color.function),
         lwd=2, pch=2)
  invisible(NULL)
}

