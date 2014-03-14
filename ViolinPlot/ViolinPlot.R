# draw a violin plot

ViolinPlot <- function(x, 
                       x.center=NA,
                       col.border="black",
                       col.fill=NA,
                       main="",
                       xlab="",
                       ylab="",
                       lwd.border=1,
                       add=FALSE,
                       bw="nrd0",
                       n=512) {
  # set n larger if when adding
  if(add) {
    plot.y.range <- par("usr")[3:4]
    violin.y.range <- range(x)
    bottom <- max(c(plot.y.range[1], violin.y.range[1]))
    top <- min(c(plot.y.range[2], violin.y.range[2]))
    overlap <- top - bottom
    if(overlap <= 0) stop("Does not intersect current plot")
    n <- round(n * (diff(range(x)) / (top-bottom)))
  }
  d <- density(x, bw=bw, n=n)
  x.c <- ifelse(is.na(x.center), 0, x.center)
  v.l <- x.c - d$y
  v.r <- x.c + d$y
  v.y <- d$x
  if(!add) {
    xlim <- c(x.c - 1.5 * max(d$y), x.c + 1.5 * max(d$y))
    ylim <- range(v.y)
    plot(x=c(v.l, rev(v.r)), y=c(v.y, rev(v.y)),
         type="n",
         main=main, xlab=xlab, ylab=ylab,
         xaxt=ifelse(is.na(x.center), "n", "s"))
  }
  if(!is.na(col.fill)) {
    polygon(x=c(v.l, rev(v.r)), y=c(v.y, rev(v.y)),
            col=col.fill)
  }
  lines(x=c(v.l, rev(v.r)), y=c(v.y, rev(v.y)), 
        col=col.border,
        lwd=lwd.border)
  invisible(list(n=n))
}

# dat <- c(rnorm(100), rnorm(200, mean=4))
# ViolinPlot(dat, lwd.border=2)

# ViolinPlot(dat, lwd.border=2, col.fill="green")

# plot(rnorm(100), rnorm(100))
# res <- ViolinPlot(dat, lwd.border=2, add=TRUE)

# plot(rnorm(100), rnorm(100))
# res <- ViolinPlot(dat, lwd.border=2, add=TRUE, col.fill="lightblue")
