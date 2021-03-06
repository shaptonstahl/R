# Copy of code posted at 
# http://www.nicebread.de/visually-weighted-watercolor-plots-new-variants-please-vote/
#
# Load using: source("http://www.haptonstahl.org/R/watercolor/watercolor.R")
# 
# (c) 2012 Felix Schönbrodt
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
## Visually weighted regression / Watercolor plots
## Idea: Solomon Hsiang, with ideas from many blog commenters
# B = number bootstrapped smoothers
# shade: plot the shaded confidence region?
# shade.alpha: should the CI shading fade out at the edges? (by reducing alpha; 0 = no alpha decrease, 0.1 = medium alpha decrease, 0.5 = strong alpha decrease)
# spag: plot spaghetti lines?
# mweight: should the median smoother be visually weighted?
# show.lm: should the linear regresison line be plotted?
# show.CI: should the 95% CI limits be plotted?
# show.median: should the median smoother be plotted?
# median.col: color of the median smoother
# method: the fitting function for the spaghettis; default: loess
# bw = TRUE: define a default b&w-palette
# slices: number of slices in x and y direction for the shaded region. Higher numbers make a smoother plot, but takes longer to draw. I wouldn'T go beyond 500
# palette: provide a custom color palette for the watercolors
# ylim: restrict range of the watercoloring
# quantize: either "continuous", or "SD". In the latter case, we get three color regions for 1, 2, and 3 SD (an idea of John Mashey)
# ...: further parameters passed to the fitting function, in the case of loess, for example, "span = .9", or "family = 'symmetric'"
vwReg <- function(formula, data, title="", B=1000, shade=TRUE, shade.alpha=.1, spag=FALSE, mweight=TRUE, show.lm=FALSE, show.median = TRUE, median.col = "white", show.CI=FALSE, method=loess, bw=FALSE, slices=200, palette=colorRampPalette(c("#FFEDA0", "#DD0000"), bias=2)(20), ylim=NULL, quantize = "continuous",  ...) {
IV <- all.vars(formula)[2]
DV <- all.vars(formula)[1]
data <- na.omit(data[order(data[, IV]), c(IV, DV)])
if (bw == TRUE) palette <- colorRampPalette(c("#EEEEEE", "#999999", "#333333"), bias=2)(20)
print("Computing boostrapped smoothers ...")
newx <- data.frame(seq(min(data[, IV]), max(data[, IV]), length=slices))
colnames(newx) <- IV
l0.boot <- matrix(NA, nrow=nrow(newx), ncol=B)
l0 <- method(formula, data)
for (i in 1:B) {
data2 <- data[sample(nrow(data), replace=TRUE), ]
data2 <- data2[order(data2[, IV]), ]
if (class(l0)=="loess") {
m1 <- method(formula, data2, control = loess.control(surface = "i", statistics="a", trace.hat="a"), ...)
} else {
m1 <- method(formula, data2, ...)
}
l0.boot[, i] <- predict(m1, newdata=newx)
}
# compute median and CI limits of bootstrap
library(plyr)
library(reshape2)
CI.boot <- adply(l0.boot, 1, function(x) quantile(x, prob=c(.025, .5, .975, pnorm(c(-3, -2, -1, 0, 1, 2, 3))), na.rm=TRUE))[, -1]
colnames(CI.boot)[1:10] <- c("LL", "M", "UL", paste0("SD", 1:7))
CI.boot$x <- newx[, 1]
CI.boot$width <- CI.boot$UL - CI.boot$LL
# scale the CI width to the range 0 to 1 and flip it (bigger numbers = narrower CI)
CI.boot$w2 <- (CI.boot$width - min(CI.boot$width))
CI.boot$w3 <- 1-(CI.boot$w2/max(CI.boot$w2))
# convert bootstrapped spaghettis to long format
b2 <- melt(l0.boot)
b2$x <- newx[,1]
colnames(b2) <- c("index", "B", "value", "x")
library(ggplot2)
library(RColorBrewer)
p1 <- ggplot(data, aes_string(x=IV, y=DV)) + theme_bw()
if (shade == TRUE) {
quantize <- match.arg(quantize, c("continuous", "SD"))
if (quantize == "continuous") {
print("Computing density estimates for each vertical cut ...")
flush.console()
if (is.null(ylim)) {
min_value <- min(min(l0.boot, na.rm=TRUE), min(data[, DV], na.rm=TRUE))
max_value <- max(max(l0.boot, na.rm=TRUE), max(data[, DV], na.rm=TRUE))
ylim <- c(min_value, max_value)
}
# vertical cross-sectional density estimate
d2 <- ddply(b2[, c("x", "value")], .(x), function(df) {
res <- data.frame(density(df$value, na.rm=TRUE, n=slices, from=ylim[1], to=ylim[2])[c("x", "y")])
#res <- data.frame(density(df$value, na.rm=TRUE, n=slices)[c("x", "y")])
colnames(res) <- c("y", "dens")
return(res)
}, .progress="text")
maxdens <- max(d2$dens)
mindens <- min(d2$dens)
d2$dens.scaled <- (d2$dens - mindens)/maxdens
## Tile approach
d2$alpha.factor <- d2$dens.scaled^shade.alpha
p1 <- p1 + geom_tile(data=d2, aes(x=x, y=y, fill=dens.scaled, alpha=alpha.factor)) + scale_fill_gradientn("dens.scaled", colours=palette) + scale_alpha_continuous(range=c(0.001, 1))
}
if (quantize == "SD") {
## Polygon approach
SDs <- melt(CI.boot[, c("x", paste0("SD", 1:7))], id.vars="x")
count <- 0
d3 <- data.frame()
col <- c(1,2,3,3,2,1)
for (i in 1:6) {
seg1 <- SDs[SDs$variable == paste0("SD", i), ]
seg2 <- SDs[SDs$variable == paste0("SD", i+1), ]
seg <- rbind(seg1, seg2[nrow(seg2):1, ])
seg$group <- count
seg$col <- col[i]
count <- count + 1
d3 <- rbind(d3, seg)
}
p1 <- p1 + geom_polygon(data=d3, aes(x=x, y=value, color=NULL, fill=col, group=group)) + scale_fill_gradientn("dens.scaled", colours=palette, values=seq(-1, 3, 1))
}
}
print("Build ggplot figure ...")
flush.console()
if (spag==TRUE) {
p1 <- p1 + geom_path(data=b2, aes(x=x, y=value, group=B), size=0.7, alpha=10/B, color="darkblue")
}
if (show.median == TRUE) {
if (mweight == TRUE) {
p1 <- p1 + geom_path(data=CI.boot, aes(x=x, y=M, alpha=w3^3), size=.6, linejoin="mitre", color=median.col)
} else {
p1 <- p1 + geom_path(data=CI.boot, aes(x=x, y=M), size = 0.6, linejoin="mitre", color=median.col)
}
}
# Confidence limits
if (show.CI == TRUE) {
p1 <- p1 + geom_path(data=CI.boot, aes(x=x, y=UL, group=B), size=1, color="red")
p1 <- p1 + geom_path(data=CI.boot, aes(x=x, y=LL, group=B), size=1, color="red")
}
# plain linear regression line
if (show.lm==TRUE) {p1 <- p1 + geom_smooth(method="lm", color="darkgreen", se=FALSE)}
p1 <- p1 + geom_point(size=1, shape=21, fill="white", color="black")
if (title != "") {
p1 <- p1 + opts(title=title)
}
p1  + opts(legend.position="none")
}