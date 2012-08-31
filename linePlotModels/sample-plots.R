# Sample code for plotting posteriors

source("http://sheer.ucdavis.edu/svn/software/public/linePlotModels/linePlotModels.R")

########################################
###  Example 1: Plot a single model  ###
########################################

# gather columns of posterior draws and put the model in a data.frame
myData <- data.frame(p1=rnorm(1000, 5, 2), p2=rexp(1000, 4), p3=runif(1000, -1, 4))
names(myData) <- c("normal", "exponential", "uniform")

# simple plot
linePlotModels(myData)

# add color and set horizontal range
linePlotModels(myData, 
  xlim=c(-5,15),
  colLines="blue"
)

#########################################
###  Example 2: Plot multiple models  ###
#########################################
myData <- list(
  m1=data.frame(p1=rnorm(1000, 5, 2), p2=rexp(1000, 4), p3=runif(1000, -1, 4)),
  m2=data.frame(p1=rnorm(1000, 4, 1), p2=rexp(1000, 5), p3=runif(1000, -2, 3))
)

# plot with two colors
linePlotModels(myData, 
  xlim=c(-5,15),
  colLines=c("blue", "red")
)

# Add nice variable names, change the symbols, make the lines thicker, 
# and generate a PDF file ready for inclusion in LaTeX

postscript("sample-plot.eps", height=2.5, width=4.5)

  linePlotModels(myData, 
    xlim=c(-5,15),
    colLines=c("blue", "red"),
    fancyVarNames=c("Normal", "Exponential", "Uniform"),
    pchModels=c(21,22),
    lwd=2
  )

dev.off()
