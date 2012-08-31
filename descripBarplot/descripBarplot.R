# descripBarplot
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Load using: source("http://sheer.ucdavis.edu/svn/software/public/descripBarplot/descripBarplot.R")

descripBarplot <- function(X, 
  yName, 
  yValues=sort(unique(X[,yName])),
  factorNames=sort(names(X)[-grep(yName,names(X))]), 
  fancyFactorNames=factorNames, 
  fancyFactors,
  main=yName, 
  sub="", 
  xlab="", 
  ylab="",
  horizScale="percent",
  mai=c(.75,0,.5,0),
  overallLabel="Overall",
  barFill="lines", 
  barHeight=.25, 
  barWidth=3, 
  barCols
) 
{
  # Draws a horizontal "stacked" barplot of one variable categorized by various other factor variables
  # 
  # __Parameters__
  # X:                  the dataframe containing the data to be described
  # yName:              name of the column with the response variable
  # yValues:            specifies the order (left to right) of the values of y; must be a permutation of unique(X[,yName])
  # factorNames:        vector of strings containing names of columns in X containing factors (coerced if necessary)
  # fancyFactorNames:   printed names of the categories
  # fancyFactors:       printed names of the category values
  # main:               passed to plot
  # sub:                passed to plot
  # xlab:               passed to plot
  # ylab:               passed to plot
  # horizScale:         either of "percent" or "fraction"
  # mai:                space around box containing bars c(bottom, left, top, right) in inches
  # overallLabel:       Title for "Overall" (Wow, how descriptive!)
  # barFill:            one of c("lines", "color", "grey")
  # barHeight:          in inches
  # barWidth:           in inches
  # barCols:            vector of colors; if set, barFill is automatically set to "color"
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://sheer.ucdavis.edu/svn/software/public/descripBarplot/descripBarplot.R")
  
  # Error checking
  X <- as.data.frame(X)
  if(!identical(sort(yValues), sort(unique(X[,yName])))) stop("yValues is not a permutation of the values of y")
  if(missing(fancyFactors)) fancyFactors <- sapply(factorNames, function(factorName) levels(as.factor(X[,factorName])), simplify=F )
  if(length(factorNames) != length(fancyFactorNames)) stop("length(factorNames) != length(fancyFactorNames)")
  # Error checking needs more work
  
  nLevels <- sapply(factorNames, function(factorName) length(unique(X[,factorName])) )
  nBars <- sum(nLevels) + 1             # number of bars; values +1 for "Overall"
  nSlots <- nBars + length(factorNames) # add empty slots
  
  nValues <- length(yValues)            # number of unique values of response variable
  
  # plan filling for bars
  if(!missing(barCols)) barFill <- "color"  # specifying colors is sufficient to say colors are desired
  if(barFill=="lines") {
    barDensity <- 15
    barCols <- NULL
    barAngles <- 45 + 180/nValues*c(0:(nValues-1))
    barLwd <- .1
  } else if(barFill=="color") {
    barDensity <- NULL
    if(!missing(barCols)) {
      if(length(barCols) != nValues) {
        barCols <- rainbow(nValues)
        warning("Using rainbow for barCols")
      }
    } else {
      barCols <- rainbow(nValues)
    }
    barAngles <- 45
    barLwd <- 1
  } else {
    barDensity <- NULL
    barCols <- grey((1:nValues)/nValues)
    barAngles <- 45
    barLwd <- 1
  }
  
  # initialize right axis labels
  rightLabels <- rev(c(
    paste(overallLabel, " (", length(X[,yName]), ")", sep=""), 
    unlist(sapply(1:length(factorNames), function(fi) 
      c(fancyFactorNames[fi], paste("   ", fancyFactors[[fi]])) 
    ))
  ))
  # add n to label for each factor
  for(fi in 1:length(factorNames)) {
    for(fvali in 1:(nLevels[fi])) {
      thisSlot <- nSlots-fi-sum(nLevels[0:(fi-1)])-fvali
      thisValue <- levels(as.factor(X[,factorNames[fi]]))[fvali]
      rightLabels[thisSlot] <- paste(rightLabels[thisSlot], " (", length(X[X[,factorNames[fi]]==thisValue,yName]), ")", sep="")
    }
  }
  
  ###################
  ###  FUNCTIONS  ###
  ###################

  plotBar <- function(vertLocation, factorName, factorValue) {
    if(missing(factorName)) {
      ySub <- X[,yName]
    } else {
      ySub <- X[X[,factorName]==factorValue,yName]
    }
    nSub <- length(ySub)
    yCounts <- sapply(yValues, function(yValue) sum(ySub==yValue))
    for(i in 1:nValues) {
      if(yCounts[i] > 0) {
        if(i==1) xleft <- 0
        else xleft <- cumsum(yCounts)[i-1]/nSub
        xright <- cumsum(yCounts)[i]/nSub
        rect(xleft, vertLocation-.8, xright, vertLocation, col="white", lwd=barLwd)
        rect(xleft, vertLocation-.8, xright, vertLocation,
          col=barCols[i], density=barDensity, angle=barAngles[i], lwd=barLwd)
        rect((xright+xleft-strwidth(yCounts[i]))/2-.005, vertLocation-.7,
          (xright+xleft+strwidth(yCounts[i]))/2+.005, vertLocation-.1,
          col="white", lwd=.1)
        text((xright+xleft)/2, vertLocation-.35, yCounts[i], cex=2.8*barHeight)
      }
    }
    invisible(NULL)
  }
  
  ################
  ### PLOTTING ###
  ################
  par(mai=mai, 
    pin=c(barWidth, nSlots*barHeight), 
    omi=c(0,0,0,max(strwidth(rightLabels, units="inches")))
  )
  plot(c(0,1), c(0,nSlots), type='n', xaxt='n', yaxt='n', bty='n', mgp=c(2,1,0), main="", sub="", xlab="", ylab="")
  title(main=main, line=2)
  title(xlab=xlab, line=1.5)
  title(sub=sub, line=2.5)
  title(ylab=ylab, line=.5)
  
  # label x-axes
  abline(v=c(.25,.5,.75), lwd=2, col="grey")
  if("percent"==horizScale) {
    axis(1, at=seq(0, 1, length.out=5), c("0%", "25%", "50%", "75%", "100%"), cex.axis=.5, padj=-2)
    axis(3, at=seq(0, 1, length.out=5), c("0%", "25%", "50%", "75%", "100%"), cex.axis=.5, padj=1)
  } else {
    axis(1, cex.axis=.5, padj=-2)
    axis(3, cex.axis=.5, padj=1)
  }
  
  # plot "Overall"
  plotBar(nSlots)
  
  # plot each factor's bars
  for(fi in 1:length(factorNames)) {
    for(fvali in 1:(nLevels[fi])) {
      thisSlot <- nSlots-fi-sum(nLevels[0:(fi-1)])-fvali
      thisValue <- levels(as.factor(X[,factorNames[fi]]))[fvali]
      plotBar(thisSlot, factorNames[fi], thisValue)
    }
  }
  # plot bar labels
  axis(4, at=1:nSlots-.35, labels=rightLabels, tick=FALSE, mgp=c(3,0,0), las=1, cex.axis=3.2*barHeight)
  
  invisible(NULL)
}
