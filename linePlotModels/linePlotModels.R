# linePlotModels
# Load using: source("http://sheer.ucdavis.edu/svn/software/public/linePlotModels/linePlotModels.R")

source("http://sheer.ucdavis.edu/svn/software/public/RoundNicely/RoundNicely.R")

linePlotModels <- function(X,
  orderShown, fancyVarNames,
  main="", xlab="", ylab="", sub= "",
  #varHeight=is.list(X)*length(X)+!is.list(X)*ncol(X),  # I don't know why I put this in here...?
  scaleBarThickness=1, scaleBarSpace=1, scaleBarLength=1,
  useMean=F, colLines, pchModels, lwd=1, xlim,
  ...
)
{
  # Graphic analogue to a typical regression table
  # For each coefficient, plots a horizontal bar like:
  # 
  #   - - - ------------0----------- - - - - 
  #  min   1st       median       3rd     max
  #      quartile               quartile     
  # 
  # Bars are grouped together for different models so effects
  # can be easily compared across models. A dotted line is plotted
  # at zero to recognize easily significant difference from zero.
  # Mean and +/- 1 standard deviation can be graphed instead.
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://sheer.ucdavis.edu/svn/software/public/linePlotModels/linePlotModels.R")
  
  ##############
  ###  PREP  ###
  ##############
  
  # coerce it into being a list of models
  if( !is.list(X[[1]]) ) X <- list(m1=X)
  # generate list of variables
  rawVarNames <- unique(unlist(lapply(X, colnames)))

  nModels <- length(X)
  nVars <- length(rawVarNames)
  
  # settle the order to show the variables
  if(!missing(orderShown)) {
    if(length(orderShown) != nVars) 
      stop(paste("length(orderShown) (", length(orderShown), ") != number of variables (", nVars, ")\n", sep=""))
  } else {
    # show in order accumulated
    orderShown <- 1:nVars
  }
  varNames <- rawVarNames[orderShown]
  
  # figure out fancyVarNames
  if(!missing(fancyVarNames)) {
    if(length(fancyVarNames) != nVars) 
      stop(paste("length(fancyVarNames) (", length(fancyVarNames), ") != number of variables (", nVars, ")\n", sep=""))
  } else {
    # show raw labels
    fancyVarNames <- varNames
  }
  
  # figure out colLines (model colors; need one per model)
  if(!missing(colLines)) {
    if(length(colLines) != nModels)
      stop(paste("length(colLines) (", length(colLines), ") != number of models (", nModels, ")\n", sep=""))
  } else {
    colLines <- rep("black", nVars)
  }
  
  # figure out pchModels (plotting symbols; need one per model)
  if(!missing(pchModels)) {
    if(length(pchModels) != nModels)
      stop(paste("length(pchModels) (", length(pchModels), ") != number of models (", nModels, ")\n", sep=""))
  } else {
    pchModels <- rep(c(19, 21:25), length.out=nModels)
  }
  
  # figure out xlim
  if(missing(xlim)) xlim <- c(roundNicely(min(unlist(X)), down=T), roundNicely(max(unlist(X))))
  
  ###################
  ###  FUNCTIONS  ###
  ###################
  # define the display of one variable
  plotLine <- function(x, vertLocation, whichModel, nModels, useMean=F, colLine, pchModel, lwd) {
    if(useMean) breaks <- c(min(x), mean(x)-sd(x), mean(x), mean(x)+sd(x), max(x))
    else breaks <- quantile(x, c(0,.25,.5,.75,1))
    y <- vertLocation-(whichModel-.5)/(nModels+1)
    lines(x=breaks[c(1,5)], y=c(y,y), col=colLine, lwd=lwd, lty=3)
    lines(x=breaks[c(2,4)], y=c(y,y), col=colLine, lwd=lwd, lty=1)
    points(x=breaks[3], y=y, col=colLine, pch=pchModel, bg="white")
  }
  plotVariable <- function(varName, vertLocation, nModels, useMean=F, colLines, pchModels, lwd) {
    # dependencies: plotLine
    for(m in 1:nModels) {
      if(sum(names(X[[m]])==varName) > 0)       # test to see if this model has this variable
        plotLine(X[[m]][,varName], vertLocation, m, nModels, useMean, colLines[m], pchModels[m], lwd)
    }
  }
  
  ##################
  ###  PLOTTING  ###
  ##################
  
  # initialize plot
  barSpace <- .2 * scaleBarSpace            # default: 1/10 inch
  barThickness <- (.1 * nModels + barSpace) * scaleBarThickness  # default: 1/10 inch for each model and space between vars
  barLength <- 3 * scaleBarLength            # default: 3 inches long
  par(pin=c(barLength, nVars*barThickness), 
    omi=c(0,0,0,max(strwidth(fancyVarNames, units="inches")))
  )
  plot(
    x=xlim, 
    y=c(0,nVars), 
    type='n', 
    xaxt='n', 
    yaxt='n', 
    bty='n', 
    mgp=c(3,1,0),
    main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
  abline(v=0, lwd=2, lty=2)
  axis(1)
  axis(3)
  
  # plot variables
  for(v in 1:nVars) {
    plotVariable(varNames[v], nVars+1-v, nModels, useMean, colLines, pchModels, lwd)
  }
  
  # plot fancyVarNames
  axis(4, at=1:nVars-.35, labels=rev(fancyVarNames), tick=FALSE, mgp=c(3,0,0), las=1, ...)
  
  ################
  ###  RETURN  ###
  ################
  
  out <- list(
    rawVarNames=rawVarNames,
    orderShown=orderShown,
    varNames=varNames,
    fancyVarNames=fancyVarNames,
    colLines=colLines,
    pchModels=pchModels,
    lwd=lwd
  )
  invisible(out)
}
