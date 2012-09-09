# unfactorColumns
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://raw.github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/unfactorColumns/unfactorColumns.R")

unfactorColumns <- function(X) {
  # Convert columns of a data.frame from factors to a vector that is not a factor.
  # If appropriate, the column is converted to numeric.
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://sheer.ucdavis.edu/svn/software/public/unfactorColumns/unfactorColumns.R")

  if( !is.data.frame(X) ) stop("unfactorColumns requires a data.frame for input")
  
  for(ic in 1:ncol(X)) {
    if(is.factor(X[,names(X)[ic]])) {
      X[,names(X)[ic]] <- levels(X[,names(X)[ic]])[as.numeric(X[,names(X)[ic]])]
      old.warn <- getOption("warn")
      options(warn = -1)
      if( 0==sum(is.na(as.numeric(X[,names(X)[ic]]))) ) X[,names(X)[ic]] <- as.numeric(X[,names(X)[ic]])
      options(warn = old.warn)
    }
  }
  return(X)
}

# Stuff to keep handy

# eval(substitute(Dem$thing, list(thing=this.column)))

# as.numeric(levels(f)[as.numeric(f)])

# sapply(names(Dem), function(this.col.name) eval(substitute(Dem$thing, list(thing=this.col.name))))
