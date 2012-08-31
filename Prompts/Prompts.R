# Prompts and text output
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Load using: source("http://sheer.ucdavis.edu/svn/software/public/Prompts/Prompts.R")

# Provides:
# - Announce: 
# - PressEnterToContinue: 
# - Say: 

Announce <- function(msg) {
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://sheer.ucdavis.edu/svn/software/public/Prompts/Prompts.R")
  width <- getOption("width")
  
  cat(paste(rep("#", width-2), collapse=""))
  cat(strwrap(x=msg, prefix="\n###  "), "\n")
  cat(paste(rep("#", width-2), collapse=""), "\n")
  
  invisible(msg)
}

PressEnterToContinue <- function(prompt="Press 'Enter' to continue...") {
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://sheer.ucdavis.edu/svn/software/public/Prompts/Prompts.R")
  invisible(readline(prompt))
}

Say <- function(msg) {
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://sheer.ucdavis.edu/svn/software/public/Prompts/Prompts.R")
  invisible(cat(strwrap(msg), sep="\n"))
}
