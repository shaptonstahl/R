# Prompts and text output
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/Prompts/Prompts.R")

# Provides:
# - Announce: 
# - PressEnterToContinue: 
# - Say: 

Announce <- function(msg) {
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://www.haptonstahl.org/R/Prompts/Prompts.R")
  width <- getOption("width")
  
  cat(paste(rep("#", width-2), collapse=""))
  cat(strwrap(x=msg, prefix="\n###  "), "\n")
  cat(paste(rep("#", width-2), collapse=""), "\n")
  
  invisible(msg)
}

PressEnterToContinue <- function(prompt="Press 'Enter' to continue...") {
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://www.haptonstahl.org/R/Prompts/Prompts.R")
  invisible(readline(prompt))
}

Say <- function(msg) {
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://www.haptonstahl.org/R/Prompts/Prompts.R")
  invisible(cat(strwrap(msg), sep="\n"))
}
