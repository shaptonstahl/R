# colorSequence: a function for generating a palette ranging between two colors
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Load using: source("http://sheer.ucdavis.edu/svn/software/public/colorSequence/colorSequence.R")

colorSequence <- function(col1, col2, length.out=2) {
  # Generate a palette ranging between two colors
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://sheer.ucdavis.edu/svn/software/public/colorSequence/colorSequence.R")
  rgb1 <- col2rgb(col1)
  rgb2 <- col2rgb(col2)
  out <- matrix(c(
    seq(from=rgb1[1,1], to=rgb2[1,1], length.out=length.out),
    seq(from=rgb1[2,1], to=rgb2[2,1], length.out=length.out),
    seq(from=rgb1[3,1], to=rgb2[3,1], length.out=length.out)), ncol=3) / 255
  names(out) <- c("red", "green", "blue")
  return( rgb(out) )
}

# See ?colorRamp for a fancy built-in that does more, and the RColorBrewer package.
