# deCruft
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Load using:  source("http://sheer.ucdavis.edu/svn/software/public/deCruft/deCruft.R")

# The following sapply detaches all attached objects except for packages and defaults
result <- sapply( search()[-c( grep("Autoloads", search()), grep("GlobalEnv", search()), grep("package", search()) )],
  function(strIn) { detach( pos=grep(strIn, search())[1] ) })
rm(list=ls())  # remove all objects
