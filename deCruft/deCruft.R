# deCruft
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# After installing and loading 'devtools', load using
# source_url("https://raw.github.com/shaptonstahl/R/master/deCruft/deCruft.R")

# The following sapply detaches all attached objects except for packages and defaults
result <- sapply( search()[-c( grep("Autoloads", search()), grep("GlobalEnv", search()), grep("package", search()) )],
  function(strIn) { detach( pos=grep(strIn, search())[1] ) })
rm(list=ls())  # remove all objects


non.default.loaded.namespaces <- rev(setdiff(loadedNamespaces(), c("base", "tools", options()$defaultPackages)))
# sapply(non.default.loaded.namespaces, unloadNamespace)  # removes environment and from search path
rm(list=ls(all.names=TRUE, envir=.GlobalEnv), envir=.GlobalEnv)  # clears *all* objects including visible and hidden environments
