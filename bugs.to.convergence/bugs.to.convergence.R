# bugs.to.convergence
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://raw.github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/bugs.to.convergence/bugs.to.convergence.R")

# Function list:
# - bugs.to.convergence: 
# - merge.bugs: 

library(R2WinBUGS)
library(abind)

bugs.to.convergence <- function(max.rhat=1.2, step.iter=1000, max.iter=10000, n.keep=1000, 
  data, inits, parameters.to.save, model.file="model.bug",
  n.chains=3, n.burnin=floor(step.iter/2), n.thin=max(1, floor(n.chains * (step.iter - n.burnin) / n.sims)),
  n.sims = 1000, bin=(step.iter - n.burnin) / n.thin, debug=FALSE, DIC=TRUE, digits=5,
  bugs.directory="c:/Program Files/WinBUGS14/", program=c("WinBUGS", "OpenBUGS", "winbugs", "openbugs"),
  working.directory=NULL, clearWD=FALSE,
  useWINE=.Platform$OS.type != "windows", WINE=NULL, newWINE=TRUE, WINEPATH=NULL, bugs.seed=NULL,
  save.history=!summary.only)
{
  # Run 'bugs' until all reported rhats are below a certain threshold.
  
  # first run
  step.fit <- bugs(n.iter=step.iter, codaPkg=FALSE, summary.only=FALSE, 
    n.burnin=n.burnin, inits=inits, 
    data=data, parameters.to.save=parameters.to.save, model.file=model.file,
    n.chains=n.chains, n.thin=n.thin, n.sims=n.sims, bin=bin,
    debug=debug, DIC=DIC, digits=digits, bugs.directory=bugs.directory,
    program=program, working.directory=working.directory, clearWD=clearWD,
    useWINE=useWINE, WINE=WINE, newWINE=newWINE, WINEPATH=WINEPATH, 
    bugs.seed=bugs.seed, save.history=TRUE)
  compound.fit <- step.fit
  step.max.rhat <- max(monitor(step.fit$sims.array)[,"Rhat"])
  completed.iter <- step.iter
  
  while(completed.iter < max.iter && max.rhat < step.max.rhat) {
    cat(completed.iter, "steps complete.  For this step, max(Rhat) =", step.max.rhat, "\n")
    
    step.fit <- bugs(n.iter=step.iter, codaPkg=FALSE, summary.only=FALSE, 
      n.burnin=1, inits=step.fit$last.values, 
      data=data, parameters.to.save=parameters.to.save, model.file=model.file,
      n.chains=n.chains, n.thin=n.thin, n.sims=n.sims, bin=bin,
      debug=debug, DIC=DIC, digits=digits, bugs.directory=bugs.directory,
      program=program, working.directory=working.directory, clearWD=clearWD,
      useWINE=useWINE, WINE=WINE, newWINE=newWINE, WINEPATH=WINEPATH, 
      bugs.seed=bugs.seed, save.history=TRUE)
    completed.iter <- completed.iter + step.iter
    step.monitor <- monitor(step.fit$sims.array)
    step.max.rhat <- max(step.monitor[,"Rhat"])
    
    compound.fit <- merge.bugs(bugs1=compound.fit, bugs2=step.fit, n.keep=n.keep, monitor=step.monitor)
  }
  cat(completed.iter, "steps complete.  Final max(Rhat) =", step.max.rhat, "\n")
  
  return( list(
    params=list(
      max.rhat=max.rhat,
      step.iter=step.iter,
      max.iter=max.iter,
      n.keep=n.keep,
      completed.iter=completed.iter,
      converged=(step.max.rhat< max.rhat)),
    bugs=compound.fit) )
}
# schools.sim <- bugs.to.convergence(step.iter=75, max.iter=2000, data=data, inits=inits, parameters.to.save=parameters, model.file=model.file, clearWD=TRUE)
# cjr.fit <- bugs.to.convergence(step.iter=500, max.iter=5000, data=cjr.data, inits=cjr.inits, parameters.to.save=cjr.parameters, working.directory=work.dir, model.file="cjr.bug", n.chains=3, debug=F)

merge.bugs <- function(bugs1, bugs2, n.keep, monitor) {
  # Appends bugs2 to bugs1.
  out <- bugs1
  
  # n.chains is retained
  # n.burnin is retained
  # n.thin is retained
  
  # Other params are modified to make n.keep correct.
  if( missing(n.keep) ) {
    n.keep <- bugs1$n.keep
  } else {
    n.keep <- min(c(n.keep, bugs1$n.keep + bugs2$n.keep))
  }
  out$n.keep <- n.keep
  out$n.iter <- bugs1$n.burnin + n.keep
  out$n.sims <- out$n.chains * n.keep
  
  out$sims.array <- abind(bugs1$sims.array, bugs2$sims.array, along=1)
  if( nrow(out$sims.array) > n.keep ) out$sims.array <- out$sims.array[(nrow(out$sims.array)-n.keep+1):nrow(out$sims.array),,]
  
  # sims.list
  for(i in 1:length(bugs1$sims.list)) {
    if( is.null(dim(bugs1$sims.list)) ) {
      # contatenate scalars, such as deviance
      new.sims.list <- c(bugs1$sims.list$deviance, bugs2$sims.list$deviance)
      if( length(new.sims.list) > n.keep ) new.sims.list <- new.sims.list[(length(new.sims.list)-n.keep+1):length(new.sims.list)]
    } else {
      # concatenate matrices, arrays
      new.sims.list <- abind(bugs1$sims.list[[i]], bugs2$sims.list[[i]], along=1)
      attr(new.sims.list, "dimnames") <- NULL
      if( nrow(new.sims.list) > n.keep ) {
        if( length(dim(new.sims.list))==2 ) new.sims.list <- new.sims.list[(nrow(out$sims.array)-n.keep+1):nrow(out$sims.array),]
        if( length(dim(new.sims.list))==3 ) new.sims.list <- new.sims.list[(nrow(out$sims.array)-n.keep+1):nrow(out$sims.array),,]
      }
    }
    out$sims.list[[names(out$sims.list)[i]]] <- new.sims.list
  }
  # need to randomly permute the rows
  
  out$sims.matrix <- rbind(bugs1$sims.matrix, bugs2$sims.matrix)
  if( nrow(out$sims.matrix) > n.keep ) out$sims.matrix <- out$sims.matrix[(nrow(out$sims.matrix)-n.keep+1):nrow(out$sims.matrix),]
  # need to randomly permute the rows
  
  if( missing(monitor) ) out$summary <- monitor(out$sims.array)
  else out$summary <- monitor
  
  # mean
  for(i in 1:(length(out$sims.list)-1)) {
    if( is.null(dim(bugs2$sims.list[[i]])) ) {
      out$mean[[names(out$sims.list)[i]]] <- mean(bugs2$sims.list[[i]])
    } else {
      out$mean[[names(out$sims.list)[i]]] <- apply(bugs2$sims.list[[i]], c(2:length(dim(bugs2$sims.list[[i]]))), mean)
    }
  }
  out$mean$deviance <- bugs2$summary["deviance","mean"]
  
  # sd
  for(i in 1:(length(out$sims.list)-1)) {
    if( is.null(dim(bugs2$sims.list[[i]])) ) {
      out$sd[[names(out$sims.list)[i]]] <- sd(bugs2$sims.list[[i]])
    } else {
      out$sd[[names(out$sims.list)[i]]] <- apply(bugs2$sims.list[[i]], c(2:length(dim(bugs2$sims.list[[i]]))), sd)
    }
  }
  out$sd$deviance <- bugs2$summary["deviance","sd"]
  
  # median
  for(i in 1:(length(out$sims.list)-1)) {
    if( is.null(dim(bugs2$sims.list[[i]])) ) {
      out$median[[names(out$sims.list)[i]]] <- median(bugs2$sims.list[[i]])
    } else {
      out$median[[names(out$sims.list)[i]]] <- apply(bugs2$sims.list[[i]], c(2:length(dim(bugs2$sims.list[[i]]))), median)
    }
  }
  out$median$deviance <- bugs2$summary["deviance","50%"]
  
  # root.short is retained
  # long.short is retained
  # indexes.short is retained
  # dimension.short is retained
  
  out$last.values <- bugs2$last.values
  
  return(out)
}
# cjr.fit.double <- merge.bugs(cjr.fit, cjr.fit)
# schools.sim.1 <- bugs(n.iter=100, debug=FALSE, data=data, inits=inits, parameters.to.save=parameters, model.file=model.file, n.chains=3, bugs.directory="c:/Program Files/WinBUGS14/", working.directory=NULL, clearWD=TRUE)
# schools.sim.2 <- bugs(n.iter=100, debug=FALSE, data=data, inits=inits, parameters.to.save=parameters, model.file=model.file, n.chains=3, bugs.directory="c:/Program Files/WinBUGS14/", working.directory=NULL, clearWD=TRUE)
# schools.sim.100 <- merge.bugs(schools.sim.1, schools.sim.2) # keeps 100
# schools.sim.150 <- merge.bugs(schools.sim.1, schools.sim.2, n.keep=150)
# schools.sim.200 <- merge.bugs(schools.sim.1, schools.sim.2, n.keep=200)
