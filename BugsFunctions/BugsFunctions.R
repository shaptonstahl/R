# BUGS functions
# These should be added to diplot eventually.
# Load using:  source("http://sheer.ucdavis.edu/svn/software/public/BugsFunctions/BugsFunctions.R")

# as.index: 
# CheckForNonconvergence: 
# ExtractCodaDraws: 
# initOnlyRNGs: 
# MissingVarNames:
# runjagsDIC: 

require(runjags)
source("http://sheer.ucdavis.edu/svn/software/public/Prompts/Prompts.R")

as.index <- function(x, return.levels=FALSE) {
  # Given a vector, returns a vector of the same length containing consecutive 
  # integers in 1:length(unique(x)) This makes the result suitable for use as 
  # the indices (subscripts) of a vector.  This is useful in multilevel models
  # when normalizing the data.
  
  if( !(is.vector(x) | is.factor(x)) ) stop("as.index is intended for use on objects of type 'vector' or 'factor'.")
  
  x.names <- names(x)
  
  # Unfactor. I can't just use the levels of the factor because (I believe) a 
  # factor variable might not have any instances of one of the levels.
  if( is.factor(x) ) x <- levels(x)[as.numeric(x)]
  
  key <- sort(unique(x))
  out <- sapply(x, function(this.x) which(key==this.x))
  names(out) <- x.names
  
  if(return.levels) out <- list(index=out, levels=key)
  
  return(out)
}

#BayesianRSquared <- function(x) {
#  # Run a variety of diagnostics on a 'coda' object of class 'mcmc' or 
#  # 'mcmc.list'.
#  if(class(mcmc) != 'mcmc' & class(mcmc) != "mcmc.list") 
#    stop("CheckForNonconvergence requires a 'coda' object of class 'mcmc' or 'mcmc.list'")
#  ExtractCodaDraws
#}

CheckForNonconvergence <- function(mcmc, plots=FALSE, thumbs=FALSE) {
  # Run a variety of diagnostics on a 'coda' object of class 'mcmc' or 
  # 'mcmc.list'.
  require(coda)
  if(class(mcmc) != 'mcmc' & class(mcmc) != "mcmc.list") 
    stop("CheckForNonconvergence requires a 'coda' object of class 'mcmc' or 'mcmc.list'")
  
  prompt <- "Press 'Enter' to continue or 'ESC' to stop the tests.\n"
  
  if(thumbs) {
    # Just give a thumbs up or down on all tests given normal acceptance rates.
    Say("This will return TRUE if there is any sign that the chains have not converged. You want to see FALSE here.")
    
    cat("Conducting Raftery and Lewis diagnostic...")
    res.raftery <- raftery.diag(mcmc)
    fail.raftery <- sum(sapply(res.raftery, function(this.chain) this.chain$resmatrix[,4] > 5*thin(mcmc))) > 0
    if(fail.raftery) cat("FAILED.\n")
    else cat("passed.\n")
    
    cat("Conducting Gelman and Rubin diagnostic...")
    res.gelman <- gelman.diag(mcmc)
    fail.gelman <- sum(res.gelman$psrf[,1] > 1.05) > 0
    if(fail.gelman) cat("FAILED.\n")
    else cat("passed.\n")
    
    cat("Conducting Geweke diagnostic...")
    res.geweke <- geweke.diag(mcmc)
    fail.geweke <- sum(sapply(res.geweke, function(this.chain) abs(this.chain$z) > 2.5)) > 0
    if(fail.geweke) cat("FAILED.\n")
    else cat("passed.\n")
    
    cat("Conducting Heidelberger and Welch diagnostic...")
    res.heidel <- heidel.diag(mcmc)
    fail.heidel <- sum(sapply(res.heidel, function(this.chain) sapply(this.chain[,"htest"], function(res) !identical(res, 1)) )) > 0
    if(fail.heidel) cat("FAILED.\n")
    else cat("passed.\n")
    
    if(fail.raftery || fail.gelman || fail.geweke || fail.heidel) {
      return(TRUE)  # It is true there is evidence of NON-convergence
    } else {
      return(FALSE)  # It is false that there is evidence of non-convergence
    }
  } else {
    # Show the results of each test
  
  Say("== Trace & Density Plots ==  In the trace plot, look for 'fuzzy caterpillar'. In the density plot, the different chains should have similar plots.")
  plot(mcmc, ask=TRUE)
  PressEnterToContinue(prompt)
  
  Say("== Acceptance Rate ==  This will be 1 for Gibbs.  The optimal rate for Metropolis given a random walk proposal in common models is 0.234.  If this is too low, there will be high autocorrelation; if this is too high, the chain will stay hear the mode and estimates of variance will be low.  Either way, the chain is not sufficiently searching the paramter space.")
  cat("Acceptance rates:", 1-rejectionRate(mcmc), "\n")
  
  Say("== Effective sample size ==  Adjusting for autocorrelation, the sample sizes are:")
  print(effectiveSize(mcmc))
  
  Say("== Raftery and Lewis Test ==  If Dependence Factor (I) is above 5*thin, may need more thinning or better starting values.")
  print(raftery.diag(mcmc))
  
  Say("== Cumulative plot ==  Plot of the 0.025, 0.5, and 0.975 quantiles as a function of iterations")
  cumuplot(mcmc, ask=TRUE)
  PressEnterToContinue(prompt)
  
  Say("== Autocorrelation ==  The slower the dropoff, the more thinning you need.")
  print(autocorr(mcmc))
  autocorr.plot(mcmc)
  PressEnterToContinue(prompt)
  
  Say("== Gelman Test ==  The closer to 1, the better. ANY one of these above 1.1 suggests the WHOLE CHAIN has not converged.")
  print(gelman.diag(mcmc))
  gelman.plot(mcmc)
  PressEnterToContinue(prompt)
  
  Say("== Geweke test ==  These are z-scores; anything outside +/- 1.96 is suspect.  If only one in twenty are outside, that can be fine. For the plots, each x is Geweke z-score of a bin 1/20 of first half compared to last half.")
  print(geweke.diag(mcmc))
  geweke.plot(mcmc)
  PressEnterToContinue(prompt)
  
  Say("== Heidelberger and Welch Test ==  A test for stationarity of the chains.")
  print(heidel.diag(mcmc))
  
  Say("==== Tests Complete ====")
  
  invisible(NULL)
  }
}

ExtractCodaDraws <- function(x, vars=varnames(x)) {
  if( class(x) != "mcmc.list" ) stop("extractCodaDraws requires an object of class 'mcmc.list'")
  for(vn in vars) if( !(vn %in% varnames(x)) ) stop("Each varible selected must exist in the 'mcmc.list'")
  out <- sapply(vars, function(vn) unlist(lapply(x, function(tc) tc[,vn])))
  rownames(out) <- NULL
  return( as.data.frame(out) )
}

initOnlyRNGs <- function(n.chains) InitOnlyRNDGs(n.chains)

InitOnlyRNGs <- function(n.chains) {
  # Returns initialization for random number generators for n chains
  base.RNGs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", 
    "base::Super-Duper", "base::Mersenne-Twister")
  return( sapply(1:n.chains, function(i) {
    dump.format(list(
      .RNG.name=sample(base.RNGs, 1), 
      .RNG.seed=(i-1)*1000+sample(1:1000, 1)))
  }) )
}

MissingVarNames <- function(x, name) {
  # Looks for 'NA' values in x. Returns a character vector
  # listing the indices of x that are NA using the variable
  # 'name' provided.
  return( paste(name, "[", which(is.na(x)), "]", sep="") )
}

runjagsDIC <- function(x, verbose=TRUE) {
  # Take the output of JAGS from `runjags` and returns the Deviance Information
  # Criterion.  Lower DIC is better; a difference of less than 2 is not 
  # significant; more than 7 in very strong.
  
  if( 0 == sum("deviance" == names(x)) ) stop("You must run JAGS with 'monitor.deviance=TRUE' to calculate DIC.")
  
  this.deviance <- mean(unlist(lapply(x$mcmc, function(tc) tc[,"deviance"])))
  this.pd <- var(ExtractCodaDraws(x$mcmc)$deviance)/2
  if( verbose ) cat(
    "== DIC calculation ==\n",
    "  deviance =", this.deviance, " defined as -2*log(likelihood) \n",
    "        pD =", this.pd, " effective number of parameters, estimated as var(deviance)/2 \n",
    "       DIC =", this.deviance + this.pd, " defined as sum of the above \n",
    "\n")
  return( this.deviance + this.pd )
}
