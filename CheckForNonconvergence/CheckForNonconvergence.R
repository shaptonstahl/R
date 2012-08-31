# Check for non-convergence
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Load using: source("http://sheer.ucdavis.edu/svn/software/public/CheckForNonconvergence/CheckForNonconvergence.R")

source("http://sheer.ucdavis.edu/svn/software/public/Prompts/Prompts.R")

CheckForNonconvergence <- function(mcmc) {
  # Run a variety of diagnostics on a 'coda' object of class 'mcmc' or 
  # 'mcmc.list'.
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://sheer.ucdavis.edu/svn/software/public/CheckForNonconvergence/CheckForNonconvergence.R")
  if(class(mcmc) != 'mcmc' & class(mcmc) != "mcmc.list") 
    stop("CheckForNonconvergence requires a 'coda' object of class 'mcmc' or 'mcmc.list'")
  
  prompt <- "Press 'Enter' to continue or 'ESC' to stop the tests.\n"
  
  Say("== Trace & Density Plots ==  In the trace plot, look for 'fuzzy caterpillar'. In the density plot, the different chains should have similar plots.")
  plot(mcmc, ask=TRUE)
  PressEnterToContinue(prompt)
  
  Say("== Acceptance Rate ==  This will be 1 for Gibbs.  If this is too low, there will be high autocorrelation; if this is too high, the chain will stay hear the mode and estimates of variance will be low.  Either way, the chain is not sufficiently searching the paramter space.")
  cat("Acceptance rate:", 1-rejectionRate(mcmc), "\n")
  PressEnterToContinue(prompt)
  
  Say("== Effective sample size ==  Adjusting for autocorrelation, the sample sizes are:")
  print(effectiveSize(mcmc))
  PressEnterToContinue(prompt)
  
  Say("== Raftery Diagnostic ==  Do we have enough draws?")
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
  
  Say("== Heidel Test ==  A test for stationarity of the chains.")
  print(heidel.diag(mcmc))
  
  Say("==== Tests Complete ====")

  invisible(NULL)
}
