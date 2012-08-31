# Bayesian Information Criterion for various R classes
# Load using: source("http://sheer.ucdavis.edu/svn/software/public/BayesianInformationCriterion/BayesianInformationCriterion.R")

AkaikeInformationCriterion <- function(obj) {
  if( "zeroinfl" %in% class(obj)) {
    return( 2 * attr(logLik(obj), "df") - 2 * as.vector(logLik(obj)) )
  } else if( "negbin" == class(obj)[1] ) {  
    return( 2 * attr(logLik(obj), "df") - 2 * as.vector(logLik(obj)) )
  } else if( "glm" == class(obj)[1] ) {  
    return( 2 * attr(logLik(obj), "df") - 2 * as.vector(logLik(obj)) )
  } else {
    cat("Unable to calculate Akaike Information Criterion for this object.\n")
    return(NULL)
  }
}

BayesianInformationCriterion <- function(obj) {
  if( "zeroinfl" %in% class(obj)) {
    return( -2 * as.vector(logLik(obj)) - obj$df.residual * log(obj$n) )
  } else if( "negbin" == class(obj)[1] ) {  
    return( -2 * as.vector(logLik(obj)) - obj$df.residual * log(length(obj$y)) )
  } else if( "glm" == class(obj)[1] ) {  
    return( as.vector(logLik(obj)) - obj$df.residual * log(length(obj$y)) )
  } else {
    cat("Unable to calculate Bayesian Information Criterion for this object.\n")
    return(NULL)
  }
}
