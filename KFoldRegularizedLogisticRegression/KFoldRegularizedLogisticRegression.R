#' Perform k-fold cross validation to choose the optimal regularization parameter
#' for regularized logistic regression
#'
#' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
#' Source: https://github.com/shaptonstahl/R
#'
#' Call with:
#'   source("http://www.haptonstahl.org/R/KFoldRegularizedLogisticRegression/KFoldRegularizedLogisticRegression.R")
#'
#'Functions provided:
#'  RegularizedLogisticRegression
#'  KFoldRegularizedLogisticRegression
#'  PlotRegularizationParameters
#'  
#' Example:
#'   X <- matrix(c(rexp(100), rnorm(100)), ncol=2)
#'   y <- as.vector(cbind(1, X) %*% c(-.3, .3, .2) + rnorm(100, sd=.2) > 0)
#'   res <- KFoldRegularizedLogisticRegression(X, y)
#'   PlotRegularizationParameters(res)

RegularizedLogisticRegression <- function(X, y, lambda) {
  X <- as.matrix(X)
  X <- cbind(1, X)
  m <- nrow(X)
  length.theta <- ncol(X)
  Cost <- function(theta) {
    - sum(y * log(plogis(X %*% theta)) + 
      (1-y) * log(1 - plogis(X %*% theta))) / m +
      sum(theta[2:length.theta] ^ 2) * lambda / (2 * m)
  }
  CostGradient <- function(theta) {
    as.vector(t(X) %*% (plogis(X %*% theta) - y) / m +
      c(0, rep(1, length.theta - 1)) * lambda * theta / m)
  }
  res <- tryCatch(optim(par=rnorm(length.theta), Cost, CostGradient, method="BFGS"),
                  error=function(e) {
                    warning('error in optimization')
                    return(list(par=NA))
                  })
  return(res$par)
}

KFoldRegularizedLogisticRegression <- function(X, y, folds=5, lambdas=3^c(-5:5)) {
  #' Given a dataset and the number of folds returns the error for each 
  #' value of lambda, the lambda that has the least error, and the value of theta
  #' corresponding to using the entire training set with the optima value of lambda 
  
  X <- as.matrix(X)
  
  Error <- function(Xcv, ycv, theta) {
    if(identical(NA, theta)) return(NA)
    Xcv <- cbind(1, Xcv)
    return(- sum(ycv * log(plogis(Xcv %*% theta)) + 
            (1-ycv) * log(1 - plogis(Xcv %*% theta))) / nrow(Xcv))
  }
  
  suppressWarnings( test.set.rows <- split(sample(1:nrow(X), nrow(X)), f=1:folds) )
  
  errors <- sapply(lambdas, function(lambda) {
    mean(sapply(1:folds, function(fold) {
      Xtest <- X[-test.set.rows[[fold]],]
      ytest <- y[-test.set.rows[[fold]]]
      Xcv <- X[test.set.rows[[fold]],]
      ycv <- y[test.set.rows[[fold]]]
      theta <- RegularizedLogisticRegression(Xtest, ytest, lambda)
      return(Error(Xcv, ycv, theta))
    }))
  })
  best.lambda <- lambdas[which.min(errors)]
  best.theta <- RegularizedLogisticRegression(X, y, best.lambda)
  if(any(is.na(errors))) warning("optimization error running some regressions")
  return(list(errors=errors,
              lambdas=lambdas,
              best.lambda=best.lambda,
              best.theta=best.theta))
}

PlotRegularizationParameters <- function(res) {
  plot(res$errors, type='l', axes=FALSE, ylab='errors', xlab='learning parameter', main='Learning Parameter Error Rates')
  axis(1, 1:length(res$lambdas), labels=round(res$lambdas, 3))
  axis(2)
  invisible(NULL)
}
