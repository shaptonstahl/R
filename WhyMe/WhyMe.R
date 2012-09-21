# WhyMe
# 
# Given a subset of observations (perhaps nodes on a network) suggests which 
# variables (node attributes) best explain how "these" nodes are different
# from "those" nodes.

WhyMe <- function(X, selected) {
  # X = data.frame, so rows are observations and columns are variables
  # selected = either a numeric vector with some of the row numbers 
  #   from X, or a logical vector, length==nrow(X), where TRUE 
  #   indicates a row in X is selected.
  
  stopifnot(!missing(X),
            !missing(selected),
            is.numeric(selected) | (is.logical(selected) & nrow(X)==length(selected)),
            is.data.frame(X))
  
  if( is.logical(selected) ) selected <- which(selected)
  y <- rep(0, nrow(X))
  y[selected] <- 1
  
  res <- glm(y ~ X)
  
  
  
}



n <- 200
X <- data.frame(x1=sample(1:3, n, TRUE),
                x2=sample(c("a", "b", "c", "d"), n, TRUE),
                x3=rnorm(n),
                x4=rexp(n))
selected <- sample(0:1, n, TRUE)
