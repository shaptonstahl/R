# linePlotModels samples

source("http://sheer.ucdavis.edu/svn/software/public/linePlotModels/linePlotModels.R")

n <- 200
model1 <- data.frame(
  b0=rnorm(n), 
  b1=rnorm(n)*2-5, 
  b2=rexp(n)+3
)
model2 <- data.frame(
  b0=rnorm(n)+4, 
  b1=rnorm(n)*1.5-4, 
  b3=rnorm(n)+2
)
model3 <- data.frame(
  b0=rnorm(n)+3, 
  b1=rnorm(n)*1.5-4, 
  b2=rexp(n)+3,
  b3=rnorm(n)+2
)

models <- list(model1, model2, model3)


linePlotModels(models)

linePlotModels(models, 
  fancyVarNames=c("intercept", "first slope", "second slope", "third slope"),
  main="Posterior Distributions of Model Coefficients",
  xlab="x-label")

linePlotModels(models, 
  fancyVarNames=c("intercept", "first slope", "second slope", "third slope"),
  main="Posterior Distributions of Model Coefficients",
  xlab="x-label",
  colLines=rainbow(3)
)
