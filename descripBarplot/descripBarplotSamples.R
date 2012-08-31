# descripBarplot Samples

source("http://sheer.ucdavis.edu/svn/software/public/descripBarplot/descripBarplot.R")

# sample data
n <- 200
sampleData <- data.frame(
  response=sample(c(0:3), n, replace=T),
  f1=as.factor(sample(c("A", "B", "C", "D"), n, replace=T)),
  f2=as.factor(sample(c("E", "F"), n, replace=T))
)

descripBarplot(X=sampleData, yName="response")

descripBarplot(X=sampleData, yName="response", factorNames=c("f1", "f2"), 
  main="Test plot", sub="Sub text", xlab="X label",
  fancyFactorNames=c("Factor 1 thing", "Factor 2 thing"),
  fancyFactors=list(c("A thingy", "B whosits", "C-note", "D-fault"), c("eMail", "F-Troop")),
  barFill="lines"
)

descripBarplot(X=sampleData, yName="response", factorNames=c("f1", "f2"), 
  main="Test plot", sub="Sub text", xlab="X label",
  fancyFactorNames=c("Factor 1 thing", "Factor 2 thing"),
  fancyFactors=list(c("A thingy", "B whosits", "C-note", "D-fault"), c("eMail", "F-Troop")),
  barFill="color"
)

descripBarplot(sampleData, yName="response", factorNames=c("f1", "f2"), 
  main="Test plot", sub="Sub text", xlab="X label",
  fancyFactorNames=c("Factor 1 thing", "Factor 2 thing"),
  fancyFactors=list(c("A thingy", "B whosits", "C-note", "D-fault"), c("eMail", "F-Troop")),
  barFill="grey"
)
