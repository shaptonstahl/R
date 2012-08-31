# BUGS functions - unit test

source("http://sheer.ucdavis.edu/svn/software/public/BugsFunctions/BugsFunctions.R")

# Munster: setwd("E:/svn/software/public/BugsFunctions")
# Day: setwd("C:/svn/software/public/BugsFunctions")
# Eris: setwd("C:/svn/software/public/BugsFunctions")

##################
###  as.index  ###
##################
t1 <- c("c", "b", "b", "a")
as.index(t1)

t2 <- as.factor(t1)
as.index(t2)

t3 <- t1[1:3]  # missing instances of level "a"
as.index(t3)

t4 <- c(5,4,3,1)
as.index(t4)

t5 <- t4
names(t5) <- c("this", "is", "an", "index")
as.index(t5)


####################
###  runjagsDIC  ###
####################
# Create fake data
n <- 100
x <- rnorm(n)
y <- x/3 + .2 + rnorm(n, sd=.5)
runjagsDIC.model <- "model{
  for (i in 1:n) { 
    y[i] ~ dnorm(mu[i], precision);
    mu[i] <- x[i] * slope + intercept;
  }
  slope ~ dnorm(0,.01);
  intercept ~ dnorm(0,.01);
  precision <- pow(conditional.sd, -2);
  conditional.sd ~ dexp(.01)  # mean of prior = 100
}"
n.chains <- 2
library(runjags)
runjagsDIC.data <- dump.format(list(n=n, y=y, x=x))
runjagsDIC.parameters <- c("slope", "intercept", "conditional.sd")

# Run with monitor.deviance=FALSE
runjagsDIC.fit.no.dev <- run.jags(model=runjagsDIC.model, data=runjagsDIC.data,
  monitor=runjagsDIC.parameters, n.chains=n.chains, thin=1, burnin=500, sample=5000,
  monitor.deviance=FALSE, check.conv=TRUE, plots=TRUE)

# Run with monitor.deviance=TRUE
runjagsDIC.fit.dev <- run.jags(model=runjagsDIC.model, data=runjagsDIC.data,
  monitor=runjagsDIC.parameters, n.chains=n.chains, thin=1, burnin=500, sample=5000,
  monitor.deviance=TRUE, check.conv=TRUE, plots=TRUE)

# Check all four
runjagsDIC(runjagsDIC.fit.no.dev)  # Should return deviance warning
runjagsDIC(runjagsDIC.fit.dev)  # Should return DIC

# Check with WinBUGS
library(R2WinBUGS)
##  Gamma  ##
wb.data <- list("y", "x", "n")
wb.inits <- function(){ list( 
  slope=rnorm(1), 
  intercept=rnorm(1),
  conditional.sd=rexp(1) ) }
cat(runjagsDIC.model, file="runjagsDICtest.bug")
wb.gamma.results.test <- bugs(data=wb.data, inits=wb.inits,
  parameters.to.save=runjagsDIC.parameters,
  model.file="runjagsDICtest.bug",
  n.iter=400, n.burnin=5, n.thin=1, n.chains=2,
  debug=TRUE, clearWD=TRUE)
wb.gamma.results <- bugs(data=wb.data, inits=wb.inits,
  parameters.to.save=runjagsDIC.parameters,
  model.file="runjagsDICtest.bug",
  n.iter=5500, n.burnin=500, n.thin=1, n.chains=2,
  clearWD=TRUE)
print(wb.gamma.results)
runjagsDIC(runjagsDIC.fit.dev)  # Should return DIC
# Are the two DICs within 1 of each other?
