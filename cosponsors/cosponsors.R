# Establish cosponsorship
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/cosponsors/cosponsors.R")

source("http://www.haptonstahl.org/R/guid/guid.R")
source("http://www.haptonstahl.org/R/insertColumn/insertColumn.R")


##   Generate fake data  ##
n.legislators <- 100
n.proposals <- 300
mean.sponsorships <- 10
sponsorships <- data.frame(legislator.id=numeric(0), proposal.id=numeric(0))

for(i in 1:n.legislators) {
  n.sponsorships <- rpois(1, lambda=mean.sponsorships)
  if(n.sponsorships > 0) sponsorships <- rbind(sponsorships, data.frame(legislator.id=i, proposal.id=sample(1:n.proposals, n.sponsorships, replace=FALSE)))
}


cosponsors <- function(x, legis.ids=sort(unique(x$legislator.id))) {
  # Establ;ish cosponsorship
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://sheer.ucdavis.edu/svn/software/public/cosponsors/cosponsors.R")
  proposal.ids <- sort(unique(x$proposal.id))
  legis.ids <- sort(unique(legis.ids))
  n.legislators <- length(legis.ids)
  out <- matrix(NA, nrow=n.legislators, ncol=n.legislators)
  for(i in 1:n.legislators) {
    for(j in i:n.legislators) {
      proposals.cosponsored <- sapply(proposal.ids, function(k) {
        return( sum(x$legislator.id==i & x$proposal.id==k) * sum(x$legislator.id==j & x$proposal.id==k) > 0 )
      })
      out[i,j] <- sum(proposals.cosponsored)
      if(j > i) out[j,i] <- out[i,j]
    }
  }
  rownames(out) <- colnames(out) <- legis.ids
  return(out)
}

## Test  ##
cospons <- cosponsors(sponsorships)
cospons
