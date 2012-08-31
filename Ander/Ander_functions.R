# Ander: functions for use in studying rollcalls
# Load using: source("http://sheer.ucdavis.edu/svn/software/public/Prompts/Ander_functions.R")

###  Provides  ###
# - basis: 
# - FilterRollcallVotes: 
# - FilterOrdVotes: 
# - is.successful.OC.run: 
# - MakeUnique:
# - ParsePERF21: 
# - ParsePERF25: 
# - PresentCommonSpacePegs:

###  USES  ###
source("http://sheer.ucdavis.edu/svn/software/public/Prompts/Prompts.R")
library(mgcv)  # provides uniquecombs

###  FUNCTIONS  ###
basis <- function(d, len) {
  out <- rep(0, len)
  out[d] <- 1
  return(out)
}

DefaultIdentification <- function(rc, d) {
  # Given legislator names and the number of dimensions returns a list
  # suitable for use with 'ideal' and 'constrain.legis' to identify an IRT
  # model of 'd' dimensions.
  
  # By 'suitable' I mean that selected legislators have dissimilar voting 
  # records. This is NO GUARANTEE that the dimensions line up in a way with
  # a known substantive interpretation.  However, the identification strategy
  # used with 'ideal' should mean that you will get the same measures of fit
  # regardless of which legislators are selected, as long as they don't have 
  # identical voting records.  This prevents that problem.
  
  # It should be well suited for estimating dimensionality.
  
  # Choose d+1 legislators with distinct voting records
  if( is.na(rc$codes$missing) ) {
    votes.without.missing <- rc$votes
    votes.without.missing[is.na(votes.without.missing)] <- "N"
  } else {
    votes.without.missing <- apply(rc$votes, c(1,2), function(this.vote) {
      if(identical(this.vote, rc$codes$missing)) return("N")
      else return(as.character(this.vote))
    })
  }
  records <- apply(votes.without.missing, 1, function(this.row) paste(this.row,
    sep="", collapse=""))
  distinct.records <- unique(records)
  # remove records with missing votes
  distinct.records <- distinct.records[-grep("N", distinct.records)]
  if( length(distinct.records) < d + 1 ) 
    stop("'DefaultIdentification' requires at least ", d + 1, 
      " distinct voting records.")
  # sort by frequency of first observed value
  qty.of.first.value <- sapply(distinct.records, function(this.record) {
    indices <- gregexpr(substr(distinct.records[1], 1,1), this.record)[[1]]
    if(indices[1] == -1) return(0)
    else return(sum(length(indices)))
  })
  distinct.records <- distinct.records[sort(qty.of.first.value, 
    index.return=TRUE, decreasing=TRUE)$ix]
  selected.distinct.records <- distinct.records[round(seq(from=1, 
    to=length(distinct.records), length.out=d+1))]
  selected.legis.rows <- sapply(selected.distinct.records, function(this.record) {
    as.numeric(which(records==this.record)[1])
  })
  
  # Assign ideal points to these d+1 legislators
  out <- sapply(d:1, function(i) basis(i, d), simplify=FALSE)
  out <- c(list(-basis(1,d)), out)
  names(out) <- rownames(rc$votes)[selected.legis.rows]
  return(out)
}

FilterRollcallVotes <- function(object, keep.votes, drop.votes=NULL) {
  # Given a 'rollcall' object (package: pscl) either keep or drop some votes.
  if( class(object) != "rollcall" ) stop("'object' must be of class 'rollcall (pscl)'")
  if( missing(keep.votes) ) {
    # drop votes
    if( !is.null(drop.votes) ) {
      keep.votes <- -1 * drop.votes
    } else {
      # do nothing
      keep.votes <- T
    }
  }
  object$votes <- as.matrix(object$votes[,keep.votes])
  if( !is.null(object$vote.data) ) object$vote.data <- object$vote.data[keep.votes,]
  object$m <- ncol(object$votes)
  return(object)
}

FilterOrdVotes <- function(ord, keep.votes) {
  # Given an ord file read using readLines, keep the votes specified by keep.votes
  votes <- sapply(1:length(ORD), function(l) 
    paste(sapply(keep.votes, function(v) substr(ORD[l], v+36, v+36)), sep="", 
      collapse=""))
  return( paste(substr(ord, 1, 36), votes , sep="") )
}

is.successful.OC.run <- function(filename) {
  # returns true if OC PERF21 exists and indicates a successful run
  if(!file.exists(filename)) {
    return(F)
  } else {
    filePERF21 <- file(filename, open="rt")
    PERF21 <- readLines(filePERF21)
    close(filePERF21)

    # test for failed OC run
    if( 0==length(grep("ELAPSED TIME OF JOB", PERF21)) ) {
      # Didn't find (grep) "ELAPSED TIME OF JOB" which indicates a failed OC run.
      # Enter row of missing values and let the researcher know with a warning.
      return(F)
    } else {
      # successful run (at least not obviously failed)
      return(T)
    }
  }
}

MakeUnique <- function(x) {
  # Given a character vector appends " (1)", " (2)", etc., to repeated values
  # to coerce the vector into having unique values.
  
  # Replace NA values
  number.na <- sum(is.na(x))
  if(number.na > 0) x[is.na(x)] <- paste("NA (", 1:number.na, ")", sep="")
  
  # replace other repeated values
  for(this.value in unique(x[duplicated(x)])) {
    number.this.value <- sum(x == this.value)
    x[x == this.value] <- paste(this.value, " (", 1:number.this.value, ")", sep="")
  }
  return(x)
}

ParsePERF21 <- function(filename, n.dim=5) {
  # Given the name of a file (readable with 'file' and 'readLines')
  # returns a list with elements parsed from an OC run
  # 
  # ex: parsed21 <- parsePERF21("R:/work/OC-5d-H84-clausen1-perf21.dat")
  # parsed21$APRE

  # test for failed OC run
  if(!successfulOCrun(filename)) {
    out <- NULL
  } else {
    # successful run (at least not obviously failed)
    out <- list()
    
    filePERF21 <- file(filename, open="rt")
    PERF21 <- readLines(filePERF21)
    close(filePERF21)
    
    # get APRE from PERF21
    if( 1 == n.dim ) {
      # APRE for 1 dimension files:
      #  a. grep lines where first 3 characters are spaces or numeric
      #  b. Take the max to get last such line.
      #  c. Grab columns 44-52 and force to be numeric.
      line.in.PERF21 <- max(grep("^ {0,2}[0-9]", PERF21, perl=T))
    } else {
      # APRE for 2- or more dimension files:
      #  a. grep lines with "MACHINE PREC" and take first line
      #  b. Grab columns 55-61 and force to be numeric.
      line.in.PERF21 <- min(grep("MACHINE PREC", PERF21, perl=T))
      
      out$mean.max.dist.to.polytope.boundary <- as.numeric(substr(PERF21[line.in.PERF21 - 1],21,26))
      out$sd.max.dist.to.polytope.boundary <- as.numeric(substr(PERF21[line.in.PERF21 - 1],30,35))
    }
    out$n.class.errors <- as.numeric(substr(PERF21[line.in.PERF21],19,26))
    out$n.choices <- as.numeric(substr(PERF21[line.in.PERF21],27,34))
    out$error.prop <- as.numeric(substr(PERF21[line.in.PERF21],37,43))
    out$classCorrect <- as.numeric(substr(PERF21[line.in.PERF21],46,52))
    out$APRE <- as.numeric(substr(PERF21[line.in.PERF21],55,61))  
  }
  return(out)
}

ParsePERF25 <- function(filename, n.dim=5) {
  # Given the name of a file (readable with 'file' and 'readLines') 
  # returns a data frame with the legislators and their ideal points
  
  ###  Right now only supports 5 dimensions  ###
  
  filePERF25 <- file(filename, open="rt")
  PERF25 <- readLines(filePERF25)
  close(filePERF25)
    
  # get lines with legislators
  legislatorLines <- grep("[a-z]", PERF25)[-1]

  out <- data.frame(
    congress      = as.numeric(substr(PERF25[legislatorLines], 8,    9)),
    id            = as.numeric(substr(PERF25[legislatorLines], 10,  14)),
    state.code    = as.numeric(substr(PERF25[legislatorLines], 15,  16)),
    district      = as.numeric(substr(PERF25[legislatorLines], 17,  18)),
    state.name    =            substr(PERF25[legislatorLines], 19,  25),
    party.code    = as.numeric(substr(PERF25[legislatorLines], 27,  29)),
    name          =            substr(PERF25[legislatorLines], 32,  44),
    n.errors      = as.numeric(substr(PERF25[legislatorLines], 45,  47)),
    n.choices     = as.numeric(substr(PERF25[legislatorLines], 49,  52)),
    correct       = as.numeric(substr(PERF25[legislatorLines], 56,  60)),
    polytope.size = as.numeric(substr(PERF25[legislatorLines], 64,  68)),
    c1            = as.numeric(substr(PERF25[legislatorLines], 71,  76)),
    c2            = as.numeric(substr(PERF25[legislatorLines], 79,  84)),
    c3            = as.numeric(substr(PERF25[legislatorLines], 87,  92)),
    c4            = as.numeric(substr(PERF25[legislatorLines], 95,  100)),
    c5            = as.numeric(substr(PERF25[legislatorLines], 103, 108))
  )
  
  return(out)
}

PresentCommonSpacePegs <- function(congress, chamber=c("H", "S"), KH, votes, 
  vote.view.root.url="ftp://voteview.ucsd.edu:21000/", return.icpsr=FALSE) 
{
  # Given a congress, chamber, and either a filtered KH object or a list of 
  # rollcall numbers returns the present left, up, right, and 
  # down legislators from 2-d common space scores.  By default returns row
  # number in considered KH object; if return.icpsr == TRUE, returns ICPSR
  # ids.
  library(Hmisc)      # enable remote reading of Stata files
  if(missing(KH)) {
    # get KH and filter using votes
    KH <- readKH(paste(vote.view.root.url, "dtaord/", 
      ifelse(chamber=="H", "hou", "sen"), ifelse(congress<10,"0",""), congress, 
      "kh.ord", sep=""))
    KH <- filter.rollcall.votes(KH, keep.votes=votes)
  }
  # Get Voteview common space 2-d OC scores
  ocCommon <- stata.get(paste(vote.view.root.url, "ocCommon/", chamber, "joint40_1_109.DTA", sep=""))
  if(chamber=="H") { 
    ocCommon <- ocCommon[ocCommon$congress==congress,]
  } else { 
    ocCommon <- ocCommon[ocCommon$Congress==congress,]
  }
  # Identify members who voted at least once in the given subset of votes
  icpsr.members.present <- KH$legis.data$icpsrLegis[apply(KH$votes, 1, 
    function(this.member.votes) return( sum(this.member.votes != 9) > 0 ))]
  # Filter common space scores to present members
  ocCommon <- ocCommon[ocCommon$idno %in% icpsr.members.present,]
  # Identify present pegs
  out <- c(ocCommon$idno[which.min(ocCommon$oc1)],   # left
           ocCommon$idno[which.max(ocCommon$oc1)],   # right
           ocCommon$idno[which.max(ocCommon$oc2)],   # up
           ocCommon$idno[which.min(ocCommon$oc2)] )  # down
  names(out) <- c("left", "right", "up", "down")
  if(!return.icpsr) out <- sapply(out, function(id) which(KH$legis.data$icpsrLegis==id))
  return(out)
}
# data(s109); PresentCommonSpacePegs(109, "S", s109, vote.view.root.url="http://sheer.ucdavis.edu/projects/voteview/")

saveOCcard <- function(
  work.folder=getwd(),
  ord.name="this.ord",
  card.comment=paste("Non-parametric unfolding in", n.dim, "dimensions"),
  n.dim=5,
  n.votes,
  legisLeft=1,
  legisUp=2,
  min.votes=1,
  cardname="perfstrt.dat",
  line.prefix.length=36) 
{
  # Given folder and parameters, save the control card for an external OC run
  fileDAT <- file(paste(work.folder, "/", cardname, sep=""), open="wt")
  writeLines(c(ord.name,
    card.comment,
    paste("    ", format(n.dim, justify="right", width=1), " ", format(n.votes, justify="right", width=4), "   20   36  ", 
      format(legisLeft, justify="right", width=3), "  ", format(legisUp, justify="right", width=3), "  ", format(min.votes, justify="right", width=3),
      " 0.005", sep=""),
    paste("(", line.prefix.length, "A1,3900I1)", sep=""),
    paste("(I5,1X,", line.prefix.length, "A1,2I5,50F8.3)", sep="")), fileDAT)
  close(fileDAT)
  invisible(T)
}
