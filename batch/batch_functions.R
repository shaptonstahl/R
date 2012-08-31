# batch_functions.R
# by Stephen R. Haptonstahl [srh@ucdavis.edu]

# Load using:  source("http://sheer.ucdavis.edu/svn/software/public/batch/batch_functions.R")

###  Function List  ###
# GenerateWorkunitControl: 
# MinionGetWorkunit: Work with PHP and MySQL to select a unit of work
# MinionNotifyFailure: 
# MinionUpload: 
# SourceCharacter: Given a character variable, source the contents

source("http://sheer.ucdavis.edu/svn/software/public/postObjects/postObjects.R")
source("http://sheer.ucdavis.edu/svn/software/public/Prompts/Prompts.R")
source("http://sheer.ucdavis.edu/svn/software/public/usePackage/usePackage.R")    # like 'library' except that it first installs the package if necessary
if( !UsePackage("RCurl") ) stop("Minion requires the RCurl package")

GenerateWorkunitControl <- function(params) {
  # Generates a data.frame with all combinations of parameters so that 
  # the first parameter changes slowest, the last one changes fastest.
  out <- rev(expand.grid(rev(params)))
  names(out) <- names(params)
  return(out)
}

MinionGetWorkunit <- function(get.from.url, pw="", client="") {
  # Work with PHP and MySQL to select a unit of work
  if( missing(get.from.url) ) stop("Must specify URL")
  return( postForm(uri=get.from.url, pw=pw, client=client) )
}

MinionNotifyFailure <- function(url.save.results.to, pw, 
  minion.workunit, client="")
{
  # 
  for(i.try in 1:10) {
    post.response <- postForm(uri=url.save.results.to, 
      client=client,
      minionworkunit=minion.workunit,
      pw=pw,
      status="failure"
    )
    if( "success" == post.response ) {
      post.successful <- TRUE
      break
    }
  }
  return(post.successful)
}

MinionUpload <- function(url.save.results.to, pw, minion.workunit, 
  minion.output=NULL, minion.output.file.name=NULL, client="")
{
  post.successful <- FALSE
  if( is.null(minion.output) ) {
    for(i.try in 1:10) {
      post.response <- postForm(uri=url.save.results.to, 
        client=client,
        status="success",
        minionworkunit=as.character(minion.workunit),
        pw=pw)
      if( "success" == post.response ) {
        post.successful <- TRUE
        break
      }
    }
  } else {
    temp.file.name <- tempfile()
    save(minion.output, file=temp.file.name)
    for(i.try in 1:10) {
      post.response <- postForm(uri=url.save.results.to, 
        uploadedfile=fileUpload(filename=temp.file.name), 
        saveto=minion.output.file.name, 
        client=client,
        status="success",
        minionworkunit=as.character(minion.workunit),
        pw=pw)
      if( "success" == post.response ) {
        post.successful <- TRUE
        break
      }
    }
    file.remove(temp.file.name)
  }
  return(post.successful)
}

SourceCharacter <- function(x) {
  # Given a character variable, source the contents
  tmp <- tempfile()
  writeLines(x, tmp)
  source(tmp, local=FALSE)
  unlink(tmp)
  invisible(x)
}
