#!/usr/bin/env Rscript

f <- file("stdin")
open(f)

# Read a single line
line.in <- readLines(con=f, n=1)
pars.in <- as.numeric(unlist(strsplit(line.in, split="\t")))
key <- current.key <- pars.in[1]
value <- pars.in[2]

#####  Begin reducer  #####
while( length(line.in <- readLines(con=f, n=1)) > 0 ) {
  if( nchar(line.in) == 0 ) next
  pars.in <- as.numeric(unlist(strsplit(line.in, split="\t")))
  key <- pars.in[1]
  if( identical(key, current.key) ) {
    cat(current.key, "\t", value, "\n")
    value <- 0
    current.key <- key
  }
  value <- value + pars.in[2]
}
cat(key, "\t", value, "\n")

close(f)
