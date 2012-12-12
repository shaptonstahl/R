#!/usr/bin/env Rscript

f <- file("stdin")
open(f)

#####  Begin mapper  #####

while( length(line.in <- readLines(con=f, n=1)) > 0 ) {
  if( nchar(line.in) == 0 ) next
  
  line.in <- gsub(" +", " ", line.in)
  line.in <- gsub("^ ", "", line.in)
  line.in <- gsub(" $", "", line.in)
  
  words <- unlist(strsplit(line.in, split=" "))
  
  for(word in words) cat(word, "1\n")
}

close(f)
