# srhSudoku
# R package to solve sudoku games
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Load using: source("http://sheer.ucdavis.edu/svn/software/public/srhSudoku/srhSudoku.R")

setClass(Class="sudoku", 
  representation=representation(
    type="character",
    solved="logical",
    nrow="numeric",
    ncol="numeric",
    symbols="character",
    start="numeric",
    current="numeric"),
  S3methods=FALSE)
