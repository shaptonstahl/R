# srhSudoku
# R package to solve sudoku games
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/srhSudoku/srhSudoku.R")

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
