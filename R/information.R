
#' Get Names of Iteration Blocks
#'
#' @param sourceCode character vector with the templated strings
#'
#' @return character vector with the names of the iteration blocks
#' @details Iteration blocks have the following form: \cr\cr
#' \code{//R# <symbol name>[[start]]} \cr
#' \code{some text with [[R#<template variable>]]} \cr
#' \code{//R# <symbol name>[[end]]} \cr\cr
#' The name of the iteration block is given by \code{<symbol name>}.
#' Blocks may contain template variables of the form [[R#<template variable>]] which will be replaced during template transformation.
#' Distinct iteration blocks can share the same name.
#'
#' @export
#' @examples
#' sourceCode <- c(
#' "//R# name1[[start]]","double [[R#fun1Name]](double x);","//R# name1[[end]]",
#' "//R# name2[[start]]","double [[R#fun2Name]](double y);","//R# name2[[end]]")
#'
#' getIterSymbols(sourceCode)
#'
getIterSymbols <- function(sourceCode) {

  regEx <- "//R# *([^[]+)\\[\\[start\\]\\].*"
  matchRes <- regexec(regEx,sourceCode)
  matchStrings <- regmatches(sourceCode,matchRes)
  foundPos <- which(sapply(matchStrings,length)>0)
  unique(sapply(matchStrings[foundPos],function(x) x[2]))
}

#' Get Template Variables
#'
#' Get the template variables occuring in iteration blocks with a specific name.
#'
#' @param iterSymbol name of the iteration block
#' @param sourceCode character vector with templates
#'
#' @return character vector with the names of the template variables
#' @export
#'
#' @examples
#' sourceCode <- c(
#' "//R# name1[[start]]","double [[R#fun1Name]](double x);","//R# name1[[end]]",
#' "//R# name2[[start]]","double [[R#fun2Name]](double [[R#argName]]);","//R# name2[[end]]")
#'
#' getVarSymbols("name1",sourceCode)
#' getVarSymbols("name2",sourceCode)
#'
getVarSymbols <- function(iterSymbol, sourceCode) {

  chunkPos <- getIterChunkPos(iterSymbol,sourceCode)
  codeChunk <- mapply(function(x,y) sourceCode[(x+1):(y-1)], chunkPos$start, chunkPos$end)
  regEx <- "\\[\\[R#(.*?)\\]\\]"
  matchRes <- gregexpr(regEx,codeChunk)
  matchRes <- regmatches(codeChunk,matchRes)
  unique(sub(regEx,"\\1",unlist(matchRes)))
}
