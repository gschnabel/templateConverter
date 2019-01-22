getIterChunkPos <- function(iterSymbol,sourceCode,showDep=TRUE) {

  regExStart <- paste0("^ *//R# *",iterSymbol,"\\[\\[start\\]\\].*$")
  regExEnd <- paste0("^ *//R# *",iterSymbol,"\\[\\[end\\]\\].*")
  startPos <- grep(regExStart,sourceCode)
  endPos <- grep(regExEnd,sourceCode)

  if (is.unsorted(as.vector(rbind(startPos,endPos)),strictly=TRUE)) {
    #browser()#debug
    stop("incomplete iteration blocks, forgot [[END]] declaration? symbol: ",iterSymbol)
  }

  indepPos <- seq_along(startPos)
  if (!showDep)
    indepPos <- grep("[[R#",sourceCode[startPos],fixed=TRUE,invert=TRUE)
  data.frame(start=startPos[indepPos],end=endPos[indepPos])
}

getChunkPars <- function(chunk) {

  firstLine <- chunk[1]
  stopifnot(grepl("^ *//R#",firstLine))
  matchRes <- gregexpr("[^ =]+=+[^ =]+",firstLine)
  parEqVal <- regmatches(firstLine,matchRes)[[1]]
  # sapply(regmatches(parEqVal,regexec("(.*)=+(.*)",parEqVal)),function(x) x[[2]])
  parValList <- regmatches(parEqVal,regexec("([^=]+)=+([^=]+)",parEqVal))
  parName <- sapply(parValList,function(x) x[[2]])
  parVal <- sapply(parValList,function(x) x[[3]])
  names(parVal) <- parName
  parVal
}


replaceSymbols <- function(iterSymbol, varFrame, sourceCode) {

  modSourceCode <- sourceCode
  while(nrow(chunkPos <- getIterChunkPos(iterSymbol, modSourceCode, showDep=FALSE))>0) {
    curChunk <- modSourceCode[chunkPos$start[1]:chunkPos$end[1]]
    chunkPars <- getChunkPars(curChunk)
    chunkBlocks <- NULL
    # varFrame conditional on specified parameters
    curVarFrame <- varFrame
    for (curPar in names(chunkPars))
      curVarFrame <- curVarFrame[curVarFrame[,curPar]==chunkPars[curPar],]
    # replicate each chunk
    for (iter in seq_len(nrow(curVarFrame))) {
      modChunk <- curChunk
      # subsitute all variables in chunk
      for (varName in names(varFrame))
        modChunk <- gsub(paste0("\\[\\[R#",varName,"\\]\\]"),curVarFrame[iter,varName],modChunk)
      #stopifnot(!any(grep("[[R#",modChunk,fixed=TRUE)))
      chunkBlocks <- c(chunkBlocks,modChunk[c(-1,-length(modChunk))])
    }
    startIdx <- seq_len(chunkPos$start[1]-1)
    endIdx <- chunkPos$end[1] + seq_len(length(modSourceCode)-chunkPos$end[1])

    if (nrow(curVarFrame)>0)
      modSourceCode <- c(modSourceCode[startIdx],chunkBlocks,modSourceCode[endIdx])
    else
      modSourceCode <- c(modSourceCode[startIdx],modSourceCode[endIdx])
  }
  modSourceCode
}

#' Transform Templated Source
#'
#' Transform character vector by replacing iteration blocks and templated variables by strings.
#'
#' @param dfList a named list with data frames as elements. The name of an element denotes the name of an iteration block.
#'               The column names of the data frames denote the names of the template variables.
#'               Each line in a data frame will be one realization of the template variables. See example.
#' @param sourceCode character vector with templated strings
#' @param fileDf data frame with columns \code{source} and \code{destination}.
#'               The templated sources will be read from the files in column \code{source} and
#'               the transformed source will be output to the files in column \code{destination}.
#' @param overwrite Should destination files be overwritten when they already exists. Default is FALSe.
#'
#' @return character vector with iteration blocks and template variables substituted according to information in \code{dfList}.
#' @details
#' The character vector \code{sourceCode} can contain iteration blocks.
#' Template variables can occur within each iteration block.
#' Iteration blocks have the following form:
#'
#' \code{//R# <symbol name>[[start]]} \cr
#' \code{some text with [[R#<template variable>]]} \cr
#' \code{//R# <symbol name>[[end]]}
#'
#' The name of the iteration block is given by \code{<symbol name>}.
#' The template variables within an iteration block are written as \code{[[R#<template variable>]]}.
#'
#' It is also possible to define iteration blocks within other iteration blocks.
#' If their template variables should be conditioned on template variables of the parent block,
#' the iteration block header has to be adapted:
#'
#' \code{//R# <symbol name[[start]] <someVar>=[[#<someVar>]]}
#'
#' Such an iteration block will not be transformed until \code{[[#someVar]]} is replaced by a value.
#' Once the value is filled in, the data frame associated with the iteration block is filtered.
#' Only lines that contain the value filled in for \code{[[R#someVar]]} will be kept.
#' See the second example for further clarification.
#'
#'
#' @export
#'
#' @examples
#' ######################################
#' # First example: basic functionality
#' ######################################
#'
#' dfList <- list(name1=data.frame(fun1Name=c("sin","cos"),stringsAsFactors=FALSE),
#' name2=data.frame(fun2Name=c("tan","atan"),argName=c("angle","x"),stringsAsFactors=FALSE))
#'
#' sourceCode <- c(
#' "//R# name1[[start]]","double [[R#fun1Name]](double x);","//R# name1[[end]]",
#' "//R# name2[[start]]","double [[R#fun2Name]](double [[R#argName]]);","//R# name2[[end]]")
#'
#' transformSource(dfList, sourceCode)
#'
#' ################################################
#' # Second example: conditional iteration blocks
#' ################################################
#'
#' dfList <- list(
#'   name1=data.frame(fun1Name=c("sin","cos"),stringsAsFactors=FALSE),
#'   name2=data.frame(fun2Name=c("tan","atan"),argName=c("angle","x"),stringsAsFactors=FALSE),
#'   name3=data.frame(argName=c("angle","x"),str=c("an angle","something else"),stringsAsFactors=FALSE)
#' )
#'
#' sourceCode <- c(
#' "//R# name1[[start]]","double [[R#fun1Name]](double x);","//R# name1[[end]]",
#' "//R# name2[[start]]",
#' "double [[R#fun2Name]](double [[R#argName]]) {",
#' "  //R# name3[[start]] argName=[[R#argName]]",
#' "  printf(\"We find [[R#str]]\");",
#' "  //R# name3[[end]]",
#' " }",
#' "//R# name2[[end]]")
#'
#' transformedSource <- transformSource(dfList, sourceCode)
#' cat("--- Original Source ---\n",paste0(sourceCode,collapse="\n"),"\n\n")
#' cat("--- Transformed Source ---\n",paste0(transformedSource,collapse="\n"),"\n\n")
#'
transformSource <- function(dfList, sourceCode) {

  iterSymbols <- names(dfList)
  finSourceCode <- NULL
  modSourceCode <- sourceCode
  while(!identical(modSourceCode,finSourceCode)) {
    finSourceCode <- modSourceCode
    for (curSymbol in iterSymbols)
      modSourceCode <- replaceSymbols(curSymbol,dfList[[curSymbol]],modSourceCode)
  }
  finSourceCode
}
#' @rdname transformSource
#' @export
transformSourceFiles <- function(dfList, fileDf,overwrite=FALSE) {

  stopifnot(is.data.frame(fileDf),
            all(names(fileDf) %in% c("source","destination")),
            !(any(file.exists(fileDf[["destination"]])) && !isTRUE(overwrite)),
            all(fileDf[["source"]]!=fileDf[["destination"]]))

  for (fileIdx in seq_len(nrow(fileDf))) {
    cat("converting ",fileDf[fileIdx,"source"]," and writing to ",fileDf[fileIdx,"destination"],"\n")
    sourceCode <- readLines(fileDf[fileIdx,"source"])
    modSourceCode <- transformSource(dfList, sourceCode)
    writeLines(modSourceCode,con=fileDf[fileIdx,"destination"])
  }
}


