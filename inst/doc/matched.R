## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(maxscoreest)

## -----------------------------------------------------------------------------
filename <- system.file("extdata", "precomp_testdata.dat", package = "maxscoreest")

## -----------------------------------------------------------------------------
matchedData <- importMatched(filename)

## -----------------------------------------------------------------------------
ineqmembers <- Cineqmembers(matchedData$mate)

## -----------------------------------------------------------------------------
dataArray <- CdataArray(matchedData$distanceMatrices, ineqmembers)

## -----------------------------------------------------------------------------
bounds <- makeBounds(matchedData$noAttr, 100)
optimParams <- getDefaultOptimParams()

## -----------------------------------------------------------------------------
randomSeed <- 42
set.seed(randomSeed)

## -----------------------------------------------------------------------------
optimizeScoreArgs <- list(
    dataArray = dataArray,
    bounds = bounds,
    optimParams = optimParams,
    getIneqSat = TRUE,
    permuteInvariant = TRUE)
optResult <- do.call(optimizeScoreFunction, optimizeScoreArgs)

## -----------------------------------------------------------------------------
optResult$optArg
optResult$optVal
calcPerMarketStats(optResult$ineqSat, makeGroupIDs(ineqmembers))

## -----------------------------------------------------------------------------
ssSize <- 2
numSubsamples <- 50

## -----------------------------------------------------------------------------
groupIDs <- makeGroupIDs(ineqmembers)

## -----------------------------------------------------------------------------
cr <- pointIdentifiedCR(
    dataArray, groupIDs, optResult$optArg,
    ssSize, numSubsamples,
    optimizeScoreArgs)

## -----------------------------------------------------------------------------
plotCR(cr$estimates)

