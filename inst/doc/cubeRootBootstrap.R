## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(maxscoreest)
filename <- system.file("extdata", "precomp_testdata.dat", package = "maxscoreest")
matchedData <- importMatched(filename)
ineqmembers <- Cineqmembers(matchedData$mate)
dataArray <- CdataArray(matchedData$distanceMatrices, ineqmembers)
bounds <- makeBounds(matchedData$noAttr, 10)
optimParams <- getDefaultOptimParams()
randomSeed <- 42
optimizeScoreArgs <- list(
    dataArray = dataArray,
    bounds = bounds,
    optimParams = optimParams,
    getIneqSat = TRUE,
    permuteInvariant = TRUE,
    numRuns = 1000)
set.seed(randomSeed)
optResult <- do.call(optimizeScoreFunction, optimizeScoreArgs)
pointEstimate <- optResult$optArg
pointEstimate

## -----------------------------------------------------------------------------
numSubsamples <- 20
ssSize <- NULL
confidenceLevel <- 0.95
groupIDs <- makeGroupIDs(ineqmembers)

## -----------------------------------------------------------------------------
options <- list()
set.seed(randomSeed)
cr <- cubeRootBootstrapCR(
    dataArray, groupIDs, pointEstimate,
    ssSize, numSubsamples, confidenceLevel,
    optimizeScoreArgs, options)
cr

## ----compare1-----------------------------------------------------------------
options$bw <- 0.8
options$Hest <- "numder"
set.seed(randomSeed)
crNumder <- cubeRootBootstrapCR(
    dataArray, groupIDs, pointEstimate,
    ssSize, numSubsamples, confidenceLevel,
    optimizeScoreArgs, options)
crNumder[c("cr", "H")]

options$Hest <- "plugin"
set.seed(randomSeed)
crPlugin <- cubeRootBootstrapCR(
    dataArray, groupIDs, pointEstimate,
    ssSize, numSubsamples, confidenceLevel,
    optimizeScoreArgs, options)
crPlugin[c("cr", "H")]

## ----rot----------------------------------------------------------------------
options$bw <- "rot"
options$Hest <- "numder"
set.seed(randomSeed)
crNumder <- cubeRootBootstrapCR(
    dataArray, groupIDs, pointEstimate,
    ssSize, numSubsamples, confidenceLevel,
    optimizeScoreArgs, options)
crNumder[c("cr", "H")]

options$Hest <- "plugin"
set.seed(randomSeed)
crPlugin <- cubeRootBootstrapCR(
    dataArray, groupIDs, pointEstimate,
    ssSize, numSubsamples, confidenceLevel,
    optimizeScoreArgs, options)
crPlugin[c("cr", "H")]

## ----makePosDef---------------------------------------------------------------
pointEstimate <- optResult$bestRuns[[67]]$optArg
pointEstimate
options$makePosDef <- FALSE
set.seed(randomSeed)
crNonPSD <- cubeRootBootstrapCR(
    dataArray, groupIDs, pointEstimate,
    ssSize, numSubsamples, confidenceLevel,
    optimizeScoreArgs, options)
crNonPSD[c("cr", "H")]
eigen(crNonPSD$H)$values # has a negative eigenvalue, is not PSD

options$makePosDef <- TRUE
set.seed(randomSeed)
crMakePosDef <- cubeRootBootstrapCR(
    dataArray, groupIDs, pointEstimate,
    ssSize, numSubsamples, confidenceLevel,
    optimizeScoreArgs, options)
crMakePosDef[c("cr", "H")]
eigen(crMakePosDef$H)$values

