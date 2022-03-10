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
optimParams <- list(NP=50, F=0.6, CR=0.5, itermax=100, trace=FALSE, reltol=1e-3)

## -----------------------------------------------------------------------------
randomSeed <- 42
set.seed(randomSeed)

## -----------------------------------------------------------------------------
optResult <- optimizeScoreFunction(
    dataArray = dataArray, bounds = bounds, optimParams = optimParams, getIneqSat = TRUE, permuteInvariant = TRUE
)

## -----------------------------------------------------------------------------
optResult$optArg
optResult$optVal
calcPerMarketStats(optResult$ineqSat, makeGroupIDs(ineqmembers))

## -----------------------------------------------------------------------------
groupIDs <- makeGroupIDs(ineqmembers)
ssSize <- 2
optionsCR <- list(progressUpdate=1, confidenceLevel=0.95, asymptotics="nests")
numSubsamples <- 50
pointEstimate <- as.numeric(optResult$optArg)
optimizeScoreArgs <- list(dataArray = dataArray, bounds = bounds, optimParams = optimParams, getIneqSat = TRUE, permuteInvariant = TRUE)
optimizeScoreArgs$dataArray <- NULL
optimizeScoreArgs$getIneqSat <- FALSE
cr <- pointIdentifiedCR(
    dataArray, groupIDs, pointEstimate,
    ssSize, numSubsamples,
    optimizeScoreArgs = optimizeScoreArgs,
    options = optionsCR)
plotCR(cr$estimates)
print(t(cr$estimates))

