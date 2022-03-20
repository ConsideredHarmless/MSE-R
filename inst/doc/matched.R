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
confidenceLevel <- 0.95
cr <- pointIdentifiedCR(
    dataArray, groupIDs, optResult$optArg,
    ssSize, numSubsamples, confidenceLevel,
    optimizeScoreArgs)

## -----------------------------------------------------------------------------
plotCR(cr$estimates)

## -----------------------------------------------------------------------------
cr$samples

## -----------------------------------------------------------------------------
matchedData$noM
matchedData$noU
matchedData$noD
matchedData$noAttr

## -----------------------------------------------------------------------------
matchedData$header

## -----------------------------------------------------------------------------
length(matchedData$distanceMatrices)
length(matchedData$matchMatrices)

## -----------------------------------------------------------------------------
mIdx <- 3
uIdx <- 4
dIdx <- 7
matchedData$matchMatrices[[mIdx]][dIdx, uIdx]

## -----------------------------------------------------------------------------
# Leaving an index blank takes all elements for that dimension.
matchedData$matchMatrices[[mIdx]][, uIdx]
# `which` returns the indices where the elements above are `1` -- i.e. the
# downstream matches of upstream '4'.
which(matchedData$matchMatrices[[mIdx]][, uIdx] == 1)

## -----------------------------------------------------------------------------
aIdx <- 3
matchedData$distanceMatrices[[mIdx]][aIdx, dIdx, uIdx]
matchedData$distanceMatrices[[mIdx]][, dIdx, uIdx]

## -----------------------------------------------------------------------------
mIdx <- 1
length(ineqmembers)
names(ineqmembers[[mIdx]])

## -----------------------------------------------------------------------------
ineqmembers[[mIdx]]$numIneqs
length(ineqmembers[[mIdx]]$fctUpIdxs)

## -----------------------------------------------------------------------------
mIdx <- 3
uIdx <- 4
dIdx <- 7
ineqIdx <- 1
termIdx <- 1
ineqmembers[[mIdx]]$fctUpIdxs[[ineqIdx]][termIdx]
ineqmembers[[mIdx]]$fctDnIdxs[[ineqIdx]][termIdx]
ineqmembers[[mIdx]]$cfcUpIdxs[[ineqIdx]][termIdx]
ineqmembers[[mIdx]]$cfcDnIdxs[[ineqIdx]][termIdx]

## -----------------------------------------------------------------------------
getIneqTermsIdxs(ineqmembers[[mIdx]], ineqIdx)
getAllIneqTermsIdxs(ineqmembers[[mIdx]])[[ineqIdx]]

## -----------------------------------------------------------------------------
dim(dataArray)

## -----------------------------------------------------------------------------
groupIDs

## -----------------------------------------------------------------------------
mIdx <- 2
qualifiedIndices <- which(groupIDs == mIdx)
qualifiedIndices
dataArray[, qualifiedIndices]

## -----------------------------------------------------------------------------
selectedGroups <- c(1, 3)
qualifiedIndices <- which(groupIDs %in% selectedGroups)
qualifiedIndices

## -----------------------------------------------------------------------------
makeBounds(matchedData$noAttr, 100)

## -----------------------------------------------------------------------------
makeBounds(matchedData$noAttr, 10, -20)

## -----------------------------------------------------------------------------
bounds <- list(lower=c(-1, -2, -3, -5), upper=c(2, 3, 5, 8))
bounds

## -----------------------------------------------------------------------------
optimParams

