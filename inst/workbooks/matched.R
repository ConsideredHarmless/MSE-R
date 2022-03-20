# Workbook template for matched data

library(maxscoreest)

# Choose data file. Use `system.file` to find a test file distributed with the
# package, or simply use the absolute path to your own file.
filename <- system.file("extdata", "precomp_testdata.dat", package = "maxscoreest")
# filename <- "/path/to/my/own/data"

# Create the relevant data structures.
matchedData <- importMatched(filename)
ineqmembers <- Cineqmembers(matchedData$mate)
dataArray <- CdataArray(matchedData$distanceMatrices, ineqmembers)

# Choose box constraints for optimization. Use `makeBounds` to get symmetrical
# constaints for all parameters, or create custom constaints.
bounds <- makeBounds(matchedData$noAttr, 100)
# bounds <- list(lower=c(-1, -2, -3, -5), upper=c(2, 3, 5, 8))

# Choose optimization parameters. Use `getDefaultOptimParams` to get a
# reasonable collection of parameters, or hand-pick them. See the
# documentation of `DEoptim::DEoptim.control` for more information.
optimParams <- getDefaultOptimParams()

# Choose a random seed for reproducibility.
randomSeed <- 42
set.seed(randomSeed)

# Calculate optimal parameters.
optimizeScoreArgs <- list(
    dataArray = dataArray,
    bounds = bounds,
    optimParams = optimParams,
    getIneqSat = TRUE,
    permuteInvariant = TRUE)
optResult <- do.call(optimizeScoreFunction, optimizeScoreArgs)

# View results.
optResult$optArg
optResult$optVal
calcPerMarketStats(optResult$ineqSat, makeGroupIDs(ineqmembers))

# Choose settings for confidence region estimation.
ssSize <- 2
numSubsamples <- 50
confidenceLevel <- 0.95
# Use `progressUpdate=1` to print iteration information to the console. Use
# `progressUpdate=0`, or omit that element, to disable printing. For more
# information, see the documentation of `pointIdentifiedCR`.
optionsCR <- list(progressUpdate=1)

# Calculate the confidence region estimates.
groupIDs <- makeGroupIDs(ineqmembers)
cr <- pointIdentifiedCR(
    dataArray, groupIDs, optResult$optArg,
    ssSize, numSubsamples, confidenceLevel,
    optimizeScoreArgs, optionsCR)

# View results.
plotCR(cr$estimates)
cr$samples
