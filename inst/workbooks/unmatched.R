# Workbook template for unmatched data

library(maxscoreest)

# Choose data file. Use `system.file` to find a test file distributed with the
# package, or simply use the absolute path to your own file.
filenameUp <- system.file("extdata", "unmatched_testdata_up.csv", package = "maxscoreest")
filenameDn <- system.file("extdata", "unmatched_testdata_dn.csv", package = "maxscoreest")
# filenameUp <- "/path/to/my/own/data_up"
# filenameDn <- "/path/to/my/own/data_dn"

# Choose a (free) parameter vector. Its length must be one less that the total
# number of attributes.
beta <- c(0.5, 1.5)

# Create the relevant data structures.
dataUnmatched <- importUnmatched(filenameUp, filenameDn)
unevalPayoffMatrices <- makeDistanceMatrices(
    dataUnmatched$attributeMatricesUp,
    dataUnmatched$attributeMatricesDn)
payoffMatrices <- evaluatePayoffMatrices(unevalPayoffMatrices, beta)

# Calculate optimal matching.
matchMatrices <- CmatchMatrices(
    payoffMatrices, dataUnmatched$quotasUp, dataUnmatched$quotasDn)

# View results.
matchMatrices
