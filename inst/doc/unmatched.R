## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(maxscoreest)

## -----------------------------------------------------------------------------
filenameUp <- system.file("extdata", "unmatched_testdata_up.csv", package = "maxscoreest")
filenameDn <- system.file("extdata", "unmatched_testdata_dn.csv", package = "maxscoreest")
dataUnmatched <- importUnmatched(filenameUp, filenameDn)

## -----------------------------------------------------------------------------
unevalPayoffMatrices <- makeDistanceMatrices(
    dataUnmatched$attributeMatricesUp,
    dataUnmatched$attributeMatricesDn)

## -----------------------------------------------------------------------------
beta <- c(0.5, 1.5)

## -----------------------------------------------------------------------------
payoffMatrices <- evaluatePayoffMatrices(unevalPayoffMatrices, beta)

## -----------------------------------------------------------------------------
matchMatrices <- CmatchMatrices(
    payoffMatrices, dataUnmatched$quotasUp, dataUnmatched$quotasDn)
matchMatrices

