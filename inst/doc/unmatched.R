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

## -----------------------------------------------------------------------------
unmatchedData <- importUnmatched(filenameUp, filenameDn)

## -----------------------------------------------------------------------------
unevalPayoffMatrices <- makeDistanceMatrices(
    unmatchedData$attributeMatricesUp,
    unmatchedData$attributeMatricesDn)

## -----------------------------------------------------------------------------
beta <- c(0.5, 1.5)
payoffMatrices <- evaluatePayoffMatrices(unevalPayoffMatrices, beta)

## -----------------------------------------------------------------------------
matchMatrices <- CmatchMatrices(
    payoffMatrices, unmatchedData$quotasUp, unmatchedData$quotasDn)
matchMatrices

## -----------------------------------------------------------------------------
unmatchedData$noM
unmatchedData$noU
unmatchedData$noD
unmatchedData$noAttr

## -----------------------------------------------------------------------------
unmatchedData$headerUp
unmatchedData$headerDn

## -----------------------------------------------------------------------------
length(unmatchedData$attributeMatricesUp)
length(unmatchedData$attributeMatricesDn)
length(unmatchedData$quotasUp)
length(unmatchedData$quotasDn)

## -----------------------------------------------------------------------------
mIdx <- 2
uIdx <- 1
aIdx <- 3
unmatchedData$attributeMatricesUp[[mIdx]][aIdx, uIdx]
unmatchedData$attributeMatricesUp[[mIdx]][, uIdx]

## -----------------------------------------------------------------------------
mIdx <- 1
unmatchedData$quotasUp[[mIdx]]
unmatchedData$quotasDn[[mIdx]]

## -----------------------------------------------------------------------------
length(payoffMatrices)
mIdx <- 1
dim(payoffMatrices[[mIdx]])
payoffMatrices[[mIdx]]
uIdx <- 1
dIdx <- 2
payoffMatrices[[mIdx]][dIdx, uIdx]

## -----------------------------------------------------------------------------
matchMatrices

## -----------------------------------------------------------------------------
mIdx <- 2
uIdx <- 2
dIdx <- 3
matchMatrices[[mIdx]][dIdx, uIdx]

## -----------------------------------------------------------------------------
# Leaving an index blank takes all elements for that dimension.
matchMatrices[[mIdx]][, uIdx]
# `which` returns the indices where the elements above are `1` -- i.e. the
# downstream matches of upstream '2'.
which(matchMatrices[[mIdx]][, uIdx] == 1)

