checkMakeDistanceMatrices <- function(dataUnmatched) {
    attributeMatricesUp <- dataUnmatched$attributeMatricesUp
    attributeMatricesDn <- dataUnmatched$attributeMatricesDn
    distanceMatrices <- makeDistanceMatrices(attributeMatricesUp, attributeMatricesDn)
    allZero <- TRUE
    for (mIdx in 1:dataUnmatched$noM) {
        noU <- dataUnmatched$noU[[mIdx]]
        noD <- dataUnmatched$noD[[mIdx]]
        for (uIdx in 1:noU) {
            for (dIdx in 1:noD) {
                for (kIdx in 1:dataUnmatched$noAttr) {
                    u <- attributeMatricesUp[[mIdx]][kIdx, uIdx]
                    v <- attributeMatricesDn[[mIdx]][kIdx, dIdx]
                    w <- distanceMatrices[[mIdx]][kIdx, dIdx, uIdx]
                    allZero <- allZero && (u * v == w)
                }
            }
        }
    }
    return(allZero)
}

test_that("makeDistanceMatrices", {
    filenameUp <- system.file("extdata", "unmatched_testdata_up.csv", package = "maxscoreest")
    filenameDn <- system.file("extdata", "unmatched_testdata_dn.csv", package = "maxscoreest")
    dataUnmatched <- importUnmatched(filenameUp, filenameDn)
    expect_true(checkMakeDistanceMatrices(dataUnmatched))
})
