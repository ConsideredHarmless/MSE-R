context("inequalities")
setwd('../../')
source("mse.R")

test_that("3 small markets", {
    payoffFunction <- function(mIdx, uIdx, dIdx) { 2^mIdx * 3^uIdx * 5^dIdx }
    mates <- list(
        list(
            UpStream  = c(1, 2, 3),
            DownMates = list(c(1), c(1, 2), c(2))),
        list(
            UpStream  = c(1, 2),
            DownMates = list(c(1), c(2))),
        list(
            UpStream  = c(1, 2, 3),
            DownMates = list(c(1), c(2), c(3))))
    ineqmembers <- CineqmembersNew(mates)
    inequalities <- CinequalitiesNew(payoffFunction, ineqmembers)
    expected <- list(c(300, 960, -180),
                     c(480),
                     c(960, 23040, 14400))
    expect_equal(inequalities, expected)
})
