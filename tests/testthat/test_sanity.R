context("Sanity check")
# This is ugly, but necessary since we are not yet in a package.
# TODO replace with library(...) when we convert to a package.
setwd('../../')
source("mse.R")

test_that("Sanity check #1", {
    expect_equal(4*10+2,  42)
    expect_equal(100/2-8, 42)
})
