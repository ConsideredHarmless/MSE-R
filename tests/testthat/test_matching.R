# The assignment tests are almost copied-and-pasted from the Mathematica ones.
# The Mathematica version uses transposed arrays (upstream first) with respect
# to our version, so we transpose before and after.

# Shared across unit tests.
payoffMatrix <- t(rbind(
    c(910.41, 707.28, 706.35, 621.92, 726.38,
      960.84, 754.13, 764.83, 801.89, 701.89,
      653.25, 719.04, 799.99, 774.68, 835.48),
    c(806.28, 686.7, 681.97, 604.14, 723.23,
      886.22, 734.38, 747.58, 770.07, 675.69,
      642.93, 693.31, 784.32, 742.03, 803.43),
    c(1271.07, 720.4, 684.3, 603.58, 728.8,
      1176.88, 730.83, 770.25, 862.68, 669.13,
      646.01, 733.68, 776.64, 879.02, 863.36),
    c(874.13, 657.26, 637.2, 565.54, 700.17,
      905.53, 690.04, 720.91, 753.48, 623.97,
      615.22, 660.75, 744.59, 744.02, 770.05),
    c(1031.32, 715.69, 691.15, 610.02, 749.09,
      1033.23, 744.3, 779.32, 827.8, 677.75,
      659.65, 723.39, 798.59, 824.87, 842.86)))

# # TODO Precompute and store in a file.
filename <- system.file("extdata", "precomp_testdata.dat", package = "maxscoreest")
data <- import(filename)
beta <- c(1, 2, 1.5, 2.3)
payoffMatrices <- evaluatePayoffMatrices(data$distanceMatrices, beta)

test_that("generateAssignmentMatrix with numeric values and 1-1 quotas", {
    quotaU <- 1
    quotaD <- 1
    expected <- t(rbind(
        c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
        c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
        c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
        c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
    expect_equal(
        generateAssignmentMatrix(payoffMatrix, quotaU, quotaD),
        expected)
})

test_that("generateAssignmentMatrix with numeric values and 3-1 quotas", {
    quotaU <- 3
    quotaD <- 1
    expected <- t(rbind(
        c(0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0),
        c(0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
        c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0),
        c(0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0),
        c(0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1)))
    expect_equal(
        generateAssignmentMatrix(payoffMatrix, quotaU, quotaD),
        expected)
})

test_that("generateAssignmentMatrix with numeric values and 3-2 quotas", {
    quotaU <- 3
    quotaD <- 2
    expected <- t(rbind(
        c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1),
        c(0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0),
        c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0),
        c(0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1),
        c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0)))
    expect_equal(
        generateAssignmentMatrix(payoffMatrix, quotaU, quotaD),
        expected)
})

test_that("generateAssignmentMatrix with numeric values and {3,3,3,3,3}-1 quotas", {
    quotaU <- c(3, 3, 3, 3, 3)
    quotaD <- 1
    expected <- t(rbind(
        c(0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0),
        c(0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
        c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0),
        c(0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0),
        c(0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1)))
    expect_equal(
        generateAssignmentMatrix(payoffMatrix, quotaU, quotaD),
        expected)
})

test_that("generateAssignmentMatrix with numeric values and {3,1,3,0,2}-1 quotas", {
    quotaU <- c(3, 1, 3, 0, 2)
    quotaD <- 1
    expected <- t(rbind(
        c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1),
        c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0),
        c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        c(0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0)))
    expect_equal(
        generateAssignmentMatrix(payoffMatrix, quotaU, quotaD),
        expected)
})

test_that("generateAssignmentMatrix with numeric values and {3,1,3,0,2}-{...} quotas", {
    quotaU <- c(3, 1, 3, 0, 2)
    quotaD <- c(1, 0, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
    expected <- t(rbind(
        c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0),
        c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0),
        c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        c(0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0)))
    expect_equal(
        generateAssignmentMatrix(payoffMatrix, quotaU, quotaD),
        expected)
})

test_that("CmatchMatrix on precomp_testdata.dat -- test 1", {
    quotasU <- rep(2, 3)
    quotasD <- rep(1, 3)
    expected <- list(
        t(rbind(
            c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
            c(0, 0, 0, 1, 0, 1, 0, 0, 0, 0),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
            c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
            c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
            c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0))),
        t(rbind(
            c(0, 0, 0, 0, 1, 0, 0, 0, 1, 0),
            c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
            c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1),
            c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            c(0, 0, 0, 0, 0, 1, 0, 1, 0, 0))),
        t(rbind(
            c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
            c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0),
            c(0, 0, 1, 0, 0, 0, 0, 1, 0, 0),
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1))))
    expect_equal(
        CmatchMatrices(payoffMatrices, quotasU, quotasD),
        expected)
})
