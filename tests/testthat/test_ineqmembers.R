context("ineqmembers")
setwd('../../')
source("mse.R")

# Note: we actually require a weaker form of equality: equality up to reordering
# of inequalities. This will be relevant when comparing results with the old
# code. Also, reordering of the terms should also not matter. The solution would
# be to converted to a canonical form (i.e. sort them) before comparing them.
# The expected structures should be read vertically by humans. As a reminder,
# each list member corresponds to an inequality, and each index quadruple to a
# term (factual-counterfactual pair).

# We also test the function getIneqTermsIdxs, which converts the output of
# CineqmembersSingle to a more human-readable form. In this form, each list
# element corresponds to an inequality, and each row to a term
# (factual-counterfactual pair).

test_that("2 pairs 1-1 relationships", {
    marketMates <- list(
        UpStream  = c(1, 2),
        DownMates = list(1, 2))
    expected <- list(
        fctUpIdxs = list(c(1, 2)),
        fctDnIdxs = list(c(1, 2)),
        cfcUpIdxs = list(c(1, 2)),
        cfcDnIdxs = list(c(2, 1)),
        numIneqs  = 1)
    expectedHR <- list(
        rbind(c(1, 1, 1, 2),
              c(2, 2, 2, 1)))
    expect_equal(CineqmembersSingle(marketMates), expected)
    expect_equivalent(getAllIneqTermsIdxs(expected), expectedHR)
})

test_that("3 pairs 1-1 relationships", {
    marketMates <- list(
        UpStream  = c(1, 2, 3),
        DownMates = list(1, 2, 3))
    expected <- list(
        fctUpIdxs = list(c(1, 2), c(1, 3), c(2, 3)),
        fctDnIdxs = list(c(1, 2), c(1, 3), c(2, 3)),
        cfcUpIdxs = list(c(1, 2), c(1, 3), c(2, 3)),
        cfcDnIdxs = list(c(2, 1), c(3, 1), c(3, 2)),
        numIneqs  = 3)
    expectedHR <- list(
        rbind(c(1, 1, 1, 2),
              c(2, 2, 2, 1)),
        rbind(c(1, 1, 1, 3),
              c(3, 3, 3, 1)),
        rbind(c(2, 2, 2, 3),
              c(3, 3, 3, 2)))
    expect_equal(CineqmembersSingle(marketMates), expected)
    expect_equivalent(getAllIneqTermsIdxs(expected), expectedHR)
})

test_that("One to many relationships", {
    marketMates <- list(
        UpStream  = c(1, 2, 3),
        DownMates = list(c(1, 2), c(2), c(2, 3)))
    expected <- list(
        fctUpIdxs = list(c(1, 1, 2), c(1, 1, 3, 3), c(2, 3, 3)),
        fctDnIdxs = list(c(1, 2, 2), c(1, 2, 2, 3), c(2, 2, 3)),
        cfcUpIdxs = list(c(1, 2, 2), c(1, 1, 3, 3), c(2, 2, 3)),
        cfcDnIdxs = list(c(2, 1, 2), c(2, 3, 1, 2), c(2, 3, 2)),
        numIneqs  = 3)
    expectedHR <- list(
        rbind(c(1, 1, 1, 2),
              c(1, 2, 2, 1),
              c(2, 2, 2, 2)),
        rbind(c(1, 1, 1, 2),
              c(1, 2, 1, 3),
              c(3, 2, 3, 1),
              c(3, 3, 3, 2)),
        rbind(c(2, 2, 2, 2),
              c(3, 2, 2, 3),
              c(3, 3, 3, 2)))
    expect_equal(CineqmembersSingle(marketMates), expected)
    expect_equivalent(getAllIneqTermsIdxs(expected), expectedHR)
})

test_that("Many to one relationships", {
    marketMates <- list(
        UpStream  = c(1, 2, 3),
        DownMates = list(c(1), c(1, 2, 3), c(3)))
    expected <- list(
        fctUpIdxs = list(c(1, 2, 2, 2), c(1, 3), c(2, 2, 2, 3)),
        fctDnIdxs = list(c(1, 1, 2, 3), c(1, 3), c(1, 2, 3, 3)),
        cfcUpIdxs = list(c(1, 1, 1, 2), c(1, 3), c(2, 3, 3, 3)),
        cfcDnIdxs = list(c(1, 2, 3, 1), c(3, 1), c(3, 1, 2, 3)),
        numIneqs  = 3)
    expectedHR <- list(
        rbind(c(1, 1, 1, 1),
              c(2, 1, 1, 2),
              c(2, 2, 1, 3),
              c(2, 3, 2, 1)),
        rbind(c(1, 1, 1, 3),
              c(3, 3, 3, 1)),
        rbind(c(2, 1, 2, 3),
              c(2, 2, 3, 1),
              c(2, 3, 3, 2),
              c(3, 3, 3, 3)))
    expect_equal(CineqmembersSingle(marketMates), expected)
    expect_equivalent(getAllIneqTermsIdxs(expected), expectedHR)
})

test_that("Unmatched in a 1-1 relationship", {
    marketMates <- list(
        UpStream  = c(1, 2, 3),
        DownMates = list(c(1), c(), c(2)))
    expected <- list(
        fctUpIdxs = list(c(1), c(1, 3), c(3)),
        fctDnIdxs = list(c(1), c(1, 2), c(2)),
        cfcUpIdxs = list(c(2), c(1, 3), c(2)),
        cfcDnIdxs = list(c(1), c(2, 1), c(2)),
        numIneqs  = 3)
    expectedHR <- list(
        rbind(c(1, 1, 2, 1)),
        rbind(c(1, 1, 1, 2),
              c(3, 2, 3, 1)),
        rbind(c(3, 2, 2, 2)))
    expect_equal(CineqmembersSingle(marketMates), expected)
    expect_equivalent(getAllIneqTermsIdxs(expected), expectedHR)
})

test_that("Unmatched in a one to many relationship (1)", {
    marketMates <- list(
        UpStream  = c(1, 2, 3),
        DownMates = list(c(1, 2), c(), c()))
    expected <- list(
        fctUpIdxs = list(c(1, 1), c(1, 1), c()),
        fctDnIdxs = list(c(1, 2), c(1, 2), c()),
        cfcUpIdxs = list(c(2, 2), c(3, 3), c()),
        cfcDnIdxs = list(c(1, 2), c(1, 2), c()),
        numIneqs  = 3)
    expectedHR <- list(
        rbind(c(1, 1, 2, 1),
              c(1, 2, 2, 2)),
        rbind(c(1, 1, 3, 1),
              c(1, 2, 3, 2)),
        rbind())
    expect_equal(CineqmembersSingle(marketMates), expected)
    expect_equivalent(getAllIneqTermsIdxs(expected), expectedHR)
})

test_that("Unmatched in a one to many relationship (2)", {
    marketMates <- list(
        UpStream  = c(1, 2, 3),
        DownMates = list(c(1), c(), c(2, 3)))
    expected <- list(
        fctUpIdxs = list(c(1), c(1, 3, 3), c(3, 3)),
        fctDnIdxs = list(c(1), c(1, 2, 3), c(2, 3)),
        cfcUpIdxs = list(c(2), c(1, 1, 3), c(2, 2)),
        cfcDnIdxs = list(c(1), c(2, 3, 1), c(2, 3)),
        numIneqs  = 3)
    expectedHR <- list(
        rbind(c(1, 1, 2, 1)),
        rbind(c(1, 1, 1, 2),
              c(3, 2, 1, 3),
              c(3, 3, 3, 1)),
        rbind(c(3, 2, 2, 2),
              c(3, 3, 2, 3)))
    expect_equal(CineqmembersSingle(marketMates), expected)
    expect_equivalent(getAllIneqTermsIdxs(expected), expectedHR)
})

test_that("All unmatched", {
    marketMates <- list(
        UpStream  = c(1, 2, 3),
        DownMates = list(c(), c(), c()))
    expected <- list(
        fctUpIdxs = list(c(), c(), c()),
        fctDnIdxs = list(c(), c(), c()),
        cfcUpIdxs = list(c(), c(), c()),
        cfcDnIdxs = list(c(), c(), c()),
        numIneqs  = 3)
    expectedHR <- list(
        rbind(),
        rbind(),
        rbind())
    expect_equivalent(getAllIneqTermsIdxs(expected), expectedHR)
    expect_equal(CineqmembersSingle(marketMates), expected)
})

# TODO
# test_that("Identity Matrix 1-1 relationships with 10 up and 10 down", {
#     marketMates <- list(
#         UpStream  = 1:10,
#         DownMates = as.list(1:10))
#     expected <- list(
#         fctUpIdxs = list(),
#         fctDnIdxs = list(),
#         cfcUpIdxs = list(),
#         cfcDnIdxs = list(),
#         numIneqs  = 45)
#     expect_equal(CineqmembersSingle(marketMates), expected)
# })

# TODO
# test_that("4 small markets", {
#     expect_equal(NULL, NULL)
# })
