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
    expect_equal(getAllIneqTermsIdxs(expected), expectedHR, ignore_attr=TRUE)
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
    expect_equal(getAllIneqTermsIdxs(expected), expectedHR, ignore_attr=TRUE)
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
    expect_equal(getAllIneqTermsIdxs(expected), expectedHR, ignore_attr=TRUE)
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
    expect_equal(getAllIneqTermsIdxs(expected), expectedHR, ignore_attr=TRUE)
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
    expect_equal(getAllIneqTermsIdxs(expected), expectedHR, ignore_attr=TRUE)
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
    expect_equal(getAllIneqTermsIdxs(expected), expectedHR, ignore_attr=TRUE)
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
    expect_equal(getAllIneqTermsIdxs(expected), expectedHR, ignore_attr=TRUE)
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
    expect_equal(CineqmembersSingle(marketMates), expected)
    expect_equal(getAllIneqTermsIdxs(expected), expectedHR, ignore_attr=TRUE)
})

test_that("Identity Matrix 1-1 relationships with 5 up and 5 down", {
    marketMates <- list(
        UpStream  = 1:5,
        DownMates = as.list(1:5))
    expected <- list(
        fctUpIdxs = list(c(1, 2), c(1, 3), c(1, 4), c(1, 5), c(2, 3),
                         c(2, 4), c(2, 5), c(3, 4), c(3, 5), c(4, 5)),
        fctDnIdxs = list(c(1, 2), c(1, 3), c(1, 4), c(1, 5), c(2, 3),
                         c(2, 4), c(2, 5), c(3, 4), c(3, 5), c(4, 5)),
        cfcUpIdxs = list(c(1, 2), c(1, 3), c(1, 4), c(1, 5), c(2, 3),
                         c(2, 4), c(2, 5), c(3, 4), c(3, 5), c(4, 5)),
        cfcDnIdxs = list(c(2, 1), c(3, 1), c(4, 1), c(5, 1), c(3, 2),
                         c(4, 2), c(5, 2), c(4, 3), c(5, 3), c(5, 4)),
        numIneqs  = 10)
    expectedHR <- list(
        rbind(c(1, 1, 1, 2),
              c(2, 2, 2, 1)),
        rbind(c(1, 1, 1, 3),
              c(3, 3, 3, 1)),
        rbind(c(1, 1, 1, 4),
              c(4, 4, 4, 1)),
        rbind(c(1, 1, 1, 5),
              c(5, 5, 5, 1)),
        rbind(c(2, 2, 2, 3),
              c(3, 3, 3, 2)),
        rbind(c(2, 2, 2, 4),
              c(4, 4, 4, 2)),
        rbind(c(2, 2, 2, 5),
              c(5, 5, 5, 2)),
        rbind(c(3, 3, 3, 4),
              c(4, 4, 4, 3)),
        rbind(c(3, 3, 3, 5),
              c(5, 5, 5, 3)),
        rbind(c(4, 4, 4, 5),
              c(5, 5, 5, 4)))
    expect_equal(CineqmembersSingle(marketMates), expected)
    expect_equal(getAllIneqTermsIdxs(expected), expectedHR, ignore_attr=TRUE)
})

test_that("3 small markets", {
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
    expected <- list(
        list(
            fctUpIdxs = list(c(1, 2, 2), c(1, 3), c(2, 2, 3)),
            fctDnIdxs = list(c(1, 1, 2), c(1, 2), c(1, 2, 2)),
            cfcUpIdxs = list(c(1, 1, 2), c(1, 3), c(2, 3, 3)),
            cfcDnIdxs = list(c(1, 2, 1), c(2, 1), c(2, 1, 2)),
            numIneqs  = 3),
        list(
            fctUpIdxs = list(c(1, 2)),
            fctDnIdxs = list(c(1, 2)),
            cfcUpIdxs = list(c(1, 2)),
            cfcDnIdxs = list(c(2, 1)),
            numIneqs  = 1),
        list(
            fctUpIdxs = list(c(1, 2), c(1, 3), c(2, 3)),
            fctDnIdxs = list(c(1, 2), c(1, 3), c(2, 3)),
            cfcUpIdxs = list(c(1, 2), c(1, 3), c(2, 3)),
            cfcDnIdxs = list(c(2, 1), c(3, 1), c(3, 2)),
            numIneqs  = 3))
    expectedHR <- list(
        list(
            rbind(c(1, 1, 1, 1),
                  c(2, 1, 1, 2),
                  c(2, 2, 2, 1)),
            rbind(c(1, 1, 1, 2),
                  c(3, 2, 3, 1)),
            rbind(c(2, 1, 2, 2),
                  c(2, 2, 3, 1),
                  c(3, 2, 3, 2))),
        list(
            rbind(c(1, 1, 1, 2),
                  c(2, 2, 2, 1))),
        list(
            rbind(c(1, 1, 1, 2),
                  c(2, 2, 2, 1)),
            rbind(c(1, 1, 1, 3),
                  c(3, 3, 3, 1)),
            rbind(c(2, 2, 2, 3),
                  c(3, 3, 3, 2))))
    expect_equal(Cineqmembers(mates), expected)
    expect_equal(lapply(expected, getAllIneqTermsIdxs), expectedHR, ignore_attr=TRUE)
})
