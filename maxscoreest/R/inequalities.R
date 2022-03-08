# Cineqmembers(mate) computes the upstream and downstream indices
# for each inequality term for all markets.
#
# For the structure mate, see the import function.
#
# Returns a list of 4-tuples containing indices. For that structure, see
# CineqmembersSingle.
Cineqmembers <- function(mate) {
    mateList <- lapply(mate, as.list)
    return(lapply(mateList, CineqmembersSingle))
}

#' Compute inequality members for a single market
#'
#' Computes the upstream and downstream indices for each inequality term for a
#' single market.
#'
#' Let \eqn{n} be the number of upstreams in the market. Each transposition
#' \eqn{(i, j)} of the set \eqn{{1, 2, ..., n}} yields a single inequality, with
#' the factual terms in the LHS and the counterfactual terms in the RHS. The
#' factual terms correspond to all current matches of both upstream indices
#' (\eqn{i} and \eqn{j}), and the counterfactual terms correspond to the matches
#' where \eqn{i} and \eqn{j} switch partners.
#'
#' @section Inequality members structure:
#' TODO
#'
#' @param marketMates A list with members:
#' \tabular{ll}{
#'   \code{$UpStream} \tab A vector of the unique indices of upstreams for this
#'     market.\cr
#'   \code{$DownMates} \tab A list with n elements, where n is the number of
#'     upstreams. Element \code{$DownMates[[i]]} is a vector of the downstream
#'     indices which the \code{i}-th upstream is matched to.
#' }
#'
#' @return A list with members:
#' \tabular{ll}{
#'   \code{$fctUpIdxs} \tab A list of upstream index vectors for the factual case.\cr
#'   \code{$fctDnIdxs} \tab A list of downstream index vectors for the factual case.\cr
#'   \code{$cfcUpIdxs} \tab A list of upstream index vectors for the counterfactual case.\cr
#'   \code{$cfcDnIdxs} \tab A list of downstream index vectors for the counterfactual case.\cr
#'   \code{$numIneqs}  \tab The number of inequalities for this market. Also the
#'     common length of the above lists.
#' }
#' Each list element of the first four members corresponds to a single
#' inequality. That element is a vector of indices, with one index for each
#' term in that inequality.
CineqmembersSingle <- function(marketMates) {
    uIdxs <- marketMates$UpStream
    n <- length(uIdxs)
    numIneqs = n*(n-1)/2
    # First, we create the transposition indices {(i, j): 1 <= i < j <= n} as a
    # pair of vectors.
    iIdxs <- unlist(lapply(1:n, function (x) { rep(x, n-x) }))
    jIdxs <- unlist(lapply(1:n, function (x) { seq(from=x+1, to=n, length=max(0,n-x)) }))
    dnMates <- marketMates$DownMates # alias for brevity
    # To create the upstream indices, we repeat them as many times as there are
    # corresponding downstream matches.
    # See test "All unmatched" in test_ineqmembers.R. rep_len returns numeric(0)
    # instead of NULL (a.k.a. c()) when n is 0.
    repUp <- function(i, j) {
        n <- length(dnMates[[j]])
        if (n == 0) { return(NULL) }
        return(rep_len(i, n))
    }
    fctUpIdxs <- mapply(function (i, j) { c(repUp(i, i),  repUp(j, j))  }, iIdxs, jIdxs, SIMPLIFY = FALSE)
    fctDnIdxs <- mapply(function (i, j) { c(dnMates[[i]], dnMates[[j]]) }, iIdxs, jIdxs, SIMPLIFY = FALSE)
    cfcUpIdxs <- mapply(function (i, j) { c(repUp(i, j),  repUp(j, i))  }, iIdxs, jIdxs, SIMPLIFY = FALSE)
    cfcDnIdxs <- mapply(function (i, j) { c(dnMates[[j]], dnMates[[i]]) }, iIdxs, jIdxs, SIMPLIFY = FALSE)
    return(list(
        fctUpIdxs = fctUpIdxs,
        fctDnIdxs = fctDnIdxs,
        cfcUpIdxs = cfcUpIdxs,
        cfcDnIdxs = cfcDnIdxs,
        numIneqs  = numIneqs))
}

# Cinequalities(payoffFunction, ineqmembers) computes, for each inequality,
# the value
#   (sum of payoff function values over LHS terms) -
#   (sum of payoff function values over RHS terms).
# payoffFunction is a function which takes three arguments (mIdx, uIdx, dIdx)
# and returns the value of the payoff function for that triple.
# For the structure ineqmembers, see the function Cineqmembers.
#
# Returns a list of vectors, one for each market. Each vector element
# corresponds to a single inequality.
Cinequalities <- function(payoffFunction, ineqmembers) {
    g <- function(mIdx, ineqmembersSingle) {
        f <- function(ineqIdx) {
            numTerms <- length(ineqmembersSingle$fctUpIdxs[[ineqIdx]])
            termsLHS <- mapply(
                payoffFunction,
                rep(mIdx, numTerms),
                ineqmembersSingle$fctUpIdxs[[ineqIdx]],
                ineqmembersSingle$fctDnIdxs[[ineqIdx]],
                SIMPLIFY = TRUE)
            termsRHS <- mapply(
                payoffFunction,
                rep(mIdx, numTerms),
                ineqmembersSingle$cfcUpIdxs[[ineqIdx]],
                ineqmembersSingle$cfcDnIdxs[[ineqIdx]],
                SIMPLIFY = TRUE)
            return(sum(termsLHS) - sum(termsRHS))
        }
        ineqIdxs <- 1:ineqmembersSingle$numIneqs
        return(sapply(ineqIdxs, f))
    }
    mIdxs <- seq_along(ineqmembers)
    return(mapply(g, mIdxs, ineqmembers, SIMPLIFY = FALSE))
}

# The next functions convert between the old and the new representations.
# FIXME check if both work correctly. Use unit tests.

# TODO docs
ineqmembersOldToNew <- function(ineqmembersOld) {
    f <- function(mIdx) {
        ineqmembersSingle <- ineqmembersOld[[mIdx]]
        fct <- ineqmembersSingle[[1]]
        cfc <- ineqmembersSingle[[2]]
        numIneqs <- length(fct)
        ineqIdxs <- 1:numIneqs
        g <- function(ineqIdx) {
            fctUp <- unlist(lapply(fct[[ineqIdx]], '[[', 2))
            fctDn <- unlist(lapply(fct[[ineqIdx]], '[[', 3))
            cfcUp <- unlist(lapply(cfc[[ineqIdx]], '[[', 2))
            cfcDn <- unlist(lapply(cfc[[ineqIdx]], '[[', 3))
            return(list(fctUp, fctDn, cfcUp, cfcDn))
        }
        w <- transpose(lapply(ineqIdxs, g))
        names(w) <- c("fctUpIdxs", "fctDnIdxs", "cfcUpIdxs", "cfcDnIdxs")
        return(append(w, list(numIneqs = numIneqs)))
    }
    mIdxs <- seq_along(ineqmembersOld)
    return(lapply(mIdxs, f))
}

# This is currently correct up to reordering of the second part of the pair of
# each market in ineqmembersOld. The old implementation does not generate them
# in canonical order.
# TODO write function that compares new and old.
ineqmembersNewToOld <- function(ineqmembersNew) {
    f <- function(mIdx) {
        ineqmembersSingle <- ineqmembersNew[[mIdx]]
        ineqIdxs <- 1:ineqmembersSingle$numIneqs
        termsIdxs <- lapply(ineqIdxs, function (i) { getIneqTermsIdxs(ineqmembersSingle, i) })
        g <- function(termIdxArray) {
            # Prepend a column with the market index.
            lhsIdxs <- unname(cbind(mIdx, termIdxArray[, 1:2]))
            rhsIdxs <- unname(cbind(mIdx, termIdxArray[, 3:4]))
            lhsIdxs <- asplit(lhsIdxs, 1)
            rhsIdxs <- asplit(rhsIdxs, 1)
            # This removes the dim attribute.
            # Not strictly necessary, unless we want to compare with all.equal.
            lhsIdxs <- lapply(lhsIdxs, as.vector)
            rhsIdxs <- lapply(rhsIdxs, as.vector)
            return(list(lhsIdxs, rhsIdxs))
        }
        # transpose() needs purrr, but we might be able to remove the dependency
        # by hand-rolling it.
        return(transpose(lapply(termsIdxs, g)))
    }
    mIdxs <- seq_along(ineqmembersNew)
    return(lapply(mIdxs, f))
}

# getIneqTermsIdxs(ineqmembersSingle, ineqIdx) returns, for a given inequality,
# the 4-tuple of indices defining each term in that inequality.
#
# ineqmembersSingle is an element of the list constructed by Cineqmembers,
# corresponding to a single market.
# ineqIdx is the index of the inequality within that market.
#
# Returns an array of dimension (numTerms, 4), where numTerms is the number of
# terms the given inequality has. The columns of the array have names
# fctUpIdxs, fctDnIdxs, ,cfcUpIdxs, cfcDnIdxs. For information on those names,
# see the documentation of CineqmembersSingle.
getIneqTermsIdxs <- function(ineqmembersSingle, ineqIdx) {
    # [1:4] drops the $numIneqs member.
    return(do.call(cbind, lapply(ineqmembersSingle[1:4], "[[", ineqIdx)))
}

getAllIneqTermsIdxs <- function(ineqmembersSingle) {
    lapply(1:ineqmembersSingle$numIneqs,
           function(ineqIdx) { getIneqTermsIdxs(ineqmembersSingle, ineqIdx) })
}
