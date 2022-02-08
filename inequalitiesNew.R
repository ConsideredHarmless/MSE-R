# CineqmembersNew(mate) computes the upstream and downstream indexes
# for each inequality term for all markets.
#
# For the structure mate, see the importNew function.
#
# Returns a list of 4-tuples containing indexes. For that structure, see
# CineqmembersSingle.
CineqmembersNew <- function(mate) {
    mateList <- lapply(mate, as.list)
    return(lapply(mateList, CineqmembersSingle))
}

# CineqmembersSingle(marketMates) computes the upstream and downstream indexes
# for each inequality term for a single market.
#
# The structure marketMates is a list with members:
#   $UpStream: a vector of the unique indexes of upstreams for this market.
#   $DownMates: a list with n elements, where n is the number of upstreams.
#     Element $DownMates[[i]] is a vector of the downstream indexes which the
#     upstream with index i is matched to.
#
# Each transposition (i, j) of the set {1, 2, ..., n} yields a single
# inequality, with the factual terms in the LHS and the counterfactual terms in
# the RHS. The factual terms correspond to all current matches of both upstream
# indexes (i and j), and the counterfactual terms correspond to the matches
# where i and j switch partners.
#
# Instead of returning a complicated nested structure, we return a 4-tuple of
# lists, each of the same length (one element for each inequality). Every
# element corresponding to the same inequality is a vector of indexes (one index
# for each term in the inequality).
#
# Returns:
# A list with members:
#   $fctUpIdxs: a list of upstream index vectors for the factual case.
#   $fctDnIdxs: a list of downstream index vectors for the factual case.
#   $cfcUpIdxs: a list of upstream index vectors for the counterfactual case.
#   $cfcDnIdxs: a list of downstream index vectors for the counterfactual case.
#   $numIneqs: the number of inequalities for this market. Also the common
#     length of the above lists.
CineqmembersSingle <- function(marketMates) {
    uIdxs <- marketMates$UpStream
    n <- length(uIdxs)
    numIneqs = n*(n-1)/2
    # First, we create the transposition indexes {(i, j): 1 <= i < j <= n} as a
    # pair of vectors.
    iIdxs <- unlist(lapply(1:n, function (x) { rep(x, n-x) }))
    jIdxs <- unlist(lapply(1:n, function (x) { seq(from=x+1, to=n, length=max(0,n-x)) }))
    dnMates <- marketMates$DownMates # alias for brevity
    # To create the upstream indexes, we repeat them as many times as there are
    # corresponding downstream matches.
    repUp <- function(i, j) { rep_len(i, length(dnMates[[j]])) }
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

# CinequalitiesNew(payoffFunction, ineqmembers) computes, for each inequality,
# the value
#   (sum of payoff function values over LHS terms) -
#   (sum of payoff function values over RHS terms).
# payoffFunction is a function which takes three arguments (mIdx, uIdx, dIdx)
# and returns the value of the payoff function for that triple.
# For the structure ineqmembers, see the function CineqmembersNew.
#
# Returns a list of vectors, one for each market. Each vector element
# corresponds to a single inequality.
CinequalitiesNew <- function(payoffFunction, ineqmembers) {
    g <- function(mIdx, ineqmembersSingle) {
        f <- function(ineqIdx) {
            numTerms <- length(ineqmembersSingle$fctUpIdxs[[ineqIdx]])
            termsLHS <- mapply(
                payoffFunction,
                rep(mIdx, numTerms),
                ineqmembersSingle$fctUpIdxs[[ineqIdx]],
                ineqmembersSingle$fctDnIdxs[[ineqIdx]],
                SIMPLIFY = FALSE)
            termsRHS <- mapply(
                payoffFunction,
                rep(mIdx, numTerms),
                ineqmembersSingle$cfcUpIdxs[[ineqIdx]],
                ineqmembersSingle$cfcDnIdxs[[ineqIdx]],
                SIMPLIFY = FALSE)
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
    mIdxs <- seq_along(ineqmembers)
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
    mIdxs <- seq_along(ineqmembers)
    return(lapply(mIdxs, f))
}

# getIneqTermsIdxs(ineqmembersSingle, ineqIdx) returns, for a given inequality,
# the 4-tuple of indexes defining each term in that inequality.
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
