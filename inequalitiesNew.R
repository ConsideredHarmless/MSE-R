# CineqmembersNew(mate) computes the upstream and downstream indexes
# for each inequality term for all markets.
#
# For the structure mate, see the importNew function.
#
# Returns a list of 4-tuples containing indexes. For that structure, see
# CineqmembersSingle.
CineqmembersNew <- function(mate) {
    mateList <- lapply(result$mate, as.list)
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
CineqmembersSingle <- function(marketMates) {
    uIdxs <- marketMates$UpStream
    n <- length(uIdxs)
    # First, we create the transposition indexes {(i, j): 1 <= i < j <= n} as a
    # pair of vectors.
    iIdxs <- unlist(lapply(1:n, function (x) { rep(x, n-x) }))
    jIdxs <- unlist(lapply(1:n, function (x) { seq(from=x+1, to=n, length=max(0,n-x)) }))
    dnMates <- marketMates$DownMates # alias for brevity
    # To create the upstream indexes, we repeat them as many times as there are
    # corresponding downstream matches.
    repUp <- function(i, j) { rep_len(i, length(dnMates[[j]])) }
    fctUpIdxs <- mapply(function (i, j) { c(repUp(i, i),  repUp(j, j))  }, iIdxs, jIdxs)
    fctDnIdxs <- mapply(function (i, j) { c(dnMates[[i]], dnMates[[j]]) }, iIdxs, jIdxs)
    cfcUpIdxs <- mapply(function (i, j) { c(repUp(i, j),  repUp(j, i))  }, iIdxs, jIdxs)
    cfcDnIdxs <- mapply(function (i, j) { c(dnMates[[j]], dnMates[[i]]) }, iIdxs, jIdxs)
    return(list(
        fctUpIdxs = fctUpIdxs,
        fctDnIdxs = fctDnIdxs,
        cfcUpIdxs = cfcUpIdxs,
        cfcDnIdxs = cfcDnIdxs))
}

CinequalitiesNew <- function(f, ineqmembers) {} # TODO

# The next functions convert between the old and the new representations.

ineqmembersOldToNew <- function(ineqmembersOld) {} # TODO
ineqmembersNewToOld <- function(ineqmembersNew) {} # TODO
