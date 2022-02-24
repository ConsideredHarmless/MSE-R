# Declaring variables used in data.table code as NULL is necessary to suppress
# notes regarding the use of non-standard evaluation when running
# devtools::check().
# See:
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# https://github.com/Rdatatable/data.table/issues/850
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/

#' @import data.table
importCommon <- function(filename) {
    Market <- UpStream <- DownStream <- NULL
    # Importing data
    DT <- fread(filename, header=TRUE)
    # Adding keys for fast filtering
    setkey(DT, Market, UpStream, DownStream)
    # Header names
    header <- colnames(DT)
    # Calculating number of attributes
    distColIdxs <- which(sapply(
        colnames(DT),
        function(s) { startsWith(s, "Distance") }))
    noAttr <- length(distColIdxs)
    # Calculating number of markets
    marketIdxs = unique(DT, by = "Market")[[1]]
    checkIndexes(marketIdxs)
    noM <- length(marketIdxs)
    # Calculating number of up streams and down streams in each market
    upIdxs <- DT[, list(x = list(unique(UpStream))),   by = Market]$x
    dnIdxs <- DT[, list(x = list(unique(DownStream))), by = Market]$x
    lapply(upIdxs, checkIndexes)
    lapply(dnIdxs, checkIndexes)
    noU <- unlist(lapply(upIdxs, length))
    noD <- unlist(lapply(dnIdxs, length))
    return(list(
        DT=DT, header=header, noM=noM, noU=noU, noD=noD, noAttr=noAttr,
        marketIdxs=marketIdxs, distColIdxs=distColIdxs,
        upIdxs=upIdxs, dnIdxs=dnIdxs))
}

extractDistanceMatrices <- function(marketData) {
    # distanceMatrices is now a list of noM arrays (one for each market) of
    # dimension (noAttr, noD[mIdx], noU[mIdx]).
    # distanceMatrices[[mIdx]][i, dIdx, uIdx] gives the i-th attribute value for
    # the triple (mIdx, uIdx, dIdx).
    # The unexpected ordering of the dimensions is due to R using column-major
    # format for its arrays.
    # For old code, replace distanceMatrices[[m]][[u]][[i]][d] by
    # distanceMatrices[[m]][i, d, u].
    distanceMatrices <- lapply(marketData$marketIdxs, function(mIdx) {
        Market <- NULL
        # The unname is important!
        colSel <- unname(marketData$distColIdxs)
        distTable <- marketData$DT[Market == mIdx, colSel, with=FALSE]
        p <- marketData$noU[mIdx]
        q <- marketData$noD[mIdx]
        # The following is now a (p*q*noAttr)-length vector, but its values are
        # not naturally ordered, due to an unfortunate mix of row-major and
        # column-major indexing.
        temp <- unlist(distTable)
        # First we separate the attribute dimension from the (now collapsed)
        # upstream-downstream dimension.
        temp <- array(temp, c(p*q, marketData$noAttr))
        # We flip the dimensions.
        temp <- t(temp)
        # Now we can properly separate the collapsed dimensions.
        return(array(temp, c(marketData$noAttr, q, p)))
    })
    return(distanceMatrices)
}

extractMatchMatrices <- function(marketData) {
    # matchMatrices is now a list of noM arrays (one for each market) of
    # dimension (noD[mIdx], noU[mIdx]).
    # matchMatrices[[mIdx]][dIdx, uIdx] is 1 if the triple
    # (mIdx, uIdx, dIdx) matches and 0 otherwise.
    # See distanceMatrices for the construction of the object.
    matchMatrices <- lapply(marketData$marketIdxs, function(mIdx) {
        Market <- Match <- NULL
        matchTable <- marketData$DT[Market == mIdx, Match]
        p <- marketData$noU[mIdx]
        q <- marketData$noD[mIdx]
        return(array(unlist(matchTable), c(q, p)))
    })
    return(matchMatrices)
}

extractMate <- function(marketData) {
    # TODO since there are many structures with this name (see Cmate and
    # Cmates), maybe rename?
    # mate is now a list of noM data.table objects (one for each market). Each
    # object has noU[mIdx] rows (one for each upstream), with fields:
    #   $UpStream: the index of that upstream.
    #   $DownMates: a list of the downstream indexes which the upstream is
    # matched to.
    mate <- lapply(marketData$marketIdxs, function(mIdx) {
        Market <- UpStream <- DownStream <- Match <- NULL
        # The following table omits upstreams with no matching downstreams.
        marketMateTableCompressed <- marketData$DT[
            Market == mIdx & Match == 1,
            list(DownMates = list(DownStream)),
            keyby = UpStream]
        # The following table has a row for each unique upstream index.
        marketUpStreamTable <- marketData$DT[
            Market == mIdx, UpStream, keyby = UpStream][, "UpStream"]
        # Using an outer join, we restore any missing upstream indexes.
        marketMateTableRestored <- marketMateTableCompressed[
            marketUpStreamTable, on = list(UpStream)]
        return(marketMateTableRestored)
    })
    return(mate)
}

# TODO This is the same as extractMatchMatrices -- merge.
extractPayoffMatrices <- function(marketData) {
    # payoffMatrices is now a list of noM arrays (one for each market) of
    # dimension (noD[mIdx], noU[mIdx]).
    # payoffMatrices[[mIdx]][dIdx, uIdx] is the value of the payoff function
    # for that index triple.
    payoffMatrices <- lapply(marketData$marketIdxs, function(mIdx) {
        Market <- Payoff <- NULL
        payoffTable <- marketData$DT[Market == mIdx, Payoff]
        p <- marketData$noU[mIdx]
        q <- marketData$noD[mIdx]
        return(array(unlist(payoffTable), c(q, p)))
    })
    return(payoffMatrices)
}

# TODO
extractQuotas <- function(marketData) {}

import <- function(filename) {
    marketData <- importCommon(filename)
    distanceMatrices <- extractDistanceMatrices(marketData)
    matchMatrices <- extractMatchMatrices(marketData)
    mate <- extractMate(marketData)
    rest <- list(
        distanceMatrices=distanceMatrices,
        matchMatrices=matchMatrices,
        mate=mate)
    return(c(marketData, rest))
}

# For the inverse problem.
# TODO docs
importInv <- function(filename) {
    marketData <- importCommon(filename)
    payoffMatrices <- extractPayoffMatrices(marketData)
    # TODO
    quotas <- extractQuotas(marketData)
    rest <- list(payoffMatrices=payoffMatrices, quotas=quotas)
    return(c(marketData, rest))
}

checkIndexes <- function(idxs) {
    # Check if the index vector is not empty.
    if (length(idxs) == 0) {
        warning("index set empty")
    }
    # Check if indexes are consecutive (1, 2, ..., n).
    if (!isTRUE(all.equal(idxs, seq_along(idxs)))) {
        warning("indexes not consecutive")
    }
}
