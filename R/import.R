# Declaring variables used in data.table code as NULL is necessary to suppress
# notes regarding the use of non-standard evaluation when running
# devtools::check().
# See:
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# https://github.com/Rdatatable/data.table/issues/850
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/

#' @import data.table
#' @keywords internal
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
    checkIndices(marketIdxs)
    noM <- length(marketIdxs)
    # Calculating number of up streams and down streams in each market
    upIdxs <- DT[, list(x = list(unique(UpStream))),   by = Market]$x
    dnIdxs <- DT[, list(x = list(unique(DownStream))), by = Market]$x
    lapply(upIdxs, checkIndices)
    lapply(dnIdxs, checkIndices)
    noU <- unlist(lapply(upIdxs, length))
    noD <- unlist(lapply(dnIdxs, length))
    return(list(
        DT=DT, header=header, noM=noM, noU=noU, noD=noD, noAttr=noAttr,
        marketIdxs=marketIdxs, distColIdxs=distColIdxs,
        upIdxs=upIdxs, dnIdxs=dnIdxs))
}

#' dimorder dummy
#' @section Dimension ordering:
#' Note the unexpected ordering of the dimensions. We use this convention in
#' this package because R is a column-major language.
#' @name dimorder
NULL

#' Extract distance matrices from imported table
#'
#' @param marketData The return value of \code{importCommon}.
#'
#' @section Distance matrix structure:
#' Let \code{mIdx} index a market. Each \code{distanceMatrix} is an array
#' (technically not a matrix) of dimension \code{(noAttr, noD[mIdx],
#' noU[mIdx])}. The element indexed by \code{[i, dIdx, uIdx]} gives the
#' \code{i}-th distance attribute value for the triple \code{(mIdx, uIdx,
#' dIdx)}.
#'
#' @inheritSection dimorder Dimension ordering
#'
#' @section Old code:
#' To port code using the previous version, replace
#' \code{distanceMatrices[[m]][[u]][[i]][d]} by
#' \code{distanceMatrices[[m]][i, d, u]}.
#'
#' @return A list of distance matrices.
#' @keywords internal
extractDistanceMatrices <- function(marketData) {
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

#' Extract match matrices from imported table
#'
#' @param marketData The return value of \code{importCommon}.
#'
#' @section Match matrix structure:
#' Let \code{mIdx} index a market. Each \code{matchMatrix} is an array of
#' dimension \code{(noD[mIdx], noU[mIdx])}. The element indexed by
#' \code{[dIdx, uIdx]} is \code{1} if the triple \code{(mIdx, uIdx, dIdx)}
#' matches and \code{0} otherwise.
#'
#' @inheritSection dimorder Dimension ordering
#'
#' @section Old code:
#' To port code using the previous version, replace
#' \code{matchMatrix[[m]][[u]][d]} by \code{matchMatrices[[m]][d, u]}.
#'
#' @return A list of match matrices.
#' @keywords internal
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

#' Extract mate tables from imported table
#'
#' @param marketData The return value of \code{importCommon}.
#'
#' @section Mate table structure:
#' Let \code{mIdx} index a market. Each mate table is a \code{data.table} object
#' with fields \code{UpStream} and \code{DownMates}. Each upstream in the market
#' corresponds to a row, whose \code{DownMates} field contains a vector of the
#' downstream indices which the upstream is matched to. The rows are ordered
#' consecutively based on the upstream index \code{UpStream}.
#'
#' @return A list of mate tables.
#' @keywords internal
extractMate <- function(marketData) {
    # TODO since there are many structures with this name (see Cmate and
    # Cmates), maybe rename?
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
        # Using an outer join, we restore any missing upstream indices.
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

#' Import matched market data
#'
#' Reads a CSV file containing data for matched markets.
#'
#' @section File structure:
#'
#' The file must be a delimiter-separated file with a header. It must contain
#' the following fields:
#' \tabular{ll}{
#'   \code{Market} \tab The market index.\cr
#'   \code{UpStream} \tab The upstream index.\cr
#'   \code{DownStream} \tab The downstream index.\cr
#'   \code{Match} \tab \code{1} if this triple matches, \code{0} otherwise.
#' }
#' It must also contain at least one field with a name starting with
#' \code{Distance}. These fields containg distance attribute values. The order
#' they appear in, and not their full name, specifies their actual order.
#'
#' Indices should have consecutive values, starting from \code{1}. Distance
#' attribute values should be numerical.
#'
#' Each row should correspond to a unique triple of market, upstream, and
#' downstream indices.
#'
#' @inheritSection extractDistanceMatrices Distance matrix structure
#'
#' @inheritSection extractMatchMatrices Match matrix structure
#'
#' @inheritSection extractMate Mate table structure
#'
#' @param filename Absolute or relative path to the file. See also the
#'   parameters to \code{\link[data.table]{fread}}.
#'
#' @return A list with members:
#' \tabular{ll}{
#'   \code{$header}             \tab A character vector of the headers of the
#'     table.\cr
#'   \code{$noM}                \tab The number of markets.\cr
#'   \code{$noU}, \code{$noD}   \tab Vectors of size \code{$noM}, whose
#'     \code{m}-th element is the number of upstreams and downstreams
#'     respectively in the \code{m}-th market.\cr
#'   \code{$noAttr}             \tab The number of distance attributes.\cr
#'   \code{$distanceMatrices}   \tab A list of arrays of distance values, one
#'     for each market. See the appropriate section for their definition.\cr
#'   \code{$matchMatrices}      \tab A list of arrays of zeros or ones
#'     describing matches, one for each market. See the appropriate section for
#'     their definition.\cr
#'   \code{$mate}               \tab A list of data.table objects describing
#'   matches, one for each market. See the appropriate section for their
#'   definition.\cr
#' }
#' Members \code{$matchMatrices} and \code{$mate} provide the same information,
#' expressed in different ways.
#'
#' @export
importMatched <- function(filename) {
    marketData <- importCommon(filename)
    distanceMatrices <- extractDistanceMatrices(marketData)
    matchMatrices <- extractMatchMatrices(marketData)
    mate <- extractMate(marketData)
    rest <- list(
        distanceMatrices=distanceMatrices,
        matchMatrices=matchMatrices,
        mate=mate)
    return(c(marketData[2:6], rest))
}

# For the inverse problem.
# TODO docs
importUnmatched <- function(filename) {
    marketData <- importCommon(filename)
    payoffMatrices <- extractPayoffMatrices(marketData)
    # TODO
    quotas <- extractQuotas(marketData)
    rest <- list(payoffMatrices=payoffMatrices, quotas=quotas)
    return(c(marketData[2:6], rest))
}

checkIndices <- function(idxs) {
    # Check if the index vector is not empty.
    if (length(idxs) == 0) {
        warning("index set empty")
    }
    # Check if indices are consecutive (1, 2, ..., n).
    if (!isTRUE(all.equal(idxs, seq_along(idxs)))) {
        warning("indices not consecutive")
    }
}
