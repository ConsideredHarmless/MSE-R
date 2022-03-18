# Declaring variables used in data.table code as NULL is necessary to suppress
# notes regarding the use of non-standard evaluation when running
# devtools::check().
# See:
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# https://github.com/Rdatatable/data.table/issues/850
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/

#' @import data.table
#' @keywords internal
importMatchedMain <- function(filename, fieldMode) {
    Market <- UpStream <- DownStream <- NULL
    DT <- fread(filename, header=TRUE)
    header <- colnames(DT)
    colIdxs <- checkHeaderMatched(header, fieldMode)
    if (fieldMode == "position") {
        newColNames <- c(
            "Market", "UpStream", "DownStream",
            sapply(
                1:length(colIdxs$distanceColIdxs),
                function(i) { sprintf("Distance%d", i)} ),
            "Match")
        setnames(DT, newColNames)
    }
    # Adding keys for fast filtering
    setkey(DT, Market, UpStream, DownStream)
    # Calculating number of attributes
    noAttr <- length(colIdxs$distanceColIdxs)
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
        colIdxs=colIdxs, marketIdxs=marketIdxs, upIdxs=upIdxs, dnIdxs=dnIdxs))
}

#' @import data.table
#' @keywords internal
importUnmatchedMain <- function(filename, fieldMode, filetype) {
    Market <- Stream <- NULL
    DT <- fread(filename, header=TRUE)
    header <- colnames(DT)
    colIdxs <- checkHeaderUnmatched(header, fieldMode, filetype)
    if (fieldMode == "position") {
        newColNames <- c(
            "Market", "Stream",
            sapply(
                1:length(colIdxs$attributeColIdxs),
                function(i) { sprintf("Attribute%d", i)} ),
            "Quota")
        setnames(DT, newColNames)
    } else if (fieldMode == "name") {
        setnames(DT, colIdxs$streamColIdx, "Stream")
    }
    # Adding keys for fast filtering
    setkey(DT, Market, Stream)
    # Calculating number of attributes
    noAttr <- length(colIdxs$attributeColIdxs)
    # Calculating number of markets
    marketIdxs = unique(DT, by = "Market")[[1]]
    checkIndices(marketIdxs)
    noM <- length(marketIdxs)
    # Calculating number of up streams and down streams in each market
    streamIdxs <- DT[, list(x = list(unique(Stream))), by = Market]$x
    lapply(streamIdxs, checkIndices)
    noS <- unlist(lapply(streamIdxs, length))
    return(list(
        DT=DT, header=header, noM=noM, noS=noS, noAttr=noAttr,
        colIdxs=colIdxs, marketIdxs=marketIdxs, streamIdxs=streamIdxs))
}

#' dimorder dummy
#' @section Dimension ordering:
#' Note the unexpected ordering of the dimensions. We use this convention in
#' this package because R is a column-major language.
#' @name dimorder
NULL

#' Extract distance matrices from imported table
#'
#' @param marketData The return value of \code{importMatchedMain}.
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
        colSel <- unname(marketData$colIdxs$distanceColIdxs)
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
#' @param marketData The return value of \code{importMatchedMain}.
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
#' @param marketData The return value of \code{importMatchedMain}.
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

#' Extract attributed matrices from imported table
#'
#' @param marketData The return value of \code{importUnmatchedMain}.
#'
#' @section Attribute matrix structure:
#' Let \code{mIdx} index a market. Each \code{attributeMatrix} is an array
#' of dimension \code{(noAttr, noS[mIdx])}, where \code{noS[mIdx]} is the size
#' of the stream (up- or down-). The element indexed by \code{[i, sIdx]} gives
#' the \code{i}-th attribute value for the pair \code{(mIdx, sIdx)}.
#'
#' @inheritSection dimorder Dimension ordering
#'
#' @return A list of attribute matrices.
#' @keywords internal
extractAttributeMatrices <- function(marketData) {
    attributeMatrices <- lapply(marketData$marketIdxs, function(mIdx) {
        Market <- NULL
        # The unname is important!
        colSel <- unname(marketData$colIdxs$attributeColIdxs)
        attrTable <- marketData$DT[Market == mIdx, colSel, with=FALSE]
        p <- marketData$noS[mIdx]
        q <- marketData$noAttr
        arr <- array(unlist(attrTable), c(p, q))
        return(t(arr))
    })
    return(attributeMatrices)
}

#' Extract quota vectors from imported table
#'
#' @param marketData The return value of \code{importUnmatchedMain}.
#'
#' @return A list of quota vectors, on for each market.
#' @keywords internal
extractQuotas <- function(marketData) {
    quotas <- lapply(marketData$marketIdxs, function(mIdx) {
        Market <- Quota <- NULL
        return(marketData$DT[Market == mIdx, Quota])
    })
    return(quotas)
}

#' Import matched market data
#'
#' Reads a CSV file containing data for matched markets.
#'
#' @section File structure:
#'
#' The file must be a delimiter-separated file with a header.
#'
#' If \code{fieldMode} is \code{"name"}, the file must contain the following
#' fields:
#' \tabular{ll}{
#'   \code{Market} \tab The market index.\cr
#'   \code{UpStream} \tab The upstream index.\cr
#'   \code{DownStream} \tab The downstream index.\cr
#'   \code{Match} \tab \code{1} if this triple matches, \code{0} otherwise.
#' }
#' It must also contain at least one field with a name starting with
#' \code{Distance}. These fields contain attribute values. The order
#' they appear in, and not their full name, specifies their actual order.
#'
#' If \code{fieldMode} is \code{"position"}, then the fields mentioned above
#' are identified by the order they appear in the header, and not their names.
#' The first three fields correspond to \code{Market}, \code{UpStream}, and
#' \code{DownStream} respectively, and the last field corresponds to
#' \code{Match}. All other fields are considered to be distance attribute
#' fields.
#'
#' Indices should have consecutive values, starting from \code{1}. Distance
#' attribute values should be numerical.
#'
#' Each row should correspond to a unique pair of market and upstream or
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
#' @param fieldMode A string denoting how to identify the fields of the file.
#'   Options are:
#'   \tabular{ll}{
#'     \code{"position"} \tab (default) Identify each field based on its order
#'       in the field list. \cr
#'     \code{"name"} \tab Identify each field based on its name.
#'   }
#' See the section "File structure" for more information.
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
importMatched <- function(filename, fieldMode = "position") {
    stopifnot(fieldMode %in% c("position", "name"))
    marketData <- importMatchedMain(filename, fieldMode)
    distanceMatrices <- extractDistanceMatrices(marketData)
    matchMatrices <- extractMatchMatrices(marketData)
    mate <- extractMate(marketData)
    rest <- list(
        distanceMatrices=distanceMatrices,
        matchMatrices=matchMatrices,
        mate=mate)
    return(c(marketData[2:6], rest))
}

#' Import unmatched market data
#'
#' Reads CSV files containing data for unmatched markets.
#'
#' @section File structure:
#'
#' Each file must be a delimiter-separated file with a header.
#'
#' If \code{fieldMode} is \code{"name"}, the file must contain the following
#' fields:
#' \tabular{ll}{
#'   \code{Market} \tab The market index.\cr
#'   \code{UpStream} or \code{DownStream} \tab The stream index.\cr
#'   \code{Quota} \tab The quota for that stream.
#' }
#' It must also contain at least one field with a name starting with
#' \code{Attribute}. These fields contain attribute values. The order
#' they appear in, and not their full name, specifies their actual order.
#'
#' If \code{fieldMode} is \code{"position"}, then the fields mentioned above
#' are identified by the order they appear in the header, and not their names.
#' The first two fields correspond to \code{Market} and \code{UpStream} or
#' \code{DownStream} respectively, and the last field corresponds to
#' \code{Quota}. All other fields are considered to be attribute fields.
#'
#' Indices should have consecutive values, starting from \code{1}. Distance
#' attribute values should be numerical.
#'
#' Each row should correspond to a unique triple of market, upstream, and
#' downstream indices.
#'
#' @inheritSection extractAttributeMatrices Attribute matrix structure
#'
#' @param filenameUp,filenameDn Absolute or relative path to the files for the
#'   upstream and the downstream data respectively. See also the parameters to
#'   \code{\link[data.table]{fread}}.
#' @inheritParams importMatched
#'
#' @return A list with members:
#' \tabular{ll}{
#'   \code{$headerUp}, \code{$headerDn} \tab Character vectors of the headers
#'     of the tables. \cr
#'   \code{$noM}                \tab The number of markets.\cr
#'   \code{$noU}, \code{$noD}   \tab Vectors of size \code{$noM}, whose
#'     \code{m}-th element is the number of upstreams and downstreams
#'     respectively in the \code{m}-th market.\cr
#'   \code{$noAttr}             \tab The number of distance attributes.\cr
#'   \code{$attributeMatricesUp}, \code{$attributeMatricesDn}
#'                              \tab A list of arrays of upstream and downstream
#'     attribute values respectively, one for each market. See the appropriate
#'     section for their definition.\cr
#'   \code{$quotasUp}, \code{$quotasDn}
#'                              \tab A list of vectors of quotas for the
#'     upstreams and downstreams respectively.\cr
#' }
#'
#' @export
importUnmatched <- function(filenameUp, filenameDn, fieldMode = "position") {
    marketDataUp <- importUnmatchedMain(filenameUp, fieldMode, "up")
    marketDataDn <- importUnmatchedMain(filenameDn, fieldMode, "dn")
    stopifnot(
        marketDataUp$noM    == marketDataDn$noM,
        marketDataUp$noAttr == marketDataDn$noAttr)
    attributeMatricesUp <- extractAttributeMatrices(marketDataUp)
    attributeMatricesDn <- extractAttributeMatrices(marketDataDn)
    quotasUp <- extractQuotas(marketDataUp)
    quotasDn <- extractQuotas(marketDataDn)
    result <- list(
        headerUp = marketDataUp$header,
        headerDn = marketDataDn$header,
        noM = marketDataUp$noM,
        noU = marketDataUp$noS,
        noD = marketDataDn$noS,
        noAttr = marketDataUp$noAttr,
        attributeMatricesUp = attributeMatricesUp,
        attributeMatricesDn = attributeMatricesDn,
        quotasUp = quotasUp,
        quotasDn = quotasDn
    )
    return(result)
}

#' Check if indices are valid
#'
#' Checks if the given vector of index values is non-empty and consecutive,
#' with values starting from \code{1}. Raises a warning otherwise.
#'
#' @param idxs A vector of index values to be checked.
#'
#' @keywords internal
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

#' Check if header is valid
#'
#' Checks if the given vector of field names satisfies the conditions stated in
#' \code{importMatched}, stopping execution if they are not met.
#'
#' @param header A vector of field names to be checked.
#' @inheritParams importMatched
#'
#' @keywords internal
checkHeaderMatched <- function(header, fieldMode) {
    n = length(header)
    if (fieldMode == "position") {
        # Three fields for market/upstream/downstream, at least one distance
        # column, and a match column.
        stopifnot(n >= 5)
        marketColIdx     <- 1
        upstreamColIdx   <- 2
        downstreamColIdx <- 3
        distanceColIdxs  <- 4:(n-1)
        matchColIdx      <- n
    } else if (fieldMode == "name") {
        marketPositions     <- which(header == "Market")
        upstreamPositions   <- which(header == "UpStream")
        downstreamPositions <- which(header == "DownStream")
        matchPositions      <- which(header == "Match")
        distancePositions   <- which(sapply(header,
                                            function(s) { startsWith(s, "Distance") }))
        stopifnot(
            length(marketPositions)     == 1,
            length(upstreamPositions)   == 1,
            length(downstreamPositions) == 1,
            length(matchPositions)      == 1,
            length(distancePositions)   == n - 4
        )
        marketColIdx     <- marketPositions
        upstreamColIdx   <- upstreamPositions
        downstreamColIdx <- downstreamPositions
        distanceColIdxs  <- distancePositions
        matchColIdx      <- matchPositions
    }
    return(list(
        marketColIdx     = marketColIdx,
        upstreamColIdx   = upstreamColIdx,
        downstreamColIdx = downstreamColIdx,
        distanceColIdxs  = distanceColIdxs,
        matchColIdx      = matchColIdx))
}

#' Check if header is valid
#'
#' Checks if the given vector of field names satisfies the conditions stated in
#' \code{importUnmatched}, stopping execution if they are not met.
#'
#' @inheritParams importUnmatched
#' @inheritParams checkHeaderUnmatched
#'
#' @keywords internal
checkHeaderUnmatched <- function(header, fieldMode, filetype) {
    n = length(header)
    if (fieldMode == "position") {
        # Two fields for market/stream, at least one attribute
        # column, and a quota column.
        stopifnot(n >= 4)
        marketColIdx     <- 1
        streamColIdx     <- 2
        attributeColIdxs <- 3:(n-1)
        quotaColIdx      <- n
    } else if (fieldMode == "name") {
        streamColName = switch(filetype, "up" = "UpStream", "dn" = "DownStream")
        marketPositions     <- which(header == "Market")
        streamPositions     <- which(header == streamColName)
        quotaPositions      <- which(header == "Quota")
        attributePositions  <- which(sapply(header,
                                            function(s) { startsWith(s, "Attribute") }))
        stopifnot(
            length(marketPositions)     == 1,
            length(streamPositions)     == 1,
            length(quotaPositions)      == 1,
            length(attributePositions)  == n - 3
        )
        marketColIdx     <- marketPositions
        streamColIdx     <- streamPositions
        attributeColIdxs <- attributePositions
        quotaColIdx      <- quotaPositions
    }
    return(list(
        marketColIdx     = marketColIdx,
        streamColIdx     = streamColIdx,
        attributeColIdxs = attributeColIdxs,
        quotaColIdx      = quotaColIdx))
}

