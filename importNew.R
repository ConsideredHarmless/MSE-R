importNew <- function(filename) {

    #Importing data
    DT <<- data.table::fread(filename, header=TRUE)

    #Adding keys for fast filtering
    setkey(DT,Market,UpStream,DownStream)

    #Header names
    header <<- colnames(DT)

    #Calculating number of attributes
    noAttr <<- length(DT)-3-1

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

    #Calculating number of markets
    marketIdxs = unique(DT, by = "Market")[[1]] # might be slightly faster
    # marketIdxs = unique(DT[[1]])
    checkIndexes(marketIdxs)
    noM <- length(marketIdxs)

    #Calculating number of up streams and down streams in each market
    upIdxs <- DT[, list(x = list(unique(UpStream))),   by = Market]$x
    dnIdxs <- DT[, list(x = list(unique(DownStream))), by = Market]$x
    lapply(upIdxs, checkIndexes)
    lapply(dnIdxs, checkIndexes)
    noU <- unlist(lapply(upIdxs, length))
    noD <- unlist(lapply(dnIdxs, length))

    #Calculating distance matrices
    # distanceMatrices is now a list of noM arrays (one for each market) of
    # dimension (noAttr, noD[mIdx], noU[mIdx]).
    # distanceMatrices[[m]][i, d, u] gives the i-th attribute value for the
    # triple (m, u, d).
    # The unexpected ordering of the dimensions is due to R using column-major
    # format for its arrays.
    # For old code, replace distanceMatrices[[m]][[u]][[i]][d] by
    # distanceMatrices[[m]][i, d, u].
    distanceMatrices <- lapply(marketIdxs, function(mIdx) {
        distTable <- DT[Market == mIdx, (3+1):(3+noAttr)]
        p <- noU[mIdx]
        q <- noD[mIdx]
        # The following is now a (p*q*noAttr)-length vector, but its values are
        # not naturally ordered, due to an unfortunate mix of row-major and
        # column-major indexing.
        temp <- unlist(distTable)
        # First we separate the attribute dimension from the (now collapsed)
        # upstream-downstream dimension.
        temp <- array(temp, c(p*q, noAttr))
        # We flip the dimensions.
        temp <- t(temp)
        # Now we can properly separate the collapsed dimensions.
        return(array(temp, c(noAttr, q, p)))
    })

    # TODO maybe rename (plural)?
    #Calculating matchMatrix
    # matchMatrix is now a list of noM arrays (one for each market) of dimension
    # (noD[mIdx], noU[mIdx]).
    # matchMatrix[[m]][d, u] is 1 if the triple (m, u, d) matches and 0
    # otherwise.
    # See distanceMatrices for the construction of the object.
    matchMatrix <- lapply(marketIdxs, function(mIdx) {
        matchTable <- DT[Market == mIdx, Match]
        p <- noU[mIdx]
        q <- noD[mIdx]
        return(array(unlist(matchTable), c(q, p)))
    })

    # TODO since there are many structures with this name (see Cmate and
    # Cmates), maybe rename?
    #Calculating mate
    # mate is now a list of noM data.table objects (one for each market). Each
    # object has noU[mIdx] rows (one for each upstream), with fields:
    #   $UpStream: the index of that upstream.
    #   $DownMates: a list of the downstream indexes which the upstream is
    # matched to.
    mate <- lapply(marketIdxs, function(mIdx) {
        # The following table omits upstreams with no matching downstreams.
        marketMateTableCompressed <- DT[
            Market == mIdx & Match == 1,
            list(DownMates = list(DownStream)),
            keyby = UpStream]
        # The following table has a row for each unique upstream index.
        marketUpStreamTable <- DT[Market == mIdx, UpStream, keyby = UpStream][, "UpStream"]
        # Using an outer join, we restore any missing upstream indexes.
        marketMateTableRestored <- marketMateTableCompressed[marketUpStreamTable, on = list(UpStream)]
        return(marketMateTableRestored)
    })

    return(list(
        "header"=header, "noM"=noM, "noU"=noU, "noD"=noD,"noAttr"=noAttr,
        "distanceMatrices"=distanceMatrices, "matchMatrix"=matchMatrix,
        "mate"=mate))
}
