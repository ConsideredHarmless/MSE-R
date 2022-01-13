import2 <- function(filename) {

    #Importing data
    DT <<- data.table::fread(filename, header=TRUE)

    #Adding keys for fast filtering
    setkey(DT,Market,UpStream,DownStream)

    #Header names
    header <<- colnames(DT)

    #Calculating number of attributes
    noAttr <<- length(DT)-3-1

    checkConsecutive <- function(v) {
    # Check if indexes are consecutive (1, 2, ..., n).
        if (!isTRUE(all.equal(v, seq_along(v)))) {
            warning("indexes not consecutive")
        }
    }

    #Calculating number of markets
    marketIdxs = unique(DT, by = "Market")[[1]] # might be slightly faster
    # marketIdxs = unique(DT[[1]])
    checkConsecutive(marketIdxs)
    noM <- length(marketIdxs)

    #Calculating number of up streams and down streams in each market
    upIdxs <- DT[, list(x = list(unique(UpStream))),   by = Market]$x
    dnIdxs <- DT[, list(x = list(unique(DownStream))), by = Market]$x
    lapply(upIdxs, checkConsecutive)
    lapply(dnIdxs, checkConsecutive)
    noU <- unlist(lapply(upIdxs, length))
    noD <- unlist(lapply(dnIdxs, length))

    #Calculating distance matrices
    # distanceMatrices is now a list of noM arrays (one for each market) of
    # dimension (noAttr, noD[midx], noU[midx]).
    # distanceMatrices[[m]][i, d, u] gives the i-th attribute value for the
    # triple (m, u, d).
    # The unexpected ordering of the dimensions is due to R using column-major
    # format for its arrays.
    # For old code, replace distanceMatrices[[m]][[u]][[i]][d] by
    # distanceMatrices[[m]][i, d, u].
    distanceMatrices <- lapply(marketIdxs, function(midx) {
        distTable <- DT[Market == midx, (3+1):(3+noAttr)]
        p <- noU[midx]
        q <- noD[midx]
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

    #Calculating matchMatrix
    # matchMatrix is now a list of noM arrays (one for each market) of dimension
    # (noD[midx], noU[midx]).
    # matchMatrix[[m]][d, u] is 1 if the triple (m, u, d) matches and 0
    # otherwise.
    # See distanceMatrices for the construction of the object.
    matchMatrix <- lapply(marketIdxs, function(midx) {
        matchTable <- DT[Market == midx, Match]
        p <- noU[midx]
        q <- noD[midx]
        return(array(unlist(matchTable), c(q, p)))
    })

    #Calculating mate
    # mate is now a list of noM data.table objects (one for each market). Each
    # object has noU[midx] rows (one for each upstream), with fields:
    #   $UpStream: the index of that upstream.
    #   $DownMates: a list of the downstream indexes which the upstream is
    # matched to.
    mate <- lapply(marketIdxs, function(midx) {
        DT[
            Market == midx & Match == 1,
            list(DownMates = list(DownStream)),
            keyby = UpStream] })

    return(list(
        "header"=header, "noM"=noM, "noU"=noU, "noD"=noD,"noAttr"=noAttr,
        "distanceMatrices"=distanceMatrices, "matchMatrix"=matchMatrix,
        "mate"=mate))
}
