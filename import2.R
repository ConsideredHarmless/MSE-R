import2<-function(filename){

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
	marketIdxs = unique(DT, keyby = Market)[[1]] # might be slightly faster
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

  # FIXME tabs/spaces

	# FIXME
	# The following assumes that all markets have the same number of upstreams and
	# downstreams. If this is not the case, then the last dimension of the arrays
	# distanceMatrices and matchMatrix should be converted to a list.

	#Calculating distance matrices
	# distanceMatrices is now an array of dimension (noAttr, noD, noU, noM).
	# distanceMatrices[i, d, u, m] gives the i-th attribute value for the triple
	# (m, u, d).
	# The unexpected ordering of the dimensions is due to R using column-major
	# format for its arrays.
	# For old code, replace distanceMatrices[[m]][[u]][[i]][d] by
	# distanceMatrices[i, d, u, m].
	distTable <- DT[, (3+1):(3+noAttr)]
	# The following is now a (noM*noU*noD*noAttr)-length vector, but its values
	# are not naturally ordered, due to an unfortunate mix of row-major and
	# column-major indexing.
	temp <- unlist(distTable)
	# First we separate the attribute dimension from the (now collapsed)
	# market-upstream-downstream dimension.
	temp <- array(temp, c(noM*noU*noD, noAttr))
	# We flip the dimensions.
	temp <- t(temp)
	# Now we can properly separate the collapsed dimensions.
	distanceMatrices <- array(temp, c(noAttr, noD, noU, noM))

	#Calculating matchMatrix
	# matchMatrix is now an array of dimension (noD, noU, noM).
	# matchMatrix[d, u, m] is 1 if the triple (m, u, d) matches and 0 otherwise.
	# See above for the construction of the array.
	matchTable <- DT[, Match]
	matchMatrix <- array(unlist(matchTable), c(noD, noU, noM))

	#Calculating mate
	# TODO recheck and document
	mate <- lapply(marketIdxs, function(midx) {
	  DT[
	    Market == midx & Match == 1,
	    list(DownMates = list(DownStream)),
	    keyby = UpStream] })

	return(list(
	  "header"=header, "noM"=noM, "noU"=noU, "noD"=noD,"noAttr"=noAttr,
	  "distanceMatrices"=distanceMatrices, "matchMatrix"=matchMatrix, "mate"=mate))
}
