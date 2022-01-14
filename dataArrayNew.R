# TODO docs
# old:
#  dataArray[[ineqIdx]]
#  dataArray[[ineqIdx]][paramIdx]
# new:
#  dataArray[, ineqIdx]
#  dataArray[paramIdx, ineqIdx]
CdataArrayNew <- function(distanceMatrices, ineqmembers) {
    mIdxs <- seq_along(ineqmembers)
    f <- function(mIdx) {
        ineqmembersSingle <- ineqmembers[[mIdx]]
        fctUpIdxsAll <- ineqmembersSingle$fctUpIdxs
        fctDnIdxsAll <- ineqmembersSingle$fctDnIdxs
        cfcUpIdxsAll <- ineqmembersSingle$cfcUpIdxs
        cfcDnIdxsAll <- ineqmembersSingle$cfcDnIdxs
        ineqIdxs <- 1:ineqmembersSingle$numIneqs
        g <- function(ineqIdx) {
            fctUpIdxs <- fctUpIdxsAll[[ineqIdx]]
            fctDnIdxs <- fctDnIdxsAll[[ineqIdx]]
            cfcUpIdxs <- cfcUpIdxsAll[[ineqIdx]]
            cfcDnIdxs <- cfcDnIdxsAll[[ineqIdx]]
            termIdxs <- seq_along(fctUpIdxs)
            h <- function(termIdx) {
                mIdxLHS <- mIdx
                mIdxRHS <- mIdx
                uIdxLHS <- fctUpIdxs[termIdx]
                dIdxLHS <- fctDnIdxs[termIdx]
                uIdxRHS <- cfcUpIdxs[termIdx]
                dIdxRHS <- cfcDnIdxs[termIdx]
                valLHS <- distanceMatrices[[mIdxLHS]][, dIdxLHS, uIdxLHS]
                valRHS <- distanceMatrices[[mIdxRHS]][, dIdxRHS, uIdxRHS]
                return(valLHS-valRHS)
            }
            # TODO Since this only needs distanceMatrices, we can move this up
            # in scope.
            hNew <- function(uIdxLHS, dIdxLHS, uIdxRHS, dIdxRHS) {
                valLHS <- distanceMatrices[[mIdx]][, dIdxLHS, uIdxLHS]
                valRHS <- distanceMatrices[[mIdx]][, dIdxRHS, uIdxRHS]
                return(valLHS-valRHS)
            }
            return(rowSums(mapply(hNew, fctUpIdxs, fctDnIdxs, cfcUpIdxs, cfcDnIdxs)))
            # TODO is it worth replacing this with
            #   mapply(..., fctUpIdxs, ...) ?
            # return(rowSums(sapply(termIdxs, h)))
        }
        return(sapply(ineqIdxs, g))
    }
    # https://stackoverflow.com/q/3414078
    return(do.call(cbind, lapply(mIdxs, f)))
}
