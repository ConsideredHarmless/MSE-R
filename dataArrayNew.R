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
        ineqIdxs <- 1:ineqmembersSingle$numIneqs
        h <- function(uIdxLHS, dIdxLHS, uIdxRHS, dIdxRHS) {
            valLHS <- distanceMatrices[[mIdx]][, dIdxLHS, uIdxLHS]
            valRHS <- distanceMatrices[[mIdx]][, dIdxRHS, uIdxRHS]
            return(valLHS-valRHS)
        }
        g <- function(ineqIdx) {
            fctUpIdxs <- ineqmembersSingle$fctUpIdxs[[ineqIdx]]
            fctDnIdxs <- ineqmembersSingle$fctDnIdxs[[ineqIdx]]
            cfcUpIdxs <- ineqmembersSingle$cfcUpIdxs[[ineqIdx]]
            cfcDnIdxs <- ineqmembersSingle$cfcDnIdxs[[ineqIdx]]
            termIdxs <- seq_along(fctUpIdxs)
            return(rowSums(mapply(h, fctUpIdxs, fctDnIdxs, cfcUpIdxs, cfcDnIdxs)))
        }
        return(sapply(ineqIdxs, g))
    }
    return(do.call(cbind, lapply(mIdxs, f)))
}
