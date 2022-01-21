# CdataArrayNew(distanceMatrices, ineqmembers) computes the data array. For the
# data array, see section 5.2 in [1]. For the distanceMatrices structure, see
# the importNew function. For the ineqmembers structure, see the CineqmembersNew
# function.
#
# Returns an array of dimension (noAttr, sumNumIneqs), where sumNumIneqs is the
# sum over all markets of the number of inequalities of each market.
#
# For old code, replace
#    dataArray[[ineqIdx]]
#    dataArray[[ineqIdx]][paramIdx]
# by
#    dataArray[, ineqIdx]
#    dataArray[paramIdx, ineqIdx]
# respectively.
#
# [1] David Santiago and Fox, Jeremy (2009). "A Toolkit for Matching Maximum Score Estimation and Point and Set Identified Subsampling Inference".
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
            return(rowSums(mapply(h, fctUpIdxs, fctDnIdxs, cfcUpIdxs, cfcDnIdxs, SIMPLIFY = FALSE)))
        }
        return(sapply(ineqIdxs, g))
    }
    return(do.call(cbind, lapply(mIdxs, f)))
}
