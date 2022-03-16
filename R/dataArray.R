#' Compute data array
#'
#' Computes the data array, as defined in section 5.2 of \[1\].
#'
#' Each column of the data array corresponds to a single inequality. To compute
#' its value, we gather the index triples (for market, upstream, and downstream)
#' appearing on either side of the inequality, and we generate the corresponding
#' term (a column vector of distance attribute values) from the distance
#' matrices. The difference between the sum of the LHS and RHS terms is equal to
#' the desired column.
#'
#' @inheritSection extractDistanceMatrices Distance matrix structure
#'
#' @inheritSection CineqmembersSingle Inequality members structure
#'
#' @section Old code:
#' To port code using the previous version, replace \code{dataArray[[ineqIdx]]}
#' and \code{dataArray[[ineqIdx]][paramIdx]} by \code{dataArray[, ineqIdx]} and
#' \code{dataArray[paramIdx, ineqIdx]} respectively.
#'
#' @section References:
#' \tabular{ll}{
#'   \[1\] \tab David Santiago and Fox, Jeremy (2009). "A Toolkit for Matching
#'   Maximum Score Estimation and Point and Set Identified Subsampling
#'   Inference".
#' }
#'
#' @param distanceMatrices A list of arrays of distance values, one
#'   for each market. See the appropriate section for their definition.
#' @param ineqmembers A list of objects describing the indices participating in
#'   the inequalities occurring in the objective function. See the appropriate
#'   section for their definition.
#'
#' @return An array of dimension \code{(noAttr, sumNumIneqs)}, where
#'   \code{noAttr} is the number of distance attributes and \code{sumNumIneqs}
#'   is the sum of the number of inequalities of each market over all markets.
#'   \code{[paramIdx, ineqIdx]} indexes the \code{paramIdx}-th attribute for the
#'   \code{ineqIdx}-th inequality.
#' @export
CdataArray <- function(distanceMatrices, ineqmembers) {
    mIdxs <- seq_along(ineqmembers)
    f <- function(mIdx) {
        ineqmembersSingle <- ineqmembers[[mIdx]]
        ineqIdxs <- 1:ineqmembersSingle$numIneqs
        noAttr <- dim(distanceMatrices[[mIdx]])[1]
        zeroCol <- rep(0, noAttr)
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
            # The following might be empty. In that case mapply returns an empty
            # list instead of an empty vector/array, so we have to special-case
            # it.
            termValues <- mapply(h, fctUpIdxs, fctDnIdxs, cfcUpIdxs, cfcDnIdxs, SIMPLIFY = TRUE)
            if (length(termValues) == 0) {
                result <- zeroCol
            } else {
                result <- rowSums(termValues)
            }
            return(result)
        }
        return(sapply(ineqIdxs, g))
    }
    return(do.call(cbind, lapply(mIdxs, f)))
}
