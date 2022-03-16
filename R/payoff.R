#' Evaluate payoff matrix
#'
#' Evaluates the payoff functions for the given parameter values in a single
#' market.
#'
#' @inheritSection evaluatePayoffMatrices Payoff matrices
#'
#' @param unevalPayoffMatrix An array of dimension \code{(noAttr, noD, noU)}.
#'   Can be an element of the list \code{distanceMatrices}, as defined in the
#'   function \code{importMatched}.
#' @param beta The vector of free parameters, of length \code{noAttr - 1}.
#'
#' @return An array of dimension \code{(noD, noU)}. Its element indexed by
#'   \code{[dIdx, uIdx]} gives the value of the payoff function for that
#'   downstream-upstream pair.
#'
#' @export
evaluatePayoffMatrix <- function(unevalPayoffMatrix, beta) {
    dims <- dim(unevalPayoffMatrix)
    noAttr <- dims[1]
    noU <- dims[2]
    noD <- dims[3]
    u <- c(1, beta)
    d <- array(unevalPayoffMatrix, c(noAttr, noU*noD))
    v <- u %*% d
    w <- array(v, c(noU, noD))
    return(w)
}

#' Evaluate payoff matrices
#'
#' Evaluates the payoff functions for the given parameter values for all
#' markets.
#'
#' @section Payoff matrices:
#'
#' In the current version, we only consider linear payoff functions of the form:
#'   \eqn{f_\beta (t) = D_{t,1} \beta_1 + D_{t,2} \beta_2 + \cdots + D_{t,n} \beta_n}
#' where
#' \itemize{
#'   \item \eqn{\beta} is the vector of free parameters (see below).
#'   \item \eqn{t} is the index triple (\code{mIdx}, \code{uIdx}, \code{dIdx}).
#'   \item \eqn{D_{t,k}} is the \eqn{k}-th distance attribute for the triple
#'     \eqn{t}.
#'   \item \eqn{n} is the number of attributes.
#' }
#' Since functions of this form are invariant with respect to the transformation
#'   \eqn{f_\beta \rightarrow (1/s) f_{s \beta}},
#' we can fix beta_1 equal to 1 and define
#'   \eqn{\beta^'_k = \beta_{k+1}}.
#' This is the convention used here, and also in the optimization functions.
#'
#' For a given market indexed by \code{mIdx}, we can consider the array of
#' payoff functions where the parameter vector has not yet been assigned. Since
#' we are interested in the linear case, this is equivalent to a
#' three-dimensional array indexed by the upstream index, the downstream index,
#' and the attribute index, whose values are \eqn{D_{t,k}}, for all relevant
#' triples \eqn{t}. This is, in fact, the array \code{distanceMatrices[[mIdx]]}
#' described in \code{importMatched}. In the general case, we would have a
#' two-dimensional array of symbolic expressions instead.
#'
#' To evaluate such an array for a given parameter vector, we can simply perform
#' a matrix-vector product, by flattening and unflattening the dimensions
#' corresponding to the upstream and downstream indices. This is performed by the
#' function \code{evaluatePayoffMatrix}.
#'
#' @param unevalPayoffMatrices A list of arrays, one for each market. Each array
#'   has dimension \code{noAttr, noD[mIdx], noU[mIdx]}. Can be the list
#'   \code{distanceMatrices}, as defined in the function \code{importMatched}.
#' @param beta The vector of free parameters, of length \code{noAttr - 1}.
#'
#' @return A list of arrays, one for each market. Each array has dimension
#'   \code{(noD[mIdx], noU[mIdx])}. Its element indexed by \code{[dIdx, uIdx]}
#'   gives the value of the payoff function for that downstream-upstream pair.
#'
#' @export
evaluatePayoffMatrices <- function(unevalPayoffMatrices, beta) {
    return(lapply(unevalPayoffMatrices, function(p) { evaluatePayoffMatrix(p, beta) }))
}

#' Convert attribute matrices to distance matrices
#'
#' Calculates the distance matrices for all markets, given data for unmatched
#' markets (the attribute matrices for upstreams and downstreams).
#'
#' Let \code{mIdx}, \code{uIdx}, \code{dIdx}, and \code{k} index markets,
#' upstreams, downstreams, and attributes respectively. The distance matrix
#' values are defined by the equation:
#'
#' \code{distanceMatrices[[mIdx]][k, dIdx, uIdx] ==
#'   attributeMatricesUp[[mIdx]][k, uIdx] * attributeMatricesDn[[mIdx]][k, dIdx]}.
#'
#' @param attributeMatricesUp,attributeMatricesDn See the return value of the
#'   function \code{importUnmatched}.
#'
#' @return See the return value of the function \code{importMatched}.
#'
#' @export
makeDistanceMatrices <- function(attributeMatricesUp, attributeMatricesDn) {
    noM <- length(attributeMatricesUp)
    distanceMatrices <- lapply(1:noM, function(mIdx) {
        attrUp <- attributeMatricesUp[[mIdx]]
        attrDn <- attributeMatricesDn[[mIdx]]
        noAttr <- dim(attrUp)[1]
        noU <- dim(attrUp)[2]
        noD <- dim(attrDn)[2]
        distList <- lapply(1:noAttr, function(k) { outer(attrDn[k, ], attrUp[k, ]) })
        return(array(t(sapply(distList, as.vector)), c(noAttr, noD, noU)))
    })
    return(distanceMatrices)
}
