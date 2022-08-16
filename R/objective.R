#' Create objective score function
#'
#' Creates a score function from the given data array, in a form suitable for
#' passing to an optimization routine. This function computes the score (number
#' of inequalities satisfied) for a given parameter vector (\eqn{\beta}).
#'
#' The function created is:
#'   \eqn{g(\beta) = \mathrm{objSign} \sum_{k=1}^{N_{\mathrm{ineqs}} (a_k \beta')}}
#' where
#' \enumerate{
#'   \item \eqn{\beta} is the vector of free parameters, of length \code{noAttr-1}.
#'   \item \eqn{\beta' = (\mathrm{coefficient1}\, \beta)} is the extended
#'     vector of parameters.
#'   \item \eqn{a_k} is the \eqn{k}-th column of \code{dataArray}.
#' }
#'
#' @param dataArray See the result of the function \code{CdataArray}.
#' @param coefficient1 The first coefficient in the extended vector of
#'   parameters. Should typically be \code{1} (the default) or \code{-1}.
#' @param objSign Use \code{-1} (the default) when passing the result to an
#'   optimization routine that can only minimize, such as
#'   \code{DEoptim::DEoptim}. Otherwise, use \code{1}.
#'
#' @return A function taking a vector of length \code{noAttr-1} (the parameters)
#'   and returning an integer (the score).
#'
#' @export
makeScoreObjFun <- function(dataArray, coefficient1 = 1, objSign = -1) {
    scoreObjFun <- function(b) {
        u <- t(dataArray) %*% c(coefficient1, b)
        return(objSign * sum(u >= 0))
    }
    return(scoreObjFun)
}

#' Create vector-valued score function
#'
#' Creates a vector-valued score function from the given data array. The purpose
#' of this function is not to be passed to an optimization routine, but to track
#' which inequalities were satisfied.
#'
#' @inheritParams makeScoreObjFun
#'
#' @return A function taking a vector of length \code{noAttr-1} (the parameters)
#'   and returning a vector of integers (the scores for each market).
#'
#' @export
makeScoreObjFunVec <- function(dataArray, coefficient1 = 1, objSign = -1) {
    scoreObjFunVec <- function(b) {
        u <- t(dataArray) %*% c(coefficient1, b)
        return(objSign * as.vector(u >= 0))
    }
    return(scoreObjFunVec)
}

makeBootstrapObjFun <- function(
    fullDataArray, sampleDataArray, betaEst, H,
    coefficient1 = 1, objSign = -1) {
    fullScoreObjFun   <- makeScoreObjFun(fullDataArray,   coefficient1, objSign = 1)
    sampleScoreObjFun <- makeScoreObjFun(sampleDataArray, coefficient1, objSign = 1)
    bootstrapObjFun <- function(b) {
        v <- b - betaEst
        q <- 0.5 * as.numeric(t(v) %*% H %*% v)
        u <- sampleScoreObjFun(b) - fullScoreObjFun(b) - q
        return(objSign * u)
    }
}
