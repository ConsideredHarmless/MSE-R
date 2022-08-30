#' Create objective score function
#'
#' Creates a score function from the given data array, in a form suitable for
#' passing to an optimization routine. This function computes the score (number
#' of inequalities satisfied) for a given parameter vector (\eqn{\beta}).
#'
#' The function created is:
#'   \eqn{g(\beta) = \frac{1}{N_{\mathrm{ineqs}} \mathrm{objSign} \sum_{k=1}^{N_{\mathrm{ineqs}} (a_k \beta')}}
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
        return(objSign * sum(u >= 0) / dim(dataArray)[2])
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
#' Create objective function used in Cattaneo's bootstrap
#'
#' Creates an objective function from the given data, in a form suitable for
#' passing to an optimization routine. This function is used internally in the
#' implementation of Cattaneo's bootstrap method.
#'
#' Let:
#' \enumerate{
#'   \item \eqn{g_{X}(\beta)} be the score function defined the by data array
#'     \eqn{X} (see \code{makeScoreObjFun}).
#'   \item \eqn{\hat{\beta}} be the estimate of \eqn{\beta}, obtained by
#'     maximizing \eqn{g_{X}}.
#'   \item \eqn{H} be the H-matrix defined in Cattaneo's paper.
#'   \item \eqn{q_{H,\hat{\beta}}(\beta)} be the quadratic form:
#'     \eqn{\frac{1}{2} (\beta - \hat{\beta})^T H (\beta - \hat{\beta})}
#' }
#' The function created is:
#'   \eqn{B(\beta) = \mathrm{objSign} (
#'     g_{X_\mathrm{sample}}(\beta) -
#'     g_{X_\mathrm{full}}(\beta) -
#'     q_{H,\hat{\beta}}(\beta)
#'   )}.
#'
#' See \code{makeScoreObjFun} for the definition of \eqn{\beta} and
#' \eqn{\mathrm{objSign}}.
#'
#' @param fullDataArray The full data array.
#' @param sampleDataArray The sample data array.
#' @param betaEst The estimate of \eqn{\beta}. This should have been obtained
#'   by calling \code{optimizeScoreFunction} on \code{fullDataArray}.
#' @param H The matrix \eqn{H}, as defined in Cattaneo's paper. Should have
#'   dimensions \eqn{d \times d}, where \eqn{d} is the number of free
#'   parameters.
#' @inheritParams makeScoreObjFun
#'
#' @return A function taking a vector of length \code{noAttr-1} (the parameters)
#'   and returning a real number.
#'
#' @export
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
