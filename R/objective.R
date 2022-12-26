#' Create objective score function
#'
#' Creates a score function from the given data array, in a form suitable for
#' passing to an optimization routine. This function computes the score (number
#' of inequalities satisfied, normalized on their total number) for a given
#' parameter vector (\eqn{\beta}).
#'
#' \loadmathjax
#'
#' The function created is:
#'   \mjsdeqn{g(\beta) = s \frac{1}{n} \sum_{k=1}^{n} a_k^T \tilde{\beta}}
#' where
#' \enumerate{
#'   \item \mjseqn{c_1} is TODO.
#'   \item \mjseqn{s} is TODO.
#'   \item \mjseqn{n} is TODO.
#'   \item \mjseqn{\beta} is the vector of free parameters, of length `noAttr-1`.
#'   \item \mjseqn{\tilde{\beta} = (c_1, \beta)} is the extended vector of
#'     parameters.
#'   \item \mjseqn{a_k} is the \mjseqn{k}-th column of `dataArray`.
#' }
#'
#' @param dataArray See the result of the function `CdataArray`.
#' @param coefficient1 The first coefficient in the extended vector of
#'   parameters. Should typically be `1` (the default) or `-1`.
#' @param objSign Use `-1` (the default) when passing the result to an
#'   optimization routine that can only minimize, such as
#'   `DEoptim::DEoptim`. Otherwise, use `1`.
#'
#' @return A function taking a vector of length `noAttr-1` (the parameters)
#'   and returning an scalar (the score).
#'
#' @export
makeScoreObjFun <- function(dataArray, coefficient1 = 1, objSign = -1) {
    scoreObjFun <- function(b) {
        u <- t(dataArray) %*% c(coefficient1, b)
        return(objSign * sum(u >= 0) / dim(dataArray)[2])
    }
    return(scoreObjFun)
}

# FIXME the docs
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

# FIXME the docs
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
#' The function created is: # FIXME
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
    coefficient1 = 1, objSign = -1, useCorrectionFactor = TRUE) {
    fullScoreObjFun   <- makeScoreObjFun(fullDataArray,   coefficient1, objSign = 1)
    sampleScoreObjFun <- makeScoreObjFun(sampleDataArray, coefficient1, objSign = 1)
    bootstrapObjFun <- function(b) {
        v <- b - betaEst
        q <- 0.5 * as.numeric(t(v) %*% H %*% v)
        correctionFactor <- if (useCorrectionFactor) {
            dim(sampleDataArray)[2] / dim(fullDataArray)[2]
        } else {
            1
        }
        u <- correctionFactor * sampleScoreObjFun(b) - fullScoreObjFun(b) - q
        return(objSign * u)
    }
}

makeBootstrapExtraObjFun <- function(
    fullDataArray, sampleDataArray, betaEst, H,
    coefficient1 = 1, objSign = -1, useCorrectionFactor = TRUE) {
    fullScoreObjFun   <- makeScoreObjFun(fullDataArray,   coefficient1, objSign = 1)
    sampleScoreObjFun <- makeScoreObjFun(sampleDataArray, coefficient1, objSign = 1)
    bootstrapObjFun <- function(b) {
        v <- b - betaEst
        q <- 0.5 * as.numeric(t(v) %*% H %*% v)
        correctionFactor <- if (useCorrectionFactor) {
            dim(sampleDataArray)[2] / dim(fullDataArray)[2]
        } else {
            1
        }
        s <- correctionFactor * sampleScoreObjFun(b)
        f <- fullScoreObjFun(b)
        u <- s - f - q
        terms <- list(s = s, f = f, q = q)
        return(list(val = objSign * u, arg = b, diffArg = v, terms = terms))
    }
}
