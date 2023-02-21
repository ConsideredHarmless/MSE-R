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
#'   \mjsdeqn{g(\beta) = s \frac{1}{n} \sum_{k=1}^{n} \mathbb{1} \left( a_k^T \tilde{\beta} \right)}
#' where
#' \itemize{
#'   \item \mjseqn{c_1} is the first coefficient of the extended vector of
#'     parameters.
#'   \item \mjseqn{s} is \mjseqn{-1} if the function has to be passed to a
#'     minimization method, \mjseqn{1} otherwise.
#'   \item \mjseqn{n} is the number of inequalities/`dataArray` rows..
#'   \item \mjseqn{\beta} is the vector of free parameters, of length `noAttr-1`.
#'   \item \mjseqn{\tilde{\beta} = (c_1, \beta)} is the extended vector of
#'     parameters.
#'   \item \mjseqn{a_k} is the \mjseqn{k}-th column of `dataArray`.
#'   \item \mjseqn{\mathbb{1}(u)} is \mjseqn{1} if \mjseqn{u \geq 0}, \mjseqn{0}
#'     otherwise.
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

#' Create vector-valued score function
#'
#' Creates a vector-valued score function from the given data array. The purpose
#' of this function is not to be passed to an optimization routine, but to track
#' which inequalities were satisfied. The objective function is similar to
#' \code{\link{makeScoreObjFun}}, except that the sum is not divided by \eqn{n}.
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

#' Create objective function used in \code{\link{cubeRootBootstrapCR}}
#'
#' Creates an objective function from the given data, in a form suitable for
#' passing to an optimization routine. This function is used internally in
#' \code{\link{cubeRootBootstrapCR}}.
#'
#' \loadmathjax
#'
#' Let:
#' \itemize{
#'   \item \mjseqn{S(\beta; X)} be the score function defined the by data array
#'     \mjseqn{X} (see \code{\link{makeScoreObjFun}}).
#'   \item \mjseqn{\hat{\beta}} be the estimate of \mjseqn{\beta}, obtained by
#'     maximizing \mjseqn{S(\beta; X)}.
#'   \item \eqn{H} be the \mjseqn{H}-matrix defined in the paper mentioned in
#'     \code{\link{cubeRootBootstrapCR}}.
#'   \item \mjseqn{q(\beta; \hat{\beta}, H)} be the quadratic form
#'     \mjseqn{\frac{1}{2} (\beta - \hat{\beta})^T H (\beta - \hat{\beta})}
#' }
#' The function created is:
#' \mjsdeqn{
#'   B(\beta; \hat{\beta}, H, X_{\mathrm{full}}, X_{\mathrm{sample}}) =
#'   c S(\beta; X_{\mathrm{sample}}) - S(\beta; X_{\mathrm{full}}) - q(\beta; \hat{\beta}, H)}
#'
#' See \code{\link{makeScoreObjFun}} for the definition of \mjseqn{\beta} and
#' `objSign`, and \code{\link{cubeRootBootstrapCR}} for the definition of the
#' correction factor \mjseqn{c}.
#'
#' @param fullDataArray The full data array.
#' @param sampleDataArray The sample data array.
#' @param betaEst The estimate of \eqn{\beta}. This should have been obtained
#'   by calling \code{optimizeScoreFunction} on \code{fullDataArray}.
#' @param H The matrix \eqn{H}, as defined in the paper by Cattaneo et al..
#'   Should have
#'   dimensions \mjseqn{d \times d}, where \eqn{d} is the number of free
#'   parameters.
#' @param useCorrectionFactor A boolean selecting whether to apply the
#'   correction factor. If `TRUE` (the default value), then
#'   \mjseqn{c = \frac{m}{n}}, otherwise it is \mjseqn{1}.
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

#' Create objective function used in the \code{\link{cubeRootBootstrapCR}}
#' (with extra information)
#'
#' Similar to \code{\link{makeBootstrapObjFun}}, except it also returns extra
#' information on the terms of \eqn{B}.
#'
#' See \code{\link{makeBootstrapObjFun}}.
#'
#' \loadmathjax
#'
#' @inherit makeBootstrapObjFun
#'
#' @return A function taking a vector `b` and returning a list with members:
#' \tabular{ll}{
#'   `$val` \tab The value that the result of \code{\link{makeScoreObjFun}} would
#'     return, if called with argument with argument `b`. \cr
#'   `$arg` \tab The argument `b`. \cr
#'   `$diffArg` \tab The value \mjseqn{\beta - \hat{\beta}}. \cr
#'   `$terms` \tab The values of the three terms of \mjseqn{B}, as a vector.
#' }
#' @keywords internal
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
