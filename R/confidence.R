#' Assign market indices to inequality members
#'
#' @param ineqmembers The result of \code{Cineqmembers}.
#' @return A vector \code{g} of market indices, with the same length as
#'   \code{ineqmembers}, with the property that if \code{g[ineqIdx]} is equal to
#'   \code{mIdx}, then the \code{ineqIdx}-th inequality belongs to the
#'   \code{mIdx}-th market.
#' @export
makeGroupIDs <- function(ineqmembers) {
    f <- function(mIdx) {
        return(rep(mIdx, ineqmembers[[mIdx]]$numIneqs))
    }
    mIdxs <- seq_along(ineqmembers)
    return(unlist(lapply(mIdxs, f)))
}

#' Randomly subsample data array
#'
#' Generates a reduced data array, with columns corresponding to a randomly
#' selected subset of the original markets.
#'
#' @param ssSize The size of the desired market subset. Must not be greater than
#'   the original number of markets. Set to \code{NULL} to sample as many
#'   markets as there are in the original set; this is how this function is
#'   supposed to be used in the cube-root method.
#' @param groupIDs The output of \code{makeGroupIDs}.
#' @param dataArray The output of \code{CdataArray}. Set to \code{NULL} to
#'   generate only the \code{$selectedGroups} and \code{$indices} elements of
#'   the result.
#' @param withReplacement A boolean indicating whether to sample with
#'   replacement, as in the cube-root method, or without, as in the
#'   point-identified method.
#'
#' @return A list with members:
#' \tabular{ll}{
#'   \code{$ssDataArray} \tab An array with the columns of \code{dataArray}
#'     belonging to the selected subset of markets. \cr
#'   \code{$marketIdxs} \tab A vector of length \code{ssSize}, containing the
#'     indices of the selected markets. \cr
#'   \code{$ineqIdxs} \tab A vector of length equal to the number of columns of
#'     the element \code{$ssDataArray}, containing the indices of the selected
#'     inequalities. It has the property that, if \code{r} is the return value
#'     of this function, then \code{dataArray[, r$ineqIdxs]} is equal to
#'     \code{r$ssDataArray}.
#' }
#'
#' @keywords internal
sampleBootstrap <- function(ssSize, groupIDs, dataArray, withReplacement) {
    allMarketIdxs <- unique(groupIDs)
    numMarkets <- max(allMarketIdxs)
    if (is.null(ssSize)) {
        ssSize <- numMarkets
    }
    stopifnot(ssSize >= 1 && ssSize <= numMarkets)
    if (!withReplacement && ssSize == numMarkets) {
        warning("sampleBootstrap: sampling full number of markets without replacement")
    }
    selectedMarketIdxs <- sample(
        allMarketIdxs, size = ssSize, replace = withReplacement)
    # Generate the inequality index vectors corresponding to each selected
    # market, and concatenate them.
    selectedIneqIdxs <- do.call("c", lapply(selectedMarketIdxs, function(mIdx) {
        return(which(groupIDs == mIdx)) }))
    ssDataArray <- if (is.null(dataArray)) { NULL } else { dataArray[, selectedIneqIdxs] }
    return(list(
        ssDataArray = ssDataArray,
        marketIdxs = selectedMarketIdxs,
        ineqIdxs = selectedIneqIdxs))

}

#' Merge passed options with default options
#'
#' Merges the passed options list with the default options list by adding to
#' the former any name-value pairs which only occur in the latter. In case of
#' name conflicts, the former pair is kept.
#'
#' @param options A list of name-value pairs.
#' @param defaultOptions A list of name-value pairs.
#'
#' @return A list of name-value pairs obtained by adding to `options` any pairs
#'   only occuring in `defaultOptions`.
#' @keywords internal
#' @examples mergeOptions(list(x = 1, y = 2), list(y = 3, z = 4))
mergeOptions <- function(options, defaultOptions) {
    if (is.null(options)) {
        options <- list()
    }
    for (name in names(defaultOptions)) {
        if (is.null(options[[name]])) {
            options[[name]] <- defaultOptions[[name]]
        }
    }
    return(options)
}

#' \loadmathjax
#' Calculate confidence region
#'
#' Generates a confidence region estimate using subsampling.
#'
#' The method used is the **point-identified** one. For the cube-root method,
#' see the function \code{\link{newBootstrapCR}}, which has the same signature.
#'
#' @seealso [newBootstrapCR()] for the cube-root method.
#'
#' The estimates are calculated by running the score optimization procedure for
#' many different randomly selected subsets of markets.
#'
#' This function calls \code{optimizeScoreFunction}, therefore we require a list
#' with arguments to be passed to it. The argument \code{optimizeScoreArgs}
#' should contain those arguments contained in a list, except \code{dataArray},
#' such that when \code{dataArray} is assigned to \code{optimizeScoreArgs}, the
#' call \code{do.call(optimizeScoreFunction, optimizeScoreArgs)} is valid.
#'
#' @param dataArray The output of \code{dataArray}.
#' @param groupIDs The output of \code{makeGroupIDs}.
#' @param pointEstimate The vector \mjseqn{\hat{\beta}} of the optimal
#'   parameters, as calculated by \code{optimizeScoreFunction}.
#' @param ssSize The size of the market subset used in subsampling. Must not be
#'   greater than the number of markets.
#' @param numSubsamples The number of subsamples.
#' @param confidenceLevel The confidence level of the region. Must be a number
#'   in the range \eqn{(0, 1)}.
#' @param optimizeScoreArgs A list with the keyword arguments to be used when
#'   \code{optimizeScoreFunction} is called. All non-optional arguments should
#'   be present, except \code{dataArray}.
#' @param options A list of options:
#'   \tabular{ll}{
#'     \code{progressUpdate}  \tab How often to print progress. Defaults to
#'       \code{0} (never). \cr
#'     \code{asymptotics}     \tab Type of asymptotics to use. Supported values
#'       are "nests" (default) or "coalitions".
#'   }
#' @return A list with members:
#' \tabular{ll}{
#'   \code{$crSymm}    \tab The confidence regions of each parameter for the
#'     symmetric case, as an array of dimension \code{(2, numFreeAttrs)}, where
#'     \code{numFreeAttrs} is the total number of attributes minus 1. \cr
#'   \code{$crAsym}    \tab Same as above, for the asymmetric case. \cr
#'   \code{$estimates} \tab The estimates for each parameter, as an array of
#'     dimension \code{(numSubsamples, numFreeAttrs)}. \cr
#'   \code{$samples} \tab The market subsets chosen at each iteration, as an
#'     array of dimension \code{(numSubsamples, ssSize)}, containing their
#'     respective indices.
#' }
#' @export
pointIdentifiedCR <- function(
        dataArray, groupIDs, pointEstimate, ssSize, numSubsamples,
        confidenceLevel, optimizeScoreArgs, options = NULL) {
    defaultOptions <- list(
        progressUpdate = 0, asymptotics = "nests")
    options <- mergeOptions(options, defaultOptions)
    progress  <- options$progressUpdate
    asymp     <- options$asymptotics

    alpha <- 1 - confidenceLevel
    numFreeAttrs <- dim(dataArray)[1] - 1
    pointEstimate <- as.numeric(pointEstimate)

    # This block sets variables that are slightly different for each of the
    # two asymptotics. subNormalization is the standardization multiplier
    # for the subsamples, fullNormalization is the multiplier for the
    # construction of the final confidence interval from all of the subsamples.
    normExponent <- switch(asymp, "nests" = 1/3, "coalitions" = 1/2)
    subNormalization  <- ssSize^normExponent
    fullNormalization <- length(unique(groupIDs))^normExponent

    # Ignore the numRuns argument, in order not to slow down the calculations.
    # This is not required in the newBootstrapCR, because
    # optimizeBootstrapFunction doesn't use it.
    optimizeScoreArgs$numRuns <- NULL
    optimizeScoreArgs$progressUpdate <- NULL

    # Standardized and raw subsample estimates.
    # Note: these arrays are transposed compared to the old function.
    # estimates[paramIdx, iterIdx] gives the estimate for the parameter with
    # index paramIdx in iteration with index iterIdx.
    samples <- array(0, dim = c(numSubsamples, ssSize))
    calcEstimate <- function(iterIdx) {
        bootstrapSample <- sampleBootstrap(
            ssSize, groupIDs, dataArray, withReplacement = FALSE)
        optimizeScoreArgs$dataArray <- bootstrapSample$ssDataArray
        optResult <- do.call(optimizeScoreFunction, optimizeScoreArgs)
        ssEstimate <- optResult$optArg
        # The <<- operator is required to modify objects outside the closure.
        samples[iterIdx, ] <<- bootstrapSample$marketIdxs
        if (progress > 0 && iterIdx %% progress == 0) {
            cat(sprintf("[pointIdentifiedCR] Iterations completed: %d\n", iterIdx))
        }
        return(ssEstimate)
    }
    ssEstimates <- sapply(1:numSubsamples, calcEstimate)
    estimates   <- subNormalization * (ssEstimates - pointEstimate)

    # For the symmetric case, we want to add and subtract the (1 - alpha')-th
    # quantile from the point estimate. We take the Abs here for simplicity:
    #   tn*Abs[x - y] == Abs[tn*(x - y)].
    # For the asymmetric case we separately take the alpha/2 and 1 - alpha/2
    # quantiles and subtract them. Keep in mind that since estimates has its
    # mean subtracted, only in freakishly unlikely cases will these two have
    # the same sign. This is not true for the symmetric case.
    calcConfRegionSymm <- function(paramIdx) {
        qSymm <- stats::quantile(
            abs(estimates[paramIdx, ]),
            c(1 - alpha, 1 - alpha),
            names=FALSE, type=1)
        qSymm <- c(-1, 1)*qSymm
        qSymm <- rev(qSymm) / fullNormalization
        return(pointEstimate[paramIdx] - qSymm)
    }
    calcConfRegionAsym <- function(paramIdx) {
        qAsym <- stats::quantile(
            (estimates[paramIdx, ]),
            c(alpha/2, 1 - alpha/2),
            names=FALSE, type=1)
        qAsym <- rev(qAsym) / fullNormalization
        return(pointEstimate[paramIdx] - qAsym)
    }
    crSymm <- sapply(1:numFreeAttrs, calcConfRegionSymm)
    crAsym <- sapply(1:numFreeAttrs, calcConfRegionAsym)

    result <- list(
        crSymm = crSymm, crAsym = crAsym, estimates = t(estimates),
        samples = samples)
    return(result)
}

#' Plots confidence region estimates
#'
#' A convenience function that creates a plot of each parameter estimate.
#' @param estimates See the output of \code{pointIdentifiedCR}.
#' @export
plotCR <- function(estimates) {
    estimates <- estimates
    graphics::plot(
        c(col(estimates)), c(estimates), type="p", col="blue", xlab="", ylab="")
    graphics::abline(h=0, v=0)
}

#' \loadmathjax
#' Create estimator for H matrix
#'
#' Creates the estimator matrix \mjseqn{\tilde{H}_n} using the numerical
#' derivative method.
#'
#' @param dataArray The output of \code{dataArray}.
#' @param betaEst The vector \mjseqn{\hat{\beta}}, of length \mjseqn{d}, whose
#'   value is the estimate of \mjseqn{\beta}, obtained from maximizing the
#'   score function corresponding to `dataArray`.
#' @param eps The numerical derivative step size matrix
#'   \mjseqn{\epsilon_{n,kl}}, of size \mjseqn{d \times d}.
#'
#' @return The matrix \mjseqn{H}.
#' @keywords internal
makeHnumder <- function(dataArray, betaEst, eps) {
    d <- dim(dataArray)[1] - 1
    scoreObjFun <- makeScoreObjFun(dataArray, objSign = 1)
    f <- function(idx1d) {
        # Convert 1d index to 2d -- remember, *column-major* order.
        col <- (idx1d - 1) %/% d + 1
        row <- (idx1d - 1) %%  d + 1
        # Use formula in section 3.1 of Cattaneo et al. paper.
        # Might not be computationally optimal.
        epsV <- eps[row, col]
        rowArgTerm <- rep(0, d)
        rowArgTerm[row] <- epsV
        colArgTerm <- rep(0, d)
        colArgTerm[col] <- epsV
        term1 <- scoreObjFun(betaEst + rowArgTerm + colArgTerm)
        term2 <- scoreObjFun(betaEst + rowArgTerm - colArgTerm)
        term3 <- scoreObjFun(betaEst - rowArgTerm + colArgTerm)
        term4 <- scoreObjFun(betaEst - rowArgTerm - colArgTerm)
        element <- (-term1 + term2 + term3 - term4) / (4*epsV^2)
    }
    H <- matrix(sapply(1:(d*d), f), d, d)
    return(H)
}

#' \loadmathjax
#' @inherit makeHnumder
#'
#' @description
#' Creates the estimator matrix \mjseqn{\tilde{H}_n} using the plug-in (kernel)
#' method.
#'
#' @details
#' ## Plug-in kernel
#' Note that the plug-in estimator method requires a kernel function with
#' specific properties, as shown in the paper and its supplement. The only
#' kernel currently available is the function \mjseqn{K(u) = \phi(u)}, i.e. the
#' pdf of the standard normal distribution, with first derivative
#' \mjseqn{\dot{K}(u) = -u\phi(u)}.
#'
#' @param h The bandwidth matrix
#'   \mjseqn{h_{n,kl}}, of size \mjseqn{d \times d}.
makeHplugin <- function(dataArray, betaEst, h) {
    d <- dim(dataArray)[1] - 1
    # First derivative K'(u) of the kernel function K(u), as it is defined in
    # the Cattaneo et al. (2020) paper. Note that, in their notation, it is not
    # K(u) that approximates the indicator function, but its antiderivative!
    # We have:
    #   K_n(u) = K(u/h_n)/h_n
    #   K_n'(u) = d/du K_n(u) = K'(u/h_n)/h_n^2
    # NOTE: If additional (or even user-supplied) kernel functions are later
    # used, there are two important points:
    # * The function K'(u) must satisfy certain conditions (see Condition K in
    #   the supplement).
    # * Certain constants in the rot function, resulting from integrals
    #   involving K'(u), will have to be recalculated.
    kernelFun1 <- function(u) { -u*stats::dnorm(u) }
    beta <- c(1, betaEst)
    x <- dataArray
    z <- beta %*% x
    f <- function(idx1d) {
        col <- (idx1d - 1) %/% d + 1
        row <- (idx1d - 1) %%  d + 1
        hV <- h[row, col]
        x_row <- as.vector(x[row + 1, ])
        x_col <- as.vector(x[col + 1, ])
        return(-mean(x_row*x_col*kernelFun1(z/hV)/hV^2))
    }
    H <- matrix(sapply(1:(d*d), f), d, d)
    return(H)
}

#' \loadmathjax
#' Create estimator for H matrix
#'
#' Creates the estimator matrix \mjseqn{\tilde{H}_n} (\mjseqn{H} for short),
#' used in \code{\link{newBootstrapCR}}. Handles choice of method, correct
#' set-up of step/bandwidth, its calculation using ROT if required, and
#' conversion to positive semidefinite.
#'
#' The construction of \mjseqn{H} is based on the paper by Cattaneo et al.
#' (2020), linked in the document page of the \code{\link{newBootstrapCR}}
#' function.
#'
#' ## Choice of method
#' Two methods for estimating the matrix \mjseqn{H_0} are implemented. The first
#' uses numerical differentiation, as shown in section 3.1., and the other
#' creates a "plug-in" estimator for this specific model, as in section 4.1..
#'
#' The choice of method is controlled by the option `Hest`.
#'
#' ### Plug-in kernel
#' Note that the plug-in estimator method requires a kernel function with
#' specific properties, as shown in the paper and its supplement. The only
#' kernel currently available is the function \mjseqn{K(u) = \phi(u)}, i.e. the
#' pdf of the standard normal distribution, with first derivative
#' \mjseqn{\dot{K}(u) = -u\phi(u)}.
#'
#' ## Step/bandwidth
#' Both methods require a matrix of parameters: for the numerical derivative
#' method, this is the step \mjseqn{\epsilon_{n,kl}}, and for the plug-in
#' method, this is the bandwidth \mjseqn{h_{n,kl}}. In both cases, this value
#' has to be a matrix of size equal to the size of \mjseqn{H}, i.e.
#' \mjseqn{d \times d}, where \mjseqn{d} is the number of free attributes.
#'
#' The user can also pass a scalar value for this parameter, in which case it
#' is automatically converted to a matrix of the appropriate size with
#' constant elements. However, if the covariates have different scale, then a
#' matrix with different entry-wise values would be more appropriate.
#'
#' The parameter scalar or matrix can be passed using the option `bw`.
#'
#' ### Rule-Of-Thumb
#' It is usually the case that the user does not have the correct values of the
#' step or bandwidth parameters. In this case, they can be automatically
#' calculated using a method called *Rule-Of-Thumb* (ROT). See the linked
#' paper and its supplement, as well the function \code{\link{rot}} for more
#' details. To select this option, the user can set the option `bw` to `"rot"`.
#'
#' ## Conversion to positive semidefinite
#' Although the matrix \mjseqn{H_0} is positive semidefinite, this is not
#' necessarily the case for its estimate \mjseqn{H}. Since this matrix is used
#' in a quadratic form in the bootstrap optimization procedure, it can be
#' useful to replace the matrix calculated with an approximation which
#' satisfies this requirement.
#'
#' Consider a non-positive-semidefinite matrix \mjseqn{H}, and let
#' \mjseqn{\lambda_1 \geq \lambda_2 \geq \dots \geq \lambda_d} be its
#' eigenvalues in descending order. We provide two approaches:
#' \enumerate{
#'   \item In the first approach, a constant
#'     \mjseqn{\kappa = \epsilon_{\mathrm{tol}} - \lambda_d} is added to all
#'     diagonal elements of \mjseqn{H}, where \mjseqn{\lambda_d < 0} is the
#'     smallest eigenvalue of \mjseqn{H}, and \mjseqn{\epsilon_{\mathrm{tol}}}
#'     is a non-negative tolerance.
#'   \item In the second approach, all negative eigenvalues of \mjseqn{H} are
#'     replaced with \mjseqn{0}.
#' }
#'
#' Note that both \mjseqn{H_0} and \mjseqn{H} are always symmetric by
#' construction.
#'
#' The options `makePosDef` and `makePosDefTol` control this feature.
#'
#' ## Bypassing the calculations
#' The user can also skip the entire calculation of \mjseqn{H} and pass their
#' own value instead, using the option `Hbypass`.
#'
#' @inheritParams newBootstrapCR
#' @param options See options `Hest`, `bw`, `makePosDef`, `makePosDefTol`,
#'   `Hbypass`, and `debugLogging` from \code{\link{newBootstrapCR}}.
#'
#' @return The matrix \mjseqn{H}.
#' @keywords internal
makeHmatrix <- function(dataArray, pointEstimate, options) {
    if (!is.null(options$Hbypass)) {
        H <- options$Hbypass
        if (options$debugLogging) {
            print("[DEBUG] in makeHmatrix: bypassing calculation of H, using provided matrix")
        }
        return(H)
    }
    bwOpts <- switch(
        tolower(options$Hest),
        numder = list(method = "bw.nd.summed", fn = makeHnumder),
        plugin = list(method = "bw.ker",       fn = makeHplugin),
        stop(sprintf("method %s not implemented", options$Hest)))
    bw <- options$bw
    if (tolower(bw) == "rot") {
        x <- dataArray
        n <- dim(x)[2]
        k <- 8
        y <- rep(1, n)
        bw <- rot(y, x, k, pointEstimate, options$debugLogging)[[bwOpts$method]]
    }
    # If bw is a scalar, replace it with a matrix with the same elements.
    if (is.atomic(bw) && length(bw) == 1) {
        numFreeAttrs <- dim(dataArray)[1] - 1
        bw <- matrix(bw, numFreeAttrs, numFreeAttrs)
    }
    H <- bwOpts$fn(dataArray, pointEstimate, bw)
    # TODO warn user if H has many zeros, or too large entries
    if (options$debugLogging) {
        print("[DEBUG] in makeHmatrix: bw =")
        print(bw)
        print("[DEBUG] in makeHmatrix: H =")
        print(H)
    }
    if (options$makePosDef) {
        eigH <- eigen(H)
        eigvals <- eigH$values
        minEigval <- min(eigvals) # λ_d
        if (minEigval <= 0) { # TODO <= 0 or < 0?
            if (options$debugLogging) {
                print("[DEBUG] in makeHmatrix: H is not positive definite")
                print(sprintf(
                    "[DEBUG] in makeHmatrix: (smallest eigenvalue is %f)", minEigval))
            }
            if (options$makePosDefTol == "drop") {
                eigvals[eigvals < 0] <- 0
                H <- eigH$vectors %*% diag(eigvals) %*% t(eigH$vectors)
                if (options$debugLogging) {
                    print("[DEBUG] in makeHmatrix: dropping negative eigenvalues")
                }
            } else {
                incr <- options$makePosDefTol - minEigval # κ = ε - λ_d
                H <- H + incr * diag(dim(H)[1])
                if (options$debugLogging) {
                    print(sprintf(
                        "[DEBUG] in makeHmatrix: adding %f to diagonal elements", incr))
                }
            }
            if (options$debugLogging) {
                print("[DEBUG] in makeHmatrix: new H =")
                print(H)
            }
        }
    }
    return(H)
}

# TODO rename to cubeRootBootstrapCR, but keep old name as alias
#' Calculate confidence region
#'
#' Generates a confidence region estimate using a bootstrap method with
#' cube-root asymptotics.
#'
#' The method used is the **cube-root** one. For the point-identfied method,
#' see the function \code{\link{pointIdentifiedCR}}, which has the same signature.
#'
#' The estimates are calculated by sampling, with replacement, a number of
#' markets equal to their total number, creating its associated data array, and
#' maximizing a function related to the difference of their scores, augmented
#' by a quadratic term.
#'
#' \loadmathjax
#'
#' In particular, let \mjseqn{S(\beta; X)} be the score function corresponding
#' to the data array \mjseqn{X} (see also \code{\link{makeScoreObjFun}}),
#' \mjseqn{\hat{\beta}} be the point estimate previously calculated, and
#' \mjseqn{H} (short for \mjseqn{\tilde{H}_n}) be the matrix which estimates
#' \mjseqn{H_0}. After sampling the full data array \mjseqn{X_{\mathrm{full}}},
#' we create the sample data array \mjseqn{X_{\mathrm{sample}}}. Define now the
#' quadratic term
#' \mjsdeqn{
#'   q(\beta; \hat{\beta}, H) =
#'   \frac{1}{2} (\beta - \hat{\beta})^T H (\beta - \hat{\beta})}
#' Then the function maximized at each bootstrap step is
#' \mjsdeqn{
#'   B(\beta; \hat{\beta}, H, X_{\mathrm{full}}, X_{\mathrm{sample}}) =
#'   c S(\beta; X_{\mathrm{sample}}) - S(\beta; X_{\mathrm{full}}) - q(\beta; \hat{\beta}, H)}
#' Note that, as defined, the score function is normalized on the number of
#' inequalities of the data array, and therefore takes values in the interval
#' \mjseqn{[0, 1]}.
#'
#' For the calculation of the matrix \mjseqn{H}, see the function
#' \code{\link{makeHmatrix}}.
#'
#' The *correction factor* \mjseqn{c} has a value which, by default, is equal to
#' the ratio of the number of inequalities in the sample data array to the
#' number of inequalities in the full data array. See also the option
#' `useCorrectionFactor` in the `options` list.
#'
#' As in `pointIdentifiedCR`, a list of arguments related to optimizing
#' the above function is required. The optimization is performed by the
#' function `optimizeBootstrapFunction`, which is similar to
#' `optimizeScoreFunction.` The argument `optimizeScoreArgs`
#' should be a list with the following elements:
#' \itemize{
#'   \item `bounds`
#'   \item `coefficient1`
#'   \item `method`
#'   \item `optimParams`
#' }
#' The list of arguments to \code{optimizeBootstrapFunction} is then internally
#' constructed using the above data.
#'
#' @seealso [pointIdentifiedCR()] for the point-identified method.
#'
#' @references
#' This method is adapted from the paper: \cr
#' M. D. Cattaneo, M. Jansson, and K. Nagasawa,
#'   “Bootstrap-Based Inference for Cube Root Asymptotics”,
#'   *Econometrica*, vol. 88, no. 5, pp. 2203–2219, September 2020. \cr
#' Links to the
#'   [paper](https://cattaneo.princeton.edu/papers/Cattaneo-Jansson-Nagasawa_2020_ECMA.pdf),
#'   its [supplement](https://cattaneo.princeton.edu/papers/Cattaneo-Jansson-Nagasawa_2020_ECMA--Supplement.pdf),
#'   and an [implementation](https://github.com/mdcattaneo/replication-CJN_2020_ECMA) are provided.
#'
#' @inheritParams pointIdentifiedCR
#' @param ssSize Currently ignored. In `pointIdentifiedCR`, this is the number
#'   of markets in each sample, but in the cube-root method, this number is
#'   always equal to the number of markets in the full data array.
#' @param options A list of options:
#'   \tabular{ll}{
#'     `progressUpdate` \tab How often to print progress. Defaults to
#'       `0` (never). \cr
#'     `centered` \tab A boolean selecting whether the confidence regions, i.e.
#'       the element `$cr` in the result, are centered (meaning that, for each
#'       attribute index \mjseqn{k}, the value \mjseqn{\hat{\beta}_k} is
#'       subtracted from the result, where \mjseqn{\hat{\beta}} is the point
#'       estimate). Note that the estimate array, i.e. the element `$estimates`
#'       in the result, are always *centered*, and the element `$rawEstimates`
#'       has estimates which are always *uncentered*. Defaults to `FALSE`. \cr
#'     `Hest` \tab Which method to use for estimating the matrix \mjseqn{H_0}.
#'       Choices are `"numder"` and `"plugin"`, which use the numerical
#'       derivative method and the plug-in method described in the paper,
#'       respectively. Note that for the plug-in method, the only kernel
#'       currently available is the function \mjseqn{K(u) = \phi(u)}, i.e. the
#'       pdf of the standard normal distribution. Defaults to `"plugin"`. \cr
#'     `bw` \tab This value is required for the calculation of \mjseqn{H}. For
#'       the numerical derivative method, this corresponds to the step
#'       \mjseqn{\epsilon_{n,kl}}, and for the plug-in method, this corresponds
#'       to the bandwidth \mjseqn{h_{n,kl}}. In both cases, this value is
#'       supposed to be a matrix of size \mjseqn{d \times d}, where \mjseqn{d}
#'       is the number of free attributes, but if given as a scalar, it is
#'       automatically converted to a matrix of the appropriate size with
#'       constant elements.
#'       Alternatively, the user can set this to `"rot"`, in which case the
#'       step or bandwidth is calculated using the Rule-Of-Thumb (ROT) method.
#'       For more information, the user can consult the paper by Cattaneo et al.
#'       (2020), linked in this document page, and its supplement.
#'       Defaults to `1`. \cr
#'     `makePosDef` \tab A boolean selecting whether to correct the calculated
#'       \mjseqn{H} matrix if it is not positive semidefinite. Two approaches
#'       are implemented. In the first, a constant value \mjseqn{\kappa} is
#'       added to each diagonal element of \mjseqn{H}. This value is defined
#'       as \mjseqn{\epsilon_{\mathrm{tol}} - \lambda_d}, where
#'       \mjseqn{\lambda_d \leq 0} is the smallest eigenvalue of \mjseqn{H}, and
#'       \mjseqn{\epsilon_{\mathrm{tol}}} is a non-negative tolerance. In the
#'       second, all negative eigenvalues of \mjseqn{H} are replaced with
#'       \mjseqn{0}. See also the option `makePosDefTol`. Defaults to `FALSE`.
#'       \cr
#'     `makePosDefTol` \tab This value controls how the positive semidefinite
#'       correction of \mjseqn{H} is done (see also the option `makePosDef`). It
#'       can be a non-negative scalar, denoting the tolerance
#'       \mjseqn{\epsilon_{\mathrm{tol}}}, if the first approach is desired, or
#'       the value `"drop"`, if the second approach is desired.
#'       Ignored if the option `makePosDef` is `FALSE`. Defaults to `1e-5` (TODO drop). \cr
#'     `Hbypass` \tab If provided, skips the entire calculation of \mjseqn{H}
#'       and uses this value instead. Defaults to `NULL`. \cr
#'     `useCorrectionFactor` \tab A boolean selecting whether to use the ratio
#'       described in the *Details* section as the correction factor
#'       (if `TRUE`), or revert to the older behavior, where that factor was
#'       equal to `1` (if `FALSE`). Defaults to `TRUE`. \cr
#'     `debugLogging` \tab Whether this function and others called from it
#'       should print information for debugging purposes. Defaults to `FALSE`.
#'   }
#' @return A list with members:
#' \tabular{ll}{
#'   `$cr` \tab The confidence regions of each parameter, as an array of
#'     dimension `(2, numFreeAttrs)`, where `numFreeAttrs` is the
#'     total number of attributes minus 1. \cr
#'   `$estimates` \tab The estimates for each parameter, as an array of
#'     dimension `(numSubsamples, numFreeAttrs)`. \cr
#'   `$rawEstimates` \tab TODO \cr
#'   `$samples` \tab The market subsets chosen at each iteration, as an
#'     array of dimension `(numSubsamples, ssSize)`, containing their
#'     respective indices. Note that here, `ssSize` denotes the total
#'     number of markets. \cr
#'   `$bootstrapEvalInfos` \tab TODO \cr
#'   `$H` \tab The matrix \mjseqn{H} used.
#' }
#' @export
newBootstrapCR <- function(
        dataArray, groupIDs, pointEstimate, ssSize, numSubsamples,
        confidenceLevel, optimizeScoreArgs, options = NULL) {
    defaultOptions <- list(
        progressUpdate = 0, centered = FALSE,
        Hest = "plugin", bw = 1, makePosDef = FALSE, makePosDefTol = 1e-5,
        Hbypass = NULL, useCorrectionFactor = TRUE,
        debugLogging = FALSE)
    options <- mergeOptions(options, defaultOptions)
    progress <- options$progressUpdate
    debugLogging <- options$debugLogging

    alpha <- 1 - confidenceLevel
    numFreeAttrs <- dim(dataArray)[1] - 1
    pointEstimate <- as.numeric(pointEstimate)

    H <- makeHmatrix(dataArray, pointEstimate, options)

    # Raw (uncentered) and centered estimates.
    # For the centered estimates, we subtract the point estimate.
    # Note: these arrays are transposed compared to the old function.
    # estimates[paramIdx, iterIdx] gives the estimate for the parameter with
    # index paramIdx in iteration with index iterIdx.
    # IMPORTANT NOTE:
    # We currently follow Cattaneo's implemenetation and sample N_market groups
    # each time, rather than ssSize groups. This might later change.
    # samples <- array(0, dim = c(numSubsamples, ssSize))
    samples <- array(0, dim = c(numSubsamples, max(groupIDs)))
    bootstrapEvalInfos <- list()
    calcEstimate <- function(iterIdx) {
        bootstrapSample <- sampleBootstrap(
            ssSize = NULL, groupIDs, dataArray, withReplacement = TRUE)
        optimizeBootstrapArgs <- list(
            fullDataArray = dataArray,
            sampleDataArray = bootstrapSample$ssDataArray,
            betaEst = pointEstimate, H = H,
            bounds = optimizeScoreArgs$bounds,
            coefficient1 = optimizeScoreArgs$coefficient1,
            method = optimizeScoreArgs$method,
            optimParams = optimizeScoreArgs$optimParams,
            useCorrectionFactor = options$useCorrectionFactor
        )
        optResult <- do.call(optimizeBootstrapFunction, optimizeBootstrapArgs)
        ssEstimate <- optResult$optArg
        # The <<- operator is required to modify objects outside the closure.
        samples[iterIdx, ] <<- bootstrapSample$marketIdxs
        bootstrapEvalInfos[[iterIdx]] <<- optResult$bootstrapEvalInfo
        if (progress > 0 && iterIdx %% progress == 0) {
            cat(sprintf("[newBootstrapCR] Iterations completed: %d\n", iterIdx))
        }
        return(ssEstimate)
    }
    rawEstimates <- sapply(1:numSubsamples, calcEstimate)
    estimates    <- rawEstimates - pointEstimate

    # The resulting confidence region is CENTERED.
    calcConfRegion <- function(paramIdx) {
        q <- stats::quantile(
            estimates[paramIdx, ],
            c(alpha/2, 1 - alpha/2),
            names=FALSE, type=1)
        return(q)
    }
    crCentered <- sapply(1:numFreeAttrs, calcConfRegion)
    if (options$centered) {
        cr <- crCentered
    } else {
        cr <- t(t(crCentered) + pointEstimate)
    }

    result <- list(
        cr = cr,
        estimates = t(estimates), rawEstimates = t(rawEstimates),
        samples = samples, bootstrapEvalInfos = bootstrapEvalInfos, H = H)
    return(result)
}
