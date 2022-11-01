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

# generateRandomSubsample(ssSize, groupIDs, dataArray) generates a subsample
# of a given size from a data array.
# ssSize is an integer denoting the number of markets in the subsample.
# For groupIDs, see the function makeGroupIDs.
# For the structure dataArray, see the function CdataArray.
# Returns an array with the same structure as dataArray, containing only the
# columns corresponding to inequalities in the (randomly) selected markets.

#' Randomly subsample data array
#'
#' Generates a reduced data array, with columns corresponding to a randomly
#' selected subset of the original markets.
#'
#' @param ssSize The size of the market subset. Must not be greater than the
#'   number of markets.
#' @param groupIDs The output of \code{makeGroupIDs}.
#' @param dataArray The output of \code{CdataArray}.
#'
#' @return A list with members:
#' \tabular{ll}{
#'   \code{$ssDataArray} \tab An array with the columns of \code{dataArray}
#'     belonging to the selected subset of markets. \cr
#'   \code{$selectedGroups} \tab A vector of length \code{ssSize}, containing
#'     the indices of the chosen markets.
#' }
#'
#' @keywords internal
generateRandomSubsample <- function(ssSize, groupIDs, dataArray) {
    uniqueGroups <- unique(groupIDs)
    selectedGroups <- sort(sample(uniqueGroups, ssSize))
    # Get the indices of the dataArray columns that correspond to the selected
    # groups.
    qualifiedIndices <- which(groupIDs %in% selectedGroups)
    ssDataArray <- dataArray[, qualifiedIndices]
    return(list(ssDataArray = ssDataArray, selectedGroups = selectedGroups))
}

# TODO document and refactor with generateRandomSubsample.
sampleBootstrap <- function(groupIDs, dataArray) {
    uniqueGroups <- unique(groupIDs)
    selectedGroups <- sample(uniqueGroups, replace = TRUE)
    numIneqs <- dim(dataArray)[2]
    f <- function(g) { return((1:numIneqs)[groupIDs == g]) }
    indices <- do.call("c", lapply(selectedGroups, f))
    ssDataArray <- dataArray[, indices]
    return(list(ssDataArray = ssDataArray,
                selectedGroups = selectedGroups,
                indices = indices))
}

#' Calculate confidence region
#'
#' Generates a confidence region estimate using subsampling.
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
#' @param pointEstimate The optimal parameters, as calculated by
#'   \code{optimizeScoreFunction}.
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
        progressUpdate = 0, confidenceLevel = 0.95, asymptotics = "nests")
    if (is.null(options)) {
        options <- list()
    }
    for (name in names(defaultOptions)) {
        if (is.null(options[[name]])) {
            options[[name]] <- defaultOptions[[name]]
        }
    }
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

    # Standardized and raw subsample estimates.
    # Note: these arrays are transposed compared to the old function.
    # estimates[paramIdx, iterIdx] gives the estimate for the parameter with
    # index paramIdx in iteration with index iterIdx.
    samples <- array(0, dim = c(numSubsamples, ssSize))
    calcEstimate <- function(iterIdx) {
        sample <- generateRandomSubsample(ssSize, groupIDs, dataArray)
        optimizeScoreArgs$dataArray <- sample$ssDataArray
        optResult <- do.call(optimizeScoreFunction, optimizeScoreArgs)
        ssEstimate <- optResult$optArg
        # The <<- operator is required to modify objects outside the closure.
        samples[iterIdx, ] <<- sample$selectedGroups
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

#' Create estimator for H matrix
#'
#' Creates the estimator matrix H_n, which is used in Cattaneo's bootstrap
#' method. Uses the numerical derivative method.
#'
#' @param dataArray TODO
#' @param betaEst A vector of length \code{d}, whose value is the estimate of
#'   \eqn{\beta}, obtained from maximizing the score function corresponding to
#'   \code{dataArray}.
#' @param eps TODO
#'
#' @return TODO
#' @export
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

#' Create estimator for H matrix
#'
#' Creates the estimator matrix H_n, which is used in Cattaneo's bootstrap
#' method. Uses the plug-in (kernel) method.
#'
#' @param dataArray TODO
#' @param betaEst A vector of length \code{d}, whose value is the estimate of
#'   \eqn{\beta}, obtained from maximizing the score function corresponding to
#'   \code{dataArray}.
#' @param h TODO
#'
#' @return TODO
#' @export
makeHplugin <- function(dataArray, betaEst, h) {
    d <- dim(dataArray)[1] - 1
    # First derivative K'(u) of the kernel function K(u), as it is defined in
    # the Cattaneo paper. Note that, in their notation, it is not K(u) that
    # approximates the indicator function, but its antiderivative!
    # We have:
    #   K_n(u) = K(u/h_n)/h_n
    #   K_n'(u) = d/du K_n(u) = K'(u/h_n)/h_n^2
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

# TODO document and refactor with pointIdentifiedCR.

#' Calculate confidence region
#'
#' Generates a confidence region estimate using Cattaneo's bootstrap method.
#'
#' See:
#' https://cattaneo.princeton.edu/papers/Cattaneo-Jansson-Nagasawa_2020_ECMA.pdf
#' https://cattaneo.princeton.edu/papers/Cattaneo-Jansson-Nagasawa_2020_ECMA--Supplement.pdf
#' https://github.com/mdcattaneo/replication-CJN_2020_ECMA
#'
#' @inheritParams pointIdentifiedCR
#' @return TODO
#'
#' @export
newBootstrapCR <- function(
        dataArray, groupIDs, pointEstimate, ssSize, numSubsamples,
        confidenceLevel, optimizeScoreArgs, options = NULL) {
    defaultOptions <- list(
        progressUpdate = 0, confidenceLevel = 0.95, centered = FALSE,
        Hest = "plugin", bw = 1)
    if (is.null(options)) {
        options <- list()
    }
    for (name in names(defaultOptions)) {
        if (is.null(options[[name]])) {
            options[[name]] <- defaultOptions[[name]]
        }
    }
    progress  <- options$progressUpdate

    alpha <- 1 - confidenceLevel
    numFreeAttrs <- dim(dataArray)[1] - 1
    pointEstimate <- as.numeric(pointEstimate)

    # As in the Cattaneo paper, we implement two methods for estimating the
    # matrix H_0: one using numerical differentiation one, and one using a
    # plug-in estimator with a kernel function. Both methods require a
    # parameter; the numerical differentiation method calls it ε_n, and the
    # plug-in method calls it h_n. However, since they are calculated in a
    # similar way, we will call them both bw (for bandwidth). This can be given
    # as a scalar value, but in the general case it can be a matrix, of same
    # size as H. In that case, the general value of the bandwidth in formulas
    # involving it is replaced by the corresponding entry of that matrix.
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
        bw <- rot(y, x, k, pointEstimate)[[bwOpts$method]]
    }
    # If bw is a scalar, replace it with a matrix with the same elements.
    if (is.atomic(bw) && length(bw) == 1) {
        bw <- matrix(bw, numFreeAttrs, numFreeAttrs)
    }
    H <- bwOpts$fn(dataArray, pointEstimate, bw)
    # TODO warn user if H has many zeros, or too large entries
    # TODO in the documentation for H, state that an entry-wise different value
    # for ε might be appropriate in the ND case if the covariates have
    # different scale
    print(bw)
    print(H)

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
    calcEstimate <- function(iterIdx) {
        sample <- sampleBootstrap(groupIDs, dataArray) # TODO `sample` shadows stdlib function
        optimizeBootstrapArgs = list(
            fullDataArray = dataArray, sampleDataArray = sample$ssDataArray,
            betaEst = pointEstimate, H = H,
            bounds = optimizeScoreArgs$bounds,
            coefficient1 = optimizeScoreArgs$coefficient1,
            method = optimizeScoreArgs$method,
            optimParams = optimizeScoreArgs$optimParams
        )
        optResult <- do.call(optimizeBootstrapFunction, optimizeBootstrapArgs)
        ssEstimate <- optResult$optArg
        # The <<- operator is required to modify objects outside the closure.
        samples[iterIdx, ] <<- sample$selectedGroups
        if (progress > 0 && iterIdx %% progress == 0) {
            cat(sprintf("[pointIdentifiedCR] Iterations completed: %d\n", iterIdx))
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
        cr <- t(t(cr$cr) + pointEstimate)
    }

    result <- list(
        cr = cr,
        estimates = t(estimates), rawEstimates = t(rawEstimates),
        samples = samples)
    return(result)
}
