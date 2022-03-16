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
#' @return An array with the columns of \code{dataArray} belonging to the
#'   selected subset of markets.
#' @keywords internal
generateRandomSubsample <- function(ssSize, groupIDs, dataArray) {
    uniqueGroups <- unique(groupIDs)
    selectedGroups <- sort(sample(uniqueGroups, ssSize))
    # Get the indices of the dataArray columns that correspond to the selected
    # groups.
    qualifiedIndices <- which(groupIDs %in% selectedGroups)
    return(dataArray[, qualifiedIndices])
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
#'     dimension \code{(numSubsamples, numFreeAttrs)}.
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
    calcEstimate <- function(iterIdx) {
        ssDataArray <- generateRandomSubsample(ssSize, groupIDs, dataArray)
        optimizeScoreArgs$dataArray <- ssDataArray
        optResult <- do.call(optimizeScoreFunction, optimizeScoreArgs)
        ssEstimate <- optResult$optArg
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

    result <- list(crSymm = crSymm, crAsym = crAsym, estimates = t(estimates))
    return(result)
}

#' Plots confidence region estimates
#'
#' A convenience function that creates a plot of each parameter estimate.
#' @param estimates See the output of \code{pointIdentifiedCR}.
#' @export
plotCR <- function(estimates) {
    estimates <- estimates
    plot(c(col(estimates)), c(estimates), type="p", col="blue", xlab="", ylab="")
    graphics::abline(h=0, v=0)
}
