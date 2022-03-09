# makeGroupIDs(ineqmembers) creates the initial group IDs.
# For the structure ineqmembers, see Cineqmembers.
# Returns a vector containing noU[mIdx] repetitions of the value mIdx, for each
# mIdx in the range 1:noM.
#' TODO
#' @param ineqmembers TODO
#' @return TODO
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
generateRandomSubsample <- function(ssSize, groupIDs, dataArray) {
    uniqueGroups <- unique(groupIDs)
    selectedGroups <- sort(sample(uniqueGroups, ssSize))
    # Get the indices of the dataArray columns that correspond to the selected
    # groups.
    qualifiedIndices <- which(groupIDs %in% selectedGroups)
    return(dataArray[, qualifiedIndices])
}

# TODO update docs
# pointIdentifiedCR(...) generates a confidence region estimate using
# subsampling.
#
# Parameters:
# ssSize          The size of each subsample to be estimated.
# numSubsamples   The number of subsamples to use in estimating the confidence
#                 region.
# pointEstimate   The point estimate to build the confidence region around
#                 (typically the output of pairwiseMSE).
# numFreeAttrs    Should be equal to noAttr-1.
# groupIDs        A data map used to generate the subsamples. See makeGroupIDs.
# dataArray       The dataArray parameter used in pairwiseMSE.
# optimParams     The parameters passed to the maximize function.
# options         An optional parameter specifying options. Available options are:
#   progressUpdate      How often to print progress (0 to disable). Default=0.
#   confidenceLevel     The confidence level of the region. Default=.95.
#   asymptotics         Type of asymptotics to use (nests or coalitions). Default=nests.
#
# Returns a list with elements:
#   crSymm      The confidence regions of each parameter for the symmetric case,
#               as an array of dimension (2, numFreeAttrs).
#   crAsym      Same as above, for the asymmetric case.
#   estimates   The estimates for each parameters, as an array of dimension
#               (numFreeAttrs, numSubsamples).

#' TODO
#' @param ssSize TODO
#' @param numSubsamples TODO
#' @param pointEstimate TODO
#' @param numFreeAttrs TODO
#' @param groupIDs TODO
#' @param dataArray TODO
#' @param optimizeScoreArgs TODO
#' @param options TODO
#' @return TODO
#' @export
pointIdentifiedCR <- function(ssSize, numSubsamples, pointEstimate,
                                 numFreeAttrs, groupIDs, dataArray,
                                 optimizeScoreArgs, options=list()) {
    defaultOptions <- list(progressUpdate=0, confidenceLevel=0.95,
                           asymptotics="nests")
    for (name in names(defaultOptions)) {
        if (is.null(options[[name]])) {
            options[[name]] <- defaultOptions[[name]]
        }
    }
    progress  <- options[["progressUpdate"]]
    confLevel <- options[["confidenceLevel"]]
    asymp     <- options[["asymptotics"]]

    alpha <- 1 - confLevel

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
    # TODO transpose
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

    result <- list(crSymm = crSymm, crAsym = crAsym, estimates = estimates)
    return(result)
}

#' TODO
#' @param estimates TODO
#' @return TODO
#' @export
plotCR <- function(estimates) {
    estimates <- t(estimates)
    plot(c(col(estimates)), c(estimates), type="p", col="blue", xlab="", ylab="")
    graphics::abline(h=0, v=0)
}
