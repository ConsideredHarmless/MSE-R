# Creates the initial group IDs.
makeGroupIDs <- function(ineqmembers) {
    f <- function(mIdx) {
        return(rep(mIdx, ineqmembers[[mIdx]]$numIneqs))
    }
    mIdxs <- seq_along(ineqmembers)
    return(unlist(lapply(mIdxs, f)))
}

# generateRandomSubsampleNew(ssSize, groupIDs, dataArray) generates a subsample
# of a given size from a data array.
generateRandomSubsampleNew <- function(ssSize, groupIDs, dataArray) {
    uniqueGroups <- unique(groupIDs)
    selectedGroups <- sort(sample(uniqueGroups, ssSize))
    # Get the indexes of the dataArray columns that correspond to the selected
    # groups.
    qualifiedIndexes <- which(groupIDs %in% selectedGroups)
    return(dataArray[, qualifiedIndexes])
}

# TODO docs
# pointIdentifiedCR[ssSize,numSubsamples,pointEstimate,args,groupIDs,dataArray,method,permuteinvariant,options] generates a confidence region estimate using subsampling
# Parameters:
# ssSize - The size of each subsample to be estimated.
# numSubsamples -The number of subsamples to use in estimating the confidence region.
# pointEstimate - The point estimate to build the confidence region around (typically the output of pairwiseMSE).
# objFunc - The objective function used in pairwiseMSE.
#   DEPRECATED args - A list of unique symbols used in pairwiseMSE.
# numFreeAttrs: ...
# groupIDs - A data map used to generate the subsamples.
# dataArray - The dataArray parameter used in pairwiseMSE.
# options - An optional parameter specifying options. Available options are:
# progressUpdate - How often to print progress (0 to disable). Default=0.
# confidenceLevel - The confidence level of the region. Default=.95.
# asymptotics - Type of asymptotics to use (nests or coalitions). Default=nests.
# subsampleMonitor - An expression to evaluate for each subsample. Default=Null.
# symmetric - True or False. If True,the confidence region will be symmetric. Default=False.
pointIdentifiedCRNew <- function(ssSize, numSubsamples, pointEstimate,
                                 numFreeAttrs, groupIDs, dataArray, options,
                                 optimParams) {
    progress  <- options[["progressUpdate"]]
    confLevel <- options[["confidenceLevel"]]
    asymp     <- options[["asymptotics"]]
    sym       <- options[["symmetric"]] # Currently not used.

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
        ssDataArray <- generateRandomSubsampleNew(ssSize, groupIDs, dataArray)
        objective <- makeObjFun(ssDataArray)
        optResult <- maximizeNew(objective, optimParams)
        ssEstimate <- optResult$bestmem
        if (progress > 0 && iterIdx %% progress == 0) {
            cat(sprintf("[pointIdentifiedCRNew] Iterations completed: %d\n", iterIdx))
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
        qSymm <- quantile(abs(estimates[paramIdx, ]), c(1 - alpha, 1 - alpha), names=FALSE, type=1)
        qSymm <- c(-1, 1)*qSymm
        qSymm <- rev(qSymm) / fullNormalization
        return(pointEstimate[paramIdx] - qSymm)
    }
    calcConfRegionAsym <- function(paramIdx) {
        qAsym <- quantile((estimates[paramIdx, ]), c(alpha/2, 1 - alpha/2), names=FALSE, type=1)
        qAsym <- rev(qAsym) / fullNormalization
        return(pointEstimate[paramIdx] - qAsym)
    }
    crSymm <- sapply(1:numFreeAttrs, calcConfRegionSymm)
    crAsym <- sapply(1:numFreeAttrs, calcConfRegionAsym)

    result <- list(crSymm = crSymm, crAsym = crAsym, estimates = estimates)
    return(result)
}

plotCR <- function(estimates) {
    estimates <- t(estimates)
    plot(c(col(estimates)), c(estimates), type="p", col="blue", xlab="", ylab="")
    abline(h=0, v=0)
}
