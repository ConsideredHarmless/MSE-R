# Optimizes the maximum score function defined by dataArray. The objective
# function is created using makeObjFun (see the objective.R) file.
#
# dataArray is the output of CdataArray.
# coefficient1 is the first coefficient of the extended parameter vector. See
#   makeObjFun.
# method is a string denoting an optimization method. Currently, only "DEoptim"
#   (the default value) is supported.
# optimParams is a list of parameters to be passed to the optimization routine.
# getIneqSat is a boolean argument. If it is TRUE, then the result also
#   contains a $ineqSat member (see below). Its default value is FALSE.
# permuteInvariant: TODO
#
# Returns a list with members:
#   $optVal: the optimal value of the objective function.
#   $optArg: the argument vector which achieves that value.
#   $ineqSat: a vector of 0s and 1s. Each element corresponds to a single
#     inequality, and is 1 if that inequality is satisfied for the optimal
#     parameters, and 0 otherwise. Only present if getIneqSat = TRUE.
optimizeScoreFunction <- function(dataArray, coefficient1 = NULL,
                                  method = NULL, optimParams = NULL,
                                  getIneqSat = FALSE,
                                  permuteInvariant = FALSE) {
    if (is.null(method)) {
        method <- "DEoptim"
    }
    objDataArray <- dataArray
    if (permuteInvariant) {
        # TODO docs
        stdDevs <- apply(dataArray, 1, stats::sd)
        perm <- order(stdDevs[-1])
        # Now sort(stdDevs[-1]) == stdDevs[-1][perm].
        objDataArray[-1, ] <- objDataArray[-1, ][perm, ]
        # Now apply(objDataArray, 1, stats::sd)[-1] is sorted.
        invPerm <- rep(0, length(perm))
        for (i in 1:length(perm)) {
            invPerm[perm[i]] <- i
        }
    }
    switch (method,
        "DEoptim" = {
            objSign <- -1
            makeObjFunArgs = list(dataArray = objDataArray, objSign = objSign)
            if (!is.null(coefficient1)) {
                makeObjFunArgs$coefficient1 <- coefficient1
            }
            objFun <- do.call(makeObjFun, makeObjFunArgs)
            result <- maximizeDEoptim(objFun, optimParams)
        },
        stop(sprintf("optimizeScoreFunction: method %s is not implemented",
                      method))
    )
    if (permuteInvariant) {
        # Return parameters with the original order.
        result$optArg <- result$optArg[invPerm]
        makeObjFunArgs$dataArray <- dataArray
    }
    if (getIneqSat) {
        objFunVec <- do.call(makeObjFunVec, makeObjFunArgs)
        result$ineqSat <- objSign * objFunVec(result$optArg)
    }
    return(result)
}

maximizeDEoptim <- function(objective, params) {
    control <- params[c("NP", "itermax", "trace", "reltol", "CR", "F")]
    outDEoptim <- DEoptim::DEoptim(objective, params$lower, params$upper, control = control)
    optArg <-  outDEoptim$optim$bestmem
    optVal <- -outDEoptim$optim$bestval
    return(list(optVal = optVal, optArg = optArg))
}

# TODO docs
# makeBounds(3, 10) -> list(lower = c(-10, -10), upper = c(10, 10))
makeBounds <- function(numAttrs, b) {
    stopifnot(numAttrs >= 2)
    n <- numAttrs - 1
    upper <- rep(b, n)
    return(list(lower = -upper, upper = upper))
}

# TODO docs
calcPerMarketStats <- function(ineqSat, groupIDs) {
    calcRow <- function (mIdx) {
        ineqIdxs <- which(groupIDs == mIdx)
        v <- ineqSat[ineqIdxs]
        numTotIneqs <- length(v)
        numSatIneqs <- sum(v)
        row <- c(mIdx, numTotIneqs, numSatIneqs, numSatIneqs/numTotIneqs)
        return(row)
    }
    mIdxs <- unique(groupIDs)
    numM <- length(mIdxs)
    perMarketStats <- t(sapply(mIdxs, calcRow))
    colnames(perMarketStats) <- c("Market ID", "Total inequalities",
                                  "Satisfied inequalities",
                                  "Satisfied/Total ratio")
    return(perMarketStats)
}
