#' Calculate maximum score
#'
#' Optimizes the maximum score function defined by the data array. The objective
#' function is created using \code{makeObjFun}.
#'
#' @section Optimization methods:
#' TODO
#' See \code{DEoptim::DEoptim.control}.
#'
#' @param dataArray The output of \code{CdataArray}.
#' @param bounds A list with elements \code{$lower}, \code{$upper} which are
#'   vectors defining lower and upper bounds for each variable in the objective
#'   function.
#' @param coefficient1 (optional) The first coefficient of the extended
#'   parameter vector. See \code{makeObjFun} for more details.
#' @param method (optional) A string denoting the optimization method.
#'   Currently, only \code{"DEoptim"} (the default value) is supported.
#' @param optimParams (optional) A list of parameters to be passed to the
#'   optimization routine. Defaults to the empty list (TODO see \code{do.call}).
#' @param getIneqSat (optional) A boolean indicating whether to include the
#'   \code{$ineqSat} member in the result. Defaults to \code{FALSE}.
#' @param permuteInvariant (optional) TODO. Defaults to \code{FALSE}.
#'
#' @return A list with members:
#' \tabular{ll}{
#'   \code{$optVal}  \tab The optimal value of the objective function.\cr
#'   \code{$optArg}  \tab The argument vector which achieves that value.\cr
#'   \code{$ineqSat} \tab A vector of \code{0}s and \code{1}s. Each element
#'     corresponds to a single inequality, and is \code{1} if that inequality is
#'     satisfied for the optimal parameters, and \code{0} otherwise. Only
#'     present if \code{getIneqSat} is \code{TRUE}.
#' }
#' @export
optimizeScoreFunction <- function(
        dataArray, bounds,
        coefficient1 = NULL, method = NULL, optimParams = NULL,
        getIneqSat = FALSE, permuteInvariant = FALSE) {
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
            result <- maximizeDEoptim(objFun, bounds$lower, bounds$upper, optimParams)
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

# TODO unname?
maximizeDEoptim <- function(objective, lower, upper, control) {
    outDEoptim <- DEoptim::DEoptim(objective, lower, upper, control = control)
    optArg <-  outDEoptim$optim$bestmem
    optVal <- -outDEoptim$optim$bestval
    return(list(optVal = optVal, optArg = optArg))
}

# makeBounds(3, 10) -> list(lower = c(-10, -10), upper = c(10, 10))
#' TODO
#' @param numAttrs TODO
#' @param b TODO
#' @return TODO
#' @export
makeBounds <- function(numAttrs, b) {
    stopifnot(numAttrs >= 2)
    n <- numAttrs - 1
    upper <- rep(b, n)
    return(list(lower = -upper, upper = upper))
}

#' TODO
#' @param ineqSat TODO
#' @param groupIDs TODO
#' @return TODO
#' @export
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
