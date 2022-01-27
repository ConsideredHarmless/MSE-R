maximizeNew <- function(objective, params) {
    # set.seed(params$RandomSeed)
    control <- params[c("NP", "itermax", "trace", "reltol", "CR", "F")]
    outDEoptim <- DEoptim(objective, params$lower, params$upper, control = control)
    bestmem <-  outDEoptim$optim$bestmem
    bestval <- -outDEoptim$optim$bestval
    return(list(bestmem = bestmem, bestval = bestval))
}

# TODO docs
# makeBounds(3, 10) -> list(lower = c(-10, -10), upper = c(10, 10))
makeBounds <- function(numAttrs, b) {
    stopifnot(numAttrs >= 2)
    n <- numAttrs - 1
    upper <- rep(b, n)
    return(list(lower = -upper, upper = upper))
}
