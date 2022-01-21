maximizeNew <- function(objective, params) {
    set.seed(params$RandomSeed)
    # Can we "sublist" somehow?
    control <- list(
        NP      = params$NP,
        itermax = params$itermax,
        trace   = params$trace,
        reltol  = params$reltol,
        CR      = params$CR,
        F       = params$F)
    outDEoptim <- DEoptim(objective, params$lower, params$upper, control = control)
    bestmem <-  outDEoptim$optim$bestmem
    bestval <- -outDEoptim$optim$bestval
    return(list(bestmem = bestmem, bestval = bestval))
}
