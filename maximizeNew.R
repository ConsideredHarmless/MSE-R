maximizeNew <- function(objective, params) {
    set.seed(params$RandomSeed)
    control <- params[c("NP", "itermax", "trace", "reltol", "CR", "F")]
    outDEoptim <- DEoptim(objective, params$lower, params$upper, control = control)
    bestmem <-  outDEoptim$optim$bestmem
    bestval <- -outDEoptim$optim$bestval
    return(list(bestmem = bestmem, bestval = bestval))
}
