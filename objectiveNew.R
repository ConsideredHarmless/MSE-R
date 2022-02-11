# Creates an objective function suitable for passing to an optimization routine.
# This function computes the score (number of inequalities satisfied) for a
# given parameter vector (beta).
#
# The function created is
#   g(beta) = objSign * sum_{k=1}^{N_ineqs} (a_k * beta'), where
#   beta is the vector of free parameters, of length noAttr-1.
#   beta' = [coefficient1 beta].
#   a_k is the k-th column of dataArray.
#   objSign can be 1 or -1. Use -1 when passing the result to an optimization
#   routine that can only minimize, such as DEoptim::DEoptim.
#
# For dataArray, see the CdataArray function.
makeObjFun <- function(dataArray, coefficient1 = 1, objSign = -1) {
    objFun <- function(b) {
        u <- t(dataArray) %*% c(coefficient1, b)
        return(objSign * sum(u >= 0))
    }
    return(objFun)
}

# Creates a vector-valued objective function, similar to the corresponding call
# to makeObjFun. If we call these functions h and g respectively, then we have:
#   sum_{k=1}^{N_ineqs} h(beta)_k = g(beta), for all beta.
#
# The purpose of this function is not to be passed to an optimization routine,
# but to track which inequalities were satisfied.
makeObjFunVec <- function(dataArray, coefficient1 = 1, objSign = -1) {
    objFunVec <- function(b) {
        u <- t(dataArray) %*% c(coefficient1, b)
        return(objSign * as.vector(u >= 0))
    }
    return(objFunVec)
}
