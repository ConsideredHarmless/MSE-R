makeObjFun <- function(dataArray, coefficient1 = 1) {
    # The negative of the actual objective function, since the DE library can
    # only minimize.
    objFun <- function(b) {
        u <- t(dataArray) %*% c(coefficient1, b)
        return(-sum(u > 0))
    }
    return(objFun)
}
