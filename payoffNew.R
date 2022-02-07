# Payoff matrices
#
# In the current version, we only consider linear payoff functions of the form:
#   f_beta(t) = D_{t,1}*beta_1 + D_{t,2}*beta_2 + ... + D_{t,n}*beta_n
# where
#   beta is the vector of parameters (see below).
#   t is the index triple (mIdx, uIdx, dIdx).
#   D_{t,k} is the k-th distance attribute for the triple (mIdx, uIdx, dIdx).
#   n is the number of attributes.
# Since functions of this form are invariant with respect to the transformation
#   f_beta -> (1/s)*f_{s*beta},
# we can fix beta_1 equal to 1 and define
#   beta'_k = beta_{k+1}.
# This is the convention used here, and also in the optimization functions. See
# the files objectiveNew.R and maximizeNew.R.
#
# For a given market indexed by mIdx, we can consider the array of payoff
# functions where the parameter vector has not yet been assigned. Since we are
# interested in the linear case, this is equivalent to a three-dimensional
# array indexed by the upstream index, the downstream index, and the attribute
# index, whose values are D_{t,k}, for all relevant triples t. This is, in fact,
# the array distanceMatrices[[mIdx]] described in importNew. In the general
# case, we would have a two-dimensional array of symbolic expressions instead.
#
# To evaluate such an array for a given parameter vector, we can simply perform
# a matrix-vector product, by flattening and unflattening the dimensions
# corresponding to the upstream and downstream indexes. This is performed by the
# function evaluatePayoffMatrix.

# evaluatePayoffMatrix(unevalPayoffMatrix, beta) evaluates the payoff functions
# for a given market.
# unevalPayoffMatrix is an array of dimension (noAttr, noD, noU). See importNew
# for more information. Note that in this case it is not literally a matrix.
# beta is a vector of length noAttr-1, containing the parameter values for
# indexes 2 through noAttr.
# Returns an array of dimension (noD, noU). Its element indexed by [dIdx, uIdx]
# gives the value of the payoff function for that downstream-upstream pair.
evaluatePayoffMatrix <- function(unevalPayoffMatrix, beta) {
    dims <- dim(unevalPayoffMatrix)
    noAttr <- dims[1]
    noU <- dims[2]
    noD <- dims[3]
    u <- c(1, beta)
    d <- array(unevalPayoffMatrix, c(noAttr, noU*noD))
    v <- u %*% d
    w <- array(v, c(noU, noD))
    return(w)
}

# As above, but for all markets. A convenience function.
evaluatePayoffMatrices <- function(unevalPayoffMatrices, beta) {
    return(lapply(unevalPayoffMatrices, function(p) { evaluatePayoffMatrix(p, beta) }))
}

