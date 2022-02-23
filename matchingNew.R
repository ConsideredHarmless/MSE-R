# TODO move to Roxygen documentation and fix LaTeX
#
# generateAssignmentMatrix(payoffMatrix, quotaU, quotaD) calculates the optimal
# matching for a given market by solving the binary ILP:
#       max     \sum_{i=1}^{N_D} \sum_{j=1}^{N_U} P_{i,j}*X_{i,j}
#     X \in S
#       s.t.    \sum_{j=1}^{N_U} X_{i,j} <= q_{D,i}, 1 <= i <= N_D
#               \sum_{i=1}^{N_D} X_{i,j} <= q_{U,j}, 1 <= j <= N_U
# where
#   N_U and N_D are the numbers of upstreams and downstreams for this market.
#   S = {0,1}^{N_D \times N_U}.
#   X_{i,j} is equal to 1 if upstream j and downstream i match, and 0 otherwise.
#   P_{i,j} is the value of the payoff function for the matching above.
#   q_U and q_D are vectors of quotas for the upstreams and downstreams;
#   upstream j can have no more than q_{U,j} matches total, and similarly for
#   q_{D,i}.
# By flattening the two dimensions of X and P, we arrive at a binary ILP
# formulation.
#
# Note that we follow the convention used in the R code, and the first dimension
# of the matrices indexes the downstreams. We use i to index downstreams, and j
# to index upstreams.
#
# Define now x = vec(X) and p = vec(P), where vec flattens the matrices into
# vectors of length N_D*N_U by concatenating columns. Observe that the two
# inequalities can be written as:
# \sum_{j=1}^{N_U} X_{i,j} <= q_{D,i} <=>
# \sum_{j=1}^{N_U} X_{i,j} 1_{N_U \times 1}_j <= q_{D,i} <=>
# (X 1_{N_U \times 1})_i <= q_{D,i}
# and
# \sum_{i=1}^{N_D} X_{i,j} <= q_{U,j} <=>
# \sum_{i=1}^{N_D} X^T_{j,i} 1_{N_D \times 1}_i <= q_{U,j} <=>
# (X^T 1_{N_D \times 1})_j <= q_{U,j} <=>
# where 1_{m \times n} is the m \times n matrix containing all ones.
#
# The inequalities can now be written as:
# X 1_{N_U \times 1} <= q_D
# X^T 1_{N_D \times 1} <= q_U
# where <= is interpreted element-wise.
#
# The identity connecting the matrix products in the two vector spaces is:
# vec(A B C) = (C^T \otimes A) vec(B)
#
# Since for a vector x we have x = vec(x) = vec(x^T), we have:
# I_{N_D} X 1_{N_U \times 1} <= q_D <=>
# vec(I_{N_D} X 1_{N_U \times 1}) <= vec(q_D) <=>
# (1_{1 \times N_U} \otimes I_{N_D}) x <= q_D
# and
# I_{N_U} X^T 1_{N_D \times 1} <= q_U <=>
# vec(1_{1 \times N_D} X I_{N_U}) <= vec(q_U^T) <=>
# (I_{N_U} \otimes 1_{1 \times N_D}) x <= q_U
#
# We can now write the vectorized formulation of the ILP:
#   max     p^T x
#    x
#   s.t.    M_D x <= q_D
#           M_U x <= q_U
#
# where M_D and M_U are defined above.

#' Generate assignment matrix
#'
#' Finds the optimal matching in a market for the given payoffs and quotas.
#'
#' @param payoffMatrix An array of payoffs for all upstream-downstream pairs in
#' this market. Note that the first dimension indexes the downstreams. See the
#' payoffNew.R file for more details.
#' @param quotaU A vector of length \code{numU}, containing integers in the
#' range \code{0:numD}. Its \code{j}-th element is the maximum allowed number of
#' matches for the corresponding upstream. Alternatively, it can be a single
#' non-negative integer. In this case, the quota vector is this value, repeated
#' the appropriate number of times.
#' @param quotaD As \code{quotaU}, but for downstreams.
#'
#' @return An array of dimension \code{(numD, numU)}. Its element indexed by \code{[dIdx, uIdx]}
#' is \code{1} if that downstream-upstream pair matches, and \code{0} otherwise.
#' @export
#'
#' @examples
generateAssignmentMatrix <- function(payoffMatrix, quotaU, quotaD) {
    numD <- dim(payoffMatrix)[1]
    numU <- dim(payoffMatrix)[2]
    if (length(quotaU) == 1) {
        quotaU <- rep(quotaU, numU)
    }
    if (length(quotaD) == 1) {
        quotaD <- rep(quotaD, numD)
    }
    if (length(quotaU) != numU) {
        error("generateAssignmentMatrix: upstream quota vector has incorrect size")
    }
    if (length(quotaD) != numD) {
        error("generateAssignmentMatrix: downstream quota vector has incorrect size")
    }

    # Constraints
    # Let x = vec(X) the flattened variables. We will convert the constraints to
    # the form M*x <= b, where M is a matrix, b is a vector, and the inequality
    # is element-wise.
    # We construct M and b from three blocks:
    # M_D and M_U correspond to the constraints above. M_S constrains the values
    # of the variables in the set {0,1}.
    onesD = t(matrix(rep(1, numD)))
    onesU = t(matrix(rep(1, numU)))
    idD = diag(numD)
    idU = diag(numU)
    mD <- kronecker(onesU, idD)
    mU <- kronecker(idU, onesD)
    mS <- diag(numD*numU)
    m <- rbind(mD, mU, mS)
    b <- c(quotaD, quotaU, rep(1, numD*numU))

    # Reminder that payoffMatrix is indexed as [dIdx, uIdx]. No transposition is
    # needed here.
    f.obj <- as.vector(payoffMatrix)
    f.con <- m
    f.dir <- rep("<=", numD + numU + numD*numU)
    f.rhs <- b
    result <- lp("max", f.obj, f.con, f.dir, f.rhs)
    x <- result$solution

    # Round to integer values and restore the dimensions.
    return(array(round(x), c(numD, numU)))
}

# CmatchMatrices(payoffMatrices, quotasU, quotasD) creates the structure
# matchMatrices (see importNew).
# TODO docs
# quotasU and quotasD are lists.
CmatchMatrices <- function(payoffMatrices, quotasU, quotasD) {
    return(mapply(generateAssignmentMatrix, payoffMatrices, quotasU, quotasD,
                  SIMPLIFY = FALSE))
}

# TODO create conversion functions: matchMatrices, mate, mates (also rename)
