# generateAssignmentMatrix(payoffMatrix, quotaU, quotaD) calculates the optimal
# matching for a given market by solving the binary ILP:
#       max     \sum_{i=1}^{N_U} \sum_{j=1}^{N_D} P_{i,j}*X_{i,j}
#     X \in S
#       s.t.    \sum_{j=1}^{N_D} X_{i,j} <= Q_{U,i}, 1 <= i <= N_U
#               \sum_{i=1}^{N_U} X_{i,j} <= Q_{D,j}, 1 <= j <= N_D
# where
#   N_U and N_D are the numbers of upstreams and downstreams for this market.
#   S = {0,1}^{N_U \times N_D}.
#   X_{i,j} is equal to 1 if upstream i and downstream j match, and 0 otherwise.
#   P_{i,j} is the value of the payoff function for the matching above.
#   Q_U and Q_D are vectors of quotas for the upstreams and downstreams;
#   upstream i can have no more than Q_{U,i} matches total, and similarly for
#   Q_{D,j}.
# By flattening the two dimensions of X and P, we arrive at a binary ILP
# formulation.
#
# For payoffMatrix, see the payoffNew.R file.
# quotaU and quotaD must be vectors of length noU and noD respectively,
# containing non-negative integers. Alternatively, either can be a non-negative
# integer. In this case, the quota vector is this value, repeated the
# appropriate times.
#
# Returns an array of dimension (noD, noU). Its element indexed by [dIdx, uIdx]
# is 1 if that downstream-upstream pair matches, and 0 otherwise.
generateAssignmentMatrix <- function(payoffMatrix, quotaU, quotaD) {
    numU <- dim(payoffMatrix)[1]
    numD <- dim(payoffMatrix)[2]
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
    # Let x = vec(X^T)[1] the flattened variables. We will convert the
    # constraints to the form M*x <= b, where M is a matrix, b is a vector, and
    # the inequality is entrywise.
    # We construct M and b from three blocks:
    # m1 and m2 correspond to the constraints above. m3 constrains the values of
    # the variables in the set {0,1}.
    # [1] The transposition is because R uses column-major order. The
    # correspondence is:
    #   X_{i,j} == x_{numD*(i-1)+j}.

    # m1 has the form
    # [1 1 ... 1                        ]
    # [          1 1 ... 1              ]
    # [                    ...          ]
    # [                        1 1 ... 1]
    # where omitted values are 0, the blocks of 1s have length numD, and there
    # are numU rows.
    m1 <- matrix(
        as.vector(outer(cbind(rep(1, numD)), data.matrix(diag(numU)))),
        nrow = numU, ncol = numD*numU, byrow=TRUE)
    # m2 has the form
    # [I_numU I_numU ... I_numU]
    # where I_n is the n x n identity matrix, and there are numD such blocks.
    m2 <- matrix(outer(diag(numD), rep(1, numU)), nrow = numD, ncol = numD*numU)
    m3 <- diag(numU*numD)
    m <- rbind(m1, m2, m3)
    b <- c(quotaU, quotaD, rep(1, numU*numD))

    # Reminder that payoffMatrix is indexed as [dIdx, uIdx]. No transposition is
    # needed here.
    f.obj <- as.vector(payoffMatrix)
    f.con <- m
    f.dir <- rep("<=", numU + numD + numU*numD)
    f.rhs <- b
    result <- lp("max", f.obj, f.con, f.dir, f.rhs)
    x <- result$solution

    # Round to integer values and restore the dimensions.
    return(array(round(x), c(numD, numU)))
}
