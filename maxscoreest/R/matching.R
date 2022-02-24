#' Generate assignment matrix
#'
#' Finds the optimal matching in a market for the given payoffs and quotas.
#'
#' The optimal matching maximizes the total payoff.
#'
#' @param payoffMatrix An array of payoffs for all upstream-downstream pairs in
#'   this market. Note that the first dimension indexes the downstreams. See the
#'   \code{payoff.R} file for more details.
#' @param quotaU A vector of length \code{numU}, containing integers in the
#'   range \code{0:numD}. Its \code{j}-th element is the maximum allowed number
#'   of matches for the corresponding upstream. Alternatively, it can be a
#'   single non-negative integer. In this case, the quota vector is this value,
#'   repeated the appropriate number of times.
#' @param quotaD As \code{quotaU}, but for downstreams.
#'
#' @return An array of dimension \code{(numD, numU)}. Its element indexed by
#'   \code{[dIdx, uIdx]} is \code{1} if that downstream-upstream pair matches,
#'   and \code{0} otherwise.
#' @export
#'
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
        stop("generateAssignmentMatrix: upstream quota vector has incorrect size")
    }
    if (length(quotaD) != numD) {
        stop("generateAssignmentMatrix: downstream quota vector has incorrect size")
    }
    argList <- formulateLP(payoffMatrix, numU, numD, quotaU, quotaD)
    result <- do.call(lpSolve::lp, argList)
    x <- result$solution
    # Round to integer values and restore the dimensions.
    return(array(round(x), c(numD, numU)))
}

#' Formulate linear program for optimal matching
#'
#' For internal use. Generates the arguments passed to \code{lpSolve::lp()} for
#' the given optimal matching problem. See \code{generateAssignmentMatrix} for
#' the parameters.
#'
#' \code{generateAssignmentMatrix(payoffMatrix, quotaU, quotaD)} calculates the
#' optimal matching for a given market by solving the following binary ILP with
#' variable \eqn{X \in S}:
#' \deqn{
#'  \begin{array}{ccc}
#'     \max & \sum_{i=1}^{N_D} \sum_{j=1}^{N_U} P_{i,j} X_{i,j} \\
#'     & \sum_{j=1}^{N_U} X_{i,j} \leq q_{D,i}, & 1 \leq i \leq N_D \\
#'     & \sum_{i=1}^{N_D} X_{i,j} \leq q_{U,j}, & 1 \leq j \leq N_U
#'   \end{array}
#' }
#' where
#' \itemize{
#'   \item \eqn{N_U} and \eqn{N_D} are the numbers of upstreams and downstreams
#'   for this market.
#'   \item \eqn{S = \{0,1\}^{N_D \times N_U}}.
#'   \item \eqn{X_{i,j}} is equal to 1 if upstream \eqn{j} and downstream
#'   \eqn{i} match, and 0 otherwise.
#'   \item \eqn{P_{i,j}} is the value of the payoff function for the matching
#'   above.
#'   \item \eqn{q_U} and \eqn{q_D} are vectors of quotas for the upstreams and
#'   downstreams; upstream \eqn{j} can have no more than \eqn{q_{U,j}} matches
#'   total, and similarly for \eqn{q_{D,i}}.
#' }
#' By flattening the two dimensions of \eqn{X} and \eqn{P}, we arrive at a
#' binary ILP formulation.
#'
#' Note that we follow the convention used in the R code, and the first dimension
#' of the matrices indexes the downstreams. We use \eqn{i} to index downstreams,
#' and \eqn{j} to index upstreams.
#'
#' Define now \eqn{x = \mathcal{V}(X)} and \eqn{p = \mathcal{V}(P)}, where the
#' vectorization operator \eqn{\mathcal{V}} flattens the matrices into vectors
#' of length \eqn{N_D N_U} by concatenating columns. Observe that the
#' inequalities can be written as:
#' \deqn{
#'   \begin{array}{cc}
#'     \sum_{j=1}^{N_U} X_{i,j} \leq q_{D,i} & \Leftrightarrow \\
#'     \sum_{j=1}^{N_U} X_{i,j} (1_{N_U \times 1})_j \leq q_{D,i} & \Leftrightarrow \\
#'     (X 1_{N_U \times 1})_i \leq q_{D,i}
#'   \end{array}
#' }
#' and
#' \deqn{
#'   \begin{array}{cc}
#'     \sum_{i=1}^{N_D} X_{i,j} \leq q_{U,j} & \Leftrightarrow \\
#'     \sum_{i=1}^{N_D} {X^T}_{j,i} (1_{N_D \times 1})_i \leq q_{U,j} & \Leftrightarrow \\
#'     (X^T 1_{N_D \times 1})_j \leq q_{U,j}
#'   \end{array}
#' }
#' where \eqn{1_{m \times n}} is the \eqn{m \times n} matrix containing all ones.
#'
#' The inequalities can now be written as:
#' \deqn{
#'   \begin{array}{c}
#'     X   1_{N_U \times 1} \leq q_D \\
#'     X^T 1_{N_D \times 1} \leq q_U
#'   \end{array}
#' }
#' where \eqn{\leq} is interpreted element-wise.
#'
#' The identity connecting the matrix products in the two vector spaces is:
#' \deqn{\mathcal{V}(A B C) = (C^T \otimes A) \mathcal{V}(B)}
#' where \eqn{\otimes} is the Kronecker product.
#'
#' Since for a vector \eqn{x} we have
#' \eqn{x = \mathcal{V}(x) = \mathcal{V}(x^T)}, we have:
#' \deqn{
#'   \begin{array}{cc}
#'     I_{N_D} X 1_{N_U \times 1} \leq q_D & \Leftrightarrow \\
#'     \mathcal{V}(I_{N_D} X 1_{N_U \times 1}) \leq \mathcal{V}(q_D) & \Leftrightarrow \\
#'     (1_{1 \times N_U} \otimes I_{N_D}) x \leq q_D
#'   \end{array}
#' }
#' and
#' \deqn{
#'   \begin{array}{cc}
#'     I_{N_U} X^T 1_{N_D \times 1} \leq q_U & \Leftrightarrow \\
#'     \mathcal{V}(1_{1 \times N_D} X I_{N_U}) \leq \mathcal{V}(q_U^T) & \Leftrightarrow \\
#'     (I_{N_U} \otimes 1_{1 \times N_D}) x \leq q_U
#'   \end{array}
#' }
#'
#' We can now write the vectorized formulation of the ILP:
#' \deqn{
#'   \begin{array}{cc}
#'     \max & p^T x \\
#'     & M_D x \leq q_D \\
#'     & M_U x \leq q_U
#'   \end{array}
#' }
#' where \eqn{M_D} and \eqn{M_U} are defined above.
#'
#' @return A list of arguments for \code{lpSolve::lp()}. Use with \code{do.call}.
#'
formulateLP <- function(payoffMatrix, numU, numD, quotaU, quotaD) {
    # Constraints
    # Let x = vec(X) the flattened variables. We will convert the constraints to
    # the form M*x <= b, where M is a matrix, b is a vector, and the inequality
    # is element-wise.
    # We construct M and b from three blocks:
    # M_D and M_U correspond to the constraints above. M_S constrains the values
    # of the variables in the set {0,1}.
    # See the function documentation for more information on these values.
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
    argList <- list(
        direction="max",
        objective.in=f.obj,
        const.mat=f.con,
        const.dir=f.dir,
        const.rhs=f.rhs)
    return(argList)
}

# CmatchMatrices(payoffMatrices, quotasU, quotasD) creates the structure
# matchMatrices (see import).
# TODO docs
# quotasU and quotasD are lists.
CmatchMatrices <- function(payoffMatrices, quotasU, quotasD) {
    return(mapply(generateAssignmentMatrix, payoffMatrices, quotasU, quotasD,
                  SIMPLIFY = FALSE))
}

# TODO create conversion functions: matchMatrices, mate, mates (also rename)
