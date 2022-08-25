# Rule-of-thumb bandwidth for numerical derivative
# Adapted from https://github.com/mdcattaneo/replication-CJN_2020_ECMA.
# See Supplement, A.2.3.2.

# TODO document
polyeval <- function(p, x) {
    s <- 0
    y <- 1
    for (f in p) {
        s <- s + f*y
        y <- y*x
    }
    return(s)
}

loglikelihoodCommon <- function(y, x, beta, gamma) {
    # The values z_i = x^i^T β.
    z <- as.vector(beta %*% x)
    # The values v_i = σ_u(x^i) = (\sum_{j=0}^k γ_j z_i^j)^(1/2).
    v <- sqrt(polyeval(gamma, z))
    # The values w_i = Φ(z_i / v_i). Note that stats::pnorm is vectorized.
    w <- stats::pnorm(z / v)
    # The terms of the sum in the log-likelihood function.
    u <- y*log(w) + (1-y)*log(1-w)
    # The values of β and γ given can sometimes create a negative value for the
    # variance. Since we don't want to consider such cases, we replace this
    # value with -∞, so these parameter values are not considered during
    # maximization.
    # TODO Do this in a more intelligent way, which avoids warnings.
    u[is.nan(u)] <- -Inf
    g <- mean(u)
    # Actually return the negative of the function value, in order to use a
    # minimizing procedure to compute its argmax.
    return(min(-g, 1e3))
}

# The log-likelihood function of the model.
# Note that we define, as per Cattaneo:
#   y = 1{x^T β + u >= 0}, where 1{.} is the indicator function/Iverson bracket,
#   x is a (d+1)-D vector of regressors, β is a (d+1)-D vector of parameters
#   (which Cattaneo calls β_0), with its first element equal to 0, and u is a
#   random variable with conditional distribution:
#   u | x ~ Normal(0, s_u(x)), with
#   s_u(x) = γ^T p(x) = σ_u^2(x)
#   p(x) is "a polynomial expansion", according to Cattaneo, but from inspecting
#     the github code (https://github.com/mdcattaneo/replication-CJN_2020_ECMA/blob/2c1bbea2190936c0697540833b93953a012bf618/main_function_maxscore.R#L72)
#     it seems that it is an expansion of z = x^T β, therefore
#     p(x) = (z^0, z^1, ..., z^k)
#   We also assume that, writing x = (x_1, x_r), with x_r a d-D vector:
#     x_1 | x_r ~ Normal(μ_1, σ_1^2)
#   This is basically a heteroskedastic probit model, since the conditional cdf
#   is
#     F_{u | x}(u | x) = F_{u | x_1, x_r}(u | x_1, x_r) = Φ(u / σ_u(x)),
#   where Φ(z) is the cdf of the standard normal distribution.
# Consider now that we have n pairs of observations (y^i, x^i). Note that in
# our case, y^i = 1 always. Then the probability of y^i = 1 given x = x^i is
#   π_i = P(y = 1 | x = x^i) = P(x^T β + u >= 0 | x = x^i) =
#   F_{u | x}(x^i^T β) = Φ(u / σ_u(x)), because of symmetry of the normal cdf.
# Likewise, the probability of y^i = 0 given x = x^i is
#   1 - π_i = 1 - F_{u | x}(x^i^T β).
# The log-likelihood function can be written as
#   L(β, γ; Y, X) = \sum_{i=1}^n ( y_i log(π_i) + (1-y_i) log(1-π_i) ).
# We then estimate the parameters β and γ using maximum likelihood estimation.
#
# Here, y is a vector of length n, x is an array of dimension (d+1, n), and par
# contains both β and γ; par[1:d] correspond to the d non-unit elements of β,
# and par[d+1:d+k+1] are the elements of γ.
# TODO document
loglikelihoodVarBeta <- function(y, x, par) {
    d <- dim(x)[1] - 1
    n <- dim(x)[2]
    k <- length(par) - (d + 1)
    beta <- c(1, par[1:d])
    gamma <- par[(d+1):(d+k+1)]
    return(loglikelihoodCommon(y, x, beta, gamma))
}

loglikelihoodFixedBeta <- function(y, x, betaEst, par) {
    beta <- c(1, betaEst)
    gamma <- par
    return(loglikelihoodCommon(y, x, beta, gamma))
}

# TODO document
rot <- function(y, x, k, betaEst = NULL) {
    stopifnot(k >= 2)
    d <- dim(x)[1] - 1
    n <- dim(x)[2]
    if (is.null(betaEst)) {
        beta0 <- rep(1, d)
        gamma0 <- c(1, rep(0, k))
        par0 <- c(beta0, gamma0)
        optimArgs <- list(par0, loglikelihoodVarBeta,   y = y, x = x)
    } else {
        gamma0 <- c(1, rep(0, k))
        par0 <- gamma0
        optimArgs <- list(par0, loglikelihoodFixedBeta, y = y, x = x, betaEst = betaEst)
    }
    optResult <- do.call(stats::optim, optimArgs)
    optPars <- optResult$par
    print(optPars)
    betaR <- if (is.null(betaEst)) { optPars[1:d] } else { betaEst }
    gamma <- if (is.null(betaEst)) { optPars[(d+1):(d+k+1)] } else { optPars }
    # For μ_1 and σ_1, we use the sample mean and std.
    mu1 <- mean(x[1, ])
    sigma1 <- stats::sd(x[1, ])
    # The estimates.
    p <- (as.vector(betaR %*% x[2:(d+1), ]) + mu1) / sigma1
    # Note that, according to our definitions,
    #   s_u(x_1, x_r)   @ {x_1 = -x_r^T β_r} = γ_0
    #   s'_u(x_1, x_r)  @ {x_1 = -x_r^T β_r} = γ_1
    #   s''_u(x_1, x_r) @ {x_1 = -x_r^T β_r} = 2 γ_2
    # where s'_u(x) = ∂/∂x_1 s_u(x), s''_u(x) = ∂^2/∂x_1^2 s_u(x).
    # However, in the supplement, on pg. 20, Cattaneo has σ_u(x) and σ_u^3(x),
    # where he has defined σ_u^2(x) = s_u(x), while in his code he uses
    # σ_u^2(x) for σ_u(x).
    # After communicating with the authors, it turns out that this is indeed an
    # inconsistency. We recover the correct values of
    # q_j = ∂^j/∂x_1^j σ_u(x) @ {x_1 = -x_r^T β_r}, which are:
    #   * q_0 = γ_0^(1/2)
    #   * q_1 = 1/2 γ_0^(-1/2) γ_1
    #   * q_2 = -1/4 γ_0^(-3/2) γ_1^2 + γ_0^(-1/2) γ_2
    # I still haven't verified these with Mathematica, but I have triple-checked
    # my calculations.
    # Note that F_0^{0,1}(x_r) can be easily shown to be equal to
    #   (1/(2*σ_1)) φ((x_r^T β_r + μ_1)/σ_1),
    # where φ is the pdf of the standard normal distribution.
    gamma_0 <- gamma[1]
    gamma_1 <- gamma[2]
    gamma_2 <- gamma[3]
    q_0 <- gamma_0^(1/2)
    q_1 <- (1/2)*gamma_0^(-1/2)*gamma_1
    q_2 <- -(1/4)*gamma_0^(-3/2)*gamma_1^2 + gamma_0^(-1/2)*gamma_2
    # q_0 <- sqrt(gamma_0)
    # q_1 <- sqrt(gamma_1)
    # q_2 <- sqrt(gamma_2)
    F0_1_3 <- -(stats::dnorm(0) / (q_0 * sigma1^3)) * stats::dnorm(p) * (p^2 - 1)
    F0_3_1 <-  (stats::dnorm(0) / (q_0^3 * sigma1)) * stats::dnorm(p) * (1 - q_2*q_0 + 2*q_1^2)
    F0_0_1 <- (1 / (2*sigma1)) * stats::dnorm(p)
    # See makeH function in confidence.R.
    makeBndElt <- function(idx1d) {
        col <- (idx1d - 1) %/% d + 1
        row <- (idx1d - 1) %%  d + 1
        x_row <- as.vector(x[row + 1, ])
        x_col <- as.vector(x[col + 1, ])
        element <- -mean((F0_1_3 + F0_3_1/3)*(x_row*x_col)*(x_row^2 + x_col^2))
    }
    makeVndElt <- function(idx1d) {
        col <- (idx1d - 1) %/% d + 1
        row <- (idx1d - 1) %%  d + 1
        x_row <- as.vector(x[row + 1, ])
        x_col <- as.vector(x[col + 1, ])
        f <- 2*abs(x_row) + 2*abs(x_col) - abs(x_row + x_col) - abs(x_row - x_col)
        element <- (1/8)*mean(f * F0_0_1)
    }
    B.nd <- matrix(sapply(1:(d*d), makeBndElt), d, d)
    V.nd <- matrix(sapply(1:(d*d), makeVndElt), d, d)
    print(B.nd)
    print(V.nd)
    nd.h <- (3*V.nd/4/B.nd^2)^(1/7)*n^(-1/7)
    return(list(bw.nd = nd.h))
}
