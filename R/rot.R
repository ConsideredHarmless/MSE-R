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
loglikelihood <- function(y, x, par) {
    d <- dim(x)[1] - 1
    n <- dim(x)[2]
    k <- length(par) - (d + 1)
    beta <- c(1, par[1:d])
    gamma <- par[(d+1):(d+k+1)]
    # The values z_i = x^i^T β.
    z <- as.vector(beta %*% x)
    # The values v_i = s_u(x^i) = \sum_{j=0}^k γ_j z_i^j,
    v <- polyeval(gamma, z)
    # The values w_i = Φ(z_i / v_i). Note that stats::pnorm is vectorized.
    w <- stats::pnorm(z / v)
    # The terms of the sum in the log-likelihood function.
    u <- y*log(w) + (1-y)*log(1-w)
    u[is.nan(u)] <- -Inf
    g <- mean(u)
    # Actually return the negative of the function value, in order to use a
    # miniziming procedure to compute its argmax.
    return(min(-g, 1e3))
}

# TODO document
rot <- function(y, x, k) {
    d <- dim(x)[1] - 1
    n <- dim(x)[2]
    beta0 <- rep(1, d)
    gamma0 <- c(1, rep(0, k))
    par0 <- c(beta0, gamma0)
    optResult <- stats::optim(par0, loglikelihood, y = y, x = x)
    optPars <- optResult$par
    betaR <- optPars[1:d]
    beta <- c(1, betaR)
    gamma <- optPars[(d+1):(d+k+1)]
    # Note that, according to our definitions,
    #   s_u(x_1, x_r)   @ {x_1 = -x_r^T β_r} = γ_0
    #   s'_u(x_1, x_r)  @ {x_1 = -x_r^T β_r} = γ_1
    #   s''_u(x_1, x_r) @ {x_1 = -x_r^T β_r} = 2 γ_2
    # where s'_u(x) = ∂/∂x_1 s_u(x), s''_u(x) = ∂^2/∂x_1^2 s_u(x).
    s0 <- gamma[1]
    s1 <- gamma[2]
    s2 <- 2*gamma[3]
    # For μ_1 and σ_1, we use the sample mean and std.
    mu1 <- mean(x[1, ])
    sigma1 <- stats::sd(x[1, ])
    # The estimates.
    p <- (as.vector(betaR %*% x[2:(d+1), ]) + mu1) / sigma1
    # Note: In the supplement, on pg. 20, Cattaneo has σ_u(x) and σ_u^3(x),
    # where he has defined σ_u^2(x) = s_u(x). However, in his code, he uses
    # σ_u^2(x) for σ_u(x). There seems to be an error, either in the formulas
    # on the supplement, or in the code. I assume it is the latter, and take the
    # square root of s0, s1, and s2. Still, I need to recheck this. Maybe q1 and
    # q2 are incorrect.
    # Note that F_0^{0,1}(x_r) can be easily shown to be equal to
    #   (1/(2*σ_1)) φ((x_r^T β_r + μ_1)/σ_1),
    # where φ is the pdf of the standard normal distribution.
    # FIXME
    q0 <- sqrt(s0)
    q1 <- sqrt(s1)
    q2 <- sqrt(s2)
    F0_1_3 <- -(stats::dnorm(0) / (q0 * sigma1^3)) * stats::dnorm(p) * (p^2 - 1)
    F0_3_1 <- (stats::dnorm(0) / (q0^3 * sigma1)) * stats::dnorm(p) * (1 - q2*q0 + 2*q1^2)
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
    nd.h <- (3*V.nd/4/B.nd^2)^(1/7)*n^(-1/7)
    return(list(bw.nd = nd.h))
}
