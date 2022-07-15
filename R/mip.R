# Broadly based on https://github.com/kflorios/maxscore-estimator-mip-inR.

standardizeX <- function(X) {
    colSds <- function(mat) {
        numCols <- dim(mat)[2]
        sds <- sapply(1:numCols, function(j) { sd(mat[, j]) })
        return(sds)
    }
    mu <- colMeans(X)
    sigma <- colSds(X)
    testX <- (X - pracma::repmat(mu, dim(X)[1], 1)) / pracma::repmat(sigma, dim(X)[1], 1)
    p <- dim(X)[2]
    for (j in 1:p) {
        if (is.nan(testX[1, j])) {
            X[, j] <- X[, j]
        }
        else {
            X[, j] <- testX[, j]
        }
    }
    return(list(X = X, mu = mu, sigma = sigma))
}

denormalizeEstimates <- function(betaNorm, mu, sigma) {
    p <- length(betaNorm)
    betaRaw <- numeric(p)
    betaHelp <- numeric(p)
    for (j in 1:p) {
        if (abs(sigma[j]) > 1e-6) {
            betaHelp[j] <- betaNorm[j] / sigma[j]
        } else {
            for (jj in 1:p) {
                if (abs(sigma[j]) > 1e-6) {
                    betaHelp[j] <- betaHelp[j] - betaNorm[jj]*mu[jj]/sigma[jj]
                }
                else {
                    jj0 <- jj
                }
            }
            betaHelp[j] <- betaHelp[j] + betaNorm[jj0]
        }
    }
    for (j in 1:p) {
        betaRaw[j] <- betaHelp[j] / betaHelp[1]
    }
    return(betaRaw)
}

definecAb <- function(X, y, w) {
    n <- dim(X)[1]
    p <- dim(X)[2]
    c1 <- pracma::repmat(-1, 1, n)
    c2 <- pracma::repmat(0, 1, p)
    c <- cbind(c1, c2)
    d <- 10 # I think this is the bound for the continuous variables -- TODO make it a parameter
    M <- numeric(n)
    for (i in 1:n) {
        M[i] <- abs(X[i, 1]) + abs(X[i, -1]) %*% t(pracma::repmat(d, 1, p - 1))
    }
    Abin <- diag(M, n, n)
    Areal <- matrix(0, n, p)
    for (i in 1:n) {
        for (j in 1:p) {
            Areal[i, j] <- (1 - 2*y[i]) * X[i, j]
        }
    }
    A <- cbind(Abin, Areal)
    b <- M
    return(list(c = c, A = A, b = b))
}

definelbub <- function(X) {
    d <- 10
    n <- dim(X)[1]
    p <- dim(X)[2]
    lb1 <- pracma::repmat(0, 1, n)
    lb2 <- pracma::repmat(-d, 1, p)
    lb2[1] <- 1
    lb <- cbind(lb1, lb2)
    ub1 <- pracma::repmat(1, 1, n)
    ub2 <- pracma::repmat(d, 1, p)
    ub2[1] <- 1
    ub <- cbind(ub1, ub2)
    Aeq <- NULL
    beq <- NULL
    best <- 0
    return(list(lb = lb, ub = ub, Aeq = Aeq, beq = beq, n = n, best = best))
}

solvemip <- function(dataArray) {
    Xunstd <- t(dataArray)
    n <- dim(Xunstd)[1]
    p <- dim(Xunstd)[2]
    k <- n + p
    y <- rep(1, n)
    w <- rep(1, n)
    sol <- solvemiphelper(Xunstd, y, w)
    stdized <- sol$stdized
    u <- sol$u
    betaNorm <- u[(n+1):k]
    beta <- denormalizeEstimates(betaNorm, stdized$mu, stdized$sigma)
    return(beta[2:p])
}

solvemiphelper <- function(Xunstd, y, w) {
    n <- dim(Xunstd)[1]
    p <- dim(Xunstd)[2]
    stdized <- standardizeX(Xunstd)
    X <- stdized$X
    cAb <- definecAb(X, y, w)
    lbub <- definelbub(X)
    k <- length(cAb$c)
    # Convert to standardized LP (new variable v := u - lb, constraint v >= 0).
    # This is done because lpSolve can only handle that case.
    Astd <- rbind(cAb$A, diag(k))
    bstd <- c(cAb$b - cAb$A %*% t(lbub$lb), lbub$ub - lbub$lb)
    cstd <- cAb$c
    lpArgs <- list(
        direction = "min",
        objective.in = cstd,
        const.mat = Astd,
        const.dir = rep("<=", k),
        const.rhs = bstd,
        binary.vec = 1:n)
    lpSol <- do.call(lpSolve::lp, lpArgs)
    v <- lpSol$solution
    u <- v + lbub$lb
    return(list(stdized=stdized, lpSol=lpSol, u=u, v=v))
}
