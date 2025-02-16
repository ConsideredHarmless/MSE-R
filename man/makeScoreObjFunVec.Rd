% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/objective.R
\name{makeScoreObjFunVec}
\alias{makeScoreObjFunVec}
\title{Create vector-valued score function}
\usage{
makeScoreObjFunVec(dataArray, coefficient1 = 1, objSign = -1)
}
\arguments{
\item{dataArray}{See the result of the function \code{CdataArray}.}

\item{coefficient1}{The first coefficient in the extended vector of
parameters. Should typically be \code{1} (the default) or \code{-1}.}

\item{objSign}{Use \code{-1} (the default) when passing the result to an
optimization routine that can only minimize, such as
\code{DEoptim::DEoptim}. Otherwise, use \code{1}.}
}
\value{
A function taking a vector of length \code{noAttr-1} (the parameters)
and returning a vector of integers (the scores for each market).
}
\description{
Creates a vector-valued score function from the given data array. The purpose
of this function is not to be passed to an optimization routine, but to track
which inequalities were satisfied. The objective function is similar to
\code{\link{makeScoreObjFun}}, except that the sum is not divided by \eqn{n}.
}
