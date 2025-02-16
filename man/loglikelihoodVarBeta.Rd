% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rot.R
\name{loglikelihoodVarBeta}
\alias{loglikelihoodVarBeta}
\title{\loadmathjax
ROT log-likelihood with variable beta}
\usage{
loglikelihoodVarBeta(y, x, par)
}
\arguments{
\item{y}{The vector of observations, of length \mjseqn{n}. This should
always be equal to all ones.}

\item{x}{The array of regressors, also called the \emph{data array} in other
functions. An array of size \mjseqn{(d+1) \times n}, where \mjseqn{d} is
the number of free attributes, and also the length of the vector
\mjseqn{\beta}.}

\item{par}{The concatenated vector of parameters.}
}
\value{
The optimal parameters.
}
\description{
\loadmathjax
ROT log-likelihood with variable beta
}
\keyword{internal}
