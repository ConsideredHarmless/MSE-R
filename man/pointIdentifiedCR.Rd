% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confidence.R
\name{pointIdentifiedCR}
\alias{pointIdentifiedCR}
\title{\loadmathjax
Calculate confidence region}
\usage{
pointIdentifiedCR(
  dataArray,
  groupIDs,
  pointEstimate,
  ssSize,
  numSubsamples,
  confidenceLevel,
  optimizeScoreArgs,
  options = NULL
)
}
\arguments{
\item{dataArray}{The output of \code{dataArray}.}

\item{groupIDs}{The output of \code{makeGroupIDs}.}

\item{pointEstimate}{The vector \mjseqn{\hat{\beta}} of the optimal
parameters, as calculated by \code{optimizeScoreFunction}.}

\item{ssSize}{The size of the market subset used in subsampling. Must not be
greater than the number of markets.}

\item{numSubsamples}{The number of subsamples.}

\item{confidenceLevel}{The confidence level of the region. Must be a number
in the range \eqn{(0, 1)}.}

\item{optimizeScoreArgs}{A list with the keyword arguments to be used when
\code{optimizeScoreFunction} is called. All non-optional arguments should
be present, except \code{dataArray}.}

\item{options}{A list of options:
\tabular{ll}{
\code{progressUpdate}  \tab How often to print progress. Defaults to
\code{0} (never). \cr
\code{asymptotics}     \tab Type of asymptotics to use. Supported values
are "nests" (default) or "coalitions".
}}
}
\value{
A list with members:
\tabular{ll}{
\code{$crSymm}    \tab The confidence regions of each parameter for the
symmetric case, as an array of dimension \code{(2, numFreeAttrs)}, where
\code{numFreeAttrs} is the total number of attributes minus 1. \cr
\code{$crAsym}    \tab Same as above, for the asymmetric case. \cr
\code{$estimates} \tab The estimates for each parameter, as an array of
dimension \code{(numSubsamples, numFreeAttrs)}. \cr
\code{$samples} \tab The market subsets chosen at each iteration, as an
array of dimension \code{(numSubsamples, ssSize)}, containing their
respective indices.
}
}
\description{
Generates a confidence region estimate using subsampling.
}
\details{
The method used is the \strong{point-identified} one. For the cube-root method,
see the function \code{\link{cubeRootBootstrapCR}}, which has the same signature.
}
\seealso{
\code{\link[=cubeRootBootstrapCR]{cubeRootBootstrapCR()}} for the cube-root method.

The estimates are calculated by running the score optimization procedure for
many different randomly selected subsets of markets.

This function calls \code{optimizeScoreFunction}, therefore we require a list
with arguments to be passed to it. The argument \code{optimizeScoreArgs}
should contain those arguments contained in a list, except \code{dataArray},
such that when \code{dataArray} is assigned to \code{optimizeScoreArgs}, the
call \code{do.call(optimizeScoreFunction, optimizeScoreArgs)} is valid.
}
