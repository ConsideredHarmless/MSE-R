% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maximize.R
\name{calcPerMarketStats}
\alias{calcPerMarketStats}
\title{Calculate per market statistics}
\usage{
calcPerMarketStats(ineqSat, groupIDs)
}
\arguments{
\item{ineqSat}{The element \code{$ineqSat} from the return value of the
function \code{optimizeScoreFunction}.}

\item{groupIDs}{A vector assigning each inequality to its market. See the
function \code{makeGroupIDs}.}
}
\value{
An array of dimension \code{(noM, 4)}. Its rows correspond to
markets, and its columns are:
\itemize{
\item The market index.
\item The total number of inequalities for that market.
\item The number of inequalities satisfied by the calculated solution.
\item The ratio of the two values above.
}
}
\description{
Calculate per market statistics
}
