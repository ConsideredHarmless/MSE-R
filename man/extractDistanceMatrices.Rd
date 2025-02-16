% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{extractDistanceMatrices}
\alias{extractDistanceMatrices}
\title{Extract distance matrices from imported table}
\usage{
extractDistanceMatrices(marketData)
}
\arguments{
\item{marketData}{The return value of \code{importMatchedMain}.}
}
\value{
A list of distance matrices.
}
\description{
Extract distance matrices from imported table
}
\section{Distance matrix structure}{

Let \code{mIdx} index a market. Each \code{distanceMatrix} is an array
(technically not a matrix) of dimension \code{(noAttr, noD[mIdx],
noU[mIdx])}. The element indexed by \code{[i, dIdx, uIdx]} gives the
\code{i}-th distance attribute value for the triple \code{(mIdx, uIdx,
dIdx)}.
}

\section{Old code}{

To port code using the previous version, replace
\code{distanceMatrices[[m]][[u]][[i]][d]} by
\code{distanceMatrices[[m]][i, d, u]}.
}

\section{Dimension ordering}{

Note the unexpected ordering of the dimensions. We use this convention in
this package because R is a column-major language.
}

\keyword{internal}
