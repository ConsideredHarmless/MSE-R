% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{extractAttributeMatrices}
\alias{extractAttributeMatrices}
\title{Extract attributed matrices from imported table}
\usage{
extractAttributeMatrices(marketData)
}
\arguments{
\item{marketData}{The return value of \code{importUnmatchedMain}.}
}
\value{
A list of attribute matrices.
}
\description{
Extract attributed matrices from imported table
}
\section{Attribute matrix structure}{

Let \code{mIdx} index a market. Each \code{attributeMatrix} is an array
of dimension \code{(noAttr, noS[mIdx])}, where \code{noS[mIdx]} is the size
of the stream (up- or down-). The element indexed by \code{[i, sIdx]} gives
the \code{i}-th attribute value for the pair \code{(mIdx, sIdx)}.
}

\section{Dimension ordering}{

Note the unexpected ordering of the dimensions. We use this convention in
this package because R is a column-major language.
}

\keyword{internal}
