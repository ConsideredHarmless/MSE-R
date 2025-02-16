% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{importUnmatched}
\alias{importUnmatched}
\title{Import unmatched market data}
\usage{
importUnmatched(filenameUp, filenameDn, fieldMode = "position")
}
\arguments{
\item{filenameUp, filenameDn}{Absolute or relative path to the files for the
upstream and the downstream data respectively. See also the parameters to
\code{\link[data.table]{fread}}.}

\item{fieldMode}{A string denoting how to identify the fields of the file.
Options are:
\tabular{ll}{
\code{"position"} \tab (default) Identify each field based on its order
in the field list. \cr
\code{"name"} \tab Identify each field based on its name.
}
See the section "File structure" for more information.}
}
\value{
A list with members:
\tabular{ll}{
\code{$headerUp}, \code{$headerDn} \tab Character vectors of the headers
of the tables. \cr
\code{$noM}                \tab The number of markets.\cr
\code{$noU}, \code{$noD}   \tab Vectors of size \code{$noM}, whose
\code{m}-th element is the number of upstreams and downstreams
respectively in the \code{m}-th market.\cr
\code{$noAttr}             \tab The number of distance attributes.\cr
\code{$colIdxsUp}, \code{$colIdxsDn} \tab TODO \cr
\code{$attributeMatricesUp}, \code{$attributeMatricesDn}
\tab A list of arrays of upstream and downstream
attribute values respectively, one for each market. See the appropriate
section for their definition.\cr
\code{$quotasUp}, \code{$quotasDn}
\tab A list of vectors of quotas for the
upstreams and downstreams respectively.\cr
}
}
\description{
Reads CSV files containing data for unmatched markets.
}
\section{File structure}{


Each file must be a delimiter-separated file with a header.

If \code{fieldMode} is \code{"name"}, the file must contain the following
fields:
\tabular{ll}{
\code{Market} \tab The market index.\cr
\code{UpStream} or \code{DownStream} \tab The stream index.\cr
\code{Quota} \tab The quota for that stream.
}
It must also contain at least one field with a name starting with
\code{Attribute}. These fields contain attribute values. The order
they appear in, and not their full name, specifies their actual order.

If \code{fieldMode} is \code{"position"}, then the fields mentioned above
are identified by the order they appear in the header, and not their names.
The first two fields correspond to \code{Market} and \code{UpStream} or
\code{DownStream} respectively, and the last field corresponds to
\code{Quota}. All other fields are considered to be attribute fields.

Indices should have consecutive values, starting from \code{1}. Distance
attribute values should be numerical.

Each row should correspond to a unique triple of market, upstream, and
downstream indices.
}

\section{Attribute matrix structure}{

Let \code{mIdx} index a market. Each \code{attributeMatrix} is an array
of dimension \code{(noAttr, noS[mIdx])}, where \code{noS[mIdx]} is the size
of the stream (up- or down-). The element indexed by \code{[i, sIdx]} gives
the \code{i}-th attribute value for the pair \code{(mIdx, sIdx)}.
}

