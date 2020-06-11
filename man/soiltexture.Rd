\name{soiltexture}
\alias{soiltexture}
\docType{data}

\title{
Lookup tables for sand, silt, clay, texture class, and textural modifers.
}

\description{
A list that contains a snapshot of the values generated using the logic from the particle size estimator calculation in NASIS, the average values per texture class, and average rock fragment values by textural modifer.
}

\usage{data("soiltexture")}

\format{
  A list with 3 data frames. The first named values which contains values for sand, silt and clay by texture class. The second with average values for sand, silt and clay per texture class. The third has fragvoltot low, rv and high values for texmod.
  \describe{
    \item{\code{clay}}{clay percentage of the fine earth fraction, a integer vector}
    \item{\code{sand}}{sand percentage of the fine earth fraction, a integer vector}
    \item{\code{silt}}{silt percentage of the fine earth fraction, a integer vector}
    \item{\code{texcl}}{texture class, a character vector}
    \item{\code{texmod}}{textural modifers, a character vector}
}}

\details{
A list that contains a snapshot of the values generated using the logic from the particle size estimator calculation in NASIS, and the average values per texture class.
}

\examples{
\donttest{
data(soiltexture)
}}
\keyword{datasets}
