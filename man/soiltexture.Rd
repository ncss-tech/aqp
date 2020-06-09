\name{soiltexture}
\alias{soiltexture}
\docType{data}

\title{
Lookup tables of sand, silt, clay, and texture class values and averages.
}

\description{
A list that contains a snapshot of the values generated using the logic from the particle size estimator calculation in NASIS, and the average values per texture class.
}

\usage{data("soiltexture")}

\format{
  A list with 2 data frames. The first named values which contains values for sand, silt and clay by texture class. The second with average values for sand, silt and clay per texture class.
  \describe{
    \item{\code{clay}}{clay percentage of the fine earth fraction, a integer vector}
    \item{\code{sand}}{sand percentage of the fine earth fraction, a integer vector}
    \item{\code{silt}}{silt percentage of the fine earth fraction, a integer vector}
    \item{\code{texcl}}{texture class, a character vector}
  }
}

\details{
A list that contains a snapshot of the values generated using the logic from the particle size estimator calculation in NASIS, and the average values per texture class. Texture classes for values that overlap classes were rounded up to the class with more clay.
}

\examples{
\donttest{
data(soiltexture)
}}
\keyword{datasets}
