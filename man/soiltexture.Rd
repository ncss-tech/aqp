\name{soiltexture}
\alias{soiltexture}
\docType{data}

\title{
Lookup table of sand, silt, clay, and texture class.
}

\description{
This table is a snapshot generated using the results of TT.points.in.classes from the soiltexture package.
}

\usage{data("soiltexture")}

\format{
  A data frame with 5151 observations on the following 4 variables.
  \describe{
    \item{\code{clay}}{clay percentage of the fine earth fraction, a integer vector}
    \item{\code{sand}}{sand percentage of the fine earth fraction, a integer vector}
    \item{\code{silt}}{silt percentage of the fine earth fraction, a integer vector}
    \item{\code{texcl}}{texture class, a character vector}
  }
}

\details{
The detail of how this lookup table was generated can be found in the ssc_to_texcl() code.
}

\examples{
\donttest{
data(soiltexture)
}}
\keyword{datasets}
