\name{huePosition}
\alias{huePosition}

\title{Munsell Hue Position for Soil Color Description}
\description{Munsell hues are typically arranged from 5R to 5PB in Munsell soil color books. This function matches a vector of Munsell hues to the position in this arrangement of 29 hues.}

\usage{
huePosition(x, returnHues=FALSE)
}

\arguments{
  \item{x}{character vector of hues, e.g. '10YR'}
  \item{returnHues}{logical, should the unique set of Munsell hues used for ordering be returned? See details.}
}

\details{This function is fully vectorized.}

\value{A vector of integer hue positions is typically returned, of the same length and order as \code{x}. If \code{returnHues} is TRUE, then the hue names and ordering is returned and \code{x} is ignored.
}

\references{
https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569
}

\author{D.E. Beaudette}

\seealso{
\code{\link{colorContrast}}
}

\examples{
# get hue ordering for setting levels of a factor
huePosition(x=NULL, returnHues=TRUE)

# get position of the '10YR' hue (7)
huePosition(x='10YR')
}

\keyword{ manip }
