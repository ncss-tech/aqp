% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/huePosition.R
\name{huePosition}
\alias{huePosition}
\title{Munsell Hue Reference and Position Searching}
\usage{
huePosition(
  x,
  returnHues = FALSE,
  includeNeutral = FALSE,
  origin = "5R",
  direction = c("cw", "ccw")
)
}
\arguments{
\item{x}{character vector of hues, e.g. c('10YR', '5YR'), optional if \code{returnHues = TRUE}}

\item{returnHues}{logical, should the full set of Munsell hues be returned? See details.}

\item{includeNeutral}{logical, add 'N' to the end of the full set of Munsell hues}

\item{origin}{hue to be used as the starting point for position searches (position 1)}

\item{direction}{indexing direction, should be \code{cw} (clock wise) or \code{ccw} (counter-clock wise)}
}
\value{
A vector of integer hue positions is returned, of the same
length and order as \code{x}. If \code{returnHues = TRUE}, then all hue
names and ordering are returned and \code{x} is ignored.
}
\description{
The 40 Munsell hues are typically arranged from 5R to 2.5R moving clock wise on the unit circle. This function matches a vector of hues to positions on that circle, with options for setting a custom origin or search direction.

This function is fully vectorized.
}
\examples{

# get hue ordering for setting levels of a factor
huePosition(returnHues = TRUE)

# get hue ordering including N (neutral)
huePosition(returnHues = TRUE, includeNeutral = TRUE)

# get position of the '10YR' hue, relative to standard origin of '5R'
# should be 7
huePosition(x = '10YR')

# get position of the '10YR' hue, relative to standard origin of '5YR'
# should be 3
huePosition(x = '10YR', origin = '5YR')

# visualize
op <- par(mar = c(0, 0, 0, 0), fg = 'white', bg = 'black')

huePositionCircle(huePosition(returnHues = TRUE, origin = '5YR'))

par(op)

}
\references{
\itemize{
\item Soil Survey Technical Note 2 \href{https://web.archive.org/web/20220704214918/https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569}{wayback machine URL}
\item Munsell book of color. 1976. Macbeth, a Division of Kollmorgen Corp., Baltimore, MD.
}
}
\seealso{
\link{colorContrast}, \link{huePositionCircle}
}
\author{
D.E. Beaudette
}
\keyword{manip}
