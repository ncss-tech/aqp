\name{contrastClass}
\alias{contrastClass}

\title{Soil Color Contrast}

\description{Determine soil color contrast class according to methods outlined in the Soil Survey Manual. This function is typically called from \code{colorContrast()} which is simpler to use and provides more information.}

\usage{
contrastClass(v1, c1, v2, c2, dH, dV, dC, verbose = FALSE)
}

\arguments{
  \item{v1}{Munsell value of first color}
  \item{c1}{Munsell chroma of first color}
  \item{v2}{Munsell value of second color}
  \item{c2}{Munsell chroma of second color}
  \item{dH}{delta Hue}
  \item{dV}{delta Value}
  \item{dC}{delta Chroma}
  \item{verbose}{return a list for testing rules/cases}
}

\details{This function is fully vectorized but expects all inputs have the same length.}

\value{A vector of color contrast classes (ordered factor). A list when \code{verbose} is TRUE.}

\references{\href{https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569}{Soil Survey Technical Note 2}}

\author{D.E. Beaudette}

\seealso{\code{\link{colorContrast}}}

\examples{
## standard use, result is an ordered factor
# 10YR 6/3 vs 5YR 3/4
contrastClass(v1=6, c1=3, v2=3, c2=4, dH=2, dV=3, dC=1)

## verbose output, useful for testing rules/cases
# 10YR 6/3 vs 5YR 3/4
contrastClass(v1=6, c1=3, v2=3, c2=4, dH=2, dV=3, dC=1, verbose = TRUE)
}

\keyword{ manip }

