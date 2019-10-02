\name{colorContrast}
\alias{colorContrast}
\alias{colorContrastPlot}

\title{Metrics of Contrast Suitable for Comparing Soil Colors}
\description{Pair-wise comparisons of Munsell color specifications, based on the NCSS color contrast classes and CIE delta-E 2000 metric.}
\usage{
colorContrast(m1, m2)
}

\arguments{
  \item{m1}{vector of Munsell colors ('10YR 3/3')}
  \item{m2}{vector of Munsell colors ('10YR 3/6')}
}

\details{This function is fully vectorized but expects input to be of the same length. Use \code{expand.grid} to generate suitable input from 1:many or many:1 type comparisons. See \href{http://ncss-tech.github.io/AQP/aqp/color-contrast.html}{this tutorial} for an expanded discussion and more examples.
}

\value{
A \code{data.frame} with the following columns:
  \item{m1}{Munsell color 1}
  \item{m2}{Munsell color 2}
  \item{dH}{delta-hue, as computed by \code{huePosition}}
  \item{dV}{delta-value, absolute value of difference in Munsell value (m1 vs. m2)}
  \item{dc}{delta-chroma, absolute value of difference in Munsell chroma (m1 vs. m2)}
  \item{dE00}{delta-E00, e.g. the \href{https://en.wikipedia.org/wiki/Color_difference#CIEDE2000}{CIE delta-E as refined in 2000}}
  \item{c}{soil color contrast class, as specified in \href{https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569}{Soil Survey Technical Note 2}}

}
\references{https://en.wikipedia.org/wiki/Color_difference}

\author{D.E. Beaudette}

\note{delta-E00 is computed by the \href{https://cran.r-project.org/web/packages/farver/index.html}{farver package}.}


\seealso{
\code{\link{colorContrastPlot}}, \code{\link{huePosition}}
}

\examples{
# two sets of colors to compare
m1 <- c('10YR 6/3', '7.5YR 3/3', '10YR 2/2', '7.5YR 3/4')
m2 <- c('5YR 3/4', '7.5YR 4/4', '2.5YR 2/2', '7.5YR 6/3')

# contrast metrics
colorContrast(m1, m2)

}

\keyword{ manip }
