\name{colorContrastPlot}
\alias{colorContrastPlot}

\title{Color Contrast Plot}
\description{A simple display of two sets of colors, NCSS color contrast class and CIE delta-E00.}

\usage{
colorContrastPlot(m1, m2, col.cex = 1, col.font = 2, d.cex = 1, 
cc.font = 3, dE00.font = 1, labels = c("m1", "m2"), 
label.cex = 1, label.font = 1, printMetrics = TRUE, 
...)
}

\arguments{
  \item{m1}{first set of Munsell colors for comparison (e.g. '5YR 3/2')}
  \item{m2}{second set of Munsell colors for comparison}
  \item{col.cex}{scaling factor for color labels}
  \item{col.font}{font for color labels}
  \item{d.cex}{contrast for contrast metric labels}
  \item{cc.font}{font for contrast class}
  \item{dE00.font}{font for delta-E00}
  \item{labels}{labels for compared colors, vector length 2}
  \item{label.cex}{scaling factor for labels}
  \item{label.font}{font for labels}
  \item{printMetrics}{logical, print metrics between color swatches}
  \item{\dots}{further arguments to \code{colorspace::swatchplot}}
}

\details{This function requires the \code{farver} package for calculation of CIE delta-E00}


\author{D.E. Beaudette}


\seealso{ \code{\link{colorContrast}} }
\examples{
# two sets of colors to compare
m1 <- c('10YR 6/3', '7.5YR 3/3', '10YR 2/2', '7.5YR 3/4')
m2 <- c('5YR 3/4', '7.5YR 4/4', '2.5YR 2/2', '7.5YR 6/3')

# contrast metrics
colorContrast(m1, m2)

# graphical display 
colorContrastPlot(m1, m2)
}

\keyword{ hplots }

