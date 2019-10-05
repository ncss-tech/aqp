\name{contrastChart}
\alias{contrastChart}

\title{Color Contrast Chart}
\description{Compare one or more pages from a simulated Munsell book of soil colors to a reference color.}
\usage{
contrastChart(m, hues)
}

\arguments{
  \item{m}{Munsell representation of a single color for comparison e.g. '10YR 4/3'}
  \item{hues}{vector of one or more Munsell hue pages to display}
}

\details{A simulated Munsell color book page or pages are used to demonstrate color contrast between all chips and the refnerece color \code{m} (highlighted in red). NCSS color contrast class and CIE delta-E00 values are printed below all other color chips.}

\value{A \code{trellis} graphics object.}

\author{D.E. Beaudette}

\examples{
# single hue page
contrastChart(m = '10YR 3/3', hues = '10YR')

# multiple hue pages
contrastChart(m = '10YR 3/3', hues = c('10YR', '2.5Y'))
}

\keyword{ hplots }
