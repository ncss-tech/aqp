\name{contrastChart}
\alias{contrastChart}

\title{Color Contrast Chart}
\description{Compare one or more pages from a simulated Munsell book of soil colors to a reference color.}
\usage{
contrastChart(m, hues, ccAbbreviate=1, style='hue')
}

\arguments{
  \item{m}{Munsell representation of a single color for comparison e.g. '10YR 4/3'}
  \item{hues}{vector of one or more Munsell hue pages to display}
  \item{ccAbbreviate}{length of abbreviated contrast classes, use 0 to suppress labels}
  \item{style}{'hue' or 'CC', see details}
}

\details{A simulated Munsell color book page or pages are used to demonstrate color contrast between all chips and the refnerece color \code{m} (highlighted in red). NCSS color contrast class and CIE delta-E00 values are printed below all other color chips. Munsell color chips for chroma 5 and 7 are ommitted, but axis labels are retained as a reminder of this fact.

Setting \code{style='hue'} emphasises the contrast classes and CIE delta-E00 of chips adjacent to \code{m}. Setting \code{style='CC'} emphasises adjacent chips according to respective contrast class via lattice panels.

Two-way panels are used when multiple hues are provided and \code{style='CC'}. The default output can be greatly enhanced via: 

\code{latticeExtra::useOuterStrips(..., 
strip = strip.custom(bg=grey(0.85)), 
strip.left = strip.custom(bg=grey(0.85))
)
}

}

\value{A \code{trellis} graphics object.}

\author{D.E. Beaudette}

\examples{
# single hue page
contrastChart(m = '10YR 3/3', hues = '10YR')

# multiple hue pages
contrastChart(m = '10YR 3/3', hues = c('10YR', '2.5Y'))

# contrast class, single hue
contrastChart(m = '10YR 3/3', hues = '10YR', style='CC')

# contrast class, multiple hues
# consider latticeExtra::useOuterStrips()
contrastChart(m = '10YR 5/6', hues = c('10YR', '2.5Y'), style='CC')
}

\keyword{ hplots }
