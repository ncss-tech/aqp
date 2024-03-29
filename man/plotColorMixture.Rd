% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotColorMixture.R
\name{plotColorMixture}
\alias{plotColorMixture}
\title{Visualize Spectral Mixing of Munsell Colors}
\usage{
plotColorMixture(
  x,
  w = rep(1, times = length(x))/length(x),
  mixingMethod = c("exact", "reference"),
  n = 1,
  swatch.cex = 1.5,
  label.cex = 0.85,
  showMixedSpec = FALSE,
  overlapFix = TRUE
)
}
\arguments{
\item{x}{vector of colors in Munsell notation, should not contain duplicates}

\item{w}{vector of weights, can sum to any number}

\item{mixingMethod}{approach used to simulate a mixture:
\itemize{
\item \code{exact}: simulate a subtractive mixture of pigments, color conversion via CIE1931 color-matching functions (see \code{\link[=mixMunsell]{mixMunsell()}})
\item \code{reference}  : simulate a subtractive mixture of pigments, selecting \code{n} closest reference spectra, requires \code{gower} package
}}

\item{n}{number of closest mixture candidates when \code{mixingMethod = 'reference'} (see \code{\link[=mixMunsell]{mixMunsell()}}), results can be hard to interpret when \code{n > 2}}

\item{swatch.cex}{scaling factor for color swatch rectangle width and height, relative to \code{label.cex}, typically between 1 and 3}

\item{label.cex}{scaling factor for swatch labels}

\item{showMixedSpec}{show weighted geometric mean (mixed) spectra as dotted line (only when \code{mixingMethod = 'reference'})}

\item{overlapFix}{attempt to "fix" overlapping chip labels via \code{\link[=fixOverlap]{fixOverlap()}}, using \code{method = 'E'}}
}
\value{
a \code{lattice} graphics object
}
\description{
Lattice visualization demonstrating subtractive mixtures of colors in Munsell notation and associated spectra.
}
\details{
If present, \code{names} attribute of \code{x} is used for the figure legend. See the \href{https://ncss-tech.github.io/AQP/aqp/mix-colors.html}{expanded tutorial} for examples.
}
\seealso{
\code{\link[=mixMunsell]{mixMunsell()}}
}
\author{
D.E. Beaudette
}
