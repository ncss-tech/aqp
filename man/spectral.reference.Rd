% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-documentation.R
\docType{data}
\name{spectral.reference}
\alias{spectral.reference}
\title{D65 standard illuminant and CIE1931 color matching functions}
\format{
An object of class \code{data.frame} with 71 rows and 5 columns.
}
\usage{
data(spectral.reference)
}
\description{
D65 standard illuminant and CIE1931 color matching functions
}
\examples{

data("spectral.reference")

matplot(
  x = spectral.reference[, 1],
  y = spectral.reference[, 3:5],
  type = 'l',
  lwd = 2, 
  lty = 1,
  las = 1,
  xlab = 'Wavelength (nm)',
  ylab = 'Weight | Intensity',
  main = 'CIE1931 Standard Observer Weights\nD65 Standard Illuminant'
)

lines(
  x = spectral.reference$w,
  y = spectral.reference$D65 / 100,
  lty = 2
)

legend(
  'topright',
  legend = c('X', 'Y', 'Z', 'D65'),
  col = c(1, 2, 3, 1),
  lwd = c(2, 2, 2, 1),
  lty = c(1, 1, 1, 2),
  bty = 'n'
)

}
\references{
Marcus, R.T. (1998). The Measurement of Color. In K. Nassau (Ed.), Color for Science, Art, and Technology (pp. 32-96). North-Holland.

"Selected colorimetric tables in Excel" http://files.cie.co.at/204.xls
}
\keyword{datasets}