\name{munsell}
\Rdversion{1.1}
\alias{munsell}
\docType{data}
\title{Munsell to sRGB Lookup Table for Common Soil Colors}
\description{A lookup table of interpolated Munsell color chips for common soil colors.}
\usage{data(munsell)}
\format{
  A data frame with 8825 observations on the following 6 variables.
  \describe{
    \item{\code{hue}}{Munsell Hue, upper case}
    \item{\code{value}}{Munsell Value}
    \item{\code{chroma}}{Munsell Chroma}
    \item{\code{r}}{sRGB "red" value (0-1)}
    \item{\code{g}}{sRGB "green" value (0-1)}
    \item{\code{b}}{sRGB "blue" value (0-1)}
  }
}
\details{
See \code{munsell2rgb} for conversion examples. Note that this table does not currently have entires for values of 2.5--common in most soil color books. These chips should be added in the next major release of aqp.
}
\source{
Color chip XYZ values: http://www.rit.edu/cos/colorscience/rc_munsell_renotation.php
}
\references{
	\describe{
		\item{http://www.brucelindbloom.com/index.html?ColorCalcHelp.html}{Color conversion equations}
		\item{http://dx.doi.org/10.1016/j.cageo.2012.10.020}{Methods used to generate this table}
	}
}
\examples{
data(munsell)
}
\keyword{datasets}
