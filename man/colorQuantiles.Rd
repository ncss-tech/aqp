% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorQuantiles.R
\name{colorQuantiles}
\alias{colorQuantiles}
\title{Soil Color Range via Quantiles}
\usage{
colorQuantiles(soilColors, p = c(0.05, 0.5, 0.95))
}
\arguments{
\item{soilColors}{vector of R colors (sRGB colorspace)}

\item{p}{marginal quantiles of interest}
}
\value{
A List containing the following elements:
\itemize{
\item \code{marginal}: \code{data.frame} containing marginal quantiles in CIELAB (D65), closest Munsell chips, and dE00
\item \code{L1}: L1 median CIELAB (D65) values, closest Munsell chip, and dE00
}
}
\description{
Estimate central tendency and spread of soil color using marginal quantiles and L1 median of CIELAB coordinates.
}
\details{
Colors are converted from sRGB to CIELAB (D65 illuminant), marginal quantiles of (L,A,B) coordinates are estimated, and L1 median (L,A,B) is estimates. The closest Munsell chips (via Munsell/CIELAB lookup table provided by \code{munsell}) and R colors are determined by locating chips closest to the marginal quantiles and L1 median.

The results can be conveniently inspected using \code{\link[=plotColorQuantiles]{plotColorQuantiles()}}.
}
\examples{

\dontrun{
# example data, see manual page for details
data(sp5)

# slice top 25 cm
# 24-25cm is the last slice
s <- dice(sp5, 0:24 ~ .)

# check some of the data
par(mar=c(0,0,0,0))
plotSPC(sample(s, 25), divide.hz = FALSE, name = '', print.id = FALSE, width = 0.5)

# colors
previewColors(unique(s$soil_color))

# compute marginal quantiles and L1 median
cq <- colorQuantiles(s$soil_color)

# simple graphical display of results
plotColorQuantiles(cq)
}

}
\author{
D.E. Beaudette
}
