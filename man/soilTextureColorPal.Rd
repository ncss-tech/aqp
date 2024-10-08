% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color-palettes.R
\name{soilTextureColorPal}
\alias{soilTextureColorPal}
\title{Soil Texture Color Palettes}
\usage{
soilTextureColorPal(simplify = FALSE, schema = "soilweb")
}
\arguments{
\item{simplify}{logical, return the base 12 (\code{TRUE}) or full 21 (\code{FALSE}) soil texture classes}

\item{schema}{select mapping between soil texture classes, and colors, currently limited to 'soilweb'}
}
\value{
\code{data.frame} from soil texture class codes and colors
}
\description{
Suggested color palettes for USDA soil texture classes, ranked according to average plant-available water holding capacity. The default color mapping schema is based on a palette used by SoilWeb applications.
}
\examples{
 
# base 12 soil texture classes
# ranked by plant available water-holding capacity
d <- soilTextureColorPal(simplify = TRUE)
soilPalette(d$color, lab = d$class, lab.cex = 1)

# full 21 soil texture classes
# ranked by plant available water-holding capacity
d <- soilTextureColorPal(simplify = FALSE)
soilPalette(d$color, lab = d$class, lab.cex = 1)

}
\author{
D.E. Beaudette, Mike Walkinshaw, A.T. O'Geen
}
