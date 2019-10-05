\name{soilPalette}
\alias{soilPalette}

\title{Sol Color Palette}
\description{A very simple function for generating labeled swatches of soil colors. Largely based on \code{colorspace::swatchplot}}

\usage{
soilPalette(colors, lab, lab.cex = 0.75, dynamic.labels=TRUE, ...)
}

\arguments{
  \item{colors}{vector of hex colors (e.g. #A66E46FF)}
  \item{lab}{vector of labels}
  \item{lab.cex}{chatacter scaling for labels}
  \item{dynamic.labels}{logical, adjust label colors based on HSV value of each swatch}
  \item{\dots}{further arguments to \code{colorspace::swatchplot}}
}



\author{D.E. Beaudette}

\note{The result is a simple figure on the active plotting device.}



\examples{
## maybe useful for teaching about soil color

par(mfrow=c(2,1), mar=c(1,1,1,1))

# demonstrate range of Munsell value
m <- sprintf('10YR \%s/4', 2:8)
# convert to hex representation
cols <- parseMunsell(m)
# plot
soilPalette(cols, m)

# demonstrate range of Munsell chroma
m <- sprintf('10YR 4/\%s', 2:8)
# convert to hex representation
cols <- parseMunsell(m)
# plot
soilPalette(cols, m)
}

\keyword{ hplots }

