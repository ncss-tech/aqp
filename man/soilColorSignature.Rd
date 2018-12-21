\name{soilColorSignature}
\alias{soilColorSignature}

\title{Soil Profile Color Signatures}
\description{Generate a color signature for each soil profile in a collection.}
\usage{
soilColorSignature(spc, r = "r", g = "g", b = "b", 
method='colorBucket', pam.k=3, RescaleLightnessBy = 1, useProportions=TRUE,
pigmentNames=c('.white.pigment', '.red.pigment', '.green.pigment', 
'.yellow.pigment', '.blue.pigment'))
}

\arguments{
  \item{spc}{a \code{SoilProfileCollection} object}
  \item{r}{horizon level attribute containing soil color (sRGB) red values}
  \item{g}{horizon level attribute containing soil color (sRGB) green values}
  \item{b}{horizon level attribute containing soil color (sRGB) blue values}
  \item{method}{algorithm used to compute color signature, `colorBucket`, `depthSlices`, or `pam`}
  \item{pam.k}{number of classes to request from \code{cluster::pam()}}
  \item{RescaleLightnessBy}{rescaling factor for CIE LAB L-coordinate}
  \item{useProportions}{use proportions or quantities, see details}
  \item{pigmentNames}{names for resulting pigment proportions or quantities}
  
}

\details{\href{http://ncss-tech.github.io/AQP/aqp/soil-color-signatures.html}{http://ncss-tech.github.io/AQP/aqp/soil-color-signatures.html}}

\value{For the `colorBucket` method, a \code{data.frame} object containing:
\describe{
  \item{id column}{set according to \code{idname(spc)}}
  \item{.white.pigment}{proportion or quantity of CIE LAB L-values}
  \item{.red.pigment}{proportion or quantity of CIE LAB positive A-values}
  \item{.green.pigment}{proportion or quantity of CIE LAB negative A-values}
  \item{.yellow.pigment}{proportion or quantity of CIE LAB positive B-values}
  \item{.blue.pigment}{proportion or quantity of CIE LAB negative B-values}
}

Column names can be adjusted with the \code{pigmentNames} argument.

For the `depthSlices` method ...

For the `pam` method ...
}

\references{https://en.wikipedia.org/wiki/Lab_color_space}
\author{D.E. Beaudette}

\seealso{\code{\link{munsell2rgb}}}
\examples{
# trivial example, not very interesting
data(sp1)
depths(sp1) <- id ~ top + bottom

# convert Munsell -> sRGB triplets
rgb.data <- munsell2rgb(sp1$hue, sp1$value, sp1$chroma, return_triplets = TRUE)
sp1$r <- rgb.data$r
sp1$g <- rgb.data$g
sp1$b <- rgb.data$b

# extract color signature
pig <- soilColorSignature(sp1)
}

\keyword{ manip }

