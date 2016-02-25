\name{soilColorSignature}
\alias{soilColorSignature}

\title{Soil Profile Color Signatures}
\description{Generate a color signature for each soil profile in a collection.}
\usage{
soilColorSignature(spc, r = "r", g = "g", b = "b", 
method='colorBucket', RescaleLightnessBy = 1, useProportions=TRUE,
pigmentNames=c('.white.pigment', '.red.pigment', '.green.pigment', 
'.yellow.pigment', '.blue.pigment'))
}

\arguments{
  \item{spc}{a \code{SoilProfileCollection} object}
  \item{r}{horizon level attribute containing soil color (RGB) red values}
  \item{g}{horizon level attribute containing soil color (RGB) green values}
  \item{b}{horizon level attribute containing soil color (RGB) blue values}
  \item{method}{algorithm used to compute color signature, currently only 'colorBucket' available}
  \item{RescaleLightnessBy}{rescaling factor for CIE LAB L-coordinate}
  \item{useProportions}{use proportions or quantities, see details}
  \item{pigmentNames}{names for resulting pigment proportions or quantities}
  
}

\details{details pending...}

\value{a \code{data.frame} object containing:
\describe{
  \item{id column}{set according to \code{idname(spc)}}
  \item{.white.pigment}{proportion or quantity of CIE LAB L-values}
  \item{.red.pigment}{proportion or quantity of CIE LAB positive A-values}
  \item{.green.pigment}{proportion or quantity of CIE LAB negative A-values}
  \item{.yellow.pigment}{proportion or quantity of CIE LAB positive B-values}
  \item{.blue.pigment}{proportion or quantity of CIE LAB negative B-values}
}

Column names can be adjusted with the \code{pigmentNames} argument.
}

\references{https://en.wikipedia.org/wiki/Lab_color_space}
\author{D.E. Beaudette}

\seealso{\code{\link{munsell2rgb}}}
\examples{
# trivial example, not very interesting
data(sp1)
depths(sp1) <- id ~ top + bottom

# convert Munsell -> RGB triplets
rgb.data <- munsell2rgb(sp1$hue, sp1$value, sp1$chroma, return_triplets = TRUE)
sp1$r <- rgb.data$r
sp1$g <- rgb.data$g
sp1$b <- rgb.data$b

# extract color signature
pig <- soilColorSignature(sp1)
}

\keyword{ manip }

