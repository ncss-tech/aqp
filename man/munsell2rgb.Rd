\name{munsell2rgb}
\Rdversion{1.1}
\alias{munsell2rgb}
\alias{rgb2munsell}
\alias{parseMunsell}
\alias{getClosestMunsellChip}
\title{
Convert Munsell Notation to and from sRGB color coordinates
}
\description{
Color conversion based on a look-up table of common soil colors.
}

\usage{
munsell2rgb(the_hue, the_value, the_chroma, alpha=1, 
maxColorValue=1, return_triplets=FALSE, returnLAB=FALSE)

rgb2munsell(color, colorSpace = c('CIE2000', 'LAB', 'sRGB'), nClosest=1)

parseMunsell(munsellColor, convertColors=TRUE, ...)

getClosestMunsellChip(munsellColor, convertColors=TRUE, ...)
}

\arguments{
  \item{the_hue}{a vector of one or more more hues, upper-case}
  \item{the_value}{a vector of one or more values}
  \item{the_chroma}{a vector of one or more chromas, may be NA for neutral hues}
  \item{alpha}{alpha channel value (for transparency effects)}
  \item{maxColorValue}{maximum sRGB color value (see \code{\link{rgb})}}
  \item{return_triplets}{should the function return sRGB triplets instead of an R color}
  \item{returnLAB}{should the function return CIE LAB (D65) coordinates}
  \item{color}{a \code{data.frame} or \code{matrix} object containing sRGB coordinates in the range of [0,1]}
  \item{colorSpace}{the distance metric (colorspace) to use for finding the closest chip: CIE2000 is the most accurate but requires farver >= 2.0.3, Euclidean distance in (CIE)LAB is a close second, while Euclidean distance in sRGB is not at all accurate and should only be used for demonstration}
  \item{nClosest}{number of closest Munsell colors to return}
  \item{munsellColor}{character vector of strings containing Musell colors, e.g. '10YR 4/3'}
  \item{convertColors}{logical, should parsed Munsell colors be converted into sRGB values}
  \item{\dots}{further arguments to \code{munsell2rgb}}
}

\details{These functions generalize to vectorized usage, as long as the length of each argument is the same. Both functions will pad output with NA if there are any NA present in the inputs.

Neutral hues are approximated by greyscale shades ranging from 20\% (darker) to 80\% (lighter). No chroma is required for neutral hues.

Gley soil colors that are missing a chroma will not be correctly interpreted. Consider using a chroma of 1.

Values of "2.5" (common in soil color descriptions) are silently truncated to "2".

Non-standard Munsell colors (e.g. '7.9YR 2.7/2.0') can be matched (nearest-neighbor, no interpolation) to the closest color within the `munsell` sRGB/CIELAB look-up table via \code{getClosestMunsellChip()}. A more accurate estimate of sRGB values from non-standard notation can be achieved with the \href{https://CRAN.R-project.org/package=munsellinterpol}{munsellinterpol} package.

See examples below.
}

\note{Care should be taken when using the resulting sRGB values; they are close to their Munsell counterparts, but will vary based on your monitor and ambient lighting conditions. Also, the value used for \code{maxColorValue} will affect the brightness of the colors. Th default value (1) will usually give acceptable results, but can be adjusted to force the colors closer to what the user thinks they should look like.}

\value{
For Munsell to sRGB conversion, a vector of R colors is returned that is the same length as the input data. If \code{return_triplets} is \code{TRUE}, then a dataframe (of sample length as input) of r,g,b values is returned.

For sRGB to Munsell conversion, a dataframe (NA-padded) of hue, value, chroma, and Euclidean distance to nearest matching color is returned.
}
\references{
\url{http://ncss-tech.github.io/AQP/}
\url{http://www.brucelindbloom.com/index.html?ColorCalcHelp.html}
\url{http://www.cis.rit.edu/mcsl/online/munsell.php}
\url{http://www.munsellcolourscienceforpainters.com/MunsellAndKubelkaMunkToolbox/MunsellAndKubelkaMunkToolbox.html}
}
\author{D.E. Beaudette}




\examples{

# Munsell to sRGB triplets: 
# function is vectorized as long as arguments are the same length
color <- munsell2rgb(the_hue=c('10YR', '2.5YR'), the_value=c(3, 5), 
the_chroma=c(5, 6), return_triplets=TRUE)

# RGB triplets to closest Munsell color (in sRGB space)
# function is vectorized
rgb2munsell(color)

# neutral heues (N) map to approximate greyscale colors
# chroma may be any number or NA
g <- expand.grid(hue='N', value=2:8, chroma=NA, stringsAsFactors=FALSE)
munsell2rgb(g$hue, g$value, g$chroma)


# basic example
d <- expand.grid(hue='10YR', value=2:8, chroma=1:8, stringsAsFactors=FALSE)
d$color <- with(d, munsell2rgb(hue, value, chroma))

# similar to the 10YR color book page
plot(value ~ chroma, data=d, col=d$color, pch=15, cex=3)

# multiple pages of hue:
hues <- c('2.5YR','5YR','7.5YR','10YR')
d <- expand.grid(hue=hues, value=2:8, chroma=seq(2,8,by=2), stringsAsFactors=FALSE)
# convert Munsell -> sRGB
d$color <- with(d, munsell2rgb(hue, value, chroma))

# extract CIELAB coordinates
with(d, munsell2rgb(hue, value, chroma, returnLAB=TRUE))

# plot: note that we are setting panel order from red-->yellow
library(lattice)
xyplot(value ~ factor(chroma) | factor(hue, levels=hues),
main="Common Soil Colors", layout=c(4,1), scales=list(alternating=1),
strip=strip.custom(bg=grey(0.85)),
data=d, as.table=TRUE, subscripts=TRUE, xlab='Chroma', ylab='Value',
panel=function(x, y, subscripts, ...)
{
panel.xyplot(x, y, pch=15, cex=4, col=d$color[subscripts])
}
)


# soils example
data(sp1)

# convert colors
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

# simple plot, may need to tweak gamma-correction...
image(matrix(1:nrow(sp1)), axes=FALSE, col=sp1$soil_color, main='Soil Colors')

# convert into a more useful color space
# you will need the colorspace package for this to work
if(require(colorspace)) {
# keep RGB triplets from conversion
sp1.rgb <- with(sp1, munsell2rgb(hue, value, chroma, return_triplets=TRUE))

# convert into LAB color space
sp1.lab <- as(with(sp1.rgb, sRGB(r,g,b)), 'LAB')
plot(sp1.lab)
}

# convert a non-standard color to closest "chip" in `munsell` look-up table
getClosestMunsellChip('7.9YR 2.7/2.0', convertColors = FALSE)
# convert directly to R color
getClosestMunsellChip('7.9YR 2.7/2.0')

##
## demonstrate options for returning sRGB and/or CIE LAB
##
# just sRGB
parseMunsell("10YR 3/5", return_triplets=TRUE)

# sRGB + CIELAB (D65 illuminant)
parseMunsell("10YR 3/5", return_triplets=TRUE, returnLAB=TRUE)

# CIELAB only
parseMunsell("10YR 3/5", return_triplets=FALSE, returnLAB=TRUE)
}


\keyword{manip}
