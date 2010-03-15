\name{munsell2rgb}
\Rdversion{1.1}
\alias{munsell2rgb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Munsell Notation to RGB
}
\description{
Color conversion based on a look-up table of common soil colors.
}
\usage{
munsell2rgb(the_hue, the_value, the_chroma, alpha=1, maxColorValue=1, return_triplets=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{the_hue}{a vector of one or more more hues, upper-case}
  \item{the_value}{a vector of one or more values}
  \item{the_chroma}{a vector of one or more chromas}
  \item{alpha}{alpha channel value (for transparency effects)}
  \item{maxColorValue}{maximum RGB color value (see\code{\link{rgb})}}
  	\item{return_triplets}{should the function return raw RGB triplets instead of an R color}
}
\details{This function generalizes to vectorized usage, as long as the lenght of each argument is the same.}
\value{
A vector of R colors is returned that is the same length as the input data. If \code{return_triplets} is \code{TRUE}, then a dataframe (of sample length as input) of r,g,b values is returned.
}
\references{
http://casoilresource.lawr.ucdavis.edu/drupal/node/201
}
\author{
Dylan E. Beaudette
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(aqp)
data(sp1)

# convert colors
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

# simple plot, may need to tweak gamma-correction...
image(matrix(1:nrow(sp1)), axes=FALSE, col=sp1$soil_color, main='Soil Colors')

# convert into a more useful color space
require(colorspace)

# keep RGB triplets from conversion
sp1.rgb <- with(sp1, munsell2rgb(hue, value, chroma, return_triplets=TRUE))

# convert into LAB color space
sp1.lab <- as(with(sp1.rgb, RGB(r,g,b)), 'LAB')
plot(sp1.lab)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}