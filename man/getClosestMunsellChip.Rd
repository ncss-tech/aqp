% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getClosestMunsellChip.R
\name{getClosestMunsellChip}
\alias{getClosestMunsellChip}
\title{Get Approximate Munsell Chip}
\usage{
getClosestMunsellChip(munsellColor, convertColors = TRUE, ...)
}
\arguments{
\item{munsellColor}{character vector of strings containing Munsell notation of color, e.g. '10YR 4/3'}

\item{convertColors}{logical, should parsed Munsell colors be converted into sRGB values}

\item{...}{further arguments to \code{munsell2rgb}}
}
\value{
a \code{data.frame} when \code{convertColors=TRUE}, otherwise character vector
}
\description{
Non-standard Munsell notation (e.g. '7.9YR 2.7/2.0') can be matched (nearest-neighbor, no interpolation) to the closest color within the \code{munsell} sRGB/CIELAB look-up table via \code{getClosestMunsellChip()}. A more accurate estimate of sRGB values from non-standard notation can be achieved with the \href{https://CRAN.R-project.org/package=munsellinterpol}{munsellinterpol} package.
}
\examples{

# convert a non-standard color to closest "chip" in `munsell` look-up table
getClosestMunsellChip('7.9YR 2.7/2.0', convertColors = FALSE)
# convert directly to R color
getClosestMunsellChip('7.9YR 2.7/2.0')

}
