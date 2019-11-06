\name{addVolumeFraction}
\alias{addVolumeFraction}
\title{Symbolize Volume Fraction on a Soil Profile Collection Plot}
\description{Symbolize volume fraction on an existing soil profile collection plot.}

\usage{
addVolumeFraction(x, colname, res = 10, cex.min = 0.1, 
cex.max = 0.5, pch = 1, col = "black")
}

\arguments{
  \item{x}{a \code{SoilProfileCollection} object}
  \item{colname}{character vector of length 1, naming the column containing volume fraction data (horizon-level attribute)}
  \item{res}{integer, resolution of the grid used to symbolize volume fraction}
  \item{cex.min}{minimum symbol size}
  \item{cex.max}{maximum symbol size}
  \item{pch}{plotting character}
  \item{col}{symbol color, either a single color or as many colors as there are horizons in `x`}
}

\details{This function can only be called after plotting a \code{SoilProfileCollection} object.}
\note{Details associated with a call to plot.SoilProfileCollection are automatically accounted for within this function: e.g. \code{plot.order}, \code{width}, etc.}

\author{D.E. Beaudette}

\seealso{
\code{\link{plotSPC}}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
