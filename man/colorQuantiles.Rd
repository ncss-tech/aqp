\name{colorQuantiles}
\alias{colorQuantiles}
\alias{plotColorQuantiles}

\title{Soil Color Range via Quantiles}

\description{Estimate central tendency and spread of soil color using marginal quantiles and L1 median of CIELAB coordinates.}

\usage{
colorQuantiles(soilColors, p = c(0.05, 0.5, 0.95))
plotColorQuantiles(res, title='', mar=c(2,1.5,1,1))
}

\arguments{
  \item{soilColors}{vector of R colors (sRGB colorspace)}
  \item{p}{marginal quantiles of interest}
  
  \item{res}{list returned by \code{colorQuantiles}}
  \item{title}{optional title printed above color quantile figure}
  \item{mar}{customized margins, see details}
}

\details{
Colors are converted from sRGB to CIELAB (D65 illuminant), marginal quantiles of L,A,B coordinates are estimated, and L1 median {L,A,B} is estimates. The closest Munsell chips (via Musell/CIELAB lookup table provided by \code{munsell}) and R colors are determined by locating chips closest to the marginal quantiles and L1 median.

The results can be conveniently inspected using \code{plotColorQuantiles}.
}

\value{A List containing the following elements:

\item{marginal}{data.frame containing marginal quantiles in CIELAB (D65), closest colors and Munsell chips}
\item{L1}{L1 median CIELAB (D65) values, closest color and Munsell chip}

}


\author{D.E. Beaudette}

\note{
This is still rather experimental.
}



\examples{
\dontrun{
# example data, see manual page for details
data(sp5)

# slice top 25 cm
s <- slice(sp5, 1:25 ~ .)

# check some of the data
par(mar=c(0,0,0,0))
plot(sample(s, 25), divide.hz=FALSE, name='', print.id=FALSE, width=0.5)

# colors
previewColors(unique(s$soil_color))

# compute marginal quantiles and L1 median
cq <- colorQuantiles(s$soil_color)

# simple graphical display of results
plotColorQuantiles(cq)
}
}

\keyword{ manip }

