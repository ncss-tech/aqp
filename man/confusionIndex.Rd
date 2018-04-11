\name{confusionIndex}
\alias{confusionIndex}


\title{Confusion Index}
\description{Calculate the confusion index of Burrough et al., 1997.}

\usage{
confusionIndex(x)
}

\arguments{
  \item{x}{vector of probabilities [0,1], should not contain NA}
}


\value{A single numeric value.}

\references{
Burrough, P.A., P.F.M. van Gaans, and R. Hootsmans. 1997. "Continuous Classification in Soil Survey: Spatial Correlation, Confusion and Boundaries." Geoderma 77: 115-35. doi:10.1016/S0016-7061(97)00018-9.
}

\author{D.E. Beaudette}

\examples{

# a very simple example
p <- c(0.25, 0.25, 0.4, 0.05, 0.05)

confusionIndex(p)

}

\keyword{ manip }

