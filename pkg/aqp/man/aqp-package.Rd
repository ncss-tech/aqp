\name{aqp-package}
\alias{aqp-package}
\alias{aqp}
\docType{package}
\title{
Algorithms for Quantitative Pedology
}
\description{
The aqp (Algorithms for Quantitative Pedology) package for R was developed to address some of the difficulties associated with processing soils information, specifically related to visualization, aggregation, and classification of soil profile data. This package is based on a mix of S3/S4 functions and classes, and most functions use basic dataframes as input, where rows represent soil horizons and columns define properties of those horizons. Common to most functions are the requirements that horizon boundaries are defined as depth from 0, and that profiles are uniquely defined by an id column. The aqp package defines an S4 class, "SoilProfileCollection", for storage of profile-level metadata, as well as summary, print, and plotting methods that have been customized for common tasks related to soils data.

Demo:
\code{demo(aqp)}

Extended Examples:
\url{http://casoilresource.lawr.ucdavis.edu/drupal/taxonomy/term/56}
}


\author{Dylan E. Beaudette <debeaudette@ucdavis.edu>}
\references{http://casoilresource.lawr.ucdavis.edu/}
\keyword{ package }
\seealso{\link{ca630}, \code{\link{sp1}, \link{sp2}, \link{sp3}, \link{sp4}, \link{sp5}}}

