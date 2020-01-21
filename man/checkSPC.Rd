\name{checkSPC}
\alias{checkSPC}

\title{Test for a valid SoilProfileCollection}
\description{Test for a valid SoilProfileCollection}

\usage{
checkSPC(x)
}

\arguments{
  \item{x}{a \code{SoilProfileCollection} object}
}

\details{Test for valid \code{SoilProfileCollection} by checking for slots defined in the class prototype. Likely only used between major versions of `aqp` where internal structure of \code{SoilProfileCollection} has changed. Use \code{checkHzDepthLogic} to check for common errors in horizon depths.}

\value{TRUE or FALSE. Consider using \code{rebuildSPC()} if FALSE.}

\author{D.E. Beaudette}

\seealso{
\code{\link{rebuildSPC}}, \code{\link{checkHzDepthLogic}}
}
