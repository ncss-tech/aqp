\name{rebuildSPC}
\alias{rebuildSPC}

\title{Rebuild a SoilProfileCollection object}
\description{Rebuild a SoilProfileCollection object}

\usage{
rebuildSPC(x)
}

\arguments{
  \item{x}{a \code{SoilProfileCollection} object}
}

\details{Attempt rebuilding a \code{SoilProfileCollection} object by splitting into components and re-assembling. Likely only used to fix outdated \code{SoilProfileCollection} objects that are missing slots.}

\value{A valid \code{SoilProfileCollection} object.}

\author{D.E. Beaudette}

\seealso{
\code{\link{checkSPC}}
}
