\name{repairSPC}
\alias{repairSPC}

\title{Repair a SoilProfileCollection object}
\description{Repair a SoilProfileCollection object}

\usage{
repairSPC(x)
}

\arguments{
  \item{x}{a \code{SoilProfileCollection} object}
}

\details{Attempt repair of a \code{SoilProfileCollection} object by splitting into components and re-assembling. Likely only used to fix outdated \code{SoilProfileCollection} objects that are missing slots.}

\value{A valid \code{SoilProfileCollection} object.}

\author{D.E. Beaudette}

\seealso{
\code{\link{isValidSPC}}
}
