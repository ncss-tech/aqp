\name{argillic.clay.increase.depth}
\alias{argillic.clay.increase.depth}
\title{
argillic.clay.increase.depth
}
\description{
Returns the top and bottom depth of the argillic horizon as a numeric vector. Uses \code{crit.clay.argillic} to determine threshold clay increase, and get.increase.matrix to determine where depth is met within a vertical distance of 30 cm.
# 
}
\usage{
argillic.clay.increase.depth(p, attr = "clay")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
A single-profile SoilProfileCllection object.
}
  \item{attr}{
OPTIONAL: horizon attribute name referring to clay content. default: `clay`
}
}
\value{
A numeric vector containing top and bottom depth of argillic horizon, if present, or NA.
}
\author{
Andrew Gene Brown 
}
\seealso{
\code{getArgillicBounds}, \code{get.increase.matrix}, \code{crit.clay.argillic}
}
\examples{
}
\keyword{ manip }
