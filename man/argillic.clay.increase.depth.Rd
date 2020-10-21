\name{argillic.clay.increase.depth}
\alias{argillic.clay.increase.depth}
\title{
Return upper boundary of argillic horizon
}
\description{
Returns the top depth of the argillic horizon as a numeric vector. 

Uses \code{crit.clay.argillic} to determine threshold clay increase, and \code{get.increase.matrix} to determine where increase is met within a vertical distance of 30 cm.
}
\usage{
argillic.clay.increase.depth(p, clay.attr = "clay")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
A single-profile \code{SoilProfileCollection} object.
}
  \item{clay.attr}{
OPTIONAL: horizon attribute name referring to clay content. default: `clay`
}
}
\value{
A numeric vector containing top depth of argillic horizon, if present, or NA.
}
\author{
Andrew Gene Brown 
}
\seealso{
\code{getArgillicBounds}, \code{get.increase.matrix}, \code{crit.clay.argillic}
}
\examples{
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents 
foo <- argillic.clay.increase.depth(p, clay.attr = attr)
foo
}
\keyword{ manip }
