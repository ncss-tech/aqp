\name{get.increase.depths}
\alias{get.increase.depths}
\title{
Return the horizon top depths from a call to get.increase.matrix()
}
\description{
\code{get.increase.depths} performs the conversion of the square matrix output of \code{get.increase.matrix} back to horizon top depth for where criteria were met.

Note that the `threshold.fun` result is allowed to contain NA, but that will result in no output for affected cells.
}
\usage{
get.increase.depths(p, attr, threshold.fun, vertical.distance)
}
\arguments{
  \item{p}{
  a SoilProfileCollection, containing a single profile 
}
  \item{attr}{
  horizon attribute name to get the "increase" of
}
  \item{threshold.fun}{
  a function that returns the threshold (as a function of attr); may return a constant single value
}
  \item{vertical.distance}{
  the vertical distance (determined from difference SPC top depth variable) within which increase must be met
}
}
\value{
Returns a numeric vector of depths where the increase retuirement is met. Typically the first is the one of interst to taxonomy.

\code{get.increase.depths} converts to horizon dop depth by using above matrix output to determine depths where increase is met.
}
\author{
Andrew Gene Brown
}
\seealso{
\code{getArgillicBounds}, \code{crit.clay.argillic}
}
\examples{
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents 
foo <- get.increase.depths(p, threshold.fun = crit.clay.argillic, 
                           attr = attr, vertical.distance = 30)
foo
}
\keyword{ manip }
