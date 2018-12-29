\name{get.increase.matrix}
\alias{get.increase.matrix}
\title{
get.increase.matrix
}
\description{
Uses matrix outer product to determine all pair-wise differences in `attr` for the horizons of `p`. Supplies `attr` to `threshold.fun` to determine the minimum value criterion to return TRUE in output matrix for an "increase". Also, computes all pair-wise distances in depth dimension to determine whether the vertical distance criteria have been met simultaneously with `attr` increase.
 
This function assumes that the `threshold.fun` supplied by the user returns either a constant or a vector of equal length to its input. 

Note that the `threshold.fun` result is allowed to contain NA, but that will result in no output for affected cells.

\code{get.increase.depths} performs the conversion of the square matrix output of \code{get.increase.matrix} back to horizon top depth for where criteria were met.
}
\usage{
get.increase.matrix(p, attr, threshold.fun, vertical.distance)
}
%- maybe also 'usage' for other objects documented here.
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
Returns a square logical matrix reflecting where the increase criteria were met.

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
foo <- get.increase.matrix(p, threshold.fun = crit.clay.argillic,
                           attr = attr, vertical.distance = 30)
foo
}
\keyword{ manip }
