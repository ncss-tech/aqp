\name{clod.hz.ids}
\alias{clod.hz.ids}
\title{
clod.hz.ids
}
\description{
clod.hz.ids returns a vector of unique indices corresponding to a depth interval. As arguments, it takes a single-profile SoilProfileCollection `p`, a top depth `z1` and an optional bottom depth `z2`. 

If just top depth is specified, the unique index of the horizon intersected by that depth is returned. If bottom depth is specified, all horizon IDs that are intersected by the depth interval are returned.
}
\usage{
clod.hz.ids(p, z1, z2 = NA, as.list = FALSE)
}
\arguments{
  \item{p}{
A single-profile SoilProfileCollection
}
  \item{z1}{
Top depth (required) - depth to intersect horizon at; if `z2` specified, top depth of intersect interval.
}
  \item{z2}{
OPTIONAL: Bottom depth - bottom depth of intersection interval
}
  \item{as.list}{
OPTIONAL: return a list? default: FALSE
}
}
\details{
This function is a workhorse used by \code{glom()} to create an index of unique horizon IDs intersecting the attribute of interest (depth) at the specified levels. This function returns the horizon IDs required to index an SPC and produce either a subset SPC or data.frame representation of the "clod" returned. 
}
\value{
A vector of unique horizon IDs (often numeric or character).
}
\author{
Andrew Gene Brown
}
\seealso{
\code{glom}, \code{slice}, \code{slab}
}
\examples{
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]

foo <- clod.hz.ids(p, 25, 100)

foo
}
\keyword{ manip }
