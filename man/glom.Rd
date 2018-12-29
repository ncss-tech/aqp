\name{glom}
\alias{glom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
glom
}
\description{
\code{glom()} returns a "clod" of horizons from a SoilProfileCollection that have a common attribute. 

Horizon intersection is based on unique ID \code{hzidname(spc)} and attribute of interest.

You "glom" SPC horizons into a "clod". A clod is a "ragged" group of soil pedon horizons. 

Currently, just depths/depth intervals are supported as common attribute -- but many others are possible and will be implemented perhaps via formula interface like \code{slice}. 

In this case, "ragged" means that horizon depths of interest, distinctness and topography (plus others?) vary from pedon to pedon. All horizons included within the specified interval are returned in their entirety (not just the portion within the interval).

A 'clod' references the original pedon data -- it is not resampled like \code{slice} or \code{slab}. 
}
\usage{
glom(p, z1, z2 = NA, as.data.frame = FALSE)
}
\arguments{
  \item{p}{
A single-profile SoilProfileCollection; e.g. \code{glom} is called via \code{profileApply()}
}
  \item{z1}{
Top depth (required) - depth to intersect horizon at; if `z2` specified, top depth of intersect interval.
}
  \item{z2}{
OPTIONAL: Bottom depth - bottom depth of intersection interval
}
  \item{as.data.frame}{
OPTIONAL: return a data.frame, by intersection with horizons(p)? default: FALSE
}
}
\value{
\code{glom()} returns a "clod" of horizons that have a common attribute of type data.frame or SoilProfileCllection.
}
\author{
Andrew Gene Brown
}
\seealso{
\code{\link{slice}}, \code{\link{slab}}
}
\examples{
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]

foo <- glom(p, 25, 100)

nrow(foo) # there are 4 horizons in the clod produced by glomming depths 25 to 100 on profile 1 in sp1
}
\keyword{ manip }
