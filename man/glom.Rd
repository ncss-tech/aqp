\name{glom}
\alias{glom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
glom
}
\description{
\code{glom()} returns a "clod" of horizons from a SoilProfileCollection that have a common attribute. 

Horizon intersection is based on unique ID \code{hzidname(spc)} and attribute of interest.

Currently, just depth (intervals) are supported as common attributes. 

You "glom" SPC horizons into a "clod". A clod is a "ragged" group of soil pedon horizons. 

In this case, "ragged" means that horizon depths of interest, distinctness and topography (plus others?) vary from pedon to pedon. 

A 'clod' references the original pedon data -- it is not resampled like a \code{slice} or \code{slab}. 
}
\usage{
glom(p, z1, z2 = NA, as.data.frame = FALSE)
}
%- maybe also 'usage' for other objects documented here.
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
\value{
\code{glom()} returns a "clod" of horizons that have a common attribute of type data.frame or SoilProfileCllection.
}
\author{
Andrew Gene Brown
}
\seealso{
\code{\link{slab}}, \code{\link{slab}}
}
\examples{
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents % 
foo <- glom(p, 25, 100)
foo[[attr]]

\keyword{ manip }
