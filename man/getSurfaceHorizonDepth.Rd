\name{getSurfaceHorizonDepth}
\alias{getSurfaceHorizonDepth}
\alias{getPlowLayerDepth} 
\alias{getMineralSoilSurfaceDepth}
\title{
Determine thickness of horizons (continuous from surface) matching a pattern
}
\description{
This function is used to find the thickness of arbitrary horizon designations that are continuous from the soil surface (depth = 0). 

The horizon designation to match is specified with the regular expression pattern `pattern`. All horizons matching that pattern, that are continuous from the soil surface, count towards the depth / thickness value that is ultimately returned.

\code{getSurfaceHorizonDepth} is used by \code{getPlowLayerDepth} for matching Ap horizons; and, it is used by \code{getMineralSoilSurfaceDepth} to find the thickness of O horizons in lieu of lab data. These examples are taxonomically relevant for both the argillic horizon and the particle size control section.
}
\usage{
getSurfaceHorizonDepth(p, pattern, hzdesgn = "hzname")
}
\arguments{
  \item{p}{
A single-profile SoilProfileCollection object.
}
  \item{pattern}{
Regular expression pattern to match for all horizons to be considered part of the "surface"
}
  \item{hzdesgn}{
The horizon-level attribute name referring to horizon designation. Default 'hzname'.
}
}
\value{
Returns a numeric value corresponding to the bottom depth of the last horizon matching `pattern` that is contiguous with other matching horizons up to the soil surface (depth = 0).
}
\author{
Andrew Gene Brown
}
\seealso{
\code{getArgillicBounds},\code{estimatePSCS}
}
\examples{
library(aqp)
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
q <- sp1[2]

# look at horizon designations in p and q
p$name
q$name

# thickness of all surface horizons containing A
getSurfaceHorizonDepth(p, pattern = 'A', hzdesgn = 'name')

# thickness of all surface horizons that start with A
getSurfaceHorizonDepth(p, pattern = '^A', hzdesgn = 'name')

# thickness of all surface horizons that start with A, and the A is not followed by B
getSurfaceHorizonDepth(p, pattern = '^A[^B]', hzdesgn = 'name')

# thickness of all surface horizons that start with A 
#  followed by a number from _2_ to 9 (returns ZERO)
getSurfaceHorizonDepth(p, pattern = '^A[2-9]', hzdesgn = 'name')

# getPlowLayerDepth matches first two horizons in fake Ap horizon data with "buried Ap"
p$aphorizons <- c("Ap1","Ap2","AB", rep('C', nrow(p) - 4), "Apb")
getPlowLayerDepth(p, hzdesgn = 'aphorizons')

# getMineralSoilSurfaceDepthmatches first 3 horizons in fake O horizon data
p$ohorizons <- c("Oi1","Oi2","Oe", rep('C', nrow(p) - 4), "2C")
getMineralSoilSurfaceDepth(p, hzdesgn='ohorizons')

# matches first Oi horizon with original horizon designations of pedon 2
getMineralSoilSurfaceDepth(q, hzdesgn='name')
}
\keyword{ manip }
