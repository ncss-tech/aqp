\name{mapunit_geom_by_ll_bbox}
\alias{mapunit_geom_by_ll_bbox}
\title{Fetch Map Unit Geometry from SDA}
\description{Fetch map unit geometry from the SDA website by WGS84 bounding box.}
\usage{
mapunit_geom_by_ll_bbox(bbox)
}
\arguments{
  \item{bbox}{a bounding box in WGS coordinates}
}
\details{The SDA website can be found at \url{http://sdmdataaccess.nrcs.usda.gov}. See examples for bounding box formatting.}
\value{A SpatialPolygonsDataFrame of map unit polygons, in WGS84 (long,lat) coordinates.}
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E Beaudette}
\note{It appears that SDA does not actually return the spatial intersecion of map unit polygons and bounding box. Rather, just those polygons that are completely within the bounding box.}
\seealso{}
\examples{
# fetch map unit geometry from a bounding-box:
# 
#         +------------- (-120.8, 37.8)
#         |                     |
#         |                     |
# (-120.95, 37.7) --------------+

b <- c(-120.9,37.7,-120.8,37.8)
\dontrun{mapunit_geom_by_ll_bbox(b)}
}
\keyword{manip}
