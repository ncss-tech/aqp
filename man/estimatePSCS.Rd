\name{estimatePSCS}
\alias{estimatePSCS}
\title{
estimatePSCS
}
\description{
Estimates the upper and lower boundary of the particle size control section by applying a programmatic version of the particle size control section key from the Keys to Soil Taxonomy (12th edition).

Requires information to identify argillic horizons (clay contents, horizon designations) with \code{getArgillicBounds()} as well as the presence of plow layers and surface organic soil material.

Requires information on taxonomic order (to handle andisols). 

WARNING: Soils in arenic and grossarenic subgroups, with fragipans, or with strongly contrasting PSCs may not be classified correctly. The author would welcome a dataset to develop this functionality for. 
}
\usage{
estimatePSCS(p, attr = "clay", hzdesgn = "hzname", 
require_t = TRUE, tax_order_field = "tax_order")
}
\arguments{
  \item{p}{
A single-profile SoilProfileCollection object
}
  \item{attr}{
Name of the horizon attribute containing clay contents. Default 'clay'
}
  \item{hzdesgn}{
Name of the horizon attribute containing the horizon designation. Default 'hzname'
}
  \item{require_t}{
Require 't' subscript for positive identification of an argillic horizon?
}
  \item{tax_order_field}{
Name of the site attribute containing taxonomic order; for handling PSCS rules for Andisols in lieu of lab data. May be NA or column missing altogether, in which case Andisol PSC possibility is ignored.
}
}
\value{
A numeric vector containing the top and bottom depth of the particle size control section. First value is top, second value is bottom.
}
\references{
Soil Survey Staff. 2014. Keys to Soil Taxonomy, 12th ed. USDA-Natural Resources Conservation Service, Washington, DC.
}
\author{
Andrew Gene Brown
}
\seealso{
\code{getArgillicBounds}, \code{getSurfaceHorizonDepth}
}
\examples{
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents 
foo <- estimatePSCS(p, hzdesgn='name', attr = attr)
foo
}
\keyword{ manip }
