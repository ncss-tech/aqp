\name{estimatePSCS}
\alias{estimatePSCS}
\title{
Estimate boundaries of the particle size control section (U.S Soil Taxonomy; 12th edition)
}
\description{
Estimates the upper and lower boundary of the particle size control section by applying a programmatic version of the particle size control section key from the Keys to Soil Taxonomy (12th edition).

Requires information to identify argillic horizons (clay contents, horizon designations) with \code{getArgillicBounds()} as well as the presence of plow layers and surface organic soil material. Any \code{getArgillicBounds()} arguments may be passed to \code{estimatePSCS}.

Requires information on taxonomic order (to handle andisols). 

WARNING: Soils in arenic or grossarenic subgroups, with fragipans, or with strongly contrasting PSCs may not be classified correctly. The author would welcome a dataset to develop this functionality for. 
}
\usage{
estimatePSCS(p, 
             hzdesgn = "hzname", 
             clay.attr = "clay", 
             texcl.attr = "texcl", 
             tax_order_field = "tax_order",
             bottom.pattern = "Cr|R|Cd", ...)
}
\arguments{
  \item{p}{
A single-profile SoilProfileCollection object
}
  \item{clay.attr}{
Name of the horizon attribute containing clay contents. Default 'clay'
}
  \item{texcl.attr}{
Name of the horizon attribute containing textural class (used for finding sandy textures). Default 'texcl'
}
  \item{hzdesgn}{
Name of the horizon attribute containing the horizon designation. Default 'hzname'
}
  \item{tax_order_field}{
Name of the site attribute containing taxonomic order; for handling PSCS rules for Andisols in lieu of lab data. May be NA or column missing altogether, in which case Andisol PSC possibility is ignored.
}
  \item{bottom.pattern}{
Regular expression pattern to match a root-restrictive contact. Default matches Cr, R or Cd. This argument is passed to both estimateSoilDepth and getArgillicBounds.
}
  \item{...}{
additional arguments are passed to getArgillicBounds()
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
foo <- estimatePSCS(p, hzdesgn='name', clay.attr = attr, texcl.attr="texture")
foo
}
\keyword{ manip }
