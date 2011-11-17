\name{sp5}
\alias{sp5}
\docType{data}
\title{Sample Soil Database #5}
\description{296 Soil Profiles from the La Rochelle region of France (F. Carre and Girard, 2002)}
\usage{data(sp5)}
\format{
  The format is:
Formal class 'SoilProfileCollection' [package "aqp"] with 6 slots
  ..@ idcol    : chr "soil"
  ..@ depthcols: chr [1:2] "top" "bottom"
  ..@ metadata :'data.frame':	1 obs. of  1 variable:
  .. ..$ depth_units: chr "cm"
  ..@ horizons :'data.frame':	1539 obs. of  17 variables:
  .. ..$ soil      : profile ID
  .. ..$ sand      : sand
  .. ..$ silt      : silt
  .. ..$ clay      : clay
  .. ..$ R25       : RGB r-coordinate
  .. ..$ G25       : RGB g-coordinate
  .. ..$ B25       : RGB b-coordinate
  .. ..$ pH        : pH
  .. ..$ EC        : EC
  .. ..$ CaCO3     : CaC03 content
  .. ..$ C         : C content
  .. ..$ Ca        : Ca
  .. ..$ Mg        : Mg
  .. ..$ Na        : Na
  .. ..$ top       : horizon top boundary
  .. ..$ bottom    : horizon bottom boundary
  .. ..$ soil_color: soil color in r-friendly format
  ..@ site     :'data.frame':	0 obs. of  0 variables
  ..@ sp       :Formal class 'SpatialPoints' [package "sp"] with 3 slots
  .. .. ..@ coords     : num [1, 1] 0
  .. .. ..@ bbox       : logi [1, 1] NA
  .. .. ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots
  .. .. .. .. ..@ projargs: chr NA
}
\details{These data are c/o F. Carre (Florence.CARRE@ineris.fr).}
\source{296 Soil Profiles from the La Rochelle region of France (F. Carre and Girard, 2002).}
\references{F. Carre, M.C. Girard. 2002. Quantitative mapping of soil types based on regression kriging of taxonomic distances with landform and land cover attributes. Geoderma. 110: 241--263.}

\examples{
data(sp5)
show(sp5)
}
\keyword{datasets}
