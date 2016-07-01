\name{sp6}
\alias{sp6}
\docType{data}

\title{Soil Physical and Chemical Data from Manganiferous Soils}
\description{Soil Physical and Chemical Data from Manganiferous Soils (Bourgault and Rabenhorst, 2011)}

\usage{data(sp6)}
\format{
  A data frame with 30 observations on the following 13 variables.
  \describe{
    \item{\code{id}}{pedon name}
    \item{\code{name}}{horizon designation}
    \item{\code{top}}{horizon top boundary in cm}
    \item{\code{bottom}}{horizon bottom boundary in cm}
    \item{\code{color}}{moist soil color in Munsell notation}
    \item{\code{texture}}{USDA soil texture class}
    \item{\code{sand}}{sand content by weight percentage}
    \item{\code{silt}}{silt content by weight percentage}
    \item{\code{clay}}{clay content by weight percentage}
    \item{\code{Fe}}{DCB-extracted Fe in g/kg (see citation)}
    \item{\code{Mn}}{DCB-extracted Mn in g/kg (see citation)}
    \item{\code{C}}{total organic carbon as g/kg}
    \item{\code{pH}}{measured in 1:1 H20 slurry}
    \item{\code{Db}}{bulk density (g/cc), clod method}
  }
}

\details{Selected soil physical and chemical data from (Bourgault and Rabenhorst, 2011).}


\source{http://www.sciencedirect.com/science/article/pii/S0016706111001972}

\references{Rebecca R. Bourgault, Martin C. Rabenhorst. 2011. 
Genesis and characterization of manganiferous soils in the 
Eastern Piedmont, USA. Geoderma. 165:84-94.}

\examples{
  # setup environment
  library(aqp)
  data(sp6)
  
  # init SPC
  depths(sp6) <- id ~ top + bottom
  # convert non-standard Munsell colors
  sp6$soil_color <- getClosestMunsellChip(sp6$color)
  
  # profile sketches
  par(mar=c(0,0,3,0))
  plot(sp6, color='soil_color')
  plot(sp6, color='Mn')
  plot(sp6, color='Fe')
  plot(sp6, color='pH')
  plot(sp6, color='texture')

}
\keyword{datasets}
