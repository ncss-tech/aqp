\name{sp4}
\alias{sp4}
\docType{data}
\title{Soil Chemical Data from Serpentinitic Soils of California}
\description{Soil Chemical Data from Serpentinitic Soils of California}
\usage{data(sp4)}
\format{
  A data frame with 30 observations on the following 19 variables.
  \describe{
    \item{\code{id}}{site name}
    \item{\code{name}}{horizon designation}
    \item{\code{top}}{horizon top boundary in cm}
    \item{\code{bottom}}{horizon bottom boundary in cm}
    \item{\code{K}}{exchangeable K in c mol/kg}
    \item{\code{Mg}}{exchangeable Mg in cmol/kg}
    \item{\code{Ca}}{exchangeable Ca in cmol/kg}
    \item{\code{CEC_7}}{cation exchange capacity (NH4OAc at pH 7)}
    \item{\code{ex_Ca_to_Mg}}{extractable Ca:Mg ratio}
    \item{\code{sand}}{sand content by weight percentage}
    \item{\code{silt}}{silt content by weight percentage}
    \item{\code{clay}}{clay content by weight percentage}
    \item{\code{CF}}{>2mm fraction by volume percentage}
  }
}
\details{Selected soil physical and chemical data from (McGahan et al., 2009).}
\source{https://www.soils.org/publications/sssaj/articles/73/6/2087}
\references{McGahan, D.G., Southard, R.J, Claassen, V.P. 2009. Plant-Available Calcium Varies Widely in Soils on Serpentinite Landscapes. Soil Sci. Soc. Am. J. 73: 2087-2095.}
\examples{
data(sp4)

sp4$soil_color <- 'white'

# optionally color horizons with 'ex_Ca_to_Mg'
if(require(RColorBrewer)) {
  cols <- rev(brewer.pal(8, 'Spectral'))
  cr <- colorRamp(cols)
  sp4$soil_color <- rgb(cr(rescaler(sp4$ex_Ca_to_Mg, type='range')), max=255)
}

# init SPC object
depths(sp4) <- id ~ top + bottom

# plot
profile_plot(sp4)

}
\keyword{datasets}
