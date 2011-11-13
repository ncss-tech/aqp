\name{sp4}
\alias{sp4}
\docType{data}
\title{Soil Chemical Data from Serpentinitic Soils of California}
\description{Soil Chemical Data from Serpentinitic Soils of California}
\usage{data(sp4)}
\format{
  A data frame with 30 observations on the following 19 variables.
  \describe{
    \item{\code{id}}{a character vector}
    \item{\code{name}}{a character vector}
    \item{\code{top}}{a numeric vector}
    \item{\code{bottom}}{a numeric vector}
    \item{\code{K}}{a numeric vector}
    \item{\code{Mg}}{a numeric vector}
    \item{\code{Ca}}{a numeric vector}
    \item{\code{CEC_7}}{a numeric vector}
    \item{\code{ex_Ca_to_Mg}}{a numeric vector}
    \item{\code{sand}}{a numeric vector}
    \item{\code{silt}}{a numeric vector}
    \item{\code{clay}}{a numeric vector}
    \item{\code{CF}}{a numeric vector}
    \item{\code{Ca_e_1}}{a numeric vector}
    \item{\code{Ca_e}}{a numeric vector}
    \item{\code{Ca_st}}{a numeric vector}
    \item{\code{Mg_st}}{a numeric vector}
    \item{\code{Ca_e_to_Ca_st}}{a numeric vector}
    \item{\code{Bt}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
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
