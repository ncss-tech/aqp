\name{rowley2019}
\alias{rowley2019}
\docType{data}

\title{Soil Morphologic, Geochemical, and Mineralogy Data from Rowlet et al. 2019.}

\description{Data from Table 1 and Supplementary Tables 1 and 2 from "A cascading influence of calcium carbonate on the biogeochemistry and pedogenic trajectories of subalpine soils, Switzerland".}

\usage{data("rowley2019")}


\format{
A \code{SoilProfileCollection} object:

site-level attributes
  \describe{
    \item{id}{profile ID}
    \item{group}{profile group}
  }

horizon-level attributes
  \describe{
\item{sample_id}{sample ID}
\item{name}{horizon name}
\item{pH}{pH}
\item{Al_exch}{cmol(+) / kg, exchangeable Al}
\item{Ca_exch}{cmol(+) / kg, exchangeable Ca}
\item{CEC_sum}{cmol(+) / kg, cation exchange capacity calculated as the sum of exchangeable cations, not including H+}
\item{Ca_exch_saturation}{percent}
\item{Al_exch_saturation}{percent}
\item{TON}{percent, total nitrogen}
\item{SOC}{percent, soil organic carbon}
\item{C_to_N}{carbon to nitrogen ratio}
\item{Alo}{g/kg, oxalate-extractable Al}
\item{Feo}{g/kg, oxalate-extractable Fe}
\item{Ald}{g/kg, dithionite-extractable Al}
\item{Fed}{g/kg, dithionite-extractable Fe}
\item{Feo_Fed}{Fe_o to Fe_d ratio}
\item{id}{profile ID}
\item{top}{horizon top (cm)}
\item{bottom}{horizon bottom (cm)}
\item{Al}{g/kg by x-ray fluorescence}
\item{Ca}{g/kg by x-ray fluorescence}
\item{Cr}{g/kg by x-ray fluorescence}
\item{Fe}{g/kg by x-ray fluorescence}
\item{K}{g/kg by x-ray fluorescence}
\item{Mg}{g/kg by x-ray fluorescence}
\item{Mn}{g/kg by x-ray fluorescence}
\item{Na}{g/kg by x-ray fluorescence}
\item{Ni}{g/kg by x-ray fluorescence}
\item{P}{g/kg by x-ray fluorescence}
\item{Si}{g/kg by x-ray fluorescence}
\item{Ti}{g/kg by x-ray fluorescence}
\item{Phyllosilicates}{percent by x-ray diffraction spectra}
\item{Quartz}{percent by x-ray diffraction spectra}
\item{K_Feldspar}{percent by x-ray diffraction spectra}
\item{Na_Plagioclase}{percent by x-ray diffraction spectra}
\item{Goethite}{percent by x-ray diffraction spectra}
\item{Unidentified}{percent by x-ray diffraction spectra}
\item{CCE_Total}{percent}
\item{CCE_Reactive}{percent}
\item{Reactive_carbonate}{percent}
\item{Sand}{percent <2um}
\item{Silt}{percent 2-50um}
\item{Clay}{percent 50-2000um}
\item{hzID}{horizon ID}
  }
}


\source{
  \href{https://www.sciencedirect.com/science/article/pii/S0016706119318683}{A cascading influence of calcium carbonate on the biogeochemistry and pedogenic trajectories of subalpine soils, Switzerland}
}

\references{
Mike C. Rowley, Stephanie Grand, Thierry Adatte, Eric P. Verrecchia,
A cascading influence of calcium carbonate on the biogeochemistry and pedogenic trajectories of subalpine soils, Switzerland,
Geoderma,
2019,
114065,
ISSN 0016-7061,
https://doi.org/10.1016/j.geoderma.2019.114065.
}

\examples{
data('rowley2019')

par(mar=c(1,1,3,1))
plotSPC(rowley2019, color='Feo_Fed', name='name', cex.names=0.85)

}

\keyword{datasets}
