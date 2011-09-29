\name{ca630}
\alias{ca630}
\docType{data}
\title{Soil Data from the Central Sierra Nevada Region of California}
\description{Site and laboratory data from soils sampled in the central Sierra Nevada Region of California.}
\usage{data(ca630)}
\format{
List containing:

$site : A data frame containing site information.
  \describe{
    \item{\code{mlra}}{the MLRA}
    \item{\code{county}}{the county}
    \item{\code{ssa}}{soil survey area}
    \item{\code{lon}}{longitude, WGS84}
    \item{\code{lat}}{latitude, WGS84}
    \item{\code{pedon_id}}{national soil profile id}
    \item{\code{user_pedon_id}}{local soil profile id}
    \item{\code{control_sec_top}}{control section top depth (cm)}
    \item{\code{control_sec_bottom}}{control section bottom depth (cm)}
    \item{\code{series}}{soil series name}
    \item{\code{soiltax}}{family-level US Soil taxonomic membership}
  }

$lab : A data frame containing horizon information.
  \describe{
    \item{\code{pedon_id}}{national soil profile id}
    \item{\code{horizon_id}}{national horizon id}
    \item{\code{top}}{horizon top (cm)}
    \item{\code{bottom}}{horizon bottom (cm)}
    \item{\code{name}}{horizon name}
    \item{\code{texture}}{USDA soil texture}
    \item{\code{nh4_sum_bases}}{sum of bases extracted by ammonium acetate (pH 7)}
    \item{\code{ex_acid}}{exchangeable acidity [method ?]}
    \item{\code{CEC_8.2}}{cation exchange capacity by sum of cations method (pH 8.2)}
    \item{\code{CEC_7}}{cation exchange capacity by ammonium acetate (pH 7)}
    \item{\code{BS_8.2}}{base saturation by sum of cations method (pH 8.2)}
    \item{\code{BS_7}}{base saturation by ammonium acetate (pH 7)}
  }
}
\details{These data were extracted from the NSSL database. `ca630` is a list composed of site and lab data, each stored as dataframes. These data are modeled by a 1:many (site:lab) relation, with the `pedon_id` acting as the primary key in the `site` table and as the foreign key in the `lab` table.}
\source{\url{http://ssldata.nrcs.usda.gov/}}
\examples{
# 1. check structure
data(ca630)
str(ca630)

# 2. extract site data and convert to SPDF
site <- ca630$site
coordinates(site) <- ~ lon + lat

# 3. aggregate CEC 7 for all profiles into 1 cm slices
# note intermediate steps... will be cleaned-up in AQP 1.0
lab <- ca630$lab
lab$prop <- lab$CEC_7
lab$id <- factor(lab$pedon_id)
s1 <- soil.slot(data=lab)

# 4. plot median & IQR by 1 cm slice
xyplot(top ~ p.q50 + p.q25 + p.q75, 
data=s1, type='S', horizontal=TRUE, col=1, lty=c(1,2,2), 
panel=panel.superpose, ylim=c(150,-5))

# 5. safely compute hz-thickness weighted mean CEC (pH 7)
lab.agg.cec_7 <- ddply(lab, .(pedon_id), 
.fun=summarise, CEC_7=wtd.mean(CEC_7, weights=bottom-top))

# 6. SoilProfileCollection Demo
data(ca630)

# combine site+horizon data into single DF
ca <- join(ca630$lab, ca630$site)

# promote to SoilProfileCollection
depths(ca) <- pedon_id ~ top + bottom

# extract site data
site(ca) <- ~ mlra + ssa + lon + lat + user_pedon_id + control_sec_top + control_sec_bottom + series + soiltax

# extract spatial data as SpatialPoints
coordinates(ca) <- ~ lon + lat


}
\keyword{datasets}
