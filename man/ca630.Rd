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
    \item{\code{user_site_id}}{national user site id}
    \item{\code{mlra}}{the MLRA}
    \item{\code{county}}{the county}
    \item{\code{ssa}}{soil survey area}
    \item{\code{lon}}{longitude, WGS84}
    \item{\code{lat}}{latitude, WGS84}
    \item{\code{pedon_key}}{national soil profile id}
    \item{\code{user_pedon_id}}{local soil profile id}
    \item{\code{cntrl_depth_to_top}}{control section top depth (cm)}
    \item{\code{cntrl_depth_to_bot}}{control section bottom depth (cm)}
    \item{\code{sampled_taxon_name}}{soil series name}
  }

$lab : A data frame containing horizon information.
  \describe{
    \item{\code{pedon_key}}{national soil profile id}
    \item{\code{layer_key}}{national horizon id}
    \item{\code{layer_sequence}}{horizon sequence number}
    \item{\code{hzn_top}}{horizon top (cm)}
    \item{\code{hzn_bot}}{horizon bottom (cm)}
    \item{\code{hzn_desgn}}{horizon name}
    \item{\code{texture_description}}{USDA soil texture}
    \item{\code{nh4_sum_bases}}{sum of bases extracted by ammonium acetate (pH 7)}
    \item{\code{ex_acid}}{exchangeable acidity [method ?]}
    \item{\code{CEC8.2}}{cation exchange capacity by sum of cations method (pH 8.2)}
    \item{\code{CEC7}}{cation exchange capacity by ammonium acetate (pH 7)}
    \item{\code{bs_8.2}}{base saturation by sum of cations method (pH 8.2)}
    \item{\code{bs_7}}{base saturation by ammonium acetate (pH 7)}
  }
}
\details{These data were extracted from the NSSL database. `ca630` is a list composed of site and lab data, each stored as dataframes. These data are modeled by a 1:many (site:lab) relation, with the `pedon_id` acting as the primary key in the `site` table and as the foreign key in the `lab` table.}
\source{\url{http://ssldata.nrcs.usda.gov/}}
\examples{
data(ca630)
str(ca630)

# 2. promote to SoilProfileCollection
# combine site+horizon data into single DF
ca <- join(ca630$lab, ca630$site, type='inner')

# promote to SoilProfileCollection
depths(ca) <- pedon_key ~ hzn_top + hzn_bot

# extract site data
site(ca) <- ~ mlra + ssa + lon + lat + cntrl_depth_to_top + cntrl_depth_to_bot + sampled_taxon_name

# extract spatial data as SpatialPoints
coordinates(ca) <- ~ lon + lat


# 3. aggregate CEC 7 for all profiles into 1 cm slices
# add new fake grouping column
ca@site$group <- 1 # messy syntax ... will be fixed in AQP 1.0
a <- soil.slot.multiple(ca, fm=group ~ bs_7)

# 4. plot median & IQR by 1 cm slice
xyplot(
top ~ p.q50, data=a, lower=a$p.q25, upper=a$p.q75, 
ylim=c(160,-5), alpha=0.5, scales=list(alternating=1, y=list(tick.num=7)),
panel=panel.depth_function, prepanel=prepanel.depth_function,
ylab='Depth (cm)', xlab='Base Saturation at pH 7', par.settings=list(superpose.line=list(col='black'))
)

# 5. safely compute hz-thickness weighted mean CEC (pH 7)
lab.agg.cec_7 <- ddply(ca630$lab, .(pedon_key), 
.fun=summarise, CEC_7=wtd.mean(bs_7, weights=hzn_bot-hzn_top))

}
\keyword{datasets}
