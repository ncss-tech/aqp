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

library(plyr)
ca <- join(ca630$lab, ca630$site, type='inner')

# promote to SoilProfileCollection
depths(ca) <- pedon_key ~ hzn_top + hzn_bot

# extract site data
site(ca) <- ~ mlra + ssa + lon + lat + cntrl_depth_to_top + cntrl_depth_to_bot + sampled_taxon_name

# extract spatial data as SpatialPoints
library(sp)
coordinates(ca) <- ~ lon + lat
# assign CRS data
proj4string(ca) <- '+proj=latlong +datum=NAD83'

# check the result
ca

# 3. aggregate \%BS 7 for all profiles into 1 cm slices
a <- slab(ca, fm= ~ bs_7)

# 4. plot median & IQR by 1 cm slice
library(lattice)
xyplot(
top ~ p.q50, data=a, lower=a$p.q25, upper=a$p.q75, 
ylim=c(160,-5), alpha=0.5, scales=list(alternating=1, y=list(tick.num=7)),
panel=panel.depth_function, prepanel=prepanel.depth_function,
ylab='Depth (cm)', xlab='Base Saturation at pH 7', 
par.settings=list(superpose.line=list(col='black', lwd=2))
)

# 4. aggregate \%BS at pH 8.2 for all profiles by MLRA, along 1 cm slices
# note that mlra is stored in @site
a <- slab(ca, mlra ~ bs_8.2)
# keep only MLRA 18 and 22
a <- subset(a, subset=mlra %in% c('18', '22'))

xyplot(
top ~ p.q50, groups=mlra , data=a, lower=a$p.q25, upper=a$p.q75, 
ylim=c(160,-5), alpha=0.5, scales=list(y=list(tick.num=7, alternating=3), x=list(alternating=1)),
panel=panel.depth_function, prepanel=prepanel.depth_function,
ylab='Depth (cm)', xlab='Base Saturation at pH 8.2', 
par.settings=list(superpose.line=list(col=c('black','blue'), lty=c(1,2), lwd=2)),
auto.key=list(columns=2, title='MLRA', points=FALSE, lines=TRUE)
)


# 5. safely compute hz-thickness weighted mean CEC (pH 7)
library(Hmisc)
head(lab.agg.cec_7 <- ddply(ca630$lab, .(pedon_key), 
.fun=summarise, CEC_7=wtd.mean(bs_7, weights=hzn_bot-hzn_top)))

# 6. extract a SPDF with horizon data along a slice at 25 cm
s.25 <- slice(ca, fm=25 ~ bs_7 + CEC7 + ex_acid)
spplot(s.25, zcol=c('bs_7','CEC7','ex_acid'))

# note that the ordering is preserved:
all.equal(s.25$pedon_key, profile_id(ca))

# 6.1 extract a data.frame with horizon data along a slices c(10,20,50)
s.multiple <- slice(ca, fm=c(10,20,50) ~ bs_7 + CEC7 + ex_acid)

# 7. Extract the 2nd horizon from all profiles as SPDF
ca.2 <- ca[, 2]

# 8. Extract the all horizons from profiles 1:10 as data.frame
ca.1.to.10 <- ca[1:10, ]

# basic plot method: profile plot
plot(ca.1.to.10, name='hzn_desgn')
}
\keyword{datasets}
