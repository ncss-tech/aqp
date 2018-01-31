library(plotKML)
library(aqp)

## simple example
data(sp1)
sp1$x <- rnorm(nrow(sp1))
sp1$y <- rnorm(nrow(sp1))
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

depths(sp1) <- id ~ top + bottom

site(sp1) <- ~ group

profile_plot(sp1, plot.order=order(sp1$group))



## test a more realistic data set
data(ca630)

# combine into single DF
ca <- join(ca630$lab, ca630$site)

# throw-out data missing MLRA
ca <- subset(ca, subset=ca$mlra != '')

# init SPC
depths(ca) <- pedon_key ~ hzn_top + hzn_bot

# extract site data
site(ca) <- ~ mlra + county + ssa + lon + lat + cntrl_depth_to_top + cntrl_depth_to_bot + sampled_taxon_name 

# extract spatial data as SpatialPoints
coordinates(ca) <- ~ lon + lat
# ... temp hack until we have complete methods for SPC
proj4string(ca@sp) <- '+proj=latlong +datum=WGS84'


# simple map: syntax isn't as nice as it could be
f <- factor(site(ca)$mlra, levels=c('18','22A','22'))
plot(ca@sp, pch=16, col=f, axes=TRUE)
legend('topright', legend=levels(f), col=1:3, pch=16)


## TODO: needs more thought
# convert to SPDF by subsetting horizon data (first horizon)
ca.SPDF <- ca[1,]
spplot(ca.SPDF, 'bs_7', col.regions=bpy.colors(10))

# export as KML
kml(ca.SPDF, file='ca630_BS7.kml', labels=as.character(ca.SPDF$pedon_key), colour=bs_7, overwrite=TRUE)

# convert to SPDF using only site data
ca.SPDF <- SpatialPointsDataFrame(ca@sp, data=site(ca))
ca.SPDF$mlra <- factor(ca.SPDF$mlra, levels=c('18','22A','22'))

# export as KML
kml(ca.SPDF, file='ca630_MLRA.kml',labels=as.character(ca.SPDF$mlra), colour=as.numeric(mlra), overwrite=TRUE)


# do some aggregation: 
# using groups defined in @site, to aggregate properties stored in @horizons
# can't have missing horizon boundaries ... 
a <- soil.slot.multiple(ca, fm=mlra ~ bs_7)
a$mlra <- factor(a$mlra, levels=c('18','22A','22'))


xyplot(
top ~ p.q50 | mlra, data=a, lower=a$p.q25, upper=a$p.q75, 
ylim=c(160,-5), alpha=0.5, scales=list(alternating=1, y=list(tick.num=7)),
panel=panel.depth_function, prepanel=prepanel.depth_function,
strip=strip.custom(bg='Yellow'), layout=c(3,1),
ylab='Depth (cm)', xlab='% Base Saturation', par.settings=list(superpose.line=list(col='black'))
)





