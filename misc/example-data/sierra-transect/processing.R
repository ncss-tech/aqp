# need these packages
library(aqp)

## load original Sierra Transect (central Sierra, granite) data from CSV
granite <- read.csv('dahlgren-granitics.csv', stringsAsFactors=FALSE)

## load parallel Merhten formation transect from CSV
# note that there are two files
andesite <- read.csv(file='rasmussen-andisitic-lahar.csv', stringsAsFactors=FALSE)
andesites.site <- read.csv(file='rasmussen-andisitic-lahar-site.csv', stringsAsFactors=FALSE)


# convert Munsell notation into R colors (sRGB)
granite$soil_color <- with(granite, munsell2rgb(hue, value, chroma))
andesite$soil_color <- with(andesite, munsell2rgb(hue, value, chroma))


## init SoilProfileCollection for granite transect
depths(granite) <- id ~ top + bottom
# transfer site level attributes
site(granite) <- ~ elev + MAAT + MAP + geo + x + y


## init SoilProfileCollection for andesite transect
depths(andesite) <- id ~ top + bottom
# transfer site level attributes
site(andesite) <- ~ elev + precip + MAP + MAT + veg + Fe_d_to_Fe_t
# join coordinates via `id`
site(andesite) <- andesites.site



## init spatial data from coordinates
coordinates(granite) <- ~ x + y
proj4string(granite) <- '+proj=longlat +datum=NAD83'

coordinates(andesite) <- ~ x + y
proj4string(andesite) <- '+proj=longlat +datum=NAD83'


## label transects via site-level attribute
granite$transect <- rep('Granite', times=length(granite))
andesite$transect <- rep('Andesite', times=length(andesite))



## union into single SPC, note that attribute names may not be the same
g <- aqp::union(list(granite, andesite))

# quick check, note issues related to union
plot(g)


## load and merge sampled raster data
gis.data <- read.csv('transect-GIS-data.csv', stringsAsFactors = FALSE)
site(g) <- gis.data


## check


## re-level factors
g$transect <- factor(g$transect, levels=c('Granite', 'Andesite'))

## normalize some names

# connotative name, IDs from transects, taxonname from NASIS data
g$soil_name <- ifelse(is.na(g$taxonname), profile_id(g), g$taxonname)

# horizon designations
g$name <- ifelse(is.na(g$name), g$hzname, g$name)

g$elev <- ifelse(is.na(g$elev), g$elev_field, g$elev)


plot(g, label='soil_name')
plot(g, label='soil_name', color='clay')

plot(g, label='soil_name', plot.order=order(g$elev))

par(mar=c(0,0,2,1))
groupedProfilePlot(g, groups='transect', label='soil_name')

## compute some idices if possible
g$Fe_o_to_Fe_d <- g$Fe_o / g$Fe_d

plot(g, label='soil_name', color='Fe_o_to_Fe_d')


## investigate Granite transect 
str(granite, 2)
granite

# sketches
par(mar=c(1,0,1,1))
plot(granite)

# order along elevation
granite.elev.order <- order(granite$elev)

par(mar=c(1,0,1,1))
plot(granite, name='name', plot.order=granite.elev.order)
axis(1, at=1:length(granite), labels=granite$elev[granite.elev.order], line=-2)

par(mar=c(1,0,3,1))
plot(granite, color='clay', plot.order=granite.elev.order)
axis(1, at=1:length(granite), labels=granite$elev[granite.elev.order], line=-2)

plot(granite, color='BS', plot.order=granite.elev.order)
axis(1, at=1:length(granite), labels=granite$elev[granite.elev.order], line=-2)

plotSPC(granite, color='Fe_d', plot.order=granite.elev.order)
axis(1, at=1:length(granite), labels=granite$elev[granite.elev.order], line=-2)


## convert soil color to sRGB
# note: must keep track of NA as the conversion will result in 'white'
na.idx <- which(is.na(g$soil_color))
# scale to {0,1}
g.rgb <- t(col2rgb(g$soil_color)) / 255

# insert NA
g.rgb[na.idx, ] <- cbind(NA, NA, NA)

head(g.rgb)

# save back to SPC
g$r <- g.rgb[, 1]
g$g <- g.rgb[, 2]
g$b <- g.rgb[, 3]




plotTransect(g, 'elev', crs=CRS('+proj=utm +zone=11 +datum=NAD83'), grad.axis.title='Elevation (m)', label='soil_name')

plotTransect(g, 'elev', crs=CRS('+proj=utm +zone=11 +datum=NAD83'), grad.axis.title='Elevation (m)', label='soil_name', spacing = 'relative')

plotTransect(g, 'rain.fraction_800', crs=CRS('+proj=utm +zone=11 +datum=NAD83'), grad.axis.title='Fraction PPT as Rain (%)', label='soil_name')

plotTransect(g, 'effective.ppt_800', crs=CRS('+proj=utm +zone=11 +datum=NAD83'), grad.axis.title='Effective PPT (mm)',  label='soil_name', spacing='relative')


plotTransect(g[which(g$transect == 'Granite'), ], 'elev', crs=CRS('+proj=utm +zone=11 +datum=NAD83'), grad.axis.title='Elevation (m)',  label='soil_name')

plotTransect(g[which(g$transect == 'Granite'), ], 'elev', crs=CRS('+proj=utm +zone=11 +datum=NAD83'), grad.axis.title='Elevation (m)',  label='soil_name', spacing = 'relative')


plotTransect(g[which(g$transect == 'Andesite'), ], grad.var.name = 'effective.ppt_800m', crs=CRS('+proj=utm +zone=11 +datum=NAD83'), grad.axis.title='Elevation (m)',  label='soil_name')

plotTransect(g[which(g$transect == 'Mineral King'), ], 'effective.ppt_800', crs=CRS('+proj=utm +zone=11 +datum=NAD83'), grad.axis.title='Effective PPT (mm)',  label='soil_name')


# granite$HzD <- hzDistinctnessCodeToOffset(substr(granite$hz_boundary, 0, 1))





intersect(horizonNames(andesite), horizonNames(granite))

par(mar=c(0,0,3,0))
plot(g, plot.order=order(g$elev))
plot(g, plot.order=order(g$elev), color='clay')
plot(g, plot.order=order(g$elev), color='Al_p')
plot(g, plot.order=order(g$elev), color='Si_o')
plot(g, plot.order=order(g$elev), color='CEC')
plot(g, plot.order=order(g$elev), color='Fe_o_to_Fe_d')

groupedProfilePlot(g, groups = 'transect', group.name.offset = -15)

g.new.order <- order(granite$elev)
a.new.order <- order(andesite$elev)

par(mfcol=c(1, 2))
plot(granite, name='name', plot.order=g.new.order, hz.distinctness.offset='HzD')
axis(1, at=1:length(granite), labels=granite$elev[g.new.order], line=-2)

plot(andesite, name='name', plot.order=a.new.order)
axis(1, at=1:length(andesite), labels=andesite$elev[a.new.order], line=-2)

pdf(file='RAD-transect.pdf', width=11, height=7)
par(mar=c(3.5,3.5,3,1))
plotTransect(granite, 'elev', crs=CRS('+proj=utm +zone=11 +datum=NAD83'), grad.axis.title='Elevation (m)')
plotTransect(granite, 'elev', crs=CRS('+proj=utm +zone=11 +datum=NAD83'), grad.axis.title='Elevation (m)', color='clay', col.label='Clay (%)')
plotTransect(granite, 'elev', crs=CRS('+proj=utm +zone=11 +datum=NAD83'), grad.axis.title='Elevation (m)', color='sand', col.label='Sand (%)')
plotTransect(granite, 'elev', crs=CRS('+proj=utm +zone=11 +datum=NAD83'), grad.axis.title='Elevation (m)', color='BS', col.label='Base Saturation (%)')
plotTransect(granite, 'elev', crs=CRS('+proj=utm +zone=11 +datum=NAD83'), grad.axis.title='Elevation (m)', color='Fe_o', col.label='Oxalate-Fe (g/kg)')
dev.off()



## low-level stuff
explainPlotSPC(g)

plot(g, color='hzID')

## combine original + slice + slab




m.order <- order(mineralKing$elev_field)

par(mar=c(1,1,2,1))

groupedProfilePlot(mineralKing, groups='taxonname', print.id=FALSE)

par(mfcol=c(1, 3))
plot(mineralKing, name='hzname', plot.order=m.order, label='taxonname')
axis(1, at=1:length(mineralKing), labels=mineralKing$elev_field[m.order], line=-2)


plot(granite, name='name', plot.order=g.new.order, hz.distinctness.offset='HzD')
axis(1, at=1:length(granite), labels=granite$elev[g.new.order], line=-2)

plot(andesite, name='name', plot.order=a.new.order)
axis(1, at=1:length(andesite), labels=andesite$elev[a.new.order], line=-2)


g <- union(list(g, mineralKing))

g$elev <- ifelse(is.na(g$elev), g$elev_field, g$elev)
g$taxonname <- ifelse(is.na(site(g)$taxonname), site(g)$id, site(g)$taxonname)

par(mar=c(1,1,2,1))
plot(g, label='taxonname', plot.order=order(g$elev))







## soil color stuff
previewColors(g$soil_color)
plotColorQuantiles(colorQuantiles(g$soil_color))

# extract sRGB coords

# color signature
pig <- soilColorSignature(g, RescaleLightnessBy = 5, method = 'pam', pam.k = 3)

# move row names over for distance matrix
row.names(pig) <- pig[, 1]
d <- daisy(pig[, -1])
dd <- diana(d)

par(mar=c(1,1,1,1))
plotProfileDendrogram(g, dd, dend.y.scale = max(d) * 2, scaling.factor = 0.25, y.offset = 6, width=0.15, cex.names=0.45)

plotProfileDendrogram(g, dd, dend.y.scale = max(d) * 2, scaling.factor = 0.25, y.offset = 6, width=0.15, cex.names=0.45, label='taxonname')


# aggregate soil colors
a <- aggregateColor(g, groups='transect', k = 6)
aggregateColorPlot(a)



## bring in KSSL data by series name

## bring in KSSL data by BBOX

## slab / slice / glom

## viz aggregate data

## new functions by Andrew

profileApply(g, getArgillicBounds, hzdesgn='name', attr='clay', simplify = FALSE)


## networks?

## pair-wise distances





# ## RI as described in Barron and Torrent, 1986
# convert to CIE LAB
# g.lab <- data.frame(convertColor(g.rgb, from = 'sRGB', to = 'Lab', from.ref.white = 'D65', clip = FALSE))
# g$RI <- with(g.lab, (a.x * sqrt((a.x^2 + b^2 )) * 10^10 ) / (b * L^6) )
# g$ln_RI <- log(g$RI)
# 
# hist(g$ln_RI)
# 
# plot(g, color='ln_RI')
# 

