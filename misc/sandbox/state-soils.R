# need the latest versions from GitHub

library(aqp)
library(soilDB)

# load data
data('us.state.soils')

# get OSDs
x <- fetchOSD(us.state.soils$series)

# fiddle with names for a simple join to the SPC
us.state.soils$series <- toupper(us.state.soils$series)
names(us.state.soils) <- c('state', 'abbreviated', 'id')

# join state names / abbreviations to SPC
site(x) <- us.state.soils

# alpha ordering
o <- order(x$abbreviated)

# make a figure, neat
par(mar = c(0,0,0,0))
plotSPC(
  x, 
  plot.depth.axis = TRUE, 
  axis.line.offset = -5,
  name = NA, 
  label = 'abbreviated', 
  width = 0.35, 
  id.style = 'top', 
  plot.order = o
)

mtext('aqp::us.state.soils', side = 1, at = 0, font = 2, line = -2, adj = 0)


# organize via color signature
library(sharpshootR)
library(cluster)

# convert moist soil colors -> sRGB coordinates
x.rgb <- munsell2rgb(x$hue, x$value, x$chroma, return_triplets = TRUE)

# hack: copy over to hz level attributes of SPC
x$r <- x.rgb$r
x$g <- x.rgb$g
x$b <- x.rgb$b

# soil color signature via PAM
# http://ncss-tech.github.io/AQP/aqp/soil-color-signatures.html
pig <- soilColorSignature(x, RescaleLightnessBy = 5, method = 'pam', pam.k = 3)

# copy profile IDs to row.names
row.names(pig) <- pig[, 1]

# distance matrix -> divisive hierarchical clustering
d <- daisy(pig[, -1])
dd <- diana(d)

# inverse colors
# order according to clustering vector
par(mar = c(0,0,0,0), bg = 'black', fg = 'white')
plotSPC(
  x, 
  plot.depth.axis = TRUE, 
  axis.line.offset = -5,
  name = NA, 
  label = 'abbreviated', 
  width = 0.35, 
  id.style = 'top', 
  plot.order = dd$order
)

mtext('aqp::us.state.soils', side = 1, at = 0, font = 2, line = -2, adj = 0)


# hang profiles from a dendrogram
par(mar=c(0,0,1,1))
plotProfileDendrogram(x, dd, dend.y.scale = max(d) * 2, scaling.factor = 0.25, y.offset = 6, width=0.25, cex.names=0.45, label = 'abbreviated')


# nMDS
library(MASS)

# replace 0-distances with min of non-zero distances
d[d == 0] <- min(d[d > 0])
# nMDS
mds <- sammon(d)

# ordination
# color points by "3rd" horizon, moist soil color
plot(mds$points, pch = 15, col = x[, 3]$soil_color, cex = 3)
text(mds$points, x$state, cex = 0.75)
mtext('aqp::us.state.soils', side = 1, at = 0, font = 2, line = -2, adj = 0)



# this time with climate data for CONUS soils
library(latticeExtra)

## note: using locally modified version of `us.state.soils`
# get extended OSD data
x <- fetchOSD(us.state.soils$id, extended = TRUE)

# join state names / abbreviations to SPC
site(x$SPC) <- us.state.soils

# remove those outside of CONUS
z <- filter(x$SPC, site(x$SPC)$id %in% x$climate.annual$series)

# plot style
trellis.par.set(plot.line = list(col = 'RoyalBlue'))

# annual climate summary and clustering object
# output is very cluttered unless graphics device is large
res <- vizAnnualClimate(x$climate.annual, IQR.cex = 1.1, cex=1.1, pch=18)
print(res$fig)


par(mar=c(0,0,1,1))
plotProfileDendrogram(z, clust = res$clust, scaling.factor = 0.075, width = 0.3, y.offset = 1.25, label = 'abbreviated')
mtext('aqp::us.state.soils', side = 1, at = 0.5, adj = 0, line = -1.5, font=4)
mtext('sorted by annual climate summaries', side = 3, at = 0.5, adj = 0, line = -1.5, font=3)


# taxonomic breakdown
# too messy with all of the soils
SoilTaxonomyDendrogram(x$SPC[1:10, ], label = 'abbreviated', width = 0.3)


# weave rug


