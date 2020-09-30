# minified SPCs
library(aqp)
library(soilDB)
library(cluster)
library(sharpshootR)
library(dendextend)

# get some different looking soils from around the country
f <- fetchOSD(c("Agawam","Houston Black","Worsham","Musick",
                "Scituate","Valentine","Pierre","Cecil"))

# we want to use blacklock but it has an oldschool O horizon (described in depths above 0)
blklock <- fetchOSD('Blacklock')
blklock@horizons <- blklock@horizons[c(2,1,3:9),]
delta_cm <- abs(blklock$bottom - blklock$top) * 2.54

# cumulative sum of hz thickness from zero, drop last value for top depth
blklock@horizons$top <- round(cumsum(c(0, delta_cm[1:nrow(blklock@horizons) - 1])))

# bottom depth is even simpelr, just cumsum
blklock@horizons$bottom <- round(cumsum(delta_cm))

# combine blacklock in with the others
f <- pbindlist(list(blklock, f))

## truncate to ~100cm
## clever!
## maybe a new convenience function truncate(SPC, bottom) ?

## probably not neccessary to use both of these together?
## one or the other should suffice, glom* is probably more efficient
f <- glomApply(f, function(p) c(0,99), truncate = TRUE)
# f <- slice(f, 0:98 ~ soil_color + hue + value + chroma)


## interesting idea!
## profile_compare with [L, A, B] coordinates is another approach
horizons(f) <- horizonColorIndices(f, "hue", "value", "chroma")

d <- profile_compare(f,
                     vars=c("barron_torrent_redness", "buntley_westin"),
                     max_d=100,
                     k=0, rescale.result=TRUE)

clust <- cluster::diana(d)

# arbitrary / optional rotatation of dendrogram
hc <- rotate(as.hclust(clust), order=sample(1:9))

# hang profiles from dendrogram
# ordering is based on profile IDs propagated from distance matrix -> clustering object
plotProfileDendrogram(f, hc, plot.depth.axis=FALSE, name=NA, dvide.hz=FALSE, width=0.4, print.id=FALSE)


## curious to see how soil colr signature would perform
## I'm thinking that the last method is likely the "best" / most generic
## https://ncss-tech.github.io/AQP/aqp/soil-color-signatures.html

## this is annoying prep work
# manually convert Munsell -> sRGB
rgb.data <- munsell2rgb(f$hue, f$value, f$chroma, return_triplets = TRUE)
f$r <- rgb.data$r
f$g <- rgb.data$g
f$b <- rgb.data$b

## a palette of 3 colors should be enough
k <- 3
pig <- soilColorSignature(f, RescaleLightnessBy = 5, method='pam', pam.k=k)

# iterate over clusters, result is a distance matrix (delta-E00)
delta.E00 <- lapply(1:k, function(i) {
  # LAB coordinates are named by cluster 1:k
  v.names <- paste(c('L', 'A', 'B'), i, sep = '.')
  # delta-E00
  d.i <- farver::compare_colour(pig[, v.names], from_space='lab', white_from = 'D65', method='cie2000')
  # copy over SPC ids
  dimnames(d.i) <- list(pig[, 1], pig[, 1])
  # convert to dist object
  d.i <- as.dist(t(d.i))
  return(d.i)
})

# sum distance matrices
d <- Reduce('+', delta.E00)
dd <- diana(d)

## further tinkering required to sufficiently fill the space
# png(width=250, height=250, filename="test.png", type = 'cairo', antialias = 'subpixel')

par(mar=c(0,0,0,0))
plotProfileDendrogram(f, dd, dend.y.scale = max(d) * 2, scaling.factor = 0.7, y.offset = max(d) / 10, plot.depth.axis=FALSE, name=NA, dvide.hz=FALSE, width=0.4, print.id=FALSE)

# dev.off()

# png(width=250, height=250, filename="test.png")
# par(mfrow=c(2,1), mai=c(0,0,0.166,0))
# plot(dend.reorder,
#      yaxt="n",
#      leaflab="none",
#      type = "rectangle",
#      edgePar = list(col = c(1), lwd = 1, lty=c(1,3)))
# par(mar=c(0,0,0,0))
# plotSPC(f, name = NA, divide.hz=F, print.id = F, width = 0.4, y.offset=-5,
#         x.idx.offset = 0.25, plot.depth.axis = F, plot.order = order.dendrogram(dend.reorder))
# dev.off()

