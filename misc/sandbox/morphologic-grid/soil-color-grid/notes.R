
# see colordistance package for a similar, and likely more efficient approach

library(aqp)
library(raster)
library(rasterVis)
library(farver)
library(cluster)
library(MASS)
library(e1071)
library(rgeos)

## other nice images
# https://www.flickr.com/photos/soilscience/5105276422/in/album-72157625093942533/
# https://www.flickr.com/photos/soilscience/5104685533/in/album-72157625093942533/

## borrowing this image, cropped for clarity
# saved to aqp/misc/sandbox/morphologic-grid/soil-color-grid
# https://www.flickr.com/photos/soilscience/5104737905/in/album-72157625093942533/
(r <- brick('leaf-john-kelley.jpg'))

# check
plotRGB(r)

# number of samples
# used to "snap" pixel stacks to a reduced set of colors via PAM(dE00)
n <- 1000

# number of medoids / clusters ("color-snapping")
k <- 8

# convert sRGB stack to CIELAB
r.lab <- r
r.lab[] <- convert_colour(r[], from = 'rgb', to = 'lab', white_from = 'D65')

# looks correct
levelplot(r.lab, scales = list(draw = FALSE))

# sample for efficiency
s <- data.frame(
  sampleRegular(r.lab, size = n)
)
s <- na.omit(s)
str(s)


## ideas, balancing efficiency / perceptual constraints

## couple of options:
# 1. lookup Munsell chip for each stack of pixels (very slow)
# 2. L1 median CIELAB / Munsell chip for each "zone" of pixels (fast, perceptually reasonable)
# 3. "snap" colors via PAM to a very small number of possible colors (very fast, usually unique chips)

# assignment of a "group" of pixels to a single Munsell chip using:
# * window mean(CIELAB -> Munsell chip) (fast / loss of contrast)
# * majority after clustering (fast)
# * subtractive mixture (very slow)
# * window L1 median(CIELAB) -> Munsell chip (ideal?)


## L1 median over sRGB stack
# aggregate to larger cell size (possible change in extent)
# vectorize
# iterate over vector zones
# sample overlapping sRGB pixels
# convert to hex notation
# colorQuantiles()
# `L1` component
# rasterize
# fix extent
# stack
# setup RAT
# visualize

# # make zones, note that they may extend beyond original extent
# z <- rasterToPolygons(aggregate(r.lab, fact = 24, fun = mean, expand = TRUE), dissolve = FALSE)
# plot(r.lab[[1]])
# plot(z, add=TRUE)
# e <- extract(r, z[1, ])[[1]]
# colorQuantiles(rgb(e / 255), p = 0.5)



## all the fuss may not be warranted, these aren't color-calibrated images!

## option 3 for now, this is the simplest but least precise


# if using PAM, it is more appropriate to use dE00 as the distance metric
# notes from: http://ncss-tech.github.io/AQP/aqp/soil-color-signatures.html
d <- compare_colour(s[, c('l', 'a', 'b')], from_space='lab', white_from = 'D65', method='cie2000')
d <- as.dist(t(d))

# a little slower, but using dE00 as distance metric
# this is possible when using samples collected from full stack
p <- pam(d, k = k, stand = FALSE, diss = TRUE)

# save clustering vector to samples
s$clust <- p$clustering

# supervised classification via LDA (very simplistic / fast)
(m <- lda(clust ~ l + a + b, data = s))

# predictions at samples
x <- predict(m, s)

# how well did it work?
# ~ 94% PCC
tab <- table(x$class, s$clust)
classAgreement(tab, match.names = TRUE)

## reduce grid size: see window function options above

# simplest / loss of contrast likely
# window mean(CIELAB -> Munsell chip) (fast / loss of contrast)
# expand = FALSE ensures that extent is preserved
r.lab.agg <- aggregate(r.lab, fact = 9, fun = mean)

# apply LDA prediction of clusters to entire stack
r.clust <- predict(r.lab.agg, m)

# these are classes
r.clust <- ratify(r.clust)

# looks OK
levelplot(r.clust, att = 'ID', margin = FALSE, scales = list(draw = FALSE))

# construct RAT
rat <- data.frame(ID = 1:k, s[p$medoids, c('l', 'a', 'b')])

# CIELAB -> sRGB -> Munsell
rat.sRGB <- convert_colour(rat[, c('l', 'a', 'b')], from = 'lab', to = 'rgb', white_from = 'D65')
rat.munsell <- rgb2munsell(rat.sRGB / 255)

# merge RAT components
rat <- cbind(
  rat,
  rat.sRGB,
  rat.munsell
)

# Munsell chip label and hex color
rat$munsell <- sprintf("%s %s/%s", rat$hue, rat$value, rat$chroma)
rat$color <- parseMunsell(rat$munsell)

# check
str(rat)

# how many times is each chip used?
# if any are > 1 then we can't easily use the following
(tab <- table(rat$munsell))

if(all(tab < 2)) {
  # hue ordering
  rat$hue <- factor(rat$hue, levels = huePosition(returnHues = TRUE), ordered = TRUE)
  # ordering using hue / value / chroma
  o <- order(rat$hue, rat$value, rat$chroma)
  # set factor levels
  rat$munsell <- factor(rat$munsell, levels = rat$munsell[o])
} else {
  print('duplicate Munsell chips, more work to do...')
}


# pack RAT
levels(r.clust) <- rat

# helper indices / ordering for custom color key
idx <- order(rat$munsell)
col.at <- 1:(length(idx) + 1)

# check order of colors / Munsell chip labels
soilPalette(rat$color[idx], lab = rat$munsell[idx])


# color key object for rasterVis::levelplot()
ck <- list(
  at = col.at,
  labels = list(
    labels = rat$munsell[idx],
    at = col.at + 0.5
  ),
  col = rat$color[idx]
)

# colors from Munsell chips in RAT
# custom color key with color chips / labels sorted by hue/value/chroma
levelplot(
  r.clust, att = 'munsell', 
  col.regions = rat$color, 
  margin = FALSE, scales = list(draw = FALSE),
  colorkey = ck
  )





# side by side in base graphics requires some tinkering
par(mfcol = c(1, 2), mar = c(4, 1, 0, 1))
plotRGB(r, margins = TRUE)
box()
plot(r.clust, col = rat$color, axes = FALSE, legend = FALSE)
# must tinker with `inset` and `ncol`
legend('bottom', legend = rat$munsell[idx], col = rat$color[idx], pch = 15, pt.cex = 2, bty = 'n', ncol = 4, inset = -0.1, xpd = TRUE, cex = 0.85)






#### not quite ready

# # images may not co-register if aggregated grid extends beyond original extent
# plotRGB(r)
# plot(r.clust, col = rat$color, axes = FALSE, legend = FALSE, alpha = 0.5, add = TRUE)
# 
# 
# # hmm
# plot(r.lab[['l']], axes = FALSE, legend = FALSE, col = grey.colors(255, start = 0, end = 1))
# plot(r.clust, col = rat$color, axes = FALSE, legend = FALSE, alpha = 0.5, add = TRUE)
# 
# 
# # faster version:
# # https://rdrr.io/github/adamlilith/fasterRaster/man/fasterVectorize.html
# 
# # convert grid -> polygons
# # 'layer' contains grid values
# # RAT is lost
# v <- rasterToPolygons(r.clust, dissolve = TRUE)
# 
# # copy attributes from RAT
# # row-order should be preserved, but just in case
# v$munsell <- rat$munsell[match(v$layer, rat$ID)]
# v$color <- rat$color[match(v$layer, rat$ID)]
# 
# # interesting
# plotRGB(r)
# plot(v, add = TRUE, col = scales::alpha(v$color, alpha = 0.5), border = 'white')
# legend('right', legend = rat$munsell[idx], col = rat$color[idx], pch = 15, pt.cex = 2, bty = 'n')
# 
# 
# # using 'L' coordinate
# plot(r.lab[['l']], axes = FALSE, legend = FALSE, col = grey.colors(255, start = 0, end = 1))
# plot(v, add = TRUE, col = scales::alpha(v$color, alpha = 0.75), border = 'white')
# 
# 
