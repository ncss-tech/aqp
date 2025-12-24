library(aqp)
library(cluster)
library(ape)
library(soilDB)
library(sharpshootR)

s.list <- c('amador', 'redding', 'pentz', 'willows', 'pardee', 'yolo', 'hanford', 'cecil', 'sycamore', 'KLAMATH', 'MOGLIA', 'vleck', 'drummer', 'CANEYHEAD', 'musick', 'sierra', 'HAYNER', 'zook', 'argonaut', 'PALAU')

# get these soil series
s <- fetchOSD(s.list)

plotSPC(s)

## TODO
# * NA in soil colors ?
#     s$soil_color[5] <- NA


## colorBucket

pig <- soilColorSignature(s, color = 'soil_color', method = 'colorBucket')
row.names(pig) <- pig[, 1]

d <- daisy(pig[, -1], stand = FALSE)
dd <- diana(d)

d <- daisy(pig[, -1], stand = TRUE)
dd.stand <- diana(d)

d <- daisy(pig[, -1], metric = 'gower', weights = c(0.85, 1, 1, 1, 1))
dd.wt <- diana(d)



par(mar = c(0, 0, 1, 1), mfrow = c(2, 1))

plotProfileDendrogram(s, dd, width = 0.33, cex.names = 0.45, shrink = TRUE, name.style = 'center-center', max.depth = 200, scaling.factor = 0.0045, name = NA, rotateToProfileID = TRUE)
title('colorBucket: Euclidean dist', line = 0, cex.main = 0.8)

plotProfileDendrogram(s, dd.stand, width = 0.33, cex.names = 0.45, shrink = TRUE, name.style = 'center-center', max.depth = 200, scaling.factor = 0.08, name = NA, rotateToProfileID = TRUE)
title('colorBucket: Euclidean dist, standardization', line = 0, cex.main = 0.8)


plotProfileDendrogram(s, dd.stand, width = 0.33, cex.names = 0.45, shrink = TRUE, name.style = 'center-center', max.depth = 200, scaling.factor = 0.08, name = NA, rotateToProfileID = TRUE)
title('colorBucket: Euclidean dist, standardization', line = 0, cex.main = 0.8)

plotProfileDendrogram(s, dd.wt, width = 0.33, cex.names = 0.45, shrink = TRUE, name.style = 'center-center', max.depth = 200, scaling.factor = 0.006, name = NA, rotateToProfileID = TRUE)
title('colorBucket: Gower dist, L-downweighted', line = 0, cex.main = 0.8)



## depthSlices vs. pam

d <- soilColorSignature(s, color = 'soil_color', method = 'depthSlices', perceptualDistMat = TRUE)
dd.slices <- diana(d)

d <- soilColorSignature(s, color = 'soil_color', method = 'pam', perceptualDistMat = TRUE)
dd.pam <- diana(d)


par(mar = c(0, 0, 1, 1), mfrow = c(2, 1))

plotProfileDendrogram(s, dd.slices, width = 0.33, cex.names = 0.45, shrink = TRUE, name.style = 'center-center', max.depth = 200, name = NA, rotateToProfileID = TRUE)
title('depthSlices: perceptual dist', line = 0, cex.main = 0.8)

plotProfileDendrogram(s, dd.pam, width = 0.33, cex.names = 0.45, shrink = TRUE, name.style = 'center-center', max.depth = 200, name = NA, rotateToProfileID = TRUE)
title('PAM: perceptual dist', line = 0, cex.main = 0.8)





pig <- soilColorSignature(s, color = 'soil_color', method = 'depthSlices')

pig <- soilColorSignature(s, color = 'soil_color', method = 'depthSlices', prob = 0.5)



pig <- soilColorSignature(s, color = 'soil_color', method = 'pam', pam.k = 3)


# move row names over for distance matrix
row.names(pig) <- pig[, 1]
d <- daisy(pig[, -1], stand = FALSE)
dd <- diana(d)

par(mar=c(0,0,1,1))
plotProfileDendrogram(s, dd, width = 0.33, cex.names = 0.45, shrink = TRUE, name.style = 'center-center', max.depth = 210)

# 
row.names(pig) <- pig[, 1]
d <- daisy(pig[, -1], stand = TRUE)
dd <- diana(d)

par(mar=c(0,0,1,1))
plotProfileDendrogram(s, dd, width = 0.33, cex.names = 0.45, shrink = TRUE, name.style = 'center-center', max.depth = 210)


##
## nMDS is a better way to view distances
##
library(MASS)

d <- soilColorSignature(s, color = 'soil_color', method = 'depthSlices', perceptualDistMat = TRUE)
mds <- sammon(d)

par(mar = c(1, 1, 1, 1), xpd = NA)
plot(mds$points, type = 'n', axes = FALSE)
abline(h = 0, v = 0, lty = 3)
text(mds$points[, 1], mds$points[, 2], labels = row.names(mds$points), cex = 0.8, font = 2, xlab = '', ylab = '')


# re-scale nMDS axis 1 to the typical horizontal scale used by plotSPC 
xoff <- aqp:::.rescaleRange(mds$points[, 1], x0 = 1, x1 = length(s))

# adjust to reduce overlap
# with an expansion of the x-axis out to length(s) + 5
set.seed(10110)
xoff.fixed <- fixOverlap(xoff, thresh = 0.4, min.x = 1, max.x = length(s) + 5, method = 'S')

# re-scale nMDS axis 2 to the typical vertical scale used by plotSPC  
yoff <- aqp:::.rescaleRange(mds$points[, 2], x0 = -10, x1 = max(s))

par(mar = c(0.25, 0.25, 0.25, 0.25))

plotSPC(s, y.offset = yoff, relative.pos = xoff.fixed, width = 0.25, name = NA, scaling.factor = 0.5, shrink = TRUE, divide.hz = FALSE)

box()



## TODO: interpret MDS axes
#   MDS1 ~ lightness
#   MDS2 ~ redness

set.seed(10110)
pos <- alignTransect(yoff, x.min = 1, x.max = length(s) + 4, thresh = 0.7)

par(mar = c(0.25, 0.25, 0.25, 0.25))

plotSPC(s, n = length(s) + 4, y.offset = yoff[pos$order], plot.order = pos$order, relative.pos = pos$relative.pos, width = 0.25, name.style = 'center-center', scaling.factor = 0.8, shrink = TRUE)




############## 

k <- 3
pig <- soilColorSignature(s, color = 'soil_color', method = 'depthSlices', pam.k = 3)

# iterate over clusters, result is a distance matrix (delta-E00)
delta.E00 <- lapply(1:k, function(i) {
  # LAB coordinates are named by cluster 1:k
  v.names <- paste(c('L', 'A', 'B'), i, sep = '.')
  # pair-wise delta-E00
  d.i <- farver::compare_colour(pig[, v.names], pig[, v.names], from_space = 'lab', white_from = 'D65', method = 'cie2000')
  # copy over SPC ids
  dimnames(d.i) <- list(pig[, 1], pig[, 1])
  # convert to dist object
  d.i <- as.dist(d.i)
  return(d.i)
})

# sum distance matrices
d <- Reduce('+', delta.E00)
# divisive clustering
dd <- diana(d)

par(mar=c(0,0,1,1))
plotProfileDendrogram(s, dd, width = 0.33, cex.names = 0.5, shrink = TRUE, name.style = 'center-center', max.depth = 210)



d <- soilColorSignature(s, color = 'soil_color', method = 'depthSlices', perceptualDistMat = TRUE)
d <- soilColorSignature(s, color = 'soil_color', method = 'pam', perceptualDistMat = TRUE)

d1 - d2




d <- soilColorSignature(s, color = 'soil_color', method = 'depthSlices', perceptualDistMat = TRUE)
dd <- diana(d)

par(mar=c(0,0,1,1))
plotProfileDendrogram(s, dd, width = 0.33, cex.names = 0.5, shrink = TRUE, name.style = 'center-center', max.depth = 210)



d <- soilColorSignature(s, color = 'soil_color', method = 'depthSlices', perceptualDistMat = TRUE, prob = 1)
dd <- diana(d)

par(mar=c(0,0,1,1))
plotProfileDendrogram(s, dd, width = 0.33, cex.names = 0.5, shrink = TRUE, name.style = 'center-center', max.depth = 210)




