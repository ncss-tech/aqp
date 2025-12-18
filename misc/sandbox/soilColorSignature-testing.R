library(aqp)
library(cluster)
library(ape)
library(soilDB)
library(sharpshootR)

s.list <- c('amador', 'redding', 'pentz', 'willows', 'pardee', 'yolo', 'hanford', 'cecil', 'sycamore', 'KLAMATH', 'MOGLIA', 'vleck', 'drummer', 'CANEYHEAD', 'musick', 'sierra', 'HAYNER', 'zook', 'argonaut', 'PALAU')

# get these soil series
s <- fetchOSD(s.list)

plotSPC(s)


pig <- soilColorSignature(s, color = 'soil_color', method = 'depthSlices')

pig <- soilColorSignature(s, color = 'soil_color', method = 'depthSlices', prob = 0.5)


pig <- soilColorSignature(s, color = 'soil_color', method = 'colorBucket')


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




s## 

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




