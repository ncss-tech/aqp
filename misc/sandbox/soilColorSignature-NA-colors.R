library(aqp)
library(cluster)
library(ape)
library(soilDB)
library(sharpshootR)
library(MASS)
library(corrplot)
library(farver)

# s.list <- c('amador', 'fresno', 'inks', 'pardee', 'toomes', 'henneke', 'crimeahouse', 'abes', 'banker')

## NA in hex-encoded colors was previously (aqp < 2.3.1) misinterpreted


s.list <- c('amador', 'banker', 'lucy')

# get these soil series
s <- fetchOSD(s.list)

plotSPC(s)

sig <- soilColorSignature(s, color = 'soil_color', method = 'depthSlices', perceptualDistMat = FALSE)

# d <- soilColorSignature(s, color = 'soil_color', method = 'depthSlices', perceptualDistMat = TRUE)


sig <- soilColorSignature(s, color = 'soil_color', method = 'pam', perceptualDistMat = FALSE)


sig

row.names(sig) <- sig$id
d <- daisy(sig[, -1])
h <- diana(d)

par(mar = c(1, 0, 0, 3))
plotProfileDendrogram(s, h, width = 0.33, cex.names = 0.45, shrink = TRUE, name.style = 'center-center', rotateToProfileID = TRUE, max.depth = 150)


s$m <- sprintf('%s %s/%s', s$hue, s$value, s$chroma)

plotProfileDendrogram(s, h, width = 0.33, cex.names = 0.45, shrink = TRUE, name.style = 'center-center', rotateToProfileID = TRUE, max.depth = 150, name = 'hue')




.n <- (ncol(sig) - 1) / 3
.colgroups <- lapply(1:.n, function(i) {
  .idx <- sprintf("%s.%s", c('L', 'A', 'B'), i)
  .col <- rgb(convert_colour(sig[, .idx], from = 'lab', to = 'rgb', white_from = 'D65', white_to = 'D65'), maxColorValue = 255)
  return(.col)
})

colorspace::swatchplot(.colgroups)

mtext(side = 1, text = paste(profile_id(s), collapse = ','))


