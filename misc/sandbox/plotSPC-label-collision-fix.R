library(soilDB)

## test single-horizon SPC
x <- data.frame(
  id = 'BOGUS',
  top = 0,
  bottom = 100,
  name = 'NODATA'
)

# init
depths(x) <- id ~ top + bottom
hzdesgnname(x) <- 'name'
x$distinctness <- NA

plotSPC(x, fixLabelCollisions = TRUE, hz.depths = TRUE, hz.depths.offset = 0.1, cex.names = 2)




x <- fetchOSD(c('ames', 'zook', 'clarksville', 'fullerton', 'inks', 'vleck', 'pardee', 'cecil', 'pierre'))

x$hzd <- hzDistinctnessCodeToOffset(x$distinctness, codes = c('abrupt', 'clear', 'gradual', 'diffuse'))

# x <- fetchOSD('clarksville')

par(mar = c(0, 0, 0, 0), mfrow = c(2, 1))
par(mar = c(0, 0, 0, 0))

plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 0.8)
plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 0.8, fixLabelCollisions = TRUE)

plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 1)
plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 1, fixLabelCollisions = TRUE)

plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 0.66)
plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 0.66, fixLabelCollisions = TRUE)



dev.off()
par(mar = c(0, 0, 0, 0))
plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 0.8, fixLabelCollisions = TRUE, hz.depths.offset = 0.05)




plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 0.8, fixLabelCollisions = TRUE, hz.depths.offset = 0.08, hz.distinctness.offset = 'hzd')

plotSPC(x, hz.depths = TRUE, name.style = 'center-center', plot.depth.axis = FALSE, cex.names = 0.8, fixLabelCollisions = TRUE, hz.depths.offset = 0.08)



## attempt in a dendrogram, checking that scaling / offset applied correctly


library(soilDB)
library(cluster)
library(ape)
library(colorspace)
library(sharpshootR)

s.list <- c('amador', 'redding', 'pentz', 'willows', 'pardee', 'yolo', 'hanford', 'cecil', 'sycamore', 'KLAMATH', 'MOGLIA', 'vleck', 'drummer', 'CANEYHEAD', 'sierra', 'PALAU')

# get these soil series
s <- fetchOSD(s.list)

s$hzd <- hzDistinctnessCodeToOffset(s$distinctness, codes = c('abrupt', 'clear', 'gradual', 'diffuse'))

# manually convert Munsell -> sRGB
rgb.data <- munsell2rgb(s$hue, s$value, s$chroma, return_triplets = TRUE)
s$r <- rgb.data$r
s$g <- rgb.data$g
s$b <- rgb.data$b

pig <- soilColorSignature(s, RescaleLightnessBy = 5, method = 'pam', pam.k = 3)


row.names(pig) <- pig[, 1]
d <- daisy(pig[, -1])
dd <- diana(d)


par(mar=c(0,0,0,0), bg = 'black', fg = 'white')

plotProfileDendrogram(s, dd, dend.y.scale = max(d) * 2, scaling.factor = 0.4, y.offset = 5, width = 0.28, cex.names = 0.66, name.style = 'center-center', shrink = TRUE, hz.depths = TRUE, cex.id = 0.66, plot.depth.axis = FALSE, fixLabelCollisions = TRUE, hz.depths.offset = 0.05, hz.distinctness.offset = 'hzd')

