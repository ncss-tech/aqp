library(aqp)
library(soilDB)
library(sharpshootR)

s.list <- c('amador', 'redding', 'pentz', 'willows', 'pardee', 'yolo', 'hanford', 'cecil', 'sycamore', 'KLAMATH', 'MOGLIA', 'vleck', 'drummer', 'CANEYHEAD', 'musick', 'sierra', 'HAYNER', 'zook', 'argonaut', 'PALAU')

# get these soil series
s <- fetchOSD(s.list)

# manually convert Munsell -> sRGB
rgb.data <- munsell2rgb(s$hue, s$value, s$chroma, return_triplets = TRUE)
s$r <- rgb.data$r
s$g <- rgb.data$g
s$b <- rgb.data$b

pig.1 <- soilColorSignature(s, RescaleLightnessBy = 5, method = 'colorBucket')
pig.2 <- soilColorSignature(s, RescaleLightnessBy = 5, method='depthSlices')
pig.3 <- soilColorSignature(s, RescaleLightnessBy = 5, method = 'pam', pam.k = 3)

head(pig.1)




pig.1.new <- soilColorSignature(s, RescaleLightnessBy = 5, method = 'colorBucket')
pig.2.new <- soilColorSignature(s, RescaleLightnessBy = 5, method='depthSlices')
pig.3.new <- soilColorSignature(s, RescaleLightnessBy = 5, method = 'pam', pam.k = 3)

all.equal(pig.1, pig.1.new)
all.equal(pig.2, pig.2.new)
all.equal(pig.3, pig.3.new)


pig <- soilColorSignature(s, RescaleLightnessBy = 5, method='depthSlices')
row.names(pig) <- pig[, 1]
d <- daisy(pig[, -1])
dd <- diana(d)


par(mar=c(0,0,1,1))
plotProfileDendrogram(s, dd, dend.y.scale = max(d) * 2, scaling.factor = 0.25, y.offset = 6, width=0.25, cex.names=0.45)
