## Demonstrate the differences between geometric / perceptual arrangement of hues
##
##

library(aqp)
library(sharpshootR)
library(vegan)
library(colorspace)

## Munsell value and chroma to combine with hues

# bright, for demonstration
value <- 6
chroma <- 15

# # closer to soil colors
# value <- 4
# chroma <- 6


# baseline set of hues
hues <- huePosition(returnHues = TRUE, includeNeutral = TRUE)

## hues on the unit circle

# ragg::agg_png(file = 'E:/temp/munsell-hue-circle.png', height = 900, width = 900, scaling = 1.75)

par(mar = c(0, 0, 0, 0), fg = 'white', bg = 'black')
huePositionCircle(hues, chroma = chroma, value = value)

# dev.off()


## simulate color vision deficiency

ragg::agg_png(file = 'E:/temp/munsell-hue-circle-cvd.png', height = 1800, width = 1700, scaling = 2.5)

par(mar = c(0, 0, 3, 0), fg = 'white', bg = 'black', mfrow = c(2,2))
huePositionCircle(hues, chroma = chroma, value = value)
title('Average Human Vision', col.main = 'white')

huePositionCircle(hues, chroma = chroma, value = value, simulateCVD = 'protan')
title('Protanope\nred deficient: L cone absent', col.main = 'white')

huePositionCircle(hues, chroma = chroma, value = value, simulateCVD = 'deutan')
title('Deuteranope\ngreen deficient: M cone absent', col.main = 'white')

huePositionCircle(hues, chroma = chroma, value = value, simulateCVD = 'tritan')
title('Tritanope\nblue deficient: S cone absent', col.main = 'white')

dev.off()


## average human perception via nMDS of CIE2000 color contrast

m <- sprintf("%s %s/%s", hues, value, chroma)
cols <- parseMunsell(m)

zz <- huePositionCircle(hues, value = value, chroma = chroma, plot = FALSE)
z <- previewColors(cols, method = 'MDS', labels = hues, pt.cex = 3)

# close, but ideally we should rotate to roughly match arrangement on the unit circle
plot(z, axes = FALSE, xlab = '', ylab = '', pch = 21, bg = cols, cex = 5)
text(z, labels = hues, col = invertLabelColor(cols), cex = 0.75)

# vegan::procrustes() to the rescue
pc <- procrustes(X = z, Y = zz[, c('x', 'y')])

plot(pc, cex = 3, ar.col = cols, lwd = 1, len = 0.1, bg = cols, pch = 21)

# apply transformation to hue circle
z.rot <- predict(pc, newdata = z)

# ragg::agg_png(file = 'E:/temp/munsell-hue-position.png', height = 900, width = 900, scaling = 1.75)

par(mar = c(0, 0, 0, 0), fg = 'white', bg = 'black')

plot(z.rot, axes = FALSE, xlab = '', ylab = '', type = 'n')

segments(x0 = z.rot[41, 1], y0 = z.rot[41, 2], x1 = z.rot[-41, 1], y1 = z.rot[-41, 2], col = 'grey')

points(z.rot, pch = 21, bg = cols, cex = 5)
text(z.rot, labels = hues, col = invertLabelColor(cols), cex = 0.66, font = 2)

points(z.rot[41, 1], z.rot[41, 2], pch = 21, bg = parseMunsell(sprintf('N %s/', value)), cex = 5.5)
text(z.rot[41, 1], z.rot[41, 2], labels = 'N', col = 'black', cex = 1, font = 2)


# dev.off()


## other options and sequential dE00 

par(mar = c(0, 0, 0, 0), fg = 'white', bg = 'black')

# from sharpshootR
huePositionPlot(chroma = 10, contour.dE00 = TRUE)

m <- sprintf("%s %s/%s", hues, 6, 10)
lab <- parseMunsell(m, convertColors = TRUE, returnLAB = TRUE)

dE00 <- sapply(1:(nrow(lab)-1), function(i) {
  farver::compare_colour(
    from = lab[i, ], 
    to = lab[i+1, ], 
    from_space = 'lab', 
    method = 'CIE2000', 
    white_from = 'D65'
  )
})

dE00




