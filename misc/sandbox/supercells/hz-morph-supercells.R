# https://jakubnowosad.com/supercells/
library(terra)
library(aqp)
library(sf)
library(supercells)

x <- rast('leaf-john-kelley.jpg')

s <- supercells(x, k = 7, compactness = 30, transform = 'to_LAB', verbose = 2, avg_fun = median)

plotRGB(x)
plot(st_geometry(s), add = TRUE, border = 'yellow')

cols <- rgb(s$leaf.john.kelley_1, s$leaf.john.kelley_2, s$leaf.john.kelley_3, maxColorValue = 255)

par(mfcol = c(1, 2))
plotRGB(x, mar = c(0, 0, 0, 0))
par(mar = c(0, 0, 0, 0))
plot(st_geometry(s), col = cols, border = NA)

m <- rgb2munsell(cbind(s$leaf.john.kelley_1, s$leaf.john.kelley_2, s$leaf.john.kelley_3) / 255)

# question: does avg_fun work on transformed or original values?

# https://github.com/Nowosad/supercells/issues/22
