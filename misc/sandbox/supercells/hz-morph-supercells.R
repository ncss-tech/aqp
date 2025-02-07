# https://jakubnowosad.com/supercells/
library(supercells)
library(aqp)
library(terra)
library(sf)


x <- rast('leaf-john-kelley.jpg')

s <- supercells(x, k = 7, compactness = 30, transform = 'to_LAB', verbose = 2, avg_fun = median)
s <- vect(s)

plotRGB(x)
lines(s, col = 'yellow')

cols <- rgb(s$leaf.john.kelley_1, s$leaf.john.kelley_2, s$leaf.john.kelley_3, maxColorValue = 255)

par(mfcol = c(1, 2))
plotRGB(x, mar = c(0, 0, 0, 0))
plot(s, col = cols, border = 'white', mar = c(0, 0, 0, 0), axes = FALSE)

m <- col2Munsell(cbind(s$leaf.john.kelley_1, s$leaf.john.kelley_2, s$leaf.john.kelley_3))
m

# question: does avg_fun work on transformed or original values?

# https://github.com/Nowosad/supercells/issues/22
