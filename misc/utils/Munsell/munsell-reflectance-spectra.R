library(aqp)
library(soilDB)
library(reshape2)
library(lattice)

library(matrixStats)

## entirely different approach using sRGB only
# http://scottburns.us/wp-content/uploads/2015/04/ILSS.txt


# base spectral library:
# http://www.munsellcolourscienceforpainters.com/MunsellResources/SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt

# https://github.com/ncss-tech/aqp/issues/101

# address in the ST forum as well:
# https://soiltxnmyforum.cals.vt.edu/forum/read.php?3,1984,1987#msg-1987

# ideas here:
# http://en.wikipedia.org/wiki/Weighted_geometric_mean

# calculation here
# https://arxiv.org/ftp/arxiv/papers/1710/1710.06364.pdf


# check: OK
aqp:::.wgm(v = c(0.5, 0.8), w = c(0.5, 0.5))

## local, testing copies

# load interpolated spectral library
# m.rel <- readRDS('interpolated-Munsell-spectra.rds')

# load wide version used for spectral comparisons
# reference <- readRDS('interpolated-Munsell-spectra-wide.rds')

# singleton
mixMunsell(c('10YR 5/3'))

# invalid Munsell notation
mixMunsell(c('10YR 5/3', '10YR 3/99'))

mixMunsell(c('10YR 5/3', '10YR 3/2'))

mixMunsell(c('10YR 5/3', '10YR 3/2'), n = 3)

plotColorMixture(c('10YR 5/3', '10YR 3/2'))

plotColorMixture(c('10YR 6/2', '5YR 5/6'))

# is this right? can the resulting spectra be "higher" than the source?
plotColorMixture(c('10YR 6/2', '5YR 5/6'), w = c(2,1))

# label collision
plotColorMixture(c('10YR 5/3', '10YR 3/2', '5R 2/2'))
plotColorMixture(c('10YR 5/3', '10YR 3/2', '5R 2/2'), swatch.cex = 4, label.cex = 0.65)


mixMunsell(c('10YR 4/6', '5YR 2/2', '5Y 4/5'))

plotColorMixture(c('10YR 4/6', '2.5Y 6/2', '5Y 2/2'), w = c(1, 1, 2))

mixMunsell(c('10YR 4/6', '5YR 2/2'), w = c(0.8, 0.2))

mixMunsell(c('10YR 4/6', '2.5Y 5/4'))

mixMunsell(c('10YR 6/6', '5P 5/4'))

mixMunsell(c('10YR 4/4', '5GY 5/4'))

mixMunsell(c('5G 6/5', '5R 5/4'))
plotColorMixture(c('5G 6/5', '5R 5/4'), w = c(1, 2))
plotColorMixture(c('5G 6/5', '5R 5/4'), w = c(3, 1), swatch.cex = 4, label.cex = 0.65)

## how does soilDB::estimateColorMixture compare?

colors <- c('10YR 6/2', '7.5YR 3/3')

d <- cbind(
  parseMunsell(colors, convertColors=FALSE),
  parseMunsell(colors, return_triplets=TRUE, returnLAB=TRUE),
  pct=c(0.5, 0.5),
  col=parseMunsell(colors, convertColors=TRUE)
)

e1 <- estimateColorMixture(d, backTransform = TRUE)
m1 <- mixMunsell(colors)

par(bg = 'black', fg = 'white', mar = c(0,0,0,0))
colorContrastPlot(
  m1$munsell,
  sprintf('%s %s/%s', e1$colorhue, e1$colorvalue, e1$colorchroma),
  labels = c('Subtractive Mixture', 'Weighted Average CIELAB')  
  )


s <- seq(0, 1, by = 0.2)
z <- lapply(s, function(i) {
  
  m <- mixMunsell(c('10YR 6/2', '5YR 5/6'), w = c(i, 1-i))
  
  return(m$munsell)
})

z <- do.call('c', z)

par(bg = 'black', fg = 'white', mar = c(0,0,0,0))
soilPalette(parseMunsell(z), sprintf('%s (%s / %s))', z, s, 1 - s))

