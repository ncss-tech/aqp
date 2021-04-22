library(aqp)

# Munsell reference spectra
data("munsell.spectra.wide")

# convert to closest Munsell chip
# sRGB -> Munsell conversion via rgb2Munsell()
spec2Munsell(munsell.spectra.wide[, '10YR 3/3'])

# attempt several
cols <- c('10YR 6/2', '5YR 5/6', '10B 4/4', '5G 4/4', '2.5Y 8/2', '10YR 3/3')

# most are exact or very close
z <- do.call(
  'rbind',
  lapply(cols, function(i) {
    spec2Munsell(munsell.spectra.wide[, i])  
  })
)

# format Munsell notation from pieces
z$m <- sprintf("%s %s/%s", z$hue, z$value, z$chroma)

# compare
colorContrastPlot(
  m1 = cols, 
  m2 = z$m, 
  labels = c('original', 'spectral\ninterpretation')
)

# do it again with different SO
z <- do.call(
  'rbind',
  lapply(cols, function(i) {
    spec2Munsell(munsell.spectra.wide[, i], SO = 'CIE1964')  
  })
)

# format Munsell notation from pieces
z$m <- sprintf("%s %s/%s", z$hue, z$value, z$chroma)

# compare
colorContrastPlot(
  m1 = cols, 
  m2 = z$m, 
  labels = c('original', 'spectral\ninterpretation')
)


# try different illuminants
spec2Munsell(munsell.spectra.wide[, '10YR 3/3'], illuminant = 'D65')  
spec2Munsell(munsell.spectra.wide[, '10YR 3/3'], illuminant = 'F2')  




# mix colors, return spectra, convert to color
cols <- c('10YR 6/2', '5YR 5/6', '10B 4/4')
res <- mixMunsell(cols, keepMixedSpec = TRUE, mixingMethod = 'reference')

# note that they are slightly different
res$mixed
spec2Munsell(res$spec)


cols <- c('10YR 6/2', '5YR 5/6', '10B 4/4')
plotColorMixture(cols, swatch.cex = 5, showMixedSpec = TRUE)

res <- mixMunsell(cols, keepMixedSpec = TRUE)
spec2Munsell(res$spec)

cols <- c('10YR 6/2', '5YR 5/6', '10B 4/4')
mixMunsell(cols, mixingMethod = 'estimate')
mixMunsell(cols, mixingMethod = 'reference')
mixMunsell(cols, mixingMethod = 'exact')

plotColorMixture(c('10YR 6/2', '5YR 5/6'), w = c(2,1), swatch.cex = 5, showMixedSpec = TRUE)

res <- mixMunsell(c('10YR 6/2', '5YR 5/6'), w = c(2,1), keepMixedSpec = TRUE)
spec2Munsell(res$spec)

colorContrastPlot(m1 = '5Y 6/3', m2 = '7.5YR 6/4')



cols <- c('10YR 6/2', '5YR 5/6')
wts <- c(2,1)

mx1 <- mixMunsell(cols, w = wts, mixingMethod = 'reference')
mx2 <- mixMunsell(cols, w = wts, mixingMethod = 'exact')

soilPalette(parseMunsell(c(cols, mx1$munsell)), lab = c(cols, mx1$munsell))
soilPalette(parseMunsell(c(cols, mx2$munsell)), lab = c(cols, mx2$munsell))

colorContrastPlot(m1 = mx1$munsell, m2 = mx2$munsell, labels = c('reference', 'exact'))

plotColorMixture(cols, w = wts, swatch.cex = 5, showMixedSpec = TRUE)
plotColorMixture(cols, w = wts, swatch.cex = 5, showMixedSpec = TRUE, mixingMethod = 'exact')




# ~ 5 minutes
# do all the colors
nm <- dimnames(munsell.spectra.wide)[[2]]
z <- lapply(2:ncol(munsell.spectra.wide), function(i) {

  res <- spec2Munsell(munsell.spectra.wide[, i], SO = 'CIE1964')
  res$label <- nm[i]

  return(res)
}
)

z <- do.call('rbind', z)

z$pred <- sprintf("%s %s/%s", z$hue, z$value, z$chroma)

# CIE1931 SO: ~ 20% not exact matches
# CIE1964 SO: ~ 80% not exact matches
prop.table(table(z$label == z$pred))

# non-matching
idx <- which(z$label != z$pred)

idx.sub <- sample(idx, size = 10)

colorContrastPlot(m1 = z$label[idx.sub], m2 = z$pred[idx.sub], labels = c('original', 'spectral\ninterpretation'))



head(z[idx, ], 20)
tail(z[idx, ], 20)


library(lattice)
library(tactile)

g <- make.groups(
  'matching' = z[-idx, ],
  'not matching' = z[idx, ]
)

# dE00 is reported by rgb2Munsell()
bwplot(
  which ~ sigma,
  data = g,
  notch = TRUE,
  varwidth = TRUE,
  par.settings = tactile.theme(),
  xlab = expression(Delta*E['00']),
  main = 'Spectral Interpretation\nMunsell Reference Spectra'
)






