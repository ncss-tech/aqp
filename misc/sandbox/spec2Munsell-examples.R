library(aqp)

data("munsell.spectra.wide")


spec2Munsell(munsell.spectra.wide[, '10YR 3/3'])

cols <- c('10YR 6/2', '5YR 5/6', '10B 4/4', '5G 4/4', '2.5Y 8/2', '10YR 3/3')

# not all colors are returned exactly
z <- do.call(
  'rbind',
  lapply(cols, function(i) {
    spec2Munsell(munsell.spectra.wide[, i])  
  })
)

z$m <- sprintf("%s %s/%s", z$hue, z$value, z$chroma)

colorContrastPlot(m1 = cols, m2 = z$m, labels = c('original', 'spectral\ninterpretation'))


cols <- c('10YR 6/2', '5YR 5/6', '10B 4/4')
plotColorMixture(cols, swatch.cex = 5, showMixedSpec = TRUE)

res <- mixMunsell(cols, keepMixedSpec = TRUE)
spec2Munsell(res$spec)

mixMunsell(cols, mixingMethod = 'estimate')
mixMunsell(cols, mixingMethod = 'spectra')
mixMunsell(cols, mixingMethod = 'reference')
mixMunsell(cols, mixingMethod = 'exact')

plotColorMixture(c('10YR 6/2', '5YR 5/6'), w = c(2,1), swatch.cex = 5, showMixedSpec = TRUE)

res <- mixMunsell(c('10YR 6/2', '5YR 5/6'), w = c(2,1), keepMixedSpec = TRUE)
spec2Munsell(res$spec)

colorContrastPlot(m1 = '5Y 6/3', m2 = '7.5YR 6/4')





# ~ 5 minutes
# # do all the colors
# nm <- dimnames(munsell.spectra.wide)[[2]]
# z <- lapply(2:ncol(munsell.spectra.wide), function(i) {
#   
#   res <- spec2Munsell(munsell.spectra.wide[, i])
#   res$label <- nm[i]
#   
#   return(res)
# }
# )
# 
# z <- do.call('rbind', z)
# 
# z$pred <- sprintf("%s %s/%s", z$hue, z$value, z$chroma)
# 
# table(z$label == z$pred)
# idx <- which(z$label != z$pred)
# 
# 
# colorContrastPlot(m1 = z$label[idx[1:10]], m2 = z$pred[idx[1:10]], labels = c('original', 'spectral\ninterpretation'))
# 
# 
# 
# head(z[idx, ], 20)
# tail(z[idx, ], 20)
# 
# 
# hist(z$sigma[idx])
