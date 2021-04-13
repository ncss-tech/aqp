library(aqp)

data("munsell.spectra.wide")


m <- '5B 6/10'
x <- munsell.spectra.wide[, m]

spec2Munsell(x)

cols <- c('10YR 6/2', '5YR 5/6', '10B 4/4')
plotColorMixture(cols, swatch.cex = 5, showMixedSpec = TRUE)

res <- mixMunsell(cols, keepMixedSpec = TRUE)
spec2Munsell(res$spec)


plotColorMixture(c('10YR 6/2', '5YR 5/6'), w = c(2,1), swatch.cex = 5, showMixedSpec = TRUE)

res <- mixMunsell(c('10YR 6/2', '5YR 5/6'), w = c(2,1), keepMixedSpec = TRUE)
spec2Munsell(res$spec)

colorContrastPlot(m1 = '5Y 6/3', m2 = '7.5YR 6/4')

