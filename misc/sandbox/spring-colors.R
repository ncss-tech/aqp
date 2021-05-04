library(aqp)
library(grDevices)

x <- read.csv('spring-colors.csv')


x.sRGB <- convertColor(x[, c('L', 'A', 'B')], from = 'Lab', to = 'sRGB', from.ref.white = 'D65', to.ref.white = 'D65')
cols <- rgb(x.sRGB, maxColorValue = 1)
x.m <- rgb2munsell(x.sRGB)

mm <- sprintf("%s %s/%s", x.m$hue, x.m$value, x.m$chroma)

d <- data.frame(
  x,
  col = cols,
  m = mm
)


previewColors(d$col, labels = d$m)

previewColors(cols, method = 'MDS', labels = mm,  labels.cex = 0.5)

previewColors(cols, method = 'manual', col.order = 1:length(cols), labels = 1:length(cols))


previewColors(cols, method = 'manual', col.order = 1:length(cols), labels = mm)

dd <- split(d, d$source)

previewColors(dd[['paint']]$col, labels = dd[['paint']]$m)

previewColors(dd[['nature']]$col, labels = dd[['nature']]$m, method = 'MDS')


colorContrastPlot('10P 4/8', '10P 4/7')

cq <- colorQuantiles(dd[['nature']]$col)
plotColorQuantiles(cq)

devtplotColorMixture(c('10P 7/8', '7.5GY 6/7'), mixingMethod = 'exact')

