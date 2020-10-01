# https://cran.r-project.org/web/packages/Polychrome/

library(Polychrome)
library(aqp)
library(soilDB)
library(colorspace)


x <- fetchOSD('musick', colorState = 'dry')
m1 <- sprintf("%s %s/%s", x$hue, x$value, x$chroma)


cols <- parseMunsell(m1)
names(cols) <- m1

swatch(cols)
swatchHue(cols)
swatchLuminance(cols)

uvscatter(cols)
luminance(cols)
rancurves(cols)
plothc(cols)
plotpc(cols)


par(mfcol=c(2,2), mar=c(1,1,3,1))
swatch(cols, main='Original')
swatch(colorDeficit(cols, target = 'tritanope'), main='Tritanope')
swatch(colorDeficit(cols, target = 'deuteranope'), main='Deuteranope')
swatch(colorDeficit(cols, target = 'protanope'), main='Protanope')


par(mfcol=c(2,2), mar=c(1,1,3,1))
swatchplot(cols)
swatch(cols)
soilPalette(cols, lab=names(cols))


plotDistances(cols)
computeDistances(cols)

