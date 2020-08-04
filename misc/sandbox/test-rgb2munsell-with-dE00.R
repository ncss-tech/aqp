library(aqp)
library(colorspace)
library(farver)

testColorConversion <- function(seed='10YR 3/4', ...) {
  x <- rgb2munsell(color = parseMunsell(seed, return_triplets=TRUE), ...)
  x$munsell <- sprintf("%s %s/%s", x$hue, x$value, x$chroma)
  x$color <- munsell2rgb(x$hue, x$value, x$chroma)
  
  cc <- colorContrast(m1=rep(seed, times=nrow(x)), m2=x$munsell)
  x$dE00 <- cc$dE00
  
  # x$label <- sprintf("%s\n %s", x$munsell, round(x$dE00, 2))
  x$label <- sprintf("%s\ndE00: %s", x$munsell, round(x$dE00, 2))
  
  return(x)  
}


n <- 6
s <- '10YR 3/5'


## not right: demonstration purposes only
cols.sRGB <- testColorConversion(seed=s, colorSpace = 'sRGB', nClosest = n)

## better
cols.LAB <- testColorConversion(seed=s, colorSpace = 'LAB', nClosest = n)

## best, requires farver >= 2.0.2
cols.dE00 <- testColorConversion(seed=s, colorSpace = 'CIE2000', nClosest = n)



par(mfrow=c(3,1), mar=c(1,3,1,1))
soilPalette(cols.sRGB$color, lab = cols.sRGB$label, lab.cex = 1.5)
mtext('sRGB', side=2, font=2)

soilPalette(cols.LAB$color, lab = cols.LAB$label, lab.cex = 1.5)
mtext('LAB', side=2, font=2)

soilPalette(cols.dE00$color, lab = cols.dE00$label, lab.cex = 1.5)
mtext('dE00', side=2, font=2)

