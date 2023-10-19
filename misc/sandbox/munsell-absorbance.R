library(aqp)
library(lattice)
library(tactile)
library(sharpshootR)

# need this for mixingMethod = 'reference'
library(gower)

# need this for colorMixtureVenn()
library(venn)

# local copy of the Munsell chip spectral library
# c/o http://www.munsellcolourscienceforpainters.com/
# odd chroma spectra via interpolation
# see ?munsell.spectra for details
# try aqp:::.summarizeMunsellSpectraRanges()
data(munsell.spectra)

# all hues, limit to specific hue / chroma slice
x <- munsell.spectra[munsell.spectra$value  == 6 & munsell.spectra$chroma == 8, ]

# each Munsell chip has a 36-element spectra
# ranging from 380-730 nm
# table(x$munsell)

# spectra IDs
x$ID <- factor(x$munsell)
# create a color / chip
cols <- parseMunsell(as.character(levels(x$ID)))

# plot style
tps <- tactile.theme(superpose.line = list(col = cols, lwd = 2))

# R -> A
x$A <- log(1 / x$reflectance, base = 10)


# final figure
xyplot(
  reflectance ~ wavelength, groups = ID, data = x, 
  par.settings = tps,
  main = 'Value 6 / Chroma 8',
  type = c('l', 'g'),
  ylab = 'Reflectance',
  xlab = 'Wavelength (nm)',
  scales = list(tick.number = 12),
  xlim = c(370, 740)
)


xyplot(
  A ~ wavelength, groups = ID, data = x, 
  par.settings = tps,
  main = 'Value 6 / Chroma 8',
  type = c('l', 'g'),
  ylab = 'Absorbance',
  xlab = 'Wavelength (nm)',
  scales = list(tick.number = 12),
  xlim = c(370, 740)
)

