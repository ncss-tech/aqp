
## eval this Munsell / illuminant / tristimulus spectral library and functions


library(colorscience)
library(farver)

data(MunsellNeutrals2sRGB)
MunsellNeutrals2sRGB

data(MunsellSpectral)
MunsellSpectral

str(MunsellSpectral)


library(aqp)

data("munsell.spectra.wide")
# data("munsell.spectra")

str(munsell.spectra.wide)


x <- munsell.spectra.wide[, '5BG 6/8']

y <- subset(MunsellSpectral, subset = H == '5BG' & V == 6 & C == 8)
y <- y[, grep('nm', names(y))]
y <- unlist(y)


sx <- munsell.spectra.wide$wavelength
sy <- 380:800

plot(sx, x, type = 'b', ylim = c(0, 1), las = 1)

lines(sy, y)


## attemp spectra -> tristimulus -> munsell conversion
# almost works
test.XYZ <- spectra2XYZ(spectraIn = cbind(sy, y))

# test.XYZ <- spectra2XYZ(spectraIn = cbind(sx, x))

test.sRGB <- convert_colour(rbind(test.XYZ), from = 'xyz', to = 'rgb', white_from = 'D65', white_to = 'D65')

rgb2munsell(test.sRGB)



MaterialReferenceData

xyz <- spectra2XYZ(MaterialReferenceData[,c('wavelength','Sand')])
colorspace::swatchplot(rgb(XYZtoRGB(xyz[1], xyz[2], xyz[3])))
