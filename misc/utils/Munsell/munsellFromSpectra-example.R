library(aqp)
library(asdreader)

m <- asdreader::get_spectra(asdreader::asd_file())

data('munsell.spectra.wide')

nm <- unique(munsell.spectra.wide$wavelength)

idx <- which(dimnames(m)[[2]] %in% nm)

x <- data.frame(
  wavelength = as.numeric(nm),
  reflectance = m[, idx]
)

plot(as.numeric(dimnames(m)[[2]]), m, type = 'l', las = 1)
points(x$wavelength, x$reflectance, pch = 16, col = 'firebrick')


# is this right?
munsellFromSpectra <- function(s) {
  data('munsell.spectra.wide')
  
  # subtract the mixture spectra, element-wise, from reference library
  # note we are removing the wavelength column
  m.diff <- sweep(munsell.spectra.wide[, -1], MARGIN = 1, STATS = s, FUN = '-')
  
  # euclidean distance is sufficient
  # D = sqrt(sum(reference - mixed))
  m.dist <- sqrt(colSums(m.diff^2))
  
  # get the spectra of the closest munsell chip
  m.match <- sort(m.dist)[1]
  
  return(m.match)
  
}

munsellFromSpectra(x$reflectance)
