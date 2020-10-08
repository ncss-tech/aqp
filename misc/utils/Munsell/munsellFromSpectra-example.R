library(aqp)
library(asdreader)
library(pbapply)

# built in example
m <- get_spectra(asdreader::asd_file())

# another
m <- get_spectra('~/Desktop/spectra/Spectrum00047.asd')


# spectral library, columns are wavelength
data('munsell.spectra.wide')

# target wavelengths
nm <- unique(munsell.spectra.wide$wavelength)

# find in new data
idx <- which(dimnames(m)[[2]] %in% nm)

# reshape
x <- data.frame(
  wavelength = as.numeric(nm),
  reflectance = m[, idx]
)

# check: ok
plot(as.numeric(dimnames(m)[[2]]), m, type = 'l', las = 1)
points(x$wavelength, x$reflectance, pch = 16, col = 'firebrick')


# maybe this is right
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

(res <- munsellFromSpectra(x$reflectance))

plotColorMixture(names(res))



## this is probably very slow, and likely faster if the entire set of comparisons were done at once
## use an array

# iterate over an entire pile of spectra
f <- list.files('~/Desktop/spectra/', pattern = '*.asd', full.names = TRUE)

# ~ 47 seconds
z <- pblapply(f, function(i) {
  m <- get_spectra(i)
  # find in new data
  idx <- which(dimnames(m)[[2]] %in% nm)
  
  # reshape
  x <- data.frame(
    wavelength = as.numeric(nm),
    reflectance = m[, idx]
  )
  
  # compare
  res <- munsellFromSpectra(x$reflectance)
  
  return(res)
})


z <- do.call('c', z)

previewColors(parseMunsell(names(z)), method = 'MDS', pt.cex = 4)

most.common <- sort(table(names(z)), decreasing = TRUE)[1:5]
soilPalette(parseMunsell(names(most.common)), sprintf("%s (%s)", names(most.common), most.common))

