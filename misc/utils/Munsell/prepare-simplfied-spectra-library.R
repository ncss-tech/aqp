## Simplify the Munsell spectral reference data for later use. Results are temporary.
##
##

## From the internal description:
# This file contains spectral reflectance measurements of X-Rite's 2007 Munsell Book
# of Color (Glossy Finish).  The measurements were made in 2012 with a ColorMunki
# spectrophotometer.  The first column is the Munsell name.  The remaining 
# columns give reflectance values for 380 nm to 730 nm, in steps
# of 10 nm.  The reflectance is a value between 0 (indicating that no light at that
# wavelength is reflected) and 1 (indicating that all the light at that wavelength
# is reflected).  Occasionally an entry is slightly greater than 1.  The likely cause
# is random variability, and those entries can be adjusted to 1 with negligible loss.
# In all, 1485 colour samples were measured.  Researchers are invited to analyze
# the data in this file.

library(aqp)
library(reshape2)

# missing odd chroma
x <- read.table('SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt.gz', skip = 13, header = TRUE, stringsAsFactors = FALSE, sep = ',')

# long format simpler to work with
m <- melt(x, id.vars = 'Name')

# remove leading 'X' from wavelength
m$variable <- as.character(m$variable)
m$v <- as.numeric(gsub(pattern = 'X', replacement = '', x = m$variable))

# subset columns and re-name
m <- m[, c('Name', 'v', 'value')]
names(m) <- c('munsell', 'wavelength', 'reflectance')

# reflectance values > 1 should be clamped at 1
# see description above
m$reflectance <- pmin(m$reflectance, 1)

# 
# 2025-06-03: more precise Munsell parsing REGEX
# verified with https://regexr.com/
#
# the main problem here is that the munsell notation in the refelctance library
# is specified without a space, and there are some 8.5 values in there
# --> 10YR4/5, 5Y8.5/2, 2.5BG6/6, etc.
# 
d <- strcapture(
  '([[:digit:]]+[.]?[[:digit:]]?[[:alpha:]]+)([[:digit:].]+)/([[:digit:]]+)', 
  m$munsell, 
  proto = data.frame(
    hue = character(), 
    value = numeric(), 
    chroma = integer(), 
    stringsAsFactors = FALSE
  )
)

# check: OK
sort(tab <- table(d$hue))

# all hues accounted for
.hp <- huePosition(returnHues = TRUE, includeNeutral = FALSE)
setdiff(names(tab), .hp)
setdiff(.hp, names(tab))

# all standard values and 
table(d$value, useNA = 'always')
table(d$chroma, useNA = 'always')


# re-assemble
m.rel <- data.frame(
  munsell = sprintf("%s %s/%s", d$hue, d$value, d$chroma),
  hue = d$hue, 
  value = d$value, 
  chroma = d$chroma, 
  wavelength = m$wavelength, 
  reflectance = m$reflectance, 
  stringsAsFactors = FALSE
)

# sort
m.rel <- m.rel[order(m.rel$hue, m.rel$value, m.rel$chroma, m.rel$wavelength), ]

# save
saveRDS(m.rel, file = 'simplified-Munsell-spectra.rds', compress = 'xz')

