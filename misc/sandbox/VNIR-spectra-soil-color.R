library(aqp)
library(reshape2)
library(lattice)
library(tactile)

# # "wide" format, no IDs
# x <- read.csv('E:/temp/lab_spectra_csv.csv')
# 
# # save a local copy for testing
# saveRDS(x[1:8, ], file = 'misc/sandbox/VNIR-example-spectra.rds')

# example data, same format as Jay's shiny app
x <- readRDS(file = 'misc/sandbox/VNIR-example-spectra.rds')

# add a temporary ID
x$.id <- 1:nrow(x)

# long format simpler to work with
m <- melt(x, id.vars = '.id')

# remove leading 'X' from wavelength
m$variable <- as.character(m$variable)
m$v <- as.numeric(gsub(pattern = 'X', replacement = '', x = m$variable))

# remove left-over columns
m$variable <- NULL

# check: ok
head(m)

# subset to visible part of the spectrum that spec2Munsell can use
m.sub <- m[which(m$v >= 380 & m$v <= 730), ]

# "round" 1nm -> 10nm resolution
m.sub$v10 <- round(m.sub$v, -1)

# check: ok
head(m.sub)

# aggregate to 10nm res via mean
a <- aggregate(value ~ .id + v10, data = m.sub, FUN = mean)

# factor
a$.id <- factor(a$.id)

# check: ok
head(a)

# iterate over IDs and estimate a soil color
# using the visible part of the spectra
z <- split(a, a$.id)

z <- lapply(z, function(i) {
  # assumes 380--730nm at 10nm resolution
  res <- spec2Munsell(i$value, convert = TRUE)
   # copy over ID
  res$.id <- i$.id[1]
  return(res)
})

z <- do.call('rbind', z)

# convert color to sRGB
z$col <- munsell2rgb(z$hue, z$value, z$chroma)
# nice label
z$m <- sprintf('%s %s/%s', z$hue, z$value, z$chroma)

# color preview
previewColors(z$col, labels = z$m)

## big assumption: colors are in the same order as levels of .id

# plot spectra
tps <- tactile.theme(
  superpose.line = list(lwd = 2, col = z$col)
)

# 10nm resolution within visible portion of spectrum
xyplot(value ~ v10, groups = .id, data = a, type = c('l', 'g'), par.settings = tps, scales = list(x = list(tick.number = 20)), xlab = 'Wavelength (nm)', ylab = 'Reflectance')

# original data
xyplot(value ~ v, groups = .id, data = m, type = c('l', 'g'), par.settings = tps, scales = list(x = list(tick.number = 20)), xlab = 'Wavelength (nm)', ylab = 'Reflectance')

# annotate with visible portion of the spectrum
xyplot(value ~ v, groups = .id, data = m, type = 'l', par.settings = tps, scales = list(x = list(tick.number = 20)), xlab = 'Wavelength (nm)', ylab = 'Reflectance', panel = function(...) {
  panel.grid(-1, -1)
  panel.abline(v = c(380, 730), lty = 2)
  panel.xyplot(...)
})


