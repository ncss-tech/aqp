##
##
##



## TODO: clamp to original range of Munsell chroma
## TODO: clamp to original range of Munsell value
## TODO: coordinate with `prepare-munsell-LUT.R`


library(lattice)
library(tactile)
library(purrr)
library(reshape2)

source('local-functions.R')

# load simplified spectra
m.rel <- readRDS('simplified-Munsell-spectra.rds')

# review original range
range(m.rel$reflectance)
min.reflectance <- min(m.rel$reflectance)

hist(m.rel$reflectance, breaks = 50)

## interpolate spectra for odd Munsell chroma

idx <- which(m.rel$hue %in% c('7.5YR') & m.rel$value == 3)
s <- m.rel[idx, ]

xyplot(reflectance ~ chroma | factor(wavelength), data=s, 
       type='b', as.table=TRUE,
       scales = list(y = list(tick.number = 10)),
       auto.key=list(lines=TRUE, points=FALSE, cex=1, space='right'),
       par.settings = tactile.theme()
)

# split by hue/value/wavelength
m <- split(m.rel, list(m.rel$hue, m.rel$value, m.rel$wavelength))

# do interpolation
mm <- map(m, .f = interpolateOddChromaSpectra, .progress = TRUE)

# combine
mm <- do.call('rbind', mm)

# re-order
m.final <- rbind(m.rel, mm)
m.final <- m.final[order(m.final$hue, m.final$value, m.final$chroma), ]


# graphical check
idx <- which(m.final$hue %in% c('7.5YR') & m.final$value == 3)
s <- m.final[idx, ]

xyplot(reflectance ~ chroma | factor(wavelength), data=s, 
       type='b', as.table=TRUE,
       scales = list(y = list(tick.number = 10)),
       par.settings = tactile.theme()
)

idx <- which(m.final$hue %in% c('2.5YR') & m.final$value == 4)
s <- m.final[idx, ]

xyplot(reflectance ~ chroma | factor(wavelength), data=s, 
       type='b', as.table=TRUE,
       scales = list(y = list(tick.number = 10)),
       par.settings = tactile.theme()
)



# check for reflectance <= 0
# 6 rows
nrow(m.final[m.final$reflectance <= 0, ])

# hmm
idx <- which(m.final$hue %in% c('2.5R') & m.final$value == 2)
s <- m.final[idx, ]

xyplot(reflectance ~ chroma | factor(wavelength), groups = reflectance <= 0, data=s, 
       type='b', as.table=TRUE,
       scales = list(y = list(tick.number = 10)),
       auto.key=list(lines=FALSE, points=TRUE, cex=1, space='top'),
       par.settings = tactile.theme()
)

# probably spline undershoots
idx <- which(m.final$reflectance <= 0)
m.final[idx, ]

# replace with original minimum reflectance, ignoring these values
m.final$reflectance[idx] <- min.reflectance

xyplot(reflectance ~ chroma | factor(wavelength), groups = reflectance <= 0, 
       data = m.final,
       subset = hue == '2.5R' & value == 2,
       type='b', as.table=TRUE,
       scales = list(y = list(tick.number = 10)),
       auto.key=list(lines=FALSE, points=TRUE, cex=1, space='top'),
       par.settings = tactile.theme()
)



## check: OK
s <- subset(m.final, subset = hue == '5YR' & value == 4 & chroma %in% 2:4)

xyplot(reflectance ~ wavelength, data = s, 
       groups = munsell, type='b',
       scales = list(y = list(tick.number = 10)),
       auto.key=list(lines=TRUE, points=FALSE, cex=1, space='top', columns = 3),
       par.settings = tactile.theme()
)


s <- subset(m.final, subset = hue == '2.5Y' & value == 4 & chroma %in% 2:4)

xyplot(reflectance ~ wavelength, data = s, 
       groups = munsell, type='b',
       scales = list(y = list(tick.number = 10)),
       auto.key=list(lines=TRUE, points=FALSE, cex=1, space='top', columns = 3),
       par.settings = tactile.theme()
)



## interpolate spectra for select half-chip Munsell values

# split by hue/chroma/wavelength
m <- split(m.final, list(m.final$hue, m.final$chroma, m.final$wavelength))


# do interpolation
mm <- map(m, .f = interpolateValueSpectra, .progress = TRUE)

# combine
mm <- do.call('rbind', mm)


# re-order
m.final <- rbind(m.final, mm)
m.final <- m.final[order(m.final$hue, m.final$value, m.final$chroma), ]

# check: OK
str(m.final)


# check for reflectance <= 0
# 3403 rows, all very close to 0
# most of these are very low value + low chroma | low value + high chroma
nrow(m.final[m.final$reflectance <= 0, ])

# hmm
idx <- which(m.final$munsell == '7.5YR 2.5/14')
s <- m.final[idx, ]

xyplot(reflectance ~ chroma | factor(wavelength), groups = reflectance <= 0, data=s, 
       type='b', as.table=TRUE,
       scales = list(y = list(tick.number = 10)),
       auto.key=list(lines=FALSE, points=TRUE, cex=1, space='top'),
       par.settings = tactile.theme()
)

# probably spline undershoots
idx <- which(m.final$reflectance <= 0)

# replace with minimum reflectance, ignoring these values
m.final$reflectance[idx] <- min.reflectance




s <- subset(m.final, subset = hue == '10YR' & chroma == 4 & value %in% c(2, 2.5, 3, 4))

xyplot(reflectance ~ wavelength, data = s, 
       groups = munsell, type='b',
       scales = list(y = list(tick.number = 10)),
       auto.key=list(lines=TRUE, points=FALSE, cex=1, space='top', columns = 3),
       par.settings = tactile.theme()
)

s <- subset(m.final, subset = hue == '2.5Y' & chroma == 4 & value %in% c(7, 8, 8.5, 9, 9.5, 10))

xyplot(reflectance ~ wavelength, data = s, 
       groups = munsell, type='b',
       scales = list(y = list(tick.number = 10)),
       auto.key=list(lines=TRUE, points=FALSE, cex=1, space='top', columns = 3),
       par.settings = tactile.theme()
)

xyplot(reflectance ~ value | wavelength, data = s, 
       type='b',
       as.table = TRUE,
       scales = list(alternating = 3, y = list(tick.number = 10)),
       auto.key=list(lines=TRUE, points=FALSE, cex=1, space='top', columns = 3),
       par.settings = tactile.theme()
)



## long -> wide for comparisons
reference <- dcast(m.final, wavelength ~ munsell, value.var = 'reflectance')


# save local copy for testing
# saveRDS(m.final, file = 'interpolated-Munsell-spectra.rds')
# saveRDS(reference, file = 'interpolated-Munsell-spectra-wide.rds')


# save package versions
munsell.spectra <- m.final
munsell.spectra.wide <- reference

save(munsell.spectra, file = '../../../data/munsell.spectra.rda', compress = 'xz')
save(munsell.spectra.wide, file = '../../../data/munsell.spectra.wide.rda', compress = 'xz')

# cleanup
unlink(
  c('interpolated-Munsell-spectra-wide.rds', 
    'interpolated-Munsell-spectra.rds', 
    'simplified-Munsell-spectra.rds'
  )
)


