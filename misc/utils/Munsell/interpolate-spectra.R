library(lattice)
library(tactile)
library(pbapply)
library(reshape2)

# load simplified spectra
m.rel <- readRDS('simplified-Munsell-spectra.rds')

## investigate slices -- can interpolate reflectance vs chroma (by wavelengths) for odd chroma

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

# interpolation of odd chroma
interpolateOddChromaSpectra <- function(i) {
  
  # 0-row input
  if(nrow(i) < 1)
    return(NULL)
  
  # chroma stats
  u.chroma <- unique(i$chroma)
  r.chroma <- range(u.chroma)
  n.chroma <- length(u.chroma)
  
  # reflectance stats
  r.reflectance <- range(i$reflectance)
  
  # sequence of candidate chroma
  s <- seq(from = r.chroma[1], to = r.chroma[2], by = 1)
  s.chroma <- setdiff(s, u.chroma)
  
  # short circuit: single chroma, interpolation impossible
  if(n.chroma < 2)
    return(NULL)
  
  # short circuit: 0 candidates for interpolation
  if(length(s.chroma) < 1)
    return(NULL)
    
  
  # setup interpolation function: natural splines
  # fit is exact at training points
  af <- splinefun(i$chroma, i$reflectance, method = 'natural')
  
  # check: fit should be exact at points
  if(sum(af(i$chroma) - i$reflectance) > 0.001){
    message('spline not fitting at training data!')
  }
  
  # interpolate candidate chroma
  s.reflectance <- af(s.chroma)
  
  # # check for over / undershoots
  # if( ! (s.reflectance < r.reflectance[1] | s.reflectance > r.reflectance[2])){
  #   message('spline exceeds original range!')
  # }
  
  # re-assemble into original format
  res <- data.frame(
    munsell = sprintf("%s %s/%s", i$hue[1], i$value[1], s.chroma),
    hue = i$hue[1],
    value = i$value[1],
    chroma = s.chroma,
    wavelength = i$wavelength[1],
    reflectance = s.reflectance,
    stringsAsFactors = FALSE
  )
  
  
  # debugging: graphical check
  # OK
  # plot(reflectance ~ chroma, data = i )
  # lines(seq(r.chroma[1], r.chroma[2], by = 0.1), af(seq(r.chroma[1], r.chroma[2], by = 0.1)), col = 'red')
  # points(s.chroma, s.reflectance, pch = 15)
  
  return(res)
  
}

# do interpolation
mm <- pblapply(m, interpolateOddChromaSpectra)

# combine
mm <- do.call('rbind', mm)

# re-order
m.final <- rbind(m.rel, mm)
m.final <- m.final[order(m.final$hue, m.final$value, m.final$chroma), ]


# graphical check
idx <- which(m.final$hue %in% c('7.5YR') & m.final$value == 5)
s <- m.final[idx, ]

xyplot(reflectance ~ chroma | factor(wavelength), data=s, 
       type='b', as.table=TRUE,
       scales = list(y = list(tick.number = 10)),
       par.settings = tactile.theme()
)


# check for reflectance <= 0
m.final[m.final$reflectance <= 0, ]

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

# replace with minimum reflectance, ignoring these values
m.final$reflectance[idx] <- min(m.final$reflectance[-idx])


## check: OK
s <- subset(m.final, subset = hue == '2.5YR' & value == 4 & chroma %in% 2:4)

xyplot(reflectance ~ wavelength, data = s, 
       groups = munsell, type='b',
       scales = list(y = list(tick.number = 10)),
       auto.key=list(lines=TRUE, points=FALSE, cex=1, space='top', columns = 3),
       par.settings = tactile.theme()
)




## interpolate 2.5 value

# just 2/3 value
m.sub <- subset(m.final, subset = value %in% 2:3)

head(m.sub)

# check
s <- subset(m.sub, subset = hue == '2.5YR' & chroma == 3)

xyplot(reflectance ~ wavelength, data = s, 
       groups = munsell, type='b',
       scales = list(y = list(tick.number = 10)),
       auto.key=list(lines=TRUE, points=FALSE, cex=1, space='top', columns = 2),
       par.settings = tactile.theme()
)


# split by hue/chroma/wavelength
m <- split(m.sub, list(m.sub$hue, m.sub$chroma, m.sub$wavelength))


interpolateValueSpectra <- function(i) {
  
  # 0 or 1 row input: no interpolation possible
  if(nrow(i) < 2)
    return(NULL)
  
  # linear interpolation between value 2--3
  a.fun <- approxfun(i$value, i$reflectance)
  
  # single value for now
  v.target <- 2.5
  
  # re-assemble into original format
  res <- data.frame(
    munsell = sprintf("%s %s/%s", i$hue[1], v.target, i$chroma[1]),
    hue = i$hue[1],
    value = v.target,
    chroma = i$chroma[1],
    wavelength = i$wavelength[1],
    reflectance = a.fun(v.target),
    stringsAsFactors = FALSE
  )
  
  return(res)
}


# do interpolation
mm <- pblapply(m, interpolateValueSpectra)

# combine
mm <- do.call('rbind', mm)


# re-order
m.final <- rbind(m.final, mm)
m.final <- m.final[order(m.final$hue, m.final$value, m.final$chroma), ]


# check: OK
str(m.final)

s <- subset(m.final, subset = hue == '2.5YR' & chroma == 3)

xyplot(reflectance ~ wavelength, data = s, 
       groups = munsell, type='b',
       scales = list(y = list(tick.number = 10)),
       auto.key=list(lines=TRUE, points=FALSE, cex=1, space='top', columns = 2),
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
unlink(c('interpolated-Munsell-spectra-wide.rds', 'interpolated-Munsell-spectra.rds', 'simplified-Munsell-spectra.rds'))


