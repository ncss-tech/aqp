library(latticeExtra)
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

# split by hue/value
m <- split(m.rel, list(m.rel$hue, m.rel$value, m.rel$wavelength))

# interpolation of odd chroma
interpolateOddChroma <- function(i) {
  
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
mm <- pblapply(m, interpolateOddChroma)


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


# check for reflectance < 0
m.final[m.final$reflectance < 0, ]

# hmm
idx <- which(m.final$hue %in% c('2.5R') & m.final$value == 2)
s <- m.final[idx, ]

xyplot(reflectance ~ chroma | factor(wavelength), groups = reflectance < 0, data=s, 
       type='b', as.table=TRUE,
       scales = list(y = list(tick.number = 10)),
       auto.key=list(lines=FALSE, points=TRUE, cex=1, space='top'),
       par.settings = tactile.theme()
)

# probably spline undershoots
idx <- which(m.final$reflectance < 0)
m.final[idx, ]

# replace with 0
m.final$reflectance[idx] <- 0



# long -> wide for comparisons
reference <- dcast(m.final, wavelength ~ munsell, value.var = 'reflectance')


# save local copy for testing
saveRDS(m.final, file = 'interpolated-Munsell-spectra.rds')
saveRDS(reference, file = 'interpolated-Munsell-spectra-wide.rds')


# save package versions
munsell.spectra <- m.final
munsell.spectra.wide <- reference

save(munsell.spectra, file = '../../../data/munsell.spectra.rda')
save(munsell.spectra.wide, file = '../../../data/munsell.spectra.wide.rda')


