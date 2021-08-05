library(aqp)

data(loafercreek, package = 'soilDB')

# generalize horizon names using REGEX rules
n <- c('Oi', 'A', 'BA','Bt1','Bt2','Bt3','Cr','R')
p <- c('O', '^A$|Ad|Ap|AB','BA$|Bw',
       'Bt1$|^B$','^Bt$|^Bt2$','^Bt3|^Bt4|CBt$|BCt$|2Bt|2CB$|^C$','Cr','R')
loafercreek$genhz <- generalize.hz(loafercreek$hzname, n, p)

# remove non-matching generalized horizon names
loafercreek$genhz[loafercreek$genhz == 'not-used'] <- NA
loafercreek$genhz <- factor(loafercreek$genhz)

a <- aggregateColor(loafercreek, 'genhz', k = 8)


## simulate color via sampling with replacement and estimated proportions
# n: number of simulations (typically horizons)
# parameters: output from aqp::aggregateColor()
.simulateColor <- function(n, parameters) {
 x <- parameters[['scaled.data']]
 
 res <- lapply(x, function(i) {
   sample(i[['munsell']], size = n, replace = TRUE, prob = i[['weight']])
 })
 
 return(res)
}


## simulate color from an RV color in Munsell notation, dE00 threshold, and vector of possible Munsell hues
# n: number of simulations (typically horizons)
# parameters: list of parameters
.simulateColorFromDE00 <- function(n, parameters) {
  # load Munsell LUT
  # safe for CRAN check
  munsell <- NULL
  load(system.file("data/munsell.rda", package = "aqp")[1])
  
  # extract parameters
  thresh <- parameters[['thresh']]
  m <- parameters[['m']]
  hues <- parameters[['hues']]
  
  ## borrowed / adapted from contrastChart()
  
  # extract just requested hues
  # along with standard value/chroma pairs found on a typical color book page
  chroma.subset <- c(1, 2, 3, 4, 6, 8)
  x <- munsell[which(munsell$value %in% 3:8 & munsell$chroma %in% chroma.subset & munsell$hue %in% hues), ]
  
  # convert into hex notation for plotting
  x$color <- munsell2rgb(x$hue, x$value, x$chroma)
  x$munsell <- sprintf("%s %s/%s", x$hue, x$value, x$chroma)
  
  # re-level hues according to color contrast guidance
  hh <- unique(x$hue)
  ll <- hh[order(huePosition(hh))]
  x$hue <- factor(x$hue, levels=ll)
  
  # setup query color table
  m <- data.frame(
    queryColor = m, 
    parseMunsell(m, convertColors = FALSE),
    stringsAsFactors = FALSE
  )
  m$value <- as.integer(m$value)
  m$chroma <- as.integer(m$chroma)
  
  # compute all pair-wise contrast classes and dE00
  cc <- colorContrast(x$munsell, rep(m$queryColor, times = nrow(x)))
  
  # join for plotting
  z <- merge(x, cc, by.x='munsell', by.y='m1', all.x=TRUE, sort=FALSE)
  
  # dE00 threshold
  z <- z[which(z$dE00 < thresh), ]
  
  # convert distances -> similarities
  s <- 1 / (1 + (z$dE00))
  
  # convert similarities (sum > 1) -> probabilities (sum == 1) 
  # via softmax function
  p <- exp(s) / sum(exp(s))
  
  # sample with replacement
  # using translated dE00 as prior probabilities
  res <- sample(z$munsell, replace = TRUE, size = n, prob = p)
  
  return(res)
  
}


# wrapper
simulateColor <- function(method = c('dE00', 'proportions'), n, parameters, SPC = NULL, ...) {
  
  method <- match.arg(method)
  
  res <- switch(
    method,
    'dE00' = {
      lapply(parameters, function(i) {
        .simulateColorFromDE00(n = n, parameters = i)
      })
    },
    'proportions' = {
      .simulateColor(n = n, parameters = parameters, ...)
    }
  )
  
  # result is a list
  if(is.null(SPC)) {
    return(res)
  } else {
    
    # result is a modified SPC
    
    # basic error checking: number of horizons
    
    # copy colors in horizon-order, profile order doesn't matter
    l <- list()
    for(i in 1:length(SPC)) {
      SPC.i <- SPC[i, ]
      
      ## TODO: hard-coded soil color column
      horizons(SPC.i)$soil_color <- parseMunsell(sapply(res, '[[', i))
      l[[i]] <- SPC.i
    }
    
    # flatten and done
    return(combine(l))
  }
  
  
}


## still a hack

n.sim <- 15

# using output from aggregateColor()
(cols <- simulateColor(method = 'proportions', n = n.sim, parameters = a))
previewColors(parseMunsell(unlist(cols)), method = 'MDS')


# self-calibration
contrastChart(m = '7.5YR 3/3', hues = c('7.5YR'), thresh = 5)
contrastChart(m = '7.5YR 4/4', hues = c('7.5YR'), thresh = 8)
contrastChart(m = '7.5YR 4/4', hues = c('5YR', '7.5YR'), thresh = 8)
contrastChart(m = '10YR 4/6', hues = c('10YR', '7.5YR'), thresh = 10)
contrastChart(m = '2.5G 6/2', hues = c('2.5G', '2.5GY', '2.5BG'), thresh = 15)


# using dE00 and hue constraints
p <- list(
  'A' = list(m = '7.5YR 3/3', thresh = 5, hues = c('7.5YR')),
  'BA' = list(m = '7.5YR 4/4', thresh = 8, hues = c('7.5YR')),
  'Bt1' = list(m = '7.5YR 4/4', thresh = 8, hues = c('5YR', '7.5YR')),
  'Bt2' = list(m = '5YR 4/5', thresh = 8, hues = c('5YR', '7.5YR')),
  'Bt3' = list(m = '10YR 4/6', thresh = 10, hues = c('10YR', '7.5YR')),
  'Cr' = list(m = '2.5G 6/2', thresh = 15, hues = c('2.5G', '2.5GY', '2.5BG'))
  )

# using dE00 threshold
(cols <- simulateColor(method = 'dE00', n = n.sim, parameters = p))
previewColors(parseMunsell(unlist(cols)), method = 'MDS')

# check L1 and marginal quantiles: looks OK
plotColorQuantiles(colorQuantiles(parseMunsell(cols$A)))
plotColorQuantiles(colorQuantiles(parseMunsell(cols$Bt1)))
plotColorQuantiles(colorQuantiles(parseMunsell(cols$Bt3)))
plotColorQuantiles(colorQuantiles(parseMunsell(cols$Cr)))


# seed profile
s <- loafercreek[7, ]

# static hz variability
horizons(s)$.hd <- 6

# simulate
ids <- sprintf("%s-%03d", 'sim', 1:n.sim)
z <- perturb(s, id = ids, boundary.attr = '.hd', min.thickness = 4)

# modify new SPC with simulated colors
# hard-coded to use 'soil_color' horizon level attribute
z <- simulateColor(method = 'dE00', n = n.sim, parameters = p, SPC = z)

# include original profile
zz <- combine(z, s)

# cool
par(mar = c(0, 0, 0, 0))
plotSPC(zz, name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE, width = 0.3)

