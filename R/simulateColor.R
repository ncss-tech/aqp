
## simulate color via sampling with replacement and estimated proportions
# n: number of simulations (typically horizons)
# parameters: output from aqp::aggregateColor()
.simulateColorFromProportions <- function(n, parameters) {
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



#' @title Simulate Soil Colors
#' 
#' @description Simulate plausible soil colors based on proportions by Munsell "chip", or using a seed Munsell chip and threshold specified via CIE2000 color contrast metric.
#' 
#' @author D.E. Beaudette
#'
#' @param method simulation method, see details
#' @param n number of simulated colors per horizon
#' @param parameters a `list`, format depends on `method`:
#'   * `proportions`: output from [`aggregateColor`] 
#'   * `dE00`: formatted as `list(m = '7.5YR 3/3', thresh = 5, hues = c('7.5YR'))`
#'   
#'   Where `m` is a single representative Munsell chip, `thresh` is a threshold specified in CIE2000 color contrast (dE00), and `hues` is a vector of allowed Munsell hues.
#' 
#' @param SPC `SoilProfileCollection`, attempt to modify `SPC` with simulated colors
#'
#' @return a `list`, unless `SPC` is specified, then a `SoilProfileCollection` object
#' 
#' @export
#'
simulateColor <- function(method = c('dE00', 'proportions'), n, parameters, SPC = NULL) {
  
  method <- match.arg(method)
  
  res <- switch(
    method,
    'dE00' = {
      lapply(parameters, function(i) {
        .simulateColorFromDE00(n = n, parameters = i)
      })
    },
    'proportions' = {
      .simulateColorFromProportions(n = n, parameters = parameters)
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

