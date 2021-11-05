
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
  x <- munsell[which(munsell$hue %in% hues), ]
  
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
  idx <- which(z$dE00 < thresh)
  
  # catch cases where the threshold is too low ... and ?
  if(length(idx) == 0) {
    message('Threshold too low.')
    return(rep(m, times = n))
  } else {
    # apply filter
    z <- z[idx, ]  
  }
  
  
  ## TODO: think about alternatives
  
  # ? --> perform conversion without RV, then re-add just before sampling 
  # z <- z[z$munsell != m$queryColor, ]
  
  # convert distances -> similarities
  
  # standard conversion
  # too fast of a drop off between RV and simulated values
  s <- 1 / (1 + (z$dE00))
    
  # linear re-mapping of dE00 -> similarity
  # simulated values too close to RV
  # s <- 1 - (z$dE00 / max(z$dE00))
  
  ## according to ?sample there is no need to convert weights -> probabilities
  

  # ## diagnostics for dE00 -> probability
  # plot(s, z$dE00, type = 'n', las = 1)
  # points(s, z$dE00, col = z$color, pch = 15)
  # text(s, 0, z$munsell, cex = 0.5, srt = 90)

  
  # sample with replacement
  # using translated dE00 as prior probabilities
  res <- sample(z$munsell, replace = TRUE, size = n, prob = s)
  
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
#' @examples 
#' 
#' # m: representative or most likely color
#' # thresh: dE00 threshold
#' # hues: allowed Munsell hues
#' p <- list(
#'   'A' = list(m = '7.5YR 3/3', thresh = 5, hues = c('7.5YR')),
#'   'BA' = list(m = '7.5YR 4/4', thresh = 8, hues = c('7.5YR')),
#'   'Bt1' = list(m = '7.5YR 4/4', thresh = 8, hues = c('5YR', '7.5YR')),
#'   'Bt2' = list(m = '5YR 4/5', thresh = 8, hues = c('5YR', '7.5YR')),
#'   'Bt3' = list(m = '10YR 4/6', thresh = 10, hues = c('10YR', '7.5YR')),
#'   'Cr' = list(m = '2.5G 6/2', thresh = 15, hues = c('2.5G', '2.5GY', '2.5BG'))
#' )
#' 
#' # simulate
#' (cols <- simulateColor(method = 'dE00', n = 10, parameters = p))
#' 
#' # preview
#' previewColors(parseMunsell(unlist(cols)), method = 'MDS')
#'
#' # another example, this time using a larger dE00 threshold
#' p <- list(
#'   'A' = list(m = '7.5YR 3/3', thresh = 20, hues = c('10YR', '7.5YR', '5YR'))
#' )
#' 
#' # simulate
#' set.seed(54654)
#' cols <- simulateColor(method = 'dE00', n = 200, parameters = p)
#' 
#' # flatten
#' cols <- unlist(cols)
#' 
#' # tabulate, sort: most frequent color should be 7.5YR 3/3
#' sort(table(cols), decreasing = TRUE)
#' 
#' # review colors
#' previewColors(parseMunsell(cols))
#' 
#' # what does a dE00 threshold look like on 3 pages of hue?
#' contrastChart('7.5YR 3/3', hues = c('10YR', '7.5YR', '5YR'), thresh = 20)
#'
simulateColor <- function(method = c('dE00', 'proportions'), n, parameters, SPC = NULL) {
  
  # safely select method
  method <- match.arg(method)
  
  # if parameters is a single-depth list, add one more level
  if(!inherits(parameters[[1]], 'list')) {
    parameters <- list(parameters)
  }
  
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
    
    ## TODO: make this mor efficient
    
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

