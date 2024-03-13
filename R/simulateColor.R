
## simulate via multivariate normal distribution
# n: number of simulations (typically horizons)
# parameters: list of parameters
#  data.frame with Munsell [hue, value, chroma]
.simulateColorFromMV <- function(n, parameters) {
  
  # sanity check, need this for rmvnorm()
  if(!requireNamespace('mvtnorm')) {
    stop('package `mvtnorm` is required for multivariate simulation', call. = FALSE)
  }
  
  ## TODO: consider pre-estimated mean vector + covariance matrix
  
  # extract parameters
  .hvc <- parameters[['hvc']]
  
  # convert Munsell -> CIELAB
  .lab <- munsell2rgb(the_hue = .hvc$hue, the_value = .hvc$value, the_chroma = .hvc$chroma, returnLAB = TRUE)
  
  # removing missing values which interfere with mean and covariance
  .lab <- na.omit(.lab)
  
  ## TODO: stop if nrow(.lab) < 3
  if(nrow(.lab) < 3) {
    return(NULL)
  }
  
  # multivariate simulation
  # assuming approx. joint normal distribution over L, A, B coordinates
  s <- mvtnorm::rmvnorm(
    n = n, 
    mean = colMeans(.lab), 
    sigma = cov(.lab),
  )
  
  
  ## TODO: consider returning CIELAB coordinates
  # .cols <- convertColor(s, from = 'Lab', to = 'sRGB', from.ref.white = 'D65', to.ref.white = 'D65')
  # previewColors(rgb(.cols, maxColorValue = 1), method = 'MDS')
  
  # this is slow
  # CIELAB -> Munsell hue, value, chroma
  m <- col2Munsell(s, space = 'CIELAB')
  
  ## TODO: consider including only hues in reference set
  
  # flatten to standard notation
  m <- sprintf('%s %s/%s', m$hue, m$value, m$chroma)
  
  return(m)
}




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
  z <- merge(x, cc, by.x = 'munsell', by.y = 'm1', all.x = TRUE, sort = FALSE)
  
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
  
  
  ## TODO: think about alternatives:
  #
  #  * ? --> perform conversion without RV, then re-add just before sampling 
  #  * z <- z[z$munsell != m$queryColor, ]
  
  ## convert distances -> similarities, interpret as sampling weights
  
  # standard conversion
  # too fast of a drop off between RV and simulated values
  s <- 1 / (1 + (z$dE00))
    
  # linear re-mapping of dE00 -> similarity
  # simulated values too close to RV
  # s <- 1 - (z$dE00 / max(z$dE00))
  
  
  ## diagnostics for dE00 -> probability
  # plot(s, z$dE00, type = 'n', las = 1)
  # points(s, z$dE00, col = z$color, pch = 15)
  # text(s, 0, z$munsell, cex = 0.5, srt = 90)

  
  # sample with replacement
  # according to ?sample, there is no need to convert weights -> probabilities
  # using translated dE00 as prior probabilities
  res <- sample(z$munsell, replace = TRUE, size = n, prob = s)
  
  return(res)
  
}



#' @title Simulate Soil Colors
#' 
#' @description Simulate plausible soil colors based on several possible parameterization of a "range in characteristics" (RIC). Soil color RIC can be specified by a list of parameters:
#'  * soil color proportions, as output from [aggregateColor()] -- `method = 'proportions'`
#'  * most likely Munsell color, CIE2000 threshold, and vector of acceptable hues -- `method = 'dE00'`
#'  * `data.frame` of Munsell hue, value, and chroma representing observed soil colors -- `method = 'mvnorm'`
#' 
#' 
#' 
#' @author D.E. Beaudette
#'
#' @param method simulation method, see details
#' 
#' @param n number of simulated colors per group
#' 
#' @param parameters a `list`, format depends on `method`:
#'   * `proportions`: output from [aggregateColor()] 
#'   * `dE00`: formatted as `list(m = '7.5YR 3/3', thresh = 5, hues = c('7.5YR'))`
#'   * `mvnorm`: formatted as `list(hvc = x)`
#'   
#'   Where `m` is a single representative Munsell chip, `thresh` is a threshold specified in CIE2000 color contrast (dE00), `hues` is a vector of allowed Munsell hues, and `x` is a `data.frame` representing columns of Munsell hue, value, and chroma having at least 3 rows.
#' 
#' @param SPC `SoilProfileCollection`, attempt to modify `SPC` with simulated colors
#'
#' @return a `list`, unless `SPC` is specified, then a `SoilProfileCollection` object
#' 
#' @export
#' 
#' @examples 
#' 
#' # restrict examples to 2 cores
#' data.table::setDTthreads(Sys.getenv("OMP_THREAD_LIMIT", unset = 2))
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
simulateColor <- function(method = c('dE00', 'proportions', 'mvnorm'), n, parameters, SPC = NULL) {
  
  # safely select method
  method <- match.arg(method)
  
  # if parameters is a single-depth list, add one more level
  if(!inherits(parameters[[1]], 'list')) {
    parameters <- list(parameters)
  }
  
  ## TODO: basic error checking, depends on method
  
  # select method
  res <- switch(
    method,
    'dE00' = {
      # manual iteration over parameters
      lapply(parameters, function(i) {
        .simulateColorFromDE00(n = n, parameters = i)
      })
    },
    # automatic iteration over output from aggregateColor()
    'proportions' = {
      .simulateColorFromProportions(n = n, parameters = parameters)
    },
    # manual iteration over parameters
    'mvnorm' = {
      lapply(parameters, function(i) {
        .simulateColorFromMV(n = n, parameters = i)
      })
    }
  )
  
  # result is a list
  if(is.null(SPC)) {
    return(res)
  } else {
    
    ## TODO: make this more efficient
    
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

