
## this will replace soilDB::estimateColorMixture() as an alternative / fallback 
## method for mixMunsell() when reference spectra are missing

.estimateColorMixture <- function(chips, w) {
 
  # convert to CIELAB
  .lab <- parseMunsell(chips, returnLAB = TRUE)
  
  # weighted mean
  .L <- weighted.mean(.lab[['L']], w = w, na.rm = TRUE)
  .A <- weighted.mean(.lab[['A']], w = w, na.rm = TRUE)
  .B <- weighted.mean(.lab[['B']], w = w, na.rm = TRUE)
  
  # LAB -> sRGB
  mixed.color <- data.frame(convertColor(cbind(.L, .A, .B), from='Lab', to='sRGB', from.ref.white='D65', to.ref.white = 'D65'))
  names(mixed.color) <- c('r', 'g', 'b')
  
  # back to Munsell
  m <- rgb2munsell(mixed.color[, c('r', 'g', 'b')])
  
  # pack into expected structure
  # scaled distance is only for spectral distance evaluated against the entire library
  res <- data.frame(
    munsell = sprintf('%s %s/%s', m$hue, m$value, m$chroma),
    distance = m$sigma,
    scaledDistance = NA,
    mixingMethod = 'estimate',
    stringsAsFactors = FALSE
  )
   
  return(res)
}



# helper function for printing out value / chroma ranges by hue
.summarizeMunsellSpectraRanges <- function() {

  # make R CMD CHECK happy
  munsell.spectra <- NULL

  # note: this is incompatible with LazyData: true
  # load look-up table from our package
  load(system.file("data/munsell.spectra.rda", package="aqp")[1])

  # set hue position
  munsell.spectra$hue <- factor(munsell.spectra$hue, levels = huePosition(returnHues = TRUE))

  # remove non-standard hues (what are they for?)
  munsell.spectra <- na.omit(munsell.spectra)

  x <- split(munsell.spectra, munsell.spectra$hue)

  x <- lapply(x, function(i) {

    data.frame(
      hue = i$hue[1],
      value = sprintf("%s-%s", min(i$value), max(i$value)),
      chroma = sprintf("%s-%s", min(i$chroma), max(i$chroma)),
      stringsAsFactors = FALSE
    )

  })


  x <- do.call('rbind', x)

  return(x)
}


## TODO: is this generic enough to use elsewhere?

# weighted geometric mean
# https://en.wikipedia.org/wiki/Weighted_geometric_mean
# note: function will fail if any(v) == 0
.wgm <- function(v, w) {
  r <- sum(w * log(v)) / sum(w)
  r <- exp(r)
  return(r)
}


# another possible approach using only sRGB coordinates
# http://scottburns.us/wp-content/uploads/2015/04/ILSS.txt

# related ticket
# https://github.com/ncss-tech/aqp/issues/101

# in response to the commentary here:
# https://soiltxnmyforum.cals.vt.edu/forum/read.php?3,1984,1987#msg-1987

# inspiration / calculations based on:
# https://arxiv.org/ftp/arxiv/papers/1710/1710.06364.pdf

# related discussion here:
# https://stackoverflow.com/questions/10254022/implementing-kubelka-munk-like-krita-to-mix-colours-color-like-paint/29967630#29967630

# base spectral library:
# http://www.munsellcolourscienceforpainters.com/MunsellResources/SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt

# see /misc/util/Munsell for:
# * spectral library prep
# * interpolation of odd chroma
# * reshaping for rapid look-up

# optimization:
# * 80% of time is spent on sweep() and colSums()
# * see matrixStats package for a compiled version: colSums2()
# * https://github.com/HenrikBengtsson/matrixStats

## TODO: use spec2Munsell vs. spectral library for more efficient / accurate (?) results!




#'
#' @title Mix Munsell Colors via Spectral Library
#'
#' @description Simulate mixing of colors in Munsell notation, similar to the way in which mixtures of pigments operate.
#'
#' @param x vector of colors in Munsell notation
#'
#' @param w vector of proportions, can sum to any number
#' 
#' @param mixingMethod approach used to simulate a mixture: 
#'    * `spectra`: simulate a subtractive mixture of pigments, limited to available reference spectra
#'    * `estimate`: closest Munsell chip to a weighted mean of CIELAB coordinates
#'    * `adaptive`: use reference spectra when possible, falling-back to weighted mean of CIELAB coordinates
#'
#' @param n number of closest matching color chips (`mixingMethod = spectra` only)
#'
#' @param keepMixedSpec keep weighted geometric mean spectra, final result is a `list` (`mixingMethod = spectra` only)
#' 
#' @param distThreshold spectral distance used to compute `scaledDistance`, default value is based on an analysis of spectral distances associated with adjacent Munsell color chips.
#'
#' @author D.E. Beaudette
#'
#' @references
#'
#' \itemize{
#'    \item{inspiration / calculations based on the work of Scott Burns: }{\url{https://arxiv.org/ftp/arxiv/papers/1710/1710.06364.pdf}}
#'
#'    \item{related discussion on Stack Overflow: }{\url{https://stackoverflow.com/questions/10254022/implementing-kubelka-munk-like-krita-to-mix-colours-color-like-paint/29967630#29967630}}
#'
#'    \item{spectral library source: }{\url{https://www.munsellcolourscienceforpainters.com/MunsellResources/SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt}}
#'
#' }
#'
#'
#' @details
#' An accurate simulation of pigment mixtures ("subtractive" color mixtures) is incredibly complex due to factors that aren't easily measured or controlled: pigment solubility, pigment particle size distribution, water content, substrate composition, and physical obstruction to name a few. That said, it is possible to simulate reasonable, subtractive color mixtures given a reference spectra library (350-800nm) and some assumptions about pigment qualities and lighting. For the purposes of estimating a mixture of soil colors (these are pigments after all) we can relax these assumptions and assume a standard light source. The only missing piece is the spectral library for all Munsell chips in our color books.
#' 
#' Thankfully, [Scott Burns has outlined the entire process](https://arxiv.org/ftp/arxiv/papers/1710/1710.06364.pdf), and Paul Centore has provided a Munsell color chip [reflectance spectra library](https://www.munsellcolourscienceforpainters.com). The estimation of a subtractive mixture of soil colors can proceed as follows:
#'   
#' 1. look up the associated spectra for each color
#' 2. computed the weighted (area proportion) geometric mean of the spectra
#' 3. search for the closest matching spectra in the reference library
#' 4. suggest that Munsell chip as the best candidate for the simulated mixture
#' 
#' Key assumptions include:
#'   
#' * similar particle size distribution
#' * similar mineralogy (i.e. pigmentation qualities)
#' * similar water content. 
#' 
#' For the purposes of estimating (for example) a "mixed soil color within the top 18cm of soil" these assumptions are usually valid. Again, these are estimates that are ultimately "snapped" to the nearest chip and not do not need to approach the accuracy of paint-matching systems.
#' 
#' A message is printed when `scaledDistance` is larger than 1.
#'
#' @return A `data.frame` with the closest matching Munsell color(s):
#' 
#'  * `munsell`: Munsell notation of the n-closest spectra
#'  * `distance`: spectral (Gower) distance to the n-closest spectra
#'  * `scaledDistance`: spectral distance scaled by `distThreshold`
#'  * `mixingMethod`: method used for each mixture
#' 
#' When `keepMixedSpec = TRUE` then a `list`:
#' 
#'  * `mixed`: a `data.drame` containing the same elements as above
#'  * `spec`: spectra for the 1st closest match
#' 
#' 
#'
#' @seealso \code{\link{munsell.spectra}}
#'
mixMunsell <- function(x, w = rep(1, times = length(x)) / length(x), mixingMethod = c('spectra', 'estimate', 'adaptive'), n = 1, keepMixedSpec = FALSE, distThreshold = 0.025) {

  # satisfy R CMD check
  munsell.spectra.wide <- NULL
  
  ## TODO
  # * more sanity checks
  # * how can we interpret returned distance? what values are too great to be of value?
  # * keep track of mixing method (distances are not compatible)
  
  # enforce mixing method
  mixingMethod <- match.arg(mixingMethod)
  
  # mixed spectra and multiple matches only possible when using mixingMethod == 'spectra'
  if(mixingMethod != 'spectra' & (n > 1 | keepMixedSpec)) {
    stop('`n` and `keepMixedSpec` options are only valid for `mixingMethod="spectra"`', call. = FALSE)
  }
  
  # sanity check, need this for gower::gower_topn()
  if(!requireNamespace('gower'))
    stop('package `gower` is required', call.=FALSE)

  # can't mix a single color, just give it back at 0 distance
  if (length(unique(x)) == 1) {
    return(data.frame(munsell = x[1], distance = 0))
  }

  # must have as many weights as length of x
  if (length(x) != length(w) && length(w) != 1) {

    stop('w should have same length as x or length one')

  } else if (length(w) == 1) {

    # cannot mix with zero weights
    stopifnot(w > 0)

    # a recycled weight is same as function default
    w <- rep(1, times = length(x)) / length(x)

  }

  # more informative error for colors missing
  if (any(w[is.na(x)] > 0)) {
    stop('cannot mix missing (NA) colors with weight greater than zero')
  }

  # more informative error for weights missing
  if (any(is.na(w))) {
    stop('cannot mix colors with missing (NA) weight')
  }

  # remove 0-weighted colors
  x <- x[w > 0]
  w <- w[w > 0]

  # x with weights > 0 must contain valid Munsell
  if (any(is.na(parseMunsell(x)))) {
    stop('input must be valid Munsell notation, neutral hues and missing not supported')
  }

  
  # main branch: mixing method
  if(mixingMethod == 'estimate') {
    
    # simple estimation by weighted mean LAB
    res <- .estimateColorMixture(chips = x, w = w)
    return(res)
    
  } else {
    # spectral mixing if possible
    
    # wide version for fast searches
    load(system.file("data/munsell.spectra.wide.rda", package="aqp")[1])
    
    # subset reference spectra for colors
    # note that search results are not in the same order as x
    # result are columns of spectra
    munsell.names <- names(munsell.spectra.wide)
    idx <- which(munsell.names %in% x)
    s <- munsell.spectra.wide[, idx, drop = FALSE]
    
    # sanity check: if there aren't sufficient reference spectra then return NA
    # must be at least the same number of spectra (columns) as unique colors specified
    if(ncol(s) < length(unique(x))){
      # helpful message
      missing.chips <- setdiff(x, munsell.names)
      msg <- sprintf(
        'reference spectra not available: %s',
        paste(missing.chips, collapse = ', ')
      )
      message(msg)
      
      # fall-back to wt. mean LAB
      if(mixingMethod == 'adaptive') {
        # assumes cleaned data
        res <- .estimateColorMixture(chips = x, w = w)
      } else {
        # otherwise return an empty result
        res <- data.frame(
          munsell = NA,
          distance = NA,
          scaledDistance = NA,
          mixingMethod = NA,
          stringsAsFactors = FALSE
        )
      }
      
      # done
      return(res)
    }
    
    # empty vector for mixture
    mixed <- vector(mode = 'numeric', length = nrow(s))
    
    # iterate over wavelength (columns in first spectra)
    for(i in seq_along(mixed)) {
      
      # prepare values:
      # select the i-th wavelength (row)
      # down-grade to a vector
      vals <- unlist(s[i, ])
      
      ## TODO: this wastes a lot of time when weights are obvious, move outside of loop
      # aggregate weights by "chip" -- in case length(x) != length(unique(x))
      wagg <- aggregate(w, by = list(chip = x), FUN = sum)
      
      # mix via weighted geometric mean
      mixed[i] <- .wgm(v = vals, w = wagg$x[match(names(s), wagg$chip)])
    }
    
    ## TODO: what is the "best" distance metric when comparing 1 spectra to the entire library?
    
    ## optimization: matrixStats::colSums2() much faster
    ## --> syntax slightly different
    
    ## operations on data.table likely faster
    
    # Gower distance: looks good, ~5x faster due to compiled code
    # https://cran.r-project.org/web/packages/gower/vignettes/intro.pdf
    # would make sense to reshape reference data
    
    ## TODO: time wasted here
    # reshape reference spectra: wavelength to columns
    z <- t(munsell.spectra.wide[, -1])
    
    # top n matches
    d <- gower::gower_topn(
      data.frame(rbind(mixed)),
      data.frame(z),
      n = n
    )
    
    res <- data.frame(
      munsell = dimnames(z)[[1]][d$index],
      distance = d$distance[, 1],
      scaledDistance = d$distance[, 1] / distThreshold,
      mixingMethod = 'spectra',
      stringsAsFactors = FALSE
    )
    
    
    # ## slower but no deps, using Euclidean distance
    # # D = sqrt(sum( [reference - mixed]^2 ))
    # 
    # # subtract the mixture spectra, element-wise, from reference library
    # # note we are removing the wavelength (1st) column
    # m.diff <- sweep(munsell.spectra.wide[, -1], MARGIN = 1, STATS = mixed, FUN = '-')
    # 
    # # finish the distance calculation
    # m.dist <- sqrt(colSums(m.diff^2))
    # 
    # # get the spectra of the closest n munsell chip(s) via sorting ASC
    # m.match <- sort(m.dist)[1:n]
    # 
    # # compile into data.frame
    # res <- data.frame(
    #   munsell = names(m.match),
    #   distance = m.match,
    #   stringsAsFactors = FALSE
    # )
    
    # report possibly problematic mixtures
    if(any(res$scaledDistance > 1)) {
      message('closest match has a spectral distance that is large, results may be unreliable')
    }
    
    # clean-up row names
    row.names(res) <- NULL
    
    # optionally return weighted geometric mean (mixed) spectra
    if(keepMixedSpec) {
      return(
        list(
          mixed = res,
          spec = mixed
        )
      )
    } else {
      return(res)
    }
  }
  
  
  

}

