


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


# another other possible approach using only sRGB coordinates
# http://scottburns.us/wp-content/uploads/2015/04/ILSS.txt

# related ticket
# https://github.com/ncss-tech/aqp/issues/101

# in response to the dumb commentary here:
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

# optimizations:
# * 80% of time is spent on sweep() and colSums()
# * see matrixStats package for a compiled version: colSums2()
# * https://github.com/HenrikBengtsson/matrixStats
# * use gower package, compiled code, new dependency -> 20x faster


#'
#' @title Mix Munsell Colors via Spectral Library
#'
#' @description Simulate subtractive mixing of colors in Munsell notation, similar to the way in which mixtures of pigments operate.
#'
#' @param x vector of colors in Munsell notation
#'
#' @param w vector of proportions, can sum to any number
#'
#' @param n number of closest matching color chips
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
#'    \item{spectral library source: }{\url{http://www.munsellcolourscienceforpainters.com/MunsellResources/SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt}}
#'
#' }
#'
#'
#' @details
#' Pending
#'
#' @note This functions is slower than \code{soilDB::estimateColorMixture()} (weighted average of colors in CIELAB coordinates) but more accurate.
#'
#' @return a \code{data.frame} with the closest matching Munsell color(s)
#'
#' @seealso \code{\link{munsell.spectra}}
#'
mixMunsell <- function(x, w = rep(1, times = length(x)) / length(x), n = 1) {

  ## TODO
  # sanity checks
  
  # sanity check, need this for gower_topn
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

  # satisfy R CMD check
  munsell.spectra.wide <- NULL

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
    message('reference spectra not available')
    res <- data.frame(
      munsell = NA,
      distance = NA,
      stringsAsFactors = FALSE
    )
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

    # aggregate weights by "chip" -- in case length(x) != length(unique(x))
    wagg <- aggregate(w, by = list(chip = x), FUN = sum)

    # mix via weighted geometric mean
    mixed[i] <- .wgm(v = vals, w = wagg$x[match(names(s), wagg$chip)])
  }

  ## TODO: what is the "best" distance metric when comparing 1 spectra to the entire library?

  ## optimization: matrixStats::colSums2() much faster
  ## --> syntax slightly different
  
  ## operations on data.table likely faster
  
  # Gower distance: looks good, 10-20x faster due to compiled code
  # https://cran.r-project.org/web/packages/gower/vignettes/intro.pdf
  # would make sense to reshape reference data

  # top n matches
  z <- t(munsell.spectra.wide[, -1])

  d <- gower::gower_topn(
    data.frame(rbind(mixed)),
    data.frame(z),
    n = n
  )

  res <- data.frame(
    munsell = dimnames(z)[[1]][d$index],
    distance = d$distance[, 1],
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

  # clean-up row names
  row.names(res) <- NULL

  return(res)

}

