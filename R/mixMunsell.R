
# weighted geometric mean
# https://en.wikipedia.org/wiki/Weighted_geometric_mean
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


#' 
#' @title Mix Munsell Colors via Spectral Library
#' 
#' @description Simulate subtractive mixing (pigments) of colors in Munsell notation.
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
#' inspiration / calculations based on:
#' https://arxiv.org/ftp/arxiv/papers/1710/1710.06364.pdf

#' related discussion here:
#' https://stackoverflow.com/questions/10254022/implementing-kubelka-munk-like-krita-to-mix-colours-color-like-paint/29967630#29967630

#' base spectral library:
#' http://www.munsellcolourscienceforpainters.com/MunsellResources/SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt
#'
#'
#' @details
#' Pending
#'
#' @return a \code{data.frame} with the closest matching Munsell color(s)
#'  
mixMunsell <- function(x, w = rep(1, times = length(x)) / length(x), n = 1) {
  
  ## TODO
  # sanity checks
  
    
  # x and w same lengths
  
  # x contains valid Munsell
  if(any(is.na(parseMunsell(x)))) {
    stop('input must be valid Munsell notation, neutral hues not supported')
  }
  
  ## TODO: could possibly do everything with munsell.spectra.wide
  
  # satisfy R CMD check
  munsell.spectra <- NULL
  munsell.spectra.wide <- NULL
  
  # safely load reference spectra
  load(system.file("data/munsell.spectra.rda", package="aqp")[1])
  # wide version for fast searches
  load(system.file("data/munsell.spectra.wide.rda", package="aqp")[1])
  
  
  # subset reference spectra for colors
  idx <- which(munsell.spectra$munsell %in% x)
  s <- munsell.spectra[idx, ]
  
  # long -> wide
  s.wide <- reshape2::dcast(s, munsell ~ wavelength, value.var = 'reflectance')
  
  # empty vector for mixture
  mixed <- vector(mode = 'numeric', length = ncol(s.wide) - 1)
  
  # iterate over wavelength (columns in first spectra)
  for(i in seq_along(mixed)) {
    
    # prepare values:
    # remove munsell color (1st column)
    # select the i-th wavelength (column)
    # down-grade to a vector
    vals <- s.wide[, -1][, i, drop = TRUE]
    
    # mix via weighted geometric mean
    mixed[i] <- .wgm( v = vals, w = w )
  }
  
  # subtract the mixture spectra, element-wise, from reference library
  # note we are removing the wavelength column
  m.diff <- sweep(munsell.spectra.wide[, -1], MARGIN = 1, STATS = mixed, FUN = '-')
  
  # euclidean distance is sufficient
  # D = sqrt(sum(reference - mixed))
  m.dist <- sqrt(colSums(m.diff^2))
  
  # get the spectra of the closest n munsell chip(s)
  m.match <- sort(m.dist)[1:n]
  
  # compile into data.frame
  res <- data.frame(
    munsell = names(m.match),
    distance = m.match,
    stringsAsFactors = FALSE
  )
  
  # clean-up row names
  row.names(res) <- as.character(1:nrow(res))
  
  return(res)
  
}

