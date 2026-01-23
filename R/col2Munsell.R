
# TODO: incorrect interpretation: .detectColorSpec(c('red', 1, NA))

#' @title Detect color specification from a vector of values, or a matrix of color coordinates
#' @param col character vector, numeric matrix with 3 columns, or data.frame with 3 columns
.detectColorSpec <- function(col) {
  
  # filter NA
  col <- na.omit(col)
  
  # matrix
  if(inherits(col, 'matrix')) {
    
    # trap 0-length input
    if(nrow(col) < 1) {
      return('unknown')
    }
    
    if(ncol(col) == 3) {
      return('color-coordinate-matrix')
    }
  }
  
  # data.frame
  if(inherits(col, 'data.frame')) {
    
    # trap 0-length input
    if(nrow(col) < 1) {
      return('unknown')
    }
    
    if(ncol(col) == 3) {
      return('color-coordinate-data.frame')
    }
  }
  
  # character vectors
  if(inherits(col, 'character')) {
    
    # trap 0-length input
    if(length(col) < 1) {
      return('unknown')
    }
    
    # hex notation of sRGB
    if(all(grepl('#', fixed = TRUE, x = col))) {
      return('hex-sRGB')
    }
    
    # Munsell notation
    # partial notation will return TRUE
    if(! all(is.na(unlist(parseMunsell(col, convertColors = FALSE))))) {
      return('munsell')
    }
    
    # named colors: error on invalid color names
    if(! inherits(try(col2rgb(col), silent = TRUE), 'try-error')) {
      return('named-colors')
    }
    
  }
  
  # otherwise, inconsistent / illegal color specification
  return('unknown')
}




#' @title Convert colors into Munsell Notation
#' 
#' @description Lookup the `n` closest Munsell chips from the `munsell` lookup table from various color notations. This function replaces `rgb2munsell()`.
#'
#' @param col character vector of colors, `data.frame` or `matrix` of color coordinates in sRGB or CIELAB color space
#' 
#' @param space character, one of `sRGB` or `CIELAB`, defines the input color system
#' 
#' @param nClosest integer, number of closest Munsell colors to return (valid range is 1-20)
#'

#'
#' @note This function is fully vectorized and will pad output with NA-records when NA are present in \code{color}.
#' 
#' @author D.E. Beaudette
#' 
#' @references 
#'   * http://ncss-tech.github.io/AQP/
#'   * http://www.brucelindbloom.com/index.html?ColorCalcHelp.html
#'   * http://www.munsellcolourscienceforpainters.com/MunsellAndKubelkaMunkToolbox/MunsellAndKubelkaMunkToolbox.html
#'   * http://www.cis.rit.edu/mcsl/online/munsell.php


#' @return an (NA-padded) `data.frame` containing `hue`, `value`, `chroma`, and CIE delta-E 2000 color contrast metric between source and nearest matching color(s).
#'
#' @export
#'
#' @examples
#' 
#' # vector of named R colors
#' col2Munsell(c('red', 'green', 'blue'))
#' 
#' # sRGB matrix in the range of 0-255
#' col2Munsell(cbind(255, 0, 0))
#' 
#' # sRGB matrix in the range of 0-1
#' col2Munsell(cbind(1, 0, 0))
#' 
#' # 10YR 5/6 in CIELAB
#' col2Munsell(
#'   cbind(51.4337, 9.917916, 38.6889), 
#'   space = 'CIELAB'
#' )
#' 
#' # 2.5YR 6/8 in hex notation
#' col2Munsell("#D18158FF")
#' 
#' # 7.5YR 8/1 in sRGB {0, 1}
#' col2Munsell(
#'   cbind(0.8240707, 0.7856834, 0.7541048)
#' )
#' 
#' # 7.5YR 8/1 in sRGB {0, 255}
#' col2Munsell(
#'   cbind(0.8240707, 0.7856834, 0.7541048) * 255
#' )
#' 
#' # multple colors in CIELAB
#' col2Munsell(
#'   parseMunsell(c('10BG 6/6', '2.5YR 4/6'), returnLAB = TRUE),
#'   space = 'CIELAB'
#' )
#' 
#' # data.frame input
#' col2Munsell(
#'   data.frame(r = 1, g = 0, b = 0),
#'   space = 'sRGB'
#' )
#'
#' # keep examples from using more than 2 cores
#' data.table::setDTthreads(Sys.getenv("OMP_THREAD_LIMIT", unset = 2))
#' 
#' # Munsell notation to sRGB triplets {0, 1} 
#' color <- munsell2rgb(
#'   the_hue = c('10YR', '2.5YR', '5YR'), 
#'   the_value = c(3, 5, 2.5), 
#'   the_chroma = c(5, 6, 2), 
#'   return_triplets = TRUE
#' )
#' 
#' # result is a data.frame of sRGB {0, 1}
#' color
#' 
#' # back-transform sRGB -> closest Munsell color
#' # sigma is the dE00 color contrast metric
#' col2Munsell(color, space = 'sRGB')
#'
col2Munsell <- function(col, space = c('sRGB', 'CIELAB'), nClosest = 1) {
  
  # reasonable constraints on n-closest chips
  if(nClosest < 1) {
    message('setting `nClosest to 1`')
    nClosest <- 1
  }
  
  if(nClosest > 20) {
    message('setting `nClosest to 20`')
    nClosest <- 20
  }
  
  # empty DF for NA-padding
  .empty <- data.frame(
    hue = NA, 
    value = NA, 
    chroma = NA, 
    sigma = NA
  )
  
  # sacrifice to CRAN gods
  munsell <- NULL
  
  # note: this is incompatible with LazyData: true
  # load look-up table from our package
  # This should be more foolproof than data(munsell) c/o PR
  load(system.file("data/munsell.rda", package="aqp")[1])
  
  # col can be either character vector or numeric matrix
  
  # a vector implies that it should be a character vector
  if(is.vector(col)) {
    col <- as.character(col)
  }
  
  # convert character vector of colors -> sRGB -> CIELAB
  if(inherits(col, 'character')) {
    
    # empty string should be interpreted as NA
    empty.idx <- which(col == '')
    col[empty.idx] <- NA
    
    # generate index to NA and not-NA
    na.idx <- which(is.na(col))
    not.na.idx <- setdiff(1:length(col), na.idx)
    
    # all NA short-circuit
    if(length(not.na.idx) < 1) {
      res <- .empty[na.idx, ]
      row.names(res) <- NULL
      return(res)
    }
    
    # sRGB matrix [0,1]
    # not NA-safe: NA are converted to [1,1,1]
    col <- t(col2rgb(col) / 255)
    
    # sRGB [0,1] -> CIELAB
    col <- convertColor(col, from = 'sRGB', to = 'Lab', from.ref.white = 'D65', to.ref.white = 'D65')
    
  } else {
    
    # convert to matrix as needed
    if(inherits(col, 'data.frame')) {
      col <- as.matrix(col)
    }
    
    # generate index to NA and not-NA
    na.idx <- which(apply(col, 1, function(i) any(is.na(i))))
    not.na.idx <- setdiff(1:nrow(col), na.idx)
    
    # all NA short-circuit
    if(length(not.na.idx) < 1) {
      res <- .empty[na.idx, ]
      row.names(res) <- NULL
      return(res)
    }
    
    # interpret space argument
    space <- match.arg(space)
    
    # sRGB input
    # convert to CIELAB
    if(space == 'sRGB') {
      
      # detect 0-1 vs. 0-255 sRGB range
      # not NA-safe
      if(range(col[not.na.idx])[2] > 2) {
        col <- col / 255
      }
      
      # sRGB [0,1] -> CIELAB
      col <- convertColor(col, from = 'sRGB', to = 'Lab', from.ref.white = 'D65', to.ref.white = 'D65')
    }
    
    # CIELAB input
    # nothing left to do
  }
  
  
  ## TODO: implement cache, we are often converting from a fixed set of possible colors
  
  # vectorize via for-loop
  n <- nrow(col)
  res <- vector(length = n, mode = 'list')
  
  # iterate over non-NA colors
  # this will leave NULL "gaps"
  for(i in not.na.idx) {
    # convert current color to matrix, this will allow matrix and DF as input
    this.color <- as.matrix(col[i, , drop = FALSE])
    dimnames(this.color)[[2]] <- c('L', 'A', 'B')
    
    # CIE dE00
    # fully-vectorized
    sigma <- farver::compare_colour(
      from = this.color, 
      to = munsell[, 7:9], 
      from_space = 'lab', 
      method = 'CIE2000', 
      white_from = 'D65'
    )
    
    # return the closest n-matches
    idx <- order(sigma)[1:nClosest]
    
    # pack results, sorted by closest results
    res[[i]] <- data.frame(
      munsell[idx, 1:3], 
      sigma = sigma[idx]
    )
    
  }
  
  # fill NULL gaps with empty DF, all NA
  null.idx <- which(sapply(res, is.null))
  if(length(null.idx) > 0) {
    for(i in null.idx){
      res[[i]] <- .empty
    }
  }
  
  
  # convert to DF
  res <- do.call('rbind', res)
  row.names(res) <- NULL
  
  # save sigma units
  attr(res, which = 'sigma') <- 'CIE delta-E 2000'
  
  return(res)
}



