

#' @title Convert colors into Munsell Notation
#' 
#' @description Lookup the `n` closest Munsell chips from the `munsell` lookup table from various color notations. This function replaces `rgb2munsell()`.
#'
#' @param col character vector of colors, `data.frame` or `matrix` of color coordinates in sRGB or CIELAB color space
#' @param space character, one of `sRGB` or `CIELAB`, defines the input color system 
#' @param nClosest integer, number of closest Munsell colors to return (valid range is 1-20)
#'

#'
#' @note This function is fully vectorized and will pad output with NA-records when NA are present in \code{color}.
#' 
#' @author D.E. Beaudette
#' 
#' @references 
#' \url{http://ncss-tech.github.io/AQP/}
#' \url{http://www.brucelindbloom.com/index.html?ColorCalcHelp.html}
#' \url{https://www.munsellcolourscienceforpainters.com/MunsellAndKubelkaMunkToolbox/MunsellAndKubelkaMunkToolbox.html}
#' http://www.cis.rit.edu/mcsl/online/munsell.php


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
  
  # sacrafice to CRAN gods
  munsell <- NULL
  
  # note: this is incompatible with LazyData: true
  # load look-up table from our package
  # This should be more foolproof than data(munsell) c/o PR
  load(system.file("data/munsell.rda", package="aqp")[1])
  
  # col can be either character vector or numeric matrix
  
  # convert numeric vector of colors -> sRGB -> CIELAB
  if(inherits(col, 'character')) {
    
    # sRGB matrix [0,1]
    col <- t(col2rgb(col) / 255)
    
    # sRGB [0,1] -> CIELAB
    col <- convertColor(col, from = 'sRGB', to = 'Lab', from.ref.white = 'D65', to.ref.white = 'D65')
    
  } else {
    
    # convert to matrix as needed
    if(inherits(col, 'data.frame')) {
      col <- as.matrix(col)
    }
    
    # interpret space argument
    space <- match.arg(space)
    
    # sRGB input
    # convert to CIELAB
    if(space == 'sRGB') {
      
      # detect 0-1 vs. 0-255 sRGB range
      if(range(col)[2] > 2) {
        col <- col / 255
      }
      
      # sRGB [0,1] -> CIELAB
      col <- convertColor(col, from = 'sRGB', to = 'Lab', from.ref.white = 'D65', to.ref.white = 'D65')
    }
    
    # CIELAB input
    # nothing left to do
    # if(space == 'CIELAB') {
    #   message('no conversion required')
    # }
  }
  
  # vectorize via for-loop
  n <- nrow(col)
  res <- vector(length = n, mode = 'list')
  
  # accounting for the possibility of NA
  # result should be an empty record
  not.na.idx <- which(apply(col, 1, function(i) ! any(is.na(i))))
  
  
  # iterate over colors
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
  
  # pad records with NA in the sRGB input
  # https://github.com/ncss-tech/aqp/issues/160
  na.idx <- which(sapply(res, is.null))
  
  if(length(na.idx) > 0) {
    for(i in na.idx){
      res[[i]] <- data.frame(
        hue = NA, 
        value = NA, 
        chroma = NA, 
        sigma = NA, 
        stringsAsFactors = FALSE
      )
    }
  }
  
  
  # convert to DF
  res <- do.call('rbind', res)
  row.names(res) <- NULL
  
  # save sigma units
  attr(res, which = 'sigma') <- 'CIE delta-E 2000'
  
  return(res)
  
}



