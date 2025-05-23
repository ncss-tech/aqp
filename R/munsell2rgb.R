
#' @title sRGB to Munsell Color Conversion
#' 
#' @description Convert sRGB color coordinates to the closest `n` Munsell chips in the `munsell` lookup table. This function will be replaced by `col2Munsell()` in **aqp 2.1**.
#'  
#'
#' @param color a `data.frame` or `matrix` object containing sRGB coordinates in the range of (0,1)
#' 
#' @param colorSpace distance metric (colorspace) to use for finding the closest chip: CIE2000 is the most accurate but requires farver >= 2.0.3, Euclidean distance in CIELAB is a close second, while Euclidean distance in sRGB is not at all accurate and should only be used for demonstration purposes.
#' 
#' @param nClosest number of closest Munsell colors to return (valid range is 1-20)
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
#'
#' @return an (NA-padded) `data.frame` containing `hue`, `value`, `chroma`, and distance (dE00 when \code{colorSpace = 'CIE2000'}, Euclidean distance otherwise) to nearest matching color.
#' 
#' @export
#'
#' @examples 
#' 
#' # keep examples from using more than 2 cores
#' data.table::setDTthreads(Sys.getenv("OMP_THREAD_LIMIT", unset = 2))
#' 
#' # Munsell notation to sRGB triplets [0-1] 
#' color <- munsell2rgb(
#'   the_hue = c('10YR', '2.5YR', '5YR'), 
#'   the_value = c(3, 5, 2.5), 
#'   the_chroma = c(5, 6, 2), 
#'   return_triplets = TRUE
#' )
#' 
#' # result is a data.frame
#' color
#' 
#' # back-transform sRGB -> closest Munsell color
#' # sigma is the dE00 color contrast metric
#' rgb2munsell(color)
#'
rgb2munsell <- function(color, colorSpace = c('CIE2000', 'LAB', 'sRGB'), nClosest = 1) {
  
  ## !! will deprecate in aqp 2.1
  # .Deprecated(new = 'col2Munsell', msg = 'please use col2Munsell() instead.')
  message('rgb2munsell() will be deprecated in aqp 2.1, please use col2Munsell() instead.')
  
  
  # argument check
  colorSpace <- match.arg(colorSpace)
  
  # reasonable constraints on n-closest chips
  if(nClosest < 1) {
    message('setting `nClosest to 1`')
    nClosest <- 1
  }
  
  if(nClosest > 20) {
    message('setting `nClosest to 20`')
    nClosest <- 20
  }
  
  
  ## TODO: detect sRGB values in the range of 0-255 and re-scale accordingly
  
  # vectorize via for-loop
  n <- nrow(color)
  res <- vector(length = n, mode='list')
  
  # This is a hack to avoid munsell2rgb: "no visible binding for global variable munsell" at package R CMD check
  munsell <- NULL
  
  # note: this is incompatible with LazyData: true
  # load look-up table from our package
  # This should be more foolproof than data(munsell) c/o PR
  load(system.file("data/munsell.rda", package="aqp")[1])
  
  
  # CIE2000 requires farver >= 2.0.3
  if(colorSpace == 'CIE2000') {
    if(packageVersion("farver") < '2.0.3' ) {
      message('rgb2munsell: using LAB color space; install farver v2.0.3 or higher for perceptual distance in CIE2000')
      colorSpace <- 'LAB';
    }
  }
  
  # accounting for the possibility of NA
  # result should be an empty record
  not.na.idx <- which(apply(color, 1, function(i) ! any(is.na(i))))
  
  
  # iterate over colors
  for(i in not.na.idx) {
    # convert current color to matrix, this will allow matrix and DF as input
    this.color <- as.matrix(color[i, , drop=FALSE])
    
    # TODO: there isn't any reason to use sRGB other than demonstration
    if(colorSpace == 'sRGB') {
      # euclidean distance (in sRGB space) is our metric for closest-color
      # d = sqrt(r^2 + g^2 + b^2)
      sq.diff <- sweep(munsell[, 4:6], MARGIN=2, STATS=this.color, FUN='-')^2
      sigma <- sqrt(rowSums(sq.diff))
      # rescale distances to 0-1
      sigma <- sigma / max(sigma)
      # return the closest n-matches
      idx <- order(sigma)[1:nClosest]
    }
    
    if(colorSpace == 'LAB') {
      # euclidean distance (in CIELAB space) is our metric for closest-color
      # convert sRGB to LAB
      this.color.lab <- convertColor(this.color, from='sRGB', to='Lab', from.ref.white='D65', to.ref.white = 'D65')
      # d = sqrt(L^2 + A^2 + B^2)
      sq.diff <- sweep(munsell[, 7:9], MARGIN=2, STATS=this.color.lab, FUN='-')^2
      sigma <- sqrt(rowSums(sq.diff))
      # rescale distances to 0-1
      sigma <- sigma / max(sigma)
      # return the closest n-matches
      idx <- order(sigma)[1:nClosest]
    }
    
    # most accurate / efficient method as of farver >= 2.0.3
    if(colorSpace == 'CIE2000') {
      # CIE dE00
      # convert sRGB to LAB
      this.color.lab <- convertColor(
        color = this.color, 
        from = 'sRGB', 
        to = 'Lab', 
        from.ref.white = 'D65', 
        to.ref.white = 'D65'
      )
      
      dimnames(this.color.lab)[[2]] <- c('L', 'A', 'B')
      
      # fully-vectorized
      sigma <- farver::compare_colour(
        from = this.color.lab, 
        to = munsell[, 7:9], 
        from_space = 'lab', 
        method = 'CIE2000', 
        white_from = 'D65'
      )
      
      # return the closest n-matches
      idx <- order(sigma)[1:nClosest]
    }
    
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
      res[[i]] <- data.frame(hue=NA, value=NA, chroma=NA, sigma=NA, stringsAsFactors=FALSE)
    }
  }
  
  
  # convert to DF
  res <- do.call('rbind', res)
  row.names(res) <- as.character(1:nrow(res))
  
  # save sigma units
  attr(res, which = 'sigma') <- switch(colorSpace, 'sRGB' = 'distance in sRGB', 'LAB' = 'distance in CIELAB', 'CIE2000' = 'dE00')
  
  return(res)
}



# internally used function for snapping a vector of user-supplied values
# to a known subset via absolute distance
# NA safely propagated
.snapValid <- function(s, v) {
  # rows are distances by the_value
  .dist <- outer(s, v, function(i, j) {abs(i - j)})
  
  # index to snapped values, safely handle NA
  idx <- apply(.dist, 1, function(i) {
    if(all(is.na(i))) {
      return(NA)
    } else {
      return(which.min(i))
    }
  })
  
  # replace with snapped + NA
  return(v[idx])
}



#' @title Convert Munsell Color Notation to other Color Space Coordinates (sRGB and CIELAB)
#' 
#' @description Color conversion based on a look-up table of common soil colors.
#'
#' @param the_hue a vector of one or more more hues, upper-case (e.g. '10YR')
#' 
#' @param the_value a vector of one or more values (e.g. '4')
#' 
#' @param the_chroma a vector of one or more chromas (e.g. '6'), may be NA for neutral hues
#' 
#' @param alpha numeric, transparency setting used when \code{return_triplets = FALSE} and \code{returnLAB = FALSE}
#' 
#' @param maxColorValue maximum sRGB color value, typically `1` (see \code{\link{rgb})}
#' 
#' @param return_triplets logical, return sRGB coordinates (range 0-1) instead of standard hex notation of sRGB (e.g. '#8080B')
#' 
#' @param returnLAB logical, return CIELAB coordinates (D65 illuminant)
#' 
#' @return A vector of R colors is returned that is the same length as the input data. When \code{return_triplets = TRUE} and/or \code{returnLAB = TRUE}, then a \code{data.frame} (of sample length as input) is returned.
#' 
#' 
#' @details This function is vectorized without recycling: i.e. the length of each argument must be the same. Both functions will pad output with NA if there are any NA present in the inputs.
#' 
#' Neutral hues are approximated by greyscale shades ranging from 20\% (darker) to 80\% (lighter). No chroma is required for neutral hues.
#' 
#' Gley soil colors that are missing a chroma will not be correctly interpreted. Consider using a chroma of 1.
# 
# 
#' Non-standard Munsell notation (e.g. '7.9YR 2.7/2.0') can be matched (nearest-neighbor, no interpolation) to the closest color within the \code{munsell} sRGB/CIELAB look-up table via \code{getClosestMunsellChip()}. A more accurate estimate of sRGB values from non-standard notation can be achieved with the \href{https://CRAN.R-project.org/package=munsellinterpol}{munsellinterpol} package.
#' 
#' See examples below.
#' 
#' @note Care should be taken when using the resulting sRGB values; they are close to their Munsell counterparts, but will vary based on your monitor and ambient lighting conditions. Also, the value used for \code{maxColorValue} will affect the brightness of the colors. Th default value (1) will usually give acceptable results, but can be adjusted to force the colors closer to what the user thinks they should look like.
#' 
#' @references 
#'  - \url{http://www.brucelindbloom.com/index.html?ColorCalcHelp.html}
#'  - \url{https://www.munsellcolourscienceforpainters.com/MunsellAndKubelkaMunkToolbox/MunsellAndKubelkaMunkToolbox.html}
#'  - \url{https://www.rit.edu/science/munsell-color-lab}
#' 
#' @author D.E. Beaudette
#' 
#' @export
#'
#' @examples
#' 
#' # neutral hues (N) can be defined with chroma of 0 or NA 
#' g <- expand.grid(hue = 'N', value = 2:8, chroma = 0, stringsAsFactors = FALSE)
#' (m <- munsell2rgb(g$hue, g$value, g$chroma))
#' soilPalette(m)
#' 
#' # back-transform to Munsell notation
#' col2Munsell(t(col2rgb(m)) / 255)
#' 
#' 
#' # basic example
#' d <- expand.grid(hue = '10YR', value = 2:8, chroma = 1:8, stringsAsFactors = FALSE)
#' d$color <- with(d, munsell2rgb(hue, value, chroma))
#' 
#' # similar to the 10YR color book page
#' plot(value ~ chroma, data = d, col = d$color, pch = 15, cex = 3, las = 1)
#' 
#' # multiple pages of hue:
#' hues <- c('2.5YR', '5YR', '7.5YR', '10YR')
#' d <- expand.grid(
#'   hue = hues, 
#'   value = c(2, 2.5, 3:8), 
#'   chroma = seq(2, 8, by = 2), 
#'   stringsAsFactors = FALSE
#' )
#' # convert Munsell -> sRGB
#' d$color <- with(d, munsell2rgb(hue, value, chroma))
#' 
#' # extract CIELAB coordinates
#' with(d, munsell2rgb(hue, value, chroma, returnLAB = TRUE))
#' 
#' # plot: note that we are setting panel order from red --> yellow
#' library(lattice)
#' 
#' xyplot(
#'   value ~ factor(chroma) | factor(hue, levels = hues),
#'   main = "Common Soil Colors", layout = c(4, 1), scales = list(alternating = 1),
#'   strip = strip.custom(bg = grey(0.85)),
#'   data = d, as.table = TRUE, subscripts = TRUE,
#'   xlab = 'Chroma', ylab = 'Value',
#'   panel = function(x, y, subscripts, ...) {
#'     panel.xyplot(x, y, pch = 15, cex = 4, col = d$color[subscripts])
#'   }
#' )
#' 
#' 
#' # convert a non-standard color to closest "chip" in `munsell` look-up table
#' getClosestMunsellChip('7.9YR 2.7/2.0', convertColors = FALSE)
#' 
#' # convert directly to hex notation of sRGB
#' getClosestMunsellChip('7.9YR 2.7/2.0')
#' 
munsell2rgb <- function(the_hue, the_value, the_chroma, alpha = 1, maxColorValue = 1, return_triplets = FALSE, returnLAB = FALSE) {
  ## important: change the default behavior of data.frame and melt
  opt.original <- options(stringsAsFactors = FALSE)
  
  # check for missing data
  if(missing(the_hue) | missing(the_chroma) | missing(the_value))
    stop('Must supply a valid Munsell color.')
  
  # check to make sure that each vector is the same length
  if(length(unique( c(length(the_hue), length(the_value), length(the_chroma)))) != 1)
    stop('All inputs must be vectors of equal length.')
  
  # in case of factors, why would anyone do this?
  if(is.factor(the_hue)) {
    the_hue <- as.character(the_hue)
  }
  if(is.factor(the_value)) {
    the_value <- as.character(the_value)
  }
  if(is.factor(the_chroma)) {
    the_chroma <- as.character(the_chroma)
  }
  
  # expected data types
  # hue: character
  the_hue <- as.character(the_hue)
  
  # value and chroma: numeric
  the_value <- as.numeric(the_value)
  the_chroma <- as.numeric(the_chroma)
  
  # note: this is incompatible with LazyData: true
  # load look-up table from our package
  # This should be moreover more foolproof than data(munsell) c/o PR
  munsell <- NULL
  load(system.file("data/munsell.rda", package="aqp")[1])
  
  ## 2016-03-07: "fix" neutral hues
  ## they will typically be missing chroma or have some arbitrary number
  ## set it to 0 for correct matching
  N.idx <- which(the_hue == 'N')
  if(length(N.idx) > 0) {
    the_chroma[N.idx] <- 0
  }
    
  
  ## any other hue with 0 chroma should be interpreted as N
  idx <- which(the_hue != 'N' & the_chroma == 0)
  if(length(idx > 0)) {
    the_hue[idx] <- 'N'
  }
  
  
  ## value / chroma should be within unique set of allowed chips
  
  # the valid range depends on the latest version of the munsell LUT
  # https://github.com/ncss-tech/aqp/issues/318
  valid.value <- unique(munsell$value)
  valid.chroma <- unique(munsell$chroma)
  
  
  # test Munsell values
  if(any(! na.omit(the_value) %in% valid.value)) {
    warning("non-standard notation in Munsell value, snapping to the nearest available value\n consider using getClosestMunsellChip()", call. = FALSE)
    
    ## TODO: optimize by only computing distances for non-standard Munsell value
    
    # snap supplied Munsell value to valid value via absolute distance
    # NA are safely propagated
    the_value <- .snapValid(the_value, valid.value)
  }
  
  # test Munsell chroma
  if(any(! na.omit(the_chroma) %in% valid.chroma)) {
    warning("non-standard notation in Munsell chroma, snapping to the nearest available chroma\n consider using getClosestMunsellChip()", call. = FALSE)
    
    ## TODO: optimize by only computing distances for non-standard Munsell chroma
    
    # snap supplied Munsell chroma to valid chroma via absolute distance
    # NA are safely propagated
    the_chroma <- .snapValid(the_chroma, valid.chroma)
  }
  
  
  
  ## join new data with look-up table
  # note that value / chroma must be same data type as in `munsell` (numeric)
  d <- data.frame(
    hue = the_hue, 
    value = the_value, 
    chroma = the_chroma, 
    stringsAsFactors = FALSE
  )
  
  ## benchmarks:
  # data.table::merge() (with conversion to/from) 5x faster than base::merge()
  
  ## TODO: maybe more efficient with keys
  # round-trip through data.table is still faster
  d <- data.table::as.data.table(d)
  munsell <- data.table::as.data.table(munsell)
  
  
  ## TODO: optimize by first filtering full lookup table on e.g. hue
  
  
  # join
  res <- merge(d, munsell, by = c('hue','value','chroma'), all.x = TRUE, sort = FALSE)
  
  # back to data.frame
  res <- as.data.frame(res)
  
  # reset options:
  options(opt.original)
  
  # syntax is now a little muddled, test for sRGB and LAB
  if(return_triplets & returnLAB)
    return(res[, c('r','g','b', 'L', 'A', 'B')])
  
  # if the user wants the raw sRGB triplets, give those back
  if(return_triplets)
    return(res[, c('r','g','b')])
  
  if(returnLAB)
    return(res[, c('L','A','B')])
  
  # keep track of NA values
  rgb.na <- which(is.na(res$r))
  
  # truncate alpha at maxColorValue, otherwise error
  if(alpha > maxColorValue) {
    alpha <- maxColorValue
  }
  
  
  ## convert to hex notation
  # init an empty column
  res$soil_color <- NA
  
  # account for missing values if present: we have to do this because rgb() doesn't like NA
  if(length(rgb.na > 0)) {
    res$soil_color[-rgb.na] <- with(
      res[-rgb.na,], 
      rgb(red = r, green = g, blue = b, alpha = alpha, maxColorValue = maxColorValue) 
    )
  }	else {
    # no missing values
    res$soil_color <- with(
      res, 
      rgb(red = r, green = g, blue = b, alpha = alpha, maxColorValue = maxColorValue) 
    )
  }
  
  
  # default behavior, vector of colors is returned
  return(res$soil_color)
}


# if (!isGeneric("munsell2spc"))
setGeneric("munsell2spc", function(object, ...) standardGeneric("munsell2spc"))

#' @title Merge Munsell Hue, Value, Chroma converted to sRGB & CIELAB into a SoilProfileCollection
#'
#' @description Convert Munsell hue, value and chroma into sRGB (\code{rgb_R, rgb_G, rgb_B}) and CIELAB (lab_L, lab_A, lab_B) color coordinates using \code{munsell2rgb}. The converted values are stored in the \code{horizons()} slot unless \code{as.spc} is \code{FALSE}, in which case the results are combined with profile and horizon ID columns and returned as the \code{data.frame} subclass used by the SPC.
#'
#' @param object A SoilProfileCollection
#' @param hue Column name containing numeric hue values. Default: \code{"hue"}
#' @param value Column name containing numeric value values. Default: \code{"value"}
#' @param chroma Column name containing numeric chroma values. Default: \code{"chroma"}
#' @param .data Optional: a character vector of equal length to number of horizons (containing Munsell notation), or a column name in the horizon data OR a data.frame containing three columns (names specified in \code{hue}, \code{value}, \code{chroma})
#'
#' @param as.spc Return a data.frame-like object with ID columns?
#'
#' @return A SoilProfileCollection or \code{data.frame}-like object
#' @seealso \code{\link{parseMunsell}} \code{\link{rgb2munsell}} \code{\link{munsell2rgb}}
#' @export 
#' @aliases munsell2spc
#' @examples
#'
#' data(sp3)
#' depths(sp3) <- id ~ top + bottom
#'
#' # inspect input data
#' horizons(sp3)[,c("hue","value","chroma")]
#'
#' # do color conversions to sRGB and LAB, join into horizon data
#' sp3 <- munsell2spc(sp3)
#'
#' # plot rgb "R" coordinate by horizon
#' plot(sp3, color = "rgb_R")
#'
#' # plot lab "A" coordinate by horizon
#' plot(sp3, color = "lab_A")
#'
#' # note that `lab_A` values do not exactly match the original `A` values
#' # this is because `lab_A` was computed from the (field determined) Munsell color notation,
#' # while `A` was directly measured in the lab by colorimeter
#' plot(sp3$A, sp3$lab_A, xlab = 'Measured', ylab = 'Converted from Field Observed Munsell')
#'
setMethod("munsell2spc", signature(object = "SoilProfileCollection"),
          function(object,
                   hue = "hue", value = "value", chroma = "chroma",
                   .data = NULL,
                   as.spc = TRUE) {
            
            # if .data vector/column/data.frame not specified
            if (is.null(.data)) {
              
              # need hue, value, chroma as existing columns in horizon data
              if (!all(c(hue, value, chroma) %in% horizonNames(object))) {
                stop("arguments `hue` [character], `value` [numeric] and `chroma` [numeric] must specify column names in the horizon data",
                     call. = FALSE)
              } else {
                h <- horizons(object)
              }
              
            } else {
              # .data might be a data.frame, containing hue, value, chroma, or a character vector with munsell notation
              #  (e.g from parseMunsell(..., convertColors=FALSE))
              if (inherits(.data, 'data.frame')) {
                
                if (ncol(.data) == 1 && is.character(.data[[1]])) {
                  
                  h <- parseMunsell(.data[[1]], convertColors = FALSE)
                  
                } else if (!all(c(hue, value, chroma) %in% colnames(.data))) {
                  stop("arguments `hue` [character], `value` [numeric] and `chroma` [numeric] must specify column names in `.data`",
                       call. = FALSE)
                } else {
                  h <- .data
                }
                
              } else {
                
                if (length(.data) == 1 && .data %in% horizonNames(object)) {
                  .data <- object[[.data]]
                  
                  # otherwise need munsell character columnname, or a vector with equal length to horizons
                } else if (length(.data) != nrow(object)) {
                  stop("argument `.data` [character or data.frame], must specify either a character vector of equal length to number of horizons , a column name in the horizon data (containing Munsell notation) or a data.frame with three columns (names specified in `hue`, `value`, `chroma`)",
                       call. = FALSE)
                }
                
                h <- parseMunsell(.data, convertColors = FALSE)
              }
            }
            
            # makes a data.frame
            # return sRGB + CIELAB at the same time, note that hex notation of color is not returned
            drgb <- munsell2rgb(h[[hue]], h[[value]], h[[chroma]], return_triplets = TRUE, returnLAB = TRUE)
            colnames(drgb)[1:3] <- paste0("rgb_", c("R","G","B"))
            colnames(drgb)[4:6] <- paste0("lab_", c("L","A","B"))
            
            # ID management
            idn <- idname(object)
            hidn <- hzidname(object)
            h <- horizons(object)
            
            # munsell2rgb does not return ID names (not inherently aware of the SPC)
            idcol <- data.frame(h[[idn]], h[[hidn]])
            colnames(idcol) <- c(idn, hidn)
            
            if (as.spc) {
              # horizons<- will ensure merge.data.table triggers if @horizons is data.table
              horizons(object) <- cbind(idcol, drgb)
              
              return(object)
            } else {
              return(.as.data.frame.aqp(cbind(idcol, drgb), aqp_df_class(object)))
            }
          })
