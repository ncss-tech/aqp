

# split standard Munsell hue into character | numeric parts
# function is vectorized

# additional ideas from munsellinterpol::HueNumberFromString

# output:
# hue.numeric hue.character
#     2.3            YR
.parseMunsellHue <- function(hue) {

  # NA not permitted, convert to ''
  hue <- ifelse(is.na(hue), '', hue)

  # extract numeric part from hue
  # danger! empty strings will result in an empty list element
  nm.part <- strsplit(hue, split='[^0-9.]+', )

  # replace empty list elements with ''
  nm.part[which(sapply(nm.part, length) < 1)] <- ''

  # the numeric part is always the first returned match
  # solves bogus rows associated with invalid hues
  # # https://github.com/ncss-tech/aqp/issues/66
  nm.part <- lapply(nm.part, '[', 1)

  # convert to vector
  hue.numeric <- unlist(nm.part)

  # extract character part from hue
  hue.character <- vector(mode='character', length = length(hue))
  for(i in seq_along(hue)){
    # check for NA
    if(is.na(hue.numeric[i])) {
      hue.character[i] <- NA
      next
    }
    # check for empty string
    if(hue.numeric[i] == '') {
      hue.character[i] <- NA
      next
    }
    # otherwise continue processing
    hue.character[i] <- gsub(hue.numeric[i], '', hue[i], fixed = TRUE)
  }
  # convert numeric part to numbers
  hue.numeric <- as.numeric(hue.numeric)
  return(data.frame(hue.numeric, hue.character, stringsAsFactors = FALSE))
}




#' @title Parse Munsell Color Notation
#' 
#' @description Split Munsell color notation into "hue", "value", and "chroma", with optional conversion to sRGB hex notation, sRGB coordinates, and CIELAB coordinates. Conversion is performed by \code{munsell2rgb}.
#'
#' @param munsellColor character vector of Munsell colors (e.g. \code{c('10YR 3/4', '5YR 4/6')})
#' @param convertColors logical, convert colors to sRGB hex notation, sRGB coordinates, CIELAB coordinates
#' @param delim optional, specify the type of delimiter used between value and chroma parts of the Munsell code. By default ":", ",:, "'", amd "/" are supported.
#' @param ... additional arguments to \code{munsell2rgb}
#'
#' @return a \code{data.frame} object
#' 
#' @importFrom stringr str_extract_all str_length
#' @export
#'
#' @examples
#' 
#' # just sRGB
#' parseMunsell("10YR 3/5", return_triplets = TRUE)
#' 
#' # sRGB + CIELAB (D65 illuminant)
#' parseMunsell("10YR 3/5", return_triplets = TRUE, returnLAB = TRUE)
#' 
#' # CIELAB only
#' parseMunsell("10YR 3/5", return_triplets = FALSE, returnLAB = TRUE)
#' 

## TODO: this will not correctly parse gley
## TODO: re-write with REGEX for extraction from within other text
## TODO: return NA for obviously wrong Munsell codes

parseMunsell <- function(munsellColor, convertColors=TRUE, delim = NA, ...) {
  # sanity check:
  if(all(is.na(munsellColor)) | all(is.null(munsellColor)) | all(munsellColor == ''))
    return(rep(NA, times=length(munsellColor)))
  
  # ensure colours are all upper case
  munsellColor <- toupper(munsellColor)
  
  # For each munsellColor value
  res <- lapply(
    munsellColor,
    function(mn) {
      # split color into pieces, first at hue[space]value/chroma
      
      # Get number of hue letters
      letters <- unlist(str_extract_all(mn, "[A-Z]+"))
      n_letters <- str_length(letters)
      
      # Use a different split depending on the number of hue letters
      if(n_letters == 1) {
        hue_split <- unlist(strsplit(mn, "(?<=[A-Z]{1})", perl = TRUE))
      } else if (n_letters == 2) {
        hue_split <- unlist(strsplit(mn, "(?<=[A-Z]{2})", perl = TRUE))
      } else {
        stop("Wrong hue string in the Munsell string.", call. = FALSE)
      }
      
      if (is.na(delim)) {
        value_chroma <- unlist(strsplit(hue_split[2], "[:,'/_]"))
      } else {
        value_chroma <- unlist(strsplit(hue_split[2], delim))
      }
      
      
      # extract pieces
      hue <- hue_split[1]
      value <- value_chroma[1]
      chroma <- value_chroma[2]
      
      data.frame(hue = hue, value = value, chroma = chroma, stringsAsFactors = FALSE)
    }
  )
  
  res <- do.call(rbind, res)
  
  # parse, without conversion to numeric / munsell
  if(convertColors == FALSE) return(res)

  # otherwise convert
  res <- munsell2rgb(res$hue, res$value, res$chroma, ...)
  return(res)
}



#' @title sRGB to Munsell Color Conversion
#' 
#' @description Convert sRGB color coordinates to the closest `n` Munsell chips in the \code{munsell} lookup table. 
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
#' \url{http://www.cis.rit.edu/mcsl/online/munsell.php}
#' \url{https://www.munsellcolourscienceforpainters.com/MunsellAndKubelkaMunkToolbox/MunsellAndKubelkaMunkToolbox.html}
#'
#' @return an (NA-padded) \code{data.frame} containing `hue`, `value`, `chroma`, and distance (dE00 when \code{colorSpace = 'CIE2000'}, Euclidean distance otherwise) to nearest matching color.
#' 
#' @export
#'
#' @examples 
#' 
#' # Munsell notation to sRGB triplets [0-1] 
#' color <- munsell2rgb(
#'   the_hue = c('10YR', '2.5YR'), 
#'   the_value = c(3, 5), 
#'   the_chroma = c(5, 6), 
#'   return_triplets = TRUE
#' )
#' 
#' # result is a data.frame
#' color
#' 
#' # sRGB triplets to closest Munsell color 
#' # dE00 distandce metric
#' # result is a data.frame
#' rgb2munsell(color)
#'
rgb2munsell <- function(color, colorSpace = c('CIE2000', 'LAB', 'sRGB'), nClosest = 1) {

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
    if( !requireNamespace('farver', quietly = TRUE) | packageVersion("farver") < '2.0.3' ) {
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
      this.color.lab <- convertColor(this.color, from='sRGB', to='Lab', from.ref.white='D65', to.ref.white = 'D65')
      dimnames(this.color.lab)[[2]] <- c('L', 'A', 'B')

      # fully-vectorized
      sigma <- farver::compare_colour(this.color.lab, munsell[, 7:9], from_space='lab', method = 'CIE2000', white_from = 'D65')

      # return the closest n-matches
      idx <- order(sigma)[1:nClosest]
    }


    # ## TODO: this doesn't actually trap the condition we are attempting to trap!
    # # https://github.com/ncss-tech/aqp/issues/160
    # # with NA as an input, there will be no output
    # if(length(idx) == 0)
    #   res[[i]] <- data.frame(hue=NA, value=NA, chroma=NA, sigma=NA, stringsAsFactors=FALSE)
    #
    # # otherwise return the closest color
    # else
    #   res[[i]] <- data.frame(munsell[idx, 1:3], sigma=sigma[idx])

    res[[i]] <- data.frame(munsell[idx, 1:3], sigma = sigma[idx])

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
#' Values of "2.5" (common in soil color descriptions) are silently truncated to "2".
# 
#' Non-standard Munsell notation (e.g. '7.9YR 2.7/2.0') can be matched (nearest-neighbor, no interpolation) to the closest color within the \code{munsell} sRGB/CIELAB look-up table via \code{getClosestMunsellChip()}. A more accurate estimate of sRGB values from non-standard notation can be achieved with the \href{https://CRAN.R-project.org/package=munsellinterpol}{munsellinterpol} package.
#' 
#' See examples below.
#' 
#' @note Care should be taken when using the resulting sRGB values; they are close to their Munsell counterparts, but will vary based on your monitor and ambient lighting conditions. Also, the value used for \code{maxColorValue} will affect the brightness of the colors. Th default value (1) will usually give acceptable results, but can be adjusted to force the colors closer to what the user thinks they should look like.
#' 
#' @references \url{http://ncss-tech.github.io/AQP/}
#' \url{http://www.brucelindbloom.com/index.html?ColorCalcHelp.html}
#' \url{http://www.cis.rit.edu/mcsl/online/munsell.php}
#' \url{https://www.munsellcolourscienceforpainters.com/MunsellAndKubelkaMunkToolbox/MunsellAndKubelkaMunkToolbox.html}
#' 
#' @author D.E. Beaudette
#' 
#' @export
#'
#' @examples
#' 
#' # neutral heues (N) map to approximate greyscale colors
#' # chroma may be any number or NA
#' g <- expand.grid(hue='N', value=2:8, chroma=NA, stringsAsFactors=FALSE)
#' munsell2rgb(g$hue, g$value, g$chroma)
#' 
#' 
#' # basic example
#' d <- expand.grid(hue='10YR', value=2:8, chroma=1:8, stringsAsFactors=FALSE)
#' d$color <- with(d, munsell2rgb(hue, value, chroma))
#' 
#' # similar to the 10YR color book page
#' plot(value ~ chroma, data=d, col=d$color, pch=15, cex=3)
#' 
#' # multiple pages of hue:
#' hues <- c('2.5YR','5YR','7.5YR','10YR')
#' d <- expand.grid(hue=hues, value=2:8, chroma=seq(2,8,by=2), stringsAsFactors=FALSE)
#' # convert Munsell -> sRGB
#' d$color <- with(d, munsell2rgb(hue, value, chroma))
#' 
#' # extract CIELAB coordinates
#' with(d, munsell2rgb(hue, value, chroma, returnLAB=TRUE))
#' 
#' # plot: note that we are setting panel order from red --> yellow
#' library(lattice)
#' xyplot(value ~ factor(chroma) | factor(hue, levels=hues),
#'        main="Common Soil Colors", layout=c(4,1), scales=list(alternating=1),
#'        strip=strip.custom(bg=grey(0.85)),
#'        data=d, as.table=TRUE, subscripts=TRUE, xlab='Chroma', ylab='Value',
#'        panel=function(x, y, subscripts, ...)
#'        {
#'          panel.xyplot(x, y, pch=15, cex=4, col=d$color[subscripts])
#'        }
#' )
#' 
#' 
#' # soils example
#' data(sp1)
#' 
#' # convert colors
#' sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))
#' 
#' # simple plot, may need to tweak gamma-correction...
#' image(matrix(1:nrow(sp1)), axes=FALSE, col=sp1$soil_color, main='Soil Colors')
#' 
#' # convert into a more useful color space
#' # you will need the colorspace package for this to work
#' if(require(colorspace)) {
#'   # keep RGB triplets from conversion
#'   sp1.rgb <- with(sp1, munsell2rgb(hue, value, chroma, return_triplets=TRUE))
#'   
#'   # convert into LAB color space
#'   sp1.lab <- as(with(sp1.rgb, sRGB(r,g,b)), 'LAB')
#'   plot(sp1.lab)
#' }
#' 
#' # convert a non-standard color to closest "chip" in `munsell` look-up table
#' getClosestMunsellChip('7.9YR 2.7/2.0', convertColors = FALSE)
#' # convert directly to R color
#' getClosestMunsellChip('7.9YR 2.7/2.0')


munsell2rgb <- function(the_hue, the_value, the_chroma, alpha = 1, maxColorValue = 1, return_triplets = FALSE, returnLAB = FALSE) {
	## important: change the default behavior of data.frame and melt
  opt.original <- options(stringsAsFactors = FALSE)

  # check for missing data
	if(missing(the_hue) | missing(the_chroma) | missing(the_value))
		stop('Must supply a valid Munsell color.')

	# check to make sure that each vector is the same length
	if(length(unique( c(length(the_hue), length(the_value), length(the_chroma)))) != 1)
		stop('All inputs must be vectors of equal length.')
  
  ## TODO: depricate this
  ## plyr <= 1.6 : check to make sure hue is a character
  if(is.factor(the_hue)) {
    cat('Notice: converting hue to character\n')
    the_hue <- as.character(the_hue)
  }


  # This is a hack to avoid munsell2rgb: "no visible binding for global variable munsell" at package R CMD check
  munsell <- NULL

  # note: this is incompatible with LazyData: true
  # load look-up table from our package
  # This should be moreover more foolproof than data(munsell) c/o PR
  load(system.file("data/munsell.rda", package="aqp")[1])

  ## 2016-03-07: "fix" neutral hues
  ## they will typically be missing chroma or have some arbitrary number
  ## set it to 0 for correct mattching
  N.idx <- which(the_hue == 'N')
  if(length(N.idx) > 0)
    the_chroma[N.idx] <- 0


  ## TODO: interpolate all 1/2 chips : https://github.com/ncss-tech/aqp/issues/178
  # 2016-03-07: "fix" values of 2.5 by rounding to 2
  the_value <- ifelse(the_value == 2.5, 2, the_value)

  ## temporary fix for #44 (https://github.com/ncss-tech/aqp/issues/44)
  # round non integer value and chroma
  if ( !isTRUE(all.equal(as.character(the_value), as.character(as.integer(the_value)) )) ) {
    the_value <- round(as.numeric(the_value))
    warning("'the_value' has been rounded to the nearest integer.", call. = FALSE)
  }
  if ( !isTRUE(all.equal(as.character(the_chroma), as.character(as.integer(the_chroma)) )) ) {
    the_chroma <- round(as.numeric(the_chroma))
    warning("'the_chroma' has been rounded to the nearest integer.", call. = FALSE)
  }
  
  
  ## join new data with look-up table
  # note that value / chroma must be same data type as in `munsell` (numeric)
  d <- data.frame(
    hue = the_hue, 
    value = as.numeric(the_value), 
    chroma = as.numeric(the_chroma), 
    stringsAsFactors = FALSE
  )
  
  ## benchmarks:
  # plyr::join 2x faster than base::merge
  # data.table::merge (with conversion to/from) 5x faster than base::merge
  
  ## TODO: maybe more efficient with keys
  # round-trip through data.table is still faster
  d <- as.data.table(d)
  munsell <- as.data.table(munsell)
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
		
	# convert to R color
	res$soil_color <- NA # init an empy column

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


if (!isGeneric("munsell2spc"))
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
#' @aliases munsell2spc
#' @seealso \code{\link{parseMunsell}} \code{\link{rgb2munsell}} \code{\link{munsell2rgb}}
#' @export munsell2spc,SoilProfileCollection-method
#'
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
