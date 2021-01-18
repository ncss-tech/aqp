

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


## TODO: this will not correctly parse gley
## TODO: re-write with REGEX for extraction from within other text
## TODO: return NA for obviously wrong Munsell codes
#
# convert a color string '10YR 4/3' to sRGB or R color
parseMunsell <- function(munsellColor, convertColors=TRUE, ...) {
  # sanity check:
  if(all(is.na(munsellColor)) | all(is.null(munsellColor)) | all(munsellColor == ''))
    return(rep(NA, times=length(munsellColor)))

  ## TODO: switch to stringr::str_split()
  ## https://github.com/ncss-tech/aqp/issues/66
  pieces <- strsplit(munsellColor, ' ', fixed=TRUE)
  pieces.2 <- sapply(pieces, function(i) strsplit(i[2], '/', fixed=TRUE))
  hue <- sapply(pieces, function(i) i[1])
  value <- sapply(pieces.2, function(i) i[1])
  chroma <- sapply(pieces.2, function(i) i[2])

  # parse, don't convert
  if(convertColors == FALSE)
    return(data.frame(hue, value, chroma, stringsAsFactors = FALSE))

  # otherwise convert
  res <- munsell2rgb(hue, value, chroma, ...)
  return(res)
}


# color: matrix/data.frame of sRGB values in range of [0,1]
# colorSpace: color space / distance metric (CIE2000, LAB, sRGB)
# nClosest: number of closest chips to return
rgb2munsell <- function(color, colorSpace = c('CIE2000', 'LAB', 'sRGB'), nClosest = 1) {

  # argument check
  colorSpace <- match.arg(colorSpace)

  # vectorize via for-loop
  n <- nrow(color)
  res <- vector(length=n, mode='list')

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

  # https://github.com/ncss-tech/aqp/issues/160
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

    res[[i]] <- data.frame(munsell[idx, 1:3], sigma=sigma[idx])

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

# TODO if alpha is greater than maxColorValue, there will be an error
munsell2rgb <- function(the_hue, the_value, the_chroma, alpha=1, maxColorValue=1, return_triplets=FALSE, returnLAB=FALSE) {
	## important: change the default behavior of data.frame and melt
  opt.original <- options(stringsAsFactors = FALSE)

  # check for missing data
	if(missing(the_hue) | missing(the_chroma) | missing(the_value))
		stop('Must supply a valid Munsell color.')

	# check to make sure that each vector is the same length
	if(length(unique( c(length(the_hue), length(the_value), length(the_chroma)))) != 1)
		stop('All inputs must be vectors of equal length.')

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

  # join new data with look-up table
  d <- data.frame(hue=the_hue, value=the_value, chroma=the_chroma, stringsAsFactors=FALSE)
  ## TODO: convert to merge() (~ 30% slower than join)
  ## TODO: experiment with on-the-fly DT invocation if available
  res <- join(d, munsell, type = 'left', by = c('hue','value','chroma'))

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

	# not really an ideal solution, but seems to work
	# if alpha > maxColorValue -- clamp alpha at maxColorValue
	if(alpha > maxColorValue)
		alpha <- maxColorValue

	# convert to R color
	res$soil_color <- NA # init an empy column

  # account for missing values if present: we have to do this because rgb() doesn't like NA
	if(length(rgb.na > 0))
		res$soil_color[-rgb.na] <- with(res[-rgb.na,], rgb(red=r, green=g, blue=b, alpha=alpha, maxColorValue=maxColorValue) )
  # no missing values
	else
		res$soil_color <- with(res, rgb(red=r, green=g, blue=b, alpha=alpha, maxColorValue=maxColorValue) )

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
