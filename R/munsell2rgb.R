

# split standard Munsell hue into character | numeric parts
# function is vectorized
# output:
# hue.numeric hue.character
# 1         2.3            YR
.parseMunsellHue <- function(hue) {
  # extract numeric part from hue
  hue.numeric <- unlist(strsplit(hue, split='[^0-9.]+'))
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

# return the closest Munsell chip from `munsell` data in aqp package
# function is vectorized
getClosestMunsellChip <- function(munsellColor, convertColors=TRUE, ...) {
  # This is a hack to avoid munsell2rgb: "no visible binding for global variable munsell" at package R CMD check
  munsell <- NULL
  
  # extract hue, value, chroma from single string
  cd <- parseMunsell(munsellColor, convertColors = FALSE)
  
  # extract pieces of hue
  hue.data <- .parseMunsellHue(cd$hue)
  
  # note: this is incompatible with LazyData: true
  # extract pieces from unique Munsell hues
  load(system.file("data/munsell.rda", package="aqp")[1])
  all.hue.data <- na.omit(.parseMunsellHue(unique(munsell$hue)))
  
  # locate closest chip in `munsell` set of hues
  closest.hue <- vector(mode = 'character', length=nrow(hue.data))
  for(i in 1:nrow(hue.data)) {
    # index possible rows based on character part of hue
    idx <- which(all.hue.data$hue.character == hue.data[i, ]$hue.character)
    # compute Euclidean distance to all possible numeric parts of hue
    distances <- abs(hue.data$hue.numeric[i] - all.hue.data$hue.numeric[idx])
    closest.idx <- which.min(distances)
    # compile closest hue
    closest.hue[i] <- paste0(all.hue.data[idx, ][closest.idx, ], collapse = '')
  }
  
  # locate closest value and chroma by rounding
  closest.value <- round(as.numeric(cd$value))
  closest.chroma <- round(as.numeric(cd$chroma))
  
  # convert values < 1 -> 1
  closest.value <- ifelse(closest.value < 1, 1, closest.value)
  closest.chroma <- ifelse(closest.chroma < 1, 1, closest.chroma)
  
  # optionally convert closest Munsell chips to RGB
  if(convertColors)
    res <- munsell2rgb(closest.hue, closest.value, closest.chroma, ...)
  # otherwise return closest chip
  else
    res <- paste0(closest.hue, ' ', closest.value, '/', closest.chroma)
  return(res)
}



## TODO: this will not correctly parse gley or neutral colors
# convert a color string '10YR 4/3' to RGB or R color
parseMunsell <- function(munsellColor, convertColors=TRUE, ...) {
  # sanity check:
  if(all(is.na(munsellColor)) | all(is.null(munsellColor)) | all(munsellColor == ''))
    return(rep(NA, times=length(munsellColor)))
  
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



# color is a matrix/vector of sRGB values in range of [0,1]
# ideally output from munsell2rgb()
rgb2munsell <- function(color, colorSpace='LAB', nClosest=1) {
  
  # vectorize via for-loop
  n <- nrow(color)
  res <- vector(length=n, mode='list')
  
  # This is a hack to avoid munsell2rgb: "no visible binding for global variable munsell" at package R CMD check
  munsell <- NULL
  
  # note: this is incompatible with LazyData: true
  # load look-up table from our package
  # This should be more foolproof than data(munsell) c/o PR
  load(system.file("data/munsell.rda", package="aqp")[1])
  
  ## TODO (this is now the default)
  ## - test
  ## - report changes, possibly save for 2.0
  ## - Euclidean distance most useful?
  
  ## TODO: this could probably be optimized
  # iterate over colors
  for(i in 1:n) {
    # convert current color to matrix, this will allow matrix and DF as input
    this.color <- as.matrix(color[i, , drop=FALSE])
    
    if(colorSpace == 'sRGB') {
      # euclidean distance (in sRGB space) is our metric for closest-color
      # d = sqrt(r^2 + g^2 + b^2)
      sq.diff <- sweep(munsell[, 4:6], MARGIN=2, STATS=this.color, FUN='-')^2
      sq.diff.sum.sqrt <- sqrt(rowSums(sq.diff))
      # rescale distances to 0-1
      sq.diff.sum.sqrt <- sq.diff.sum.sqrt / max(sq.diff.sum.sqrt)
      # return the closest n-matches
      idx <- order(sq.diff.sum.sqrt)[1:nClosest]
    }
    if(colorSpace == 'LAB') {
      # euclidean distance (in LAB space) is our metric for closest-color
      # convert sRGB to LAB
      this.color.lab <- convertColor(this.color, from='sRGB', to='Lab', from.ref.white='D65', to.ref.white = 'D65')
      # d = sqrt(L^2 + A^2 + B^2)
      sq.diff <- sweep(munsell[, 7:9], MARGIN=2, STATS=this.color.lab, FUN='-')^2
      sq.diff.sum.sqrt <- sqrt(rowSums(sq.diff))
      # rescale distances to 0-1
      sq.diff.sum.sqrt <- sq.diff.sum.sqrt / max(sq.diff.sum.sqrt)
      # return the closest n-matches
      idx <- order(sq.diff.sum.sqrt)[1:nClosest]
    }

    # with NA as an input, there will be no output
    if(length(idx) == 0)
      res[[i]] <- data.frame(hue=NA, value=NA, chroma=NA, sigma=NA, stringsAsFactors=FALSE)
    
    # otherwise return the closest color
    else
      res[[i]] <- data.frame(munsell[idx, 1:3], sigma=sq.diff.sum.sqrt[idx])
  }
  
  # convert to DF and return
  return(ldply(res))
}

# TODO if alpha is greater than maxColorValue, there will be an error
# TODO: properly convert N chips
# TODO: correctly interpret values of 2.5
# convert munsell Hue, Value, Chroma into sRGB
# user can adjust how rgb() function will return an R-friendly color
munsell2rgb <- function(the_hue, the_value, the_chroma, alpha=1, maxColorValue=1, return_triplets=FALSE) {
	## important: change the default behavior of data.frame and melt
  opt.original <- options(stringsAsFactors = FALSE)
  
  # check for missing data
	if(missing(the_hue) | missing(the_chroma) | missing(the_value))
		stop('Must supply a valid Munsell color.')
	
	# check to make sure that each vector is the same length
	if(length(unique( c(length(the_hue),length(the_value),length(the_chroma)))) != 1)
		stop('All inputs must be vectors of equal length.')
	
  ## plyr <= 1.6 : check to make sure hue is a character
  if(class(the_hue) == 'factor') {
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
  
  
  ## 2016-03-07: "fix" values of 2.5 by rounding to 2
  the_value <- ifelse(the_value == 2.5, 2, the_value)
  
  
  # join new data with look-up table
  d <- data.frame(hue=the_hue, value=the_value, chroma=the_chroma, stringsAsFactors=FALSE)
  res <- join(d, munsell, type='left', by=c('hue','value','chroma')) # result has original munsell + r,g,b
	
  # reset options:
  options(opt.original)
  
	# if the user wants the raw RGB triplets, give those back
	if(return_triplets)
		return(res[, c('r','g','b')])
	
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
