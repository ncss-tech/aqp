



#' @title Get Approximate Munsell Chip
#' 
#' @description Non-standard Munsell notation (e.g. '7.9YR 2.7/2.0') can be matched (nearest-neighbor, no interpolation) to the closest color within the `munsell` sRGB/CIELAB look-up table via \code{getClosestMunsellChip()}. A more accurate estimate of sRGB values from non-standard notation can be achieved with the \href{https://CRAN.R-project.org/package=munsellinterpol}{munsellinterpol} package.
#'
#' @param munsellColor character vector of strings containing Munsell notation of color, e.g. '10YR 4/3'
#' @param convertColors logical, should parsed Munsell colors be converted into sRGB values
#' @param ... further arguments to \code{munsell2rgb}
#'
#'
#' @return a \code{data.frame} when \code{convertColors=TRUE}, otherwise character vector
#' @export
#'
#' @examples
#' 
#' # convert a non-standard color to closest "chip" in `munsell` look-up table
#' getClosestMunsellChip('7.9YR 2.7/2.0', convertColors = FALSE)
#' # convert directly to R color
#' getClosestMunsellChip('7.9YR 2.7/2.0')
#' 
getClosestMunsellChip <- function(munsellColor, convertColors = TRUE, ...) {
  
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
  
  ## TODO: don't round 2.5 values: https://github.com/ncss-tech/aqp/issues/251
  # locate closest value and chroma by rounding
  closest.value <- round(as.numeric(cd$value))
  closest.chroma <- round(as.numeric(cd$chroma))
  
  # convert values < 1 -> 1
  closest.value <- ifelse(closest.value < 1, 1, closest.value)
  closest.chroma <- ifelse(closest.chroma < 1, 1, closest.chroma)
  
  # optionally convert closest Munsell chips to sRGB
  if(convertColors) {
    res <- munsell2rgb(closest.hue, closest.value, closest.chroma, ...)
  } else {
    # otherwise return closest chip
    res <- paste0(closest.hue, ' ', closest.value, '/', closest.chroma)
  }
    
  return(res)
}
