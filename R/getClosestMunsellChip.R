
#' @title Get Approximate Munsell Chip
#' 
#' @description Non-standard Munsell notation ('7.9YR 2.7/2.0') can be matched (nearest-neighbor, no interpolation) to the closest color within the `munsell` sRGB/CIELAB look-up table via `getClosestMunsellChip()`. A more accurate estimate of sRGB values from non-standard notation can be achieved with the \href{https://CRAN.R-project.org/package=munsellinterpol}{munsellinterpol} package. For example, conversion from Munsell to CIELAB, assuming a D65 illuminant via: `MunsellToLab('0.1Y 3.3/4.4', white='D65', adapt='Bradford')`.
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
#' 
#' # convert directly to R color
#' getClosestMunsellChip('7.9YR 2.7/2.0')
#' 
#' # special case for 2.5 value -> no rounding, we have these records in the conversion LUT
#' getClosestMunsellChip('7.5YR 2.5/2', convertColors = FALSE)
#' 
#' 
#' getClosestMunsellChip('7.5YR 6.8/4.4', convertColors = FALSE)
#' 
getClosestMunsellChip <- function(munsellColor, convertColors = TRUE, ...) {
  
  # This is a hack to avoid munsell2rgb: "no visible binding for global variable munsell" at package R CMD check
  munsell <- NULL
  
  # extract hue, value, chroma from single string
  cd <- parseMunsell(munsellColor, convertColors = FALSE)
  
  # extract pieces of hue
  hue.data <- .parseMunsellHue(cd$hue)
  
  ## TODO: evaluate closest standard hue via evaluation of hue positions
  ##       -> huePosition(returnHues = TRUE)
  ##       -> interpreting 10YR as the same as 0Y
  
  
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
  
  # valid value / chroma in our LUT
  valid.value <- unique(munsell$value)
  valid.chroma <- unique(munsell$chroma)
  
  # treat as numeric
  cd$value <- as.numeric(cd$value)
  cd$chroma <- as.numeric(cd$chroma)
  
  # locate closest value / chroma
  closest.value <- vector(mode = 'numeric', length = nrow(cd))
  closest.chroma <- vector(mode = 'numeric', length = nrow(cd))
  for(i in 1:nrow(cd)) {
    # search for closest value
    idx <- which.min(abs(cd$value[i] - valid.value))
    closest.value[i] <- valid.value[idx]
    
    # search for closest chroma
    idx <- which.min(abs(cd$chroma[i] - valid.chroma))
    closest.chroma[i] <- valid.chroma[idx]
  }
  
  # convert values and chroma < 1 -> 1
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
