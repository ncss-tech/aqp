
#' @title Format Munsell Notation from Hue, Value, and Chroma
#' 
#' @description Format hue, value, and chroma values into valid Munsell notation, or NA when not possible. The following rules are applied to the input:
#' 
#'   * for all hues other than N, there must be a valid hue, value, and chroma
#'   * N hues may specify chroma as 0, NA, or '' (empty string)
#'   * when `standardHues = TRUE`, hue must be a standard Munsell hue, see [huePosition()]
#' 
#' 
#'
#' @param hue character vector of Munsell hue
#' @param value character or numeric vector of Munsell value
#' @param chroma character or numeric vector of Munsell chroma
#' @param neutralConvention character, neutral color encoding convention
#' 
#' Neutral colors are encoded as:
#'   * 'empty': N 3/
#'   * 'zero': N 3/0
#' 
#' @param standardHues logical, when `TRUE` non-standard hues are converted to NA
#'
#' @author D.E. Beaudette
#'
#' @returns character vector of same length as `hue`, `value`, and `chroma`
#' @export
#' 
#' @seealso [launderMunsell()]
#'
#' @examples
#' 
#' d <- data.frame(
#' hue = c('10YR', NA, 'N', 'N', 'N', '5G', '5Z'),
#' value = c(4, 3, 2, 4, 3, NA, 4),
#' chroma = c(4, 3, NA, 0, NA_integer_, 6, 3)
#' )
#' 
#' formatMunsell(d$hue, d$value, d$chroma)
#' formatMunsell(d$hue, d$value, d$chroma, neutralConvention = 'empty')
#' formatMunsell(d$hue, d$value, d$chroma, standardHues = FALSE)
#' 
#' # all result in 'N 6/0' (neutralConvention = 'zero')
#' formatMunsell('N', 6, 0)
#' formatMunsell('N', 6, '')
#' formatMunsell('N', 6, NA)
#' 
formatMunsell <- function(hue, value, chroma, neutralConvention = c('zero', 'empty'), standardHues = TRUE) {
  
  neutralConvention <- match.arg(neutralConvention)
  
  # value and chroma must be safely convertable to numeric
  value <- as.numeric(value)
  chroma <- as.numeric(chroma)
  
  # chroma could be unspecified ('' or NA)
  
  # optionally enforce standard hues
  if(standardHues) {
    .stdhues <- huePosition(returnHues = TRUE, includeNeutral = TRUE)
    
    # this will also match NA
    .badhue <- which(! hue %in% .stdhues)
    
    # set to NA, these colors will not be formatted
    if(length(.badhue) > 0) {
      hue[.badhue] <- NA
      message('some colors have non-standard hue, result is NA')
    }
  }
  
  
  # neutral hues, may have NA in chroma
  .n <- which(hue == 'N')
  
  # normalize neutral chroma according to convention
  if(length(.n) > 0) {
    .replacement <- switch(neutralConvention, 'empty' = '', 'zero' = '0')
    
    chroma[.n] <- .replacement
  }
  
  
  # all other hues
  # NA in hue | value | chroma => formatting not possible
  .naidx <- which(is.na(hue) | is.na(value) | is.na(chroma))
  
  # compile standard notation
  .res <- sprintf("%s %s/%s", hue, value, chroma)
  
  # NA in hue | value => result is NA
  if(length(.naidx) > 0) {
    .res[.naidx] <- NA
    message('some colors missing hue or value, result is NA')
  }
  
  
  return(.res)
  
}



