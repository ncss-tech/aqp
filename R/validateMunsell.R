



#' @title Validate Standard Munsell Notation
#' 
#' @description
#' This function validates Munsell color notation according to the following requirements:
#' 
#'  1. *hue* must not be absent, and within the set of "standard hues", see [huePosition()]
#'  2. *value* must not be absent, and within `vRange`
#'  3. *chroma* must be within `cRange`, and is only allowed to be absent in neutral colors (e.g. `N 4/`)
#' 
#' @seealso [huePosition()] 
#'
#' @param m character vector of colors in Munsell notation
#' 
#' @param vRange numeric vector of length 2, range of expected Munsell value
#' 
#' @param cRange numeric vector of length 2, range of expected Munsell chroma
#'
#' @returns logical vector,
#'  * `TRUE`: valid Munsell notation
#'  * `FALSE`: invalid notation
#' 
#' @export
#'
#' @examples
#' 
#' # valid
#' validateMunsell('5Y 6/8')
#' validateMunsell('N 4/')
#' validateMunsell('2.5Y 4/0')
#' 
#' # invalid
#' validateMunsell('5G 0/4')
#' validateMunsell(NA)
#' 
#' # mixture
#' validateMunsell(c('5G 4/4', 'N 2/', NA, 'NA', '100R 3/3'))
#' 
validateMunsell <- function(m, vRange = c(1, 12), cRange = c(0, 50)) {
  
  # short-circuits
  if(all(is.na(m))) {
    return(rep(FALSE, times = length(m)))
  }
  
  # split / interpret input ----
  
  # split into [H, V, C] expecting standard notation
  #  * non-standard hues will be split
  #  * Neutral chips, NA chroma converted to 0 chroma
  .p <- suppressWarnings(
    parseMunsell(m, convertColors = FALSE)
  )
  
  ## TODO: maybe consider non-standard hues
  
  # standard hues
  .hp <- huePosition(returnHues = TRUE, includeNeutral = TRUE)
  
  # requirements:
  #  1. must be a standard hue, and not NA
  #  2. value must be within vRange, and not NA
  #  3. chroma must be within cRange, and not NA (unless Neutral chip)
  #
  # --> TRUE: valid, FALSE: invalid
  
  # req. 1 ----
  # NA automatically => FALSE
  .req1 <- .p$hue %in% .hp
  
  # req. 2 ----
  .req2 <- .p$value > vRange[1] & .p$value <= vRange[2]
  
  ## handle NA ----
  .req2.na <- is.na(.p$value)
  
  .idx <- which(is.na(.req2))
  if(length(.idx) > 0) {
    # invert logic -> Munsell notation is invalid
    .req2[.idx] <- ! .req2.na[.idx]
  }
  
  # req 3. ----
  # Neutral chip + NA correctly handled by parseMunsell()
  .req3 <- .p$chroma >= cRange[1] & .p$chroma <= cRange[2]
  
  ## handle NA for non-Neutral chips ----
  .req3.na <- is.na(.p$chroma)
  
  .idx <- which(is.na(.req3))
  if(length(.idx) > 0) {
    # invert logic -> Munsell notation is invalid
    .req3[.idx] <- ! .req3.na[.idx]
  }
  
  # a valid color meets all three requirements
  .res <- .req1 & .req2 & .req3
  
  return(.res)
}

