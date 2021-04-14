

#' @title Convert Pantone PMS codes to Munsell notation
#' 
#' @details Conversion of \href{https://en.wikipedia.org/wiki/Pantone}{Pantone spot colors} (PMS code) is performed by look-up from [`pms.munsell.lut`]. Association is based on the "closest" Munsell color via \href{https://en.wikipedia.org/wiki/Color_difference#CIEDE2000}{CIE2000 distance metric (dE00)} (see [`rgb2munsell`]). This is an experimental association between the two color systems and should not be used for precision color matching or paint mixing applications.
#'
#' Possible uses include rough estimation of soil colors in the field, by means of color swatches based on the Pantone system. This type of color matching is most appropriate in an educational setting where official soil color books may be too expensive.
#'
#' @param codes vector of PMS codes (e.g. '7630-c'), may contain NA, see [`pms.munsell.lut`]
#'
#' @return `data.frame` containing closest associated Munsell color (via `rgb2munsell`), hex notation, and perceptual color distance (dE00) between sRGB values and closest Munsell "chip".
#' 
#' @note Inspired by the work and outreach efforts of Dr. Karen Vaughan (UWY).
#' 
#' @author D.E. Beaudette
#' 
#' @export
#'
#' @examples
#' 
#' # safely handles NA
#' codes <- c(NA, "7630-c", "102-c")
#' 
#' PMS2Munsell(codes)
#' 
PMS2Munsell <- function(codes) {
  
  # make R CMD check happy
  pms.munsell.lut <- NULL
  
  # note: this is incompatible with LazyData: true
  # load look-up table from our package
  load(system.file("data/pms.munsell.lut.rda", package="aqp")[1])
  
  # valid PMS code?
  test.codes <- codes %in% pms.munsell.lut$code
  
  # there must be at least 1 non-NA / valid code
  if(all(!test.codes)) {
    stop('all codes are either NA or missing from `pms.munsell.lut` conversion table', call. = FALSE)
  }
  
  # find associated records, safely returning NA-records
  idx <- match(codes, pms.munsell.lut$code)
  res <- pms.munsell.lut[idx, ]
  
  # reset rownames
  row.names(res) <- NULL
  
  return(res)
}

