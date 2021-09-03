# determine index of Munsell hue from 5R ---> 5PB
# x: vector of Munsell hues
# returnHues: return hue ordering, x is ignored
#' Munsell Hue Position for Soil Color Description
#'
#' Munsell hues are typically arranged from 5R to 5PB in Munsell soil color
#' books. This function matches a vector of Munsell hues to the position in
#' this arrangement of 29 hues.
#'
#' This function is fully vectorized.
#'
#' @param x character vector of hues, e.g. '10YR'
#' @param returnHues logical, should the unique set of Munsell hues used for
#' ordering be returned? See details.
#' 
#' @param includeNeutral logical, add 'N' to the beginning of the unique set of Munsell hues 
#' 
#' @return A vector of integer hue positions is typically returned, of the same
#' length and order as \code{x}. If \code{returnHues} is TRUE, then the hue
#' names and ordering is returned and \code{x} is ignored.
#' @author D.E. Beaudette
#' @seealso \code{\link{colorContrast}}
#' @references
#' https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569
#' @keywords manip
#' @examples
#'
#' # get hue ordering for setting levels of a factor
#' huePosition(x=NULL, returnHues=TRUE)
#'
#' # get position of the '10YR' hue (7)
#' huePosition(x='10YR')
#'
huePosition <- function(x, returnHues = FALSE, includeNeutral = FALSE) {
  # ordering via Tech Note #2
  # https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569
  hues <- c('5R', '7.5R', '10R',
            '2.5YR', '5YR', '7.5YR', '10YR',
            '2.5Y', '5Y', '7.5Y', '10Y',
            '2.5GY', '5GY', '7.5GY', '10GY',
            '2.5G', '5G', '7.5G', '10G',
            '2.5BG', '5BG', '7.5BG', '10BG',
            '2.5B', '5B', '7.5B', '10B',
            '2.5PB', '5PB')

  # just the hues
  if(returnHues) {
    # N is added to the beginning, position 0
    if(includeNeutral) {
      return(c('N', hues))
    } else {
      return(hues)
    }
  } else {
    # convert hue into position
    res <- match(x, hues)
    return(res)
  }

}
